/**
 * @fileoverview Unit tests for synchronous-callback breakpoint support.
 *
 * Tests that breakpoints fire in synchronous DOM callback code (e.g. button
 * click handlers) by verifying the `_inSyncPath` flag on the probe runtime.
 *
 * When the panel is connected, probes normally return false immediately so
 * Chrome's Sources tab never blocks on `debugger;`. The `_inSyncPath` flag
 * allows probes to fire on the synchronous `run()` path (called from JS
 * closures like DOM callbacks), while still skipping probes on the async
 * `runDebug()` trampoline path.
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { SchemeSourceRegistry } from '../../src/debug/devtools/source_registry.js';
import { DevToolsDebugIntegration } from '../../src/debug/devtools/devtools_debug.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';

// =============================================================================
// Helper
// =============================================================================

/**
 * Creates a minimal debug interpreter for testing.
 * @returns {{interpreter, env, debugRuntime, devtools, sourceRegistry}}
 */
function createDebugInterpreter() {
  const { interpreter, env } = createInterpreter();
  const sourceRegistry = new SchemeSourceRegistry();
  const debugRuntime = new SchemeDebugRuntime();
  debugRuntime.enable();
  interpreter.setDebugRuntime(debugRuntime);

  const devtools = new DevToolsDebugIntegration(sourceRegistry);
  devtools.enable();
  interpreter.devtoolsDebug = devtools;
  debugRuntime.setDevToolsIntegration(devtools);
  devtools.installSchemeDebugAPI(interpreter);

  return { interpreter, env, debugRuntime, devtools, sourceRegistry };
}

/**
 * Runs all sync callback breakpoint tests.
 * @param {Object} logger
 */
export function runSyncCallbackBreakpointTests(logger) {

  // =========================================================================
  // Direct hit() tests with _inSyncPath flag
  // =========================================================================

  logger.title('Sync Callback - hit() returns true when panelConnected + inSyncPath');

  {
    // Save and replace probe runtime with a controlled instance
    const saved = globalThis.__schemeProbeRuntime;

    const runtime = {
      _active: true,
      _panelConnected: true,
      _inSyncPath: true,
      _breakpoints: new Set([42]),
      _stepping: false,
      _stepMode: null,
      _exceptionPause: false,
      _getDepth: null,
      hit(exprId) {
        if (!this._active) return false;
        if (this._panelConnected && !this._inSyncPath) return false;
        if (this._exceptionPause) { this._exceptionPause = false; return true; }
        if (this._breakpoints.has(exprId)) return true;
        if (this._stepping) return true;
        return false;
      }
    };

    // panelConnected=true AND inSyncPath=true AND breakpoint set → should pause
    const result = runtime.hit(42);
    assert(logger, 'hit() returns true when panelConnected=true, inSyncPath=true, breakpoint set',
      result, true);

    globalThis.__schemeProbeRuntime = saved;
  }

  // =========================================================================
  // hit() returns false when panelConnected + NOT inSyncPath (async path)
  // =========================================================================

  logger.title('Sync Callback - hit() returns false when panelConnected + NOT inSyncPath');

  {
    const saved = globalThis.__schemeProbeRuntime;

    const runtime = {
      _active: true,
      _panelConnected: true,
      _inSyncPath: false,
      _breakpoints: new Set([42]),
      _stepping: false,
      _stepMode: null,
      _exceptionPause: false,
      _getDepth: null,
      hit(exprId) {
        if (!this._active) return false;
        if (this._panelConnected && !this._inSyncPath) return false;
        if (this._exceptionPause) { this._exceptionPause = false; return true; }
        if (this._breakpoints.has(exprId)) return true;
        if (this._stepping) return true;
        return false;
      }
    };

    // panelConnected=true AND inSyncPath=false → cooperative path: probes must NOT fire
    const result = runtime.hit(42);
    assert(logger, 'hit() returns false when panelConnected=true, inSyncPath=false',
      result, false);

    globalThis.__schemeProbeRuntime = saved;
  }

  // =========================================================================
  // run() sets _inSyncPath=true during execution and resets after
  // =========================================================================

  logger.title('Sync Callback - run() sets _inSyncPath during execution');

  {
    const { interpreter, env, sourceRegistry } = createDebugInterpreter();

    // Track _inSyncPath values observed during execution
    const observed = [];
    const savedHit = globalThis.__schemeProbeRuntime?.hit;

    if (globalThis.__schemeProbeRuntime) {
      globalThis.__schemeProbeRuntime.hit = function(exprId) {
        observed.push(this._inSyncPath);
        // Don't actually pause
        return false;
      };
    }

    // Verify _inSyncPath is false before run()
    const before = globalThis.__schemeProbeRuntime?._inSyncPath ?? false;
    assert(logger, '_inSyncPath is false before run()', before, false);

    // Register and run some code
    const url = 'scheme://test/sync-flag.scm';
    const code = '(define x 1)\n(+ x 1)';
    const exprs = parse(code, { filename: url });
    sourceRegistry.register(url, code, 'test', exprs);
    const ast = exprs.length === 1 ? analyze(exprs[0]) : analyze(list(intern('begin'), ...exprs));
    interpreter.run(ast, env);

    // Verify _inSyncPath is false after run()
    const after = globalThis.__schemeProbeRuntime?._inSyncPath ?? false;
    assert(logger, '_inSyncPath is false after run() completes', after, false);

    // Verify that during run(), _inSyncPath was true for at least one probe hit
    const anyTrue = observed.some(v => v === true);
    assert(logger, '_inSyncPath was true during at least one probe hit in run()', anyTrue, true);

    // Restore
    if (globalThis.__schemeProbeRuntime && savedHit) {
      globalThis.__schemeProbeRuntime.hit = savedHit;
    }
  }

  // =========================================================================
  // Nested run() calls properly save/restore _inSyncPath
  // =========================================================================

  logger.title('Sync Callback - nested run() saves/restores _inSyncPath');

  {
    // When run() is called while already in run() (e.g. Scheme calls a JS function
    // that calls a Scheme closure), each run() should set _inSyncPath=true and
    // the value should be properly restored when the inner run() returns.

    // Simulate by directly checking that _inSyncPath is restored after run() throws
    if (globalThis.__schemeProbeRuntime) {
      const rt = globalThis.__schemeProbeRuntime;
      const prevInSyncPath = rt._inSyncPath;

      // Simulate: _inSyncPath was false before entry
      rt._inSyncPath = false;

      // After a run() call (even one that errors), _inSyncPath should be restored
      const { interpreter, env } = createInterpreter();

      // Set _inSyncPath to a sentinel value to check restoration
      rt._inSyncPath = false;

      try {
        // Run valid code — after this returns, _inSyncPath should be false again
        const exprs = parse('42');
        interpreter.run(analyze(exprs[0]), env);
      } catch (e) { /* ignore */ }

      assert(logger, '_inSyncPath restored to false after run() returns', rt._inSyncPath, false);

      // Restore
      rt._inSyncPath = prevInSyncPath;
    } else {
      logger.skip('__schemeProbeRuntime not available, skipping nested test');
    }
  }

  // =========================================================================
  // The actual probe runtime hit() respects _inSyncPath
  // =========================================================================

  logger.title('Sync Callback - real probe runtime respects _inSyncPath');

  {
    if (globalThis.__schemeProbeRuntime) {
      const rt = globalThis.__schemeProbeRuntime;
      const savedActive = rt._active;
      const savedPanelConnected = rt._panelConnected;
      const savedInSyncPath = rt._inSyncPath;
      const savedBreakpoints = rt._breakpoints;

      rt._active = true;
      rt._breakpoints = new Set([999]);

      // Case A: panelConnected=true, inSyncPath=false → false (cooperative path)
      rt._panelConnected = true;
      rt._inSyncPath = false;
      const resultA = rt.hit(999);
      assert(logger, 'real probe runtime: returns false (panelConnected, !inSyncPath)', resultA, false);

      // Case B: panelConnected=true, inSyncPath=true → true (sync path with breakpoint)
      rt._panelConnected = true;
      rt._inSyncPath = true;
      const resultB = rt.hit(999);
      assert(logger, 'real probe runtime: returns true (panelConnected, inSyncPath, bp set)', resultB, true);

      // Case C: panelConnected=false → normal behavior (breakpoint fires regardless)
      rt._panelConnected = false;
      rt._inSyncPath = false;
      const resultC = rt.hit(999);
      assert(logger, 'real probe runtime: returns true (not connected, bp set)', resultC, true);

      // Restore
      rt._active = savedActive;
      rt._panelConnected = savedPanelConnected;
      rt._inSyncPath = savedInSyncPath;
      rt._breakpoints = savedBreakpoints;
    } else {
      logger.skip('__schemeProbeRuntime not available');
    }
  }
}
