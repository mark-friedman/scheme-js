/**
 * @fileoverview Tests for the __schemeDebug global API.
 *
 * Tests the page-side API that the Chrome DevTools extension uses to
 * inspect Scheme call stacks, variables, and control stepping.
 * Part of Phase 5: DevTools Extension — Scheme Stack Sidebar.
 */

import { assert } from '../../harness/helpers.js';
import { DevToolsDebugIntegration } from '../../../src/debug/devtools/devtools_debug.js';
import { SchemeSourceRegistry } from '../../../src/debug/devtools/source_registry.js';
import { SchemeDebugRuntime } from '../../../src/debug/scheme_debug_runtime.js';
import { createInterpreter } from '../../../src/core/interpreter/index.js';
import { Environment } from '../../../src/core/interpreter/environment.js';
// Side-effect import: installs globalThis.__schemeProbeRuntime
import '../../../src/debug/devtools/probe_runtime.js';

/**
 * Creates a test interpreter with DevTools integration and the
 * __schemeDebug API installed.
 *
 * @returns {{interpreter, env, debugRuntime, devtools, registry}}
 */
function createTestSetup() {
    const { interpreter, env } = createInterpreter();
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();

    // Create and attach debug runtime (createInterpreter doesn't set one up)
    const debugRuntime = new SchemeDebugRuntime();
    debugRuntime.enable();
    interpreter.setDebugRuntime(debugRuntime);
    debugRuntime.setDevToolsIntegration(devtools);
    interpreter.devtoolsDebug = devtools;

    // Install the __schemeDebug API
    devtools.installSchemeDebugAPI(interpreter);

    return { interpreter, env, debugRuntime, devtools, registry };
}

/**
 * Runs all __schemeDebug API tests.
 * @param {Object} logger - Test logger
 */
export async function runSchemeDebugApiTests(logger) {

    // =========================================================================
    // getStack — empty stack
    // =========================================================================
    logger.title('__schemeDebug API - getStack');

    // Test: getStack returns empty array when no frames
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const stack = api.getStack();
        assert(logger, 'getStack returns array', Array.isArray(stack), true);
        assert(logger, 'empty stack has 0 frames', stack.length, 0);
    }

    // Test: getStack returns frames after enterFrame
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const env1 = new Environment();
        debugRuntime.enterFrame({
            name: 'main_42',
            originalName: 'main',
            source: { filename: 'scheme://app/test.scm', line: 1, column: 1 },
            env: env1
        });

        const env2 = new Environment();
        debugRuntime.enterFrame({
            name: 'factorial_99',
            originalName: 'factorial',
            source: { filename: 'scheme://app/test.scm', line: 5, column: 3 },
            env: env2
        });

        const stack = api.getStack();
        assert(logger, 'stack has 2 frames', stack.length, 2);

        // Frames are bottom-to-top (index 0 = bottom)
        assert(logger, 'frame 0 name is main', stack[0].name, 'main');
        assert(logger, 'frame 0 source line', stack[0].source.line, 1);
        assert(logger, 'frame 1 name is factorial', stack[1].name, 'factorial');
        assert(logger, 'frame 1 source line', stack[1].source.line, 5);

        // tcoCount should be 0
        assert(logger, 'frame 0 tcoCount', stack[0].tcoCount, 0);
        assert(logger, 'frame 1 tcoCount', stack[1].tcoCount, 0);

        debugRuntime.exitFrame();
        debugRuntime.exitFrame();
    }

    // Test: getStack reflects TCO replacement
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        debugRuntime.enterFrame({
            name: 'loop',
            originalName: 'loop',
            source: { filename: 'scheme://app/test.scm', line: 10, column: 1 },
            env: new Environment()
        });

        debugRuntime.replaceFrame({
            name: 'loop_tail',
            originalName: 'loop',
            source: { filename: 'scheme://app/test.scm', line: 10, column: 1 },
            env: new Environment()
        });

        debugRuntime.replaceFrame({
            name: 'loop_tail2',
            originalName: 'loop',
            source: { filename: 'scheme://app/test.scm', line: 10, column: 1 },
            env: new Environment()
        });

        const stack = api.getStack();
        assert(logger, 'TCO: stack has 1 frame', stack.length, 1);
        assert(logger, 'TCO: frame has tcoCount 2', stack[0].tcoCount, 2);

        debugRuntime.exitFrame();
    }

    // =========================================================================
    // getLocals — variable inspection
    // =========================================================================
    logger.title('__schemeDebug API - getLocals');

    // Test: getLocals returns bindings for a frame
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const env = new Environment();
        env.define('n', 5);
        env.define('result', 120);

        debugRuntime.enterFrame({
            name: 'factorial',
            originalName: 'factorial',
            source: null,
            env
        });

        const locals = api.getLocals(0);
        assert(logger, 'getLocals returns array', Array.isArray(locals), true);
        assert(logger, 'locals has 2 entries', locals.length, 2);

        // Find the 'n' entry
        const nEntry = locals.find(l => l.name === 'n');
        assert(logger, 'n entry exists', !!nEntry, true);
        assert(logger, 'n value is "5"', nEntry.value, '5.0');
        assert(logger, 'n type is number', nEntry.type, 'number');

        // Find the 'result' entry
        const resultEntry = locals.find(l => l.name === 'result');
        assert(logger, 'result entry exists', !!resultEntry, true);
        assert(logger, 'result value is "120"', resultEntry.value, '120.0');

        debugRuntime.exitFrame();
    }

    // Test: getLocals with invalid index returns empty
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const locals = api.getLocals(-1);
        assert(logger, 'invalid index returns empty', locals.length, 0);

        const locals2 = api.getLocals(999);
        assert(logger, 'out of range returns empty', locals2.length, 0);
    }

    // Test: getLocals for specific frame in multi-frame stack
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const env0 = new Environment();
        env0.define('x', 10);
        debugRuntime.enterFrame({
            name: 'outer', originalName: 'outer', source: null, env: env0
        });

        const env1 = new Environment();
        env1.define('y', 20);
        debugRuntime.enterFrame({
            name: 'inner', originalName: 'inner', source: null, env: env1
        });

        // Frame 0 (bottom) should have x
        const locals0 = api.getLocals(0);
        const x = locals0.find(l => l.name === 'x');
        assert(logger, 'frame 0 has x', !!x, true);
        assert(logger, 'frame 0 x value', x.value, '10.0');

        // Frame 1 (top) should have y
        const locals1 = api.getLocals(1);
        const y = locals1.find(l => l.name === 'y');
        assert(logger, 'frame 1 has y', !!y, true);
        assert(logger, 'frame 1 y value', y.value, '20.0');

        debugRuntime.exitFrame();
        debugRuntime.exitFrame();
    }

    // =========================================================================
    // getSource — source location
    // =========================================================================
    logger.title('__schemeDebug API - getSource');

    // Test: getSource returns source info for a frame
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const source = { filename: 'scheme://app/factorial.scm', line: 3, column: 5 };
        debugRuntime.enterFrame({
            name: 'factorial', originalName: 'factorial', source, env: new Environment()
        });

        const result = api.getSource(0);
        assert(logger, 'getSource returns object', typeof result, 'object');
        assert(logger, 'source filename', result.filename, 'scheme://app/factorial.scm');
        assert(logger, 'source line', result.line, 3);
        assert(logger, 'source column', result.column, 5);

        debugRuntime.exitFrame();
    }

    // Test: getSource with null source
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        debugRuntime.enterFrame({
            name: 'anon', originalName: 'anon', source: null, env: new Environment()
        });

        const result = api.getSource(0);
        assert(logger, 'null source returns null', result, null);

        debugRuntime.exitFrame();
    }

    // Test: getSource with invalid index
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const result = api.getSource(999);
        assert(logger, 'invalid index returns null', result, null);
    }

    // =========================================================================
    // eval — expression evaluation in frame context
    // =========================================================================
    logger.title('__schemeDebug API - eval');

    // Test: eval in frame context
    {
        const { interpreter, debugRuntime } = createTestSetup();

        const env = new Environment(interpreter.globalEnv);
        env.define('n', 5);
        debugRuntime.enterFrame({
            name: 'test', originalName: 'test', source: null, env
        });

        const api = globalThis.__schemeDebug;
        const result = api.eval('(+ n 1)');
        assert(logger, 'eval (+ n 1) = "6"', result, '6.0');

        debugRuntime.exitFrame();
    }

    // Test: eval defaults to top frame
    {
        const { interpreter, debugRuntime } = createTestSetup();

        const bottomEnv = new Environment(interpreter.globalEnv);
        bottomEnv.define('x', 100);
        debugRuntime.enterFrame({
            name: 'bottom', originalName: 'bottom', source: null, env: bottomEnv
        });

        const topEnv = new Environment(interpreter.globalEnv);
        topEnv.define('x', 999);
        debugRuntime.enterFrame({
            name: 'top', originalName: 'top', source: null, env: topEnv
        });

        const api = globalThis.__schemeDebug;
        // Default (no frameIndex) should use top frame
        const result = api.eval('x');
        assert(logger, 'eval defaults to top frame', result, '999.0');

        // Explicit frame 0 (bottom)
        const result0 = api.eval('x', 0);
        assert(logger, 'eval in frame 0', result0, '100.0');

        debugRuntime.exitFrame();
        debugRuntime.exitFrame();
    }

    // Test: eval with error returns error string
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const result = api.eval('(error "test error")');
        assert(logger, 'eval error starts with #<error:', result.startsWith('#<error:'), true);
    }

    // Test: eval with invalid frame index falls back to global env
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const result = api.eval('(+ 1 2)', 999);
        assert(logger, 'eval with invalid frame falls back to global env', result, '3.0');
    }

    // =========================================================================
    // Stepping delegation (cooperative, via debugRuntime PauseController)
    // =========================================================================
    logger.title('__schemeDebug API - Stepping');

    // Test: stepInto delegates to debugRuntime, transitions state to 'stepping'
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        // Must be paused first for stepInto to make sense
        debugRuntime.pauseController.pause('test');
        api.stepInto();

        assert(logger, 'stepInto transitions to stepping state',
            debugRuntime.pauseController.getState(), 'stepping');
        assert(logger, 'stepInto sets stepMode to into',
            debugRuntime.pauseController.getStepMode(), 'into');

        debugRuntime.pauseController.reset();
    }

    // Test: stepOver delegates to debugRuntime, transitions state to 'stepping'
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        debugRuntime.pauseController.pause('test');
        api.stepOver();

        assert(logger, 'stepOver transitions to stepping state',
            debugRuntime.pauseController.getState(), 'stepping');
        assert(logger, 'stepOver sets stepMode to over',
            debugRuntime.pauseController.getStepMode(), 'over');

        debugRuntime.pauseController.reset();
    }

    // Test: stepOut delegates to debugRuntime, transitions state to 'stepping'
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        debugRuntime.pauseController.pause('test');
        api.stepOut();

        assert(logger, 'stepOut transitions to stepping state',
            debugRuntime.pauseController.getState(), 'stepping');
        assert(logger, 'stepOut sets stepMode to out',
            debugRuntime.pauseController.getStepMode(), 'out');

        debugRuntime.pauseController.reset();
    }

    // =========================================================================
    // API availability
    // =========================================================================
    logger.title('__schemeDebug API - Global Installation');

    // Test: __schemeDebug is installed on globalThis
    {
        createTestSetup();
        assert(logger, '__schemeDebug exists on globalThis',
            typeof globalThis.__schemeDebug, 'object');
        assert(logger, 'getStack is a function',
            typeof globalThis.__schemeDebug.getStack, 'function');
        assert(logger, 'getLocals is a function',
            typeof globalThis.__schemeDebug.getLocals, 'function');
        assert(logger, 'getSource is a function',
            typeof globalThis.__schemeDebug.getSource, 'function');
        assert(logger, 'eval is a function',
            typeof globalThis.__schemeDebug.eval, 'function');
        assert(logger, 'stepInto is a function',
            typeof globalThis.__schemeDebug.stepInto, 'function');
        assert(logger, 'stepOver is a function',
            typeof globalThis.__schemeDebug.stepOver, 'function');
        assert(logger, 'stepOut is a function',
            typeof globalThis.__schemeDebug.stepOut, 'function');
        assert(logger, 'getSources is a function',
            typeof globalThis.__schemeDebug.getSources, 'function');
        assert(logger, 'getSourceContent is a function',
            typeof globalThis.__schemeDebug.getSourceContent, 'function');
        assert(logger, 'activate is a function',
            typeof globalThis.__schemeDebug.activate, 'function');
        assert(logger, 'setBreakpoint is a function',
            typeof globalThis.__schemeDebug.setBreakpoint, 'function');
        assert(logger, 'removeBreakpoint is a function',
            typeof globalThis.__schemeDebug.removeBreakpoint, 'function');
        assert(logger, 'getAllBreakpoints is a function',
            typeof globalThis.__schemeDebug.getAllBreakpoints, 'function');
        assert(logger, 'getCurrentLocation is a function',
            typeof globalThis.__schemeDebug.getCurrentLocation, 'function');
        assert(logger, 'getStatus is a function',
            typeof globalThis.__schemeDebug.getStatus, 'function');
        assert(logger, 'resume is a function',
            typeof globalThis.__schemeDebug.resume, 'function');
    }

    // =========================================================================
    // getSources — source list
    // =========================================================================
    logger.title('__schemeDebug API - getSources');

    // Test: getSources returns empty array when no sources registered
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const sources = api.getSources();
        assert(logger, 'getSources returns array', Array.isArray(sources), true);
        assert(logger, 'empty registry yields empty list', sources.length, 0);
    }

    // Test: getSources returns registered sources
    {
        const { registry } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const code1 = '(define x 1)';
        const code2 = '(define y 2)\n(define z 3)';
        const url1 = 'scheme://scheme-sources/a.scm';
        const url2 = 'scheme://inline-scripts/script-0.scm';

        const exprs1 = (await import('../../../src/core/interpreter/reader.js'))
            .parse(code1, { filename: url1 });
        const exprs2 = (await import('../../../src/core/interpreter/reader.js'))
            .parse(code2, { filename: url2 });

        registry.register(url1, code1, 'external', exprs1);
        registry.register(url2, code2, 'inline', exprs2);

        const sources = api.getSources();
        assert(logger, 'getSources returns 2 sources', sources.length, 2);

        const s1 = sources.find(s => s.url === url1);
        assert(logger, 'source 1 url', !!s1, true);
        assert(logger, 'source 1 content', s1.content, code1);
        assert(logger, 'source 1 origin', s1.origin, 'external');
        assert(logger, 'source 1 lines', s1.lines, 1);

        const s2 = sources.find(s => s.url === url2);
        assert(logger, 'source 2 url', !!s2, true);
        assert(logger, 'source 2 origin', s2.origin, 'inline');
        assert(logger, 'source 2 lines', s2.lines, 2);
    }

    // =========================================================================
    // getSourceContent — fetch content by URL
    // =========================================================================
    logger.title('__schemeDebug API - getSourceContent');

    // Test: getSourceContent returns content for a registered URL
    {
        const { registry } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const code = '(define answer 42)';
        const url = 'scheme://scheme-sources/answer.scm';
        const exprs = (await import('../../../src/core/interpreter/reader.js'))
            .parse(code, { filename: url });
        registry.register(url, code, 'external', exprs);

        const content = api.getSourceContent(url);
        assert(logger, 'getSourceContent returns string', typeof content, 'string');
        assert(logger, 'getSourceContent returns correct content', content, code);
    }

    // Test: getSourceContent returns null for unknown URL
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const content = api.getSourceContent('scheme://nonexistent/x.scm');
        assert(logger, 'unknown URL returns null', content, null);
    }

    // Test: getSourceContent for inline script
    {
        const { registry } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const code = '(+ 1 2)';
        const url = 'scheme://inline-scripts/script-0.scm';
        const exprs = (await import('../../../src/core/interpreter/reader.js'))
            .parse(code, { filename: url });
        registry.register(url, code, 'inline', exprs);

        const content = api.getSourceContent(url);
        assert(logger, 'inline source content retrieved', content, code);
    }

    // =========================================================================
    // activate — enables debug mode and returns status
    // =========================================================================
    logger.title('__schemeDebug API - activate');

    // Test: activate returns active:true when debugRuntime is enabled
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const result = api.activate();
        assert(logger, 'activate returns object', typeof result, 'object');
        assert(logger, 'activate returns active:true when enabled', result.active, true);
        assert(logger, 'activate needsReload:false when already active', result.needsReload, false);
    }

    // Test: activate sets globalThis.__SCHEME_JS_DEBUG for next page load
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const prevFlag = globalThis.__SCHEME_JS_DEBUG;
        api.activate();
        assert(logger, 'activate sets __SCHEME_JS_DEBUG', !!globalThis.__SCHEME_JS_DEBUG, true);

        // Restore
        globalThis.__SCHEME_JS_DEBUG = prevFlag;
    }

    // =========================================================================
    // setBreakpoint / removeBreakpoint / getAllBreakpoints
    // =========================================================================
    logger.title('__schemeDebug API - breakpoint management');

    // Test: setBreakpoint returns an ID string
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const id = api.setBreakpoint('scheme://test.scm', 5);
        assert(logger, 'setBreakpoint returns string id', typeof id, 'string');
        assert(logger, 'setBreakpoint id starts with bp-', id.startsWith('bp-'), true);

        debugRuntime.breakpointManager.clearAll();
    }

    // Test: setBreakpoint adds to breakpointManager
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        api.setBreakpoint('scheme://test.scm', 10, 3);
        const bps = debugRuntime.breakpointManager.getAllBreakpoints();
        assert(logger, 'breakpoint was added to manager', bps.length, 1);
        assert(logger, 'breakpoint has correct filename', bps[0].filename, 'scheme://test.scm');
        assert(logger, 'breakpoint has correct line', bps[0].line, 10);
        assert(logger, 'breakpoint has correct column', bps[0].column, 3);

        debugRuntime.breakpointManager.clearAll();
    }

    // Test: removeBreakpoint removes from breakpointManager
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const id = api.setBreakpoint('scheme://test.scm', 5);
        assert(logger, 'before remove: 1 breakpoint', debugRuntime.breakpointManager.hasAny(), true);

        const removed = api.removeBreakpoint(id);
        assert(logger, 'removeBreakpoint returns true', removed, true);
        assert(logger, 'after remove: 0 breakpoints', debugRuntime.breakpointManager.hasAny(), false);
    }

    // Test: removeBreakpoint returns false for unknown ID
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const result = api.removeBreakpoint('bp-nonexistent');
        assert(logger, 'removeBreakpoint unknown returns false', result, false);
    }

    // Test: getAllBreakpoints returns all breakpoints
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        api.setBreakpoint('scheme://a.scm', 1);
        api.setBreakpoint('scheme://b.scm', 2);
        const all = api.getAllBreakpoints();

        assert(logger, 'getAllBreakpoints returns array', Array.isArray(all), true);
        assert(logger, 'getAllBreakpoints returns 2 breakpoints', all.length, 2);

        debugRuntime.breakpointManager.clearAll();
    }

    // =========================================================================
    // getStatus — current debugger state
    // =========================================================================
    logger.title('__schemeDebug API - getStatus');

    // Test: getStatus returns active:true when debugRuntime enabled
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const status = api.getStatus();
        assert(logger, 'getStatus returns object', typeof status, 'object');
        assert(logger, 'getStatus active:true when enabled', status.active, true);
        assert(logger, 'getStatus state is running initially', status.state, 'running');
        assert(logger, 'getStatus reason is null initially', status.reason, null);
    }

    // Test: getStatus reflects paused state
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        debugRuntime.pauseController.pause('breakpoint');
        const status = api.getStatus();

        assert(logger, 'getStatus state is paused', status.state, 'paused');
        assert(logger, 'getStatus reason is breakpoint', status.reason, 'breakpoint');

        debugRuntime.pauseController.reset();
    }

    // =========================================================================
    // getCurrentLocation — current pause source location
    // =========================================================================
    logger.title('__schemeDebug API - getCurrentLocation');

    // Test: getCurrentLocation returns null when not paused
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        // Not paused, stack empty
        const loc = api.getCurrentLocation();
        assert(logger, 'getCurrentLocation null when not paused/empty', loc, null);
    }

    // Test: getCurrentLocation returns top frame source when paused
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        const source = { filename: 'scheme://test.scm', line: 7, column: 2 };
        debugRuntime.enterFrame({
            name: 'foo', originalName: 'foo', source, env: new Environment()
        });
        debugRuntime.pauseController.pause('breakpoint');

        const loc = api.getCurrentLocation();
        assert(logger, 'getCurrentLocation returns source', loc !== null, true);
        assert(logger, 'getCurrentLocation filename', loc?.filename, 'scheme://test.scm');
        assert(logger, 'getCurrentLocation line', loc?.line, 7);

        debugRuntime.exitFrame();
        debugRuntime.pauseController.reset();
    }

    // =========================================================================
    // resume — unpauses execution
    // =========================================================================
    logger.title('__schemeDebug API - resume');

    // Test: resume transitions paused state to running
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        debugRuntime.pauseController.pause('breakpoint');
        assert(logger, 'before resume: state is paused',
            debugRuntime.pauseController.getState(), 'paused');

        api.resume();

        assert(logger, 'after resume: state is running',
            debugRuntime.pauseController.getState(), 'running');
        assert(logger, 'after resume: reason is null',
            debugRuntime.pauseController.getPauseReason(), null);
    }

    // Test: resume when already running is a safe no-op
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        // Should not throw
        api.resume();
        assert(logger, 'resume when running: still running',
            debugRuntime.pauseController.getState(), 'running');
    }

    // =========================================================================
    // Probe runtime wiring — setBreakpoint / removeBreakpoint sync to probe runtime
    // =========================================================================
    logger.title('__schemeDebug API - probe runtime breakpoint wiring');

    // Lazy-import parse so we can register real sources with expression spans
    const { parse } = await import('../../../src/core/interpreter/reader.js');

    // Test: setBreakpoint with a registered source wires the exprId to probe runtime
    {
        const { registry, debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        // Reset any residual probe breakpoints from prior tests
        globalThis.__schemeProbeRuntime._breakpoints.clear();

        const url = 'scheme://inline-scripts/probe-test.scm';
        const code = '(define x 42)\n(define y 99)';
        const exprs = parse(code, { filename: url });
        registry.register(url, code, 'inline', exprs);

        // Verify expression spans exist for the source
        const spans = api.getExpressions(url);
        assert(logger, 'probe test: spans registered', spans.length > 0, true);

        // Set a line-level breakpoint on line 1
        const id = api.setBreakpoint(url, 1);
        assert(logger, 'probe test: setBreakpoint returns id', typeof id, 'string');

        // At least one exprId from line 1 should now be in probe runtime
        const spansOnLine1 = spans.filter(s => s.line === 1);
        assert(logger, 'probe test: spans on line 1', spansOnLine1.length > 0, true);

        const anyWired = spansOnLine1.some(
            s => globalThis.__schemeProbeRuntime._breakpoints.has(s.exprId)
        );
        assert(logger, 'probe test: exprIds wired to probe runtime', anyWired, true);

        debugRuntime.breakpointManager.clearAll();
        globalThis.__schemeProbeRuntime._breakpoints.clear();
    }

    // Test: removeBreakpoint deregisters exprIds from probe runtime
    {
        const { registry, debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        globalThis.__schemeProbeRuntime._breakpoints.clear();

        const url = 'scheme://inline-scripts/probe-remove-test.scm';
        const code = '(define a 1)';
        const exprs = parse(code, { filename: url });
        registry.register(url, code, 'inline', exprs);

        const id = api.setBreakpoint(url, 1);
        const spans = api.getExpressions(url);
        const spansOnLine1 = spans.filter(s => s.line === 1);

        // Should be wired after set
        const wiredAfterSet = spansOnLine1.some(
            s => globalThis.__schemeProbeRuntime._breakpoints.has(s.exprId)
        );
        assert(logger, 'probe remove test: wired after set', wiredAfterSet, true);

        // Remove and verify unwired
        api.removeBreakpoint(id);
        const wiredAfterRemove = spansOnLine1.some(
            s => globalThis.__schemeProbeRuntime._breakpoints.has(s.exprId)
        );
        assert(logger, 'probe remove test: unwired after remove', wiredAfterRemove, false);

        debugRuntime.breakpointManager.clearAll();
        globalThis.__schemeProbeRuntime._breakpoints.clear();
    }

    // Test: setBreakpoint for unregistered source still registers with breakpointManager
    {
        const { debugRuntime } = createTestSetup();
        const api = globalThis.__schemeDebug;

        globalThis.__schemeProbeRuntime._breakpoints.clear();

        // No source registered for this URL
        const id = api.setBreakpoint('scheme://missing/file.scm', 5);
        assert(logger, 'unregistered source: id returned', typeof id, 'string');

        // BreakpointManager should have it
        const bps = debugRuntime.breakpointManager.getAllBreakpoints();
        assert(logger, 'unregistered source: in breakpointManager', bps.length, 1);

        // No probe exprIds should be set (no spans available)
        assert(logger, 'unregistered source: no probe wiring', globalThis.__schemeProbeRuntime._breakpoints.size, 0);

        debugRuntime.breakpointManager.clearAll();
    }

    // Clean up global
    delete globalThis.__schemeDebug;
}
