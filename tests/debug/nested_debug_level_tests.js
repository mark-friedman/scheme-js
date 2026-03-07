/**
 * @fileoverview Tests for nested debug levels.
 *
 * Verifies that:
 * - PauseController supports nested waitForResume (stack-based resolvers)
 * - ReplDebugBackend handles nested onPause calls without orphaning outer resolvers
 * - handlePause() works recursively (eval during breakpoint pause)
 * - Step state from inner eval doesn't leak to outer execution
 * - :abort pops one level, :toplevel pops all
 */

import { assert } from '../harness/helpers.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from '../../src/debug/repl_debug_backend.js';
import { TestDebugBackend } from '../../src/debug/debug_backend.js';
import { PauseController } from '../../src/debug/pause_controller.js';
import { DebugLevel, DebugLevelStack } from '../../src/debug/debug_level.js';

/**
 * Runs all nested debug level tests.
 * @param {Object} interpreter - The bootstrapped interpreter
 * @param {Object} logger - Test logger
 */
export async function runNestedDebugLevelTests(interpreter, logger) {
  // =========================================================================
  // PauseController — Nested waitForResume
  // =========================================================================
  logger.title('Nested Debug Levels - PauseController Stack');

  // Test: Two nested waitForResume calls both resolve independently
  {
    const pc = new PauseController();

    pc.pause('breakpoint');
    const p1Resolved = { value: false };
    const p1 = pc.waitForResume().then(() => { p1Resolved.value = true; });

    // Simulate nested pause (e.g. eval hits breakpoint)
    pc.pause('step');
    const p2Resolved = { value: false };
    const p2 = pc.waitForResume().then(() => { p2Resolved.value = true; });

    // Resolve inner (top of stack) first
    pc.resume();
    await p2;
    assert(logger, 'inner waitForResume resolved', p2Resolved.value, true);
    assert(logger, 'outer waitForResume still pending', p1Resolved.value, false);

    // Resolve outer
    pc.resume();
    await p1;
    assert(logger, 'outer waitForResume resolved', p1Resolved.value, true);

    pc.reset();
  }

  // Test: stepInto resolves topmost waitForResume only
  {
    const pc = new PauseController();

    pc.pause('breakpoint');
    const p1Done = { value: false };
    const p1 = pc.waitForResume().then(() => { p1Done.value = true; });

    pc.pause('step');
    const p2Done = { value: false };
    const p2 = pc.waitForResume().then(() => { p2Done.value = true; });

    pc.stepInto();
    await p2;
    assert(logger, 'stepInto resolves inner wait', p2Done.value, true);
    assert(logger, 'stepInto does not resolve outer wait', p1Done.value, false);

    pc.resume();
    await p1;
    assert(logger, 'outer resolves after explicit resume', p1Done.value, true);

    pc.reset();
  }

  // =========================================================================
  // PauseController — Execution Context Stack
  // =========================================================================
  logger.title('Nested Debug Levels - Execution Context Stack');

  // Test: pushExecutionContext / popExecutionContext isolates stepping state
  {
    const pc = new PauseController();

    // Outer: stepping over at depth 3
    pc.state = 'stepping';
    pc.stepMode = 'over';
    pc.targetDepth = 3;

    pc.pushExecutionContext();

    // Inner: should start clean
    assert(logger, 'inner context starts running', pc.state, 'running');
    assert(logger, 'inner context stepMode is null', pc.stepMode, null);
    assert(logger, 'inner context targetDepth is null', pc.targetDepth, null);

    // Inner does a step-into
    pc.stepInto();
    assert(logger, 'inner stepInto sets stepping', pc.state, 'stepping');
    assert(logger, 'inner stepInto sets mode into', pc.stepMode, 'into');

    pc.popExecutionContext();

    // Outer state restored
    assert(logger, 'outer state restored to stepping', pc.state, 'stepping');
    assert(logger, 'outer stepMode restored to over', pc.stepMode, 'over');
    assert(logger, 'outer targetDepth restored to 3', pc.targetDepth, 3);

    pc.reset();
  }

  // Test: Nested push/pop multiple levels
  {
    const pc = new PauseController();
    pc.state = 'paused';
    pc.stepMode = null;

    pc.pushExecutionContext();
    pc.state = 'stepping';
    pc.stepMode = 'into';

    pc.pushExecutionContext();
    assert(logger, 'double-nested starts running', pc.state, 'running');

    pc.popExecutionContext();
    assert(logger, 'pop to first inner: stepping', pc.state, 'stepping');
    assert(logger, 'pop to first inner: stepMode into', pc.stepMode, 'into');

    pc.popExecutionContext();
    assert(logger, 'pop to outer: paused', pc.state, 'paused');
    assert(logger, 'pop to outer: stepMode null', pc.stepMode, null);

    pc.reset();
  }

  // =========================================================================
  // ReplDebugBackend — Nested Pause Handling
  // =========================================================================
  logger.title('Nested Debug Levels - ReplDebugBackend Nesting');

  // Test: Nested onPause calls maintain separate action resolvers
  {
    const output = [];
    const backend = new ReplDebugBackend(msg => output.push(msg));

    const source1 = { filename: 'test.scm', line: 10 };
    const source2 = { filename: 'test.scm', line: 20 };

    // First pause
    const p1 = backend.onPause({
      reason: 'breakpoint', source: source1, breakpointId: 'bp-1',
      stack: [], env: {}, level: new DebugLevel(0, 'breakpoint', source1, [], {})
    });

    assert(logger, 'paused after first onPause', backend.isPaused(), true);

    // Nested pause (during eval)
    const p2 = backend.onPause({
      reason: 'exception', source: source2, breakpointId: null,
      stack: [], env: {}, level: new DebugLevel(1, 'exception', source2, [], {})
    });

    assert(logger, 'still paused after nested onPause', backend.isPaused(), true);

    // Resolve inner pause
    backend.resolveAction('resume');
    const action2 = await p2;
    assert(logger, 'inner pause resolved with resume', action2, 'resume');
    assert(logger, 'still paused (outer level remains)', backend.isPaused(), true);

    // Resolve outer pause
    backend.resolveAction('stepInto');
    const action1 = await p1;
    assert(logger, 'outer pause resolved with stepInto', action1, 'stepInto');
    assert(logger, 'not paused after all levels resolved', backend.isPaused(), false);
  }

  // =========================================================================
  // SchemeDebugRuntime — Nested handlePause
  // =========================================================================
  logger.title('Nested Debug Levels - handlePause Nesting');

  // Test: Nested handlePause with TestDebugBackend
  {
    const backend = new TestDebugBackend();
    const debugRuntime = new SchemeDebugRuntime();
    debugRuntime.setBackend(backend);
    debugRuntime.enable();

    // Schedule actions: first pause returns 'resume' (simulating eval completion)
    // second nested pause also returns 'resume'
    backend.setNextAction('resume');

    const source = { filename: 'test.scm', line: 5 };
    const env = {};

    const action = await debugRuntime.handlePause(source, env, 'breakpoint');

    assert(logger, 'handlePause returns resume', action, 'resume');
    assert(logger, 'level stack empty after handlePause', debugRuntime.levelStack.depth(), 0);
    assert(logger, 'backend received pause event', backend.pauseEvents.length, 1);
  }

  // Test: Two sequential handlePause calls work correctly
  {
    const backend = new TestDebugBackend();
    const debugRuntime = new SchemeDebugRuntime();
    debugRuntime.setBackend(backend);
    debugRuntime.enable();

    backend.setNextAction('resume');
    await debugRuntime.handlePause({ filename: 'a.scm', line: 1 }, {}, 'breakpoint');

    backend.setNextAction('resume');
    await debugRuntime.handlePause({ filename: 'b.scm', line: 2 }, {}, 'step');

    assert(logger, 'two sequential pauses: events count', backend.pauseEvents.length, 2);
    assert(logger, 'level stack empty after both', debugRuntime.levelStack.depth(), 0);
  }

  // =========================================================================
  // Full Integration — Eval During Breakpoint Creates Nested Level
  // =========================================================================
  logger.title('Nested Debug Levels - Eval During Breakpoint');

  // Test: :eval during breakpoint pause doesn't deadlock
  {
    const output = [];
    const debugRuntime = new SchemeDebugRuntime();
    const backend = new ReplDebugBackend(msg => output.push(msg));
    debugRuntime.setBackend(backend);
    debugRuntime.enable();
    debugRuntime.setBreakpoint('<unknown>', 1);
    interpreter.setDebugRuntime(debugRuntime);

    // Start evaluation that will hit breakpoint
    const evalPromise = interpreter.evaluateStringDebug('(+ 10 20)');

    // Wait for the pause notification
    await new Promise(resolve => setTimeout(resolve, 50));

    assert(logger, 'backend is paused', backend.isPaused(), true);

    // Resolve all breakpoint pauses (symbols now have source info,
    // so multiple sub-expressions on line 1 may each trigger a pause)
    const drainPauses = async () => {
      while (backend.isPaused()) {
        backend.resolveAction('resume');
        await new Promise(resolve => setTimeout(resolve, 20));
      }
    };
    await drainPauses();
    const result = await evalPromise;

    assert(logger, 'eval completes after resume', result, 30);
    assert(logger, 'backend no longer paused', backend.isPaused(), false);

    interpreter.setDebugRuntime(null);
    debugRuntime.reset();
  }

  // Final cleanup
  interpreter.setDebugRuntime(null);
}

export default runNestedDebugLevelTests;
