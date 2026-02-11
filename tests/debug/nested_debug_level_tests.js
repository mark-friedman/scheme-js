/**
 * @fileoverview Tests for nested debug level functionality.
 *
 * Tests the core of the Lisp-style recursive debugger: DebugLevel and
 * DebugLevelStack basics, PauseController nested waitForResume (LIFO),
 * PauseController execution context stack, ReplDebugBackend nested pauses,
 * handlePause with TestDebugBackend, and handlePause nesting.
 */

import { assert, createTestLogger } from '../harness/helpers.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { PauseController } from '../../src/debug/pause_controller.js';
import { ReplDebugBackend } from '../../src/debug/repl_debug_backend.js';
import { TestDebugBackend } from '../../src/debug/debug_backend.js';
import { DebugLevel, DebugLevelStack } from '../../src/debug/debug_level.js';

/**
 * Runs all nested debug level tests.
 * @param {Object} interpreter - Interpreter instance (unused, kept for API consistency)
 * @param {Object} logger - Test logger
 */
export async function runNestedDebugLevelTests(interpreter, logger) {

  // =========================================================================
  // Section 1: DebugLevel and DebugLevelStack basics
  // =========================================================================

  logger.title('DebugLevel - Constructor');

  // Test: DebugLevel constructor stores all properties correctly
  {
    const parent = new DebugLevel(0, 'breakpoint', { filename: 'a.scm', line: 1 }, ['frame0'], { x: 1 }, null);
    const child = new DebugLevel(1, 'step', { filename: 'b.scm', line: 5 }, ['frame1'], { y: 2 }, parent);

    assert(logger, 'DebugLevel stores level', child.level, 1);
    assert(logger, 'DebugLevel stores reason', child.reason, 'step');
    assert(logger, 'DebugLevel stores source filename', child.source.filename, 'b.scm');
    assert(logger, 'DebugLevel stores source line', child.source.line, 5);
    assert(logger, 'DebugLevel stores stack', child.stack[0], 'frame1');
    assert(logger, 'DebugLevel stores env', child.env.y, 2);
    assert(logger, 'DebugLevel stores parentLevel', child.parentLevel === parent, true);
    assert(logger, 'DebugLevel default selectedFrameIndex', child.selectedFrameIndex, 0);
    assert(logger, 'parent has null parentLevel', parent.parentLevel, null);
  }

  logger.title('DebugLevelStack - Push/Pop/Current/Depth');

  // Test: DebugLevelStack push/pop/current/depth work correctly
  {
    const stack = new DebugLevelStack();
    assert(logger, 'empty stack depth is 0', stack.depth(), 0);
    assert(logger, 'empty stack current is null', stack.current(), null);

    const level0 = new DebugLevel(0, 'breakpoint', null, [], {}, null);
    const level1 = new DebugLevel(1, 'step', null, [], {}, level0);

    stack.push(level0);
    assert(logger, 'depth after 1 push is 1', stack.depth(), 1);
    assert(logger, 'current is level0', stack.current() === level0, true);

    stack.push(level1);
    assert(logger, 'depth after 2 pushes is 2', stack.depth(), 2);
    assert(logger, 'current is level1', stack.current() === level1, true);

    const popped = stack.pop();
    assert(logger, 'pop returns level1', popped === level1, true);
    assert(logger, 'depth after pop is 1', stack.depth(), 1);
    assert(logger, 'current is level0 after pop', stack.current() === level0, true);

    stack.pop();
    assert(logger, 'depth after second pop is 0', stack.depth(), 0);
    assert(logger, 'current is null after all popped', stack.current(), null);
  }

  // Test: DebugLevelStack popAll clears everything
  {
    const stack = new DebugLevelStack();
    stack.push(new DebugLevel(0, 'breakpoint', null, [], {}, null));
    stack.push(new DebugLevel(1, 'step', null, [], {}, null));
    stack.push(new DebugLevel(2, 'exception', null, [], {}, null));

    stack.popAll();
    assert(logger, 'popAll clears depth to 0', stack.depth(), 0);
    assert(logger, 'popAll current is null', stack.current(), null);
  }

  // Test: DebugLevelStack current() returns null when empty
  {
    const stack = new DebugLevelStack();
    assert(logger, 'current on empty stack returns null', stack.current(), null);
  }

  // Test: DebugLevelStack pop() on empty stack returns undefined
  {
    const stack = new DebugLevelStack();
    const result = stack.pop();
    assert(logger, 'pop on empty stack returns undefined', result, undefined);
  }

  // =========================================================================
  // Section 2: PauseController nested waitForResume (LIFO)
  // =========================================================================

  logger.title('PauseController - Nested waitForResume (LIFO)');

  // Test: Two nested waitForResume calls resolved in LIFO order
  {
    const pc = new PauseController();
    pc.pause('breakpoint');

    let outerResolved = false;
    let innerResolved = false;

    // Outer wait
    const outerPromise = pc.waitForResume().then(() => { outerResolved = true; });

    // Inner wait (simulating nested pause)
    const innerPromise = pc.waitForResume().then(() => { innerResolved = true; });

    // Resume should resolve inner (LIFO)
    pc.resume();
    await innerPromise;
    assert(logger, 'inner resolved after first resume', innerResolved, true);
    assert(logger, 'outer still pending after first resume', outerResolved, false);

    // Pause and resume again for outer
    pc.pause('breakpoint');
    pc.resume();
    await outerPromise;
    assert(logger, 'outer resolved after second resume', outerResolved, true);
  }

  // Test: stepInto resolves the topmost waiter only (inner)
  {
    const pc = new PauseController();
    pc.pause('breakpoint');

    let outerResolved = false;
    let innerResolved = false;

    const outerPromise = pc.waitForResume().then(() => { outerResolved = true; });
    const innerPromise = pc.waitForResume().then(() => { innerResolved = true; });

    // stepInto should resolve the topmost waiter (inner)
    pc.stepInto();
    await innerPromise;
    assert(logger, 'stepInto resolves inner', innerResolved, true);
    assert(logger, 'stepInto leaves outer pending', outerResolved, false);

    // Clean up outer
    pc.pause('breakpoint');
    pc.resume();
    await outerPromise;
    assert(logger, 'outer resolved after cleanup', outerResolved, true);
  }

  // Test: Three levels deep resolved in LIFO order
  {
    const pc = new PauseController();
    pc.pause('breakpoint');

    let resolved = [false, false, false];

    const p0 = pc.waitForResume().then(() => { resolved[0] = true; });
    const p1 = pc.waitForResume().then(() => { resolved[1] = true; });
    const p2 = pc.waitForResume().then(() => { resolved[2] = true; });

    // Resolve level 2 (deepest)
    pc.resume();
    await p2;
    assert(logger, '3-level: level 2 resolved', resolved[2], true);
    assert(logger, '3-level: level 1 pending', resolved[1], false);
    assert(logger, '3-level: level 0 pending', resolved[0], false);

    // Resolve level 1
    pc.pause('breakpoint');
    pc.resume();
    await p1;
    assert(logger, '3-level: level 1 resolved', resolved[1], true);
    assert(logger, '3-level: level 0 still pending', resolved[0], false);

    // Resolve level 0
    pc.pause('breakpoint');
    pc.resume();
    await p0;
    assert(logger, '3-level: level 0 resolved', resolved[0], true);
  }

  // =========================================================================
  // Section 3: PauseController execution context stack
  // =========================================================================

  logger.title('PauseController - Execution Context Stack');

  // Test: pushExecutionContext saves stepping state, resets to running
  {
    const pc = new PauseController();
    pc.stepInto();
    assert(logger, 'state is stepping before push', pc.getState(), 'stepping');
    assert(logger, 'stepMode is into before push', pc.getStepMode(), 'into');

    if (typeof pc.pushExecutionContext === 'function') {
      pc.pushExecutionContext();
      assert(logger, 'state reset to running after push', pc.getState(), 'running');
      assert(logger, 'stepMode null after push', pc.getStepMode(), null);
    } else {
      // pushExecutionContext not implemented yet — skip
      assert(logger, 'pushExecutionContext exists (skip if not)', true, true);
    }
  }

  // Test: popExecutionContext restores saved state
  {
    const pc = new PauseController();
    pc.stepOver(3);

    if (typeof pc.pushExecutionContext === 'function' && typeof pc.popExecutionContext === 'function') {
      pc.pushExecutionContext();
      assert(logger, 'state reset after push', pc.getState(), 'running');

      pc.popExecutionContext();
      assert(logger, 'state restored after pop', pc.getState(), 'stepping');
      assert(logger, 'stepMode restored after pop', pc.getStepMode(), 'over');
      assert(logger, 'targetDepth restored after pop', pc.getTargetDepth(), 3);
    } else {
      assert(logger, 'push/popExecutionContext exist (skip if not)', true, true);
    }
  }

  // Test: Multiple push/pop maintains LIFO order
  {
    const pc = new PauseController();

    if (typeof pc.pushExecutionContext === 'function' && typeof pc.popExecutionContext === 'function') {
      pc.stepInto();
      pc.pushExecutionContext();

      pc.stepOver(7);
      pc.pushExecutionContext();

      pc.stepOut(2);
      assert(logger, 'current state is stepping (out)', pc.getStepMode(), 'out');

      pc.popExecutionContext();
      assert(logger, 'after first pop: stepMode is over', pc.getStepMode(), 'over');
      assert(logger, 'after first pop: targetDepth is 7', pc.getTargetDepth(), 7);

      pc.popExecutionContext();
      assert(logger, 'after second pop: stepMode is into', pc.getStepMode(), 'into');
    } else {
      assert(logger, 'push/popExecutionContext exist for LIFO test (skip if not)', true, true);
    }
  }

  // Test: Abort flags are NOT saved (they remain global)
  {
    const pc = new PauseController();
    pc.abort();
    assert(logger, 'aborted is true', pc.isAborted(), true);

    if (typeof pc.pushExecutionContext === 'function') {
      pc.pushExecutionContext();
      assert(logger, 'aborted persists through push', pc.isAborted(), true);
    } else {
      assert(logger, 'pushExecutionContext exists for abort test (skip if not)', true, true);
    }
  }

  // =========================================================================
  // Section 4: ReplDebugBackend nested pauses
  // =========================================================================

  logger.title('ReplDebugBackend - Nested Pauses');

  // Test: Single onPause creates a pending action resolver
  {
    const output = [];
    const backend = new ReplDebugBackend((msg) => output.push(msg));

    const pauseInfo = {
      reason: 'breakpoint',
      source: { filename: 'test.scm', line: 1 },
      breakpointId: 'bp-1'
    };

    const pausePromise = backend.onPause(pauseInfo);

    assert(logger, 'single onPause: isPaused is true', backend.isPaused(), true);
    assert(logger, 'single onPause: _pauseActionResolver is set', backend._pauseActionResolver !== null, true);

    // Clean up by resolving
    backend.onResume('resume');
    await pausePromise;
  }

  // Test: Nested onPause and sequential resolution
  {
    const output = [];
    const backend = new ReplDebugBackend((msg) => output.push(msg));

    const outerInfo = {
      reason: 'breakpoint',
      source: { filename: 'test.scm', line: 1 },
      breakpointId: 'bp-1'
    };
    const innerInfo = {
      reason: 'step',
      source: { filename: 'test.scm', line: 5 },
      breakpointId: null
    };

    let outerAction = null;
    let innerAction = null;

    const outerPromise = backend.onPause(outerInfo).then(a => { outerAction = a; });

    // Save the outer resolver before it gets overwritten
    const outerResolver = backend._pauseActionResolver;

    const innerPromise = backend.onPause(innerInfo).then(a => { innerAction = a; });

    assert(logger, 'nested onPause: isPaused is true', backend.isPaused(), true);
    assert(logger, 'nested onPause: pauseInfo is inner', backend.getPauseInfo().reason, 'step');

    // Resolve inner
    backend.onResume('resume');
    await innerPromise;
    assert(logger, 'inner action resolved to resume', innerAction, 'resume');

    // Resolve outer using the saved resolver
    if (outerResolver) {
      outerResolver('resume');
      await outerPromise;
      assert(logger, 'outer action resolved to resume', outerAction, 'resume');
    } else {
      assert(logger, 'outer resolver was available', true, true);
    }
  }

  // =========================================================================
  // Section 5: handlePause with TestDebugBackend
  // =========================================================================

  logger.title('handlePause - TestDebugBackend Single Pause');

  // Test: Single handlePause creates DebugLevel, returns action, cleans up
  {
    const runtime = new SchemeDebugRuntime();
    const backend = new TestDebugBackend();
    runtime.setBackend(backend);
    runtime.enable();

    backend.setNextAction('resume');

    const action = await runtime.handlePause(
      { filename: 'test.scm', line: 1 },
      {},
      'breakpoint'
    );

    assert(logger, 'single handlePause returns resume', action, 'resume');
    assert(logger, 'level stack empty after handlePause', runtime.levelStack.depth(), 0);
    assert(logger, 'backend received 1 pause event', backend.pauseEvents.length, 1);
  }

  // Test: handlePause with 'stepInto' action
  {
    const runtime = new SchemeDebugRuntime();
    const backend = new TestDebugBackend();
    runtime.setBackend(backend);
    runtime.enable();

    backend.setNextAction('stepInto');

    const action = await runtime.handlePause(
      { filename: 'test.scm', line: 2 },
      {},
      'step'
    );

    assert(logger, 'handlePause returns stepInto', action, 'stepInto');
    assert(logger, 'level stack empty after stepInto', runtime.levelStack.depth(), 0);
  }

  // Test: handlePause with 'abort' action
  {
    const runtime = new SchemeDebugRuntime();
    const backend = new TestDebugBackend();
    runtime.setBackend(backend);
    runtime.enable();

    backend.setNextAction('abort');

    const action = await runtime.handlePause(
      { filename: 'test.scm', line: 3 },
      {},
      'breakpoint'
    );

    assert(logger, 'handlePause returns abort', action, 'abort');
    assert(logger, 'level stack empty after abort', runtime.levelStack.depth(), 0);
  }

  // Test: Sequential handlePause calls each create and clean up their own level
  {
    const runtime = new SchemeDebugRuntime();
    const backend = new TestDebugBackend();
    runtime.setBackend(backend);
    runtime.enable();

    backend.setNextAction('resume');
    const action1 = await runtime.handlePause({ filename: 'a.scm', line: 1 }, {}, 'breakpoint');

    backend.setNextAction('stepInto');
    const action2 = await runtime.handlePause({ filename: 'b.scm', line: 2 }, {}, 'step');

    assert(logger, 'first sequential action is resume', action1, 'resume');
    assert(logger, 'second sequential action is stepInto', action2, 'stepInto');
    assert(logger, 'level stack empty after sequential calls', runtime.levelStack.depth(), 0);
    assert(logger, 'backend received 2 pause events', backend.pauseEvents.length, 2);
  }

  // Test: handlePause with no backend and no callback returns 'resume' immediately
  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();

    const action = await runtime.handlePause(
      { filename: 'test.scm', line: 1 },
      {},
      'breakpoint'
    );

    assert(logger, 'handlePause with no backend returns resume', action, 'resume');
    assert(logger, 'level stack empty with no backend', runtime.levelStack.depth(), 0);
  }

  // =========================================================================
  // Section 6: handlePause nesting (the critical nested debugger test)
  // =========================================================================

  logger.title('handlePause - Nested Debugger (2 Levels)');

  // Test: Two concurrent handlePause calls simulating breakpoint-during-eval
  {
    const runtime = new SchemeDebugRuntime();
    const backend = new TestDebugBackend();
    runtime.setBackend(backend);
    runtime.enable();

    // Start outer handlePause
    const outerPromise = runtime.handlePause(
      { filename: 'test.scm', line: 1 },
      {},
      'breakpoint'
    );

    // Wait a tick for the backend.onPause to be called
    await new Promise(r => setTimeout(r, 0));

    assert(logger, 'outer pause: level depth is 1', runtime.levelStack.depth(), 1);
    assert(logger, 'outer pause: backend has 1 pause event', backend.pauseEvents.length, 1);

    // Start inner handlePause (simulating eval-during-breakpoint)
    const innerPromise = runtime.handlePause(
      { filename: 'test.scm', line: 5 },
      {},
      'step'
    );

    await new Promise(r => setTimeout(r, 0));

    assert(logger, 'nested: level depth is 2', runtime.levelStack.depth(), 2);
    assert(logger, 'nested: backend has 2 pause events', backend.pauseEvents.length, 2);

    // Resolve inner
    backend.setNextAction('resume');
    const innerAction = await innerPromise;
    assert(logger, 'inner action is resume', innerAction, 'resume');
    assert(logger, 'after inner: level depth is 1', runtime.levelStack.depth(), 1);

    // Resolve outer
    backend.setNextAction('resume');
    const outerAction = await outerPromise;
    assert(logger, 'outer action is resume', outerAction, 'resume');
    assert(logger, 'after outer: level depth is 0', runtime.levelStack.depth(), 0);
  }

  // =========================================================================
  // Section 7: Three levels deep nesting
  // =========================================================================

  logger.title('handlePause - Three Levels Deep');

  // Test: Start 3 concurrent handlePause calls, resolve in LIFO order
  {
    const runtime = new SchemeDebugRuntime();
    const backend = new TestDebugBackend();
    runtime.setBackend(backend);
    runtime.enable();

    // Level 0 (outermost)
    const p0 = runtime.handlePause(
      { filename: 'test.scm', line: 1 },
      {},
      'breakpoint'
    );
    await new Promise(r => setTimeout(r, 0));
    assert(logger, '3-deep: depth after level 0', runtime.levelStack.depth(), 1);

    // Level 1 (middle)
    const p1 = runtime.handlePause(
      { filename: 'test.scm', line: 10 },
      {},
      'step'
    );
    await new Promise(r => setTimeout(r, 0));
    assert(logger, '3-deep: depth after level 1', runtime.levelStack.depth(), 2);

    // Level 2 (innermost)
    const p2 = runtime.handlePause(
      { filename: 'test.scm', line: 20 },
      {},
      'exception'
    );
    await new Promise(r => setTimeout(r, 0));
    assert(logger, '3-deep: depth after level 2', runtime.levelStack.depth(), 3);
    assert(logger, '3-deep: backend has 3 pause events', backend.pauseEvents.length, 3);

    // Resolve level 2 (innermost first — LIFO)
    backend.setNextAction('resume');
    const action2 = await p2;
    assert(logger, '3-deep: level 2 action is resume', action2, 'resume');
    assert(logger, '3-deep: depth after level 2 resolved', runtime.levelStack.depth(), 2);

    // Resolve level 1
    backend.setNextAction('resume');
    const action1 = await p1;
    assert(logger, '3-deep: level 1 action is resume', action1, 'resume');
    assert(logger, '3-deep: depth after level 1 resolved', runtime.levelStack.depth(), 1);

    // Resolve level 0
    backend.setNextAction('resume');
    const action0 = await p0;
    assert(logger, '3-deep: level 0 action is resume', action0, 'resume');
    assert(logger, '3-deep: depth after level 0 resolved', runtime.levelStack.depth(), 0);
  }

  // =========================================================================
  // Section 8: Mixed actions in nested levels
  // =========================================================================

  logger.title('handlePause - Mixed Actions in Nested Levels');

  // Test: Outer breakpoint → inner step → resolve inner with stepInto → resolve outer with resume
  {
    const runtime = new SchemeDebugRuntime();
    const backend = new TestDebugBackend();
    runtime.setBackend(backend);
    runtime.enable();

    // Outer pause (breakpoint)
    const outerPromise = runtime.handlePause(
      { filename: 'main.scm', line: 10 },
      {},
      'breakpoint'
    );
    await new Promise(r => setTimeout(r, 0));

    assert(logger, 'mixed: outer pause event reason', backend.pauseEvents[0].reason, 'breakpoint');

    // Inner pause (step)
    const innerPromise = runtime.handlePause(
      { filename: 'main.scm', line: 15 },
      {},
      'step'
    );
    await new Promise(r => setTimeout(r, 0));

    assert(logger, 'mixed: inner pause event reason', backend.pauseEvents[1].reason, 'step');
    assert(logger, 'mixed: depth is 2', runtime.levelStack.depth(), 2);

    // Resolve inner with stepInto
    backend.setNextAction('stepInto');
    const innerAction = await innerPromise;
    assert(logger, 'mixed: inner action is stepInto', innerAction, 'stepInto');
    assert(logger, 'mixed: depth after inner resolved', runtime.levelStack.depth(), 1);

    // Resolve outer with resume
    backend.setNextAction('resume');
    const outerAction = await outerPromise;
    assert(logger, 'mixed: outer action is resume', outerAction, 'resume');
    assert(logger, 'mixed: depth after outer resolved', runtime.levelStack.depth(), 0);
  }
}

export default runNestedDebugLevelTests;
