/**
 * @fileoverview Tests for REPL debug session lifecycle.
 *
 * These tests simulate the web/Node.js REPL's evaluate() flow to verify
 * correct state management during pause/resume/continue cycles.
 *
 * Key scenarios tested:
 * - isEvaluating flag integrity during :continue from a paused long-running computation
 * - Pause button (requestPause) working after :continue resumes a computation
 * - Backend pause/resume state consistency with REPL evaluation state
 * - Stale debug state between sessions (snapshotted vs live stack)
 * - Frame selection reset on new pause
 */

import { assert } from '../harness/helpers.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from '../../src/debug/repl_debug_backend.js';
import { ReplDebugCommands } from '../../src/debug/repl_debug_commands.js';
import { PauseController } from '../../src/debug/pause_controller.js';

/**
 * Runs all REPL debug session lifecycle tests.
 * @param {Object} interpreter - The bootstrapped interpreter
 * @param {Object} logger - Test logger
 */
export async function runReplDebugSessionTests(interpreter, logger) {

  // =========================================================================
  // Helper: Simulate the web REPL's evaluate/debug-command flow
  // =========================================================================

  /**
   * Creates a simulated REPL environment with the same state management
   * logic as web/repl.js evaluate(). Returns an object with methods to
   * simulate user actions and inspect state.
   */
  function createReplSimulation() {
    const output = [];
    const debugRuntime = new SchemeDebugRuntime();
    const backend = new ReplDebugBackend(msg => output.push(msg));
    debugRuntime.setBackend(backend);
    debugRuntime.enable();

    const commands = new ReplDebugCommands(interpreter, debugRuntime, backend);
    interpreter.setDebugRuntime(debugRuntime);

    let isEvaluating = false;
    let promptVisible = true;
    let pauseBtnVisible = false;

    /**
     * Simulates the onResume handler from web/repl.js.
     * This is patched onto the backend like the web REPL does.
     */
    const originalOnResume = backend.onResume.bind(backend);
    backend.onResume = () => {
      originalOnResume();
      if (isEvaluating) {
        promptVisible = false;
        pauseBtnVisible = true;
      }
    };

    /**
     * Simulates the onPause callback from web/repl.js.
     */
    backend.setOnPause(() => {
      promptVisible = true;
      pauseBtnVisible = false;
    });

    return {
      debugRuntime,
      backend,
      commands,
      output,
      /** Gets whether the REPL thinks it's evaluating */
      get isEvaluating() { return isEvaluating; },
      set isEvaluating(v) { isEvaluating = v; },
      get promptVisible() { return promptVisible; },
      set promptVisible(v) { promptVisible = v; },
      get pauseBtnVisible() { return pauseBtnVisible; },
      set pauseBtnVisible(v) { pauseBtnVisible = v; },

      /**
       * Simulates the CORRECTED evaluate() from web/repl.js for
       * debug commands entered while paused. Does NOT touch isEvaluating.
       */
      async handleDebugInput(code) {
        if (code.startsWith(':')) {
          const result = await commands.execute(code);
          return result;
        } else {
          const result = await commands.handleEval(code);
          return result;
        }
      },

      /**
       * Simulates the BUGGY (old) evaluate() from web/repl.js.
       * The old code ran debug commands through the same finally block
       * that reset isEvaluating.
       */
      async handleDebugInputBuggy(code) {
        isEvaluating = true;
        promptVisible = false;
        pauseBtnVisible = true;
        try {
          if (code.startsWith(':')) {
            return await commands.execute(code);
          } else {
            return await commands.handleEval(code);
          }
        } finally {
          isEvaluating = false;
          promptVisible = true;
          pauseBtnVisible = false;
        }
      },

      /**
       * Simulates starting a top-level evaluation (long-running computation).
       * Sets up UI state as evaluate() does.
       */
      startEvaluation() {
        isEvaluating = true;
        promptVisible = false;
        pauseBtnVisible = true;
      },

      /**
       * Simulates the finally block of evaluate() completing.
       */
      finishEvaluation() {
        isEvaluating = false;
        if (!backend.isPaused()) {
          promptVisible = true;
          pauseBtnVisible = false;
        }
      },

      cleanup() {
        interpreter.setDebugRuntime(null);
        debugRuntime.reset();
      }
    };
  }

  // =========================================================================
  // Test: :continue should not clobber isEvaluating
  // =========================================================================
  logger.title('REPL Debug Session - isEvaluating Integrity');

  // Test: After manual pause and :continue, isEvaluating stays true
  {
    const repl = createReplSimulation();

    // Simulate: user starts a long-running computation
    repl.startEvaluation();
    assert(logger, 'isEvaluating true after starting eval', repl.isEvaluating, true);
    assert(logger, 'prompt hidden during eval', repl.promptVisible, false);
    assert(logger, 'pause button visible during eval', repl.pauseBtnVisible, true);

    // Simulate: manual pause triggers (via requestPause → handlePause → backend.onPause)
    const pausePromise = repl.backend.onPause({
      reason: 'manual',
      source: { filename: '<repl>', line: 1 },
      breakpointId: null,
      stack: [],
      env: {},
    });

    // onPause callback fires
    assert(logger, 'prompt visible when paused', repl.promptVisible, true);
    assert(logger, 'pause button hidden when paused', repl.pauseBtnVisible, false);
    assert(logger, 'backend reports paused', repl.backend.isPaused(), true);

    // User types :continue — using the CORRECT handler (doesn't touch isEvaluating)
    const result = await repl.handleDebugInput(':c');
    assert(logger, ':continue output', result, ';; Continuing...');

    // isEvaluating should STILL be true (the original computation is still running)
    assert(logger, 'isEvaluating still true after :continue', repl.isEvaluating, true);

    // The action resolver was popped, so backend is no longer paused
    assert(logger, 'backend no longer paused after :continue', repl.backend.isPaused(), false);

    // The onResume callback should have hidden the prompt and shown the pause button
    // (because isEvaluating is still true)
    const resumeAction = await pausePromise;
    assert(logger, 'pause promise resolved with resume', resumeAction, 'resume');

    // Now the computation finishes
    repl.finishEvaluation();
    assert(logger, 'isEvaluating false after computation finishes', repl.isEvaluating, false);
    assert(logger, 'prompt visible after computation finishes', repl.promptVisible, true);
    assert(logger, 'pause button hidden after computation finishes', repl.pauseBtnVisible, false);

    repl.cleanup();
  }

  // Test: The OLD buggy handler WOULD clobber isEvaluating
  {
    const repl = createReplSimulation();

    repl.startEvaluation();

    // Simulate pause
    repl.backend.onPause({
      reason: 'manual',
      source: { filename: '<repl>', line: 1 },
      breakpointId: null,
      stack: [],
      env: {},
    });

    // Use the BUGGY handler that runs :c through the same finally block
    const result = await repl.handleDebugInputBuggy(':c');
    assert(logger, 'buggy :continue output', result, ';; Continuing...');

    // This demonstrates the bug: isEvaluating is now false when it should be true
    assert(logger, 'buggy handler clobbers isEvaluating to false', repl.isEvaluating, false);

    repl.cleanup();
  }

  // =========================================================================
  // Test: requestPause works after :continue
  // =========================================================================
  logger.title('REPL Debug Session - requestPause After Continue');

  // Test: requestPause can trigger a second pause after continuing
  {
    const repl = createReplSimulation();
    repl.startEvaluation();

    // First pause
    const p1 = repl.backend.onPause({
      reason: 'manual',
      source: null,
      breakpointId: null,
      stack: [],
      env: {},
    });

    assert(logger, 'paused after first requestPause', repl.backend.isPaused(), true);

    // Continue
    await repl.handleDebugInput(':c');
    const action1 = await p1;
    assert(logger, 'first pause resolved', action1, 'resume');
    assert(logger, 'not paused after continue', repl.backend.isPaused(), false);

    // Second pause (simulating another requestPause during resumed computation)
    const p2 = repl.backend.onPause({
      reason: 'manual',
      source: null,
      breakpointId: null,
      stack: [],
      env: {},
    });

    assert(logger, 'paused after second requestPause', repl.backend.isPaused(), true);
    assert(logger, 'isEvaluating still true through second pause', repl.isEvaluating, true);

    // Continue again
    await repl.handleDebugInput(':c');
    const action2 = await p2;
    assert(logger, 'second pause resolved', action2, 'resume');

    repl.finishEvaluation();
    repl.cleanup();
  }

  // =========================================================================
  // Test: Debug commands while paused don't allow normal expressions
  // =========================================================================
  logger.title('REPL Debug Session - Input Gating While Paused');

  // Test: Non-command input while paused goes to eval-in-scope, not top-level
  {
    const repl = createReplSimulation();
    repl.startEvaluation();

    // Simulate pause with a mock stack frame that has a local binding
    const mockEnv = interpreter.globalEnv.extend();
    mockEnv.define('paused-var', 99n);

    repl.debugRuntime.stackTracer.enterFrame({
      name: 'test-proc',
      source: { filename: 'test.scm', line: 10 },
      env: mockEnv
    });

    repl.backend.onPause({
      reason: 'breakpoint',
      source: { filename: 'test.scm', line: 10 },
      breakpointId: null,
      stack: repl.debugRuntime.getStack(),
      env: mockEnv,
    });

    // Disable break-on-exception so eval errors don't create nested levels
    repl.debugRuntime.exceptionHandler.breakOnUncaughtException = false;

    // Non-command input while paused should be eval-in-scope
    const evalResult = await repl.handleDebugInput('(+ 1 2)');
    assert(logger, 'expression while paused goes to eval',
      evalResult.includes('result:'), true);

    // isEvaluating should still be true
    assert(logger, 'isEvaluating unchanged after debug eval', repl.isEvaluating, true);

    // Cleanup
    repl.debugRuntime.exceptionHandler.breakOnUncaughtException = true;
    repl.debugRuntime.stackTracer.clear();
    repl.backend.resolveAction('resume');
    repl.finishEvaluation();
    repl.cleanup();
  }

  // =========================================================================
  // Test: Stepping commands maintain isEvaluating
  // =========================================================================
  logger.title('REPL Debug Session - Step Commands State');

  // Test: :step, :next, :finish don't clobber isEvaluating
  {
    const repl = createReplSimulation();
    repl.startEvaluation();

    for (const cmd of [':s', ':n', ':fin']) {
      // Simulate a pause
      repl.backend.onPause({
        reason: 'step',
        source: { filename: 'test.scm', line: 1 },
        breakpointId: null,
        stack: [],
        env: {},
      });

      await repl.handleDebugInput(cmd);
      assert(logger, `isEvaluating intact after ${cmd}`, repl.isEvaluating, true);
      assert(logger, `backend not paused after ${cmd}`, repl.backend.isPaused(), false);
    }

    repl.finishEvaluation();
    repl.cleanup();
  }

  // =========================================================================
  // Test: :abort during pause correctly terminates
  // =========================================================================
  logger.title('REPL Debug Session - Abort During Pause');

  // Test: :abort resolves the pause with abort action
  {
    const repl = createReplSimulation();
    repl.startEvaluation();

    const pausePromise = repl.backend.onPause({
      reason: 'breakpoint',
      source: { filename: 'test.scm', line: 1 },
      breakpointId: null,
      stack: [],
      env: {},
    });

    await repl.handleDebugInput(':abort');
    const action = await pausePromise;
    assert(logger, ':abort resolves with abort action', action, 'abort');
    assert(logger, 'isEvaluating still true after :abort (eval catches error)',
      repl.isEvaluating, true);

    repl.finishEvaluation();
    repl.cleanup();
  }

  // =========================================================================
  // Test: Stale stack data uses snapshot, not live stack
  // =========================================================================
  logger.title('REPL Debug Session - Snapshotted Stack on Pause');

  // Test: :bt uses the snapshotted stack from pause info, not live stack
  {
    const repl = createReplSimulation();

    // Set up a stack frame before pausing
    repl.debugRuntime.stackTracer.enterFrame({
      name: 'original-fn',
      source: { filename: 'test.scm', line: 5 },
      env: interpreter.globalEnv
    });

    const snapshotStack = repl.debugRuntime.getStack();

    // Pause with snapshotted stack
    repl.backend.onPause({
      reason: 'breakpoint',
      source: { filename: 'test.scm', line: 5 },
      breakpointId: null,
      stack: [...snapshotStack],
      env: {},
    });

    // Mutate the live stack after pausing (simulating continued background work)
    repl.debugRuntime.stackTracer.enterFrame({
      name: 'stale-fn',
      source: { filename: 'other.scm', line: 99 },
      env: interpreter.globalEnv
    });

    const btOutput = await repl.handleDebugInput(':bt');

    // Should show original-fn from the snapshot, not stale-fn from live stack
    assert(logger, 'backtrace shows snapshotted frame',
      btOutput.includes('original-fn'), true);
    assert(logger, 'backtrace does not show stale frame',
      btOutput.includes('stale-fn'), false);

    // Cleanup
    repl.debugRuntime.stackTracer.clear();
    repl.backend.resolveAction('resume');
    repl.cleanup();
  }

  // =========================================================================
  // Test: Frame selection resets on new pause
  // =========================================================================
  logger.title('REPL Debug Session - Frame Selection Reset');

  // Test: :up selection from one pause doesn't carry to the next pause
  {
    const repl = createReplSimulation();

    // First pause with two frames
    repl.debugRuntime.stackTracer.enterFrame({
      name: 'frame-a',
      source: { filename: 'test.scm', line: 1 },
      env: interpreter.globalEnv
    });
    repl.debugRuntime.stackTracer.enterFrame({
      name: 'frame-b',
      source: { filename: 'test.scm', line: 2 },
      env: interpreter.globalEnv
    });

    repl.backend.onPause({
      reason: 'breakpoint',
      source: { filename: 'test.scm', line: 2 },
      breakpointId: null,
      stack: repl.debugRuntime.getStack(),
      env: {},
    });

    // Navigate up
    await repl.handleDebugInput(':up');
    const stack1 = repl.commands._getPausedStack();
    assert(logger, 'first pause: frame moved to 0 after :up',
      repl.commands._getSelectedIndex(stack1), 0);

    // Continue
    repl.backend.resolveAction('resume');
    repl.debugRuntime.stackTracer.clear();
    repl.commands.resetSelection();

    // Second pause with new frames
    repl.debugRuntime.stackTracer.enterFrame({
      name: 'frame-c',
      source: { filename: 'test.scm', line: 10 },
      env: interpreter.globalEnv
    });
    repl.debugRuntime.stackTracer.enterFrame({
      name: 'frame-d',
      source: { filename: 'test.scm', line: 11 },
      env: interpreter.globalEnv
    });

    repl.backend.onPause({
      reason: 'step',
      source: { filename: 'test.scm', line: 11 },
      breakpointId: null,
      stack: repl.debugRuntime.getStack(),
      env: {},
    });

    // Without reset, the selection from the first pause could leak
    const stack2 = repl.commands._getPausedStack();
    assert(logger, 'second pause: frame starts at newest (1)',
      repl.commands._getSelectedIndex(stack2), 1);

    // Cleanup
    repl.debugRuntime.stackTracer.clear();
    repl.backend.resolveAction('resume');
    repl.cleanup();
  }

  // =========================================================================
  // Test: Full pause → continue → pause → continue cycle
  // =========================================================================
  logger.title('REPL Debug Session - Full Pause/Continue Cycle');

  // Test: Complete cycle with real interpreter evaluation
  {
    const output = [];
    const debugRuntime = new SchemeDebugRuntime();
    const backend = new ReplDebugBackend(msg => output.push(msg));
    debugRuntime.setBackend(backend);
    debugRuntime.enable();
    interpreter.setDebugRuntime(debugRuntime);

    let isEvaluating = false;
    let pauseCount = 0;

    // Patch onResume like the web REPL does
    const origResume = backend.onResume.bind(backend);
    backend.onResume = () => {
      origResume();
    };

    backend.setOnPause(() => {
      pauseCount++;
    });

    // Start a computation that will be paused via requestPause
    isEvaluating = true;

    const code = `
      (define (slow-loop n acc)
        (if (<= n 0)
            acc
            (slow-loop (- n 1) (+ acc 1))))
      (slow-loop 100000 0)
    `;

    // Schedule first pause
    setTimeout(() => {
      debugRuntime.pauseController.requestPause();
    }, 5);

    // Schedule continue after first pause is detected
    const pollAndContinue = () => {
      if (backend.isPaused()) {
        // Simulate :continue command
        backend.resolveAction('resume');

        // Schedule second pause
        setTimeout(() => {
          if (!backend.isPaused()) {
            debugRuntime.pauseController.requestPause();
          }
        }, 5);

        // Schedule final continue
        const pollAndFinish = () => {
          if (backend.isPaused()) {
            backend.resolveAction('resume');
          } else {
            setTimeout(pollAndFinish, 5);
          }
        };
        setTimeout(pollAndFinish, 10);
      } else {
        setTimeout(pollAndContinue, 5);
      }
    };
    setTimeout(pollAndContinue, 10);

    const result = await interpreter.evaluateStringDebug(code);
    isEvaluating = false;

    assert(logger, 'computation completes after pause/continue cycles', result, 100000);
    assert(logger, 'at least one pause occurred', pauseCount >= 1, true);
    assert(logger, 'backend not paused at end', backend.isPaused(), false);

    interpreter.setDebugRuntime(null);
    debugRuntime.reset();
  }

  // Final cleanup
  interpreter.setDebugRuntime(null);
}

export default runReplDebugSessionTests;
