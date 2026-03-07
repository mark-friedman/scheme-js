/**
 * @fileoverview Integration tests for Phase 2 debug runtime with the interpreter.
 *
 * Tests the runDebug() async trampoline with cooperative yielding and debug
 * pause handling, and the evaluateStringDebug() string-based wrapper.
 *
 * These methods integrate the SchemeDebugRuntime with the interpreter's
 * async execution path, providing breakpoint-aware evaluation with
 * pause/resume/step support.
 */

import { assert, run } from '../harness/helpers.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { DebugLevel, DebugLevelStack } from '../../src/debug/debug_level.js';
import { TestDebugBackend } from '../../src/debug/debug_backend.js';

/**
 * Runs all debug integration tests.
 * @param {Interpreter} interpreter - The bootstrapped interpreter
 * @param {Object} logger - Test logger
 */
export async function runDebugIntegrationTests(interpreter, logger) {
  // Helper: run sync using the interpreter
  const runSync = (code) => run(interpreter, code);

  // Helper: run via the debug-aware async path
  const runDebug = (code) => interpreter.evaluateStringDebug(code);

  // =========================================================================
  // runDebug() Correctness — same results as sync run()
  // =========================================================================
  logger.title('Debug Integration - runDebug() Correctness');

  // Test: Literal number
  {
    const syncResult = runSync('42');
    const debugResult = await runDebug('42');
    assert(logger, 'literal number', debugResult, syncResult);
  }

  // Test: Literal string
  {
    const syncResult = runSync('"hello"');
    const debugResult = await runDebug('"hello"');
    assert(logger, 'literal string', debugResult, syncResult);
  }

  // Test: Literal boolean
  {
    const syncResult = runSync('#t');
    const debugResult = await runDebug('#t');
    assert(logger, 'literal boolean', debugResult, syncResult);
  }

  // Test: Function call
  {
    const syncResult = runSync('(+ 1 2 3)');
    const debugResult = await runDebug('(+ 1 2 3)');
    assert(logger, 'function call (+ 1 2 3)', debugResult, syncResult);
  }

  // Test: Nested expressions
  {
    const syncResult = runSync('(* (+ 1 2) (- 10 5))');
    const debugResult = await runDebug('(* (+ 1 2) (- 10 5))');
    assert(logger, 'nested expressions', debugResult, syncResult);
  }

  // Test: Recursive function (factorial)
  {
    const code = `
      (define (factorial n)
        (if (<= n 1)
            1
            (* n (factorial (- n 1)))))
      (factorial 10)
    `;
    const syncResult = runSync(code);
    const debugResult = await runDebug(code);
    assert(logger, 'recursive factorial', debugResult, syncResult);
  }

  // Test: Tail-recursive function (sum-to)
  {
    const code = `
      (define (sum-to n acc)
        (if (<= n 0)
            acc
            (sum-to (- n 1) (+ acc n))))
      (sum-to 100 0)
    `;
    const syncResult = runSync(code);
    const debugResult = await runDebug(code);
    assert(logger, 'tail-recursive sum', debugResult, syncResult);
  }

  // =========================================================================
  // Breakpoint → Pause → Resume Flow
  // =========================================================================
  logger.title('Debug Integration - Breakpoint Pause/Resume');

  // Test: Breakpoint triggers pause, resume produces correct result
  {
    let pauseInfo = null;
    const debugRuntime = new SchemeDebugRuntime({
      onPause: (info) => {
        pauseInfo = info;
        // Schedule resume after a tick
        setTimeout(() => debugRuntime.resume(), 10);
      }
    });

    debugRuntime.enable();
    debugRuntime.setBreakpoint('<unknown>', 1);
    interpreter.setDebugRuntime(debugRuntime);

    const result = await runDebug('(+ 10 20)');

    assert(logger, 'breakpoint pause was triggered', pauseInfo !== null, true);
    assert(logger, 'pause reason is breakpoint', pauseInfo?.reason, 'breakpoint');
    assert(logger, 'pause has source info', pauseInfo?.source !== null, true);
    assert(logger, 'source filename is <unknown>', pauseInfo?.source?.filename, '<unknown>');
    assert(logger, 'source line is 1', pauseInfo?.source?.line, 1);
    assert(logger, 'result after resume is correct', result, 30);

    interpreter.setDebugRuntime(null);
  }

  // Test: PauseController enters paused state during breakpoint
  {
    let wasPaused = false;
    const debugRuntime = new SchemeDebugRuntime({
      onPause: (info) => {
        wasPaused = debugRuntime.pauseController.isPaused();
        setTimeout(() => debugRuntime.resume(), 10);
      }
    });

    debugRuntime.enable();
    debugRuntime.setBreakpoint('<unknown>', 1);
    interpreter.setDebugRuntime(debugRuntime);

    await runDebug('(+ 1 1)');

    assert(logger, 'pauseController was in paused state', wasPaused, true);

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // Stepping (step into)
  // =========================================================================
  logger.title('Debug Integration - Stepping');

  // Test: After breakpoint, stepInto causes another pause with reason 'step'
  // Uses a multi-expression to ensure there are subsequent source-bearing AST nodes
  // panelConnected must be true so handlePause blocks and waits for stepInto/resume
  {
    const pauseReasons = [];
    let pauseCount = 0;
    const debugRuntime = new SchemeDebugRuntime({
      onPause: (info) => {
        pauseCount++;
        pauseReasons.push(info.reason);
        if (pauseCount === 1) {
          // First pause (breakpoint) — step into
          setTimeout(() => debugRuntime.stepInto(), 10);
        } else {
          // Subsequent pauses — resume to finish
          setTimeout(() => debugRuntime.resume(), 10);
        }
      }
    });

    debugRuntime.enable();
    debugRuntime.panelConnected = true;
    debugRuntime.setBreakpoint('<unknown>', 1);
    interpreter.setDebugRuntime(debugRuntime);

    const result = await runDebug('(define (f x) (+ x 1)) (f 10)');

    assert(logger, 'stepInto triggered at least 2 pauses', pauseCount >= 2, true);
    assert(logger, 'first pause reason is breakpoint', pauseReasons[0], 'breakpoint');
    assert(logger, 'second pause reason is step', pauseReasons[1], 'step');
    assert(logger, 'result after stepping', result, 11);

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // Manual Pause via requestPause()
  // =========================================================================
  logger.title('Debug Integration - Manual Pause');

  // Test: requestPause() triggers pause during long-running computation
  {
    let pauseInfo = null;
    const debugRuntime = new SchemeDebugRuntime({
      onPause: (info) => {
        pauseInfo = info;
        setTimeout(() => debugRuntime.resume(), 10);
      }
    });

    debugRuntime.enable();
    interpreter.setDebugRuntime(debugRuntime);

    // Start a long computation; schedule requestPause shortly after
    const code = `
      (define (loop n acc)
        (if (<= n 0)
            acc
            (loop (- n 1) (+ acc 1))))
      (loop 1000 0)
    `;

    // Schedule a pause request after a small delay
    setTimeout(() => {
      debugRuntime.pauseController.requestPause();
    }, 5);

    const result = await runDebug(code);

    assert(logger, 'manual pause was triggered', pauseInfo !== null, true);
    assert(logger, 'manual pause reason', pauseInfo?.reason, 'manual');
    assert(logger, 'result after manual pause resume', result, 1000);

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // Cooperative Yielding
  // =========================================================================
  logger.title('Debug Integration - Cooperative Yielding');

  // Test: runDebug() yields to the event loop (doesn't block)
  {
    let yieldOccurred = false;

    // Schedule a microtask/timer to check if the event loop is free
    const yieldPromise = new Promise(resolve => {
      setTimeout(() => {
        yieldOccurred = true;
        resolve();
      }, 0);
    });

    const code = `
      (define (loop n acc)
        (if (<= n 0)
            acc
            (loop (- n 1) (+ acc 1))))
      (loop 50000 0)
    `;

    const resultPromise = runDebug(code);

    // Wait for both the computation and the yield check
    const result = await resultPromise;
    await yieldPromise;

    assert(logger, 'event loop yielded during execution', yieldOccurred, true);
    assert(logger, 'result is correct after yielding', result, 50000);
  }

  // =========================================================================
  // handlePause Creates DebugLevel
  // =========================================================================
  logger.title('Debug Integration - DebugLevel on Pause');

  // Test: When paused with a backend, a DebugLevel is created
  {
    const backend = new TestDebugBackend();
    const debugRuntime = new SchemeDebugRuntime();
    debugRuntime.setBackend(backend);
    debugRuntime.enable();
    debugRuntime.setBreakpoint('<unknown>', 1);

    // Pre-set enough resume actions for all sub-expressions on line 1
    // (the Cons cell (+ 5 5) and the symbol + both match the line breakpoint)
    for (let i = 0; i < 10; i++) backend.setNextAction('resume');

    interpreter.setDebugRuntime(debugRuntime);

    const result = await runDebug('(+ 5 5)');

    assert(logger, 'backend received pause event', backend.pauseEvents.length >= 1, true);
    const event = backend.pauseEvents[0];
    assert(logger, 'pause event has reason', event?.reason, 'breakpoint');
    assert(logger, 'pause event has source', event?.source !== null, true);
    assert(logger, 'pause event has env', event?.env !== undefined, true);
    assert(logger, 'result after backend resume', result, 10);

    interpreter.setDebugRuntime(null);
  }

  // Test: DebugLevel properties are correct
  {
    const levelStack = new DebugLevelStack();
    const source = { filename: 'test.scm', line: 5, column: 1 };
    const stack = [{ name: 'my-fn', source }];
    const env = { test: true };

    const level = new DebugLevel(0, 'breakpoint', source, stack, env, null);
    levelStack.push(level);

    assert(logger, 'debug level number', level.level, 0);
    assert(logger, 'debug level reason', level.reason, 'breakpoint');
    assert(logger, 'debug level source', level.source.filename, 'test.scm');
    assert(logger, 'debug level stack length', level.stack.length, 1);
    assert(logger, 'debug level env', level.env.test, true);
    assert(logger, 'level stack depth', levelStack.depth(), 1);

    levelStack.pop();
    assert(logger, 'level stack empty after pop', levelStack.depth(), 0);
  }

  // =========================================================================
  // Abort During Pause
  // =========================================================================
  logger.title('Debug Integration - Abort During Pause');

  // Test: Aborting while paused terminates execution
  // panelConnected must be true so handlePause blocks and waits for abort
  {
    let abortError = null;
    const debugRuntime = new SchemeDebugRuntime({
      onPause: (info) => {
        // Abort instead of resuming
        setTimeout(() => debugRuntime.abort(), 10);
      }
    });

    debugRuntime.enable();
    debugRuntime.panelConnected = true;
    debugRuntime.setBreakpoint('<unknown>', 1);
    interpreter.setDebugRuntime(debugRuntime);

    try {
      await runDebug('(+ 1 2)');
    } catch (e) {
      abortError = e;
    }

    assert(logger, 'abort throws error', abortError !== null, true);
    assert(logger, 'abort error message mentions abort',
      abortError?.message?.toLowerCase().includes('abort'), true);

    // Reset for subsequent tests
    debugRuntime.pauseController.reset();
    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // evaluateStringDebug
  // =========================================================================
  logger.title('Debug Integration - evaluateStringDebug');

  // Test: evaluateStringDebug produces correct results for string code
  {
    const result = await interpreter.evaluateStringDebug('(+ 100 200)');
    assert(logger, 'evaluateStringDebug arithmetic', result, 300);
  }

  // Test: evaluateStringDebug handles multiple expressions
  {
    const result = await interpreter.evaluateStringDebug(`
      (define x-debug-test 42)
      (+ x-debug-test 8)
    `);
    assert(logger, 'evaluateStringDebug multiple expressions', result, 50);
  }

  // Test: evaluateStringDebug with string result
  {
    const result = await interpreter.evaluateStringDebug('"debug-test-string"');
    assert(logger, 'evaluateStringDebug string result', result, 'debug-test-string');
  }

  // Test: evaluateStringDebug with boolean result
  {
    const result = await interpreter.evaluateStringDebug('(> 5 3)');
    assert(logger, 'evaluateStringDebug boolean result', result, true);
  }

  // Test: evaluateStringDebug with list result
  {
    const syncResult = runSync("'(1 2 3)");
    const debugResult = await interpreter.evaluateStringDebug("'(1 2 3)");
    assert(logger, 'evaluateStringDebug list result', debugResult, syncResult);
  }

  // Final cleanup
  interpreter.setDebugRuntime(null);
}

export default runDebugIntegrationTests;
