/**
 * @fileoverview Integration tests for debug runtime with the interpreter's
 * runAsync method. Tests breakpoint pause/resume, stepping, cooperative
 * yielding, handlePause, and abort behavior.
 */

import { assert, run, createTestLogger } from '../harness/helpers.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { TestDebugBackend } from '../../src/debug/debug_backend.js';

/**
 * Runs all debug integration tests.
 * @param {import('../../src/core/interpreter/interpreter.js').Interpreter} interpreter
 * @param {object} logger - Test logger from createTestLogger
 */
export async function runDebugIntegrationTests(interpreter, logger) {
  logger.title('Debug Integration - runAsync correctness with debug enabled');

  // Helper: sync run
  const runSync = (code) => run(interpreter, code);

  // Helper: async run
  const runAsync = (code, options = {}) => interpreter.evaluateStringAsync(code, options);

  // =========================================================================
  // 1. runAsync correctness with debug enabled (no breakpoints)
  // =========================================================================

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();
    interpreter.setDebugRuntime(runtime);

    const syncLiteral = runSync('42');
    const asyncLiteral = await runAsync('42');
    assert(logger, 'debug enabled: literal number', asyncLiteral, syncLiteral);

    const syncArith = runSync('(+ 1 2 3)');
    const asyncArith = await runAsync('(+ 1 2 3)');
    assert(logger, 'debug enabled: arithmetic', asyncArith, syncArith);

    const recursionCode = `
      (define (factorial n)
        (if (<= n 1) 1 (* n (factorial (- n 1)))))
      (factorial 10)
    `;
    const syncRecur = runSync(recursionCode);
    const asyncRecur = await runAsync(recursionCode);
    assert(logger, 'debug enabled: recursion', asyncRecur, syncRecur);

    const tailCode = `
      (define (sum-to n acc)
        (if (<= n 0) acc (sum-to (- n 1) (+ acc n))))
      (sum-to 100 0)
    `;
    const syncTail = runSync(tailCode);
    const asyncTail = await runAsync(tailCode);
    assert(logger, 'debug enabled: tail recursion', asyncTail, syncTail);

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // 2. Breakpoint pause/resume
  // =========================================================================

  logger.title('Debug Integration - Breakpoint pause/resume');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();
    interpreter.setDebugRuntime(runtime);
    runtime.setBreakpoint('<unknown>', 1);

    let pauseCount = 0;
    let capturedReason = null;
    runtime.onPause = (info) => {
      pauseCount++;
      capturedReason = info.reason;
      Promise.resolve().then(() => runtime.resume());
    };

    const result = await runAsync('(+ 1 2)');
    assert(logger, 'breakpoint: result correct', result, 3);
    assert(logger, 'breakpoint: paused at least once', pauseCount >= 1, true);
    assert(logger, 'breakpoint: reason is breakpoint', capturedReason, 'breakpoint');

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // 3. PauseController state during breakpoint
  // =========================================================================

  logger.title('Debug Integration - PauseController state during breakpoint');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();
    interpreter.setDebugRuntime(runtime);
    runtime.setBreakpoint('<unknown>', 1);

    let pausedDuringCallback = null;
    runtime.onPause = (info) => {
      pausedDuringCallback = runtime.pauseController.isPaused();
      Promise.resolve().then(() => runtime.resume());
    };

    await runAsync('(+ 10 20)');
    assert(logger, 'pauseController isPaused during onPause', pausedDuringCallback, true);

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // 4. Step into
  // =========================================================================

  logger.title('Debug Integration - Step into');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();
    interpreter.setDebugRuntime(runtime);
    const code = '(define (add-one x) (+ x 1)) (add-one 2)';
    const bpId = runtime.setBreakpoint('<unknown>', 1);

    const pauseReasons = [];
    let callCount = 0;
    runtime.onPause = (info) => {
      callCount++;
      pauseReasons.push(info.reason);
      if (callCount === 1) {
        // First pause (breakpoint) — remove BP so it doesn't keep firing, then step
        runtime.removeBreakpoint(bpId);
        Promise.resolve().then(() => runtime.stepInto());
      } else {
        // Subsequent pauses — resume
        Promise.resolve().then(() => runtime.resume());
      }
    };

    const result = await runAsync(code, { stepsPerYield: 1000 });
    assert(logger, 'step into: result correct', result, 3);
    assert(logger, 'step into: at least two pauses', pauseReasons.length >= 2, true);
    assert(logger, 'step into: first reason is breakpoint', pauseReasons[0], 'breakpoint');

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // 5. Manual pause via pauseController.pause()
  // =========================================================================

  logger.title('Debug Integration - Manual pause');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();
    interpreter.setDebugRuntime(runtime);

    let pauseReceived = false;
    let yieldCount = 0;

    runtime.onPause = (info) => {
      pauseReceived = true;
      Promise.resolve().then(() => runtime.resume());
    };

    const code = `
      (define (loop n)
        (if (<= n 0) 'done (loop (- n 1))))
      (loop 5000)
    `;

    const result = await interpreter.evaluateStringAsync(code, {
      stepsPerYield: 50,
      onYield: () => {
        yieldCount++;
        // On first yield, manually pause via the pause controller + fire onPause
        if (yieldCount === 1) {
          runtime.pauseController.pause('manual', null);
          if (runtime.onPause) {
            runtime.onPause({ reason: 'manual', source: null, stack: [], env: null });
          }
        }
      }
    });

    assert(logger, 'manual pause: result correct', result.name || result, 'done');
    assert(logger, 'manual pause: pause was received', pauseReceived, true);

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // 6. Cooperative yielding in debug mode
  // =========================================================================

  logger.title('Debug Integration - Cooperative yielding');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();
    interpreter.setDebugRuntime(runtime);

    let yieldCount = 0;
    const code = `
      (define (loop n)
        (if (<= n 0) 'done (loop (- n 1))))
      (loop 500)
    `;

    const result = await interpreter.evaluateStringAsync(code, {
      stepsPerYield: 50,
      onYield: () => { yieldCount++; }
    });

    assert(logger, 'cooperative yield: result correct', result.name || result, 'done');
    assert(logger, 'cooperative yield: yielded multiple times', yieldCount > 1, true);

    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // 7. handlePause creates DebugLevel
  // =========================================================================

  logger.title('Debug Integration - handlePause creates DebugLevel');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();

    const fakeSource = { filename: 'test.scm', line: 5, column: 1 };
    const fakeEnv = {};

    let capturedLevel = null;
    runtime.onPause = (info) => {
      capturedLevel = info.level;
      // Resume so handlePause can complete
      Promise.resolve().then(() => runtime.resume());
    };

    const action = await runtime.handlePause(fakeSource, fakeEnv, 'breakpoint');

    assert(logger, 'handlePause: returned resume', action, 'resume');
    assert(logger, 'handlePause: level was created', capturedLevel !== null, true);
    assert(logger, 'handlePause: level reason', capturedLevel.reason, 'breakpoint');
    assert(logger, 'handlePause: level source', capturedLevel.source, fakeSource);
    assert(logger, 'handlePause: level env', capturedLevel.env, fakeEnv);
    assert(logger, 'handlePause: levelStack empty after', runtime.levelStack.depth(), 0);
  }

  // =========================================================================
  // 8. Abort during pause
  // =========================================================================

  logger.title('Debug Integration - Abort during pause');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();
    interpreter.setDebugRuntime(runtime);
    runtime.setBreakpoint('<unknown>', 1);

    runtime.onPause = (info) => {
      Promise.resolve().then(() => runtime.abort());
    };

    let abortError = null;
    try {
      await runAsync('(+ 1 2)');
    } catch (e) {
      abortError = e;
    }

    assert(logger, 'abort: threw an error', abortError !== null, true);
    assert(logger, 'abort: error message mentions abort',
      abortError && abortError.message && abortError.message.toLowerCase().includes('abort'), true);

    // Reset the abort state for subsequent tests
    runtime.pauseController.reset();
    interpreter.setDebugRuntime(null);
  }

  // =========================================================================
  // 9. handlePause with TestDebugBackend — resume
  // =========================================================================

  logger.title('Debug Integration - handlePause with TestDebugBackend');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();

    const backend = new TestDebugBackend();
    backend.setNextAction('resume');
    runtime.setBackend(backend);

    const fakeSource = { filename: 'test.scm', line: 10, column: 1 };
    const fakeEnv = {};

    const action = await runtime.handlePause(fakeSource, fakeEnv, 'breakpoint');

    assert(logger, 'handlePause+backend: action is resume', action, 'resume');
    assert(logger, 'handlePause+backend: backend received pause', backend.pauseEvents.length, 1);
    assert(logger, 'handlePause+backend: pause reason', backend.pauseEvents[0].reason, 'breakpoint');
    assert(logger, 'handlePause+backend: levelStack empty after', runtime.levelStack.depth(), 0);
  }

  // =========================================================================
  // 10. handlePause returns step actions
  // =========================================================================

  logger.title('Debug Integration - handlePause returns step actions');

  {
    const runtime = new SchemeDebugRuntime();
    runtime.enable();

    const backend = new TestDebugBackend();
    backend.setNextAction('stepInto');
    runtime.setBackend(backend);

    const fakeSource = { filename: 'test.scm', line: 15, column: 1 };
    const fakeEnv = {};

    const action = await runtime.handlePause(fakeSource, fakeEnv, 'step');

    assert(logger, 'handlePause stepInto: action is stepInto', action, 'stepInto');
    assert(logger, 'handlePause stepInto: backend received pause', backend.pauseEvents.length, 1);
  }
}

export default runDebugIntegrationTests;
