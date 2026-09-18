/**
 * @fileoverview Unit tests for evaluator instrumentation.
 *
 * Instrumentation produces deterministic counts rather than timings, which is
 * what makes it useful for attributing an optimization: a change that removes
 * work shows up as a smaller step count on every machine, whereas a change that
 * merely got lucky with the JIT does not.
 */

import { assert, run } from '../harness/helpers.js';
import { instrumentInterpreter } from '../../src/debug/instrumentation.js';

/**
 * Runs the instrumentation unit tests.
 * @param {Object} interpreter - A bootstrapped interpreter instance.
 * @param {Object} logger - The test logger.
 * @returns {Promise<void>}
 */
export async function runInstrumentationTests(interpreter, logger) {
    logger.title('Evaluator Instrumentation Tests');

    // Counting a trivial literal evaluation.
    {
        const probe = instrumentInterpreter(interpreter);
        run(interpreter, '42');
        const stats = probe.stop();

        assert(logger, 'records at least one step', stats.totalSteps > 0, true);
        assert(logger, 'stepsByType is populated',
            Object.keys(stats.stepsByType).length > 0, true);
    }

    // Counts must be deterministic: the same program run twice yields the same
    // totals. This is the property that makes instrumentation trustworthy.
    {
        const probeA = instrumentInterpreter(interpreter);
        run(interpreter, '(define (loop-a n) (if (< n 1) 0 (loop-a (- n 1)))) (loop-a 20)');
        const a = probeA.stop();

        const probeB = instrumentInterpreter(interpreter);
        run(interpreter, '(define (loop-b n) (if (< n 1) 0 (loop-b (- n 1)))) (loop-b 20)');
        const b = probeB.stop();

        assert(logger, 'step counts are deterministic across identical runs',
            a.totalSteps, b.totalSteps);
    }

    // More work must mean strictly more steps, and proportionally so: doubling
    // the iteration count of a tail loop should roughly double the step count.
    {
        const probeSmall = instrumentInterpreter(interpreter);
        run(interpreter, '(define (cnt-s n) (if (< n 1) 0 (cnt-s (- n 1)))) (cnt-s 10)');
        const small = probeSmall.stop();

        const probeLarge = instrumentInterpreter(interpreter);
        run(interpreter, '(define (cnt-l n) (if (< n 1) 0 (cnt-l (- n 1)))) (cnt-l 20)');
        const large = probeLarge.stop();

        assert(logger, 'more iterations means more steps',
            large.totalSteps > small.totalSteps, true);
    }

    // Non-tail recursion must push continuation frames; a tail loop must not
    // grow the stack. This is the measurement that verifies TCO still holds
    // after any change to the evaluator.
    {
        const probeTail = instrumentInterpreter(interpreter);
        run(interpreter, '(define (t n) (if (< n 1) 0 (t (- n 1)))) (t 50)');
        const tail = probeTail.stop();

        const probeDeep = instrumentInterpreter(interpreter);
        run(interpreter, '(define (d n) (if (< n 1) 0 (+ 1 (d (- n 1))))) (d 50)');
        const deep = probeDeep.stop();

        assert(logger, 'non-tail recursion reaches a deeper frame stack than tail recursion',
            deep.maxStackDepth > tail.maxStackDepth, true);
    }

    // Instrumentation must leave the interpreter exactly as it found it.
    // Note these compare identity and report booleans: the assert helper
    // converts functions to `undefined`, so comparing them directly would
    // silently pass whatever happened.
    {
        const before = interpreter.step;
        const hadOwnStepBefore = Object.prototype.hasOwnProperty.call(interpreter, 'step');

        const probe = instrumentInterpreter(interpreter);
        assert(logger, 'step is replaced while instrumented',
            interpreter.step !== before, true);

        probe.stop();
        assert(logger, 'step is restored to the original function after stop',
            interpreter.step === before, true);
        assert(logger, 'restoring does not leave a shadowing own property',
            Object.prototype.hasOwnProperty.call(interpreter, 'step'), hadOwnStepBefore);
    }

    // Results must be unaffected by observation.
    {
        const probe = instrumentInterpreter(interpreter);
        const result = run(interpreter, '(+ 1 2 3)');
        probe.stop();
        assert(logger, 'instrumentation does not alter results', result, 6n);
    }

    // stop() must be idempotent, so a caller can stop in a finally block
    // without tracking whether it already stopped.
    {
        const probe = instrumentInterpreter(interpreter);
        run(interpreter, '1');
        const first = probe.stop();
        const second = probe.stop();
        assert(logger, 'stop is idempotent', first.totalSteps, second.totalSteps);
    }
}
