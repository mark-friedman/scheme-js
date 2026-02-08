/**
 * @fileoverview Tests for exception debugging features.
 *
 * These tests verify that the debugger can pause on exceptions,
 * both caught and uncaught, and that the exception state is
 * available for inspection.
 */

import { createTestLogger, assert, skip } from '../harness/helpers.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { RaiseNode } from '../../src/core/interpreter/ast_nodes.js';
import { Symbol } from '../../src/core/interpreter/symbol.js';

/**
 * Runs exception debugging tests.
 * @param {Object} interpreter - The interpreter instance
 * @param {Object} logger - Test logger
 */
export async function runExceptionDebuggingTests(interpreter, logger) {
    logger.title('Exception Debugging Tests');

    // =========================================================================
    // Configuration Tests
    // =========================================================================

    // Test: Basic pause controller functionality
    {
        const runtime = new SchemeDebugRuntime({
            onPause: () => { }
        });
        runtime.enable();

        assert(logger, 'pause controller exists',
            runtime.pauseController !== undefined, true);
        assert(logger, 'exception handler exists',
            runtime.exceptionHandler !== undefined, true);
    }

    // Test: breakOnUncaughtException defaults to true
    {
        const runtime = new SchemeDebugRuntime({
            onPause: () => { }
        });

        assert(logger, 'breakOnUncaughtException defaults to true',
            runtime.breakOnUncaughtException, true);
    }

    // Test: breakOnCaughtException defaults to false
    {
        const runtime = new SchemeDebugRuntime({
            onPause: () => { }
        });

        assert(logger, 'breakOnCaughtException defaults to false',
            runtime.breakOnCaughtException, false);
    }

    // =========================================================================
    // Phase 4.6: Break on Uncaught Exception with Async Pause
    // =========================================================================

    // Test: Uncaught exception triggers pause callback (direct RaiseNode)
    {
        const pauseEvents = [];
        let resumeCalled = false;

        const runtime = new SchemeDebugRuntime({
            onPause: (event) => {
                pauseEvents.push(event);
                // Schedule resume for next tick
                setTimeout(() => {
                    resumeCalled = true;
                    runtime.resume();
                }, 10);
            }
        });
        runtime.breakOnUncaughtException = true;
        interpreter.setDebugRuntime(runtime);
        runtime.enable();

        // Create RaiseNode directly - proven to work
        const exceptionVal = new Symbol('test-exception');
        const raiseNode = new RaiseNode(exceptionVal, false);

        try {
            await interpreter.runAsync(raiseNode, interpreter.globalEnv, { stepsPerYield: 1 });
        } catch (e) {
            // Expected - uncaught exception propagates after resume
        }

        // Give time for async pause/resume
        await new Promise(resolve => setTimeout(resolve, 100));

        // Clean up
        interpreter.setDebugRuntime(null);

        assert(logger, 'uncaught exception fires onPause',
            pauseEvents.length >= 1, true);
        assert(logger, 'pause reason is exception',
            pauseEvents[0]?.reason, 'exception');
        assert(logger, 'exception value captured',
            pauseEvents[0]?.exception instanceof Symbol &&
            pauseEvents[0]?.exception.name === 'test-exception', true);
    }

    // Test: No pause when breakOnUncaughtException is false
    {
        const pauseEvents = [];
        const runtime = new SchemeDebugRuntime({
            onPause: (event) => pauseEvents.push(event)
        });
        runtime.breakOnUncaughtException = false;
        interpreter.setDebugRuntime(runtime);
        runtime.enable();

        const exceptionVal = new Symbol('should-not-pause');
        const raiseNode = new RaiseNode(exceptionVal, false);

        try {
            await interpreter.runAsync(raiseNode, interpreter.globalEnv, { stepsPerYield: 1 });
        } catch (e) {
            // Expected
        }

        interpreter.setDebugRuntime(null);

        assert(logger, 'no pause when uncaught exception disabled',
            pauseEvents.length, 0);
    }

    // Test: Resume continues after pause
    {
        const pauseEvents = [];
        let resumed = false;

        const runtime = new SchemeDebugRuntime({
            onPause: (event) => {
                pauseEvents.push(event);
                setTimeout(() => {
                    resumed = true;
                    runtime.resume();
                }, 10);
            }
        });
        runtime.breakOnUncaughtException = true;
        interpreter.setDebugRuntime(runtime);
        runtime.enable();

        const raiseNode = new RaiseNode(new Symbol('resumable'), false);

        try {
            await interpreter.runAsync(raiseNode, interpreter.globalEnv, { stepsPerYield: 1 });
        } catch (e) {
            // Exception should still propagate after resume
        }

        await new Promise(resolve => setTimeout(resolve, 100));
        interpreter.setDebugRuntime(null);

        assert(logger, 'resume was called after pause',
            resumed, true);
    }

    // =========================================================================
    // Remaining tests - keeping as skips until full integration
    // =========================================================================

    skip(logger, 'stack trace at exception (requires frame tracking)');
    skip(logger, 'nested handlers pause correctly (complex scenario)');
    skip(logger, 'restartable conditions (future enhancement)');
}

export default runExceptionDebuggingTests;
