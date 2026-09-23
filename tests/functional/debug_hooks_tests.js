/**
 * @fileoverview Functional tests for interpreter debug hooks.
 *
 * Tests that the interpreter properly integrates with the debug runtime.
 * Covers breakpoint management, pause/resume, stepping, and state inspection.
 */

import { assert, createTestEnv, run } from '../harness/helpers.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { instrumentInterpreter } from '../../src/debug/instrumentation.js';

/**
 * Runs all debug hooks tests.
 * @param {Object} interpreter - The interpreter instance
 * @param {Object} logger - Test logger
 */
export async function runDebugHooksTests(interpreter, logger) {
    logger.title('Debug Hooks - Interpreter Integration');

    // Test: Interpreter runs normally when debugRuntime is null
    {
        interpreter.setDebugRuntime(null);
        const result = run(interpreter, '(+ 1 2)');
        assert(logger, 'runs without debugRuntime', result, 3);
    }

    // Test: Interpreter accepts debugRuntime via setDebugRuntime
    {
        const debugRuntime = new SchemeDebugRuntime();
        interpreter.setDebugRuntime(debugRuntime);
        assert(logger, 'setDebugRuntime sets runtime', interpreter.debugRuntime !== null, true);

        const result = run(interpreter, '(+ 3 4)');
        assert(logger, 'runs with debugRuntime', result, 7);

        interpreter.setDebugRuntime(null);
    }

    // Test: No pause when no breakpoints and not stepping
    {
        let pauseCount = 0;
        const debugRuntime = new SchemeDebugRuntime({
            onPause: () => pauseCount++
        });
        interpreter.setDebugRuntime(debugRuntime);

        run(interpreter, '(+ 1 2 3)');
        assert(logger, 'no pause without breakpoints', pauseCount, 0);

        interpreter.setDebugRuntime(null);
    }

    // Test: Debug hooks don't affect return values
    {
        const debugRuntime = new SchemeDebugRuntime();
        interpreter.setDebugRuntime(debugRuntime);

        const numResult = run(interpreter, '(* 6 7)');
        assert(logger, 'number result preserved', numResult, 42);

        const strResult = run(interpreter, '"hello"');
        assert(logger, 'string result preserved', strResult, 'hello');

        // Lists are converted to JS arrays by the run helper
        const vectorResult = run(interpreter, '#(1 2 3)');
        assert(logger, 'vector result preserved type', Array.isArray(vectorResult), true);

        interpreter.setDebugRuntime(null);
    }

    logger.title('Debug Hooks - Breakpoint Pause');

    // Test: Breakpoint management works
    {
        const debugRuntime = new SchemeDebugRuntime();
        const bp1 = debugRuntime.setBreakpoint('test.scm', 10);
        const bp2 = debugRuntime.setBreakpoint('test.scm', 20, 5);

        assert(logger, 'breakpoint IDs are strings', typeof bp1, 'string');
        assert(logger, 'hasBreakpoint true', debugRuntime.breakpointManager.hasBreakpoint(
            { filename: 'test.scm', line: 10, column: 1 }
        ), true);
        assert(logger, 'hasBreakpoint with column', debugRuntime.breakpointManager.hasBreakpoint(
            { filename: 'test.scm', line: 20, column: 5 }
        ), true);

        debugRuntime.removeBreakpoint(bp1);
        assert(logger, 'hasBreakpoint after remove', debugRuntime.breakpointManager.hasBreakpoint(
            { filename: 'test.scm', line: 10, column: 1 }
        ), false);
    }

    // Test: Multiple breakpoints can be set
    {
        const debugRuntime = new SchemeDebugRuntime();
        const bp1 = debugRuntime.setBreakpoint('file1.scm', 10);
        const bp2 = debugRuntime.setBreakpoint('file2.scm', 20);
        const bp3 = debugRuntime.setBreakpoint('file1.scm', 30);

        assert(logger, 'multiple breakpoints count', debugRuntime.getAllBreakpoints().length, 3);

        debugRuntime.removeBreakpoint(bp2);
        assert(logger, 'breakpoint removed', debugRuntime.getAllBreakpoints().length, 2);
    }

    // Test: shouldPause logic works correctly
    {
        const debugRuntime = new SchemeDebugRuntime();
        debugRuntime.enable();
        debugRuntime.setBreakpoint('test.scm', 10);

        // Source matches breakpoint
        assert(logger, 'shouldPause true for matching breakpoint',
            debugRuntime.shouldPause({ filename: 'test.scm', line: 10, column: 1 }, {}), true);

        // Source doesn't match
        assert(logger, 'shouldPause false for non-matching',
            debugRuntime.shouldPause({ filename: 'test.scm', line: 20, column: 1 }, {}), false);

        // No source
        assert(logger, 'shouldPause false for null source',
            debugRuntime.shouldPause(null, {}), false);
    }

    // Test: Breakpoint integration - pause triggers on matching source location
    // This test verifies the full integration: parser -> analyzer -> AST -> interpreter -> debugger
    {
        let pauseInfo = null;
        const debugRuntime = new SchemeDebugRuntime({
            onPause: (info) => { pauseInfo = info; }
        });

        // Set a breakpoint on line 1 of <unknown> (the default filename from parser)
        debugRuntime.enable();
        debugRuntime.setBreakpoint('<unknown>', 1);
        interpreter.setDebugRuntime(debugRuntime);

        run(interpreter, '(+ 1 2)');

        assert(logger, 'breakpoint triggered pause', pauseInfo !== null, true);
        if (pauseInfo) {
            assert(logger, 'pause reason is breakpoint', pauseInfo.reason, 'breakpoint');
            assert(logger, 'pause has source', pauseInfo.source !== null, true);
            assert(logger, 'source has filename', pauseInfo.source?.filename, '<unknown>');
            assert(logger, 'source has line', pauseInfo.source?.line, 1);
        }

        interpreter.setDebugRuntime(null);
    }

    logger.title('Debug Hooks - Enable/Disable');

    // Test: Disabled debug runtime - shouldPause returns false
    {
        const debugRuntime = new SchemeDebugRuntime();
        debugRuntime.setBreakpoint('test.scm', 10);

        debugRuntime.disable();
        assert(logger, 'disabled shouldPause false',
            debugRuntime.shouldPause({ filename: 'test.scm', line: 10, column: 1 }, {}), false);

        debugRuntime.enable();
        assert(logger, 'enabled shouldPause true',
            debugRuntime.shouldPause({ filename: 'test.scm', line: 10, column: 1 }, {}), true);
    }

    // Test: Integration with interpreter - disabled means no pause
    {
        const debugRuntime = new SchemeDebugRuntime();
        debugRuntime.disable();
        interpreter.setDebugRuntime(debugRuntime);

        // Should run without any issues even with debug runtime attached
        const result = run(interpreter, '(+ 10 20)');
        assert(logger, 'disabled runtime runs', result, 30);

        interpreter.setDebugRuntime(null);
    }

    logger.title('Debug Hooks - Stack Operations');

    // Test: Debug runtime tracks stack depth
    {
        const debugRuntime = new SchemeDebugRuntime();
        interpreter.setDebugRuntime(debugRuntime);

        // Initially empty
        assert(logger, 'initial stack depth', debugRuntime.getDepth(), 0);

        // Manual frame tracking (would be called by AST nodes in real usage)
        debugRuntime.enterFrame({ name: 'test', source: null, env: {} });
        assert(logger, 'after enterFrame depth', debugRuntime.getDepth(), 1);

        debugRuntime.exitFrame();
        assert(logger, 'after exitFrame depth', debugRuntime.getDepth(), 0);

        interpreter.setDebugRuntime(null);
    }

    // Test: isPaused state tracking
    {
        const debugRuntime = new SchemeDebugRuntime();

        assert(logger, 'initial not paused', debugRuntime.isPaused(), false);

        debugRuntime.pauseController.pause();
        assert(logger, 'after pause isPaused', debugRuntime.isPaused(), true);

        debugRuntime.pauseController.resume();
        assert(logger, 'after resume not paused', debugRuntime.isPaused(), false);
    }

    logger.title('Debug Hooks - Reset');

    // Test: reset clears all state
    {
        const debugRuntime = new SchemeDebugRuntime();
        debugRuntime.setBreakpoint('test.scm', 10);
        debugRuntime.enterFrame({ name: 'test', source: null, env: {} });
        debugRuntime.pauseController.pause();

        debugRuntime.reset();

        assert(logger, 'reset clears breakpoints', debugRuntime.getAllBreakpoints().length, 0);
        assert(logger, 'reset clears stack', debugRuntime.getDepth(), 0);
        assert(logger, 'reset clears pause', debugRuntime.isPaused(), false);
    }

    // =========================================================================
    // Tail calls under the debugger
    // =========================================================================
    // Attaching a debug runtime must not change the evaluator's space
    // behaviour. Each procedure entry pushes a DebugExitFrame, so unless tail
    // calls reuse the existing one, a tail-recursive loop accumulates one frame
    // per iteration and tail-call optimization is lost whenever debugging is
    // switched on.
    logger.title('Debug Hooks - Tail Call Optimization');

    {
        interpreter.setDebugRuntime(null);
        run(interpreter, '(define (tco-loop n) (if (< n 1) 0 (tco-loop (- n 1))))');
        run(interpreter, '(define (tco-deep n) (if (< n 1) 0 (+ 1 (tco-deep (- n 1)))))');

        /**
         * Measures the deepest the frame stack gets while running `code`.
         * @param {string} code - Scheme source to evaluate.
         * @param {Object|null} runtime - A debug runtime to attach, or null.
         * @returns {{depth: number, result: *}} Max frame depth and the result.
         */
        const measure = (code, runtime) => {
            interpreter.setDebugRuntime(runtime);
            const probe = instrumentInterpreter(interpreter);
            let result;
            let stats;
            try {
                result = run(interpreter, code);
            } finally {
                stats = probe.stop();
                interpreter.setDebugRuntime(null);
            }
            return { depth: stats.maxStackDepth, result };
        };

        const makeRuntime = () => {
            const runtime = new SchemeDebugRuntime({ onPause: () => { } });
            runtime.enable();
            return runtime;
        };

        const shallow = measure('(tco-loop 50)', makeRuntime());
        const long = measure('(tco-loop 800)', makeRuntime());

        assert(logger, 'tail loop still produces the right answer under the debugger',
            long.result, 0);
        assert(logger, 'tail loop frame depth does not grow with iteration count',
            long.depth, shallow.depth);

        // The complementary check: if tail calls were detected too eagerly the
        // stack would stop growing for genuine recursion too, and the debugger
        // would report a flat stack for a deeply nested call.
        const deepSmall = measure('(tco-deep 20)', makeRuntime());
        const deepLarge = measure('(tco-deep 40)', makeRuntime());

        assert(logger, 'non-tail recursion still grows the frame stack',
            deepLarge.depth > deepSmall.depth, true);
        assert(logger, 'non-tail recursion still produces the right answer',
            deepLarge.result, 40);

        // A tail call that ends a body of several expressions. Tail position
        // is detected by finding the procedure's DebugExitFrame on top of the
        // stack, so anything left above it -- a frame for an exhausted
        // sequence, say -- makes every iteration look like a new call: the
        // frame stack grows, and so does the shadow stack the debugger shows.
        run(interpreter, "(define (tco-body-loop n) (car '(1)) (if (< n 1) 0 (tco-body-loop (- n 1))))");

        /**
         * Makes a debug runtime that also records the deepest its shadow call
         * stack gets.
         * @returns {{runtime: Object, maxShadowDepth: function(): number}}
         */
        const makeShadowRuntime = () => {
            const runtime = makeRuntime();
            let maxShadowDepth = 0;
            const enterFrame = runtime.enterFrame.bind(runtime);
            runtime.enterFrame = (frameInfo) => {
                enterFrame(frameInfo);
                maxShadowDepth = Math.max(maxShadowDepth, runtime.getDepth());
            };
            return { runtime, maxShadowDepth: () => maxShadowDepth };
        };

        const bodySmallRuntime = makeShadowRuntime();
        const bodyLargeRuntime = makeShadowRuntime();
        const bodySmall = measure('(tco-body-loop 50)', bodySmallRuntime.runtime);
        const bodyLarge = measure('(tco-body-loop 800)', bodyLargeRuntime.runtime);

        assert(logger, 'tail call ending a multi-expression body: right answer under the debugger',
            bodyLarge.result, 0);
        assert(logger, 'tail call ending a multi-expression body: frame depth does not grow under the debugger',
            bodyLarge.depth, bodySmall.depth);
        assert(logger, 'tail call ending a multi-expression body: shadow call stack does not grow',
            bodyLargeRuntime.maxShadowDepth(), bodySmallRuntime.maxShadowDepth());
    }
}

export default runDebugHooksTests;
