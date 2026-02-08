/**
 * @fileoverview Unit tests for StackTracer.
 *
 * Tests for call stack tracking with TCO (Tail Call Optimization) awareness.
 * Part of Phase 1: Debug Runtime Core.
 */

import { assert } from '../harness/helpers.js';
import { StackTracer } from '../../src/debug/stack_tracer.js';

/**
 * Runs all StackTracer tests.
 * @param {Object} logger - Test logger
 */
export function runStackTracerTests(logger) {
    logger.title('StackTracer - Basic Operations');

    // Test: enterFrame pushes new frame
    {
        const tracer = new StackTracer();
        tracer.enterFrame({
            name: 'foo',
            source: { filename: 'test.scm', line: 10, column: 1 },
            env: {}
        });

        assert(logger, 'enterFrame increases depth', tracer.getDepth(), 1);
        assert(logger, 'getStack returns 1 frame', tracer.getStack().length, 1);
    }

    // Test: exitFrame pops frame
    {
        const tracer = new StackTracer();
        tracer.enterFrame({ name: 'foo', source: null, env: {} });
        tracer.enterFrame({ name: 'bar', source: null, env: {} });
        tracer.exitFrame();

        assert(logger, 'exitFrame decreases depth', tracer.getDepth(), 1);
        assert(logger, 'remaining frame is foo', tracer.getStack()[0].name, 'foo');
    }

    // Test: getStack returns frames in correct order (bottom to top)
    {
        const tracer = new StackTracer();
        tracer.enterFrame({ name: 'main', source: null, env: {} });
        tracer.enterFrame({ name: 'foo', source: null, env: {} });
        tracer.enterFrame({ name: 'bar', source: null, env: {} });

        const stack = tracer.getStack();
        assert(logger, 'stack has 3 frames', stack.length, 3);
        assert(logger, 'stack[0] is main (bottom)', stack[0].name, 'main');
        assert(logger, 'stack[1] is foo', stack[1].name, 'foo');
        assert(logger, 'stack[2] is bar (top)', stack[2].name, 'bar');
    }

    // Test: TCO frame replacement (tail position flag)
    logger.title('StackTracer - TCO Handling');
    {
        const tracer = new StackTracer();
        tracer.enterFrame({ name: 'main', source: null, env: {} });
        tracer.enterFrame({ name: 'recurse', source: null, env: {} });

        // Tail call replaces current frame
        tracer.replaceFrame({ name: 'recurse-tail', source: null, env: {} });

        const stack = tracer.getStack();
        assert(logger, 'TCO replacement keeps depth', tracer.getDepth(), 2);
        assert(logger, 'TCO replaced frame name', stack[1].name, 'recurse-tail');
    }

    // Test: TCO counter increments on replacement
    {
        const tracer = new StackTracer();
        tracer.enterFrame({ name: 'main', source: null, env: {} });
        tracer.enterFrame({ name: 'loop', source: null, env: {} });

        tracer.replaceFrame({ name: 'loop', source: null, env: {} });
        tracer.replaceFrame({ name: 'loop', source: null, env: {} });
        tracer.replaceFrame({ name: 'loop', source: null, env: {} });

        const stack = tracer.getStack();
        assert(logger, 'TCO counter tracks replacements', stack[1].tcoCount, 3);
    }

    // Test: Function name extraction from identifier application
    logger.title('StackTracer - Frame Information');
    {
        const tracer = new StackTracer();
        tracer.enterFrame({
            name: 'my-function',
            source: { filename: 'lib.scm', line: 42, column: 1 },
            env: {}
        });

        const frame = tracer.getStack()[0];
        assert(logger, 'frame has name', frame.name, 'my-function');
    }

    // Test: Function name for lambda (shows <lambda>)
    {
        const tracer = new StackTracer();
        tracer.enterFrame({
            name: '<lambda>',
            source: { filename: 'test.scm', line: 5, column: 10 },
            env: {}
        });

        assert(logger, 'lambda frame name', tracer.getStack()[0].name, '<lambda>');
    }

    // Test: Function name for primitive (shows primitive name)
    {
        const tracer = new StackTracer();
        tracer.enterFrame({
            name: '<primitive:+>',
            source: null,
            env: {}
        });

        assert(logger, 'primitive frame name', tracer.getStack()[0].name, '<primitive:+>');
    }

    // Test: Deep stack (100+ frames) handling
    logger.title('StackTracer - Edge Cases');
    {
        const tracer = new StackTracer();
        for (let i = 0; i < 150; i++) {
            tracer.enterFrame({ name: `frame-${i}`, source: null, env: {} });
        }

        assert(logger, 'deep stack depth', tracer.getDepth(), 150);
        assert(logger, 'deep stack top frame', tracer.getStack()[149].name, 'frame-149');
    }

    // Test: Empty stack edge case
    {
        const tracer = new StackTracer();

        assert(logger, 'empty stack depth is 0', tracer.getDepth(), 0);
        assert(logger, 'empty getStack returns empty array', tracer.getStack().length, 0);
    }

    // Test: Frame includes source location
    {
        const tracer = new StackTracer();
        const source = { filename: 'test.scm', line: 10, column: 5 };
        tracer.enterFrame({ name: 'test', source, env: {} });

        const frame = tracer.getStack()[0];
        assert(logger, 'frame has source', frame.source !== null, true);
        assert(logger, 'frame source filename', frame.source.filename, 'test.scm');
        assert(logger, 'frame source line', frame.source.line, 10);
    }

    // Test: Frame includes environment reference
    {
        const tracer = new StackTracer();
        const env = { bindings: { x: 42, y: 'hello' } };
        tracer.enterFrame({ name: 'test', source: null, env });

        const frame = tracer.getStack()[0];
        assert(logger, 'frame has env', frame.env !== null, true);
        assert(logger, 'frame env is correct', frame.env.bindings.x, 42);
    }

    // Test: getCurrentFrame returns top frame
    {
        const tracer = new StackTracer();
        tracer.enterFrame({ name: 'bottom', source: null, env: {} });
        tracer.enterFrame({ name: 'top', source: null, env: {} });

        const current = tracer.getCurrentFrame();
        assert(logger, 'getCurrentFrame returns top', current.name, 'top');
    }

    // Test: getCurrentFrame returns null when empty
    {
        const tracer = new StackTracer();

        assert(logger, 'getCurrentFrame null when empty', tracer.getCurrentFrame(), null);
    }

    // Test: exitFrame on empty stack (graceful handling)
    {
        const tracer = new StackTracer();
        tracer.exitFrame(); // Should not throw

        assert(logger, 'exitFrame on empty does not throw', tracer.getDepth(), 0);
    }
}

export default runStackTracerTests;
