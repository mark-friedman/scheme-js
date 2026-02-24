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
        assert(logger, 'n value is "5"', nEntry.value, '5');
        assert(logger, 'n type is number', nEntry.type, 'number');

        // Find the 'result' entry
        const resultEntry = locals.find(l => l.name === 'result');
        assert(logger, 'result entry exists', !!resultEntry, true);
        assert(logger, 'result value is "120"', resultEntry.value, '120');

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
        assert(logger, 'frame 0 x value', x.value, '10');

        // Frame 1 (top) should have y
        const locals1 = api.getLocals(1);
        const y = locals1.find(l => l.name === 'y');
        assert(logger, 'frame 1 has y', !!y, true);
        assert(logger, 'frame 1 y value', y.value, '20');

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
        assert(logger, 'eval (+ n 1) = "6"', result, '6');

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
        assert(logger, 'eval defaults to top frame', result, '999');

        // Explicit frame 0 (bottom)
        const result0 = api.eval('x', 0);
        assert(logger, 'eval in frame 0', result0, '100');

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

    // Test: eval with invalid frame index
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;

        const result = api.eval('(+ 1 2)', 999);
        assert(logger, 'eval with invalid frame returns error', result.startsWith('#<error:'), true);
    }

    // =========================================================================
    // Stepping delegation
    // =========================================================================
    logger.title('__schemeDebug API - Stepping');

    // Test: stepInto delegates to probe runtime
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;
        const runtime = globalThis.__schemeProbeRuntime;

        // Record state before
        const wasStepping = runtime._stepping;

        api.stepInto();
        assert(logger, 'stepInto sets _stepping', runtime._stepping, true);

        // Reset
        runtime._stepping = wasStepping;
    }

    // Test: stepOver delegates to probe runtime
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;
        const runtime = globalThis.__schemeProbeRuntime;

        api.stepOver();
        assert(logger, 'stepOver sets _stepping', runtime._stepping, true);
        runtime._stepping = false;
    }

    // Test: stepOut delegates to probe runtime
    {
        createTestSetup();
        const api = globalThis.__schemeDebug;
        const runtime = globalThis.__schemeProbeRuntime;

        api.stepOut();
        assert(logger, 'stepOut sets _stepping', runtime._stepping, true);
        runtime._stepping = false;
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
    }

    // Clean up global
    delete globalThis.__schemeDebug;
}
