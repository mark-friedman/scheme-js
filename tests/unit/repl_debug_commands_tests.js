/**
 * @fileoverview Unit tests for ReplDebugCommands.
 */

import { createTestLogger, assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { SchemeDebugRuntime } from '../../src/core/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from '../../src/core/debug/repl_debug_backend.js';
import { ReplDebugCommands } from '../../src/core/debug/repl_debug_commands.js';
import { Symbol } from '../../src/core/interpreter/symbol.js';

export async function runReplDebugCommandsTests(interpreter, logger) {
    logger.title('ReplDebugCommands Tests');

    const runtime = new SchemeDebugRuntime();
    const backend = new ReplDebugBackend(() => { });
    const commands = new ReplDebugCommands(interpreter, runtime, backend);
    runtime.setBackend(backend);
    interpreter.setDebugRuntime(runtime);

    // Test: isDebugCommand
    {
        assert(logger, 'identifies debug command with colon',
            commands.isDebugCommand(':break 10'), true);
        assert(logger, 'identifies non-debug command',
            commands.isDebugCommand('(define x 10)'), false);
    }

    // Test: handleDebug
    {
        runtime.disable();
        assert(logger, 'debug toggle on',
            commands.execute(':debug on'), ';; Debugging enabled');
        assert(logger, 'runtime is enabled',
            runtime.enabled, true);

        assert(logger, 'debug status check',
            commands.execute(':debug'), ';; Debugging is ON');

        assert(logger, 'debug toggle off',
            commands.execute(':debug off'), ';; Debugging disabled');
        assert(logger, 'runtime is disabled',
            runtime.enabled, false);
    }

    // Test: Breakpoints
    {
        runtime.enable();
        const output = commands.execute(':break test.scm 10');
        assert(logger, 'set breakpoint output',
            output.includes('Breakpoint bp-1 set at test.scm:10'), true);

        const list = commands.execute(':breakpoints');
        assert(logger, 'list breakpoints',
            list.includes('bp-1: test.scm:10'), true);

        const remove = commands.execute(':unbreak bp-1');
        assert(logger, 'remove breakpoint',
            remove, ';; Breakpoint bp-1 removed');

        assert(logger, 'list empty breakpoints',
            commands.execute(':breakpoints'), ';; No breakpoints set');
    }

    // Test: Locals and Eval in Scope (Simulation)
    {
        // Mock a stack frame
        const mockEnv = interpreter.globalEnv.extend();
        mockEnv.define('debug-var', 42n);

        runtime.stackTracer.enterFrame({
            name: 'test-proc',
            source: { filename: 'test.scm', line: 20 },
            env: mockEnv
        });

        // Mock pause state
        backend.paused = true;

        const locals = commands.execute(':locals');
        assert(logger, 'show locals contains variable',
            locals.includes('debug-var = 42'), true);

        const checkVar = commands.execute(':eval debug-var');
        assert(logger, 'eval variable direct',
            checkVar, ';; result: 42');

        const evalResult = commands.execute(':eval (+ debug-var #e8)');
        assert(logger, 'eval in scope',
            evalResult, ';; result: 50');

        const evalError = commands.execute(':eval (undefined-var)');
        assert(logger, 'eval error handling',
            evalError.includes('Error during eval'), true);

        runtime.stackTracer.exitFrame();
        backend.paused = false;
    }

    // Test: Stack Navigation
    {
        runtime.stackTracer.enterFrame({ name: 'frame-0', env: interpreter.globalEnv });
        runtime.stackTracer.enterFrame({ name: 'frame-1', env: interpreter.globalEnv });

        backend.paused = true;

        assert(logger, 'initial frame is newest',
            commands._getSelectedIndex(runtime.getStack()), 1);

        commands.execute(':up');
        assert(logger, 'frame up',
            commands._getSelectedIndex(runtime.getStack()), 0);

        commands.execute(':up'); // stay at oldest
        assert(logger, 'frame up at boundary',
            commands._getSelectedIndex(runtime.getStack()), 0);

        commands.execute(':down');
        assert(logger, 'frame down',
            commands._getSelectedIndex(runtime.getStack()), 1);

        runtime.stackTracer.clear();
        backend.paused = false;
        commands.resetSelection();
    }

    interpreter.setDebugRuntime(null);
}

export default runReplDebugCommandsTests;
