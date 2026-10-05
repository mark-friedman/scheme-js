/**
 * @fileoverview Unit tests for ReplDebugCommands.
 */

import { createTestLogger, assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from '../../src/debug/repl_debug_backend.js';
import { ReplDebugCommands } from '../../src/debug/repl_debug_commands.js';
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
            await commands.execute(':debug on'), ';; Debugging enabled');
        assert(logger, 'runtime is enabled',
            runtime.enabled, true);

        assert(logger, 'debug status check',
            await commands.execute(':debug'), ';; Debugging is ON');

        // A run that is not being debugged holds a browser's page until it
        // ends, which turning debugging off there warns of.
        const browser = !(typeof process !== 'undefined' && process.versions?.node);
        assert(logger, 'debug toggle off',
            await commands.execute(':debug off'),
            browser ? ';; Debugging disabled\n;; WARNING: Fast Mode enabled. UI will freeze during long computations.' : ';; Debugging disabled');

        assert(logger, 'runtime is disabled',
            runtime.enabled, false);
    }

    // Test: Breakpoints
    {
        runtime.enable();
        const output = await commands.execute(':break test.scm 10');
        assert(logger, 'set breakpoint output',
            output.includes('Breakpoint bp-1 set at test.scm:10'), true);

        const list = await commands.execute(':breakpoints');
        assert(logger, 'list breakpoints',
            list.includes('bp-1: test.scm:10'), true);

        const remove = await commands.execute(':unbreak bp-1');
        assert(logger, 'remove breakpoint',
            remove, ';; Breakpoint bp-1 removed');

        assert(logger, 'list empty breakpoints',
            await commands.execute(':breakpoints'), ';; No breakpoints set');
    }

    // Test: Locals and Eval in Scope (Simulation)
    {
        // Mock a stack frame
        const mockEnv = interpreter.globalEnv.extend();
        mockEnv.define('debug-var', 42n);

        runtime.enterFrame({
            name: 'test-proc',
            source: { filename: 'test.scm', line: 20 },
            env: mockEnv
        });
        runtime.pause(null, null, 'test');

        const locals = await commands.execute(':locals');
        assert(logger, 'show locals contains variable',
            locals.includes('debug-var = 42'), true);

        const checkVar = await commands.execute(':eval debug-var');
        assert(logger, 'eval variable direct',
            checkVar, ';; result: 42');

        const evalResult = await commands.execute(':eval (+ debug-var #e8)');
        assert(logger, 'eval in scope',
            evalResult, ';; result: 50');

        const evalError = await commands.execute(':eval (undefined-var)');
        assert(logger, 'eval error handling',
            evalError.includes('Error during eval'), true);

        runtime.exitFrame();
        runtime.resume();
    }

    // Test: Stack Navigation
    {
        runtime.enterFrame({ name: 'frame-0', env: interpreter.globalEnv });
        runtime.enterFrame({ name: 'frame-1', env: interpreter.globalEnv });
        runtime.pause(null, null, 'test');

        // The backtrace marks the selected frame.
        const selected = async () => (await commands.execute(':bt')).match(/=> #(\d+)/)[1];
        assert(logger, 'initial frame is newest', await selected(), '1');

        await commands.execute(':up');
        assert(logger, 'frame up', await selected(), '0');

        await commands.execute(':up'); // stay at oldest
        assert(logger, 'frame up at boundary', await selected(), '0');

        await commands.execute(':down');
        assert(logger, 'frame down', await selected(), '1');

        runtime.exitFrame();
        runtime.exitFrame();
        runtime.resume();
        commands.resetSelection();
    }

    interpreter.setDebugRuntime(null);
}

export default runReplDebugCommandsTests;
