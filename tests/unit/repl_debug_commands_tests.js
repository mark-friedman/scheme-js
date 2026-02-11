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

        const debugOffResult = await commands.execute(':debug off');
        assert(logger, 'debug toggle off',
            debugOffResult.startsWith(';; Debugging disabled'), true);

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

        runtime.stackTracer.enterFrame({
            name: 'test-proc',
            source: { filename: 'test.scm', line: 20 },
            env: mockEnv
        });

        // Mock pause state by pushing a fake pause info onto the backend stack
        backend._pauseInfoStack.push({ reason: 'breakpoint', source: { filename: 'test.scm', line: 20 } });

        // Disable break-on-exception for eval error test — we want the error
        // to propagate to handleEval's catch, not create a nested debug level.
        runtime.exceptionHandler.breakOnUncaughtException = false;

        const locals = await commands.execute(':locals');
        // BigInt 42n is serialized with description '42n' by StateInspector
        assert(logger, 'show locals contains variable',
            locals.includes('debug-var') && (locals.includes('42n') || locals.includes('42')), true);

        const checkVar = await commands.execute(':eval debug-var');
        assert(logger, 'eval variable direct',
            checkVar, ';; result: 42');

        const evalResult = await commands.execute(':eval (+ debug-var #e8)');
        assert(logger, 'eval in scope',
            evalResult, ';; result: 50');

        const evalError = await commands.execute(':eval (undefined-var)');
        assert(logger, 'eval error handling',
            evalError.includes('Error during eval'), true);

        runtime.exceptionHandler.breakOnUncaughtException = true;
        runtime.stackTracer.exitFrame();
        backend._pauseInfoStack.pop();
    }

    // Test: Stack Navigation
    {
        runtime.stackTracer.enterFrame({ name: 'frame-0', env: interpreter.globalEnv });
        runtime.stackTracer.enterFrame({ name: 'frame-1', env: interpreter.globalEnv });

        backend._pauseInfoStack.push({ reason: 'breakpoint' });

        assert(logger, 'initial frame is newest',
            commands._getSelectedIndex(runtime.getStack()), 1);

        await commands.execute(':up');
        assert(logger, 'frame up',
            commands._getSelectedIndex(runtime.getStack()), 0);

        await commands.execute(':up'); // stay at oldest
        assert(logger, 'frame up at boundary',
            commands._getSelectedIndex(runtime.getStack()), 0);

        await commands.execute(':down');
        assert(logger, 'frame down',
            commands._getSelectedIndex(runtime.getStack()), 1);

        runtime.stackTracer.clear();
        backend._pauseInfoStack.pop();
        commands.resetSelection();
    }

    interpreter.setDebugRuntime(null);
}

export default runReplDebugCommandsTests;
