
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { assert, createTestLogger } from '../harness/helpers.js';

const logger = createTestLogger({ verbose: true });

async function runTests() {
    logger.title('REPL Mode Switching Tests');

    // 1. Setup Environment
    const { interpreter, env } = createInterpreter();
    const runtime = new SchemeDebugRuntime();
    interpreter.setDebugRuntime(runtime);

    // Mock Methods to track calls
    let runSyncCalled = 0;
    let runDebugCalled = 0;

    // We hold references to original methods to actually execute if needed,
    // or we can just mock the return for this test.
    const originalRun = interpreter.run.bind(interpreter);
    const originalRunDebug = interpreter.runDebug.bind(interpreter);

    interpreter.run = (ast, env, stack, thisCtx, options) => {
        runSyncCalled++;
        return 'sync-result';
    };

    interpreter.runDebug = async (ast, env, options) => {
        runDebugCalled++;
        return 'async-result';
    };

    // The Logic to be tested (Simulating the Logic we will put in REPLs)
    async function simulatedReplEval(code) {
        // This logic mimics what we will put in repl.js and web/repl.js
        if (interpreter.debugRuntime && interpreter.debugRuntime.enabled) {
            // Debug Mode -> Async
            return interpreter.runDebug(null, env);
        } else {
            // Fast Mode -> Sync
            return interpreter.run(null, env);
        }
    }

    // Test 1: Default State (Debug On)
    logger.log('Test 1: Default State (Debug On)');
    runSyncCalled = 0;
    runDebugCalled = 0;

    // Ensure enabled by default
    if (!runtime.enabled) runtime.enable();

    await simulatedReplEval('(test)');

    if (runDebugCalled === 1 && runSyncCalled === 0) {
        logger.pass('Called runDebug in Debug Mode');
    } else {
        logger.fail(`Expected runDebug call. Debug: ${runDebugCalled}, Sync: ${runSyncCalled}`);
    }

    // Test 2: Debug Off
    logger.log('Test 2: Debug Off');
    runSyncCalled = 0;
    runDebugCalled = 0;

    runtime.disable(); // :debug off

    await simulatedReplEval('(test)');

    if (runSyncCalled === 1 && runDebugCalled === 0) {
        logger.pass('Called run in Fast Mode');
    } else {
        logger.fail(`Expected run call. Debug: ${runDebugCalled}, Sync: ${runSyncCalled}`);
    }

    // Test 3: Debug On again
    logger.log('Test 3: Re-enable Debug');
    runSyncCalled = 0;
    runDebugCalled = 0;

    runtime.enable(); // :debug on

    await simulatedReplEval('(test)');

    if (runDebugCalled === 1 && runSyncCalled === 0) {
        logger.pass('Called runDebug after re-enabling');
    } else {
        logger.fail(`Expected runDebug call. Debug: ${runDebugCalled}, Sync: ${runSyncCalled}`);
    }

    logger.summary();
}

runTests().catch(e => console.error(e));
