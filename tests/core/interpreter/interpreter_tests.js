import { assert, createTestLogger } from '../../harness/helpers.js';
import { Interpreter } from '../../../src/core/interpreter/interpreter.js';
import { Closure, createClosure } from '../../../src/core/interpreter/values.js';

export function runInterpreterTests(logger) {
    logger.title('Running Interpreter Unit Tests...');

    const interpreter = new Interpreter();
    interpreter.setGlobalEnv({ mocked: true });

    // 1. Initial State
    assert(logger, "Depth starts at 0", interpreter.depth, 0);
    assert(logger, "Global env set", interpreter.globalEnv.mocked, true);

    // 2. Step Logic (Mocked)


    // Case A: AST Step

    let stepCalled = false;
    const mockAst = {
        step: (regs) => {
            stepCalled = true;
            return true; // continue
        }
    };
    const regs1 = [null, mockAst, null, []];
    const res1 = interpreter.step(regs1);

    assert(logger, "Step calls AST.step", stepCalled, true);
    assert(logger, "Step returns true (from AST)", res1, true);

    // Case B: Frame Stack Restore

    let frameStepCalled = false;
    const mockFrame = {
        step: (regs) => {
            frameStepCalled = true;
            return false; // halt
        }
    };
    // Registers for frame step:
    const regs2 = [null, mockFrame, null, []];
    const res2 = interpreter.step(regs2);

    assert(logger, "Step calls Frame.step", frameStepCalled, true);
    assert(logger, "Step returns false (from Frame)", res2, false);


    // 3. Callable Closures
    // Verify that closures are callable JS functions.
    // We use createClosure factory to simulate a real closure creation.
    const mockSchemaBody = { step: () => { } };
    const callableClosure = createClosure(['x'], mockSchemaBody, {}, null, interpreter);

    // Test: The result of createClosure is a function
    assert(logger, "Closures are callable functions", typeof callableClosure, 'function');



}
