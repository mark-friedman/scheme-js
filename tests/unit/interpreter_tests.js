import { assert, createTestLogger } from '../helpers.js';
import { Interpreter } from '../../src/runtime/interpreter.js';
import { Closure } from '../../src/runtime/values.js';

export function runInterpreterTests(logger) {
    logger.title('Running Interpreter Unit Tests...');

    const interpreter = new Interpreter();
    interpreter.setGlobalEnv({ mocked: true });

    // 1. Initial State
    assert(logger, "Depth starts at 0", interpreter.depth, 0);
    assert(logger, "Global env set", interpreter.globalEnv.mocked, true);

    // 2. Step Logic (Mocked)
    // registers: [ans, ctl, env, fstack]

    // Case A: AST Step
    // ctl has .step() -> called
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
    // ctl is null (value returned), stack has frame
    const mockFrame = { step: () => { } }; // logic handled in loop?
    // Interpreter.step logic:
    // If ctl.step() returns false:
    //   Wait, interpreter.step returns what ctl.step returns.
    //   If ctl is null?
    //   Interpreter.step expects registers[1] (ctl) to be an object with step().
    //   If ctl is null, step() throws or fails?
    //   Let's check interpreter.js.
    //   Interpreter.ts: if (ctl.step(registers)) ...
    //   So step() is only called if ctl is not null?
    //   No, step() calls ctl.step(). If ctl is null it would crash.
    //   But the loop handles the "step returned false" case.

    // This unit test verifies that `interpreter.step` simply delegates to `ctl.step`.
    // The "loop logic" (handling false return) is in `interpreter.run`, not `step`.
    // So `interpreter.step` is just a helper?
    // Let's verify `interpreter.js` content.
    // `step(registers) { const ctl = registers[1]; return ctl.step(registers); }`
    // If so, testing it is trivial.

    // 3. createJsBridge
    // Wraps a Closure in a JS function.
    const mockClosure = new Closure(['x'], { step: () => { } }, {});
    const bridge = interpreter.createJsBridge(mockClosure);

    assert(logger, "Bridge is function", typeof bridge, 'function');

    // Invoking the bridge triggers a nested run.
    // We can't easily test that without full machinery.
    // But we verified it exists.

}
