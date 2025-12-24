import { assert, createTestLogger } from '../../harness/helpers.js';
import { IfFrame, SetFrame, DefineFrame, BeginFrame, LetFrame, Literal, Variable } from '../../../src/core/interpreter/ast.js';


export function runFramesTests(logger) {
    logger.title('Running Frames Unit Tests...');

    // Mock registers: [ans, ctl, env, fstack]
    const createRegisters = () => [null, null, { id: 'env1' }, []];

    // 1. IfFrame
    // (if test consequent alternative)
    const env = { id: 'env-parent' };
    const consequent = new Literal("cons");
    const alternative = new Literal("alt");
    const ifFrame = new IfFrame(env, consequent, alternative);

    // Case A: Test is true
    const regs1 = createRegisters();
    regs1[0] = true; // ans (test result)
    ifFrame.step(regs1);

    assert(logger, "IfFrame(true) sets ctl", regs1[1], consequent);
    assert(logger, "IfFrame restores env", regs1[2], env);

    // Case B: Test is false
    const regs2 = createRegisters();
    regs2[0] = false; // ans
    ifFrame.step(regs2);

    assert(logger, "IfFrame(false) sets ctl", regs2[1], alternative);

    // 2. SetFrame
    // (set! var val) -> SetFrame waits for val
    const setEnv = { set: (k, v) => { setEnv.last = [k, v]; } };
    const setFrame = new SetFrame(setEnv, "x");

    const regs3 = createRegisters();
    regs3[0] = 99; // ans (the value)
    setFrame.step(regs3);

    assert(logger, "SetFrame sets value", setEnv.last[0] === "x" && setEnv.last[1] === 99, true);
    assert(logger, "SetFrame sets ans (undefined)", regs3[0], undefined);
    assert(logger, "SetFrame does NOT set ctl (continues to loop)", regs3[1], null); // step returns undefined? 
    // Wait, frames usually return undefined in step? 
    // They modify registers. If ctl is null, loop checks fstack.
    // SetFrame.step modifies registers[1] (ctl) to null implied? 
    // No, step returns nothing. ctl is null. Correct.

    // 3. BeginFrame
    // (begin exp1 exp2) -> evaluates exp1, then BeginFrame gets result, sets ctl to exp2
    const beginEnv = { id: 'env' };
    const exps = [new Literal(2), new Literal(3)];
    const beginFrame = new BeginFrame(beginEnv, exps);

    const regs4 = createRegisters();
    regs4[0] = 1; // result of first expression
    beginFrame.step(regs4);

    assert(logger, "BeginFrame sets ctl to next exp", regs4[1].value, 2);
    assert(logger, "BeginFrame updates exps list", beginFrame.expressions.length, 1); // consumed one

    // 4. LetFrame
    // (let ((x 1)) body)
    // Actually LetFrame is simpler? R7RS `let` desugared to `Lambda` + `App`.
    // Layer 1 `Let` might be simple binding?
    // LetFrame (env, var, body)
    // Step: ans is val. extend env. run body.
    const letEnv = { extend: (k, v) => ({ parent: letEnv, k, v }) };
    const body = new Literal("body");
    const letFrame = new LetFrame(letEnv, "x", body);

    const regs5 = createRegisters();
    regs5[0] = 100; // val
    letFrame.step(regs5);

    assert(logger, "LetFrame extends env", regs5[2].v, 100);
    assert(logger, "LetFrame sets ctl", regs5[1], body);

}
