import { assert, createTestLogger } from '../../helpers.js';
import { Literal, Variable, If, Begin, Set, Define, AppFrame, IfFrame, SetFrame, DefineFrame, BeginFrame } from '../../../src/core/interpreter/ast.js';
import { Symbol, intern } from '../../../src/core/interpreter/symbol.js';

export function runNodesTests(logger) {
    logger.title('Running Nodes Unit Tests...');

    // Mock registers
    // [ans, ctl, env, fstack]
    const createRegisters = () => [null, null, { lookup: () => 10, set: () => { } }, []];

    // 1. Literal
    const regs1 = createRegisters();
    const lit = new Literal(42);
    regs1[1] = lit;
    const res1 = lit.step(regs1);

    assert(logger, "Literal returns false (halt)", res1, false);
    assert(logger, "Literal sets ans", regs1[0], 42);
    assert(logger, "Literal clears ctl", regs1[1], null);

    // 2. Variable
    const regs2 = createRegisters();
    const vari = new Variable("x");
    regs2[1] = vari;
    const res2 = vari.step(regs2);

    assert(logger, "Variable returns false", res2, false);
    assert(logger, "Variable lookup", regs2[0], 10); // Mock env returns 10

    // 3. If
    const regs3 = createRegisters();
    const ifNode = new If(new Literal(true), new Literal("consequent"), new Literal("alternate"));
    regs3[1] = ifNode;
    const res3 = ifNode.step(regs3);

    assert(logger, "If returns true (continue)", res3, true);
    assert(logger, "If pushes frame", regs3[3].length, 1);
    assert(logger, "If frame type", regs3[3][0] instanceof IfFrame, true);
    assert(logger, "If sets ctl to test", regs3[1] instanceof Literal && regs3[1].value === true, true);

    // 4. Set
    const regs4 = createRegisters();
    const setNode = new Set("x", new Literal(99));
    regs4[1] = setNode;
    const res4 = setNode.step(regs4);

    assert(logger, "Set pushes frame", regs4[3].length, 1);
    assert(logger, "Set frame type", regs4[3][0] instanceof SetFrame, true);

    // 5. Define
    const regs5 = createRegisters();
    const defNode = new Define("x", new Literal(99));
    regs5[1] = defNode;
    const res5 = defNode.step(regs5);

    assert(logger, "Define pushes frame", regs5[3].length, 1);
    assert(logger, "Define frame type", regs5[3][0] instanceof DefineFrame, true);

    // 6. Begin
    const regs6 = createRegisters();
    const beginNode = new Begin([new Literal(1), new Literal(2)]);
    regs6[1] = beginNode;
    const res6 = beginNode.step(regs6);

    assert(logger, "Begin pushes frame", regs6[3].length, 1);
    assert(logger, "Begin frame type", regs6[3][0] instanceof BeginFrame, true);
    assert(logger, "Begin sets ctl to first expr", regs6[1] instanceof Literal && regs6[1].value === 1, true);

}
