import { assert, createTestLogger } from '../../harness/helpers.js';
import { LiteralNode, VariableNode, IfNode, BeginNode, SetNode, DefineNode, AppFrame, IfFrame, SetFrame, DefineFrame, BeginFrame } from '../../../src/core/interpreter/ast.js';
import { Symbol, intern } from '../../../src/core/interpreter/symbol.js';

export function runNodesTests(logger) {
    logger.title('Running Nodes Unit Tests...');

    // Mock registers
    // [ans, ctl, env, fstack]
    const createRegisters = () => [null, null, { lookup: () => 10, set: () => { } }, []];

    // 1. LiteralNode
    const regs1 = createRegisters();
    const lit = new LiteralNode(42);
    regs1[1] = lit;
    const res1 = lit.step(regs1);

    assert(logger, "LiteralNode returns false (halt)", res1, false);
    assert(logger, "LiteralNode sets ans", regs1[0], 42);
    assert(logger, "LiteralNode clears ctl", regs1[1], null);

    // 2. VariableNode
    const regs2 = createRegisters();
    const vari = new VariableNode("x");
    regs2[1] = vari;
    const res2 = vari.step(regs2);

    assert(logger, "VariableNode returns false", res2, false);
    assert(logger, "VariableNode lookup", regs2[0], 10); // Mock env returns 10

    // 3. IfNode
    const regs3 = createRegisters();
    const ifNode = new IfNode(new LiteralNode(true), new LiteralNode("consequent"), new LiteralNode("alternate"));
    regs3[1] = ifNode;
    const res3 = ifNode.step(regs3);

    assert(logger, "IfNode returns true (continue)", res3, true);
    assert(logger, "IfNode pushes frame", regs3[3].length, 1);
    assert(logger, "IfNode frame type", regs3[3][0] instanceof IfFrame, true);
    assert(logger, "IfNode sets ctl to test", regs3[1] instanceof LiteralNode && regs3[1].value === true, true);

    // 4. SetNode
    const regs4 = createRegisters();
    const setNode = new SetNode("x", new LiteralNode(99));
    regs4[1] = setNode;
    const res4 = setNode.step(regs4);

    assert(logger, "SetNode pushes frame", regs4[3].length, 1);
    assert(logger, "SetNode frame type", regs4[3][0] instanceof SetFrame, true);

    // 5. DefineNode
    const regs5 = createRegisters();
    const defNode = new DefineNode("x", new LiteralNode(99));
    regs5[1] = defNode;
    const res5 = defNode.step(regs5);

    assert(logger, "DefineNode pushes frame", regs5[3].length, 1);
    assert(logger, "DefineNode frame type", regs5[3][0] instanceof DefineFrame, true);

    // 6. BeginNode
    const regs6 = createRegisters();
    const beginNode = new BeginNode([new LiteralNode(1), new LiteralNode(2)]);
    regs6[1] = beginNode;
    const res6 = beginNode.step(regs6);

    assert(logger, "BeginNode pushes frame", regs6[3].length, 1);
    assert(logger, "BeginNode frame type", regs6[3][0] instanceof BeginFrame, true);
    assert(logger, "BeginNode sets ctl to first expr", regs6[1] instanceof LiteralNode && regs6[1].value === 1, true);

}
