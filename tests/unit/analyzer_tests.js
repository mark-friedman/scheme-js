import { assert, createTestLogger } from '../helpers.js';
import { SyntacticAnalyzer } from '../../src/runtime/analysis/syntactic_analyzer.js';
import { Literal, Variable, If } from '../../src/runtime/ast.js';
import { list, cons } from '../../src/runtime/cons.js';
import { intern } from '../../src/runtime/symbol.js';

export function runAnalyzerTests(interpreter, logger) {
    logger.title('Running SyntacticAnalyzer Tests...');

    // Mock MacroEnv
    const mockMacroReg = {
        isMacro: () => false,
        lookup: () => null,
        define: () => { }
    };

    // 1. Instantiation
    const analyzer = new SyntacticAnalyzer(mockMacroReg);
    assert(logger, "Instance created", analyzer instanceof SyntacticAnalyzer, true);

    // 2. Basic Analysis
    const lit = analyzer.analyze(10);
    assert(logger, "Analyze number", lit instanceof Literal && lit.value === 10, true);

    const sym = analyzer.analyze(intern('x'));
    assert(logger, "Analyze symbol", sym instanceof Variable && sym.name === 'x', true);

    // 3. Special Form Registry
    logger.log("Testing registry overwrite...");

    // Create a new analyzer with a custom 'if' handler
    const customAnalyzer = new SyntacticAnalyzer(mockMacroReg);

    let handlerCalled = false;
    customAnalyzer.registerSpecialForm('if', (exp, ana) => {
        handlerCalled = true;
        return new Literal("custom-if");
    });

    const ifExp = list(intern('if'), new Literal(true), new Literal(1), new Literal(2));
    const result = customAnalyzer.analyze(ifExp);

    assert(logger, "Custom handler called", handlerCalled, true);
    assert(logger, "Custom handler result", result.value, "custom-if");

    // Verify isolation (original analyzer shouldn't be affected)
    const stdIf = analyzer.analyze(ifExp);
    assert(logger, "Isolation check", stdIf instanceof If, true);

    // 4. Test Dependency Injection
    // The handler receives the analyzer instance.
    // We can verify valid recursion.
    // (recurse-me (recurse-me 1))

    customAnalyzer.registerSpecialForm('recurse-me', (exp, ana) => {
        // exp is (recurse-me arg)
        // analyze arg
        const arg = exp.cdr.car; // cadr
        const analyzedArg = ana.analyze(arg);
        return new Literal(`wrapped-${analyzedArg.value}`);
    });

    const recurseExp = list(intern('recurse-me'), new Literal("start"));
    const recurseResult = customAnalyzer.analyze(recurseExp);
    assert(logger, "Recursion via analyzer injection", recurseResult.value, "wrapped-start");

}
