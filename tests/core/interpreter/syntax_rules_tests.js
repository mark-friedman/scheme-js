import { assert, createTestLogger } from '../../helpers.js';
import { compileSyntaxRules } from '../../../src/core/interpreter/syntax_rules.js';
import { parse } from '../../../src/core/interpreter/reader.js';
import { Symbol, intern } from '../../../src/core/interpreter/symbol.js';
import { list, cons } from '../../../src/core/interpreter/cons.js';

export function runSyntaxRulesUnitTests(logger) {
    logger.title('Running Syntax Rules Unit Tests...');

    // Helper to parse pattern/template
    const p = (str) => parse(str)[0];

    // 1. Simple Substitution
    // (syntax-rules () ((_ x) x))
    const clauses = [
        [p("(_ x)"), p("x")]
    ];
    const literals = [];
    const transformer = compileSyntaxRules(literals, clauses);

    const input = p("(macro 10)");
    const output = transformer(input);

    assert(logger, "Identity expansion", output, 10);

    // 2. Literals matching
    // (syntax-rules (else) ((_ else) 1) ((_ x) 2))
    const clauses2 = [
        [p("(_ else)"), p("1")],
        [p("(_ x)"), p("2")]
    ];
    const literals2 = [intern('else')];
    const transformer2 = compileSyntaxRules(literals2, clauses2);

    // (macro else) -> 1
    const res1 = transformer2(p("(macro else)"));
    assert(logger, "Literal match", res1, 1);

    // (macro other) -> 2
    const res2 = transformer2(p("(macro other)"));
    assert(logger, "Non-literal match", res2, 2);

    // 3. Ellipsis
    // (syntax-rules () ((_ x ...) (list x ...)))
    const clauses3 = [
        [p("(_ x ...)"), p("(list x ...)")]
    ];
    const transformer3 = compileSyntaxRules(literals, clauses3);

    const res3 = transformer3(p("(macro 1 2 3)"));
    // output: (list 1 2 3)
    // check structure
    assert(logger, "Ellipsis expansion car", res3.car.name, 'list');
    assert(logger, "Ellipsis expansion length", res3.cdr.toArray().length, 3);
}
