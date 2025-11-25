import { run, createTestLogger, createTestEnv } from '../helpers.js';
import { Variable } from '../../src/syntax/ast.js';

export function runQuoteTests(interpreter, logger) {
    logger.title("Quote Tests");

    const tests = [
        { name: "Quote Basic", code: "(quote a)", expected: "a" },
        { name: "Quote List", code: "(quote (1 2 3))", expected: [1, 2, 3] },
        { name: "Quote Nested", code: "(quote (a (b c)))", expected: ["a", ["b", "c"]] },
        { name: "Quote Shorthand", code: "'a", expected: "a" },
        { name: "Quote Shorthand List", code: "'(1 2 3)", expected: [1, 2, 3] }
    ];

    for (const test of tests) {
        try {
            const result = run(interpreter, test.code);

            // Helper to compare results (simplified version of quasiquote_tests one)
            const equal = (a, b) => {
                if (Array.isArray(a) && Array.isArray(b)) {
                    if (a.length !== b.length) return false;
                    return a.every((val, i) => equal(val, b[i]));
                }
                if (a instanceof Variable) return a.name === b;
                return a === b;
            };

            if (equal(result, test.expected)) {
                logger.pass(test.name);
            } else {
                logger.fail(`${test.name}: Expected ${JSON.stringify(test.expected)}, got ${JSON.stringify(result)}`);
            }
        } catch (e) {
            logger.fail(`${test.name}: Crashed - ${e.message}`);
        }
    }
}
