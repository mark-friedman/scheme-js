import { run, assert, createTestLogger, createTestEnv } from '../helpers.js';

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
            assert(logger, test.name, result, test.expected);
        } catch (e) {
            logger.fail(`${test.name}: Crashed - ${e.message}`);
        }
    }
}
