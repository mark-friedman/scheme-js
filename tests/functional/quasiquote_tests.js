import { run, assert, createTestLogger, createTestEnv } from '../helpers.js';

/**
 * Runs quasiquote tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runQuasiquoteTests(interpreter, logger) {
    logger.title("Quasiquote Tests");

    const tests = [
        {
            name: "Simple Quasiquote Atom",
            code: "`x",
            expected: "x" // Symbol x
        },
        {
            name: "Simple Quasiquote Number",
            code: "`1",
            expected: 1
        },
        {
            name: "Simple Quasiquote List",
            code: "`(1 2 3)",
            expected: [1, 2, 3]
        },
        {
            name: "Unquote",
            code: "`(1 ,(+ 1 1) 3)",
            expected: [1, 2, 3]
        },
        {
            name: "Unquote Splicing",
            code: "`(1 ,@(list 2 3) 4)",
            expected: [1, 2, 3, 4]
        },
        {
            name: "Nested Splicing",
            code: "`(a ,@(list 1 2) b)",
            expected: ["a", 1, 2, "b"] // "a" and "b" are symbols (Variables)
        },
        {
            name: "Splicing empty list",
            code: "`(1 ,@(list) 2)",
            expected: [1, 2]
        },
        {
            name: "Nested Quasiquote",
            code: "(let ((x 10)) `(1 `(2 ,,x) 3))",
            // Expected: (1 (quasiquote (2 (unquote 10))) 3)
            // In JS structure: [1, [Variable(quasiquote), [2, [Variable(unquote), 10]]], 3]
            // But my equal helper compares Variable(name) with string.
            // So: [1, ["quasiquote", [2, ["unquote", 10]]], 3]
            expected: [1, ["quasiquote", [2, ["unquote", 10]]], 3]
        }
    ];

    for (const test of tests) {
        try {
            const result = run(interpreter, test.code);
            assert(logger, test.name, result, test.expected);
        } catch (e) {
            logger.fail(`${test.name}: Crashed - ${e.message}`);
            console.error(e);
        }
    }
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runQuasiquoteTests(interpreter, logger);
}
