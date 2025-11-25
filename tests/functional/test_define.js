import { run, assert, createTestLogger, createTestEnv } from '../helpers.js';

/**
 * Runs define tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runDefineTests(interpreter, logger) {
    logger.title("Define Special Form Tests");

    const tests = [
        {
            name: "Basic Definition",
            code: `
              (begin
                (define x 10)
                x)
            `,
            expected: 10
        },
        {
            name: "Function Definition",
            code: `
              (begin
                (define (sq x) (* x x))
                (sq 5))
            `,
            expected: 25
        },
        {
            name: "Redefinition",
            code: `
              (begin
                (define x 1)
                (define x 2)
                x)
            `,
            expected: 2
        },
        {
            name: "Re-assignment",
            code: `
              (begin
                (define x 10)
                (set! x 20)
                x)
            `,
            expected: 20
        },
        {
            name: "Nested Define (Shadowing)",
            code: `
              (begin
                (define x 1)
                (define (foo)
                  (define x 2)
                  x)
                (foo))
            `,
            expected: 2
        },
        {
            name: "Nested Define (Outer Unchanged)",
            code: `
              (begin
                (define x 1)
                (define (foo)
                  (define x 2)
                  x)
                (foo)
                x)
            `,
            expected: 1
        },
        {
            name: "Nested Define (Mutual Recursion)",
            code: `
              (begin
                (define (is-even? n)
                  (define (even? n) (if (= n 0) #t (odd? (- n 1))))
                  (define (odd? n) (if (= n 0) #f (even? (- n 1))))
                  (even? n))
                (is-even? 4))
            `,
            expected: true
        }
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

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runDefineTests(interpreter, logger);
}
