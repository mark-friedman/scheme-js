import { globalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';
import { run, assert } from '../harness/helpers.js';
import { list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import { SchemeUnboundError } from '../../src/core/interpreter/errors.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';

export async function runMacroTests(interpreter, logger) {
    logger.title('Macro Tests');

    // --- Helper: Register a macro ---
    // (my-if test then else) -> (if test then else)
    globalMacroRegistry.define('my-if', (exp) => {
        // exp is Cons(my-if, Cons(test, Cons(then, Cons(else, null))))
        const test = exp.cdr.car;
        const thenBranch = exp.cdr.cdr.car;
        const elseBranch = exp.cdr.cdr.cdr.car;

        return list(intern('if'), test, thenBranch, elseBranch);
    });

    // --- Helper: Register a recursive macro ---
    // (my-or a b) -> (if a a b)
    globalMacroRegistry.define('my-or', (exp) => {
        const a = exp.cdr.car;
        const b = exp.cdr.cdr.car;
        return list(intern('if'), a, a, b);
    });

    // --- Helper: Register a macro that expands to another macro ---
    // (nested-macro x) -> (my-if x 1 2)
    globalMacroRegistry.define('nested-macro', (exp) => {
        const x = exp.cdr.car;
        return list(intern('my-if'), x, 1, 2);
    });

    try {
        // Test 1: Basic Macro Expansion
        const result1 = run(interpreter, '(my-if #t 10 20)');
        assert(logger, 'Basic Macro Expansion (my-if #t)', result1, 10);

        const result2 = run(interpreter, '(my-if #f 10 20)');
        assert(logger, 'Basic Macro Expansion (my-if #f)', result2, 20);

        // Test 2: Recursive Expansion (macro output is analyzed again)
        // (nested-macro #t) -> (my-if #t 1 2) -> (if #t 1 2) -> 1
        const result3 = run(interpreter, '(nested-macro #t)');
        assert(logger, 'Recursive Macro Expansion', result3, 1);

        // Test 3: define-syntax parsing (should not crash)
        // We expect it to return null (void)
        const result4 = run(interpreter, '(define-syntax foo (syntax-rules () ((_) 1)))');
        assert(logger, 'define-syntax parsing', result4, null);

        // Test 4: Malformed define-syntax
        try {
            run(interpreter, '(define-syntax foo)');
            logger.fail('Malformed define-syntax should throw');
        } catch (e) {
            logger.pass('Malformed define-syntax threw error');
        }

        // Test 5: Hygienic Shadowing (Referential Transparency - Standard Lib)
        // Macro 'use-list' uses 'list' (global). Shadowing 'list' should not affect it.
        run(interpreter, `
            (define-syntax use-list
                (syntax-rules ()
                    ((_) (list 1 2))))
        `);

        // Shadow 'list' with a symbol 'broken
        const hygieneResult1 = run(interpreter, `
            (let ((list 'broken))
                (use-list))
        `);
        // Should evaluate to (1 2)
        assert(logger, 'Hygiene: Ref Transparency (Standard Lib)', hygieneResult1.car, 1);

        // Test 6: Hygienic Shadowing (Referential Transparency - User Global)
        run(interpreter, `(define hidden 100)`);
        run(interpreter, `
            (define-syntax return-hidden
                (syntax-rules ()
                    ((_) hidden)))
        `);

        const hygieneResult2 = run(interpreter, `
            (let ((hidden 200))
                (return-hidden))
        `);
        assert(logger, 'Hygiene: Ref Transparency (User Global)', hygieneResult2, 100);

        // Test 7: Hygienic Renaming (Avoid Capture)
        // Macro introduces 'tmp'. User uses 'tmp'. No collision.
        run(interpreter, `
            (define-syntax hygienic-swap
                (syntax-rules ()
                    ((hygienic-swap a b)
                     (let ((tmp a))
                        (set! a b)
                        (set! b tmp)))))
        `);

        run(interpreter, `(define tmp 10)`);
        run(interpreter, `(define val 20)`);
        run(interpreter, `(hygienic-swap tmp val)`);

        const tmpVal = run(interpreter, `tmp`);
        assert(logger, 'Hygiene: Renaming (avoid capture)', tmpVal, 20);

        // Test 8: Internal define-syntax scoping
        // define-syntax inside let should not leak to outer scope
        {
            const code = `
                (let
                  ((helper (lambda (x) (* x 2))))
                    (define-syntax my-double (syntax-rules () ((my-double x) (helper x)))))
            `;

            try {
                run(interpreter, code);
                logger.pass('Internal define-syntax definition succeeded');
            } catch (e) {
                logger.fail(`Internal define-syntax definition failed: ${e.message}`);
            }

            // Attempt to call my-double outside the let
            const callCode = `(my-double 5)`;
            try {
                run(interpreter, callCode);
                logger.fail("Should have thrown Unbound variable error, but succeeded");
            } catch (e) {
                // Check error type instead of message string
                if (e instanceof SchemeUnboundError) {
                    logger.pass("Correctly threw SchemeUnboundError for out-of-scope macro");
                } else {
                    logger.fail(`Threw wrong error type: ${e.constructor.name} - ${e.message}`);
                }
            }
        }

        // Test 9: Local macro masking global macro
        {
            // Define global macro
            run(interpreter, `
                (define-syntax masked-foo (syntax-rules () ((_) 'global)))
            `);

            // Mask locally
            const code = `
                (let ()
                    (define-syntax masked-foo (syntax-rules () ((_) 'local)))
                    (masked-foo))
            `;

            const result = run(interpreter, code);
            if (result && result.name === 'local') {
                logger.pass("Local macro masked global macro");
            } else {
                logger.fail(`Local macro failed to mask global. Got: ${result}`);
            }

            // Verify global is restored
            const resultGlobal = run(interpreter, `(masked-foo)`);
            if (resultGlobal && resultGlobal.name === 'global') {
                logger.pass("Global macro restored after let");
            } else {
                logger.fail(`Global macro not restored. Got: ${resultGlobal}`);
            }
        }

        // Test 10: Custom Ellipsis Identifier
        // (syntax-rules ::: () ...)
        run(interpreter, `
            (define-syntax list-custom-ellipsis
                (syntax-rules ::: ()
                    ((_ x :::) (list x :::))))
        `);
        const ellipsisResult = run(interpreter, `(list-custom-ellipsis 1 2 3)`);
        assert(logger, "Custom ellipsis (:::) supported", ellipsisResult, [1, 2, 3]);

        // Test 11: A define-macro transformer knows the source it came from.
        // The debugger places a procedure by its source span, so a transformer
        // without one cannot be placed. Both forms are checked: the shorthand
        // `(define-macro (name args...) body...)`, whose lambda the analyzer
        // builds, and `(define-macro name (lambda ...))`, whose lambda is read.
        {
            /**
             * Evaluates source attributed to a filename, so that spans have one.
             * @param {string} source - Scheme source.
             * @param {string} filename - The name the reader records in spans.
             * @returns {*} The value of the last form.
             */
            const evaluate = (source, filename) => {
                let result;
                for (const form of parse(source, { filename })) {
                    result = interpreter.run(analyze(form), interpreter.globalEnv);
                }
                return result;
            };

            evaluate(`(define-macro (src-swap! a b)
  (list 'let (list (list 'tmp a))
        (list 'set! a b) (list 'set! b 'tmp)))`, 'swap.scm');
            const swapped = run(interpreter,
                "(let ((p 1) (q 2)) (src-swap! p q) (list p q))");
            assert(logger, "define-macro shorthand transformer still expands", swapped, [2, 1]);

            const shorthand = globalMacroRegistry.lookup('src-swap!').transformerProcedure;
            const span = shorthand && shorthand.source;
            assert(logger, "define-macro shorthand transformer has a source span",
                span !== null && span !== undefined, true);
            assert(logger, "the span names the file", span && span.filename, 'swap.scm');
            assert(logger, "the span starts at the definition", span && span.line, 1);
            assert(logger, "the span ends where the definition does", span && span.endLine, 3);

            evaluate(`(define-macro src-twice
  (lambda (x)
    (list 'begin x x)))`, 'twice.scm');
            const explicit = globalMacroRegistry.lookup('src-twice').transformerProcedure;
            const explicitSpan = explicit && explicit.source;
            assert(logger, "define-macro with an explicit lambda: span names the file",
                explicitSpan && explicitSpan.filename, 'twice.scm');
            assert(logger, "define-macro with an explicit lambda: span is the lambda's",
                explicitSpan && [explicitSpan.line, explicitSpan.endLine], [2, 3]);
        }

    } catch (e) {
        logger.fail(`Macro tests crashed: ${e.message}`);
    }
}
