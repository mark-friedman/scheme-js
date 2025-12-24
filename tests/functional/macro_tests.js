import { globalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';
import { run, assert } from '../harness/helpers.js';
import { list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';

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

    } catch (e) {
        logger.fail(`Macro tests crashed: ${e.message}`);
    } finally {
        // Cleanup
        globalMacroRegistry.clear();
    }
}
