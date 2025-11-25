import { globalMacroRegistry } from '../../src/syntax/macro_registry.js';
import { Variable, Literal } from '../../src/syntax/ast.js';
import { run, assert } from '../helpers.js';

export async function runMacroTests(interpreter, logger) {
    logger.title('Macro Tests');

    // --- Helper: Register a macro ---
    // (my-if test then else) -> (if test then else)
    globalMacroRegistry.define('my-if', (exp) => {
        // exp is [Variable('my-if'), test, then, else]
        // We want to return [Variable('if'), test, then, else]
        return [new Variable('if'), exp[1], exp[2], exp[3]];
    });

    // --- Helper: Register a recursive macro ---
    // (my-or a b) -> (if a a b)  <-- simplistic, evaluates a twice, but good for testing expansion
    globalMacroRegistry.define('my-or', (exp) => {
        const a = exp[1];
        const b = exp[2];
        return [new Variable('if'), a, a, b];
    });

    // --- Helper: Register a macro that expands to another macro ---
    // (nested-macro x) -> (my-if x 1 2)
    globalMacroRegistry.define('nested-macro', (exp) => {
        return [new Variable('my-if'), exp[1], new Literal(1), new Literal(2)];
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

    } catch (e) {
        logger.fail(`Macro tests crashed: ${e.message}`);
    } finally {
        // Cleanup
        globalMacroRegistry.clear();
    }
}
