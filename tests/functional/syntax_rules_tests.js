import { run, assert } from '../harness/helpers.js';
import { globalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';

export async function runSyntaxRulesTests(interpreter, logger) {
  logger.title('Syntax-Rules Tests');

  try {
    // Test 1: Simple Substitution (my-let)
    // (my-let ((x 10)) x) -> ((lambda (x) x) 10) -> 10
    run(interpreter, `
            (define-syntax my-let
              (syntax-rules ()
                ((_ ((x v)) b)
                 ((lambda (x) b) v))))
        `);

    const result1 = run(interpreter, `(my-let ((x 10)) x)`);
    assert(logger, 'Simple Substitution (my-let)', result1, 10);

    // Test 2: Literals (my-cond)
    // (my-cond (else 100)) -> 100
    // (my-cond (#t 10)) -> 10
    run(interpreter, `
            (define-syntax my-cond
              (syntax-rules (else)
                ((_ (else result))
                 result)
                ((_ (test result))
                 (if test result #f))))
        `);

    const result2 = run(interpreter, `(my-cond (else 100))`);
    assert(logger, 'Literals (else match)', result2, 100);

    const result3 = run(interpreter, `(my-cond (#t 10))`);
    assert(logger, 'Literals (non-else match)', result3, 10);

    // Test 3: Ellipsis (my-begin)
    // (my-begin 1 2 3) -> (begin 1 2 3) -> 3
    run(interpreter, `
            (define-syntax my-begin
              (syntax-rules ()
                ((_ e ...)
                 (begin e ...))))
        `);

    const result4 = run(interpreter, `(my-begin 1 2 3)`);
    assert(logger, 'Ellipsis (my-begin)', result4, 3);

    // Test 4: Recursive Macro (my-and)
    // (my-and) -> #t
    // (my-and x) -> x
    // (my-and x y ...) -> (if x (my-and y ...) #f)
    run(interpreter, `
            (define-syntax my-and
              (syntax-rules ()
                ((_) #t)
                ((_ x) x)
                ((_ x y ...)
                 (if x (my-and y ...) #f))))
        `);

    const result5 = run(interpreter, `(my-and)`);
    assert(logger, 'Recursive (my-and empty)', result5, true);

    const result6 = run(interpreter, `(my-and #t 10)`);
    assert(logger, 'Recursive (my-and #t 10)', result6, 10);

    const result7 = run(interpreter, `(my-and #t #f 10)`);
    assert(logger, 'Recursive (my-and #t #f 10)', result7, false);

    // Test 5: Ellipsis with multiple vars (let-values style)
    // (my-list (x y) ...) -> (list (list x y) ...)
    run(interpreter, `
            (define-syntax my-pairs
              (syntax-rules ()
                ((_ (x y) ...)
                 (list (list x y) ...))))
        `);

    // (my-pairs (1 2) (3 4)) -> (list (list 1 2) (list 3 4)) -> ((1 2) (3 4))
    // Note: 'list' primitive must exist.
  } catch (e) {
    logger.fail(`Syntax-Rules tests crashed: ${e.message}`);
  } finally {
    globalMacroRegistry.clear();
  }
}
