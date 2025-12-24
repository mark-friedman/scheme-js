import { run, assert } from '../harness/helpers.js';

export async function runEvalApplyTests(interpreter, logger) {
    logger.title('Eval & Apply Tests');

    try {
        // --- Apply Tests ---

        // 1. Basic apply
        const res1 = run(interpreter, '(apply + \'(1 2 3))');
        assert(logger, 'apply + (1 2 3)', res1, 6);

        // 2. Apply with extra args
        const res2 = run(interpreter, '(apply + 1 2 \'(3 4))');
        assert(logger, 'apply + 1 2 (3 4)', res2, 10);

        // 3. Apply with empty list
        const res3 = run(interpreter, '(apply + \'())');
        assert(logger, 'apply + ()', res3, 0);

        // 4. Apply with user function
        run(interpreter, '(define (f x y) (+ x y))');
        const res4 = run(interpreter, '(apply f \'(10 20))');
        assert(logger, 'apply user-func', res4, 30);

        // --- Eval Tests ---

        // 5. Basic eval
        const res5 = run(interpreter, '(eval \'(+ 1 2) (interaction-environment))');
        assert(logger, 'eval (+ 1 2)', res5, 3);

        // 6. Eval with defined variable
        run(interpreter, '(define x 100)');
        const res6 = run(interpreter, '(eval \'x (interaction-environment))');
        assert(logger, 'eval variable', res6, 100);

        // 7. Eval definition
        run(interpreter, '(eval \'(define y 200) (interaction-environment))');
        const res7 = run(interpreter, 'y');
        assert(logger, 'eval define', res7, 200);

        // --- TCO Verification ---

        // 8. Apply TCO
        // If TCO fails, this recursion depth will likely blow the stack (depending on JS engine limits, but 10000 is usually enough)
        run(interpreter, `
            (define (loop-apply n)
                (if (= n 0)
                    'done
                    (apply loop-apply (list (- n 1)))))
        `);
        const res8 = run(interpreter, '(loop-apply 2000000)');
        assert(logger, 'apply TCO', res8, 'done');

        // 9. Eval TCO
        // Note: We need to quote the expression carefully.
        // (eval '(loop-eval (- n 1)) ...)
        run(interpreter, `
            (define (loop-eval n)
                (if (= n 0)
                    'done
                    (eval (list 'loop-eval (- n 1)) (interaction-environment))))
        `);
        const res9 = run(interpreter, '(loop-eval 2000000)'); // Lower count for eval as it's slower, but still recursive
        assert(logger, 'eval TCO', res9, 'done');

    } catch (e) {
        logger.fail(`Eval/Apply tests crashed: ${e.message}`);
        console.error(e);
    }
}
