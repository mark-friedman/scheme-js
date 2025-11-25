import { assert, run, createTestLogger, createTestEnv } from '../helpers.js';

/**
 * Runs functional tests (Basic, TCO, Call/CC, Async, Runtime Errors, Edge Cases).
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export async function runFunctionalTests(interpreter, logger) {

    logger.title("Basic Evaluation & Native Calls");

    let result = run(interpreter, `(+ 2 3)`);
    assert(logger, "Native '+' call", result, 5);

    result = run(interpreter, `(let ((x 10)) (* x 2))`);
    assert(logger, "'let' binding", result, 20);

    result = run(interpreter, `(if (> 10 5) "yes" "no")`);
    assert(logger, "'if' expression (true)", result, 'yes');

    // --- TCO Test ---
    logger.title("Tail Call Optimization (TCO)");

    const tcoTest = `
        (letrec ((loop (lambda (n acc)
                         (if (= n 0)
                             acc
                             (loop (- n 1) (+ acc n))))))
          (loop 5 0))`;

    result = run(interpreter, tcoTest);
    assert(logger, "TCO sum (1-5)", result, 15);

    const tcoLargeTest = `
        (letrec ((loop (lambda (n acc)
                         (if (= n 0)
                             acc
                             (loop (- n 1) (+ acc n))))))
          (loop 5000 0))`;

    try {
        result = run(interpreter, tcoLargeTest);
        assert(logger, "TCO deep recursion (n=5000)", result, 12502500);
    } catch (e) {
        logger.fail(`TCO deep recursion (n=5000) failed: ${e.message}`);
    }

    // --- Call/CC Tests ---
    logger.title("First-Class Continuations (call/cc)");

    result = run(interpreter, `
        (let ((val (+ 1 (call/cc (lambda (k) (k 10))))))
          val)`);
    assert(logger, "call/cc simple return", result, 11);

    result = run(interpreter, `
        (let ((val (+ 1 (call/cc (lambda (k) 10)))))
          val)`);
    assert(logger, "call/cc capture but don't use", result, 11);

    // --- call/cc (Capture and Invoke) ---
    logger.title("call/cc (Capture and Invoke)");

    run(interpreter, `(set! k-holder #f)`);

    result = run(interpreter, `
        (let ((val (+ 10 (call/cc (lambda (k)
                                   (begin
                                     (set! k-holder k)
                                     5))))))
          val)`);
    assert(logger, "call/cc capture and save", result, 15);

    result = run(interpreter, `(k-holder 20)`);
    assert(logger, "call/cc invoke saved continuation", result, 30);

    run(interpreter, `(set! foo (lambda () (k-holder 50)))`);

    result = run(interpreter, `(let ((x 1000)) (foo))`);
    assert(logger, "call/cc invoke from deep stack", result, 60);

    // --- Async Callback Test ---
    logger.title("Native Async Callback (JS -> Scheme)");

    run(interpreter, `
        (js-set-timeout (lambda () (display "Async callback finished!")) 
                        100)`);
    logger.log("Scheduled async callback... (check console for 'DISPLAY: Async callback finished!')", 'info');


    // --- Async call/cc Interoperability Test ---
    logger.title("Async call/cc Interoperability");

    logger.log("Starting async cancellation test...", 'info');

    run(interpreter, `(set! final-result "not-run")`);
    run(interpreter, `(set! cancel-k #f)`);

    run(interpreter, `
        (set! cancel
          (lambda (reason)
            (if cancel-k
                (let ((k cancel-k))
                  (begin
                    (set! cancel-k #f)
                    (k reason)))
                (display "no-k"))))`);

    run(interpreter, `
        (set! final-result
          (call/cc
           (lambda (k)
             (begin
               (set! cancel-k k)
               (js-fetch-data
                (lambda (data)
                  (begin
                    (if cancel-k
                        (let ((k cancel-k))
                          (begin
                            (set! cancel-k #f)
                            (k (list "success" data))))
                        (display "Data callback was cancelled"))
                    (newline))))
               "waiting"))))`);

    let immediateResult = run(interpreter, `final-result`);
    assert(logger, "Async: Initial result", immediateResult, 'waiting');

    logger.log("Simulating user cancellation...", 'info');
    run(interpreter, `(cancel "Cancelled by user")`);

    let cancelledResult = run(interpreter, `final-result`);
    assert(logger, "Async: Result after cancellation", cancelledResult, "Cancelled by user");

    await new Promise(resolve => setTimeout(resolve, 2000));

    logger.log("Async: Checking final result after 2s...", 'info');
    let finalResult = run(interpreter, `final-result`);
    assert(logger, "Async: Final result (cancellation wins)", finalResult, "Cancelled by user");

    // This log will appear *after* the async callbacks have fired,
    // because the 'await' above pauses this function.
    logger.log("...Async test assertions scheduled.", 'info');

    // --- Runtime Error Tests ---
    logger.title("Runtime Error Tests");

    try {
        run(interpreter, `(1 2)`);
        logger.fail("Runtime: Apply non-function - FAILED to throw");
    } catch (e) {
        assert(logger, "Runtime: Apply non-function", e.message.includes("Not a function"), true);
    }

    try {
        run(interpreter, `(undefined-var)`);
        logger.fail("Runtime: Unbound variable - FAILED to throw");
    } catch (e) {
        assert(logger, "Runtime: Unbound variable", e.message.includes("Unbound variable"), true);
    }

    // --- Edge Case Tests ---
    logger.title("Edge Case Tests");

    result = run(interpreter, `(begin)`);
    assert(logger, "Edge: Empty begin", result, null);

    result = run(interpreter, `(if #t 1 2)`);
    assert(logger, "Edge: If #t", result, 1);

    result = run(interpreter, `(if #f 1 2)`);
    assert(logger, "Edge: If #f", result, 2);

    // set! returns the value set
    result = run(interpreter, `(let ((x 1)) (set! x 2))`);
    assert(logger, "Edge: set! return value", result, 2);

    // letrec self-reference (limitation: returns null instead of error)
    result = run(interpreter, `(letrec ((x x)) x)`);
    assert(logger, "Edge: letrec self-reference", result, null);


    // --- Continuation Arity Tests ---
    logger.title("Continuation Arity Tests");

    // Calling k with no args -> null
    result = run(interpreter, `(call/cc (lambda (k) (k)))`);
    assert(logger, "Continuation: 0 args", result, null);

    // Calling k with multiple args -> first arg used
    result = run(interpreter, `(call/cc (lambda (k) (k 1 2 3)))`);
    assert(logger, "Continuation: multiple args", result, 1);
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    // Mock window for async tests
    global.window = {
        fetchData: (cb) => setTimeout(() => cb("Fetched data from JS"), 1000),
        setTimeout: setTimeout
    };

    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runFunctionalTests(interpreter, logger);
}
