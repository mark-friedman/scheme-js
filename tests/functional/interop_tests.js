import { assert, run, createTestLogger, createTestEnv } from '../helpers.js';
import { Literal, TailApp } from '../../src/runtime/ast.js';

/**
 * Runs JS Interop tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runInteropTests(interpreter, logger) {
    // --- JS Interop Tests ---
    logger.title("JS Interop Tests");

    // Globally define 'loop' for TCO interop test
    run(interpreter, `
        (define loop (lambda (n acc)
                      (if (= n 0)
                          acc
                          (loop (- n 1) (+ acc n)))))`);

    logger.title("call/cc (JS Interop)");

    // Test 1: JS stores k, then JS invokes k
    run(interpreter, `(define final-result "interop-test-1")`);

    // Define helper to invoke globalK from JS
    interpreter.globalEnv.bindings.set('invoke-global-k', (val) => {
        if (window.globalK) window.globalK(val);
    });

    let result = run(interpreter, `
        (let ((val (+ 100 (call/cc (lambda (k)
                                    (begin
                                      (js-store-k k)
                                      5))))))
          (set! final-result val))`);
    assert(logger, "JS Interop: Store k (initial run)", result, 105);

    logger.log("JS Interop: Invoking k from JS...", 'info');
    run(interpreter, `(invoke-global-k 25)`);

    result = run(interpreter, `final-result`);
    assert(logger, "JS Interop: Result after JS invokes k", result, 125); // 100 + 25

    // Test 2: Capture k *inside* a JS-initiated callback
    run(interpreter, `(define callback-k #f)`);

    // Define helper to run callback
    interpreter.globalEnv.bindings.set('invoke-callback', (cb) => cb());

    run(interpreter, `
        (invoke-callback
         (lambda ()
           (let ((val (+ 1000 (call/cc (lambda (k)
                                         (begin
                                           (set! callback-k k)
                                           10))))))
             (set! final-result val))))`);

    result = run(interpreter, `final-result`);
    assert(logger, "JS Interop: Capture k inside JS callback", result, 1010);

    run(interpreter, `(callback-k 30)`);
    result = run(interpreter, `final-result`);
    assert(logger, "JS Interop: Invoke k captured in callback", result, 1030); // 1000 + 30

    // Test 3: TCO from JS-initiated callback
    run(interpreter, `(set! final-result "tco-test-not-run")`);
    try {
        run(interpreter, `
            (invoke-callback
             (lambda ()
               (set! final-result (loop 5000 0))))`);

        result = run(interpreter, `final-result`);
        assert(logger, "JS Interop: TCO from JS callback (n=5000)", result, 12502500);
    } catch (e) {
        logger.fail(`JS Interop: TCO from JS callback (n=5000) failed: ${e.message}`);
    }

    // --- JS Interop Error Tests ---
    logger.title("JS Interop Error Tests");

    if (interpreter.globalEnv) {
        // Add a throwing native function
        // Add a throwing native function
        interpreter.globalEnv.bindings.set('js-throw', () => { throw new Error("Boom from JS"); });

        try {
            run(interpreter, `(js-throw)`);
            logger.fail("JS Interop: Throw - FAILED to throw");
        } catch (e) {
            assert(logger, "JS Interop: Throw", e.message, "Boom from JS");
        }
    } else {
        logger.log("Skipping JS Interop Error test (no globalEnv access)", 'warn');
    }

    // --- Advanced JS Interop Tests ---
    logger.title("Advanced JS Interop Tests");

    if (interpreter.globalEnv) {
        // Setup: Define js-apply
        // Setup: Define js-apply
        interpreter.globalEnv.bindings.set('js-apply', (fn, ...args) => {
            // fn is a Scheme Closure (wrapped in a Bridge by AppFrame)
            // We call it as a JS function
            return fn(...args);
        });

        // 1. Sync Round-trip
        // Scheme -> JS (js-apply) -> Scheme (lambda)
        result = run(interpreter, `(+ 1 (js-apply (lambda (x) (* x 2)) 10))`);
        assert(logger, "Interop: Sync Round-trip", result, 21);

        // 2. Non-abortive call/cc
        // Scheme -> JS -> Scheme (invoking k)
        // k captures the outer continuation: (+ 1 [])
        // Inner run invokes k, which replaces inner stack with (+ 1 []).
        // Inner run returns 11.
        // js-apply aborts.
        // Outer run receives 11.
        result = run(interpreter, `(+ 1 (call/cc (lambda (k) (js-apply (lambda () (k 10))))))`);
        assert(logger, "Interop: Non-abortive call/cc", result, 11);

        // 3. Ping-Pong Recursion (TCO check across boundaries? No, JS stack grows)
        // We just want to verify it works for small N.
        interpreter.globalEnv.bindings.set('js-pong', (n) => {
            if (n === 0) return "pong-done";
            // Call scheme-ping
            const ping = interpreter.globalEnv.lookup('scheme-ping');
            // 'ping' is a Closure.
            // Since we are in JS, we need to manually wrap it if we want to call it?
            // OR, since we are in a JS function called by Scheme, 'ping' might be raw Closure.
            // Wait, 'lookup' returns the raw value from the environment.
            // The environment holds the Closure object.
            // So 'ping' is a Closure.
            // We must use interpreter.createJsBridge(ping) to call it!
            // OR use interpreter.run directly.

            // Using createJsBridge is cleaner:
            const bridge = interpreter.createJsBridge(ping);
            return bridge(n - 1);
        });

        run(interpreter, `
      (define scheme-ping (lambda (n)
        (if (= n 0)
            "ping-done"
            (js-pong (- n 1)))))
    `);

        result = run(interpreter, `(scheme-ping 10)`);
        assert(logger, "Interop: Ping-Pong (N=10)", result, "ping-done");

    }

    // --- Hybrid Interop Tests (Merged) ---
    logger.title("Hybrid Interoperability Tests");

    // 1. Scheme calling raw JS function (Scheme -> JS)
    try {
        let capturedArg = null;
        const rawJsFunc = (arg) => {
            capturedArg = arg;
            return "called-raw";
        };

        interpreter.globalEnv.define('raw-js-func', rawJsFunc);

        const result = run(interpreter, `
            (raw-js-func "hello")
        `);

        assert(logger, "Transparent Scheme -> JS call", result, "called-raw");
        assert(logger, "Argument passed correctly", capturedArg, "hello");

    } catch (e) {
        logger.fail(`Scheme -> JS failed: ${e.message}`);
    }

    // 2. JS calling Scheme closure (JS -> Scheme Bridge)
    try {
        // Define a Scheme function
        run(interpreter, `
            (define (scheme-add a b) (+ a b))
        `);

        // Get the function back to JS via interpreter.run (which applies the Bridge)
        const schemeAdd = run(interpreter, `scheme-add`);

        // In the NEW implementation, this should be a JS function (the Bridge),
        // NOT a Closure instance.
        assert(logger, "Scheme function is a JS function", typeof schemeAdd, 'function');

        // Call it directly
        const result = schemeAdd(10, 20);
        assert(logger, "Bridge returns correct result", result, 30);

    } catch (e) {
        logger.fail(`JS -> Scheme failed: ${e.message}`);
    }

    // 3. Vectors as raw Arrays
    try {
        const rawArray = [10, 20, 30];
        interpreter.globalEnv.define('my-vec', rawArray);

        // Test vector-ref
        const refResult = run(interpreter, `(vector-ref my-vec 1)`);
        assert(logger, "vector-ref on raw array", refResult, 20);

        // Test vector-length
        const lenResult = run(interpreter, `(vector-length my-vec)`);
        assert(logger, "vector-length on raw array", lenResult, 3);

        // Test vector-set!
        run(interpreter, `(vector-set! my-vec 0 99)`);
        assert(logger, "vector-set! mutated raw array", rawArray[0], 99);

    } catch (e) {
        logger.fail(`Vector/Array interop failed: ${e.message}`);
    }

    // 4. Vector Literal as raw Array
    try {
        // This relies on the Reader/Analyzer producing a raw array for #(...)
        const vec = run(interpreter, `#(1 2 3)`);
        assert(logger, "Vector literal is Array", Array.isArray(vec), true);
        assert(logger, "Vector literal content", vec[0], 1);

    } catch (e) {
        logger.fail(`Vector literal failed: ${e.message}`);
    }
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    // Mock window for interop tests
    global.window = {
        globalK: null,
        fetchData: () => { },
        setTimeout: setTimeout
    };

    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runInteropTests(interpreter, logger);
}
