import { assert, run, createTestLogger, createTestEnv } from '../helpers.js';
import { Literal, TailApp } from '../../src/syntax/ast.js';

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
        (set! loop (lambda (n acc)
                     (if (= n 0)
                         acc
                         (loop (- n 1) (+ acc n)))))`);

    logger.title("call/cc (JS Interop)");

    // Test 1: JS stores k, then JS invokes k
    run(interpreter, `(set! final-result "interop-test-1")`);

    let result = run(interpreter, `
        (let ((val (+ 100 (call/cc (lambda (k)
                                    (begin
                                      (js-store-k k)
                                      5))))))
          (set! final-result val))`);
    assert(logger, "JS Interop: Store k (initial run)", result, 105);

    logger.log("JS Interop: Invoking k from JS...", 'info');
    run(interpreter, `(js-invoke-k-from-js 25)`);

    result = run(interpreter, `final-result`);
    assert(logger, "JS Interop: Result after JS invokes k", result, 125); // 100 + 25

    // Test 2: Capture k *inside* a JS-initiated callback
    run(interpreter, `(set! callback-k #f)`);
    run(interpreter, `
        (js-run-callback
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
            (js-run-callback
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
        interpreter.globalEnv.bindings.set('js-throw', new (interpreter.globalEnv.lookup('+').constructor)(
            () => { throw new Error("Boom from JS"); },
            interpreter
        ));

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
        interpreter.globalEnv.bindings.set('js-apply', new (interpreter.globalEnv.lookup('+').constructor)(
            (fn, ...args) => {
                // fn is a Scheme Closure (or NativeJsFunction)
                // We call it as a JS function (the wrapper handles the interpreter.run)
                return fn(...args);
            },
            interpreter
        ));

        // 1. Sync Round-trip
        // Scheme -> JS (js-apply) -> Scheme (lambda)
        result = run(interpreter, `(+ 1 (js-apply (lambda (x) (* x 2)) 10))`);
        assert(logger, "Interop: Sync Round-trip", result, 21);

        // 2. Non-abortive call/cc
        // Scheme -> JS -> Scheme (invoking k)
        // k captures the outer continuation: (+ 1 [])
        // Inner run invokes k, which replaces inner stack with (+ 1 []).
        // Inner run returns 11.
        // js-apply returns 11.
        // Outer run receives 11 as result of call/cc.
        // Outer run computes (+ 1 11) -> 12.
        result = run(interpreter, `(+ 1 (call/cc (lambda (k) (js-apply (lambda () (k 10))))))`);
        assert(logger, "Interop: Non-abortive call/cc", result, 12);

        // 3. Ping-Pong Recursion (TCO check across boundaries? No, JS stack grows)
        // We just want to verify it works for small N.
        interpreter.globalEnv.bindings.set('js-pong', new (interpreter.globalEnv.lookup('+').constructor)(
            (n) => {
                if (n === 0) return "pong-done";
                // Call scheme-ping
                const ping = interpreter.globalEnv.lookup('scheme-ping');
                // We must invoke it via the wrapper
                // Note: We can't easily look up the wrapper, but we can construct a call.
                // Actually, 'ping' is a Closure. We can call it directly if we wrap it?
                // No, NativeJsFunction.call wraps arguments.
                // But here we are IN JS.
                // We can use the same mechanism as js-apply: just call it?
                // Wait, 'ping' is a Closure instance. It's not a JS function.
                // We need to wrap it or use interpreter.run.
                // The easiest way is to use the 'js-apply' logic:
                // But we are inside the body of a NativeJsFunction.
                // We can create a new run.
                const ast = new TailApp(new Literal(ping), [new Literal(n - 1)]);
                return interpreter.run(ast, interpreter.globalEnv);
            },
            interpreter
        ));

        run(interpreter, `
      (set! scheme-ping (lambda (n)
        (if (= n 0)
            "ping-done"
            (js-pong (- n 1)))))
    `);

        result = run(interpreter, `(scheme-ping 10)`);
        assert(logger, "Interop: Ping-Pong (N=10)", result, "ping-done");

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
