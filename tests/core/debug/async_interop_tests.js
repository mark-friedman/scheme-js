/**
 * @fileoverview Async Interop Stress Tests
 * 
 * Tests that async execution with yields works correctly at Scheme/JS boundaries:
 * - Scheme → JS → Scheme callback chains
 * - JS → Scheme → JS nested calls
 * - Interleaved loops with yields
 * - TCO under async execution
 * - call/cc across JS boundaries under async
 */

import { assert, run, createTestLogger } from '../../harness/helpers.js';

/**
 * Runs async interop stress tests.
 * @param {Interpreter} interpreter - The bootstrapped interpreter
 * @param {Object} logger - Test logger
 */
export async function runAsyncInteropTests(interpreter, logger) {
  const runAsync = (code, options = {}) => interpreter.evaluateStringAsync(code, options);

  logger.title('Async Interop - Scheme → JS → Scheme');

  // Test: Scheme calls JS that calls Scheme closure
  {
    // Set up a JS function that calls a Scheme callback
    interpreter.globalEnv.define('js-callback-caller', (callback, value) => {
      return callback(value * 2);
    });

    const code = `
      (define (my-callback x) (+ x 100))
      (js-callback-caller my-callback 21)
    `;

    const result = await runAsync(code, { stepsPerYield: 5 });
    assert(logger, 'Scheme→JS→Scheme callback chain', result, 142);  // 21*2 + 100
  }

  // Test: Multiple nested Scheme→JS→Scheme calls with yields
  {
    interpreter.globalEnv.define('js-double', (fn, n) => fn(n) * 2);

    const code = `
      (define (scheme-add-10 x) (+ x 10))
      (+ (js-double scheme-add-10 5)   ; (5+10)*2 = 30
         (js-double scheme-add-10 3))  ; (3+10)*2 = 26
    `;

    const result = await runAsync(code, { stepsPerYield: 3 });
    assert(logger, 'Multiple nested callbacks', result, 56);
  }

  logger.title('Async Interop - JS → Scheme → JS');

  // Test: JS function that triggers Scheme that calls JS
  {
    let jsCounter = 0;
    interpreter.globalEnv.define('js-increment', () => ++jsCounter);

    const code = `
      (define (do-increments n)
        (if (<= n 0)
            (js-increment)
            (begin
              (js-increment)
              (do-increments (- n 1)))))
      (do-increments 5)
    `;

    jsCounter = 0;
    await runAsync(code, { stepsPerYield: 2 });
    assert(logger, 'Scheme loop calling JS with yields', jsCounter, 6);
  }

  // Test: JS array map via Scheme closure
  {
    interpreter.globalEnv.define('test-js-array', [1, 2, 3, 4, 5]);

    const code = `
      (define arr test-js-array)
      (define (double x) (* x 2))
      (js-invoke arr "map" double)
    `;

    const result = await runAsync(code, { stepsPerYield: 5 });
    assert(logger, 'JS array.map with Scheme closure', Array.from(result), [2, 4, 6, 8, 10]);
  }

  logger.title('Async Interop - Interleaved Loops');

  // Test: Scheme loop with JS call each iteration, with aggressive yields
  {
    let sum = 0;
    interpreter.globalEnv.define('js-accumulate', (n) => { sum += n; return sum; });

    const code = `
      (define (accumulate-loop n)
        (if (<= n 0)
            'done
            (begin
              (js-accumulate n)
              (accumulate-loop (- n 1)))))
      (accumulate-loop 10)
    `;

    sum = 0;
    const result = await runAsync(code, { stepsPerYield: 1 });
    assert(logger, 'Interleaved loop result', result.name || result, 'done');
    assert(logger, 'JS accumulator correct', sum, 55);  // 1+2+...+10
  }

  // Test: Mutual recursion across boundary
  {
    // JS function that calls Scheme, which calls JS, etc.
    let depth = 0;
    interpreter.globalEnv.define('js-ping', (schemePong, n) => {
      depth++;
      if (n <= 0) return 'done';
      return schemePong(n - 1);
    });

    const code = `
      (define (scheme-pong n)
        (if (<= n 0)
            'done
            (js-ping scheme-pong (- n 1))))
      (js-ping scheme-pong 10)
    `;

    depth = 0;
    const result = await runAsync(code, { stepsPerYield: 2 });
    assert(logger, 'Mutual recursion result', result.name || result, 'done');
    assert(logger, 'JS depth counter', depth, 6);  // Called 6 times (10,8,6,4,2,0)
  }

  logger.title('Async Interop - TCO Preservation');

  // Test: Tail-recursive function with JS calls maintains TCO
  {
    let callCount = 0;
    interpreter.globalEnv.define('js-identity', (x) => { callCount++; return x; });

    // This MUST be tail-recursive for TCO
    const code = `
      (define (tail-sum n acc)
        (if (<= n 0)
            (js-identity acc)
            (tail-sum (- n 1) (+ acc (js-identity n)))))
      (tail-sum 1000 0)
    `;

    callCount = 0;
    const result = await runAsync(code, { stepsPerYield: 50 });
    assert(logger, 'TCO with JS calls result', result, 500500);
    assert(logger, 'JS called expected times', callCount, 1001);  // 1000 + final
  }

  // Test: Deep tail recursion doesn't blow stack under async
  {
    const code = `
      (define (deep-tail n acc)
        (if (<= n 0)
            acc
            (deep-tail (- n 1) (+ acc 1))))
      (deep-tail 50000 0)
    `;

    const result = await runAsync(code, { stepsPerYield: 100 });
    assert(logger, 'Deep tail recursion under async', result, 50000);
  }

  // Test: Deep tail recursion doesn't blow stack and maintains stable memory
  {
    const code = `
      (define (deep-tail-mem n acc)
        (if (<= n 0)
            acc
            (deep-tail-mem (- n 1) (+ acc 1))))
      
      (let ((initial-heap (garbage-collect-and-get-heap-usage)))
        (deep-tail-mem 50000 0)
        (let ((final-heap (garbage-collect-and-get-heap-usage)))
          ;; Check if memory grew by more than 1MB (allowing for small transients)
          ;; In a truly leaking scenario, 50k frames would be many MBs.
          (< (- final-heap initial-heap) 1000000)))
    `;

    const result = await runAsync(code, { stepsPerYield: 100 });
    assert(logger, 'Memory stable during deep tail recursion', result, true);
  }


  logger.title('Async Interop - call/cc Across Boundaries');

  // Test: call/cc captured before JS call, invoked after yields
  {
    interpreter.globalEnv.define('js-add-100', (x) => x + 100);

    const code = `
      (+ 1 (call/cc (lambda (k)
                      (+ 2 (js-add-100 (k 10))))))
    `;

    const result = await runAsync(code, { stepsPerYield: 2 });
    assert(logger, 'call/cc escape before JS call', result, 11);  // 1 + 10
  }

  // Test: Continuation invoked from within JS callback
  {
    interpreter.globalEnv.define('js-apply', (fn, arg) => fn(arg));

    const code = `
      (define result #f)
      (define k-captured #f)
      (set! result
        (+ 1 (call/cc (lambda (k)
                        (set! k-captured k)
                        (js-apply (lambda (x) (k x)) 42)))))
      result
    `;

    const result = await runAsync(code, { stepsPerYield: 3 });
    assert(logger, 'Continuation from JS callback', result, 43);  // 1 + 42
  }

  logger.title('Async Interop - Dynamic-Wind with JS');

  // Test: dynamic-wind thunks run correctly with JS boundaries
  {
    const log = [];
    interpreter.globalEnv.define('js-log', (msg) => { log.push(msg); });
    interpreter.globalEnv.define('js-get-log', () => log);

    const code = `
      (dynamic-wind
        (lambda () (js-log "before"))
        (lambda () 
          (js-log "body")
          42)
        (lambda () (js-log "after")))
    `;

    log.length = 0;
    const result = await runAsync(code, { stepsPerYield: 2 });
    assert(logger, 'dynamic-wind result with JS', result, 42);
    assert(logger, 'dynamic-wind JS log order', log, ['before', 'body', 'after']);
  }

  // Test: dynamic-wind with continuation escape through JS
  {
    const log = [];
    interpreter.globalEnv.define('js-log2', (msg) => { log.push(msg); });
    interpreter.globalEnv.define('js-call', (fn) => fn());

    const code = `
      (call/cc (lambda (escape)
        (dynamic-wind
          (lambda () (js-log2 "before"))
          (lambda () 
            (js-log2 "body")
            (js-call (lambda () (escape 99)))
            (js-log2 "never"))
          (lambda () (js-log2 "after")))))
    `;

    log.length = 0;
    const result = await runAsync(code, { stepsPerYield: 2 });
    assert(logger, 'dynamic-wind escape result', result, 99);
    assert(logger, 'dynamic-wind after thunk ran', log.includes('after'), true);
    assert(logger, 'dynamic-wind never not reached', log.includes('never'), false);
  }

  logger.title('Async Interop - Yield Stability');

  // Test: State consistent after many yields with complex interop
  {
    let jsState = 0;
    interpreter.globalEnv.define('js-bump', () => ++jsState);
    interpreter.globalEnv.define('js-get-state', () => jsState);

    const code = `
      (define (complex-interop n acc)
        (if (<= n 0)
            acc
            (let ((x (js-bump)))
              (let ((y (+ x acc)))
                (complex-interop (- n 1) y)))))
      (complex-interop 100 0)
    `;

    jsState = 0;
    let yieldCount = 0;
    const result = await interpreter.evaluateStringAsync(code, {
      stepsPerYield: 3,
      onYield: () => { yieldCount++; }
    });

    assert(logger, 'Complex interop result', result, 5050);  // 1+2+...+100
    assert(logger, 'JS state correct', jsState, 100);
    assert(logger, 'Yielded many times', yieldCount > 50, true);
  }
}

export default runAsyncInteropTests;
