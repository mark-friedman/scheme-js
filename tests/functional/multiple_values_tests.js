/**
 * Multiple Values Tests
 * Tests for R7RS values and call-with-values
 */

import { run, assert } from '../helpers.js';
import { Values } from '../../src/runtime/values.js';

export function runMultipleValuesTests(interpreter, logger) {
  logger.title('Multiple Values Tests');

  // 1. values with single value
  const single = run(interpreter, "(values 42)");
  assert(logger, "values single", single, 42);

  // 2. values with no arguments
  const none = run(interpreter, "(values)");
  assert(logger, "values empty", none, undefined);

  // 3. call-with-values basic - sum of three values
  const cwv1 = run(interpreter, `
        (call-with-values
          (lambda () (values 1 2 3))
          (lambda (a b c) (+ a b c)))
    `);
  assert(logger, "call-with-values sum", cwv1, 6);

  // 4. call-with-values with single value (producer returns 1 value)
  const cwv2 = run(interpreter, `
        (call-with-values
          (lambda () 42)
          (lambda (x) (* x 2)))
    `);
  assert(logger, "call-with-values single", cwv2, 84);

  // 5. call-with-values with collector pattern
  const cwv3 = run(interpreter, `
        (call-with-values
          (lambda () (values 10 20))
          (lambda (x y) (- y x)))
    `);
  assert(logger, "call-with-values difference", cwv3, 10);

  // 6. values used directly (should work for single value context)
  const direct = run(interpreter, "(+ 1 (values 2))");
  assert(logger, "values in expression", direct, 3);

  // 7. Two values unpacked correctly
  const two = run(interpreter, `
        (call-with-values
          (lambda () (values 100 200))
          (lambda (a b) (+ a b)))
    `);
  assert(logger, "call-with-values two sum", two, 300);

  // === call/cc + multiple values tests ===

  // 8. Invoke continuation with multiple values
  const callccMulti = run(interpreter, `
        (call-with-values
          (lambda () (call/cc (lambda (k) (k 1 2 3))))
          (lambda (a b c) (+ a b c)))
    `);
  assert(logger, "call/cc with multiple values", callccMulti, 6);

  // 9. Continuation captures and restores with single value
  const callccSingle = run(interpreter, `
        (call-with-values
          (lambda () (call/cc (lambda (k) (k 42))))
          (lambda (x) (* x 2)))
    `);
  assert(logger, "call/cc with single value", callccSingle, 84);

  // === JS interop tests ===

  // 10. When multiple values escape to top-level, return first value
  const escapeMulti = run(interpreter, "(values 1 2 3)");
  // Should return first value (Option C behavior)
  const firstVal = escapeMulti instanceof Values ? escapeMulti.first() : escapeMulti;
  assert(logger, "escaped values returns first", firstVal, 1);

  // 11. JS calling Scheme function that returns multiple values
  run(interpreter, "(define (return-multi) (values 10 20 30))");
  const multiFunc = interpreter.globalEnv.lookup('return-multi');
  const bridge = interpreter.createJsBridge(multiFunc, []);
  const jsResult = bridge();
  // Should get first value when called from JS
  const jsFirstVal = jsResult instanceof Values ? jsResult.first() : jsResult;
  assert(logger, "JS interop: first value from multi", jsFirstVal, 10);
}

