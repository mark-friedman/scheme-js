/**
 * @fileoverview JavaScript that Scheme calls, and that calls Scheme: what the
 * DevTools tests step between (tests/devtools/stepping_tests.js). The tests
 * set breakpoints by line, so lines are not to move.
 */

/**
 * Adds one, as JavaScript.
 * @param {number} n - A number.
 * @returns {number}
 */
export function jsAddOne(n) {
  const m = n + 1;
  return m;
}

/**
 * Calls a Scheme procedure, and doubles what it returns.
 * @param {Function} proc - The procedure.
 * @param {number} x - Its argument.
 * @returns {number}
 */
export function jsCallsScheme(proc, x) {
  const y = proc(x);
  return y * 2;
}
