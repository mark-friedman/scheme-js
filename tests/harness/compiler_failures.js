/**
 * @fileoverview The compiler's own failures, for the harnesses that run with
 * the compiler on to fail on.
 *
 * An error raised inside the compiler leaves its procedure interpreted, with a
 * warning on the error port (`unless-failing` in `src/compiler/driver.scm`), so
 * a program never sees it but by its speed. The compiler keeps each one until
 * it is taken; the test suites and the benchmark harnesses take them and fail
 * on any, so that a bug in the compiler shows where it can be fixed.
 */

import { compilerExports } from '../../src/compiler/lowering.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { toArray } from '../../src/core/interpreter/cons.js';

/**
 * Takes the compiler's failures since they were last taken: none if the
 * compiler has not started here.
 * @returns {Array<{name: string, message: string}>} Each failure, oldest first.
 */
export function takeCompilerFailures() {
  const compiler = compilerExports();
  if (compiler === null) return [];
  return toArray(callSchemeProcedure(compiler.get('take-compiler-failures!'), []))
    .map((failure) => ({ name: String(failure.car), message: String(failure.cdr) }));
}

/**
 * The failures as one line, for a report.
 * @param {Array<{name: string, message: string}>} failures - The failures.
 * @returns {string}
 */
export function describeCompilerFailures(failures) {
  return failures.map(({ name, message }) => `${name}: ${message}`).join('; ');
}
