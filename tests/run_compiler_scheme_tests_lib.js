/**
 * @fileoverview Runs Scheme tests of the compiler's own Scheme.
 *
 * The compiler's passes -- liveness, the emitter's text, the lifting plan -- are
 * Scheme procedures, so their tests are Scheme too. Most of them are internal
 * to the `(scheme-js compiler)` library, which exports only its entry points,
 * and the library is loaded into a registry of the compiler's own
 * (`src/compiler/lowering.js`), so a test file could not import it anyway. So
 * these test files are run in the library's own environment, with the same
 * `test` harness the other Scheme tests use.
 */

import { parse } from '../src/core/interpreter/reader.js';
import { analyze, expandToCore } from '../src/core/interpreter/expand.js';
import { writeString } from '../src/core/primitives/io/printer.js';
import { compilerEnvironment } from '../src/compiler/lowering.js';
import { SCHEME_PRIMITIVE } from '../src/core/interpreter/values.js';

/**
 * Evaluates Scheme source in the compiler's environment.
 * @param {Object} interpreter - The compiler's interpreter.
 * @param {Object} env - Its environment.
 * @param {string} source - Scheme source.
 * @returns {*} The value of the last form.
 */
function evaluate(interpreter, env, source) {
  let value;
  for (const form of parse(source)) {
    value = interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
  }
  return value;
}

/**
 * Runs Scheme test files in the compiler's environment.
 * @param {Object} logger - Test logger.
 * @param {Array<string>} testFiles - The files, relative to the repository.
 * @param {function(string): Promise<string>} fileLoader - Reads a file.
 * @returns {Promise<void>}
 */
export async function runCompilerSchemeTests(logger, testFiles, fileLoader) {
  logger.title('Running Scheme Tests of the Compiler...');
  const { interpreter, env } = compilerEnvironment();

  // Takes Scheme values, which it writes as Scheme does: converted for
  // JavaScript, an exact integer beyond 2^53 could not be passed at all.
  const reportTestResult = (name, passed, expected, actual) => {
    const detail = `(Expected: ${writeString(expected)}, Got: ${writeString(actual)})`;
    if (passed) logger.pass(`${name} ${detail}`); else logger.fail(`${name} ${detail}`);
  };
  reportTestResult[SCHEME_PRIMITIVE] = true;
  env.define('native-report-test-result', reportTestResult);
  env.define('native-log-title', (title) => logger.title(title));
  // The expander is a library of the library system's own, which this
  // environment does not import, so a test that wants to lower real source
  // rather than a hand-written core form asks for it through this: a `define`
  // or a `lambda` form, as data, to the lambda's core form, as the lowering
  // receives it.
  const analyzeLambda = (form) => {
    const core = expandToCore(form);
    return core.car.name === 'define' ? core.cdr.cdr.car : core;
  };
  analyzeLambda[SCHEME_PRIMITIVE] = true;
  env.define('analyze-lambda', analyzeLambda);
  // And a top-level form, as the lowering receives one.
  const analyzeForm = (form) => expandToCore(form);
  analyzeForm[SCHEME_PRIMITIVE] = true;
  env.define('analyze-form', analyzeForm);
  env.define('native-report-test-skip', (name, reason) => logger.skip(`${name} (Reason: ${reason})`));
  evaluate(interpreter, env, await fileLoader('tests/core/scheme/test.scm'));

  for (const file of testFiles) {
    evaluate(interpreter, env, await fileLoader(file));
    if (evaluate(interpreter, env, '(test-report)') !== true) logger.fail(`${file} FAILED`);
    evaluate(interpreter, env, '(set! *test-failures* 0) (set! *test-passes* 0)');
  }
}
