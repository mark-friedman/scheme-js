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
import { analyze } from '../src/core/interpreter/analyzer.js';
import { writeString } from '../src/core/primitives/io/printer.js';
import { compilerEnvironment } from '../src/compiler/lowering.js';

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

  env.define('native-report-test-result', (name, passed, expected, actual) => {
    const detail = `(Expected: ${writeString(expected)}, Got: ${writeString(actual)})`;
    if (passed) logger.pass(`${name} ${detail}`); else logger.fail(`${name} ${detail}`);
  });
  env.define('native-log-title', (title) => logger.title(title));
  env.define('native-report-test-skip', (name, reason) => logger.skip(`${name} (Reason: ${reason})`));
  evaluate(interpreter, env, await fileLoader('tests/core/scheme/test.scm'));

  for (const file of testFiles) {
    evaluate(interpreter, env, await fileLoader(file));
    if (evaluate(interpreter, env, '(test-report)') !== true) logger.fail(`${file} FAILED`);
    evaluate(interpreter, env, '(set! *test-failures* 0) (set! *test-passes* 0)');
  }
}
