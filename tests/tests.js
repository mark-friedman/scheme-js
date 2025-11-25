import { runUnitTests } from './unit_tests.js';
import { runFunctionalTests } from './functional_tests.js';
import { runInteropTests } from './interop_tests.js';
import { runQuasiquoteTests } from './quasiquote_tests.js';
import { runDefineTests } from './test_define.js';

export { runUnitTests };

/**
 * Runs all functional tests (aggregating the new split tests).
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export async function runAllTests(interpreter, logger) {
  await runFunctionalTests(interpreter, logger);
  runInteropTests(interpreter, logger);
  runQuasiquoteTests(interpreter, logger);
  runDefineTests(interpreter, logger);
}
