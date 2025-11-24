import { runUnitTests } from './tests/unit_tests.js';
import { runFunctionalTests } from './tests/functional_tests.js';
import { runInteropTests } from './tests/interop_tests.js';
import { runQuasiquoteTests } from './tests/quasiquote_tests.js';
import { runDefineTests } from './tests/test_define.js';

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
