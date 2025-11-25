import { runUnitTests } from './unit/unit_tests.js';
import { runFunctionalTests } from './functional/functional_tests.js';
import { runInteropTests } from './functional/interop_tests.js';
import { runQuasiquoteTests } from './functional/quasiquote_tests.js';
import { runDefineTests } from './functional/test_define.js';

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
