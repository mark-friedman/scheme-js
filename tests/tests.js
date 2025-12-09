import { runUnitTests } from './unit/unit_tests.js';
import { runCoreTests } from './functional/core_tests.js';
import { runInteropTests } from './functional/interop_tests.js';
import { runQuasiquoteTests } from './functional/quasiquote_tests.js';
import { runDefineTests } from './functional/define_tests.js';

export { runUnitTests };

/**
 * Runs all functional tests (aggregating the new split tests).
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runAllTests(interpreter, logger) {
  runCoreTests(interpreter, logger);
  runInteropTests(interpreter, logger);
  runQuasiquoteTests(interpreter, logger);
  runDefineTests(interpreter, logger);
}
