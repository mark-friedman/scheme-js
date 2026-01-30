import { runUnitTests } from './unit/unit_tests.js';
import { runCoreTests } from './functional/core_tests.js';
import { runInteropTests } from './functional/interop_tests.js';
import { runQuasiquoteTests } from './functional/quasiquote_tests.js';
import { runDefineTests } from './functional/define_tests.js';
import { runJsGlobalTests } from './functional/js_global_tests.js';
import { runInteropConversionTests } from './functional/interop_conversion_tests.js';

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
  runJsGlobalTests(interpreter, logger);
  runInteropConversionTests(interpreter, logger);
}
