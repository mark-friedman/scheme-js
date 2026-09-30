/**
 * Unit tests of the console ports, the current output and error ports
 * wherever the CLI has not put the process's own descriptors in their place:
 * in a browser, the REPLs and the test and benchmark harnesses.
 */

import { ConsoleOutputPort } from '../../../../src/core/primitives/io/console_port.js';
import { assert } from '../../../harness/helpers.js';

/**
 * Runs the console port's unit tests.
 * @param {Object} logger - Test logger.
 */
export function runConsolePortTests(logger) {
  logger.title('Console Output Port');

  const calls = [];
  const realLog = console.log;
  const realError = console.error;
  console.log = (...args) => calls.push(['log', ...args]);
  console.error = (...args) => calls.push(['error', ...args]);
  try {
    const out = new ConsoleOutputPort('stdout');
    const err = new ConsoleOutputPort('stderr');
    out.writeString('one\ntw');
    err.writeString('bad\n');
    out.writeChar('o');
    out.flush();
    err.writeString('worse');
    err.flush();
    out.flush();
    console.log = realLog;
    console.error = realError;
    assert(logger, 'each line goes to console.log, and the error port\'s to console.error',
      calls, [['log', 'one'], ['error', 'bad'], ['log', 'two'], ['error', 'worse']]);
  } finally {
    console.log = realLog;
    console.error = realError;
  }
}
