/**
 * A program the CLI runs that begins with `import` declarations sees what
 * they import and nothing else (R7RS 5.1, 5.6.1); one with none sees
 * everything, as before. Each test runs `repl.js` in a child process, so these
 * are Node-only.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { assert } from '../harness/helpers.js';
import { runCli } from '../harness/cli_process.js';

/**
 * Runs the tests of what a CLI program sees.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>} Resolves when every run has finished.
 */
export async function runCliProgramTests(logger) {
  if (typeof process === 'undefined') {
    logger.skip('CLI program tests (Node.js only)');
    return;
  }

  logger.title('CLI - a program sees what it imports');

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-js-cli-program-'));
  const program = (name, source) => {
    const file = path.join(dir, name);
    fs.writeFileSync(file, source);
    return file;
  };

  try {
    const [imported, unimported, none, expression] = await Promise.all([
      runCli([program('imported.scm', "(import (scheme base) (scheme write)) (display (car '(1 2)))")]),
      runCli([program('unimported.scm', "(import (only (scheme base) car quote) (scheme write)) (display (cdr '(1 2)))")]),
      runCli([program('none.scm', "(display (cdr '(1 2)))")]),
      runCli(['-e', "(import (only (scheme base) car quote)) (cdr '(1 2))"])
    ]);
    assert(logger, 'a program sees what it imports', [imported.status, imported.stdout], [0, '1']);
    assert(logger, 'and nothing else',
      [unimported.status, /unbound variable: cdr/.test(unimported.stderr)], [1, true]);
    assert(logger, 'a program with no import declarations sees everything', [none.status, none.stdout], [0, '(2)']);
    assert(logger, 'code given with -e that begins with import declarations sees nothing else either',
      [expression.status, /unbound variable: cdr/.test(expression.stderr)], [1, true]);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
