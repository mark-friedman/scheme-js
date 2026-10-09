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

    // The CLI looks for libraries in the directory it runs in first; a file
    // there named like one a library of the system's includes is not
    // included in its place: what a library includes is found beside it.
    logger.title('CLI - run beside files named like what the system\'s libraries include');
    const beside = path.join(dir, 'beside');
    fs.mkdirSync(beside);
    fs.writeFileSync(path.join(beside, 'list.scm'), '(this is not the list library\n');
    fs.writeFileSync(path.join(beside, 'numbers.scm'), '(this is not the numbers library\n');
    const there = await runCli([program('there.scm', '(import (scheme base) (scheme write)) (write (list 1 2))')],
      { cwd: beside });
    assert(logger, 'a program run there sees the system\'s libraries as they are',
      [there.status, there.stdout, there.stderr], [0, '(1 2)', '']);

    // Debugged in DevTools, a procedure has to be compiled to be stepped
    // into, and the tier compiles one only on its second call; so with Node's
    // inspector on, or asked to, the CLI has every procedure compiled as it
    // is defined.
    logger.title('CLI - compiling every procedure, for DevTools');
    const once = program('once.scm', `(import (scheme base) (scheme write) (scheme-js interop))
      (define (once x) (+ x 1))
      (write (list (once 1) (eq? #t (js-ref once "$compiled"))))`);
    const inspect = ['--inspect=127.0.0.1:0'];
    const [plain, asked, inspected, declined] = await Promise.all([
      runCli([once]),
      runCli(['--devtools', once]),
      runCli([once], { nodeOptions: inspect }),
      runCli(['--no-devtools', once], { nodeOptions: inspect })
    ]);
    assert(logger, 'a procedure called once is not compiled', [plain.status, plain.stdout], [0, '(2 #f)']);
    assert(logger, 'but is with --devtools, as it is defined', [asked.status, asked.stdout], [0, '(2 #t)']);
    assert(logger, "and with Node's inspector on", [inspected.status, inspected.stdout], [0, '(2 #t)']);
    assert(logger, 'unless --no-devtools says not to', [declined.status, declined.stdout], [0, '(2 #f)']);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
