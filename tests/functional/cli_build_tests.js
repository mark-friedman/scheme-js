/**
 * What the build steps and the compiler's harnesses, Scheme programs run from
 * the CLI, need of it: libraries found in a directory `-I` names, the
 * compiler's library imported as any library, and the build's doors,
 * `(scheme-js compiler build)` (src/compiler/build_host.js), with the library
 * the build steps share, `(scheme-js prebuild)` (scripts/lib/prebuild.scm),
 * making a library's table as `npm run prebuild` makes every shipped one.
 * Each test runs `repl.js` in a child process, so these are Node-only.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';
import { assert } from '../harness/helpers.js';
import { runCli } from '../harness/cli_process.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');

/**
 * Runs the tests of what the CLI gives a build step.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>} Resolves when every run has finished.
 */
export async function runCliBuildTests(logger) {
  if (typeof process === 'undefined') {
    logger.skip('CLI build tests (Node.js only)');
    return;
  }

  logger.title('CLI - what a build step run from it is given');

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-js-cli-build-'));
  const file = (name, source) => {
    const where = path.join(dir, name);
    fs.writeFileSync(where, source);
    return where;
  };

  try {
    file('greeting.sld', '(define-library (demo greeting) (import (scheme base)) (export greeting)'
      + ' (begin (define greeting "hello")))');
    file('lib.sld', '(define-library (demo lib) (import (scheme base)) (export twice) (include "lib.scm"))');
    file('lib.scm', '(define (twice x) (* 2 x))\n(define answer (twice 21))\n');
    const sourceDirs = [dir, 'src/core/scheme', 'src/extras/scheme'].map((d) => JSON.stringify(path.resolve(ROOT, d)));

    const [found, notFound, compiler, built] = await Promise.all([
      runCli(['-I', dir, file('found.scm', '(import (scheme base) (scheme write) (demo greeting)) (display greeting)')]),
      runCli([file('not_found.scm', '(import (scheme base) (scheme write) (demo greeting)) (display greeting)')]),
      runCli([file('compiler.scm', '(import (scheme base) (scheme write) (scheme-js compiler))'
        + ' (display (list (procedure? generate-environment) (procedure? generated-name)))')]),
      runCli(['-I', 'scripts/lib', file('built.scm', `
        (import (scheme base) (scheme write) (scheme-js compiler) (scheme-js compiler build) (scheme-js prebuild))
        (define read-source (source-reader (list ${sourceDirs.join(' ')})))
        (define result #f)
        (with-private-libraries
         (library-resolver read-source)
         (lambda (name env)
           (let ((forms (take-noted!)))
             (if (equal? name '("demo" "lib"))
                 (let* ((outcome (generate-environment env #t #f #f))
                        (table (library-table-for read-source name env forms (car outcome) (cdr outcome))))
                   (install-table-code! env table)
                   (set! result (list (length forms) (map car (library-table-entries table))
                                      (library-table-restored table) (library-table-files table)
                                      (string? (library-table-fingerprint table))))))))
         (lambda () (load-library '(demo lib) note!)))
        (write result)`)])
    ]);

    assert(logger, 'a library is found in a directory -I names', [found.status, found.stdout], [0, 'hello']);
    assert(logger, 'and not without it', [notFound.status, /not found/i.test(notFound.stderr)], [1, true]);
    assert(logger, 'a program imports the compiler as any library, the accessors of its records with it',
      [compiler.status, compiler.stdout], [0, '(#t #t)']);
    assert(logger, 'a library loaded in a registry of the program\'s own is tabled as the build tables it: '
      + 'its forms noted, its procedure compiled and restored, its files fingerprinted',
      [built.status, built.stdout.trim()],
      [0, '(2 ("twice") ("twice") ("lib.sld" "lib.scm") #t)']);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
