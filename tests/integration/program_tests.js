/**
 * @fileoverview A program's environment (R7RS 5.1, 5.6.1): a program that
 * begins with `import` declarations sees what they import and nothing else,
 * as a library does, and what it defines is its own; one with none sees
 * everything, as a program's top level always has, so that a page or a quick
 * script with no imports keeps working.
 *
 * And a program's `include` and `include-ci` forms (R7RS 4.1.7), which read
 * their files through the file resolver.
 *
 * JavaScript tests, since what is tested is how the CLI, a page and the
 * benchmarks start a program: JavaScript asking the library system for the
 * program's environment (`programEnvironment` in `library_loader.js`) and
 * running its forms there (`runProgramForm`), with files a resolver set from
 * JavaScript serves.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { withPrivateLibraries } from '../../src/core/interpreter/library_registry.js';
import { programEnvironment, runProgramForm } from '../../src/core/interpreter/library_loader.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { writeString } from '../../src/core/primitives/io/printer.js';

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runProgramTests(logger) {
  logger.title('A program that begins with import declarations sees only them');

  // The shipped libraries, read from the bundled sources, in a registry of
  // their own so as to leave the shared one alone.
  const bundled = (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] ?? BUNDLED_SOURCES[name[name.length - 1]];
  const seen = withPrivateLibraries({ resolver: bundled }, () => {
    const { interpreter, env } = createInterpreter();

    /**
     * Runs a program as the CLI and a page do.
     * @param {string} source - The program.
     * @returns {{value: string, env: Object}} Its last value, as `write`
     *   writes it, or what it raised; and the environment it ran in.
     */
    const runProgram = (source) => {
      const program = programEnvironment(parse(source), analyze, interpreter, env);
      try {
        let value;
        for (const form of program.forms) {
          value = runProgramForm(form, analyze, interpreter, program.env, { jsAutoConvert: 'raw' });
        }
        return { value: writeString(value), env: program.env };
      } catch (e) {
        return { value: `raised: ${e.message}`, env: program.env };
      }
    };

    const strict = runProgram('(import (only (scheme base) define list car)) (define program-test-x (list 1 2)) (car program-test-x)');
    const lenient = runProgram('(define program-test-y 6) (cdr (quote (1 2)))');
    return {
      imported: strict.value,
      ownEnvironment: strict.env !== env,
      ownDefinition: strict.env.bindings.has('program-test-x') && !env.bindings.has('program-test-x'),
      unimported: runProgram("(import (only (scheme base) car quote)) (cdr '(1 2))").value,
      unimportedMacro: runProgram('(import (only (scheme base) quote)) (let*-values (((a) 1)) a)').value,
      ownMacro: runProgram(`(import (scheme base))
        (define-syntax program-test-swap!
          (syntax-rules () ((_ a b) (let ((t a)) (set! a b) (set! b t)))))
        (define p 1) (define q 2) (program-test-swap! p q) (list p q)`).value,
      severalDeclarations: runProgram('(import (only (scheme base) list)) (import (only (scheme char) char-upcase)) (list (char-upcase #\\a))').value,
      lenient: lenient.value,
      lenientEnvironment: lenient.env === env && env.bindings.has('program-test-y')
    };
  });

  assert(logger, 'it sees what it imports', seen.imported, '1');
  assert(logger, 'in an environment of its own', seen.ownEnvironment, true);
  assert(logger, 'where what it defines is bound, and not in the interaction environment', seen.ownDefinition, true);
  assert(logger, 'a primitive it did not import is unbound',
    seen.unimported.startsWith('raised:') && seen.unimported.includes('cdr'), true);
  assert(logger, 'as is a macro it did not import', seen.unimportedMacro.startsWith('raised:'), true);
  assert(logger, 'a macro it defines is its own, and expands into what it imported', seen.ownMacro, '(2 1)');
  assert(logger, 'every import declaration it begins with counts', seen.severalDeclarations, '(#\\A)');
  assert(logger, 'a program with no import declarations sees everything', seen.lenient, '(2)');
  assert(logger, 'and runs in the interaction environment, as before', seen.lenientEnvironment, true);

  // `include` and `include-ci` as forms (R7RS 4.1.7), not library
  // declarations: the forms of files, found by the file resolver, put where
  // the form is.
  logger.title('include and include-ci, as forms');
  const files = {
    'include-a.scm': '(define include-test-a 1)',
    'include-b.scm': '(define include-test-b (+ include-test-a 1))',
    'include-ci.scm': '(define INCLUDE-TEST-FOLDED 3)',
    'include-expression.scm': '(* 6 7)'
  };
  const included = withPrivateLibraries({ resolver: (name) => files[name.join('/')] ?? bundled(name) }, () => {
    const { interpreter, env } = createInterpreter();
    const valueOf = (source) => {
      const program = programEnvironment(parse(source), analyze, interpreter, env);
      try {
        let value;
        for (const form of program.forms) {
          value = runProgramForm(form, analyze, interpreter, program.env, { jsAutoConvert: 'raw' });
        }
        return writeString(value);
      } catch (e) {
        return `raised: ${e.message}`;
      }
    };
    return {
      files: valueOf('(import (scheme base)) (include "include-a.scm" "include-b.scm") (list include-test-a include-test-b)'),
      folded: valueOf('(import (scheme base)) (include-ci "include-ci.scm") include-test-folded'),
      expression: valueOf('(import (scheme base)) (define (include-test-f) (include "include-expression.scm")) (include-test-f)'),
      missing: valueOf('(import (scheme base)) (include "include-missing.scm")')
    };
  });
  assert(logger, 'include puts the forms of its files where it is, in order', included.files, '(1 2)');
  assert(logger, 'include-ci reads them folding case', included.folded, '3');
  assert(logger, 'in a body, as an expression', included.expression, '42');
  assert(logger, 'a file that cannot be read is a syntax error naming it',
    included.missing.startsWith('raised:') && included.missing.includes('include-missing.scm'), true);
}
