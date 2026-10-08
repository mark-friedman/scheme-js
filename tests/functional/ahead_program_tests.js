/**
 * @fileoverview Programs compiled ahead of time, run with no interpreter.
 *
 * The build (scripts/build_ahead.scm, over scripts/lib/ahead.scm) compiles
 * every form a program runs, and every form of each library it uses that
 * runs as the library loads, and writes them as a table; `runProgram` in
 * src/compiler/ahead.js runs the table with no interpreter, expander, reader
 * or library system. These build programs with the CLI, as a user would, run
 * what it wrote, and check what the programs print -- the interpreter's
 * answers, which each was checked against -- and that a program the runtime
 * could not run is refused, by the name of what it reaches. Only JavaScript
 * can run the generated code, and only Node can run the build.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';
import { execFileSync } from 'child_process';
import { assert } from '../harness/helpers.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');

/**
 * Builds a program ahead of time with the CLI.
 * @param {string} dir - Where to write the program and the table.
 * @param {string} name - The program's name.
 * @param {string} source - Its text.
 * @returns {{file: (string|null), refusals: string[]}} The table's file, or
 *   null and why the build refused the program.
 */
function build(dir, name, source) {
  const program = path.join(dir, `${name}.scm`);
  const table = path.join(dir, `${name}.js`);
  fs.writeFileSync(program, source);
  try {
    execFileSync(process.execPath, ['repl.js', '-I', 'scripts/lib', 'scripts/build_ahead.scm', program, table],
      { cwd: ROOT, stdio: ['ignore', 'pipe', 'pipe'] });
    return { file: table, refusals: [] };
  } catch (e) {
    return { file: null, refusals: e.stderr.toString().trim().split('\n') };
  }
}

/**
 * Runs a table the build wrote, with what it writes to the console caught.
 * @param {string} file - The table's file.
 * @returns {Promise<{output: string, error: (Error|null), program: Object}>}
 */
async function run(file) {
  const { runProgram, AHEAD_PRIMITIVES } = await import('../../src/compiler/ahead.js');
  const program = (await import(`${new URL(`file://${file}`)}`)).default;
  // The console port is the process's: what an earlier test left in it
  // unfinished is written out before this program's output is caught.
  AHEAD_PRIMITIVES['%console-output-port']().flush();
  const lines = [];
  const log = console.log;
  console.log = (...parts) => lines.push(parts.join(' '));
  let error = null;
  try {
    runProgram(program);
  } catch (e) {
    error = e;
  } finally {
    AHEAD_PRIMITIVES['%console-output-port']().flush();
    console.log = log;
  }
  return { output: lines.join('\n'), error, program };
}

/**
 * The names of the procedures a table binds for a library.
 * @param {Object} program - The table.
 * @param {string} library - The library's key, `scheme.core`.
 * @returns {string[]}
 */
function proceduresOf(program, library) {
  const unit = program.units.find((u) => u.library !== undefined && u.library.join('.') === library);
  return unit === undefined ? [] : unit.items.filter((i) => i.procedure !== undefined).map((i) => i.procedure);
}

// It reads `map` as it imported it, before defining its own, as the
// interpreter lets a program do.
const DATA = `(import (scheme base) (scheme write) (prefix (scheme cxr) c:)
        (rename (only (scheme base) car) (car first)))
(define-record-type point (make-point x y) point? (x point-x set-point-x!) (y point-y))
(define-syntax swap!
  (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
(begin (define a 1) (define b 2))
(swap! a b)
(define p (make-point 3 4))
(set-point-x! p 10)
(define-values (q r) (floor/ 17 5))
(define v (make-vector 3 0))
(let loop ((i 0)) (when (< i 3) (vector-set! v i (* i i)) (loop (+ i 1))))
(define level (make-parameter 10 (lambda (x) (* x 2))))
(define (never-called) (dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3)))
(define first-map map)
(define (map f l) 'mine)
(write (list a b (point? p) (point-x p) (point-y p) (assq 'two '((one . 1) (two . 2))) q r v
             (first-map car '((1) (2))) (map 1 2)
             (first '(9 8)) (c:caddr '(1 2 3)) (string-upcase "abc") (level)
             (call-with-values (lambda () (values 1 2 3)) list) (apply + 1 2 '(3 4))
             (inexact 1/3) (expt 2 100)))
(newline)
(display (vector-map (lambda (x) (+ x 1)) #(1 2 3)))
`;

const CONTINUATIONS = `(import (scheme base) (scheme write))
(define (find-first pred items)
  (call/cc (lambda (return) (for-each (lambda (x) (if (pred x) (return x))) items) #f)))
(define saved #f)
(define (capture-and-return x) (+ x (call/cc (lambda (k) (set! saved k) 0))))
(define (re-enter)
  (let ((count 0) (results '()))
    (let ((r (capture-and-return 10)))
      (set! results (cons r results))
      (set! count (+ count 1))
      (if (< count 3) (saved count) (reverse results)))))
(define (deep n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))
(define deep-k #f)
(define (deep-save n) (if (= n 0) (call/cc (lambda (k) (set! deep-k k) 0)) (+ 1 (deep-save (- n 1)))))
(define (deep-reenter)
  (let ((runs 0))
    (let ((r (deep-save 20000)))
      (set! runs (+ runs 1))
      (if (< runs 3) (deep-k runs) (list r runs)))))
(write (list (find-first even? '(1 3 4 5)) (re-enter) (deep 100000) (deep-reenter)))
`;

const RAISES = `(import (scheme base) (scheme write))
(display "before")
(newline)
(error "something went wrong:" 42)
(display "after")
`;

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runAheadProgramTests(logger) {
  logger.title('Programs compiled ahead of time, run with no interpreter');
  if (typeof process === 'undefined') {
    logger.skip('programs compiled ahead of time (Node.js only)');
    return;
  }
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-ahead-'));
  try {
    const data = build(dir, 'data', DATA);
    assert(logger, 'a program of records, macros, several values, parameters and import filters builds',
      data.refusals, []);
    if (data.file !== null) {
      const ran = await run(data.file);
      assert(logger, 'and prints what the interpreter does',
        [ran.output, ran.error?.message ?? null],
        ['(2 1 #t 10 4 (two . 2) 3 2 #(0 1 4) (1 2) mine 9 3 "ABC" 20 (1 2 3) 10 0.3333333333333333 '
          + '1267650600228229401496703205376)\n#(2 3 4)', null]);
      assert(logger, 'with only the library procedures it reaches: vector-map, but not string-map',
        [proceduresOf(ran.program, 'scheme.core').includes('vector-map'),
          proceduresOf(ran.program, 'scheme.core').includes('string-map')], [true, false]);
      const program = ran.program.units[ran.program.units.length - 1];
      assert(logger, 'and none of its own it does not: a procedure naming dynamic-wind, never called, is left out',
        program.items.some((i) => i.procedure === 'never-called'), false);
    }

    const continuations = build(dir, 'continuations', CONTINUATIONS);
    if (continuations.file === null) {
      assert(logger, 'a program of continuations builds', continuations.refusals, []);
    } else {
      const ran = await run(continuations.file);
      assert(logger, 'escapes, re-entries, and recursions deep enough that the driver moves frames to the heap',
        [ran.output, ran.error?.message ?? null], ['(4 (10 11 12) 100000 (20002 3))', null]);
    }

    const raises = build(dir, 'raises', RAISES);
    if (raises.file === null) {
      assert(logger, 'a program that raises builds', raises.refusals, []);
    } else {
      const ran = await run(raises.file);
      assert(logger, 'an error nobody handles is thrown to whoever ran the program, after what it printed',
        [ran.output, ran.error?.message, ran.error?.irritants], ['before', 'something went wrong:', [42]]);
    }

    logger.title('Programs refused, by what they reach');

    const refusalOf = (name, source) => build(dir, name, source).refusals;
    assert(logger, 'one with no import declarations',
      refusalOf('bare', '(define (f x) x)\n(f 1)\n'),
      ['a program compiled ahead of time begins with the import declarations of the libraries it uses']);
    assert(logger, 'one handling an exception with guard, by the procedure that does',
      refusalOf('guard', '(import (scheme base))\n(define (safe-div a b) (guard (e (#t 0)) (/ a b)))\n(safe-div 1 0)\n')
        .some((line) => line.startsWith('safe-div in the program is not compiled')), true);
    assert(logger, 'one using parameterize, by the library procedure that winds',
      refusalOf('parameterize', '(import (scheme base))\n(define p (make-parameter 1))\n(parameterize ((p 2)) (p))\n')
        .some((line) => line.startsWith('param-dynamic-bind in (scheme core) is not compiled')), true);
    assert(logger, 'one reading, by the primitive the runtime does not carry',
      refusalOf('read', '(import (scheme base) (scheme read))\n(read)\n'),
      ['the primitive %read is not carried by the runtime of a program compiled ahead of time']);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
