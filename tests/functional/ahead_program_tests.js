/**
 * @fileoverview Programs compiled ahead of time, run with no interpreter.
 *
 * `node repl.js --build PROGRAM -o OUTPUT` compiles every form a program
 * runs, and every form of each library it uses that runs as the library
 * loads, keeps what the program reaches, and writes it with the runtime as
 * one ES module (scripts/lib/ahead.scm, src/packaging/ahead_bundle.js), which
 * `node OUTPUT` runs with no interpreter, expander, reader or library system.
 * These build programs with the CLI and run them as a user would, and check
 * what they print -- the interpreter's answers, which each was checked
 * against -- and that a program that could not run so is refused, by the
 * name of what it reaches. Only Node can run the build.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';
import { execFileSync, spawnSync } from 'child_process';
import { assert } from '../harness/helpers.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');

/**
 * Builds a program ahead of time with the CLI.
 * @param {string} dir - Where to write the program and the module.
 * @param {string} name - The program's name.
 * @param {string} source - Its text.
 * @returns {{file: (string|null), refusals: string[]}} The module's file, or
 *   null and why the build refused the program.
 */
function build(dir, name, source) {
  const program = path.join(dir, `${name}.scm`);
  const output = path.join(dir, `${name}.mjs`);
  fs.writeFileSync(program, source);
  try {
    execFileSync(process.execPath, [path.join(ROOT, 'repl.js'), '--build', program, '-o', output],
      { cwd: dir, stdio: ['ignore', 'pipe', 'pipe'] });
    return { file: output, refusals: [] };
  } catch (e) {
    return { file: null, refusals: e.stderr.toString().trim().split('\n') };
  }
}

/**
 * Runs a program built ahead of time, as `node OUTPUT`.
 * @param {string} file - The module.
 * @param {string} [input] - What is piped to its standard input.
 * @returns {Array<string|number>} What it wrote to standard output, less its
 *   last newline, and to standard error, and its exit status.
 */
function run(file, input = '') {
  const ran = spawnSync(process.execPath, [file], { encoding: 'utf8', input });
  return [ran.stdout.replace(/\n$/, ''), ran.stderr, ran.status];
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

// JavaScript interop, a class, a promise and the command line: primitives that
// need nothing of the interpreter, and a Scheme procedure JavaScript calls
// back, which runs on the driver.
const INTEROP = `(import (scheme base) (scheme write) (scheme process-context) (scheme-js interop)
        (scheme-js promise))
(define-class <Point> Point point? (fields (x point-x point-x-set!) (y point-y)) (methods))
(define p (Point 3 4))
(point-x-set! p 30)
(define obj (js-obj "name" "scheme" "size" 3))
(js-set! obj "size" 4)
(define squares (js-invoke (js-eval "[1, 2, 3]") "map" (lambda (x . rest) (* x x))))
(write (list (point? p) (point-x p) (point-y p) (js-ref obj "name") (js-ref obj "size")
             (vector->list squares) (js-typeof obj) (js-promise? (js-promise-resolve 1))
             (string? (car (command-line)))))
`;

// `read`, whose reader is a library the library system's seed loads for
// itself: compiled with the program instead.
const READS = `(import (scheme base) (scheme read) (scheme write))
(define port (open-input-string "(1 (2 . 3) #(4 \\"five\\") six) 7"))
(define first (read port))
(define second (read port))
(write (list first second (eof-object? (read port)) (read)))
`;

// A class whose constructor and methods read `this`, the receiver JavaScript
// calls them with.
const CLASSES = `(import (scheme base) (scheme write) (scheme-js interop))
(define-class <Counter> Counter counter?
  (fields (count counter-count counter-count-set!))
  (constructor (start) (set! this.count start))
  (methods
    (bump! (by) (counter-count-set! this (+ this.count by)) this.count)))
(define c (Counter 10))
(c.bump! 5)
(write (list (counter? c) (counter-count c) (c.bump! 1)))
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
      assert(logger, 'and prints what the interpreter does', run(data.file),
        ['(2 1 #t 10 4 (two . 2) 3 2 #(0 1 4) (1 2) mine 9 3 "ABC" 20 (1 2 3) 10 0.3333333333333333 '
          + '1267650600228229401496703205376)\n#(2 3 4)', '', 0]);
      const text = fs.readFileSync(data.file, 'utf8');
      assert(logger, 'with only the library procedures it reaches: vector-map, but not string-map',
        [text.includes('procedure: "vector-map"'), text.includes('procedure: "string-map"')], [true, false]);
      assert(logger, 'and none of its own it does not: a procedure naming dynamic-wind, never called, is left out',
        text.includes('procedure: "never-called"'), false);
    }

    const continuations = build(dir, 'continuations', CONTINUATIONS);
    assert(logger, 'escapes, re-entries, and recursions deep enough that the driver moves frames to the heap',
      continuations.file === null ? continuations.refusals : run(continuations.file),
      ['(4 (10 11 12) 100000 (20002 3))', '', 0]);

    const interop = build(dir, 'interop', INTEROP);
    assert(logger, 'JavaScript interop, classes, promises and the command line, a callback from JavaScript included',
      interop.file === null ? interop.refusals : run(interop.file),
      ['(#t 30 4 "scheme" 4 (1 4 9) "object" #t #t)', '', 0]);

    const reads = build(dir, 'reads', READS);
    assert(logger, 'read, from a string port and from standard input, as the CLI gives it',
      reads.file === null ? reads.refusals : run(reads.file, '(piped in)'),
      ['((1 (2 . 3) #(4 "five") six) 7 #t (piped in))', '', 0]);

    const classes = build(dir, 'classes', CLASSES);
    assert(logger, 'a class whose constructor and methods read this',
      classes.file === null ? classes.refusals : run(classes.file), ['(#t 15 16)', '', 0]);

    const raises = build(dir, 'raises', RAISES);
    assert(logger, 'an error nobody handles is reported as the CLI reports it, after what the program printed',
      raises.file === null ? raises.refusals : run(raises.file),
      ['before', `Error executing ${path.join(dir, 'raises.scm')}: something went wrong:\n`, 1]);

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
    assert(logger, 'one that cannot be expanded, as running it would say',
      refusalOf('malformed', '(import (scheme base))\n(if)\n')[0].startsWith('Error building'), true);
    assert(logger, 'one evaluating, which needs the expander and an interpreter, by the form',
      refusalOf('eval', '(import (scheme base) (scheme eval))\n(eval 1 (environment (quote (scheme base))))\n'),
      ['a top-level form in the program is not compiled: references control global \'eval\'']);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
