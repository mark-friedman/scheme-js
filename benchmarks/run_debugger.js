/**
 * @fileoverview What the debugger costs a program it is not stopping.
 *
 * ## Why this exists
 *
 * A debugger that costs nothing until it stops is the promise of the REPLs'
 * and a page's debugging, and nothing measured it but a run of `(fib 18)` by
 * hand (task 67 in `docs/compiler_plan_completed.md`). A program being
 * debugged asks the debugger at each step of the evaluator whether to pause
 * (`should-pause?` in `src/core/scheme/debugger.scm`), and compiled code runs
 * as itself only where a breakpoint is (task 40), so the cost falls on the
 * interpreted tier and on what the tier leaves interpreted.
 *
 * ## What is measured
 *
 * Six kernels, in the shapes the canonical suite's classes name -- calls,
 * lists, symbols, inexact arithmetic, strings -- each run with its procedures
 * interpreted and compiled (`compileProgram`), the standard library compiled
 * as it ships, under four states of the debugger:
 *
 *  - none attached;
 *  - a debug runtime attached, debugging off, as the CLI starts;
 *  - debugging on, with nothing to stop at;
 *  - debugging on, with a breakpoint set in another file, so that the
 *    debugger has a breakpoint to test each step against.
 *
 * Each figure is the best of `--runs` samples, in milliseconds a call -- a
 * compiled kernel's sample makes the call a hundred times -- with its factor
 * over the run with no debugger attached.
 *
 * Usage:
 *   node benchmarks/run_debugger.js [--runs N]
 */

import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/expand.js';
import { compileProgram } from '../src/compiler/index.js';
import { SchemeDebugRuntime } from '../src/debug/scheme_debug_runtime.js';
import { interpretedLibrary, installStandardLibrary } from '../tests/harness/standard_library.js';

const args = process.argv.slice(2);
const RUNS = Number(args.includes('--runs') ? args[args.indexOf('--runs') + 1] : 3);

/**
 * The kernels' definitions, read under one file's name, which the breakpoint
 * set "elsewhere" does not name.
 * @type {string}
 */
const KERNELS = `
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (tak x y z)
  (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))
(define (queens board-size)
  (define (ok? row dist placed)
    (or (null? placed)
        (and (not (= (car placed) (+ row dist)))
             (not (= (car placed) (- row dist)))
             (ok? row (+ dist 1) (cdr placed)))))
  (define (try rows placed)
    (if (null? rows)
        1
        (let loop ((before '()) (rows rows) (count 0))
          (if (null? rows)
              count
              (loop (cons (car rows) before) (cdr rows)
                    (if (ok? (car rows) 1 placed)
                        (+ count (try (append before (cdr rows)) (cons (car rows) placed)))
                        count))))))
  (let make ((i board-size) (rows '()))
    (if (= i 0) (try rows '()) (make (- i 1) (cons i rows)))))
(define (deriv a)
  (cond ((not (pair? a)) (if (eq? a 'x) 1 0))
        ((eq? (car a) '+) (cons '+ (map deriv (cdr a))))
        ((eq? (car a) '*) (list '* a (cons '+ (map (lambda (b) (list '/ (deriv b) b)) (cdr a)))))
        (else (error "deriv: unknown operator" (car a)))))
(define (deriv-loop n)
  (let loop ((i 0) (r #f))
    (if (= i n) (length r) (loop (+ i 1) (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))))))
(define (fibfp n) (if (< n 2.0) n (+ (fibfp (- n 1.0)) (fibfp (- n 2.0)))))
(define (string-loop n)
  (let loop ((i 0) (s ""))
    (if (= i n) (string-length s)
        (loop (+ i 1) (if (> (string-length s) 100) (substring s 50 (string-length s))
                          (string-append s (number->string i)))))))
`;

/**
 * Each kernel's call, at a size the interpreter takes tens of milliseconds
 * over.
 * @type {Array<[string, string]>}
 */
const CALLS = [
  ['fib: calls', '(fib 18)'],
  ['tak: calls', '(tak 14 9 4)'],
  ['queens: lists', '(queens 7)'],
  ['deriv: symbols', '(deriv-loop 500)'],
  ['fibfp: inexact arithmetic', '(fibfp 18.0)'],
  ['string-loop: strings', '(string-loop 3000)']
];

/**
 * The debugger's states.
 * @type {Array<{label: string, set: function(Object, SchemeDebugRuntime): void}>}
 */
const STATES = [
  { label: 'no debugger', set: (interpreter) => interpreter.setDebugRuntime(null) },
  { label: 'attached, off', set: (interpreter, runtime) => { interpreter.setDebugRuntime(runtime); runtime.disable(); } },
  { label: 'on, nothing to stop at', set: (interpreter, runtime) => { interpreter.setDebugRuntime(runtime); runtime.enable(); } },
  {
    label: 'on, a breakpoint elsewhere',
    set: (interpreter, runtime) => {
      interpreter.setDebugRuntime(runtime);
      runtime.enable();
      return [runtime.setBreakpoint('elsewhere.scm', 1)];
    }
  }
];

/**
 * An interpreter with the standard library compiled and the kernels defined,
 * interpreted or compiled.
 * @param {boolean} compiled - Whether to compile the kernels.
 * @returns {{interpreter: Object, env: Object, runtime: SchemeDebugRuntime}}
 */
function setUp(compiled) {
  const { interpreter, env } = interpretedLibrary({ filenames: true });
  installStandardLibrary(env);
  const asts = parse(KERNELS, { filename: 'kernels.scm' }).map((form) => analyze(form));
  if (compiled) {
    const outcome = compileProgram(asts, env, interpreter);
    if (outcome.compiled.length < 7) throw new Error(`only ${outcome.compiled.join(', ')} compiled`);
  } else {
    for (const ast of asts) interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
  }
  return { interpreter, env, runtime: new SchemeDebugRuntime() };
}

/**
 * How many times each sample makes the call, in each tier: compiled, the
 * kernels take a fraction of a millisecond, too little to time.
 * @type {Object<string, number>}
 */
const REPETITIONS = { interpreted: 3, compiled: 100 };

/**
 * The milliseconds a call takes, the best of `RUNS` samples after one not
 * timed, which lets the state just set settle; and its value.
 * @param {Object} pair - The interpreter and environment.
 * @param {Object} ast - The call.
 * @param {number} repetitions - How many times a sample makes it.
 * @returns {{ms: number, value: *}}
 */
function bestTime({ interpreter, env }, ast, repetitions) {
  let ms = Infinity;
  let value;
  for (let r = 0; r <= RUNS; r++) {
    const start = performance.now();
    for (let i = 0; i < repetitions; i++) value = interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
    if (r > 0) ms = Math.min(ms, (performance.now() - start) / repetitions);
  }
  return { ms, value };
}

const tiers = { interpreted: setUp(false), compiled: setUp(true) };

console.log(`The debugger, not stopping: best of ${RUNS}, milliseconds, and the factor over no debugger`);
for (const [label, call] of CALLS) {
  const ast = analyze(parse(call, { filename: 'call.scm' })[0]);
  console.log(`\n${label}  ${call}`);
  console.log(`  ${''.padEnd(28)} ${'interpreted'.padStart(18)} ${'compiled'.padStart(18)}`);
  const base = {};
  const answers = new Set();
  for (const state of STATES) {
    const cells = [];
    for (const [tier, pair] of Object.entries(tiers)) {
      pair.breakpoints = state.set(pair.interpreter, pair.runtime) ?? [];
      const { ms, value } = bestTime(pair, ast, REPETITIONS[tier]);
      answers.add(String(value));
      base[tier] ??= ms;
      cells.push(`${ms.toFixed(ms < 1 ? 3 : 1).padStart(9)} ${`${(ms / base[tier]).toFixed(2)}x`.padStart(8)}`);
      for (const id of pair.breakpoints ?? []) pair.runtime.removeBreakpoint(id);
      pair.breakpoints = [];
      pair.interpreter.setDebugRuntime(null);
    }
    console.log(`  ${state.label.padEnd(28)} ${cells.join(' ')}`);
  }
  if (answers.size !== 1) throw new Error(`${label}: the runs answered differently: ${[...answers].join(', ')}`);
}
