/**
 * @fileoverview What SRFI 125 hash tables and record access cost under the
 * compiler tier.
 *
 * ## Why this exists
 *
 * Compiler analyses written in Scheme will live on hash tables and records, so
 * the tier's handling of both sits on every analysis's hottest path. The last
 * time a hot path like that went unmeasured, `ir.scm` spent most of its time
 * calling an interpreted `memq` and `assq`, and it cost 10x. This measures
 * before anything depends on the answer.
 *
 * ## What is measured
 *
 * Every workload is a compiled loop -- user code under the tier, as analyses
 * will be -- that cycles through a fixed set of keys. Each is timed with the
 * table library interpreted and with it compiled, since a library imported
 * after start-up is not compiled unless something compiles it:
 *
 *   - `eq?` lookups, hits only, at 4 to 256 keys, beside `assq` on an alist of
 *     the same keys: where a table starts to win is what decides which of
 *     `ir.scm`'s lists are worth replacing.
 *   - `equal?` lookups on two-element list keys, beside `assoc`.
 *   - `hash-table-update!/default`, counting occurrences.
 *   - A record accessor read, beside `car`.
 *
 * Figures are nanoseconds per operation, with the cost of the same loop doing
 * no lookup subtracted, best of several runs.
 *
 * Usage:
 *   node benchmarks/run_hash_tables.js [--ops N] [--runs N]
 */

import fs from 'fs';
import path from 'path';

import { createBenchmarkInterpreter, PROJECT_ROOT } from './lib/harness.js';
import { compileEnvironment, tryCompileDefinition } from '../src/compiler/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { invoke, settle } from '../src/compiler/runtime.js';

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? Number(args[i + 1]) : fallback;
};

const OPS = valueOf('--ops', 50000);
const RUNS = valueOf('--runs', 5);
const SIZES = [4, 8, 16, 32, 64, 256];

/** The library's sources, in load order. */
const LIBRARY = [
  'src/core/scheme/case_lambda.scm',
  'src/extras/scheme/comparator.scm',
  'src/extras/scheme/hash_table.scm'
];

/**
 * The workload's Scheme: data built at a size, and one loop per operation.
 *
 * Every loop has the same shape and differs only in the expression it adds up,
 * so subtracting the `baseline` loop leaves the operation itself.
 *
 * @param {number} size - How many keys.
 * @returns {{setup: string, loops: Object<string, string>}}
 */
function workload(size) {
  const loop = (name, expr) => `
    (define (${name} n)
      (let loop ((i 0) (ks keys) (acc 0))
        (cond ((= i n) acc)
              ((null? ks) (loop i keys acc))
              (else (let ((k (car ks)))
                      (loop (+ i 1) (cdr ks) (+ acc ${expr})))))))`;
  const listLoop = (name, expr) => `
    (define (${name} n)
      (let loop ((i 0) (ks list-keys) (acc 0))
        (cond ((= i n) acc)
              ((null? ks) (loop i list-keys acc))
              (else (let ((k (car ks)))
                      (loop (+ i 1) (cdr ks) (+ acc ${expr})))))))`;
  return {
    setup: `
      (define keys
        (let build ((i 0) (acc '()))
          (if (= i ${size}) acc
              (build (+ i 1) (cons (string->symbol (string-append "k" (number->string i))) acc)))))
      (define list-keys (map (lambda (k) (list k 1)) keys))
      (define alist (map (lambda (k) (cons k 1)) keys))
      (define list-alist (map (lambda (k) (cons k 1)) list-keys))
      (define eq-table (make-hash-table eq?))
      (for-each (lambda (k) (hash-table-set! eq-table k 1)) keys)
      (define equal-table (make-hash-table equal?))
      (for-each (lambda (k) (hash-table-set! equal-table k 1)) list-keys)
      (define counts (make-hash-table eq?))
      (define-record-type point (make-point x y) point? (x point-x) (y point-y))
      (define a-point (make-point 1 2))
      (define a-pair (cons 1 2))`,
    loops: {
      baseline: loop('baseline', '1'),
      'eq? table': loop('eq-table-ref', '(hash-table-ref/default eq-table k 0)'),
      assq: loop('assq-ref', '(cdr (assq k alist))'),
      'equal? table': listLoop('equal-table-ref', '(hash-table-ref/default equal-table k 0)'),
      assoc: listLoop('assoc-ref', '(cdr (assoc k list-alist))'),
      'update!/default': loop('update-count',
        '(begin (hash-table-update!/default counts k (lambda (c) (+ c 1)) 0) 1)'),
      'record accessor': loop('record-read', '(point-x a-point)'),
      car: loop('car-read', '(car a-pair)')
    }
  };
}

/**
 * Builds an environment holding the workload at a size, with every loop
 * compiled.
 * @param {number} size - How many keys.
 * @param {boolean} compileLibrary - Whether to compile the table library.
 * @returns {{env: Object, interpreter: Object, uncompiled: Array<string>}}
 */
function build(size, compileLibrary) {
  const { interpreter, env, run } = createBenchmarkInterpreter({ compileStdlib: true });
  for (const file of LIBRARY) run(fs.readFileSync(path.join(PROJECT_ROOT, file), 'utf8'));
  if (compileLibrary) compileEnvironment(env);

  const { setup, loops } = workload(size);
  run(setup);
  const uncompiled = [];
  for (const source of Object.values(loops)) {
    const ast = analyze(parse(source)[0]);
    const result = tryCompileDefinition(ast, env);
    if (result.compiled) env.define(result.name, result.procedure);
    else {
      uncompiled.push(ast.name);
      interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
    }
  }
  return { env, interpreter, uncompiled };
}

/**
 * Times a compiled loop: the best of several runs, in nanoseconds per
 * iteration.
 * @param {Function} loop - The compiled procedure.
 * @returns {number}
 */
function time(loop) {
  // `invoke` enters the procedure the way compiled code calls it and `settle`
  // runs its tail calls to the end; calling it as a JavaScript function would
  // return its first tail call unrun.
  const call = (n) => settle(invoke(loop, [BigInt(n)]));
  const expected = BigInt(OPS);
  call(OPS / 10);
  let best = Infinity;
  for (let r = 0; r < RUNS; r++) {
    const start = process.hrtime.bigint();
    const result = call(OPS);
    best = Math.min(best, Number(process.hrtime.bigint() - start) / OPS);
    // Every loop adds 1 per operation, so a wrong total means it measured
    // something other than what it says.
    if (result !== expected) throw new Error(`loop returned ${result}, expected ${expected}`);
  }
  return best;
}

/** Procedure names by workload label. */
const NAMES = {
  baseline: 'baseline', 'eq? table': 'eq-table-ref', assq: 'assq-ref',
  'equal? table': 'equal-table-ref', assoc: 'assoc-ref',
  'update!/default': 'update-count', 'record accessor': 'record-read', car: 'car-read'
};

/**
 * Measures every workload at one size.
 * @param {number} size - How many keys.
 * @param {boolean} compileLibrary - Whether the table library is compiled.
 * @returns {Object<string, number>} Net nanoseconds per operation by workload.
 */
function measure(size, compileLibrary) {
  const { env, uncompiled } = build(size, compileLibrary);
  if (uncompiled.length > 0) {
    throw new Error(`loops the tier declined, which would measure the interpreter: ${uncompiled}`);
  }
  const raw = {};
  for (const [label, name] of Object.entries(NAMES)) raw[label] = time(env.lookup(name));
  const net = {};
  for (const label of Object.keys(NAMES)) {
    if (label !== 'baseline') net[label] = raw[label] - raw.baseline;
  }
  net.baseline = raw.baseline;
  return net;
}

const fmt = (ns) => ns.toFixed(1).padStart(8);

console.log(`${OPS} operations per run, best of ${RUNS}; ns per operation, loop cost subtracted\n`);

const results = {};
for (const compileLibrary of [false, true]) {
  const label = compileLibrary ? 'library compiled' : 'library interpreted';
  results[label] = {};
  console.log(`## ${label}\n`);
  console.log(`${'keys'.padStart(5)}${'eq? tbl'.padStart(9)}${'assq'.padStart(9)}` +
    `${'equal?'.padStart(9)}${'assoc'.padStart(9)}${'update!'.padStart(9)}` +
    `${'record'.padStart(9)}${'car'.padStart(9)}${'loop'.padStart(9)}`);
  for (const size of SIZES) {
    const net = measure(size, compileLibrary);
    results[label][size] = net;
    console.log(`${String(size).padStart(5)} ${fmt(net['eq? table'])} ${fmt(net.assq)}` +
      ` ${fmt(net['equal? table'])} ${fmt(net.assoc)} ${fmt(net['update!/default'])}` +
      ` ${fmt(net['record accessor'])} ${fmt(net.car)} ${fmt(net.baseline)}`);
  }
  console.log('');
}

if (args.includes('--json')) console.log(JSON.stringify(results, null, 2));
