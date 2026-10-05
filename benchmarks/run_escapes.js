/**
 * What the policy for procedures that capture a continuation costs on
 * escapes.
 *
 * The compiler tier used to decline a procedure that captures, and every
 * procedure that can reach one (`src/compiler/safety.scm`), because compiling a
 * capture made `btsearch` 2x slower: a backtracking search re-enters its
 * continuations, and each capture unwinds and reifies the compiled frames
 * beneath it. Real libraries mostly capture for another reason -- to return
 * early, the continuation called once, before the capture returns, often from
 * inside a callback. Of the corpus `decline_reasons.js --corpus` measures,
 * nearly every `call/cc` is that. This times that shape three ways:
 * everything interpreted; the old rule, declining; and every procedure
 * compiled, captures included, which is the tier's default now.
 *
 * Two shapes, each at several depths of compiled frames beneath the capture,
 * since unwinding costs more the more there are:
 *  - `callback`: `(call/cc (lambda (return) (for-each (lambda (x) ...
 *    (return x)) lst) #f))`, as SRFI 113 and SRFI 146 search.
 *  - `abort`: the continuation called directly from a recursion, as SRFI 1's
 *    fold helpers abort.
 *
 * Usage: node benchmarks/run_escapes.js [--calls N]
 */

import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/expand.js';
import { DefineNode } from '../src/core/interpreter/ast_nodes.js';
import { tryCompileDefinition } from '../src/compiler/index.js';
import { unsafeDefinitions } from '../src/compiler/index.js';
import { settle } from '../src/compiler/runtime.js';
import { interpretedLibrary, installStandardLibrary } from '../tests/harness/standard_library.js';

const args = process.argv.slice(2);
const CALLS = Number(args.includes('--calls') ? args[args.indexOf('--calls') + 1] : 20000);

/**
 * The program: an escaping search of each shape, a recursion `depth` frames
 * deep above it, and a loop driving it.
 * @type {string}
 */
const PROGRAM = `
(define (find-first pred lst)
  (call/cc
    (lambda (return)
      (for-each (lambda (x) (if (pred x) (return x))) lst)
      #f)))

(define (all-below? limit lst)
  (call/cc
    (lambda (abort)
      (let recur ((lst lst))
        (cond ((null? lst) #t)
              ((>= (car lst) limit) (abort #f))
              (else (recur (cdr lst))))))))

(define items
  (let loop ((i 0) (acc '()))
    (if (= i 20) acc (loop (+ i 1) (cons i acc)))))

(define (callback n) (find-first (lambda (x) (= x n)) items))
(define (abort n) (if (all-below? n items) 1 0))

(define (deep shape d n)
  (if (= d 0) (shape n) (+ 0 (deep shape (- d 1) n))))

(define (drive shape calls depth)
  (let loop ((i 0) (sum 0))
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (deep shape depth (remainder i 20)))))))`;

/**
 * Sets the program up under one policy.
 * @param {string} policy - `interpreted`, `declining` or `compiled`.
 * @returns {{pair: Object, compiled: Array<string>}} Where it runs, and the
 *   procedures compiled.
 */
function setUp(policy) {
  const pair = interpretedLibrary();
  installStandardLibrary(pair.env);
  const asts = parse(PROGRAM).map((form) => analyze(form));
  const declined = policy === 'declining'
    ? unsafeDefinitions(asts.filter((ast) => ast instanceof DefineNode), pair.env)
    : new Map();
  const compiled = [];
  for (const ast of asts) {
    if (policy !== 'interpreted' && ast instanceof DefineNode && !declined.has(ast.name)) {
      const result = tryCompileDefinition(ast, pair.env, { declineCaptures: policy === 'declining' });
      if (result.compiled) {
        pair.env.define(result.name, result.procedure);
        compiled.push(ast.originalName ?? ast.name);
        continue;
      }
    }
    settle(pair.interpreter.run(ast, pair.env, [], undefined, { jsAutoConvert: 'raw' }));
  }
  return { pair, compiled };
}

/**
 * Times one shape at one depth: microseconds per search, after a warm-up.
 * @param {Object} pair - Where the program runs.
 * @param {string} shape - `callback` or `abort`.
 * @param {number} depth - Frames beneath the capture.
 * @returns {{us: number, value: *}} The time, and the loop's total.
 */
function time(pair, shape, depth) {
  const run = (calls) => settle(pair.interpreter.run(
    analyze(parse(`(drive ${shape} ${calls} ${depth})`)[0]), pair.env, [], undefined, { jsAutoConvert: 'raw' }));
  run(Math.max(100, CALLS / 10));
  const start = performance.now();
  const value = run(CALLS);
  return { us: (performance.now() - start) * 1000 / CALLS, value };
}

const policies = ['interpreted', 'declining', 'compiled'];
const tiers = Object.fromEntries(policies.map((policy) => [policy, setUp(policy)]));
for (const policy of policies) console.log(`${policy.padEnd(11)} compiles: ${tiers[policy].compiled.join(' ') || 'nothing'}`);
console.log(`\n${'shape'.padEnd(9)}${'depth'.padStart(6)}${policies.map((p) => p.padStart(13)).join('')}   compiled vs declining`);
for (const shape of ['callback', 'abort']) {
  for (const depth of [0, 10, 50]) {
    const results = policies.map((policy) => time(tiers[policy].pair, shape, depth));
    if (new Set(results.map((r) => String(r.value))).size !== 1) throw new Error(`${shape} ${depth}: the policies disagree`);
    const cells = results.map((r) => `${r.us.toFixed(2)} us`.padStart(13)).join('');
    console.log(`${shape.padEnd(9)}${String(depth).padStart(6)}${cells}   ${(results[1].us / results[2].us).toFixed(2)}x`);
  }
}
