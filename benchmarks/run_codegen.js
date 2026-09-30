/**
 * @fileoverview Targeted benchmarks for code-generation decisions.
 *
 * ## Why this exists
 *
 * The canonical suite (`run_r7rs.js`) says what a change is worth to whole
 * programs, by workload class, and that is the measure that decides whether a
 * change ships. It is a poor instrument for a change aimed at one construct.
 * A construct that is hot in some programs and absent from the suite's -- a
 * `case` dispatch, say -- can get much faster or much slower and move no class
 * at all. So each code-generation decision also gets a workload here that
 * exercises exactly that construct, in the shapes that decide its cost, and is
 * timed in both tiers.
 *
 * ## What is measured
 *
 * Each workload is a loop calling one small procedure -- a `case` dispatcher,
 * for instance -- with an argument read from a global, so nothing can be
 * folded at compile time, and adding up what it returns. The same loop around a
 * procedure that just returns 1 is the baseline, and figures are nanoseconds
 * per call with the baseline subtracted, best of several runs. The loop and the
 * procedure are compiled in the compiled tier and interpreted in the
 * interpreter tier; the standard library is compiled in both, as it ships.
 * Each workload's total is compared across the tiers, so a figure cannot come
 * from code that computes something else.
 *
 * Add a group when a code-generation decision is made, beside its entry in
 * `docs/compiler_plan.md`. A group whose calls are long can take fewer of them
 * with `scale`.
 *
 * Usage:
 *   node benchmarks/run_codegen.js [--ops N] [--runs N] [--only group] [--json]
 */

import { createBenchmarkInterpreter } from './lib/harness.js';
import { tryCompileDefinition } from '../src/compiler/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { settle } from '../src/compiler/runtime.js';
import { LambdaNode } from '../src/core/interpreter/ast_nodes.js';

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};

const OPS = Number(valueOf('--ops', 200000));
const RUNS = Number(valueOf('--runs', 5));
const ONLY = valueOf('--only', null);

/**
 * The interpreter is tens of times slower, so it gets fewer calls for about
 * the same wall time. Per-call figures do not depend on the count.
 */
const INTERPRETER_OPS = Math.max(1000, Math.floor(OPS / 20));

/**
 * The workload groups. Each has definitions -- the procedures under test and
 * the globals they are called with -- and workloads, each a call expression.
 * The baseline, `(one key-a)`, is added to every group.
 */
const GROUPS = [
  {
    name: 'case',
    about: 'dispatch on eqv?: a symbol, boolean or () datum is ===, other data call eqv?',
    definitions: `
      (define (one x) 1)
      (define (two-clauses x) (case x ((a) 1) ((b) 2) (else 0)))
      (define (eight-clauses x)
        (case x ((a) 1) ((b) 2) ((c) 3) ((d) 4) ((e) 5) ((f) 6) ((g) 7) ((h) 8) (else 0)))
      (define (three-per-clause x)
        (case x ((a b c) 1) ((d e f) 2) ((g h i) 3) ((j k l) 4) (else 0)))
      (define (eight-integers x)
        (case x ((1) 1) ((2) 2) ((3) 3) ((4) 4) ((5) 5) ((6) 6) ((7) 7) ((8) 8) (else 0)))
      (define (eight-characters x)
        (case x ((#\\a) 1) ((#\\b) 2) ((#\\c) 3) ((#\\d) 4) ((#\\e) 5) ((#\\f) 6) ((#\\g) 7)
                ((#\\h) 8) (else 0)))
      (define key-a 'a)
      (define key-h 'h)
      (define key-l 'l)
      (define key-z 'z)
      (define key-8 8)
      (define key-char-h #\\h)`,
    workloads: [
      ['symbol, 2 clauses, first matches', '(two-clauses key-a)'],
      ['symbol, 8 clauses, last matches', '(eight-clauses key-h)'],
      ['symbol, 8 clauses, falls to else', '(eight-clauses key-z)'],
      ['symbol, 3 data a clause, last matches', '(three-per-clause key-l)'],
      ['exact integer, 8 clauses, last matches', '(eight-integers key-8)'],
      ['character, 8 clauses, last matches', '(eight-characters key-char-h)']
    ]
  },
  {
    name: 'arithmetic',
    about: 'two exact integers or two flonums are one JavaScript operator; other pairs take the tower',
    definitions: `
      (define (one x) 1)
      (define (add a b) (if (= (+ a b) 0) 0 1))
      (define (multiply-subtract a b) (if (< (- (* a b) a) b) 1 1))
      (define (polynomial x) (if (> (+ (* x x) (* 2.0 x) 1.0) -1.0) 1 0))
      (define key-a 'a)
      (define exact-1 3)
      (define exact-2 4)
      (define flonum-1 1.5)
      (define flonum-2 2.25)
      (define rational 1/3)`,
    workloads: [
      ['exact integers: + and =', '(add exact-1 exact-2)'],
      ['flonums: + and =', '(add flonum-1 flonum-2)'],
      ['flonums: *, - and <', '(multiply-subtract flonum-1 flonum-2)'],
      ['flonum polynomial: 3 ops, 2 constants, >', '(polynomial flonum-1)'],
      ['exact integer and flonum: + and =', '(add exact-1 flonum-2)'],
      ['rational and flonum: + and =', '(add rational flonum-2)']
    ]
  },
  {
    name: 'vectors',
    about: 'vector-ref, vector-set! and vector-length on an array and an exact index in range',
    definitions: `
      (define (one x) 1)
      (define (ref v) (if (eq? (vector-ref v 3) 'never) 0 1))
      (define (set v) (vector-set! v 3 'x) 1)
      (define (len v) (if (= (vector-length v) 0) 0 1))
      (define (swap v) (let ((a (vector-ref v 1))) (vector-set! v 1 (vector-ref v 2)) (vector-set! v 2 a) 1))
      (define (sum-of v)
        (let loop ((i 0) (acc 0))
          (if (= i (vector-length v)) (if (= acc -1) 0 1) (loop (+ i 1) (+ acc (vector-ref v i))))))
      (define key-a 'a)
      (define small (vector 1 2 3 4 5 6 7 8))
      (define scratch (vector 'a 'b 'c 'd))`,
    workloads: [
      ['vector-ref', '(ref small)'],
      ['vector-set!', '(set scratch)'],
      ['vector-length', '(len small)'],
      ['swap two elements: 2 refs, 2 sets', '(swap scratch)'],
      ['sum of 8 elements: a loop of refs', '(sum-of small)']
    ]
  },
  {
    name: 'tail-calls',
    about: 'a tail call to another procedure: made directly within a stack budget, else trampolined',
    definitions: `
      (define (one x) 1)
      (define (target x) 1)
      (define (hop x) (target x))
      (define (hop-2 x) (hop-3 x))
      (define (hop-3 x) (hop-4 x))
      (define (hop-4 x) (target x))
      (define (count-down n) (if (= n 0) 1 (count-down-again (- n 1))))
      (define (count-down-again n) (count-down n))
      (define (to-primitive s) (string-length s))
      (define key-a 'a)
      (define ten 10)
      (define word "abc")`,
    workloads: [
      ['one tail call to a compiled procedure', '(hop key-a)'],
      ['a chain of three tail calls', '(hop-2 key-a)'],
      ['mutual recursion: 10 tail calls', '(count-down ten)'],
      ['a tail call to a primitive', '(to-primitive word)']
    ]
  },
  {
    // Every call site with no raw entry on its callee tells a primitive or a
    // compiled procedure, called directly, from a JavaScript function, called
    // as the interpreter calls one -- its arguments converted and moves of
    // frames suspended (`callForeign` in src/compiler/runtime.js).
    name: 'calls',
    about: 'a callee with no raw entry: a primitive not inlined, a compiled procedure, a JavaScript function',
    definitions: `
      (define (one x) 1)
      (define (length-of s) (if (= (string-length s) 0) 0 1))
      (define (through f x) (if (eq? (f x) 0) 0 1))
      (define (ident x) x)
      (define key-a 'a)
      (define word "hello")
      (define js-ident (js-eval "(x) => x"))`,
    workloads: [
      ['a primitive the compiler does not inline', '(length-of word)'],
      ['a compiled procedure, passed in', '(through ident key-a)'],
      ['a JavaScript function, passed in', '(through js-ident key-a)']
    ]
  },
  {
    name: 'recursion',
    about: 'non-tail recursion: each procedure that calls takes its frame from the room left on the stack',
    definitions: `
      (define (one x) 1)
      (define (count n) (if (= n 0) 1 (+ 0 (count (- n 1)))))
      (define (fib n) (if (< n 2) 1 (if (= (+ (fib (- n 1)) (fib (- n 2))) 0) 0 1)))
      (define key-a 'a)
      (define ten 10)
      (define hundred 100)`,
    workloads: [
      ['10 levels deep', '(count ten)'],
      ['100 levels deep', '(count hundred)'],
      ['fib 10: 177 calls, 2 a frame', '(fib ten)']
    ]
  },
  {
    // Deeper than the JavaScript stack holds compiled frames, so the frames
    // move to the heap on the way down; the interpreter keeps them there
    // anyway. Far fewer calls per timing, each being tens of thousands.
    name: 'deep-recursion',
    about: 'recursion deeper than the JavaScript stack: compiled frames move to the heap',
    scale: 1 / 4000,
    definitions: `
      (define (one x) 1)
      (define (count n) (if (= n 0) 1 (+ 0 (count (- n 1)))))
      (define key-a 'a)
      (define twenty-thousand 20000)
      (define hundred-thousand 100000)`,
    workloads: [
      ['20,000 levels deep', '(count twenty-thousand)'],
      ['100,000 levels deep', '(count hundred-thousand)']
    ]
  }
];

/**
 * The loop around one call: adds up what the call returns, `n` times.
 * @param {string} name - The loop procedure's name.
 * @param {string} call - The call expression.
 * @returns {string} Its definition.
 */
function loopDefinition(name, call) {
  return `(define (${name} n)
            (let loop ((i 0) (acc 0))
              (if (= i n) acc (loop (+ i 1) (+ acc ${call})))))`;
}

/**
 * Builds an environment holding a group, compiled or interpreted.
 * @param {Object} group - The group.
 * @param {boolean} compiled - Whether to compile its procedures.
 * @returns {{interpreter: Object, env: Object}} The interpreter and environment.
 */
function build(group, compiled) {
  const { interpreter, env } = createBenchmarkInterpreter({ compileStdlib: true });
  const loops = [['baseline', '(one key-a)'], ...group.workloads]
    .map(([, call], i) => loopDefinition(`workload-${i}`, call));
  for (const form of parse(group.definitions + loops.join('\n'))) {
    const ast = analyze(form);
    const result = compiled ? tryCompileDefinition(ast, env) : { compiled: false };
    if (result.compiled) {
      env.define(result.name, result.procedure);
    } else {
      // A procedure the tier declined would measure the interpreter under
      // the compiled tier's name.
      if (compiled && (ast.valueExpr ?? ast.value) instanceof LambdaNode) {
        throw new Error(`the tier declined ${ast.name}: ${result.reason}`);
      }
      interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
    }
  }
  return { interpreter, env };
}

/**
 * Times a loop: the best of several runs, in nanoseconds per call.
 * @param {Object} interpreter - The interpreter to run it with.
 * @param {Object} env - The environment it is defined in.
 * @param {string} loop - The loop procedure's name.
 * @param {number} ops - Calls per run.
 * @returns {{ns: number, total: *}} Per-call time, and the loop's result.
 */
function time(interpreter, env, loop, ops) {
  // Called by the interpreter, as a program calls compiled code: compiled code
  // may move its frames to the heap only beneath the interpreter, which the
  // deep recursion group needs. One entry per run, not per call. `settle` runs
  // out a tail call the loop may end in.
  const call = (n) => settle(interpreter.run(
    analyze(parse(`(${loop} ${n})`)[0]), env, [], undefined, { jsAutoConvert: 'raw' }));
  call(Math.max(1, Math.floor(ops / 10)));
  let best = Infinity;
  let total;
  for (let r = 0; r < RUNS; r++) {
    const start = process.hrtime.bigint();
    total = call(ops);
    best = Math.min(best, Number(process.hrtime.bigint() - start) / ops);
  }
  return { ns: best, total };
}

/**
 * Measures one group in one tier.
 * @param {Object} group - The group.
 * @param {boolean} compiled - Which tier.
 * @returns {Array<{label: string, ns: number, total: *}>} Net per-call figures.
 */
function measure(group, compiled) {
  const { interpreter, env } = build(group, compiled);
  const ops = Math.max(5, Math.floor((compiled ? OPS : INTERPRETER_OPS) * (group.scale ?? 1)));
  const labels = ['baseline', ...group.workloads.map(([label]) => label)];
  const raw = labels.map((label, i) => ({ label, ...time(interpreter, env, `workload-${i}`, ops) }));
  // Totals scale with the count, so compare them per call.
  return raw.map((r) => ({
    label: r.label,
    ns: r.label === 'baseline' ? r.ns : r.ns - raw[0].ns,
    perCall: Number(r.total) / ops
  }));
}

const fmt = (ns) => ns.toFixed(1).padStart(10);
const results = {};
console.log(`best of ${RUNS}; ns per call, the baseline loop subtracted `
  + `(${OPS} calls compiled, ${INTERPRETER_OPS} interpreted)\n`);

for (const group of GROUPS) {
  if (ONLY && group.name !== ONLY) continue;
  const compiled = measure(group, true);
  const interpreted = measure(group, false);
  results[group.name] = compiled.map((c, i) => ({
    label: c.label, compiled: c.ns, interpreted: interpreted[i].ns
  }));

  console.log(`## ${group.name} -- ${group.about}\n`);
  console.log(`${''.padEnd(42)}${'compiled'.padStart(10)}${'interpreted'.padStart(12)}`);
  for (let i = 0; i < compiled.length; i++) {
    const c = compiled[i];
    const agree = c.perCall === interpreted[i].perCall;
    console.log(`${c.label.padEnd(42)}${fmt(c.ns)}  ${fmt(interpreted[i].ns)}`
      + (agree ? '' : `   TIERS DISAGREE: ${c.perCall} vs ${interpreted[i].perCall}`));
  }
  console.log('');
}

if (args.includes('--json')) console.log(JSON.stringify(results, null, 2));
