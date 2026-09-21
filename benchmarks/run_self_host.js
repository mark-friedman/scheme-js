/**
 * @fileoverview What the compiler costs to run on itself.
 *
 * The compiler's lowering pass is Scheme (`src/compiler/ir.scm`), so the tier
 * has a customer whose performance is the project's own. This measures that
 * customer three ways and checks that all three agree about every answer.
 *
 * ## The three configurations
 *
 *   - **interpreted** -- the bootstrap, and the CSP-safe mode. Whatever else
 *     changes, this has to keep working: it is what lets a checkout with no
 *     prebuilt code compile itself from nothing.
 *   - **compiled** -- `ir.scm` through the tier, standard library interpreted.
 *   - **compiled, with the library compiled too** -- what ships.
 *
 * The gap between the second and the third is the finding this measurement was
 * originally built to produce, and it is worth restating whenever it is read:
 * lowering calls `memq` and `assq` on every scope lookup and every global it
 * records, and those are themselves Scheme. With them interpreted the tier is
 * worth about 1.5x on this code; with them compiled, about 20x. Almost all of
 * a compiled module's cost can be the interpreted library underneath it.
 *
 * ## Agreement, first
 *
 * Every lambda in the corpus is lowered under all three configurations and the
 * results compared field by field, because a faster wrong answer is not a
 * result. This is a differential test of the compiler against its own source:
 * the interpreted run is the reference semantics, and a disagreement means the
 * tier changed the meaning of the compiler.
 *
 * ## Against JavaScript
 *
 * The pass used to be JavaScript. On the commit that removed it, the two
 * implementations agreed on all 952 lambdas in this corpus and the Scheme one
 * was 18x slower -- 70 ms a pass against 3.8 ms, plus 7 ms of marshalling. That
 * number is fixed in history rather than recomputed here, because keeping a
 * second implementation of the lowering to re-measure it is the cost the
 * comparison was being used to decide about.
 *
 * Usage:
 *   node benchmarks/run_self_host.js [--reps N]
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { createTestEnv, run as runScheme } from '../tests/harness/helpers.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../src/core/interpreter/index.js';
import { compileProgram } from '../src/compiler/index.js';
import { DefineNode, LambdaNode } from '../src/core/interpreter/ast_nodes.js';
import { Cons } from '../src/core/interpreter/cons.js';
import { invoke, settle } from '../src/compiler/runtime.js';
import { astToScheme, irToJs, toArray } from '../src/compiler/marshal.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const BOOTSTRAP = ['macros', 'equality', 'cxr', 'numbers', 'list', 'control', 'case_lambda'];
const COMPILER_SCHEME = 'src/compiler/ir.scm';

const args = process.argv.slice(2);
const REPS = parseInt(args[args.indexOf('--reps') + 1] ?? '3', 10);

/**
 * Bootstraps an interpreter with the standard library loaded.
 * @returns {{interpreter: Object, env: Object}} The pair.
 */
function bootstrap() {
  const { interpreter } = createTestEnv();
  for (const file of BOOTSTRAP) {
    runScheme(interpreter, fs.readFileSync(path.join(ROOT, `src/core/scheme/${file}.scm`), 'utf8'));
  }
  return { interpreter, env: interpreter.globalEnv };
}

/**
 * Creates a fresh environment sharing a bootstrapped one's bindings.
 *
 * Fresh rather than shared, so that compiling the library in one configuration
 * cannot leak into another and flatter it.
 *
 * @param {Object} template - The bootstrapped global environment.
 * @returns {{interpreter: Object, env: Object}} A fresh pair.
 */
function fresh(template) {
  const made = createInterpreter();
  for (const [name, value] of template.bindings) {
    if (!made.env.bindings.has(name)) made.env.define(name, value);
  }
  return { interpreter: made.interpreter, env: made.env };
}

/**
 * Loads the compiler's Scheme into an environment, compiling it or not.
 *
 * @param {Object} template - The bootstrapped global environment.
 * @param {boolean} compiled - Whether to run `ir.scm` through the tier.
 * @param {boolean} [stdlib=false] - Whether to compile the standard library.
 * @returns {Object} The `lower-lambda` procedure and what was compiled.
 */
function loadCompiler(template, compiled, stdlib = false) {
  const { interpreter, env } = fresh(template);

  let libCompiled = 0;
  let libTotal = 0;
  if (stdlib) {
    for (const file of BOOTSTRAP) {
      const src = fs.readFileSync(path.join(ROOT, `src/core/scheme/${file}.scm`), 'utf8');
      let defs;
      try {
        defs = parse(src).map((form) => analyze(form)).filter((a) => a instanceof DefineNode);
      } catch { continue; }
      // Definitions only: the library's macro definitions and side-effecting
      // top-level forms have already run during bootstrap, and re-running them
      // here would redefine what is being replaced.
      const out = compileProgram(defs, env, interpreter);
      libCompiled += out.compiled.length;
      libTotal += defs.length;
    }
  }

  const source = fs.readFileSync(path.join(ROOT, COMPILER_SCHEME), 'utf8');
  const asts = parse(source).map((form) => analyze(form));
  const definitions = asts.filter((a) => a instanceof DefineNode);

  let outcome = { compiled: [], declined: [] };
  if (compiled) {
    outcome = compileProgram(definitions, env, interpreter);
  } else {
    for (const ast of asts) {
      interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
    }
  }

  const entry = definitions.find((d) => (d.originalName || d.name) === 'lower-lambda');
  return {
    proc: env.lookup(entry.name),
    compiled: outcome.compiled.length,
    total: definitions.length,
    declined: outcome.declined,
    libCompiled, libTotal
  };
}

/**
 * Collects analyzed lambdas from real Scheme source, as a corpus to lower.
 * @returns {Array<{name: string, node: Object}>} The corpus.
 */
function corpus() {
  const files = [];
  const benchDir = path.join(ROOT, 'benchmarks/r7rs/src');
  for (const f of fs.readdirSync(benchDir).filter((f) => f.endsWith('.scm')).sort()) {
    files.push(path.join(benchDir, f));
  }
  for (const f of BOOTSTRAP) files.push(path.join(ROOT, `src/core/scheme/${f}.scm`));
  files.push(path.join(ROOT, COMPILER_SCHEME));

  const out = [];
  for (const file of files) {
    let forms;
    try { forms = parse(fs.readFileSync(file, 'utf8')); } catch { continue; }
    for (const form of forms) {
      let ast;
      try { ast = analyze(form); } catch { continue; }
      if (!(ast instanceof DefineNode)) continue;
      const value = ast.valueExpr ?? ast.value;
      if (!(value instanceof LambdaNode)) continue;
      out.push({ name: `${path.basename(file)}:${ast.name}`, node: value });
    }
  }
  return out;
}

/**
 * Renders an IR tree as text, for comparing configurations.
 *
 * Normalising here keeps the harmless disagreements out of the diff -- how
 * "absent" is spelled, what order globals were first seen in -- while leaving
 * every difference that matters.
 *
 * @param {*} node - A JavaScript IR node, or any value inside one.
 * @returns {string} A canonical rendering.
 */
function renderIr(node) {
  if (node === null || node === undefined || node === false) {
    return node === false ? '#f' : '()';
  }
  if (node === true) return '#t';
  if (typeof node === 'bigint') return `${node}n`;
  if (typeof node === 'number' || typeof node === 'string') return JSON.stringify(node);
  if (Array.isArray(node)) return `[${node.map(renderIr).join(' ')}]`;
  if (node instanceof Cons) {
    // Walked by hand rather than with `toArray`, which assumes a proper list.
    // A quoted literal can be a dotted pair, and one in the corpus is.
    const parts = [];
    let rest = node;
    while (rest instanceof Cons) { parts.push(renderIr(rest.car)); rest = rest.cdr; }
    if (rest !== null && rest !== undefined) parts.push('.', renderIr(rest));
    return `(${parts.join(' ')})`;
  }
  if (node.name !== undefined && node.constructor && node.constructor.name === 'Symbol') {
    return `'${node.name}`;
  }
  if (typeof node === 'object' && node.k !== undefined) {
    const keys = Object.keys(node).sort();
    return `{${keys.map((key) => `${key}:${renderIr(node[key])}`).join(' ')}}`;
  }
  return String(node);
}

/**
 * Lowers one lambda with one configuration's `lower-lambda`.
 * @param {Function} proc - A `lower-lambda` procedure.
 * @param {Object} node - An analyzed `LambdaNode`.
 * @returns {string} A canonical rendering of the whole result.
 */
function lowerAndRender(proc, node) {
  const parts = toArray(settle(invoke(proc, [astToScheme(node)])));
  if (parts[0].name === 'fail') return `fail ${JSON.stringify(parts[1])}`;
  const globals = toArray(parts[2]).map((s) => s.name).sort().join(' ');
  return `ok ${renderIr(irToJs(parts[1]))} globals[${globals}]`
    + ` unknown:${parts[3]} captures:${parts[4]}`;
}

/**
 * Times a function over the whole corpus.
 * @param {Array<*>} items - The inputs.
 * @param {Function} fn - What to do with each.
 * @param {number} reps - How many passes over the corpus.
 * @returns {number} Milliseconds per pass.
 */
function time(items, fn, reps) {
  const start = process.hrtime.bigint();
  for (let r = 0; r < reps; r++) for (const item of items) fn(item);
  const ns = Number(process.hrtime.bigint() - start);
  return ns / 1e6 / reps;
}

function main() {
  const { env: template } = bootstrap();

  const interpreted = loadCompiler(template, false);
  const compiled = loadCompiler(template, true);
  const withStdlib = loadCompiler(template, true, true);

  console.log('=== The compiler\'s own Scheme ===\n');
  console.log(`${COMPILER_SCHEME}   `
    + `${fs.readFileSync(path.join(ROOT, COMPILER_SCHEME), 'utf8').split('\n').length} lines`);
  console.log(`\nthe tier compiles ${compiled.compiled} of ${compiled.total} of its definitions`);
  for (const d of compiled.declined) console.log(`  declined ${d.name}: ${d.reason}`);
  console.log(`and ${withStdlib.libCompiled} of ${withStdlib.libTotal} standard library definitions`);

  const items = corpus();
  console.log(`\n=== Agreement, over ${items.length} lambdas from real source ===\n`);

  let agree = 0;
  const differences = [];
  for (const item of items) {
    const reference = lowerAndRender(interpreted.proc, item.node);
    const others = [
      ['compiled', lowerAndRender(compiled.proc, item.node)],
      ['compiled + stdlib', lowerAndRender(withStdlib.proc, item.node)]
    ];
    const bad = others.find(([, text]) => text !== reference);
    if (bad === undefined) agree++;
    else differences.push(`${item.name}: ${bad[0]} differs from interpreted`);
  }

  if (differences.length === 0) {
    console.log(`the tier does not change what the lowering answers: `
      + `${agree} of ${items.length}`);
  } else {
    console.log(`DISAGREEMENT on ${differences.length} of ${items.length}:`);
    for (const d of differences.slice(0, 20)) console.log(`  ${d}`);
    console.log('\nNot timing a compiler that changes its own meaning.');
    process.exit(1);
  }

  console.log(`\n=== Speed, over ${items.length} lambdas ===\n`);
  const nodes = items.map((i) => i.node);
  const measured = [
    ['interpreted', time(nodes, (n) => lowerAndRender(interpreted.proc, n), REPS)],
    ['compiled', time(nodes, (n) => lowerAndRender(compiled.proc, n), REPS)],
    ['compiled, + compiled stdlib', time(nodes, (n) => lowerAndRender(withStdlib.proc, n), REPS)]
  ];
  const slowest = measured[0][1];

  console.log('                                   per pass    vs interpreted');
  for (const [label, ms] of measured) {
    console.log(`  ${label.padEnd(30)} ${ms.toFixed(1).padStart(8)} ms`
      + `  ${(slowest / ms).toFixed(2).padStart(9)}x`);
  }

  console.log('');
  console.log(`the tier is worth ${(measured[0][1] / measured[1][1]).toFixed(2)}x on this workload, `
    + `${(measured[0][1] / measured[2][1]).toFixed(2)}x with the standard library compiled too`);
  console.log('Compiling the library is not an optimization of compiling the compiler --');
  console.log('lowering spends its time in memq and assq, which are themselves Scheme.');
}

main();
