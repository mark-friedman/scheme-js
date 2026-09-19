/**
 * CPU profiler for the compiler tier.
 *
 * Same approach as `profile.js`, but the program's definitions are compiled
 * first. Reports self time by function so that the remaining cost in *compiled*
 * code is attributable, rather than being guessed at from what looks expensive.
 *
 * Usage: node benchmarks/profile_compiled.js [benchmark] [--size N] [--top N]
 */

import fs from 'fs';
import path from 'path';
import { Session } from 'inspector';

import { BENCHMARKS, sizeFor } from './programs/manifest.js';
import { PROGRAM_DIR, createBenchmarkInterpreter, renderResult, RUN_OPTIONS } from './lib/harness.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { DefineNode } from '../src/core/interpreter/ast_nodes.js';
import { compileProgram } from '../src/compiler/index.js';
import { settle } from '../src/compiler/runtime.js';

/**
 * Aggregates a V8 profile into self time per function.
 * @param {Object} profile - A `Profiler.takeProfile` result.
 * @returns {{total: number, rows: Array<Object>}} Totals and per-function rows.
 */
function summarize(profile) {
  const byId = new Map(profile.nodes.map((n) => [n.id, n]));
  const self = new Map();
  for (let i = 0; i < profile.samples.length; i++) {
    const node = byId.get(profile.samples[i]);
    if (!node) continue;
    const frame = node.callFrame;
    const file = (frame.url || '').split('/').slice(-2).join('/') || '(generated)';
    const key = `${frame.functionName || '(anonymous)'}  @${file}:${frame.lineNumber + 1}`;
    self.set(key, (self.get(key) || 0) + (profile.timeDeltas[i] || 0));
  }
  const total = [...self.values()].reduce((a, b) => a + b, 0);
  const rows = [...self.entries()]
    .map(([key, us]) => ({ key, us, share: total > 0 ? us / total : 0 }))
    .sort((a, b) => b.us - a.us);
  return { total, rows };
}

/**
 * Runs the compiled entry point. Named so its self time -- which includes the
 * inlined generated code V8 folds into its caller -- stays attributable.
 * @param {Object} interpreter - The interpreter.
 * @param {Object} ast - The analyzed entry expression.
 * @param {Object} env - The environment.
 * @returns {*} The result.
 */
function drive(interpreter, ast, env) {
  return settle(interpreter.run(ast, env, [], undefined, RUN_OPTIONS));
}

async function main() {
  const args = process.argv.slice(2);
  const name = args.find((a) => !a.startsWith('--')) || 'fib';
  const sizeIndex = args.indexOf('--size');
  const topIndex = args.indexOf('--top');
  const top = topIndex >= 0 ? parseInt(args[topIndex + 1], 10) : 20;

  const bench = BENCHMARKS.find((b) => b.name === name);
  if (!bench) {
    console.error(`Unknown benchmark '${name}'. Available: ${BENCHMARKS.map((b) => b.name).join(', ')}`);
    process.exit(1);
  }
  const size = sizeIndex >= 0 ? parseInt(args[sizeIndex + 1], 10) : sizeFor(bench, 'quick');

  const { interpreter, env, run, compile } = createBenchmarkInterpreter();
  run(`(define bench-size ${size})`);
  const asts = parse(fs.readFileSync(path.join(PROGRAM_DIR, bench.file), 'utf8'))
    .map((form) => analyze(form));
  const definitions = asts.filter((a) => a instanceof DefineNode);
  const others = asts.filter((a) => !(a instanceof DefineNode));
  const outcome = compileProgram(definitions, env, interpreter);
  for (const ast of others) interpreter.run(ast, env, [], undefined, RUN_OPTIONS);

  const entry = compile('(bench-run)');
  drive(interpreter, entry, env); // warm up steady-state inline caches

  const session = new Session();
  session.connect();
  const post = (method, params) => new Promise((resolve, reject) =>
    session.post(method, params, (err, result) => err ? reject(err) : resolve(result)));

  await post('Profiler.enable');
  await post('Profiler.setSamplingInterval', { interval: 100 });
  await post('Profiler.start');
  const start = performance.now();
  const value = drive(interpreter, entry, env);
  const elapsed = performance.now() - start;
  const { profile } = await post('Profiler.stop');
  session.disconnect();

  const { total, rows } = summarize(profile);

  console.log('='.repeat(84));
  console.log(`Compiled profile: ${bench.name} (size ${size})`);
  console.log('='.repeat(84));
  console.log(`${outcome.compiled.length} procedure(s) compiled` +
    (outcome.unitDeclined ? `, unit declined: ${outcome.unitDeclined}` : ''));
  console.log(`wall time ${elapsed.toFixed(1)} ms, result ${renderResult(value)}`);
  console.log(`sampled ${(total / 1000).toFixed(1)} ms`);
  console.log('');
  console.log(' share |    self | function');
  console.log('-------|---------|------------------------------------------------------');
  for (const row of rows.slice(0, top)) {
    console.log(`${(100 * row.share).toFixed(1).padStart(6)}% |` +
      `${(row.us / 1000).toFixed(1).padStart(8)} | ${row.key}`);
  }

  // Generated code has no source file, so it is the way to spot how much time
  // is in compiled Scheme versus in the primitives and runtime it calls.
  const buckets = {
    'generated code': /@\(generated\)|<anonymous>/,
    'primitives': /primitives\//,
    'interpreter': /interpreter\//,
    'compiler runtime': /compiler\//
  };
  console.log('');
  for (const [label, pattern] of Object.entries(buckets)) {
    const share = rows.filter((r) => pattern.test(r.key)).reduce((a, r) => a + r.share, 0);
    console.log(`${label.padEnd(18)} ${(100 * share).toFixed(1)}%`);
  }
}

main().catch((err) => {
  console.error('Profiling failed:', err);
  process.exit(1);
});
