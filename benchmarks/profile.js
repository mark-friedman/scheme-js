/**
 * CPU profiler for the scheme-js-4 evaluator.
 *
 * Runs one benchmark under V8's sampling profiler and reports self time by
 * function, so that the effect of a change can be attributed rather than
 * guessed at. The profiler is driven through the inspector API in-process,
 * which avoids the noise that `node --cpu-prof` picks up from module loading
 * and library bootstrap.
 *
 * Usage:
 *   node benchmarks/profile.js [benchmark] [--size N] [--top N] [--json out.json]
 *   npm run benchmark:profile
 *
 * Example:
 *   node benchmarks/profile.js fib --size 25 --top 20
 */

import fs from 'fs';
import path from 'path';
import { Session } from 'inspector';

import { BENCHMARKS, sizeFor } from './programs/manifest.js';
import { PROGRAM_DIR, createBenchmarkInterpreter, renderResult, RUN_OPTIONS } from './lib/harness.js';

/**
 * Invokes the evaluator's trampoline.
 *
 * This exists as its own named function purely for profile legibility. V8
 * inlines the body of `Interpreter.run` -- which is one long-running `while`
 * loop -- into whichever function calls it, so that caller absorbs the
 * trampoline's self time. Giving the call site a name keeps that time
 * attributable to the evaluator instead of hiding it inside `main`.
 *
 * @param {Object} interpreter - The interpreter instance.
 * @param {Object} ast - The analyzed expression to evaluate.
 * @param {Object} env - The environment to evaluate in.
 * @returns {*} The result of evaluation.
 */
function trampoline(interpreter, ast, env) {
  return interpreter.run(ast, env, [], undefined, RUN_OPTIONS);
}

/**
 * Aggregates a V8 CPU profile into self time per function.
 * @param {Object} profile - A V8 `Profiler.takeProfile` result.
 * @returns {{total: number, rows: Array<{key: string, us: number, share: number}>}}
 *   Total sampled microseconds and per-function self time, descending.
 */
function summarize(profile) {
  const byId = new Map();
  for (const node of profile.nodes) byId.set(node.id, node);

  const self = new Map();
  for (let i = 0; i < profile.samples.length; i++) {
    const node = byId.get(profile.samples[i]);
    if (!node) continue;
    const delta = profile.timeDeltas[i] || 0;
    const frame = node.callFrame;
    const file = (frame.url || '').split('/').slice(-2).join('/');
    const key = `${frame.functionName || '(anonymous)'}  @${file}:${frame.lineNumber + 1}`;
    self.set(key, (self.get(key) || 0) + delta);
  }

  const total = [...self.values()].reduce((a, b) => a + b, 0);
  const rows = [...self.entries()]
    .map(([key, us]) => ({ key, us, share: total > 0 ? us / total : 0 }))
    .sort((a, b) => b.us - a.us);

  return { total, rows };
}

async function main() {
  const args = process.argv.slice(2);
  const name = args.find(a => !a.startsWith('--')) || 'fib';
  const sizeIndex = args.indexOf('--size');
  const topIndex = args.indexOf('--top');
  const jsonIndex = args.indexOf('--json');
  const top = topIndex >= 0 ? parseInt(args[topIndex + 1], 10) : 25;

  const bench = BENCHMARKS.find(b => b.name === name);
  if (!bench) {
    console.error(`Unknown benchmark '${name}'. Available: ${BENCHMARKS.map(b => b.name).join(', ')}`);
    process.exit(1);
  }
  const size = sizeIndex >= 0 ? parseInt(args[sizeIndex + 1], 10) : sizeFor(bench, 'quick');

  // Bootstrap and load the program BEFORE profiling starts, so the profile
  // reflects evaluation only and not parsing, analysis or library loading.
  const { interpreter, env, run, compile } = createBenchmarkInterpreter();
  run(`(define bench-size ${size})`);
  run(fs.readFileSync(path.join(PROGRAM_DIR, bench.file), 'utf8'));

  // Invoke the interpreter directly rather than through the harness wrapper:
  // V8 attributes inlined frames to their caller, so a wrapper closure in the
  // hot path silently absorbs the evaluator's own self time.
  const ast = compile('(bench-run)');
  trampoline(interpreter, ast, env); // warm up steady-state ICs

  const session = new Session();
  session.connect();

  const post = (method, params) => new Promise((resolve, reject) => {
    session.post(method, params, (err, result) => err ? reject(err) : resolve(result));
  });

  await post('Profiler.enable');
  await post('Profiler.setSamplingInterval', { interval: 100 });
  await post('Profiler.start');

  const start = performance.now();
  const value = trampoline(interpreter, ast, env);
  const elapsed = performance.now() - start;

  const { profile } = await post('Profiler.stop');
  session.disconnect();

  const { total, rows } = summarize(profile);

  console.log('='.repeat(78));
  console.log(`CPU profile: ${bench.name} (size ${size})`);
  console.log('='.repeat(78));
  console.log(`wall time ${elapsed.toFixed(1)} ms, result ${renderResult(value)}`);
  console.log(`sampled ${(total / 1000).toFixed(1)} ms across ${profile.samples.length} samples`);
  console.log('');
  console.log(' share |    self | function');
  console.log('-------|---------|--------------------------------------------------------');
  for (const row of rows.slice(0, top)) {
    console.log(
      `${(100 * row.share).toFixed(1).padStart(6)}% |` +
      `${(row.us / 1000).toFixed(1).padStart(8)} | ${row.key}`
    );
  }

  // Interpretive overhead is anything inside the evaluator itself; real work is
  // primitives. The split is the headline number for the compiler argument.
  // `trampoline` counts as evaluator time because V8 inlines Interpreter.run's
  // loop into it -- see the comment on that function.
  const interpreterShare = rows
    .filter(r => /interpreter\/(interpreter|frames|ast_nodes|environment|stepables)|^trampoline /.test(r.key))
    .reduce((a, r) => a + r.share, 0);
  const primitiveShare = rows
    .filter(r => /primitives\//.test(r.key))
    .reduce((a, r) => a + r.share, 0);

  console.log('');
  console.log(`evaluator overhead: ${(100 * interpreterShare).toFixed(1)}%`);
  console.log(`primitive work:     ${(100 * primitiveShare).toFixed(1)}%`);

  if (jsonIndex >= 0) {
    const out = args[jsonIndex + 1];
    fs.writeFileSync(out, JSON.stringify({
      benchmark: bench.name, size, elapsedMs: elapsed,
      interpreterShare, primitiveShare, rows
    }, null, 2));
    console.log(`\nwrote ${out}`);
  }
}

main().catch(err => {
  console.error('Profiling failed:', err);
  process.exit(1);
});
