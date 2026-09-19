/**
 * Runs the canonical R7RS benchmark suite under this implementation.
 *
 * ## What this is for
 *
 * `benchmarks/run_standard.js` runs eight programs written in Stage 0 against
 * this interpreter, and every optimization since was chosen by measuring
 * against them. That made the suite and the optimizations fit each other: 98%
 * of the microbenchmarks' calls land on a primitive the compiler inlines,
 * against 34% in real code, and the ~12x the tier reports there becomes 1.39x
 * on the project's own Scheme (R20 in `docs/compiler_strategy.md`). These
 * programs come from the Gabriel and Gambit lineage by way of Larceny, nobody
 * here chose them, and they cover workloads the eight do not touch at all --
 * flonums, bignums, bytevectors, strings, records, dynamic-wind.
 *
 * ## Reading the output
 *
 * Results are grouped by workload class and **never blended into a single
 * number**. There is no average Scheme program to weight the classes against,
 * so a blended figure would bake a guess about an unknown target workload into
 * every future decision -- which is how the overfitting above happened. The
 * summary therefore reports each class separately and names the *worst* class
 * as well as the best, because the worst class is the one that would have
 * caught the overfitting immediately.
 *
 * Usage:
 *   node benchmarks/run_r7rs.js [--profile default|full] [--target SECONDS]
 *                               [--only name,name] [--tier interpreter|compiled|both]
 */

import { execFileSync } from 'child_process';
import path from 'path';
import { fileURLToPath } from 'url';

import { selectBenchmarks, WORKLOADS } from './r7rs/manifest.js';
import { calibrate, checkResolution } from './lib/r7rs_harness.js';

const WORKER = path.join(path.dirname(fileURLToPath(import.meta.url)), 'lib', 'r7rs_worker.js');

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};

const PROFILE = valueOf('--profile', 'default');
const TARGET = parseFloat(valueOf('--target', '1.0'));
const TIER = valueOf('--tier', 'both');
const ONLY = valueOf('--only', null);

/**
 * Wall-clock budget for one measurement, in seconds.
 *
 * A benchmark that blows this is reported as exceeding it rather than being
 * allowed to stall the suite. That is a result, not a harness failure: `takl`
 * does not finish one iteration inside two minutes here while Gambit's
 * interpreter finishes it in well under a second, which says something about
 * this implementation worth knowing.
 */
const BUDGET = parseFloat(valueOf('--budget', '120'));

/** This implementation's clock resolution, for the trustworthiness check. */
const JIFFIES_PER_SECOND = 1000;

/**
 * Runs one measurement in a child process, under a wall-clock budget.
 *
 * Isolated because a benchmark can hang or exhaust the stack, and neither
 * should take the suite with it.
 *
 * @param {Object} bench - Manifest entry.
 * @param {number} count - Repetitions.
 * @param {boolean} useCompiler - Whether to run under the compiler tier.
 * @returns {{seconds: (number|null), incorrect: boolean, error: (string|null),
 *   compiled: number, definitions: number}} The child's result, or a
 *   synthesised one if it was killed.
 */
function runIsolated(bench, count, useCompiler) {
  const request = JSON.stringify({ name: bench.name, params: bench.params, count, useCompiler });
  try {
    const out = execFileSync(process.execPath, [WORKER, request], {
      stdio: ['pipe', 'pipe', 'pipe'], encoding: 'utf8', timeout: BUDGET * 1000
    });
    return JSON.parse(out);
  } catch (e) {
    const killed = e.killed || e.signal === 'SIGTERM';
    return {
      seconds: null, incorrect: false, compiled: 0, definitions: 0,
      error: killed ? `exceeded the ${BUDGET}s budget` : (e.message || 'failed').slice(0, 120)
    };
  }
}

/**
 * Runs one benchmark at a calibrated repetition count.
 *
 * The calibration run is thrown away rather than reused. It is the first time
 * the program executes, so it carries V8's warm-up as well as the work, and
 * counting it would flatter whichever tier happens to warm up faster.
 *
 * @param {Object} bench - Manifest entry.
 * @param {boolean} useCompiler - Whether to run under the compiler tier.
 * @returns {{seconds: (number|null), count: number, compiled: number,
 *   definitions: number, incorrect: boolean, error: (string|null),
 *   resolution: Object}} Per-iteration seconds and supporting detail.
 */
function measure(bench, useCompiler) {
  const c = calibrate((count) => runIsolated(bench, count, useCompiler), TARGET);
  const last = c.result;
  if (c.seconds === null) {
    return {
      seconds: null, count: c.count, compiled: last.compiled, definitions: last.definitions,
      incorrect: last.incorrect,
      error: c.unmeasurable ? 'below clock resolution' : last.error,
      resolution: { ok: false, message: null }
    };
  }
  return {
    seconds: c.seconds,
    count: c.count,
    compiled: last.compiled,
    definitions: last.definitions,
    incorrect: false,
    error: null,
    resolution: checkResolution(c.seconds * c.count, JIFFIES_PER_SECOND)
  };
}

/**
 * Geometric mean of a list of ratios.
 *
 * Geometric rather than arithmetic, because an arithmetic mean over ratios is
 * dominated by whichever entry improved most, which flatters the result.
 *
 * @param {number[]} values - Positive ratios.
 * @returns {number|null} The geometric mean, or null if the list is empty.
 */
function geometricMean(values) {
  if (values.length === 0) return null;
  const sum = values.reduce((acc, v) => acc + Math.log(v), 0);
  return Math.exp(sum / values.length);
}

/**
 * Formats seconds for a table cell.
 * @param {number|null} seconds - A per-iteration time.
 * @returns {string} A human-readable duration.
 */
function formatSeconds(seconds) {
  if (seconds === null) return '--';
  if (seconds >= 1) return `${seconds.toFixed(2)} s`;
  return `${(seconds * 1000).toFixed(1)} ms`;
}

/**
 * Entry point.
 * @returns {void}
 */
function main() {
  let benchmarks = selectBenchmarks(PROFILE);
  if (ONLY) {
    const wanted = new Set(ONLY.split(','));
    benchmarks = benchmarks.filter((b) => wanted.has(b.name));
  }

  const wantInterpreter = TIER === 'both' || TIER === 'interpreter';
  const wantCompiled = TIER === 'both' || TIER === 'compiled';

  console.log(`Canonical R7RS benchmarks -- profile '${PROFILE}', `
    + `${benchmarks.length} programs, calibrated to ~${TARGET}s per measurement`);
  console.log('Sources: ecraven/r7rs-benchmarks (Larceny, from Gabriel and Gambit).');
  console.log('');

  const rows = [];
  for (const bench of benchmarks) {
    const interpreted = wantInterpreter ? measure(bench, false) : null;
    const compiled = wantCompiled ? measure(bench, true) : null;

    let speedup = null;
    if (interpreted && compiled && interpreted.seconds && compiled.seconds) {
      speedup = interpreted.seconds / compiled.seconds;
    }

    const row = { name: bench.name, workload: bench.workload, interpreted, compiled, speedup };
    rows.push(row);

    const detail = compiled
      ? `${compiled.compiled}/${compiled.definitions} compiled`
      : '';
    const problem = (interpreted && interpreted.error) || (compiled && compiled.error)
      || ((interpreted && interpreted.incorrect) || (compiled && compiled.incorrect)
        ? 'WRONG ANSWER' : '');
    console.log(
      `  ${bench.name.padEnd(12)} ${bench.workload.padEnd(13)}`
      + ` ${formatSeconds(interpreted && interpreted.seconds).padStart(9)}`
      + ` ${formatSeconds(compiled && compiled.seconds).padStart(9)}`
      + ` ${(speedup ? speedup.toFixed(2) + 'x' : '--').padStart(7)}`
      + `  ${detail}${problem ? '  ' + problem.slice(0, 60) : ''}`
    );
  }

  const lowConfidence = rows.filter((r) =>
    (r.interpreted && r.interpreted.seconds !== null && !r.interpreted.resolution.ok)
    || (r.compiled && r.compiled.seconds !== null && !r.compiled.resolution.ok));
  if (lowConfidence.length > 0) {
    console.log('');
    console.log('LOW CONFIDENCE (too few clock ticks; raise --target):');
    for (const r of lowConfidence) console.log(`  ${r.name}`);
  }

  if (!wantInterpreter || !wantCompiled) return;

  console.log('');
  console.log('=== Compiler tier by workload class ===');
  console.log('');
  console.log('Reported per class and never blended. There is no average Scheme program to');
  console.log('weight these against, and a single number is what hid the overfitting before.');
  console.log('');

  const byClass = new Map();
  for (const row of rows) {
    if (row.speedup === null) continue;
    if (!byClass.has(row.workload)) byClass.set(row.workload, []);
    byClass.get(row.workload).push(row);
  }

  const summaries = [];
  for (const [workload, group] of byClass) {
    const mean = geometricMean(group.map((r) => r.speedup));
    const sorted = [...group].sort((a, b) => a.speedup - b.speedup);
    summaries.push({ workload, mean, group, worst: sorted[0], best: sorted[sorted.length - 1] });
  }
  summaries.sort((a, b) => a.mean - b.mean);

  for (const s of summaries) {
    console.log(`  ${s.workload.padEnd(13)} ${s.mean.toFixed(2)}x`.padEnd(24)
      + ` over ${String(s.group.length).padStart(2)} programs`
      + `   (${s.worst.name} ${s.worst.speedup.toFixed(2)}x`
      + ` .. ${s.best.name} ${s.best.speedup.toFixed(2)}x)`);
    console.log(`  ${''.padEnd(13)} ${WORKLOADS[s.workload]}`);
  }

  if (summaries.length > 0) {
    console.log('');
    console.log(`Worst class: ${summaries[0].workload} at ${summaries[0].mean.toFixed(2)}x. `
      + `Best: ${summaries[summaries.length - 1].workload} `
      + `at ${summaries[summaries.length - 1].mean.toFixed(2)}x.`);
    console.log('Quote the worst. An optimization is worth shipping when it improves at least');
    console.log('one class and regresses none -- a rule that needs no weighting at all.');
  }

  console.log('');
  console.log('--- JSON Results ---');
  console.log(JSON.stringify({
    profile: PROFILE,
    target: TARGET,
    rows: rows.map((r) => ({
      name: r.name,
      workload: r.workload,
      interpretedSeconds: r.interpreted && r.interpreted.seconds,
      compiledSeconds: r.compiled && r.compiled.seconds,
      speedup: r.speedup,
      compiled: r.compiled && r.compiled.compiled,
      definitions: r.compiled && r.compiled.definitions
    })),
    byClass: summaries.map((s) => ({ workload: s.workload, geometricMean: s.mean, programs: s.group.length }))
  }, null, 2));
}

main();
