/**
 * Runs the canonical R7RS suite under every available Scheme implementation.
 *
 * ## Why this exists
 *
 * Two reasons, and the second is the more valuable one.
 *
 * The obvious reason is standing: how far behind Gambit and Racket are we, on
 * programs nobody here chose. The less obvious reason is that cross-
 * implementation agreement is a **validity check on the benchmark itself**. If
 * a program is relatively expensive for us *and* relatively expensive for
 * Gambit and Racket, it is measuring something intrinsic to the program. If it
 * is expensive only for us, it is measuring our implementation -- also useful,
 * but a different claim, and one a single implementation's numbers cannot tell
 * apart. That distinction is exactly what the eight microbenchmarks in
 * `benchmarks/programs/` got wrong: they were accurate about our standing and
 * inaccurate about which optimizations help.
 *
 * These programs make the check sharper than the project's own Scheme could,
 * because `ecraven/r7rs-benchmarks` publishes results for more than twenty
 * implementations. A Gambit or Racket number that disagrees with the published
 * one is evidence about *this harness*, before it is evidence about anything
 * else.
 *
 * ## What is compared
 *
 * Both of our tiers, as `run_r7rs.js` sets them up -- see
 * `docs/r7rs_benchmark_results.md` for exactly what each one compiles --
 * against three references that bracket them:
 *
 * - Gambit `gsi`, an interpreter: the fair comparison for our interpreter.
 * - Gambit `gsc`, compiling to C: an ahead-of-time native Scheme compiler.
 * - Gambit `gsc -target js`, compiling to JavaScript and run on the same Node
 *   as us: the closest thing to our compiler, same engine, same target.
 * - Racket CS, compiling to machine code through Chez Scheme.
 *
 * Our figures can come from an earlier `run_r7rs.js` run (`--ours`) instead of
 * being measured again. Measuring both our tiers is most of this script's
 * running time, and nothing about the references changes when our code does.
 *
 * ## Measurement
 *
 * Every implementation is timed inside the Scheme program by
 * `run-r7rs-benchmark`, using R7RS `current-jiffy`, on the same source. The
 * reference implementations run under upstream's own preludes, unmodified, so
 * `gsc` compiles with upstream's `(declare (standard-bindings)
 * (extended-bindings) (block))` and stays in safe mode. A compiler's compile
 * time is never charged: `gsc` builds each program once, before any timed run,
 * and Racket compiles on load, before the program starts its clock.
 *
 * Repetition counts are **calibrated per implementation** and results reported
 * per iteration. This matters more than it sounds: Racket's clock ticks a
 * thousand times a second, so a count giving this interpreter a second of work
 * gives Racket one or two ticks, and a two-tick measurement is not a
 * measurement. Three earlier comparisons in this project were wrong for
 * precisely this family of reasons -- a clock too coarse for what was being
 * timed, a startup cost larger than the work, and one side charged for
 * compilation the others did once -- so the clock is probed and inadequate
 * resolution is labelled rather than assumed away.
 *
 * Racket needs its R7RS language: `raco pkg install r7rs`. `gsc` needs the C
 * compiler Gambit was configured with; `benchmarks/r7rs/README.md` says what
 * to do when that compiler is not the one installed.
 *
 * Usage: node benchmarks/compare_r7rs.js [--profile default|full]
 *          [--target SECONDS] [--only name,name] [--tier interpreter|compiled|both]
 *          [--ours RUN_R7RS_OUTPUT]
 */

import { execFileSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';

import { fileURLToPath } from 'url';

import { selectBenchmarks } from './r7rs/manifest.js';
import { R7RS_DIR, buildInput, parseCsvLine, calibrate } from './lib/r7rs_harness.js';
import { extractJsonResults, oursFromR7rsResults, summarizeByClass } from './lib/r7rs_compare.js';

const WORKER = path.join(path.dirname(fileURLToPath(import.meta.url)), 'lib', 'r7rs_worker.js');

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};

const PROFILE = valueOf('--profile', 'default');
const TARGET = parseFloat(valueOf('--target', '1.0'));
const ONLY = valueOf('--only', null);
const TIER = valueOf('--tier', 'both');
const OURS_FROM = valueOf('--ours', null);

/** Our tiers to report, in table order. */
const TIERS = TIER === 'both' ? ['interpreter', 'compiled'] : [TIER];

/** Wall-clock budget for one measurement of ours, in seconds. */
const BUDGET = parseFloat(valueOf('--budget', '180'));

/**
 * Options for every `gsc` build.
 *
 * Gambit refuses to load compiled code whose layout of the runtime's own
 * structures differs from the runtime's, and a C compiler newer than the one
 * that built the runtime can produce such code: one that supports
 * `__attribute__((musttail))` (GCC 15 and later) switches how compiled code
 * returns to the runtime, and the result is "Module is incompatible", or a
 * standalone executable that exits with status 71 and prints nothing. This
 * define is Gambit's own switch for code built by a different compiler than the
 * runtime; it keeps the return path the runtime expects and changes nothing
 * when the two compilers are the same.
 * @type {string[]}
 */
const GSC_OPTIONS = ['-cc-options', '-D___SUPPORT_MULTIPLE_C_COMPILERS'];

/**
 * Reference implementations, each with the prelude upstream uses for it.
 *
 * The preludes are vendored unmodified in `r7rs/src/`. Substituting our own
 * would mean measuring a different program than the published results measure.
 * `gsi` and `gsc` share Gambit's prelude, as upstream's do; its declarations
 * mean nothing to the interpreter.
 * @type {Array<Object>}
 */
const REFERENCES = [
  {
    name: 'gambit-gsi',
    label: 'Gambit gsi (interpreter)',
    command: 'gsi',
    prelude: 'GambitC-prelude.scm',
    extension: '.scm'
  },
  {
    name: 'gambit-gsc',
    label: 'Gambit gsc (compiled to C)',
    command: 'gsc',
    prelude: 'GambitC-prelude.scm',
    extension: '.scm',
    target: 'C'
  },
  {
    name: 'gambit-js',
    label: 'Gambit gsc -target js (on Node)',
    command: 'gsc',
    prelude: 'GambitC-prelude.scm',
    extension: '.scm',
    target: 'js'
  },
  {
    name: 'racket',
    label: 'Racket CS (compiled)',
    command: 'racket',
    prelude: 'Racket-prelude.scm',
    extension: '.rkt'
  }
];

/**
 * Checks whether a command exists on PATH.
 * @param {string} command - Executable name.
 * @returns {boolean} True if found.
 */
function isAvailable(command) {
  try { execFileSync('which', [command], { stdio: 'pipe' }); return true; } catch { return false; }
}

/**
 * Builds a standalone program with `gsc`.
 *
 * The C target produces a native executable. The JavaScript target produces
 * one self-contained `.js` file, runtime included, run by the same Node -- and
 * so the same V8 -- that runs scheme-js-4, which makes it the reference closest
 * to what our compiler does.
 *
 * @param {string} source - Path to the Scheme source.
 * @param {string} target - `'C'` or `'js'`.
 * @returns {{command: (string|null), args: string[], error: (string|null)}}
 *   How to run the result, or the first line the build reported if it failed.
 */
function buildWithGsc(source, target) {
  const js = target === 'js';
  const output = source.replace(/\.scm$/, js ? '.js' : '.exe');
  const options = js ? ['-target', 'js'] : GSC_OPTIONS;
  try {
    execFileSync('gsc', [...options, '-exe', '-o', output, source],
      { stdio: 'pipe', encoding: 'utf8', timeout: 600000 });
    return js
      ? { command: process.execPath, args: [output], error: null }
      : { command: output, args: [], error: null };
  } catch (e) {
    const detail = `${e.stderr || ''}${e.stdout || ''}`.split('\n').find((l) => l.trim())
      || e.message || 'failed';
    return { command: null, args: [], error: detail.slice(0, 120) };
  }
}

/** Programs `gsc` has already built, by source path, so each is built once. */
const builds = new Map();

/**
 * Runs a Scheme source file under one reference implementation.
 *
 * A compiling reference builds the file the first time it is asked to run it
 * and reuses the executable after, so calibration rounds -- which change only
 * the repetition count, and that arrives on standard input -- never recompile.
 *
 * @param {Object} ref - A reference implementation entry.
 * @param {string} file - Path to the source.
 * @param {Object} [options] - Options for `execFileSync`: `cwd`, `input`, `timeout`.
 * @returns {string} What the program printed.
 * @throws {Error} If the build or the run failed.
 */
function execute(ref, file, options = {}) {
  const spawn = { stdio: ['pipe', 'pipe', 'pipe'], encoding: 'utf8', timeout: 600000, ...options };
  if (!ref.target) return execFileSync(ref.command, [file], spawn);
  if (!builds.has(file)) builds.set(file, buildWithGsc(file, ref.target));
  const build = builds.get(file);
  if (build.command === null) throw new Error(`gsc build failed: ${build.error}`);
  return execFileSync(build.command, build.args, spawn);
}

/**
 * Asks an implementation how fine its clock is.
 *
 * Reported with every result. A time is only as meaningful as the clock behind
 * it, and two implementations here differ by three orders of magnitude. For
 * `gsc` this is also the check that it can build an executable here at all.
 *
 * @param {Object} ref - A reference implementation entry.
 * @param {string} dir - Scratch directory.
 * @returns {{clock: (number|null), error: (string|null)}} Jiffies per second,
 *   or why it could not be asked.
 */
function probeClock(ref, dir) {
  const prelude = fs.readFileSync(path.join(R7RS_DIR, 'src', ref.prelude), 'utf8');
  const file = path.join(dir, `clock-${ref.name}${ref.extension}`);
  // The newline matters: Gambit's JavaScript runtime drops an unterminated last
  // line of output at exit.
  fs.writeFileSync(file,
    `${prelude}\n(import (scheme time) (scheme write))\n(write (jiffies-per-second))\n(newline)\n`);
  try {
    const value = parseInt(execute(ref, file, { timeout: 120000 }).trim(), 10);
    return { clock: Number.isFinite(value) ? value : null, error: null };
  } catch (e) {
    return { clock: null, error: (e.message || 'failed').split('\n')[0].slice(0, 120) };
  }
}

/**
 * Runs one benchmark under one reference implementation.
 * @param {Object} ref - A reference implementation entry.
 * @param {Object} bench - Manifest entry.
 * @param {number} count - Repetitions.
 * @param {string} dir - Scratch directory.
 * @returns {{seconds: (number|null), incorrect: boolean, error: (string|null)}} Outcome.
 */
function runReference(ref, bench, count, dir) {
  // The source does not depend on the count, which arrives on standard input,
  // so it is written once per implementation and program.
  const file = path.join(dir, `${bench.name}-${ref.name}${ref.extension}`);
  if (!fs.existsSync(file)) {
    const read = (name) => fs.readFileSync(path.join(R7RS_DIR, 'src', name), 'utf8');
    // The reference implementations get the program with its `(import ...)`
    // intact -- unlike us, they support it, and removing it would be a change
    // to the program under measurement.
    fs.writeFileSync(file, [read(ref.prelude), read(`${bench.name}.scm`), read('common.scm'),
      read('common-postlude.scm')].join('\n'));
  }
  try {
    const out = execute(ref, file, {
      cwd: R7RS_DIR, input: buildInput(bench.name, bench.params, count)
    });
    const parsed = parseCsvLine(out);
    return { seconds: parsed.seconds, incorrect: parsed.incorrect, error: null };
  } catch (e) {
    return { seconds: null, incorrect: false, error: (e.message || 'failed').slice(0, 80) };
  }
}

/**
 * Measures one benchmark under one reference, calibrating the count first.
 * @param {Object} ref - A reference implementation entry.
 * @param {Object} bench - Manifest entry.
 * @param {string} dir - Scratch directory.
 * @returns {{seconds: (number|null), count: number, error: (string|null),
 *   incorrect: boolean}} Per-iteration seconds and supporting detail.
 */
function measureReference(ref, bench, dir) {
  const c = calibrate((count) => runReference(ref, bench, count, dir), TARGET);
  return {
    seconds: c.seconds,
    count: c.count,
    error: c.unmeasurable ? 'below clock resolution' : c.result.error,
    incorrect: c.result.incorrect
  };
}

/**
 * Runs one measurement of ours in a child process, under a wall-clock budget.
 *
 * The reference implementations already get a budget from `execFileSync`'s
 * timeout; ours needs the same treatment for the same reason, and a benchmark
 * that hangs should be reported as hanging rather than stalling the run.
 *
 * @param {Object} bench - Manifest entry.
 * @param {number} count - Repetitions.
 * @param {boolean} useCompiler - Whether to run under the compiler tier.
 * @returns {{seconds: (number|null), incorrect: boolean, error: (string|null)}} Outcome.
 */
function runOurs(bench, count, useCompiler) {
  const request = JSON.stringify({ name: bench.name, params: bench.params, count, useCompiler });
  try {
    return JSON.parse(execFileSync(process.execPath, [WORKER, request], {
      stdio: ['pipe', 'pipe', 'pipe'], encoding: 'utf8', timeout: BUDGET * 1000
    }));
  } catch (e) {
    const killed = e.killed || e.signal === 'SIGTERM';
    return {
      seconds: null, incorrect: false,
      error: killed ? `exceeded the ${BUDGET}s budget` : (e.message || 'failed').slice(0, 80)
    };
  }
}

/**
 * Measures one benchmark under one of our tiers, calibrating the count.
 * @param {Object} bench - Manifest entry.
 * @param {string} tier - `'interpreter'` or `'compiled'`.
 * @returns {number|null} Per-iteration seconds, or null if there is no time.
 */
function measureOurs(bench, tier) {
  const c = calibrate((count) => runOurs(bench, count, tier === 'compiled'), TARGET);
  return c.result.incorrect ? null : c.seconds;
}

/**
 * Formats seconds for a table cell.
 * @param {number|null} seconds - A per-iteration time.
 * @returns {string} A human-readable duration.
 */
function formatSeconds(seconds) {
  if (seconds === null || seconds === undefined) return '--';
  if (seconds >= 1) return `${seconds.toFixed(2)} s`;
  if (seconds >= 1e-3) return `${(seconds * 1e3).toFixed(1)} ms`;
  return `${(seconds * 1e6).toFixed(1)} us`;
}

/**
 * Formats a ratio of our time to a reference's as slower or faster.
 * @param {number} ratio - Ours divided by the reference.
 * @returns {string} For example `12.3x slower` or `4.10x faster`.
 */
function formatRatio(ratio) {
  return ratio >= 1 ? `${ratio.toFixed(2)}x slower` : `${(1 / ratio).toFixed(2)}x faster`;
}

/**
 * Where our figures come from: a previous `run_r7rs.js` run, or a fresh
 * measurement per program.
 * @returns {function(Object): Object<string, (number|null)>} Seconds per
 *   iteration for each requested tier of one manifest entry.
 */
function oursSource() {
  if (!OURS_FROM) {
    return (bench) => Object.fromEntries(TIERS.map((tier) => [tier, measureOurs(bench, tier)]));
  }
  const results = extractJsonResults(fs.readFileSync(OURS_FROM, 'utf8'));
  if (results.profile !== PROFILE || results.target !== TARGET) {
    console.log(`  note: ${OURS_FROM} was run with profile '${results.profile}', target `
      + `${results.target}; references use '${PROFILE}', ${TARGET}`);
  }
  const ours = oursFromR7rsResults(results);
  return (bench) => {
    const row = ours.get(bench.name) || {};
    return Object.fromEntries(TIERS.map((tier) => [tier, row[tier] ?? null]));
  };
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

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-r7rs-compare-'));

  console.log(`Canonical R7RS suite across implementations -- profile '${PROFILE}', `
    + `${benchmarks.length} programs`);
  console.log('');
  console.log(`  scheme-js-4 (${TIERS.join(', ')}): clock 1,000 jiffies/s`
    + (OURS_FROM ? `, figures from ${OURS_FROM}` : ''));
  const available = [];
  for (const ref of REFERENCES) {
    if (!isAvailable(ref.command)) {
      console.log(`  ${ref.label}: NOT AVAILABLE (${ref.command} not on PATH)`);
      continue;
    }
    const probe = probeClock(ref, dir);
    if (ref.target && probe.error) {
      console.log(`  ${ref.label}: NOT AVAILABLE (${probe.error})`);
      continue;
    }
    available.push(ref);
    console.log(`  ${ref.label}: clock ${probe.clock ? probe.clock.toLocaleString() : 'unknown'} jiffies/s`);
  }
  console.log('');

  const oursFor = oursSource();
  const columns = [...TIERS.map((t) => `ours ${t}`), ...available.map((r) => r.name)];
  console.log(`  ${'program'.padEnd(12)} ${'class'.padEnd(13)}`
    + columns.map((c) => c.padStart(18)).join(''));

  const rows = [];
  for (const bench of benchmarks) {
    const ours = oursFor(bench);
    const refs = {};
    for (const ref of available) refs[ref.name] = measureReference(ref, bench, dir);
    rows.push({ name: bench.name, workload: bench.workload, ours, refs });

    const problems = available
      .filter((ref) => refs[ref.name].error || refs[ref.name].incorrect)
      .map((ref) => `${ref.name}: ${refs[ref.name].incorrect ? 'WRONG ANSWER' : refs[ref.name].error}`);
    console.log(`  ${bench.name.padEnd(12)} ${bench.workload.padEnd(13)}`
      + TIERS.map((t) => formatSeconds(ours[t]).padStart(18)).join('')
      + available.map((r) => formatSeconds(refs[r.name].seconds).padStart(18)).join('')
      + (problems.length ? '  ' + problems.join('; ') : ''));
  }

  const summary = summarizeByClass(
    rows.map((r) => ({
      ...r,
      refs: Object.fromEntries(Object.entries(r.refs).map(([k, v]) => [k, v.seconds]))
    })),
    TIERS, available.map((r) => r.name));

  console.log('');
  console.log('=== Standing by workload class ===');
  console.log('');
  console.log('Geometric mean over each class of our time divided by the reference\'s.');
  console.log('Per class, never blended: there is no average Scheme program to weight these');
  console.log('against, and one number is what hid the overfitting of the earlier suite.');
  for (const tier of TIERS) {
    for (const ref of available) {
      console.log('');
      console.log(`  scheme-js-4 ${tier} vs ${ref.label}:`);
      for (const s of summary.filter((x) => x.tier === tier && x.reference === ref.name)) {
        console.log(`    ${s.workload.padEnd(13)} ${formatRatio(s.geometricMean).padStart(14)}`
          + `   (${s.min.toFixed(2)}x to ${s.max.toFixed(2)}x, ${s.programs} programs)`);
      }
    }
  }
  console.log('');

  fs.rmSync(dir, { recursive: true, force: true });

  console.log('--- JSON Results ---');
  console.log(JSON.stringify({
    profile: PROFILE,
    target: TARGET,
    oursFrom: OURS_FROM,
    rows: rows.map((r) => ({
      name: r.name,
      workload: r.workload,
      ours: r.ours,
      references: Object.fromEntries(Object.entries(r.refs).map(([k, v]) =>
        [k, { seconds: v.seconds, count: v.count, error: v.error, incorrect: v.incorrect }]))
    })),
    byClass: summary
  }, null, 2));
}

main();
