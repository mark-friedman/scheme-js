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
 * ## Measurement
 *
 * Every implementation is timed inside the Scheme program by
 * `run-r7rs-benchmark`, using R7RS `current-jiffy`, on the same source. The
 * reference implementations run under upstream's own preludes, unmodified.
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
 * Racket needs its R7RS language: `raco pkg install r7rs`.
 *
 * Both our tiers are measured: the interpreter, and the compiled tier as a
 * user would run it -- the standard library compiled and the program's
 * definitions compiled. Gambit compiled to JavaScript (`gsc -target js`) is the
 * closest reference there is: another Scheme compiled to JavaScript, run by the
 * same V8. Gambit compiled to C needs a C toolchain, and is measured when one
 * is there.
 *
 * Usage: node benchmarks/compare_r7rs.js [--profile default|full]
 *                                        [--target SECONDS] [--only name,name]
 *                                        [--tiers interpreter,compiled]
 */

import { execFileSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';

import { fileURLToPath } from 'url';

import { selectBenchmarks } from './r7rs/manifest.js';
import { PLAIN_JS_PROGRAMS } from './r7rs/plain_js_kernels.js';
import { R7RS_DIR, buildInput, parseCsvLine, calibrate } from './lib/r7rs_harness.js';

const WORKER = path.join(path.dirname(fileURLToPath(import.meta.url)), 'lib', 'r7rs_worker.js');

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};

const PROFILE = valueOf('--profile', 'default');
const TARGET = parseFloat(valueOf('--target', '1.0'));
const ONLY = valueOf('--only', null);
const TIERS = valueOf('--tiers', 'interpreter,compiled').split(',');

/** Wall-clock budget for one measurement, in seconds. */
const BUDGET = parseFloat(valueOf('--budget', '180'));

/**
 * Reference implementations, each with the prelude upstream uses for it.
 *
 * The preludes are vendored unmodified in `r7rs/src/`. Substituting our own
 * would mean measuring a different program than the published results measure.
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
    name: 'racket',
    label: 'Racket CS (compiled)',
    command: 'racket',
    prelude: 'Racket-prelude.scm',
    extension: '.rkt'
  },
  {
    // Built once per program, then run by Node like our own code.
    name: 'gambit-js',
    label: 'Gambit compiled to JavaScript',
    command: 'gsc',
    prelude: 'GambitC-prelude.scm',
    extension: '.scm',
    build: (file) => {
      const out = file.replace(/\.scm$/, '.js');
      execFileSync('gsc', ['-target', 'js', '-exe', '-o', out, file], { stdio: 'pipe', timeout: 600000 });
      return [process.execPath, [out]];
    }
  },
  {
    name: 'gambit-c',
    label: 'Gambit compiled to C',
    command: 'gsc',
    prelude: 'GambitC-prelude.scm',
    extension: '.scm',
    build: (file) => {
      const out = file.replace(/\.scm$/, '');
      execFileSync('gsc', ['-exe', '-o', out, file], { stdio: 'pipe', timeout: 600000 });
      return [out, []];
    },
    // `gsc` is there without a C toolchain, and then cannot build anything.
    // On macOS, asking the stub `cc` would offer to install one, so the
    // developer directory is looked for instead.
    usable: () => {
      try {
        if (process.platform === 'darwin') {
          const developer = execFileSync('xcode-select', ['-p'], { stdio: 'pipe', encoding: 'utf8' }).trim();
          return fs.existsSync(path.join(developer, 'usr', 'bin', 'clang'));
        }
        execFileSync('cc', ['--version'], { stdio: 'pipe' });
        return true;
      } catch {
        return false;
      }
    }
  },
  {
    // What a JavaScript programmer would write for the same work, for the few
    // programs where that means something: `r7rs/plain_js_kernels.js`.
    name: 'plain-js',
    label: 'plain JavaScript',
    command: 'node',
    prelude: null,
    extension: '.js',
    supports: (bench) => PLAIN_JS_PROGRAMS.includes(bench.name),
    build: (file, bench) => [process.execPath, [
      path.join(path.dirname(fileURLToPath(import.meta.url)), 'r7rs', 'plain_js_kernels.js'), bench.name]]
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
 * Asks an implementation how fine its clock is.
 *
 * Reported with every result. A time is only as meaningful as the clock behind
 * it, and two implementations here differ by three orders of magnitude.
 *
 * @param {Object} ref - A reference implementation entry.
 * @param {string} dir - Scratch directory.
 * @returns {number|null} Jiffies per second, or null if it could not be asked.
 */
function probeClock(ref, dir) {
  const prelude = fs.readFileSync(path.join(R7RS_DIR, 'src', ref.prelude), 'utf8');
  const file = path.join(dir, `clock${ref.extension}`);
  fs.writeFileSync(file, `${prelude}\n(import (scheme time) (scheme write))\n(write (jiffies-per-second))\n`);
  try {
    const out = execFileSync(ref.command, [file], { stdio: 'pipe', encoding: 'utf8', timeout: 60000 });
    const value = parseInt(out.trim(), 10);
    return Number.isFinite(value) ? value : null;
  } catch {
    return null;
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
  const file = path.join(dir, `${ref.name}-${bench.name}${ref.extension}`);
  const inputFile = path.join(dir, `${bench.name}.input`);
  fs.writeFileSync(inputFile, buildInput(bench.name, bench.params, count));

  try {
    const [command, commandArgs] = buildReference(ref, bench, file);
    const out = execFileSync(command, commandArgs, {
      stdio: ['pipe', 'pipe', 'pipe'],
      encoding: 'utf8',
      timeout: 600000,
      cwd: R7RS_DIR,
      input: fs.readFileSync(inputFile, 'utf8')
    });
    const parsed = parseCsvLine(out);
    return { seconds: parsed.seconds, incorrect: parsed.incorrect, error: null };
  } catch (e) {
    return { seconds: null, incorrect: false, error: (e.message || 'failed').slice(0, 80) };
  }
}

/**
 * Writes one program for a reference implementation, and builds it if the
 * implementation compiles ahead of time -- once per program, since the program
 * is the same at every repetition count and only its input changes.
 * @param {Object} ref - A reference implementation entry.
 * @param {Object} bench - Manifest entry.
 * @param {string} file - Where to write the program.
 * @returns {[string, Array<string>]} The command that runs it, and its arguments.
 */
function buildReference(ref, bench, file) {
  if (built.has(file)) return built.get(file);
  if (ref.prelude === null) {
    const run = ref.build(file, bench);
    built.set(file, run);
    return run;
  }
  const prelude = fs.readFileSync(path.join(R7RS_DIR, 'src', ref.prelude), 'utf8');
  const common = fs.readFileSync(path.join(R7RS_DIR, 'src', 'common.scm'), 'utf8');
  const postlude = fs.readFileSync(path.join(R7RS_DIR, 'src', 'common-postlude.scm'), 'utf8');
  // The reference implementations get the program with its `(import ...)`
  // intact -- unlike us, they support it, and removing it would be a change to
  // the program under measurement.
  const program = fs.readFileSync(path.join(R7RS_DIR, 'src', `${bench.name}.scm`), 'utf8');
  fs.writeFileSync(file, [prelude, program, common, postlude].join('\n'));
  const run = ref.build ? ref.build(file) : [ref.command, [file]];
  built.set(file, run);
  return run;
}

/**
 * Programs already written, and built where their implementation builds, by
 * file.
 * @type {Map<string, [string, Array<string>]>}
 */
const built = new Map();

/**
 * Measures one benchmark under one reference, calibrating the count first.
 * @param {Object} ref - A reference implementation entry.
 * @param {Object} bench - Manifest entry.
 * @param {string} dir - Scratch directory.
 * @returns {{seconds: (number|null), count: number, error: (string|null),
 *   incorrect: boolean}} Per-iteration seconds and supporting detail.
 */
function measureReference(ref, bench, dir) {
  if (ref.supports && !ref.supports(bench)) {
    return { seconds: null, count: 0, error: null, incorrect: false };
  }
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
 * @returns {{seconds: (number|null), incorrect: boolean, error: (string|null)}} Outcome.
 */
function runOurs(bench, count, useCompiler) {
  const request = JSON.stringify({
    name: bench.name, params: bench.params, count, useCompiler
  });
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
 * Measures one benchmark under this implementation, calibrating the count.
 * @param {Object} bench - Manifest entry.
 * @param {string} tier - `interpreter` or `compiled`.
 * @returns {{seconds: (number|null), count: number, error: (string|null),
 *   incorrect: boolean}} Per-iteration seconds and supporting detail.
 */
function measureOurs(bench, tier) {
  const c = calibrate((count) => runOurs(bench, count, tier === 'compiled'), TARGET);
  return {
    seconds: c.seconds,
    count: c.count,
    error: c.unmeasurable ? 'below clock resolution' : c.result.error,
    incorrect: c.result.incorrect
  };
}

/**
 * Geometric mean of a list of ratios.
 * @param {number[]} values - Positive ratios.
 * @returns {number|null} The geometric mean, or null if empty.
 */
function geometricMean(values) {
  if (values.length === 0) return null;
  return Math.exp(values.reduce((acc, v) => acc + Math.log(v), 0) / values.length);
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
  const available = REFERENCES.filter((r) => isAvailable(r.command) && (!r.usable || r.usable()));

  console.log(`Canonical R7RS suite across implementations -- profile '${PROFILE}', `
    + `${benchmarks.length} programs`);
  console.log('');
  console.log(`  scheme-js-4 (${TIERS.join(', ')}): clock 1,000 jiffies/s`);
  for (const ref of REFERENCES) {
    if (!available.includes(ref)) {
      console.log(`  ${ref.label}: NOT AVAILABLE`);
      continue;
    }
    const clock = ref.build ? null : probeClock(ref, dir);
    console.log(`  ${ref.label}${clock ? `: clock ${clock.toLocaleString()} jiffies/s` : ''}`);
  }
  console.log('');
  console.log(`  ${'program'.padEnd(12)} ${'class'.padEnd(13)}`
    + TIERS.map((t) => t.padStart(12)).join('')
    + available.map((r) => r.name.padStart(12)).join('') + '   (ms per iteration)');

  const rows = [];
  for (const bench of benchmarks) {
    const ours = Object.fromEntries(TIERS.map((tier) => [tier, measureOurs(bench, tier)]));
    const refs = {};
    for (const ref of available) refs[ref.name] = measureReference(ref, bench, dir);
    rows.push({ name: bench.name, workload: bench.workload, ours, refs });

    const ms = (r) => (r && r.seconds !== null ? (r.seconds * 1000).toFixed(r.seconds < 0.01 ? 3 : 1) : '--');
    const problems = [...Object.entries(ours), ...Object.entries(refs)]
      .filter(([, r]) => r.error || r.incorrect)
      .map(([k, r]) => `${k}: ${r.incorrect ? 'WRONG ANSWER' : r.error.slice(0, 40)}`);
    console.log(`  ${bench.name.padEnd(12)} ${bench.workload.padEnd(13)}`
      + TIERS.map((t) => ms(ours[t]).padStart(12)).join('')
      + available.map((r) => ms(refs[r.name]).padStart(12)).join('')
      + (problems.length ? '  ' + problems.join('; ') : ''));
  }

  console.log('');
  console.log('=== How far behind, by workload class ===');
  console.log('');
  console.log('Per class, never blended: there is no average Scheme program to weight these');
  console.log('against, and one number is what hid the overfitting of the earlier suite.');
  console.log('Each figure is our time over theirs: above 1, we are slower.');
  console.log('');

  const classes = [...new Set(rows.map((r) => r.workload))];
  for (const tier of TIERS) {
    for (const ref of available) {
      console.log(`  ${tier} vs ${ref.label}:`);
      for (const workload of classes) {
        const ratios = rows
          .filter((r) => r.workload === workload && r.ours[tier].seconds !== null
            && r.refs[ref.name] && r.refs[ref.name].seconds !== null)
          .map((r) => r.ours[tier].seconds / r.refs[ref.name].seconds);
        if (ratios.length === 0) continue;
        const mean = geometricMean(ratios);
        const sorted = [...ratios].sort((a, b) => a - b);
        console.log(`    ${workload.padEnd(13)} ${mean.toFixed(2)}x`.padEnd(30)
          + ` (${sorted[0].toFixed(2)}x to ${sorted[sorted.length - 1].toFixed(2)}x,`
          + ` ${ratios.length} programs)`);
      }
      console.log('');
    }
  }

  fs.rmSync(dir, { recursive: true, force: true });

  console.log('--- JSON Results ---');
  console.log(JSON.stringify({
    profile: PROFILE,
    target: TARGET,
    rows: rows.map((r) => ({
      name: r.name,
      workload: r.workload,
      ours: Object.fromEntries(Object.entries(r.ours).map(([k, v]) =>
        [k, { seconds: v.seconds, count: v.count }])),
      references: Object.fromEntries(Object.entries(r.refs).map(([k, v]) =>
        [k, { seconds: v.seconds, count: v.count }]))
    }))
  }, null, 2));
}

main();
