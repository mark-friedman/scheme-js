/**
 * Records a benchmark snapshot and regenerates the progress report.
 *
 * The point of this script is that the progress document is *generated*, never
 * hand-edited. A table of numbers maintained by hand drifts from reality the
 * first time someone forgets to update it, and a progress report that might be
 * stale is worse than none at all.
 *
 * Snapshots accumulate in `benchmarks/history.json` (append-only) and
 * `docs/performance_progress.md` is rewritten from the whole history each time.
 *
 * Usage:
 *   node benchmarks/record_progress.js --stage "Stage 1" [--note "what changed"]
 *   npm run benchmark:record -- --stage "Stage 1" --note "binary arithmetic"
 *
 *   --skip-implementations   omit the Gambit/Racket comparison (much faster)
 *   --runs N                 repetitions per benchmark (default 5)
 *   --regenerate             rewrite the report from the recorded history
 *                            without measuring anything
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { execFileSync } from 'child_process';
import { fileURLToPath } from 'url';

import { BENCHMARKS, sizeFor } from './programs/manifest.js';
import { runBenchmark } from './lib/harness.js';
import { countBenchmark, SWEEP_SIZES } from './lib/step_counts.js';
import { renderProgressReport } from './lib/progress_report.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PROJECT_ROOT = path.join(__dirname, '..');
const HISTORY_PATH = path.join(PROJECT_ROOT, 'benchmarks', 'history.json');
const REPORT_PATH = path.join(PROJECT_ROOT, 'docs', 'performance_progress.md');

/**
 * Reads the current git commit, if available. Recorded with each snapshot so a
 * measurement can be traced back to the code that produced it.
 * @returns {string|null} The short commit hash, or null outside a git checkout.
 */
function currentCommit() {
  try {
    return execFileSync('git', ['rev-parse', '--short', 'HEAD'], {
      cwd: PROJECT_ROOT, encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore']
    }).trim();
  } catch {
    return null;
  }
}

/**
 * Parses command-line options.
 * @returns {{stage: string, note: string, runs: number, skipImplementations: boolean}}
 */
function parseArgs() {
  const args = process.argv.slice(2);
  const valueOf = (flag, fallback) => {
    const i = args.indexOf(flag);
    return i >= 0 ? args[i + 1] : fallback;
  };
  const stage = valueOf('--stage', null);
  if (!stage) {
    console.error('A stage label is required, e.g. --stage "Stage 1"');
    process.exit(1);
  }
  return {
    stage,
    note: valueOf('--note', ''),
    runs: parseInt(valueOf('--runs', '5'), 10),
    skipImplementations: args.includes('--skip-implementations')
  };
}

/**
 * Runs the external implementations for comparison, via the existing script so
 * the two paths cannot disagree about how the numbers are produced.
 * @param {number} runs - Repetitions per benchmark.
 * @returns {Object} Cross-implementation results keyed by benchmark name.
 */
function measureImplementations(runs) {
  const out = execFileSync('node',
    [path.join(PROJECT_ROOT, 'benchmarks', 'compare_implementations.js'), '--runs', String(runs)],
    { cwd: PROJECT_ROOT, encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 });
  const marker = '--- JSON Results ---';
  const parsed = JSON.parse(out.slice(out.lastIndexOf(marker) + marker.length).trim());
  return parsed.results;
}

function main() {
  // Regenerating is separate from recording: the report is derived from the
  // history, so a label or wording fix should not require re-measuring and
  // should not perturb numbers that were taken on a quieter machine.
  if (process.argv.includes('--regenerate')) {
    const history = JSON.parse(fs.readFileSync(HISTORY_PATH, 'utf8'));
    fs.writeFileSync(REPORT_PATH, renderProgressReport(history));
    console.log(`regenerated ${path.relative(PROJECT_ROOT, REPORT_PATH)} ` +
      `from ${history.snapshots.length} snapshot(s)`);
    return;
  }

  const { stage, note, runs, skipImplementations } = parseArgs();

  console.log(`Recording snapshot for "${stage}" (runs: ${runs})`);

  const timings = {};
  const stepCounts = {};
  let failures = 0;

  for (const bench of BENCHMARKS) {
    const size = sizeFor(bench, 'quick');
    process.stdout.write(`  ${bench.name} `);

    const { median, result, error } = runBenchmark(bench, size, runs);
    if (error) {
      console.log(`ERROR: ${error}`);
      timings[bench.name] = { size, ms: null, result: null, error };
      failures++;
      continue;
    }
    if (bench.expected !== null && result !== bench.expected) {
      console.log(`WRONG RESULT: expected ${bench.expected}, got ${result}`);
      failures++;
    }
    timings[bench.name] = { size, ms: Number(median.toFixed(3)), result, error: null };
    process.stdout.write('timed ');

    const stepSize = SWEEP_SIZES[bench.name] ?? bench.quick;
    const counted = countBenchmark(bench, stepSize);
    stepCounts[bench.name] = {
      size: stepSize,
      totalSteps: counted.stats.totalSteps,
      maxStackDepth: counted.stats.maxStackDepth
    };
    console.log(`counted (${counted.stats.totalSteps.toLocaleString()} steps)`);
  }

  if (failures > 0) {
    console.error(`\n${failures} benchmark(s) failed. Snapshot NOT recorded --`);
    console.error('a progress report built from wrong results would be worse than none.');
    process.exit(1);
  }

  let crossImplementation = null;
  if (!skipImplementations) {
    console.log('  comparing against Gambit and Racket...');
    crossImplementation = measureImplementations(Math.min(runs, 3));
  }

  const snapshot = {
    stage,
    note,
    timestamp: new Date().toISOString(),
    commit: currentCommit(),
    nodeVersion: process.version,
    platform: `${os.platform()}/${os.arch()}`,
    timings,
    stepCounts,
    crossImplementation
  };

  const history = fs.existsSync(HISTORY_PATH)
    ? JSON.parse(fs.readFileSync(HISTORY_PATH, 'utf8'))
    : { snapshots: [] };

  // Re-recording a stage replaces it rather than accumulating duplicates, so
  // a stage can be re-measured after a fix without polluting the history.
  const existing = history.snapshots.findIndex(s => s.stage === stage);
  if (existing >= 0) {
    console.log(`  replacing existing snapshot for "${stage}"`);
    history.snapshots[existing] = snapshot;
  } else {
    history.snapshots.push(snapshot);
  }

  fs.writeFileSync(HISTORY_PATH, JSON.stringify(history, null, 2) + '\n');
  fs.writeFileSync(REPORT_PATH, renderProgressReport(history));

  console.log(`\nwrote ${path.relative(PROJECT_ROOT, HISTORY_PATH)}`);
  console.log(`wrote ${path.relative(PROJECT_ROOT, REPORT_PATH)}`);
}

main();
