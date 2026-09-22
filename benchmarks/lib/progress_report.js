/**
 * Renders the performance progress report from the recorded snapshot history.
 *
 * Everything in `docs/performance_progress.md` comes from here, so the document
 * cannot drift from the measurements. Comparisons are only drawn between
 * measurements taken at the same benchmark size; where a size changed between
 * stages the cell is marked rather than silently compared, since comparing a
 * 25-deep `fib` against a 30-deep one would manufacture a speedup out of
 * nothing.
 */

import { BENCHMARKS } from '../programs/manifest.js';

/**
 * Formats a millisecond timing.
 * @param {number|null} ms - The timing.
 * @returns {string} A padded display string.
 */
function ms(value) {
  if (value === null || value === undefined) return '—';
  if (value >= 1000) return `${(value / 1000).toFixed(2)} s`;
  if (value >= 10) return `${value.toFixed(0)} ms`;
  return `${value.toFixed(1)} ms`;
}

/**
 * Formats a speedup factor.
 * @param {number|null} factor - Baseline divided by current.
 * @returns {string} A display string.
 */
function speedup(factor) {
  if (factor === null || factor === undefined || !isFinite(factor)) return '—';
  if (factor >= 100) return `${factor.toFixed(0)}x`;
  if (factor >= 10) return `${factor.toFixed(1)}x`;
  return `${factor.toFixed(2)}x`;
}

/**
 * Builds a markdown table.
 * @param {string[]} headers - Column headers.
 * @param {Array<string[]>} rows - Row cells.
 * @returns {string} The rendered table.
 */
function table(headers, rows) {
  const lines = [];
  lines.push(`| ${headers.join(' | ')} |`);
  lines.push(`|${headers.map(() => '---').join('|')}|`);
  for (const row of rows) lines.push(`| ${row.join(' | ')} |`);
  return lines.join('\n');
}

/**
 * Renders the full progress report.
 * @param {{snapshots: Array<Object>}} history - The recorded history.
 * @returns {string} Markdown source for `docs/performance_progress.md`.
 */
export function renderProgressReport(history) {
  const snapshots = history.snapshots;
  const baseline = snapshots[0];
  const latest = snapshots[snapshots.length - 1];
  const out = [];

  out.push('# Performance progress');
  out.push('');
  out.push('> **This file is generated.** Do not edit it by hand — run');
  out.push('> `npm run benchmark:record -- --stage "<name>" --note "<what changed>"`,');
  out.push('> which appends to `benchmarks/history.json` and rewrites this document.');
  out.push('');
  out.push('Tracks the effect of each stage of the compiler effort described in');
  out.push('[compiler_findings.md](compiler_findings.md). Methodology and the full Stage 0');
  out.push('analysis are in [performance_baseline.md](performance_baseline.md).');
  out.push('');

  // --- Snapshot index ------------------------------------------------------
  out.push('## Snapshots');
  out.push('');
  out.push(table(
    ['Stage', 'Recorded', 'Commit', 'Platform', 'What changed'],
    snapshots.map(s => [
      `**${s.stage}**`,
      s.timestamp.slice(0, 10),
      s.commit ? `\`${s.commit}\`` : '—',
      `${s.platform}, Node ${s.nodeVersion}`,
      s.note || '—'
    ])
  ));
  out.push('');

  if (snapshots.length === 1) {
    out.push('Only one snapshot has been recorded so far, so there is nothing to compare against');
    out.push('yet. The tables below become more useful as stages land.');
    out.push('');
  }

  // --- Timings -------------------------------------------------------------
  out.push('## Wall-clock timings');
  out.push('');
  out.push('Lower is better. Sizes are held fixed across stages so the numbers stay comparable;');
  out.push('a cell reading `size changed` means that benchmark was re-sized and cannot be');
  out.push('compared to the baseline.');
  out.push('');

  const timingHeaders = ['Benchmark', 'size', ...snapshots.map(s => s.stage)];
  if (snapshots.length > 1) timingHeaders.push('vs baseline');

  const timingRows = BENCHMARKS.map(bench => {
    const base = baseline.timings[bench.name];
    const cells = snapshots.map(s => {
      const entry = s.timings[bench.name];
      if (!entry) return '—';
      if (base && entry.size !== base.size) return `${ms(entry.ms)} (size ${entry.size})`;
      return ms(entry.ms);
    });
    const row = [`\`${bench.name}\``, base ? String(base.size) : '—', ...cells];
    if (snapshots.length > 1) {
      const last = latest.timings[bench.name];
      const comparable = base && last && base.size === last.size && base.ms && last.ms;
      row.push(comparable ? `**${speedup(base.ms / last.ms)}**` : 'size changed');
    }
    return row;
  });
  out.push(table(timingHeaders, timingRows));
  out.push('');

  if (snapshots.length > 1) {
    const comparablePairs = BENCHMARKS
      .map(b => [baseline.timings[b.name], latest.timings[b.name]])
      .filter(([a, b]) => a && b && a.size === b.size && a.ms && b.ms);
    if (comparablePairs.length > 0) {
      const total = comparablePairs.reduce((acc, [a, b]) => acc + a.ms / b.ms, 0);
      const geometric = comparablePairs.reduce((acc, [a, b]) => acc * (a.ms / b.ms), 1)
        ** (1 / comparablePairs.length);
      out.push(`**Mean speedup vs ${baseline.stage}:** ${speedup(total / comparablePairs.length)} ` +
        `arithmetic, ${speedup(geometric)} geometric ` +
        `(over ${comparablePairs.length} comparable benchmarks).`);
      out.push('');
      out.push('The geometric mean is the one to quote. An arithmetic mean over ratios is');
      out.push('dominated by whichever benchmark improved most, which flatters the result.');
      out.push('');
    }
  }

  // --- Step counts ---------------------------------------------------------
  out.push('## Evaluator step counts');
  out.push('');
  out.push('Deterministic dispatch counts, measured at smaller sizes than the timings. These are');
  out.push('the honest measure of whether an optimization removed work: they are identical on');
  out.push('every machine and immune to JIT warm-up, so a change here is real in a way that a');
  out.push('change in wall-clock time is not.');
  out.push('');

  const stepHeaders = ['Benchmark', 'size', ...snapshots.map(s => s.stage)];
  if (snapshots.length > 1) stepHeaders.push('reduction');

  const stepRows = BENCHMARKS.map(bench => {
    const base = baseline.stepCounts?.[bench.name];
    const cells = snapshots.map(s => {
      const entry = s.stepCounts?.[bench.name];
      return entry ? entry.totalSteps.toLocaleString() : '—';
    });
    const row = [`\`${bench.name}\``, base ? String(base.size) : '—', ...cells];
    if (snapshots.length > 1) {
      const last = latest.stepCounts?.[bench.name];
      const comparable = base && last && base.size === last.size && base.totalSteps;
      row.push(comparable ? `**${speedup(base.totalSteps / last.totalSteps)}**` : 'size changed');
    }
    return row;
  });
  out.push(table(stepHeaders, stepRows));
  out.push('');

  // --- Cross-implementation ------------------------------------------------
  const withImpls = snapshots.filter(s => s.crossImplementation);
  if (withImpls.length > 0) {
    out.push('## Distance to reference implementations');
    out.push('');
    out.push('How many times slower than each reference, at the same size. Gambit `gsi` is an');
    out.push('*interpreter*, so reaching parity with it means we have stopped being slow for');
    out.push('avoidable reasons. Racket CS is a compiler and is the target for the compiler');
    out.push('stages, not for interpreter work.');
    out.push('');

    for (const reference of [
      { key: 'gambit-gsi', label: 'Gambit `gsi` (interpreter)' },
      { key: 'racket', label: 'Racket CS (compiled)' }
    ]) {
      out.push(`### vs ${reference.label}`);
      out.push('');
      const rows = BENCHMARKS.map(bench => {
        const cells = withImpls.map(s => {
          const row = s.crossImplementation[bench.name];
          const ours = row?.scheme_js_4;
          const theirs = row?.[reference.key];
          if (!ours || !theirs) return '—';
          return `${speedup(ours / theirs)}`;
        });
        return [`\`${bench.name}\``, ...cells];
      });
      out.push(table(['Benchmark', ...withImpls.map(s => s.stage)], rows));
      out.push('');
    }
  }

  out.push('---');
  out.push('');
  out.push('*Regenerate with `npm run benchmark:record -- --stage "<name>"`.*');
  out.push('');

  return out.join('\n');
}
