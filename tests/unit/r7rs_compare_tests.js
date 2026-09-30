/**
 * Unit tests for the arithmetic behind the cross-implementation comparison of
 * the canonical R7RS suite (`benchmarks/compare_r7rs.js`).
 *
 * The comparison reuses scheme-js-4 figures from an earlier `run_r7rs.js` run
 * rather than measuring them again, and reports each of our tiers against each
 * reference implementation, per workload class. Getting a ratio upside down or
 * letting a failed run into a geometric mean would produce a table that looks
 * like a result and is not one, so the arithmetic is tested on its own.
 */

import {
  RESULTS_MARKER,
  extractJsonResults,
  oursFromR7rsResults,
  geometricMean,
  summarizeByClass
} from '../../benchmarks/lib/r7rs_compare.js';
import { assert } from '../harness/helpers.js';

/**
 * Runs the comparison-arithmetic tests.
 * @param {Object} logger - Test logger.
 */
export function runR7rsCompareTests(logger) {
  logger.title('Running R7RS comparison arithmetic tests...');

  // === Reading a run_r7rs.js log ===

  {
    const log = [
      'Canonical R7RS benchmarks -- profile \'default\'',
      '  fib          call           187.8 ms    2.8 ms  66.71x  4/4 compiled',
      '',
      RESULTS_MARKER,
      '{ "profile": "default", "rows": [] }'
    ].join('\n');
    const parsed = extractJsonResults(log);
    assert(logger, 'extractJsonResults reads the JSON after the marker',
      parsed.profile, 'default');
  }

  {
    let message = null;
    try {
      extractJsonResults('fib 187.8 ms\nno results here\n');
    } catch (e) {
      message = e.message;
    }
    assert(logger, 'extractJsonResults names the missing marker',
      message !== null && message.includes(RESULTS_MARKER), true);
  }

  // === Our figures, keyed by program ===

  {
    const ours = oursFromR7rsResults({
      rows: [
        { name: 'fib', workload: 'call', interpretedSeconds: 0.19, compiledSeconds: 0.0028 },
        { name: 'equal', workload: 'list', interpretedSeconds: null, compiledSeconds: null }
      ]
    });
    assert(logger, 'oursFromR7rsResults keeps the interpreted time',
      ours.get('fib').interpreter, 0.19);
    assert(logger, 'oursFromR7rsResults keeps the compiled time',
      ours.get('fib').compiled, 0.0028);
    assert(logger, 'oursFromR7rsResults keeps a failed run as null',
      ours.get('equal').compiled, null);
  }

  // === Geometric mean ===

  assert(logger, 'geometricMean of nothing is null', geometricMean([]), null);
  assert(logger, 'geometricMean of 2 and 8 is 4',
    Math.abs(geometricMean([2, 8]) - 4) < 1e-12, true);

  // === Per-class summary ===

  const rows = [
    { name: 'fib', workload: 'call',
      ours: { interpreter: 10, compiled: 2 }, refs: { gsi: 1, racket: 0.5 } },
    { name: 'tak', workload: 'call',
      ours: { interpreter: 40, compiled: 8 }, refs: { gsi: 2, racket: 1 } },
    { name: 'pi', workload: 'bignum',
      ours: { interpreter: 6, compiled: 6 }, refs: { gsi: 0.1, racket: null } },
    { name: 'equal', workload: 'list',
      ours: { interpreter: null, compiled: null }, refs: { gsi: 1, racket: 1 } }
  ];
  const summary = summarizeByClass(rows, ['interpreter', 'compiled'], ['gsi', 'racket']);
  const find = (workload, tier, reference) => summary.find((s) =>
    s.workload === workload && s.tier === tier && s.reference === reference);

  {
    const call = find('call', 'compiled', 'gsi');
    assert(logger, 'summary ratio is ours over the reference (2/1 and 8/2 -> geomean 2.83)',
      Math.abs(call.geometricMean - Math.sqrt(8)) < 1e-12, true);
    assert(logger, 'summary reports the smallest ratio', call.min, 2);
    assert(logger, 'summary reports the largest ratio', call.max, 4);
    assert(logger, 'summary counts the programs behind a figure', call.programs, 2);
  }

  {
    const interp = find('call', 'interpreter', 'racket');
    assert(logger, 'summary covers every tier and reference (10/0.5 and 40/1 -> 28.28)',
      Math.abs(interp.geometricMean - Math.sqrt(20 * 40)) < 1e-9, true);
  }

  assert(logger, 'a reference that failed is left out rather than counted',
    find('bignum', 'compiled', 'racket'), undefined);
  assert(logger, 'a class where our run failed is left out rather than counted',
    find('list', 'compiled', 'gsi'), undefined);
  assert(logger, 'classes appear in the order the programs do',
    summary.map((s) => s.workload).filter((w, i, all) => all.indexOf(w) === i).join(','),
    'call,bignum');
}
