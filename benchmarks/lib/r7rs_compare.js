/**
 * @fileoverview Arithmetic for comparing scheme-js-4 with other Scheme
 * implementations on the canonical R7RS suite.
 *
 * Kept apart from `compare_r7rs.js`, which runs processes, so that it can be
 * unit tested -- in the browser as well as Node, which is why nothing here
 * imports a Node module. A ratio upside down, or a failed run averaged in as
 * if it were a time, would produce a table that looks like a result and is not
 * one.
 */

/**
 * The line `run_r7rs.js` prints before its machine-readable results. Anything
 * before it is the human-readable report.
 * @type {string}
 */
export const RESULTS_MARKER = '--- JSON Results ---';

/**
 * Reads the JSON results out of everything `run_r7rs.js` printed.
 *
 * Lets a comparison reuse a run already made -- scheme-js-4 in both tiers takes
 * far longer than the reference implementations, and nothing about the
 * references changes when our code does.
 *
 * @param {string} text - The complete output of a `run_r7rs.js` run.
 * @returns {Object} The parsed results: `{profile, target, rows, byClass}`.
 * @throws {Error} If the output has no results section.
 */
export function extractJsonResults(text) {
  const at = text.indexOf(RESULTS_MARKER);
  if (at < 0) {
    throw new Error(`no "${RESULTS_MARKER}" section: not the output of run_r7rs.js, `
      + 'or the run did not finish');
  }
  return JSON.parse(text.slice(at + RESULTS_MARKER.length));
}

/**
 * Indexes `run_r7rs.js` results by program.
 *
 * @param {{rows: Array<Object>}} results - Parsed `run_r7rs.js` results.
 * @returns {Map<string, {interpreter: (number|null), compiled: (number|null)}>}
 *   Seconds per iteration in each tier, null where that run failed.
 */
export function oursFromR7rsResults(results) {
  const ours = new Map();
  for (const row of results.rows) {
    ours.set(row.name, {
      interpreter: row.interpretedSeconds ?? null,
      compiled: row.compiledSeconds ?? null
    });
  }
  return ours;
}

/**
 * Geometric mean of a list of ratios.
 *
 * Geometric rather than arithmetic, because an arithmetic mean over ratios is
 * dominated by whichever entry is largest.
 *
 * @param {number[]} values - Positive ratios.
 * @returns {number|null} The geometric mean, or null if the list is empty.
 */
export function geometricMean(values) {
  if (values.length === 0) return null;
  return Math.exp(values.reduce((acc, v) => acc + Math.log(v), 0) / values.length);
}

/**
 * Summarizes, per workload class, how each of our tiers compares with each
 * reference implementation.
 *
 * Each figure is the geometric mean over the class's programs of our time
 * divided by the reference's, so above 1 means slower than the reference.
 * A program is left out of a figure when either side has no time -- a failed
 * or unmeasurable run is not a time, and a class with no program left gets no
 * figure at all rather than a misleading one.
 *
 * @param {Array<{name: string, workload: string,
 *   ours: Object<string, (number|null)>, refs: Object<string, (number|null)>}>} rows
 *   Seconds per iteration, keyed by tier and by reference.
 * @param {string[]} tiers - Which of our tiers to report, e.g. `['compiled']`.
 * @param {string[]} references - Which references to report.
 * @returns {Array<{workload: string, tier: string, reference: string,
 *   geometricMean: number, min: number, max: number, programs: number}>}
 *   One entry per class, tier and reference that has any data, classes in the
 *   order their programs first appear.
 */
export function summarizeByClass(rows, tiers, references) {
  const classes = [...new Set(rows.map((r) => r.workload))];
  const summary = [];
  for (const workload of classes) {
    for (const tier of tiers) {
      for (const reference of references) {
        const ratios = rows
          .filter((r) => r.workload === workload)
          .map((r) => [r.ours[tier], r.refs[reference]])
          .filter(([ours, ref]) => ours > 0 && ref > 0)
          .map(([ours, ref]) => ours / ref);
        if (ratios.length === 0) continue;
        summary.push({
          workload, tier, reference,
          geometricMean: geometricMean(ratios),
          min: Math.min(...ratios),
          max: Math.max(...ratios),
          programs: ratios.length
        });
      }
    }
  }
  return summary;
}
