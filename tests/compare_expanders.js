/**
 * @fileoverview Runs a process with the two expanders compared on every form
 * it analyzes at a top level (harness/expander_comparison.js), and reports
 * where they disagree when it exits. Loaded before the process's own module:
 *
 *     node --import ./tests/compare_expanders.js run_tests_node.js
 *
 * which `npm run test:expanders` does. `COMPARE_REPORT=<file>` writes the
 * whole report there; `COMPARE_LIMIT=<n>` keeps that many disagreements in full.
 */

import fs from 'fs';
import { installExpanderComparison, comparisonReport } from './harness/expander_comparison.js';

const record = installExpanderComparison({ limit: Number(process.env.COMPARE_LIMIT ?? 200) });

process.on('exit', () => {
  const report = comparisonReport(record);
  if (process.env.COMPARE_REPORT) fs.writeFileSync(process.env.COMPARE_REPORT, report);
  console.log(report.split('\n').slice(0, 80).join('\n'));
  if (record.count > 0) process.exitCode = 1;
});
