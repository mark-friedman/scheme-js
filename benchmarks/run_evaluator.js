/**
 * @fileoverview Task 68's ceiling: the evaluator, written in Scheme to the
 * interpreter's design and compiled, against the interpreter itself.
 *
 * Runs `benchmarks/evaluator/kernels.scm` three ways, each in a process of its
 * own: on the interpreter, with the tier off (`benchmarks/evaluator/
 * interpreted.scm` under `--no-compile`); on the evaluator in Scheme
 * (`benchmarks/run_evaluator.scm`), which the tier compiles; and compiled by
 * the tier, for scale. Prints each kernel's best time of several runs and the
 * evaluator in Scheme's time as a multiple of the interpreter's, and checks
 * that every way gave every kernel's answer.
 *
 * Usage:
 *   node benchmarks/run_evaluator.js [--runs N]
 */

import { execFileSync } from 'child_process';
import path from 'path';
import { fileURLToPath } from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
const runsAt = args.indexOf('--runs');
const RUNS = runsAt >= 0 ? args[runsAt + 1] : '5';

/**
 * Runs a driver and reads its lines, `label<TAB>ms<TAB>answer-right`.
 * @param {Array<string>} argv - The CLI's arguments.
 * @returns {Map<string, {ms: number, right: boolean}>}
 */
function run(argv) {
  const out = execFileSync('node', ['repl.js', ...argv, RUNS], { cwd: ROOT, encoding: 'utf8' });
  const rows = new Map();
  for (const line of out.trim().split('\n')) {
    const [label, ms, right] = line.split('\t');
    if (right !== undefined) rows.set(label, { ms: Number(ms), right: right === '#t' });
  }
  return rows;
}

const interpreted = run(['--no-compile', 'benchmarks/evaluator/interpreted.scm']);
const scheme = run(['-I', 'scripts/lib', 'benchmarks/run_evaluator.scm']);
const compiled = run(['benchmarks/evaluator/interpreted.scm']);

console.log(`best of ${RUNS}; ms\n`);
console.log('kernel'.padEnd(46), 'interpreter'.padStart(12), 'in Scheme'.padStart(10), 'ratio'.padStart(7),
  'compiled'.padStart(10));
let product = 1;
let count = 0;
for (const [label, row] of interpreted) {
  const s = scheme.get(label);
  const c = compiled.get(label);
  const ratio = s.ms / row.ms;
  product *= ratio;
  count++;
  const wrong = [row, s, c].some((r) => !r.right) ? '  WRONG ANSWER' : '';
  console.log(label.padEnd(46), row.ms.toFixed(1).padStart(12), s.ms.toFixed(1).padStart(10),
    ratio.toFixed(2).padStart(7), c.ms.toFixed(2).padStart(10), wrong);
}
console.log(`\ngeometric mean of the ratios: ${Math.pow(product, 1 / count).toFixed(2)}`);
