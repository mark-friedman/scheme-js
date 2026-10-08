/**
 * Runs one benchmark compiled ahead of time in its own process, and reports
 * the time it measured itself.
 *
 * Its own process for the reason `r7rs_worker.js` has one: a benchmark can
 * hang or exhaust the stack. And a program compiled ahead of time runs with
 * no interpreter, so this imports nothing but its runtime: not even
 * r7rs_harness.js, whose imports load the interpreter and the library system's
 * tables, which would sit in the heap the program's collections walk.
 *
 * Usage (not intended to be run by hand):
 *   node benchmarks/lib/ahead_worker.js <table.js>
 */

import path from 'path';
import { fileURLToPath, pathToFileURL } from 'url';
import { runProgram, AHEAD_PRIMITIVES } from '../../src/compiler/ahead.js';

// Upstream runs every benchmark from the suite's directory, as r7rs_worker.js does.
process.chdir(path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'r7rs'));

const program = (await import(pathToFileURL(path.resolve(process.argv[2])).href)).default;
const lines = [];
console.log = (...args) => lines.push(args.join(' '));
let error = null;
try {
  runProgram(program);
} catch (e) {
  error = String(e?.message ?? e).slice(0, 120);
}
AHEAD_PRIMITIVES['%console-output-port']().flush();

// `run-r7rs-benchmark` writes `+!CSVLINE!+<implementation>,<name>,<seconds>`,
// or INCORRECT for the seconds (`parseCsvLine` in r7rs_harness.js).
const line = lines.find((l) => l.includes('+!CSVLINE!+'));
const value = line === undefined ? '' : (line.split(',')[2] ?? '').trim();
process.stdout.write(JSON.stringify({
  seconds: value === '' || value === 'INCORRECT' ? null : parseFloat(value),
  incorrect: value === 'INCORRECT',
  error
}));
