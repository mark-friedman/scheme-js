/**
 * Runs a single R7RS benchmark measurement in its own process.
 *
 * Exists because a benchmark can hang or exhaust the stack, and neither should
 * take the suite with it. On the first run of this suite, `takl` did not finish
 * one iteration in two minutes and `equal` did not finish at any size -- both
 * cases where a wrong answer would have been more convenient than none, and
 * both worth reporting as results rather than as a stalled harness.
 *
 * Reads a JSON request on argv and writes a JSON result to stdout, so the
 * parent can impose a wall-clock budget with `execFileSync`'s timeout and treat
 * a killed child as a timing-out benchmark.
 *
 * Usage (not intended to be run by hand):
 *   node benchmarks/lib/r7rs_worker.js '<json request>'
 */

import { runR7rsBenchmark, R7RS_DIR } from './r7rs_harness.js';

// Several programs open their data by a path relative to the suite root --
// `dynamic` reads "inputs/dynamic.data", `read1` reads "inputs/parsing.data".
// Upstream runs every benchmark from that directory, so we do too, rather than
// rewriting paths inside vendored sources that are meant to stay verbatim.
process.chdir(R7RS_DIR);

const request = JSON.parse(process.argv[2]);

// The workload's own output would otherwise be interleaved with the result.
const chunks = [];
const realLog = console.log;
console.log = (...args) => chunks.push(args.join(' '));

let result;
try {
  const run = runR7rsBenchmark(request.name, request.params, request.count, {
    useCompiler: request.useCompiler
  });
  result = {
    seconds: run.seconds,
    incorrect: run.incorrect,
    error: run.error ? run.error.slice(0, 120) : null,
    compiled: run.compiled,
    definitions: run.definitions
  };
} catch (e) {
  result = {
    seconds: null, incorrect: false, error: (e.message || String(e)).slice(0, 120),
    compiled: 0, definitions: 0
  };
}

console.log = realLog;
process.stdout.write(JSON.stringify(result));
