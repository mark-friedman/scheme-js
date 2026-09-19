/**
 * Harness for the canonical R7RS benchmark suite.
 *
 * ## Why these programs
 *
 * The eight microbenchmarks in `benchmarks/programs/` were written in Stage 0
 * against this implementation, and every optimization since was chosen by
 * measuring against them -- so the suite and the optimizations were fitted to
 * each other (see R20 in `docs/compiler_strategy.md`). These programs were not.
 * They come from the Gabriel and Gambit benchmark lineage by way of Larceny and
 * `ecraven/r7rs-benchmarks`, they predate this project by decades, and published
 * results exist for more than twenty other Scheme implementations. That makes
 * them useful in two ways a home-grown suite cannot be: as a workload nobody
 * here chose, and as a check that our *harness* agrees with everyone else's.
 *
 * ## The protocol
 *
 * Upstream assembles each run by concatenating
 *
 *     <implementation prelude> <program>.scm common.scm common-postlude.scm
 *
 * and piping `inputs/<program>.input` to standard input. The program reads three
 * or more data from that stream -- a repetition `count`, the program's own
 * parameters, and the expected result -- and hands them to `run-r7rs-benchmark`
 * in `common.scm`, which runs the thunk `count` times, checks the result, and
 * prints the elapsed time.
 *
 * This module reproduces that protocol rather than replacing it, because the
 * value of these programs lies in being the same programs. Two accommodations
 * are made, both recorded in `benchmarks/r7rs/README.md`:
 *
 *  - The `(import ...)` form is removed. Benchmarks are run against an
 *    interpreter bootstrapped exactly as the rest of `benchmarks/` bootstraps
 *    one, so that benchmark startup does not depend on the library loader.
 *  - `read` is rebound so that a call with no argument draws from the input
 *    text through a string port instead of from standard input, which has no
 *    meaning in a browser. A call *with* a port must still read from that port:
 *    `dynamic`, `read0`, `read1` and `sum1` open their own data files, and a
 *    shim that ignored the argument made all four return wrong answers rather
 *    than fail -- the same silent-wrong-answer shape as R15 and R24.
 *
 * ## Timing
 *
 * Timing is taken **inside** the Scheme program by `run-r7rs-benchmark`, using
 * R7RS `current-jiffy`, for every implementation including this one. Timing our
 * own runs from JavaScript would be more precise, but it would measure a
 * different region than the Gambit and Racket runs measure, and asymmetric
 * measurement has already produced three wrong answers in this project (R24).
 * The cost is that this implementation's `jiffies-per-second` is 1000, so a run
 * must last well beyond a millisecond for the figure to mean anything --
 * `checkResolution` exists to say when it does not.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { createBenchmarkInterpreter } from './harness.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { DefineNode } from '../../src/core/interpreter/ast_nodes.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { unsafeDefinitions } from '../../src/compiler/safety.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/** Default location of the vendored suite. */
export const R7RS_DIR = path.join(__dirname, '..', 'r7rs');

/**
 * Escapes a string for inclusion as a Scheme string literal.
 * @param {string} text - Arbitrary text.
 * @returns {string} A Scheme string literal denoting that text.
 */
function schemeStringLiteral(text) {
  return '"' + text.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"';
}

/**
 * Builds the prelude that stands in for upstream's per-implementation prelude.
 *
 * Upstream preludes exist to name the implementation and to import whatever
 * that Scheme calls its R7RS libraries. Ours additionally redirects `read`,
 * since the input arrives as text rather than on standard input.
 *
 * @param {string} inputText - The contents of the program's `.input` file.
 * @param {string} implName - Name reported by `this-scheme-implementation-name`.
 * @returns {string} Scheme source.
 */
export function buildPrelude(inputText, implName) {
  return `(define %bench-input-port (open-input-string ${schemeStringLiteral(inputText)}))
(define %bench-read read)
(define (read . port)
  (if (null? port)
      (%bench-read %bench-input-port)
      (%bench-read (car port))))
(define (this-scheme-implementation-name) ${schemeStringLiteral(implName)})
`;
}

/**
 * Reads a benchmark program with its `(import ...)` form removed.
 * @param {string} name - Program name, without extension.
 * @param {string} [dir] - Suite directory.
 * @returns {string} Scheme source.
 */
export function readProgram(name, dir = R7RS_DIR) {
  const source = fs.readFileSync(path.join(dir, 'src', `${name}.scm`), 'utf8');
  return source.replace(/^\(import[\s\S]*?\)\s*$/m, '');
}

/**
 * Builds the input text for one run.
 *
 * Every program's input begins with a repetition count, followed by that
 * program's own parameters and the expected result. The count is always
 * supplied by the caller, because the right number of repetitions depends on
 * the implementation being measured: at a count that gives this interpreter a
 * second of work, Racket finishes in one tick of its millisecond clock and the
 * measurement means nothing. Each implementation is calibrated separately and
 * results are reported per iteration.
 *
 * Canonical sizes are far beyond what this implementation can run in a sitting
 * -- upstream's `fib` input asks for five repetitions of `fib(40)`, which takes
 * Gambit's interpreter 143 seconds. A program that needs a smaller size carries
 * replacement parameters in the manifest, written out in full so that reading
 * the manifest tells you exactly what was run. The canonical file stays in the
 * tree verbatim, and is used as-is whenever the manifest supplies no override.
 *
 * @param {string} name - Program name.
 * @param {string|null} params - Replacement text for everything after the
 *   count, or null to keep the canonical parameters.
 * @param {number} count - Repetitions.
 * @param {string} [dir] - Suite directory.
 * @returns {string} Input text.
 */
export function buildInput(name, params, count, dir = R7RS_DIR) {
  if (params !== null && params !== undefined) return `${count}\n${params}\n`;
  const canonical = fs.readFileSync(path.join(dir, 'inputs', `${name}.input`), 'utf8');
  // Replace the first datum -- the count -- leaving any leading comment block
  // in place. Several input files open with a paragraph explaining the sizes.
  return canonical.replace(/^((?:\s*;[^\n]*\n)*\s*)(\S+)/, `$1${count}`);
}

/**
 * Assembles the full source for one benchmark run.
 * @param {string} name - Program name.
 * @param {string|null} params - Replacement parameters, or null for canonical.
 * @param {number} count - Repetitions.
 * @param {string} implName - Implementation name for the CSV line.
 * @param {string} [dir] - Suite directory.
 * @returns {string} Complete Scheme source, ready to evaluate.
 */
export function assemble(name, params, count, implName, dir = R7RS_DIR) {
  const { prelude, body } = assembleParts(name, params, count, implName, dir);
  return `${prelude}\n${body}`;
}

/**
 * Assembles a run as two parts: the harness prelude and the benchmark itself.
 *
 * They are kept apart because the compiler tier must not be pointed at the
 * prelude. The prelude is scaffolding this harness wrote -- a `read` shim and
 * an implementation-name stub -- and compiling it would mean reporting a
 * measurement of our own test rig as if it were the workload. It also produced
 * a wrong answer: with the shim compiled, `sum` and `tak` returned inexact
 * results, which is a compiler-tier defect worth chasing separately but not one
 * this benchmark should be provoking.
 *
 * @param {string} name - Program name.
 * @param {string|null} params - Replacement parameters, or null for canonical.
 * @param {number} count - Repetitions.
 * @param {string} implName - Implementation name for the CSV line.
 * @param {string} [dir] - Suite directory.
 * @returns {{prelude: string, body: string}} The two parts.
 */
export function assembleParts(name, params, count, implName, dir = R7RS_DIR) {
  const common = fs.readFileSync(path.join(dir, 'src', 'common.scm'), 'utf8');
  return {
    prelude: buildPrelude(buildInput(name, params, count, dir), implName),
    body: [readProgram(name, dir), common, '(run-benchmark)'].join('\n')
  };
}

/**
 * Parses the elapsed time out of `run-r7rs-benchmark`'s output.
 *
 * The program prints a line beginning `+!CSVLINE!+` carrying
 * `<implementation>,<name>:<params>,<seconds>`, or the word `INCORRECT` in
 * place of the seconds when the result did not match the expected value. A
 * wrong answer is reported rather than thrown, because on this suite a wrong
 * answer is a finding about the implementation and not a broken benchmark.
 *
 * @param {string} output - Everything the program printed.
 * @returns {{seconds: (number|null), label: string, incorrect: boolean}} The
 *   parsed timing, the benchmark's self-reported label, and whether the result
 *   was rejected.
 */
export function parseCsvLine(output) {
  const line = output.split('\n').find((l) => l.includes('+!CSVLINE!+'));
  if (!line) return { seconds: null, label: '', incorrect: false };
  const fields = line.slice(line.indexOf('+!CSVLINE!+') + 11).split(',');
  const value = (fields[2] || '').trim();
  return {
    seconds: value === 'INCORRECT' ? null : parseFloat(value),
    label: (fields[1] || '').trim(),
    incorrect: value === 'INCORRECT'
  };
}

/**
 * Runs one benchmark under this implementation.
 *
 * A fresh interpreter is built per run so that global state from one program
 * cannot reach another, matching `runBenchmark` in `harness.js`.
 *
 * Under the compiler tier, definitions are compiled **as they appear** rather
 * than in a sweep beforehand. Sweeping first is what made the first macro
 * benchmark report the tier as slower than the interpreter (R21): a sweep can
 * only see the definitions that already exist, and a program's own hot
 * procedures are defined while it loads.
 *
 * Which definitions are *safe* to compile is a separate question and is decided
 * over the whole program up front, because it depends on the call graph rather
 * than on any one definition's text.
 *
 * @param {string} name - Program name.
 * @param {string|null} params - Replacement parameters, or null for canonical.
 * @param {number} count - Repetitions.
 * @param {Object} [options] - Options.
 * @param {string} [options.dir] - Suite directory.
 * @param {boolean} [options.useCompiler] - Compile top-level definitions.
 * @returns {{seconds: (number|null), label: string, incorrect: boolean,
 *   error: (string|null), compiled: number, definitions: number,
 *   output: string}} The run's outcome, with how many of the program's
 *   definitions the compiler accepted -- a speedup on a program where nothing
 *   compiled would mean the harness was measuring the wrong thing.
 */
export function runR7rsBenchmark(name, params, count, options = {}) {
  const { dir = R7RS_DIR, useCompiler = false } = options;
  const { prelude, body } = assembleParts(name, params, count, 'scheme-js-4', dir);

  const chunks = [];
  const realLog = console.log;
  console.log = (...args) => chunks.push(args.join(' '));

  let compiled = 0;
  let definitions = 0;
  try {
    const { interpreter, env } = createBenchmarkInterpreter();
    for (const form of parse(prelude)) {
      interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    }

    const asts = parse(body).map((form) => analyze(form));

    // Which definitions a continuation could be captured inside. Computed over
    // the whole program before anything runs, because the answer for one
    // procedure depends on what its callees do -- `maze`'s `make-maze` names no
    // control global and is still unsafe, because `dig-maze` escapes through it
    // (R34). Definitions are still compiled *as they appear* rather than in a
    // sweep, which is what R21 fixed.
    const unsafe = useCompiler ? unsafeDefinitions(asts, env) : new Map();

    for (const ast of asts) {
      if (ast instanceof DefineNode) {
        definitions++;
        if (useCompiler && !unsafe.has(ast.name)) {
          const result = tryCompileDefinition(ast, env);
          if (result.compiled) {
            env.define(result.name, result.procedure);
            compiled++;
            continue;
          }
        }
      }
      interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
    }
  } catch (e) {
    console.log = realLog;
    return {
      seconds: null, label: '', incorrect: false, compiled, definitions,
      error: e.message, output: chunks.join('\n')
    };
  } finally {
    console.log = realLog;
  }

  const output = chunks.join('\n');
  return { ...parseCsvLine(output), error: null, compiled, definitions, output };
}

/**
 * Chooses a repetition count that puts a run well clear of the clock.
 *
 * Calibrating per implementation is the point: a count that gives this
 * interpreter a second of work gives Racket one tick of its millisecond clock,
 * and a single tick is not a measurement. Results are reported per iteration,
 * so the counts differing between implementations costs nothing.
 *
 * A zero reading means the probe finished inside one tick, which says nothing
 * about how long it took. Scaling from it would be dividing by a number we did
 * not measure, so it returns a large count and the caller measures again --
 * `calibrate` below does that loop.
 *
 * @param {number} secondsForOne - Time for a single iteration.
 * @param {number} target - Desired total seconds.
 * @returns {number} A repetition count of at least 1.
 */
export function calibrateCount(secondsForOne, target) {
  if (!(secondsForOne > 0)) return Math.max(1, Math.round(target * 1000));
  return Math.max(1, Math.round(target / secondsForOne));
}

/**
 * Measures one benchmark, raising the repetition count until the total is
 * long enough to mean something.
 *
 * A single calibrate-then-measure pass is not enough at the fast end. Racket
 * runs `deriv` a thousand times inside one tick of its millisecond clock, and
 * the resulting zero produced a per-iteration time of zero and a ratio of
 * `Infinity` -- a number that looks like a result and is not one. Each round
 * scales the count from what was actually observed, and a run that still reads
 * zero after the last round is reported as unmeasurable rather than as fast.
 *
 * @param {function(number): {seconds: (number|null)}} runAt - Runs the
 *   benchmark at a given repetition count.
 * @param {number} target - Desired total seconds.
 * @param {number} [floor] - Total seconds below which a reading is rejected.
 *   Defaults to half the target, which is the point at which `checkResolution`
 *   is satisfied on this implementation's millisecond clock.
 * @param {number} [rounds] - How many times to raise the count.
 * @returns {{seconds: (number|null), count: number, unmeasurable: boolean,
 *   result: Object}} Per-iteration seconds, the count used, and the last raw result.
 */
export function calibrate(runAt, target, floor = null, rounds = 4) {
  const minimum = floor === null ? target / 2 : floor;
  let count = 1;
  let result = runAt(count);
  for (let round = 0; round < rounds; round++) {
    if (result.seconds === null) break;
    if (result.seconds >= minimum) {
      return { seconds: result.seconds / count, count, unmeasurable: false, result };
    }
    const next = Math.max(count * 2, calibrateCount(result.seconds / count, target));
    if (next === count) break;
    count = next;
    result = runAt(count);
  }
  if (result.seconds === null) {
    return { seconds: null, count, unmeasurable: false, result };
  }
  if (result.seconds <= 0) {
    return { seconds: null, count, unmeasurable: true, result };
  }
  return { seconds: result.seconds / count, count, unmeasurable: false, result };
}

/**
 * Reports whether a measured time is long enough for the clock behind it.
 *
 * Two of the three measurement bugs found in this project were a clock that
 * could not resolve the thing being timed, so this is stated with every result
 * rather than assumed.
 *
 * @param {number} seconds - The measured time.
 * @param {number} jiffiesPerSecond - The implementation's clock resolution.
 * @returns {{ok: boolean, message: (string|null)}} Whether the measurement is
 *   trustworthy, and if not, why not.
 */
export function checkResolution(seconds, jiffiesPerSecond) {
  const ticks = seconds * jiffiesPerSecond;
  if (ticks >= 500) return { ok: true, message: null };
  return {
    ok: false,
    message: `measured over ${Math.round(ticks)} clock ticks `
      + `(${jiffiesPerSecond} jiffies/s); raise the repetition count`
  };
}
