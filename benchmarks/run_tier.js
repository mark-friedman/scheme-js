/**
 * @fileoverview Programs run as a page runs them, with the tier attached,
 * compiling counted.
 *
 * ## Why this exists
 *
 * The canonical suite (`run_r7rs.js`) compiles each definition as it appears,
 * before the run it times, so it measures compiled code and never what
 * compiling it cost. A program under the tier -- every page, every CLI run --
 * pays both, at the moment the tier decides to compile, and a short program can
 * spend most of its run compiling (R94 in `docs/compiler_findings.md`). This
 * measures that, program by program, and the tier's policy -- what it compiles,
 * and when -- is decided on it.
 *
 * ## The programs
 *
 * Four sets, chosen with `--set`, since no one set is the Scheme a page runs:
 *
 *  - `canonical`: the canonical suite's programs (`benchmarks/r7rs/`) at the
 *    suite's sizes. Kernels, each built to stress one workload: a handful of
 *    top-level procedures and one hot loop.
 *  - `tests`: this repository's Scheme test files, each run after the test
 *    harness it is written for (`tests/core/scheme/test.scm`): scripts of many
 *    top-level forms, with the harness's procedures called hundreds of times.
 *  - `corpus`: the test programs of other people's libraries, downloaded by
 *    `benchmarks/corpus/fetch.js` and listed as `tests` in its manifest. Their
 *    libraries are not shipped, so the tier compiles them as a program's own,
 *    from their first call once they have loaded.
 *  - `page`: `benchmarks/tier_programs/`, synthetic programs in the shapes of
 *    a page's code that the others lack -- handlers made once and called by
 *    events, templates called a few times each, a model updated by messages.
 *
 * ## What is measured
 *
 * Each program is run once from start to end on an interpreter set up as a
 * page sets one up: every shipped library restored from its prebuilt table,
 * its source never read (`tests/harness/page_libraries.js`), the tier
 * attached, and each top-level form run through `runTopLevel`, so the tier
 * sees it as it sees a page's. A run that reads a shipped library's source,
 * its table stale, stops the measurement: no page would make it. Reported:
 * the whole run, the part of it
 * spent in the tier's three procedures -- `bound`, `due` and `form`, which is
 * where every compile happens -- how many of the program's names the tier
 * compiled, and the same run with the tier not attached. Each figure is the
 * best of several runs, each in a fresh interpreter, since a compile happens
 * once a run. A run is wrong if its output with the tier differs from its
 * output without, a canonical program's if it reports a wrong answer, and a
 * test file's if its tests fail.
 *
 * ## Other policies
 *
 * `--policies` measures other policies than today's, so that a change to the
 * policy is measured before it is made. Each is `N` -- a procedure that
 * neither loops nor makes procedures is compiled at its Nth call
 * (`calls-before-compiling` in `src/compiler/tier.scm`) and one that does at
 * definition -- or `loops:N`, where only one that loops is compiled at
 * definition (`compiled-when-bound?`); either followed by `/M` for a
 * library's procedures to wait M calls after it loads, however they loop
 * (`library-calls-before-compiling`), where they wait as many as the
 * compiler says. Today's is `2`, which is `2/10`. `bound` compiles every
 * procedure a program binds as soon as it is bound, whatever it does: what a
 * debugger stepping into a procedure on its first call would need, its code
 * compiled before it first runs. The policies are
 * interleaved, each program run under every one in turn, round after round,
 * each round starting at the next, so that whatever else the machine is doing
 * falls on them alike; naming one twice measures how far apart two runs of
 * the same policy come out.
 *
 * Usage:
 *   node benchmarks/run_tier.js [--set canonical,tests,corpus,page | all]
 *     [--only name,name] [--runs N] [--policies 2,10,loops:2,bound] [--json]
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { createInterpreter } from '../src/core/interpreter/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/expand.js';
import { withPrivateLibraries } from '../src/core/interpreter/library_registry.js';
import { programEnvironment, runProgramForm } from '../src/core/interpreter/library_loader.js';
import { callSchemeProcedure, SCHEME_PRIMITIVE } from '../src/core/interpreter/values.js';
import { attachTier } from '../src/compiler/tiering.js';
import { compilerEnvironment } from '../src/compiler/lowering.js';
import { assembleParts, R7RS_DIR } from './lib/r7rs_harness.js';
import { asOwnPage, shippedLibraries, STANDARD_IMPORTS } from './lib/harness.js';
import { corpusIndex, corpusResolver, isBundled } from './lib/corpus_libraries.js';
import { pageLibraries } from '../tests/harness/page_libraries.js';
import { describeCompilerFailures, takeCompilerFailures } from '../tests/harness/compiler_failures.js';
import { R7RS_BENCHMARKS } from './r7rs/manifest.js';
import { schemeTestFiles, tieredSchemeTestFiles } from '../tests/test_manifest.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};
const SET_NAMES = ['canonical', 'tests', 'corpus', 'page'];
const SETS = valueOf('--set', 'canonical') === 'all' ? SET_NAMES : valueOf('--set', 'canonical').split(',');
const ONLY = valueOf('--only', null)?.split(',') ?? null;
const RUNS = Number(valueOf('--runs', 3));
const JSON_OUT = args.includes('--json');

/**
 * The policies to measure, as `--policies` names them.
 * @type {Array<{label: string, wait: number, loopsOnly: boolean, always: boolean}>}
 */
/**
 * How many calls a library's procedures wait, as the compiler has it now.
 * @type {number}
 */
const LIBRARY_WAIT = Number(compilerEnvironment().env.lookup('library-calls-before-compiling'));

const POLICIES = valueOf('--policies', '2').split(',').map((spec) => {
  if (spec === 'bound') return { label: spec, wait: 2, loopsOnly: false, always: true, libraryWait: LIBRARY_WAIT };
  const match = /^(loops:)?([0-9]+)(?:\/([0-9]+))?$/.exec(spec);
  if (match === null) throw new Error(`a policy is N, loops:N or bound, then /M if it says, not ${spec}`);
  return {
    label: spec, wait: Number(match[2]), loopsOnly: match[1] !== undefined, always: false,
    libraryWait: Number(match[3] ?? LIBRARY_WAIT)
  };
});

for (const set of SETS) {
  if (!SET_NAMES.includes(set)) throw new Error(`no set ${set}; the sets are ${SET_NAMES.join(', ')}`);
}

/**
 * The rule today's policy compiles at definition by, kept to be put back.
 * @type {Function}
 */
const loopsOrProcedures = compilerEnvironment().env.lookup('compiled-when-bound?');

/**
 * The rule the `bound` policy compiles at definition by: every procedure.
 * @returns {boolean}
 */
const everyProcedure = () => true;
everyProcedure[SCHEME_PRIMITIVE] = true;

/**
 * Sets the tier's policy: both parts are globals of the compiler's library,
 * which its compiled code reads through their cells, so a change applies from
 * the next decision on.
 * @param {{wait: number, loopsOnly: boolean, always: boolean, libraryWait: number}} policy - The policy.
 */
function usePolicy(policy) {
  const { env } = compilerEnvironment();
  env.set('calls-before-compiling', BigInt(policy.wait));
  env.set('library-calls-before-compiling', BigInt(policy.libraryWait));
  env.set('compiled-when-bound?',
    policy.always ? everyProcedure : policy.loopsOnly ? env.lookup('contains-loop?') : loopsOrProcedures);
}

// =============================================================================
// Libraries
// =============================================================================

// =============================================================================
// The sets
// =============================================================================

/**
 * The canonical suite's programs, each assembled as `run_r7rs.js` assembles
 * it. Its prelude -- the suite's input and timing procedures -- runs before
 * the tier is attached, as the suite's own harness rather than the program.
 * @returns {Array<Object>} The programs.
 */
function canonicalPrograms() {
  return R7RS_BENCHMARKS.filter((bench) => bench.status === 'ok').map((bench) => ({
    name: bench.name,
    kind: bench.workload,
    cwd: R7RS_DIR,
    libraries: shippedLibraries,
    setup(interpreter, env, evaluate) {
      const { prelude, body } = assembleParts(bench.name, bench.params, 1, 'scheme-js-4', R7RS_DIR);
      evaluate(STANDARD_IMPORTS);
      for (const form of parse(prelude)) interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
      return body;
    },
    // The result line holds the run's time, so the output is not compared.
    comparesOutput: false,
    right: (output) => output.some((line) => line.includes('+!CSVLINE!+') && !line.includes('INCORRECT'))
  }));
}

/**
 * The libraries the Scheme test files are written against, which a test file
 * may use without importing them, as it does in `tests/run_tiered_scheme_tests_lib.js`.
 * @type {string}
 */
const TEST_IMPORTS = `(import (scheme base) (scheme write) (scheme read) (scheme repl) (scheme lazy)
  (scheme case-lambda) (scheme eval) (scheme time) (scheme complex) (scheme cxr) (scheme char)
  (scheme inexact) (scheme file) (scheme process-context)
  (scheme-js promise) (scheme-js interop) (scheme-js js-conversion))`;

/**
 * Test files that cannot be run this way: each needs something a page set up
 * as this one is does not have.
 * @type {Object<string, string>}
 */
const TESTS_LEFT_OUT = {
  'tests/core/scheme/dynamic_wind_interop_tests.scm': 'needs a browser window',
  'tests/scripts/table_writer_tests.scm': 'tests a build tool, from scripts/lib/, which no page loads'
};

/**
 * The repository's Scheme test files, each run after the test harness, which
 * is part of the program: its procedures are the program's, for the tier to
 * compile.
 * @returns {Array<Object>} The programs.
 */
function testPrograms() {
  const harness = fs.readFileSync(path.join(ROOT, 'tests/core/scheme/test.scm'), 'utf8');
  return [...schemeTestFiles, ...tieredSchemeTestFiles]
    .filter((file) => !(file in TESTS_LEFT_OUT))
    .map((file) => ({
      name: path.basename(file, '.scm'),
      kind: path.dirname(file).replace(/^tests\/?/, '') || 'tests',
      cwd: ROOT,
      libraries: shippedLibraries,
      setup(interpreter, env, evaluate, withTier) {
        evaluate(TEST_IMPORTS);
        const quiet = (fn) => { fn[SCHEME_PRIMITIVE] = true; return fn; };
        env.define('native-report-test-result', quiet(() => {}));
        env.define('native-report-test-skip', quiet(() => {}));
        env.define('native-log-title', quiet(() => {}));
        evaluate(`(define *tier-attached* ${withTier ? '#t' : '#f'})`);
        return `${harness}\n${fs.readFileSync(path.join(ROOT, file), 'utf8')}`;
      },
      right: (output, evaluate) => evaluate('(test-report)') === true
    }));
}

/**
 * The corpus's test programs, each in the directory it was written in, with
 * the corpus's libraries read where it keeps them.
 * @returns {Array<Object>} The programs.
 */
function corpusPrograms() {
  const index = corpusIndex();
  return index.tests.map((test) => ({
    name: test.name,
    kind: test.library,
    cwd: path.dirname(test.file),
    libraries: () => pageLibraries({ resolve: corpusResolver(index).resolve, isShipped: (name) => isBundled(index, name) }),
    setup: () => fs.readFileSync(test.file, 'utf8'),
    right: () => true
  }));
}

/**
 * The synthetic programs in the shapes of a page's code.
 * @returns {Array<Object>} The programs.
 */
function pagePrograms() {
  const dir = path.join(ROOT, 'benchmarks/tier_programs');
  return fs.readdirSync(dir).filter((f) => f.endsWith('.scm')).sort().map((file) => ({
    name: path.basename(file, '.scm'),
    kind: 'page',
    cwd: dir,
    libraries: shippedLibraries,
    setup: () => fs.readFileSync(path.join(dir, file), 'utf8'),
    right: () => true
  }));
}

const PROGRAMS = { canonical: canonicalPrograms, tests: testPrograms, corpus: corpusPrograms, page: pagePrograms };

// =============================================================================
// Running one
// =============================================================================

/**
 * Wraps one of the tier's procedures so that the time spent in it is counted.
 * Marked as taking Scheme values, so the evaluator's call converts nothing.
 * @param {Function} hook - The tier's procedure.
 * @param {{ms: number}} spent - Where the time is added.
 * @returns {Function} The wrapper.
 */
function timed(hook, spent) {
  const wrapper = (...hookArgs) => {
    const start = performance.now();
    try {
      return callSchemeProcedure(hook, hookArgs);
    } finally {
      spent.ms += performance.now() - start;
    }
  };
  wrapper[SCHEME_PRIMITIVE] = true;
  return wrapper;
}

/**
 * A run's output without what is expected to differ between runs: a test
 * framework's report of how long its tests took.
 * @param {string[]} output - The lines.
 * @returns {string}
 */
const comparable = (output) => output.join('\n').replace(/in [0-9.e-]+ seconds/g, 'in N seconds');

/**
 * Runs one program once, as a page runs it.
 * @param {Object} program - The program.
 * @param {boolean} withTier - Whether to attach the tier.
 * @returns {{ms: number, compilingMs: number, compiled: number, right: boolean, output: string, error: (string|null)}}
 */
function runOnce(program, withTier) {
  const output = [];
  const log = console.log;
  const errorLog = console.error;
  const cwd = process.cwd();
  const exit = process.exit;
  const libraries = program.libraries();
  let result = { ms: 0, compilingMs: 0, compiled: 0, right: false, error: null };
  // A program's `exit` ends the program, not this process: a test framework
  // exits once it has reported, as chibi's does.
  let exited = false;
  process.exit = () => {
    exited = true;
    throw new Error('the program exited');
  };
  process.chdir(program.cwd);
  try {
    // Each run is a page of its own (`asOwnPage`).
    asOwnPage(() => withPrivateLibraries({ resolver: libraries.resolve, hook: libraries.hook, restorer: libraries.restorer }, () => {
      const { interpreter, env } = createInterpreter();
      const evaluate = (source) => {
        let value;
        for (const form of parse(source)) value = interpreter.runTopLevel(analyze(form), env, { jsAutoConvert: 'raw' });
        return value;
      };
      // The program itself is run as the CLI and a page run one: one that
      // begins with import declarations sees only them.
      const runProgram = (source) => {
        const run = programEnvironment(parse(source), analyze, interpreter, env);
        for (const form of run.forms) runProgramForm(form, analyze, interpreter, run.env, { jsAutoConvert: 'raw' });
      };
      console.log = (...line) => output.push(line.join(' '));
      console.error = (...line) => output.push(line.join(' '));
      const body = program.setup(interpreter, env, evaluate, withTier);
      const spent = { ms: 0 };
      let tier = null;
      if (withTier) {
        tier = attachTier(interpreter, env, { isPrebuilt: libraries.isPrebuilt });
        for (const hook of ['bound', 'due', 'form']) tier[hook] = timed(tier[hook], spent);
      }
      const earlier = new Set(env.bindings.keys());
      const start = performance.now();
      try {
        runProgram(body);
      } catch (e) {
        if (!exited) result.error = e.message;
      }
      const ms = performance.now() - start;
      // The console port keeps a line until its newline, and is shared with
      // the next run, so what is left of this one's last line is written now.
      try {
        evaluate('(flush-output-port (current-output-port))');
      } catch (e) {
        // A program that left nothing to flush with has nothing to flush.
      }
      const compiled = tier === null ? 0
        : [...tier.outcomes].filter(([name, outcome]) => outcome === 'compiled' && !earlier.has(name)).length;
      result = { ...result, ms, compilingMs: spent.ms, compiled, right: result.error === null && program.right(output, evaluate) };
    }));
  } catch (e) {
    result.error = e.message;
  } finally {
    console.log = log;
    console.error = errorLog;
    process.exit = exit;
    process.chdir(cwd);
  }
  // A shipped library read from its source makes a run no page makes.
  if (libraries.fromSource.size > 0) {
    throw new Error(`${[...libraries.fromSource].join(', ')} read from source, where a page restores `
      + 'it from its table: the table is stale, and `npm run prebuild` rebuilds it');
  }
  // A procedure the compiler failed on ran interpreted, a run no correct
  // compiler makes; its warning went where the program's output did.
  const failures = takeCompilerFailures();
  if (failures.length > 0) {
    throw new Error(`the compiler failed on ${describeCompilerFailures(failures)}: a bug in the compiler`);
  }
  return { ...result, output: comparable(output) };
}

// =============================================================================
// Reporting
// =============================================================================

/**
 * Formats milliseconds.
 * @param {number} ms - Milliseconds.
 * @returns {string}
 */
const fmt = (ms) => `${ms.toFixed(1)}`.padStart(8);

const rows = [];
if (!JSON_OUT) {
  console.log(`best of ${RUNS}; ms for one run of each program, as a page runs it; `
    + `policies ${POLICIES.map((p) => p.label).join(', ')} (today's is 2)`);
}
for (const set of SETS) {
  const single = POLICIES.length === 1;
  if (!JSON_OUT && single) {
    console.log(`\n${set}\n${'program'.padEnd(26)} ${'tier'.padStart(8)} ${'compiling'.padStart(9)} `
      + `${'share'.padStart(6)} ${'compiled'.padStart(8)} ${'no tier'.padStart(8)}`);
  }
  if (!JSON_OUT && !single) {
    console.log(`\n${set}, ms with the tier under each policy\n${'program'.padEnd(26)}`
      + `${POLICIES.map((p) => p.label.padStart(10)).join('')} ${'no tier'.padStart(9)}`);
  }
  for (const program of PROGRAMS[set]()) {
    if (ONLY && !ONLY.includes(program.name)) continue;
    const best = POLICIES.map(() => null);
    let plain = null;
    for (let r = 0; r < RUNS; r++) {
      const interpreted = runOnce(program, false);
      if (plain === null || interpreted.ms < plain.ms) plain = interpreted;
      // Each round starts at the next policy, since the run after the
      // interpreted one comes out slower than the rest.
      for (let k = 0; k < POLICIES.length; k++) {
        const i = (r + k) % POLICIES.length;
        usePolicy(POLICIES[i]);
        const tiered = runOnce(program, true);
        if (best[i] === null || tiered.ms < best[i].ms) best[i] = tiered;
      }
    }
    usePolicy(POLICIES[0]);
    const programRows = POLICIES.map((policy, i) => {
      // Wrong only where the tier changed something: a program whose tests
      // fail or that stops on an error without the tier is reported as such.
      const differs = program.comparesOutput !== false && best[i].output !== plain.output;
      const wrong = plain.right && plain.error === null
        ? !best[i].right || best[i].error !== null || differs
        : differs;
      return {
        set, name: program.name, kind: program.kind, policy: policy.label, policyIndex: i, ms: best[i].ms,
        compilingMs: best[i].compilingMs, compiled: best[i].compiled, noTierMs: plain.ms, wrong,
        broken: plain.right && plain.error === null ? null : (plain.error ?? 'its own checks fail without the tier')
      };
    });
    rows.push(...programRows);
    if (!JSON_OUT && single) {
      const [row] = programRows;
      const share = row.ms > 0 ? `${Math.round(100 * row.compilingMs / row.ms)}%` : '-';
      console.log(`${program.name.slice(0, 26).padEnd(26)} ${fmt(row.ms)} ${fmt(row.compilingMs)}  `
        + `${share.padStart(5)} ${String(row.compiled).padStart(8)} ${fmt(row.noTierMs)}`
        + `${row.wrong ? '  WRONG ANSWER' : ''}${row.broken ? `  (without the tier: ${row.broken.slice(0, 60)})` : ''}`);
    }
    if (!JSON_OUT && !single) {
      console.log(`${program.name.slice(0, 26).padEnd(26)}`
        + `${programRows.map((row) => `${row.ms.toFixed(1)}${row.wrong ? '!' : ' '}`.padStart(10)).join('')} `
        + `${plain.ms.toFixed(1).padStart(9)}${programRows[0].broken ? '  (fails without the tier)' : ''}`);
    }
  }
  if (!JSON_OUT) {
    POLICIES.forEach((policy, i) => {
      const inSet = rows.filter((r) => r.set === set && r.policyIndex === i);
      const sum = (key) => inSet.reduce((s, r) => s + r[key], 0);
      console.log(`${(single ? 'total' : `total, policy ${policy.label}`).padEnd(26)} ${fmt(sum('ms'))} `
        + `${fmt(sum('compilingMs'))}  `
        + `${`${Math.round(100 * sum('compilingMs') / Math.max(sum('ms'), 1e-9))}%`.padStart(5)} `
        + `${String(sum('compiled')).padStart(8)} ${fmt(sum('noTierMs'))}; `
        + `${inSet.filter((r) => r.ms > r.noTierMs).length} of ${inSet.length} faster without the tier`
        + `${inSet.some((r) => r.wrong) ? `; ${inSet.filter((r) => r.wrong).length} WRONG` : ''}`);
    });
  }
}
if (JSON_OUT) console.log(JSON.stringify(rows, null, 2));
