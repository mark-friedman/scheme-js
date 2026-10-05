/**
 * @fileoverview Runs Scheme test files twice: with the program's own code
 * interpreted, and with it compiled by the tier as a page's is.
 *
 * Both runs start as a page does: every library the bundle ships is installed
 * from its prebuilt table as it loads (`src/packaging/scheme_entry.js`), in a
 * library registry of the run's own, and each file is run form by form through
 * `runTopLevel`, so that the tier sees each top-level form as it would a
 * script's. The second run attaches the tier; the first does not, as a page
 * that has turned compiling its own code off. So a test file says once what
 * should hold, and the two runs check that it holds whichever tier runs the
 * code.
 *
 * A test file can tell which run it is in from `*tier-attached*`, and so
 * expect a failure in one run only: `(test-expect-fail (and *tier-attached*
 * "why") ...)`. A file run with the tier attached that the tier compiled
 * nothing from is reported as a failure, since that run would only have tested
 * the interpreter a second time.
 */

import { createInterpreter } from '../src/core/interpreter/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/expand.js';
import { withPrivateLibraries } from '../src/core/interpreter/library_registry.js';
import { writeString } from '../src/core/primitives/io/printer.js';
import { installLibraryTable } from '../src/compiler/prebuilt.js';
import { attachTier } from '../src/compiler/tiering.js';
import { SCHEME_PRIMITIVE } from '../src/core/interpreter/values.js';
import prebuiltLibraries from '../src/packaging/compiled_libraries.js';
import { BUNDLED_SOURCES } from '../src/packaging/bundled_libraries.js';

/**
 * The two runs.
 * @type {Array<{label: string, tier: boolean}>}
 */
const CONFIGURATIONS = [
  { label: 'program interpreted', tier: false },
  { label: 'program compiled by the tier', tier: true }
];

/**
 * The libraries a page imports as it starts, and those the Scheme tests are
 * written against besides.
 * @type {string}
 */
const IMPORTS = `(import (scheme base) (scheme write) (scheme read) (scheme repl) (scheme lazy)
                         (scheme case-lambda) (scheme eval) (scheme time) (scheme complex)
                         (scheme cxr) (scheme char) (scheme inexact)
                         (scheme-js promise) (scheme-js interop) (scheme-js js-conversion))`;

/**
 * A library's source, or a file a library includes, from the bundle.
 * @param {string[]} name - A library name, or an include's path.
 * @returns {string} Its source.
 * @throws {Error} If the bundle has no such file.
 */
function bundledSource(name) {
  const last = name[name.length - 1];
  const source = BUNDLED_SOURCES[`${last}.sld`] ?? BUNDLED_SOURCES[last];
  if (source === undefined) throw new Error(`no bundled library ${name.join('/')}`);
  return source;
}

/**
 * Whether a library ships with the bundle, and so has a prebuilt table that
 * the tier leaves alone.
 * @param {string[]} libraryName - The library's name.
 * @returns {boolean}
 */
function isPrebuilt(libraryName) {
  return BUNDLED_SOURCES[`${libraryName[libraryName.length - 1]}.sld`] !== undefined;
}

/**
 * Installs a shipped library's prebuilt table as the library loads.
 * @param {string[]} libraryName - The library's name.
 * @param {Object} libraryEnv - Its environment.
 */
function installTable(libraryName, libraryEnv) {
  if (!isPrebuilt(libraryName) || !libraryEnv) return;
  installLibraryTable(prebuiltLibraries, libraryName, libraryEnv, (file) => BUNDLED_SOURCES[file]);
}

/**
 * A logger that names the run in every result it passes on, and passes on the
 * rest of the logger's methods as they are.
 * @param {Object} logger - The test logger.
 * @param {string} label - The run.
 * @returns {Object} The prefixing logger.
 */
function labelled(logger, label) {
  return {
    ...logger,
    pass: (message) => logger.pass(`[${label}] ${message}`),
    fail: (message) => logger.fail(`[${label}] ${message}`),
    skip: (message) => logger.skip(`[${label}] ${message}`),
    title: (title) => logger.title(`${title} -- ${label}`)
  };
}

/**
 * A test name as the logger prints it: a string as it is, and the expression
 * a two-argument `test` is named by, written out.
 * @param {*} name - The name, a Scheme value.
 * @returns {string}
 */
const nameOf = (name) => (typeof name === 'string' ? name : writeString(name));

/**
 * Marks a reporter as taking Scheme values, so that what a test expected and
 * got is printed as Scheme writes it, an exact integer as `285` and not the
 * `285.0` it would be once converted for a JavaScript function.
 * @param {Function} fn - The reporter.
 * @returns {Function} The same function.
 */
function takesSchemeValues(fn) {
  fn[SCHEME_PRIMITIVE] = true;
  return fn;
}

/**
 * The names a tier has compiled, leaving out some already bound.
 * @param {Object} tier - The tier.
 * @param {Set<string>} earlier - Names bound before the code in question ran,
 *   such as the harness's, which the tier may compile while a test file runs.
 * @returns {Array<string>}
 */
export function compiledSince(tier, earlier) {
  return [...tier.outcomes]
    .filter(([name, outcome]) => outcome === 'compiled' && !earlier.has(name))
    .map(([name]) => name);
}

/**
 * Runs Scheme test files in one configuration, in a library registry of its
 * own.
 * @param {Object} logger - A logger for this configuration.
 * @param {boolean} withTier - Whether to attach the tier.
 * @param {string} harness - The source of `tests/core/scheme/test.scm`.
 * @param {Array<{file: string, source: string}>} files - The test files.
 */
function runConfiguration(logger, withTier, harness, files) {
  withPrivateLibraries({ resolver: bundledSource, hook: installTable }, () => {
    const { interpreter, env } = createInterpreter();
    const evaluate = (source) => {
      let value;
      for (const form of parse(source)) value = interpreter.runTopLevel(analyze(form), env, { jsAutoConvert: 'raw' });
      return value;
    };

    evaluate(IMPORTS);
    env.define('native-report-test-result', takesSchemeValues((name, passed, expected, actual) => {
      const detail = `(Expected: ${writeString(expected)}, Got: ${writeString(actual)})`;
      if (passed) logger.pass(`${nameOf(name)} ${detail}`); else logger.fail(`${nameOf(name)} ${detail}`);
    }));
    env.define('native-report-test-skip', takesSchemeValues((name, reason) => {
      logger.skip(`${nameOf(name)} (Reason: ${String(reason)})`);
    }));
    env.define('native-log-title', takesSchemeValues((title) => logger.title(String(title))));
    evaluate(`(define *tier-attached* ${withTier ? '#t' : '#f'})`);
    const tier = withTier ? attachTier(interpreter, env, { isPrebuilt }) : null;
    if (withTier && tier === null) {
      logger.fail('the tier could not be attached, so nothing was compiled');
      return;
    }
    evaluate(harness);

    for (const { file, source } of files) {
      const earlier = new Set(env.bindings.keys());
      try {
        evaluate(source);
      } catch (e) {
        logger.fail(`${file} crashed: ${e.message}`);
      }
      if (evaluate('(test-report)') !== true) logger.fail(`${file} FAILED`);
      if (withTier && compiledSince(tier, earlier).length === 0) {
        logger.fail(`${file}: the tier compiled nothing from it, so this run tested the interpreter again`);
      }
      evaluate('(set! *test-failures* 0) (set! *test-passes* 0) (set! *test-skips* 0)');
    }
  });
}

/**
 * Runs Scheme test files with the program interpreted, and again with it
 * compiled by the tier.
 * @param {Object} logger - Test logger.
 * @param {Array<string>} testFiles - The files, relative to the repository.
 * @param {function(string): Promise<string>} fileLoader - Reads a file.
 * @returns {Promise<void>}
 */
export async function runTieredSchemeTests(logger, testFiles, fileLoader) {
  const harness = await fileLoader('tests/core/scheme/test.scm');
  const files = [];
  for (const file of testFiles) files.push({ file, source: await fileLoader(file) });
  for (const { label, tier } of CONFIGURATIONS) {
    logger.title(`Running Scheme Tests in Both Tiers -- ${label}`);
    runConfiguration(labelled(logger, label), tier, harness, files);
  }
}
