/**
 * @fileoverview The JavaScript interop suites, run a second time with the
 * compiler tier attached.
 *
 * The suites are written against an interpreter and run with the program's own
 * code interpreted. A page attaches the tier, which compiles that code as it
 * runs, so each suite is run here again on an interpreter with the tier
 * attached, with the standard library interpreted beneath it as the shared
 * interpreter has it. Only the suites from which the tier compiles something
 * are run: most of their procedures are called once, so from the rest it
 * compiles nothing and a second run would test the interpreter again. Each is
 * checked to have compiled some procedure of its own, so that a change which
 * stops the tier compiling any of them is reported rather than passed.
 *
 * The Scheme tests of JavaScript calling Scheme procedures, which cover the
 * compiled tier deliberately, are in `tests/tiers/` and run through
 * `tests/run_tiered_scheme_tests_lib.js`.
 */

import { interpretedLibrary } from '../harness/standard_library.js';
import { attachTier } from '../../src/compiler/tiering.js';
import { globalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';
import { compiledSince } from '../run_tiered_scheme_tests_lib.js';

/**
 * The suites, by module and entry point, relative to this directory.
 * @type {Array<{path: string, fn: string}>}
 */
const SUITES = [
  { path: '../extras/primitives/interop_tests.js', fn: 'runInteropTests' },
  { path: './js_exception_tests.js', fn: 'runJsExceptionTests' },
  { path: './class_interop_tests.js', fn: 'runClassInteropTests' },
  { path: './callable_closures_tests.js', fn: 'runCallableClosuresTests' }
];

/**
 * A logger that marks every result it passes on as coming from a run with the
 * tier attached, and passes on the rest of the logger's methods as they are.
 * @param {Object} logger - The test logger.
 * @returns {Object} The prefixing logger.
 */
function tiered(logger) {
  const label = '[tier attached]';
  return {
    ...logger,
    pass: (message) => logger.pass(`${label} ${message}`),
    fail: (message) => logger.fail(`${label} ${message}`),
    skip: (message) => logger.skip(`${label} ${message}`),
    title: (title) => logger.title(`${title} ${label}`)
  };
}

/**
 * Runs each interop suite on an interpreter with the tier attached.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runTieredInteropTests(logger) {
  const labelled = tiered(logger);
  for (const { path, fn } of SUITES) {
    // Every interpreter shares one macro registry, and the tests that ran
    // before this leave their macros in it -- `macro_tests.js` a `foo`, which
    // a suite here defines as a procedure and calls. So each suite starts from
    // the standard library's macros alone, as it did the first time it ran,
    // and the registry the rest of the tests share is given back afterwards.
    const savedMacros = new Map(globalMacroRegistry.macros);
    globalMacroRegistry.macros.clear();
    try {
      await runSuite(labelled, path, fn);
    } finally {
      globalMacroRegistry.macros.clear();
      for (const [name, macro] of savedMacros) globalMacroRegistry.macros.set(name, macro);
    }
  }
}

/**
 * Runs one interop suite on a fresh interpreter with the tier attached, and
 * checks that the tier compiled some procedure of its own.
 * @param {Object} logger - The prefixing logger.
 * @param {string} path - The suite's module.
 * @param {string} fn - Its entry point.
 * @returns {Promise<void>}
 */
async function runSuite(logger, path, fn) {
  const { interpreter, env } = interpretedLibrary();
  const tier = attachTier(interpreter, env);
  if (tier === null) {
    logger.fail(`${path}: the tier could not be attached`);
    return;
  }
  const earlier = new Set(env.bindings.keys());
  const suite = (await import(path))[fn];
  await suite(interpreter, logger);
  const compiled = compiledSince(tier, earlier);
  if (compiled.length > 0) {
    logger.pass(`${path}: the tier compiled ${compiled.join(', ')}`);
  } else {
    logger.fail(`${path}: the tier compiled nothing from it, so this run tested the interpreter again`);
  }
}
