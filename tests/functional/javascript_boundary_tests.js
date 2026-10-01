/**
 * @fileoverview A Scheme procedure's plain call, as JavaScript makes it, is
 * exactly the public conversions around the public call that converts
 * nothing: its arguments converted into Scheme by `jsToScheme`, the call made
 * by `callSchemeProcedure`, and its result converted out by `schemeToJsDeep`.
 * So a developer can do step by step whatever the plain call does, and nothing
 * about the boundary is private to the implementation.
 *
 * Checked for each kind of procedure a program makes -- interpreted closures,
 * and procedures the tier compiled, top-level and nested -- since the two
 * tiers give a procedure its plain call in different places
 * (`createClosure` in `src/core/interpreter/values.js`, and the code
 * `src/compiler/emit.scm` generates). JavaScript tests, since only JavaScript
 * makes a plain call.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { jsToScheme, schemeToJsDeep } from '../../src/core/interpreter/js_interop.js';
import { attachTier } from '../../src/compiler/tiering.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';

/**
 * The procedures under test, and how each is reached once defined.
 * @type {string}
 */
const PROGRAM = `
  (define (describe x)
    (cond ((and (number? x) (exact? x)) (list 'exact x))
          ((number? x) (list 'inexact x))
          ((string? x) (string-append x "!"))
          ((vector? x) (vector-map (lambda (e) (if (and (number? e) (exact? e)) 'exact e)) x))
          (else x)))
  (define (pair-up a b) (vector a b (string-copy "made")))
  (define (two-values) (values 7 8))
  (define (adder n) (lambda (x) (+ x n)))
  (define add-five (adder 5))`;

/**
 * Argument lists, as JavaScript passes them, for each procedure.
 * @type {Object<string, Array<Array<*>>>}
 */
const CALLS = {
  describe: [[1], [2.5], ['ab'], [[1, 2.5]], [true]],
  'pair-up': [[1, 'x'], [2.5, [3]]],
  'two-values': [[]],
  'add-five': [[1], [0.5]]
};

/**
 * Whether two JavaScript values are the same, deeply: a number and a `BigInt`
 * of the same value are different, as they are to JavaScript.
 * @param {*} a - One value.
 * @param {*} b - The other.
 * @returns {boolean}
 */
function same(a, b) {
  if (typeof a !== typeof b) return false;
  if (a === null || b === null || typeof a !== 'object') return Object.is(a, b);
  if (a.constructor !== b.constructor) return false;
  const keys = Object.keys(a);
  return keys.length === Object.keys(b).length && keys.every((k) => same(a[k], b[k]));
}

/**
 * Runs the program in an interpreter with the standard library, with the tier
 * attached or not, and calls each procedure twice from Scheme, so that the tier
 * compiles those it compiles on their second call.
 * @param {boolean} withTier - Whether to attach the tier.
 * @returns {Object} The environment.
 */
function program(withTier) {
  const { interpreter, env } = interpretedLibrary();
  installStandardLibrary(env);
  if (withTier) attachTier(interpreter, env);
  const run = (source) => {
    for (const form of parse(source)) interpreter.runTopLevel(analyze(form), env, { jsAutoConvert: 'raw' });
  };
  run(PROGRAM);
  run('(describe 1) (describe 1) (pair-up 1 2) (pair-up 1 2) (two-values) (two-values) (add-five 1) (add-five 1)');
  return env;
}

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runJavaScriptBoundaryTests(logger) {
  for (const withTier of [false, true]) {
    logger.title(`The Plain Call Is the Public Conversions Around the Call That Converts Nothing -- ${withTier ? 'compiled by the tier' : 'interpreted'}`);
    const env = program(withTier);
    const procedures = Object.keys(CALLS).map((name) => env.lookup(name));
    assert(logger, `setup: the procedures are ${withTier ? '' : 'not '}compiled`,
      procedures.every((p) => p.$compiled === true), withTier);
    for (const [name, calls] of Object.entries(CALLS)) {
      const procedure = env.lookup(name);
      for (const args of calls) {
        const plain = procedure(...args);
        const stepwise = schemeToJsDeep(callSchemeProcedure(procedure, args.map(jsToScheme)));
        assert(logger, `${name} of ${JSON.stringify(args)}: the plain call gives what the steps give`,
          same(plain, stepwise) ? 'same' : `plain ${String(plain)}, steps ${String(stepwise)}`, 'same');
      }
    }
  }
}
