/**
 * @fileoverview The differential fuzzer: random programs, run in both tiers.
 *
 * `program_generator.scm` builds a program from a seed and says which of its
 * procedures to compile. Here it is run twice: once with everything
 * interpreted, the reference semantics; and once with the standard library
 * compiled, as the browser installs it, and the chosen procedures compiled too
 * -- captures allowed, so the tiers alternate at random, captures included. The
 * two answers must agree.
 *
 * A third run attaches the compiler tier (`src/compiler/tiering.js`) and runs
 * each form as a program's top level is run, so procedures are compiled as the
 * program goes -- some when defined, some in the middle of a recursion or a
 * loop through them -- and must agree too.
 *
 * The tier's serious bugs were found by whole programs giving wrong answers,
 * not by unit tests, because unit tests are written for the shapes their author
 * had in mind. This generalises the whole-program check to shapes nobody had in
 * mind. It does not reach interop or I/O: the programs use neither.
 *
 * Each tier keeps one environment across programs. Every program defines the
 * same names afresh -- its globals first -- so nothing one leaves behind is read
 * by the next, and the standard library is loaded once per tier rather than
 * once per program.
 */

import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { DefineNode } from '../../src/core/interpreter/ast_nodes.js';
import { tryCompileDefinition, tryCompileExpression, runCompiledThunk } from '../../src/compiler/index.js';
import { settle } from '../../src/compiler/runtime.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import { attachTier } from '../../src/compiler/tiering.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';

/**
 * Runs Scheme source in an environment and returns the last value.
 * @param {{interpreter: Object, env: Object}} pair - Where to run it.
 * @param {string} source - Scheme source.
 * @returns {*} The last value.
 */
function evaluate({ interpreter, env }, source) {
  let value;
  for (const form of parse(source)) {
    value = settle(interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' }));
  }
  return value;
}

/**
 * Loads the program generator.
 * @param {Function} loader - Reads a file by its path from the project root.
 * @returns {Promise<Function>} Given a seed, returns the program: its forms as
 *   text, and the names of the procedures to compile.
 */
export async function createGenerator(loader) {
  const pair = interpretedLibrary();
  evaluate(pair, await loader('tests/fuzz/program_generator.scm'));
  return (seed) => {
    const result = settle(pair.interpreter.run(
      analyze(parse(`(generate-program ${seed})`)[0]), pair.env, [], undefined, { jsAutoConvert: 'raw' }));
    const forms = [];
    for (let c = result.car; c !== null; c = c.cdr) forms.push(writeString(c.car));
    const compiled = [];
    for (let c = result.cdr.car; c !== null; c = c.cdr) compiled.push(c.car.name);
    return { seed, forms, compiled };
  };
}

/**
 * The three runs' environments.
 * @returns {{reference: Object, compiled: Object, tiered: Object}} The
 *   interpreter and environment each runs in.
 */
export function createTiers() {
  const reference = interpretedLibrary();
  const compiled = interpretedLibrary();
  installStandardLibrary(compiled.env);
  const tiered = interpretedLibrary();
  installStandardLibrary(tiered.env);
  attachTier(tiered.interpreter, tiered.env);
  return { reference, compiled, tiered };
}

/**
 * Runs a program with the compiler tier deciding what to compile.
 * @param {{interpreter: Object, env: Object}} pair - The tiered run.
 * @param {Array<string>} forms - The program's forms, as text.
 * @returns {{answer: string, compiled: number}} The driver's value written
 *   out, or `error:` and the message; and how many names the tier compiled
 *   while it ran.
 */
export function runTiered({ interpreter, env }, forms) {
  const outcomes = interpreter.tier.outcomes;
  outcomes.clear();
  const compiled = () => [...outcomes.values()].filter((o) => o === 'compiled').length;
  try {
    let value;
    for (const text of forms) {
      value = settle(interpreter.runTopLevel(analyze(parse(text)[0]), env, { jsAutoConvert: 'raw' }));
    }
    return { answer: writeString(value), compiled: compiled() };
  } catch (e) {
    return { answer: `error: ${e.message}`, compiled: compiled() };
  }
}

/**
 * Runs a program in one tier.
 * @param {{interpreter: Object, env: Object}} pair - The tier.
 * @param {Array<string>} forms - The program's forms, as text.
 * @param {Array<string>} toCompile - The procedures to compile; null for the
 *   reference, which compiles nothing. Otherwise top-level expressions are
 *   compiled too, where the tier accepts them.
 * @returns {{answer: string, compiled: number}} The driver's value written
 *   out, or `error:` and the message; and how many procedures compiled.
 */
export function runProgram({ interpreter, env }, forms, toCompile) {
  const wanted = new Set(toCompile ?? []);
  let compiled = 0;
  try {
    let value;
    for (const text of forms) {
      const ast = analyze(parse(text)[0]);
      if (ast instanceof DefineNode && wanted.has(ast.originalName || ast.name)) {
        const result = tryCompileDefinition(ast, env, { allowCaptures: true });
        if (result.compiled) {
          env.define(result.name, result.procedure);
          compiled++;
          continue;
        }
      } else if (toCompile !== null && !(ast instanceof DefineNode)) {
        const result = tryCompileExpression(ast, env, { allowCaptures: true });
        if (result.compiled) {
          value = runCompiledThunk(interpreter, env, result.procedure);
          compiled++;
          continue;
        }
      }
      value = settle(interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' }));
    }
    return { answer: writeString(value), compiled };
  } catch (e) {
    return { answer: `error: ${e.message}`, compiled };
  }
}

/**
 * Runs one generated program in all three ways.
 * @param {Object} tiers - From `createTiers`.
 * @param {Object} program - From the generator.
 * @returns {{agree: boolean, reference: string, compiled: string, tiered: string,
 *   compiledCount: number, tieredCount: number, ms: number}}
 */
export function runBoth(tiers, program) {
  const start = Date.now();
  const reference = runProgram(tiers.reference, program.forms, null);
  const compiled = runProgram(tiers.compiled, program.forms, program.compiled);
  const tiered = runTiered(tiers.tiered, program.forms);
  return {
    agree: reference.answer === compiled.answer && reference.answer === tiered.answer,
    reference: reference.answer,
    compiled: compiled.answer,
    tiered: tiered.answer,
    compiledCount: compiled.compiled,
    tieredCount: tiered.compiled,
    ms: Date.now() - start
  };
}
