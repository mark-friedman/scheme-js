/**
 * @fileoverview The door into the expander, `(scheme-js expander)`
 * (src/core/scheme/expander.scm), which turns a form into a core form; the
 * assembler (assembler.js) turns that into the evaluator's nodes.
 *
 * The expander is one of the system's own libraries, loaded beside the
 * library system, on its interpreter (`systemLibrary` in library_seed.js).
 * While the JavaScript analyzer (analyzer.js) is still here, a form analyzed
 * where a program's or library's top level is -- by `analyze` in analyzer.js,
 * which every caller uses -- is analyzed by the one this selects: the analyzer
 * by default, the expander where a Node process has `SCHEME_JS_EXPANDER=scheme`
 * in its environment or `useSchemeExpander` says so, or whatever a test puts
 * in their place (`useTopLevelAnalyzer`). The library system's seed analyzes
 * its own libraries with the analyzer whatever this says, since the expander
 * is one of them.
 */

import { analyze, analyzeInJavaScript, SyntacticEnv } from './analyzer.js';
import { systemLibrary } from './library_seed.js';
import { assemble } from './assembler.js';
import { callSchemeProcedure } from './values.js';
import { Executable } from './stepables_base.js';
import { intern } from './symbol.js';

/**
 * A form expanded by the Scheme expander where a program's top level is, as
 * the evaluator's node.
 * @param {*} form - The form.
 * @returns {Executable}
 */
export function expand(form) {
  if (form instanceof Executable) return form;
  return assemble(callSchemeProcedure(expander('expand'), [form]), analyze);
}

/**
 * A form analyzed by the JavaScript analyzer where a program's top level is.
 * @param {*} form - The form.
 * @param {Object|null} context - The interpreter's context.
 * @returns {Executable}
 */
const inJavaScript = (form, context) => analyzeInJavaScript(form, null, context);

/**
 * What analyzes a form where a program's or library's top level is.
 * @type {function(*, Object|null): Executable}
 */
let topLevelAnalyzer = typeof process !== 'undefined' && process.env?.SCHEME_JS_EXPANDER === 'scheme'
  ? expand : inJavaScript;

/**
 * Analyzes a form where a program's or library's top level is, with the
 * expander in use.
 * @param {*} form - The form.
 * @param {Object|null} context - The interpreter's context.
 * @returns {Executable}
 */
export function analyzeTopLevel(form, context) {
  return topLevelAnalyzer(form, context);
}

/**
 * Selects the expander a top level's forms are expanded with.
 * @param {boolean} on - The Scheme expander if true, else the JavaScript
 *   analyzer.
 */
export function useSchemeExpander(on) {
  topLevelAnalyzer = on ? expand : inJavaScript;
}

/**
 * Whether the Scheme expander expands a top level's forms.
 * @returns {boolean}
 */
export function schemeExpanderInUse() {
  return topLevelAnalyzer === expand;
}

/**
 * Puts a procedure in place of the expander in use, for a test that compares
 * the two: the procedure of a form and a context.
 * @param {function(*, Object|null): Executable} analyzer - The procedure.
 * @returns {function(*, Object|null): Executable} What was in use.
 */
export function useTopLevelAnalyzer(analyzer) {
  const previous = topLevelAnalyzer;
  topLevelAnalyzer = analyzer;
  return previous;
}

/**
 * One of the expander's procedures.
 * @param {string} name - Its name.
 * @returns {Function}
 */
export function expander(name) {
  return systemLibrary(['scheme-js', 'expander']).get(name);
}

/**
 * A form, analyzed by the expander in use inside an environment the
 * evaluator made, so that it sees the environment's renamed locals under the
 * names they were written with: an expression typed in a paused frame.
 * @param {*} form - The form.
 * @param {Environment} env - The environment.
 * @param {Object} context - The interpreter's context, for the analyzer.
 * @returns {Executable}
 */
export function analyzeInEnvironment(form, env, context) {
  if (schemeExpanderInUse()) {
    return assemble(callSchemeProcedure(expander('expand-in-environment'), [form, env]), analyze);
  }
  return analyzeInJavaScript(form, syntacticEnvFor(env), context);
}

/**
 * The analyzer's view of an environment: each scope's renamed locals under
 * the names they were written with.
 * @param {Environment} env - The environment.
 * @returns {SyntacticEnv|null}
 */
function syntacticEnvFor(env) {
  const chain = [];
  for (let scope = env; scope; scope = scope.parent) chain.push(scope);
  let syntacticEnv = null;
  for (const scope of chain.reverse()) {
    syntacticEnv = new SyntacticEnv(syntacticEnv);
    for (const [written, renamed] of scope.nameMap ?? []) {
      syntacticEnv.bindings.push({ id: intern(written), newName: renamed });
    }
  }
  return syntacticEnv;
}
