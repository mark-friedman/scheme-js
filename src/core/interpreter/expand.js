/**
 * @fileoverview The door into the expander, `(scheme-js expander)`
 * (src/core/scheme/expander.scm), which turns a form into a core form; the
 * assembler (assembler.js) turns that into the evaluator's nodes. The
 * expander is one of the library system's seed libraries, loaded on its
 * interpreter (`systemLibrary` in library_seed.js).
 */

import { systemLibrary } from './library_seed.js';
import { assemble } from './assembler.js';
import { callSchemeProcedure } from './values.js';
import { Executable } from './stepables_base.js';

/**
 * One of the expander's procedures.
 * @param {string} name - Its name.
 * @returns {Function}
 */
export function expander(name) {
  return systemLibrary(['scheme-js', 'expander']).get(name);
}

/**
 * A form, expanded where a program's or library's top level is -- in the
 * library or program whose scope is the one being defined in, if any -- as
 * the evaluator's node. A node is itself.
 * @param {*} form - The form.
 * @returns {Executable}
 */
export function analyze(form) {
  if (form instanceof Executable) return form;
  return assemble(expandToCore(form), analyze);
}

/**
 * A form, expanded where a program's or library's top level is, as its core
 * form.
 * @param {*} form - The form.
 * @returns {*}
 */
export function expandToCore(form) {
  return callSchemeProcedure(expander('expand'), [form]);
}

/**
 * A form, expanded inside an environment the evaluator made, so that it sees
 * the environment's renamed locals under the names they were written with: an
 * expression typed in a paused frame.
 * @param {*} form - The form.
 * @param {Environment} env - The environment.
 * @returns {Executable}
 */
export function analyzeInEnvironment(form, env) {
  return assemble(callSchemeProcedure(expander('expand-in-environment'), [form, env]), analyze);
}
