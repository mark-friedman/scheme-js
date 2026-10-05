/**
 * @fileoverview The prebuilt tables' writer, `(scheme-js table-writer)`, for
 * the build scripts: loads it beside the libraries a build has loaded, and
 * calls it. The writing itself is Scheme, in table_writer.scm.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { setFileResolver, loadLibrarySync } from '../../src/core/interpreter/library_loader.js';
import { getFileResolver, getLibraryEnv } from '../../src/core/interpreter/library_registry.js';
import { compileEnvironment } from '../../src/compiler/index.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { expandToCore } from '../../src/core/interpreter/expand.js';
import { assemble } from '../../src/core/interpreter/assembler.js';
import { callSchemeProcedure, SCHEME_PRIMITIVE } from '../../src/core/interpreter/values.js';
import { Cons, list, toArray } from '../../src/core/interpreter/cons.js';
import { RUNTIME_INTERFACE } from '../../src/compiler/prebuilt.js';

const HERE = path.dirname(fileURLToPath(import.meta.url));

/**
 * Loads the writer into the current registry, which must hold the libraries
 * it imports, `(scheme base)` and SRFI 152, as every build's does: its files
 * are found here, and any other through the resolver already set.
 *
 * @param {Object} interpreter - The build's interpreter.
 * @param {Object} env - Its global environment.
 * @returns {{writable: (constants: Array<*>) => boolean,
 *   restoring: (forms: Array<*>, env: Object, entries: Array<Object>) => Object,
 *   datum: (value: *) => (string|null),
 *   render: (module: Object) => string}} Whether a constant pool can be written
 *   down (`constants-expression`); what restores a library (`restoring`); the
 *   JavaScript that rebuilds a datum, or null if it cannot be written down
 *   (`constant-expression`); and a
 *   module's text (`render-tables`), from `{generator, title, libraries}`,
 *   each library `{key, fingerprint, files, entries, restore, declaration}` and each entry
 *   as `generateEnvironment` gives it, with a `span` if it is restored.
 */
export function tableWriter(interpreter, env) {
  const previous = getFileResolver();
  setFileResolver((name) => {
    const file = name[name.length - 1];
    for (const candidate of [`${file}.sld`, file]) {
      const where = path.join(HERE, candidate);
      if (fs.existsSync(where)) return fs.readFileSync(where, 'utf8');
    }
    return previous(name);
  });
  let exports;
  try {
    exports = loadLibrarySync(['scheme-js', 'table-writer'], analyze, interpreter, env);
  } finally {
    setFileResolver(previous);
  }
  // It writes megabytes, and has no prebuilt table, not being shipped: the
  // build compiles it here.
  compileEnvironment(getLibraryEnv(['scheme-js', 'table-writer']));
  const call = (name, ...args) => callSchemeProcedure(exports.get(name), args);
  const entry = (e) => list(e.name, list(...e.params), e.rest ?? false, list(...e.constants), e.source,
    e.span ?? false);

  /**
   * What restores a library from its table (`restore-sequence`): the forms
   * its loading ran, each a procedure the table restores, the core form the
   * form expanded into, or the form itself, and which procedures those are,
   * each given the span of its source. A procedure is restored when its
   * definition made its final binding: the closure bound now, made in the
   * library's own environment from source inside the form, and compiled in an
   * entry.
   * @param {Array<Cons>} forms - The library's top-level forms, in the order
   *   loading ran them, each `(form . core)` (`notingExpander`).
   * @param {Object} env - The library's environment, before its table is
   *   installed.
   * @param {Array<Object>} entries - The entries its table will hold.
   * @returns {{restore: (Cons|boolean), restored: Set<string>}} The sequence,
   *   or false if a form in it cannot be written down; and the names restored.
   */
  const restoring = (forms, env, entries) => {
    const byName = new Map(entries.map((e) => [e.name, e]));
    const madeFinal = (name, form) => {
      const closure = env.bindings.get(name.name);
      const entry = byName.get(name.name);
      return entry !== undefined && entry.closure === closure && closure.env === env
        && within(closure.source, form.source);
    };
    madeFinal[SCHEME_PRIMITIVE] = true;
    const restore = call('restore-sequence', list(...forms), madeFinal);
    const restored = new Set(toArray(restore).filter((item) => item.car.name === 'procedure')
      .map((item) => item.cdr.car.name));
    for (const name of restored) byName.get(name).span = JSON.stringify(byName.get(name).closure.source);
    return { restore: call('restore-writable?', restore) ? restore : false, restored };
  };

  return {
    writable: (constants) => call('constants-expression', list(...constants)) !== false,
    restoring,
    datum: (value) => {
      const text = call('constant-expression', value);
      return text === false ? null : String(text);
    },
    json: (value) => {
      const text = call('json-datum', value);
      return text === false ? null : String(text);
    },
    render: ({ generator, title, libraries }) => String(call('render-tables', generator, title, RUNTIME_INTERFACE,
      list(...libraries.map((l) => list(l.key, l.fingerprint, list(...l.files), list(...l.entries.map(entry)),
        // A restore sequence of nothing, a library of re-exports, is the
        // empty list, JavaScript's null; false is one that cannot restore.
        l.restore === undefined ? false : l.restore, l.declaration ?? false)))))
  };
}

/**
 * Whether a span of source lies within another.
 * @param {Object|undefined} inner - The inner span: `{filename, line, column,
 *   endLine, endColumn}`.
 * @param {Object|undefined} outer - The outer span.
 * @returns {boolean}
 */
function within(inner, outer) {
  if (!inner || !outer || inner.filename !== outer.filename) return false;
  const before = (l1, c1, l2, c2) => l1 < l2 || (l1 === l2 && c1 <= c2);
  return before(outer.line, outer.column, inner.line, inner.column)
    && before(inner.endLine, inner.endColumn, outer.endLine, outer.endColumn);
}

/**
 * An `analyze` that expands each form it is given and notes it with its core
 * form, for the libraries' loading: the evaluator analyzes each top-level form
 * of a library's body with it, all of one library's after the libraries it
 * imports have loaded, and before its load hook runs.
 * @returns {{analyze: Function, take: () => Array<Cons>}} The noting
 *   analyzer, and the forms noted since last asked, each `(form . core)`,
 *   which it forgets.
 */
export function notingExpander() {
  let noted = [];
  return {
    analyze: (form) => {
      const core = expandToCore(form);
      noted.push(new Cons(form, core));
      return assemble(core, analyze);
    },
    take: () => { const forms = noted; noted = []; return forms; }
  };
}
