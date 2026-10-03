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
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { list } from '../../src/core/interpreter/cons.js';
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
 *   render: (module: Object) => string}} Whether a constant pool can be written
 *   down (`constants-expression`); and a module's text (`render-tables`), from
 *   `{generator, title, libraries}`, each library `{key, fingerprint, files,
 *   entries}` and each entry as `generateEnvironment` gives it.
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
  const entry = (e) => list(e.name, list(...e.params), e.rest ?? false, list(...e.constants), e.source);
  return {
    writable: (constants) => call('constants-expression', list(...constants)) !== false,
    render: ({ generator, title, libraries }) => String(call('render-tables', generator, title, RUNTIME_INTERFACE,
      list(...libraries.map((l) => list(l.key, l.fingerprint, list(...l.files), list(...l.entries.map(entry)))))))
  };
}
