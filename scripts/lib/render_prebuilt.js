/**
 * @fileoverview Rendering a table of compiled procedures as a module.
 *
 * Two build steps write one of these -- the standard library's and the
 * compiler's own -- and `installPrebuilt` reads both. One renderer rather than
 * two keeps the shape it reads and the shape they write the same thing.
 */

/**
 * Renders a procedure's constant pool as a JavaScript expression, or reports
 * that it cannot be.
 *
 * ## Why a pool exists at all
 *
 * The emitter writes immediates straight into the generated source and interns
 * everything else -- symbols, pairs, vectors -- because those have identity
 * that `eq?` can observe, so re-creating one per evaluation would be wrong as
 * well as slow. The pool is built once and handed to the procedure's factory.
 *
 * ## What can be written down
 *
 * A symbol survives being written down exactly: `intern("lambda")` read back
 * yields *the same object*, so the identity the pool exists to preserve is
 * preserved by reconstructing it.
 *
 * A pair, a character, and the immediates and strings inside them are rebuilt as new
 * objects, once, when the module loads, and that object is then the constant
 * every call of the procedure sees -- which is all a literal promises. Two
 * evaluations of `'(a b)` in one procedure give the same object, compiled or
 * interpreted. What reconstruction cannot preserve is identity with the
 * *interpreted* procedure's literal, and nothing could observe that unless the
 * literal escaped before the compiled version was installed. The compiler's
 * own Scheme is what needs these: `case` compiles to a test against a quoted
 * list, and the emitter compares characters.
 *
 * Anything else -- a vector, a record -- still leaves the procedure
 * out, interpreted, which is correct rather than merely safe.
 *
 * @param {Array<*>} constants - A procedure's constant pool.
 * @returns {string|null} A JavaScript array expression, or null if some value
 *   in the pool cannot be written down.
 */
export function serializeConstants(constants) {
  const parts = constants.map(serializeValue);
  return parts.includes(null) ? null : `[${parts.join(', ')}]`;
}

/**
 * Renders one constant as a JavaScript expression that rebuilds it.
 * @param {*} value - The constant.
 * @returns {string|null} The expression, or null if it cannot be written down.
 */
function serializeValue(value) {
  if (value === null) return 'null';
  if (value === true || value === false) return String(value);
  if (typeof value === 'bigint') return `${value}n`;
  if (typeof value === 'number' && Number.isFinite(value)) return String(value);
  if (typeof value === 'string') return JSON.stringify(value);
  if (typeof value !== 'object' || !value.constructor) return null;
  switch (value.constructor.name) {
    case 'Symbol':
      return typeof value.name === 'string' ? `intern(${JSON.stringify(value.name)})` : null;
    case 'Char':
      return `new Char(${value.codePoint})`;
    case 'Cons': {
      const car = serializeValue(value.car);
      const cdr = serializeValue(value.cdr);
      return car === null || cdr === null ? null : `new Cons(${car}, ${cdr})`;
    }
    default:
      return null;
  }
}

/**
 * Renders a generated prebuilt table as JavaScript module text.
 *
 * @param {Object} table - What to render.
 * @param {string} table.generator - The script writing it, for the banner.
 * @param {string} table.title - One line saying what the table holds.
 * @param {string} table.fingerprint - Fingerprint of the sources compiled from.
 * @param {string} table.filesName - Name to export the file list under.
 * @param {string[]} table.files - The sources, in the order the fingerprint
 *   covers them.
 * @param {Array<Object>} table.entries - Entries from `generateEnvironment`.
 * @returns {string} JavaScript module text.
 */
export function renderTable(table) {
  const entries = table.entries.map((entry) => {
    // The generated source is a function *body*: it declares the procedure,
    // marks it, and returns it. Wrapping it in an arrow makes it a value the
    // module can export, with no `new Function` anywhere.
    const body = entry.source.split('\n').map((line) => '    ' + line).join('\n');
    return `  ${JSON.stringify(entry.name)}: {\n`
      + `    params: ${JSON.stringify(entry.params)},\n`
      + `    rest: ${JSON.stringify(entry.rest ?? null)},\n`
      + `    constants: ${serializeConstants(entry.constants)},\n`
      + `    make: (R, E, K) => {\n${body}\n    }\n`
      + `  }`;
  });

  // Each constructor is imported only when some constant needs it, so a table
  // of procedures with no pooled constants stays a module with no
  // dependencies.
  const pools = table.entries.map((entry) => serializeConstants(entry.constants)).join('');
  const imports = [
    pools.includes('intern(') ? "import { intern } from '../core/interpreter/symbol.js';" : null,
    pools.includes('new Cons(') ? "import { Cons } from '../core/interpreter/cons.js';" : null,
    pools.includes('new Char(') ? "import { Char } from '../core/primitives/char_class.js';" : null
  ].filter(Boolean);

  return `// Auto-generated by ${table.generator} - do not edit manually
//
// ${table.title}
//
// Each entry holds the JavaScript the compiler would otherwise generate at
// startup, as a factory taking the runtime, the environment its globals resolve
// in, and its constant pool.
//
// \`FINGERPRINT\` is of the sources this was generated from. \`installPrebuilt\` in
// src/compiler/prebuilt.js recomputes it and installs nothing if it differs, so
// a stale build leaves those procedures interpreted rather than running code
// for source that has since changed.
${imports.length > 0 ? `\n${imports.join('\n')}\n` : ''}
/** @type {string} Fingerprint of the sources this was generated from. */
export const FINGERPRINT = ${JSON.stringify(table.fingerprint)};

/** @type {string[]} The sources, in the order the fingerprint covers them. */
export const ${table.filesName} = ${JSON.stringify(table.files, null, 2)};

/** @type {Object<string, {params: string[], rest: (string|null), constants: Array<*>, make: Function}>} */
export const PROCEDURES = {
${entries.join(',\n')}
};

export default { fingerprint: FINGERPRINT, files: ${table.filesName}, procedures: PROCEDURES };
`;
}
