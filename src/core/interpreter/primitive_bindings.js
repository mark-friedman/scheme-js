/**
 * @fileoverview Whether a primitive's name is still bound to that primitive,
 * everywhere.
 *
 * Scheme lets a program redefine `car`. Compiled code expands `car` inline and
 * so has to know, every time it runs the expansion, that the name it reads
 * still denotes the primitive. Asking the environment on every use is a hash
 * lookup per `car` -- measured, removing those lookups outright made the
 * `call` and `fixnum` benchmark classes nearly twice as fast -- and caching the
 * answer inside a procedure only lasts until its next call, which in recursive
 * code is the next expression.
 *
 * So the question is turned around. Rather than compiled code asking whether
 * `car` has changed, every binding write reports whether it rebinds a
 * primitive's name to something else, and the answer is kept here, one cell
 * per name, for compiled code to read with a single property load. A cell only
 * ever goes from intact to not: once `car` has been rebound anywhere, code that
 * inlined it goes back to checking the binding on each use, which is exactly
 * what it did before.
 *
 * The flag is deliberately coarse. It is per name, not per environment, so a
 * library defining its own `car` for its own use turns off the shortcut for
 * `car` in every environment -- slower there, never wrong. Being coarse is what
 * lets it be sound without knowing which environments a piece of compiled code
 * might resolve its globals in.
 *
 * Which writes report: `Environment.define` and `Environment.set`, which are
 * how programs, imports and the REPL bind and assign. Three other paths write a
 * frame's bindings directly and need not. The interpreter's `letrec` frames
 * bind only renamed locals, which can never be a primitive's name.
 * `substituteLibraryValues` only replaces an interpreted closure with its
 * compiled form, so the name it writes was already bound to something other
 * than a primitive, and that binding was reported when it was made. And
 * installing the primitives themselves registers each one here instead.
 *
 * This module is the interpreter's, because the writes it has to see are the
 * interpreter's; the compiler only reads it.
 */

/**
 * @typedef {Object} PrimitiveCell
 * @property {boolean} intact - Whether every binding of the name ever made has
 *   been to `primitive`.
 * @property {Function|null} primitive - The primitive the name was installed
 *   with.
 */

/** @type {Map<string, PrimitiveCell>} One cell per primitive's name. */
const cells = new Map();

/**
 * Stands in for a name no primitive was ever installed under, so compiled code
 * can read a cell unconditionally. It is never intact.
 * @type {PrimitiveCell}
 */
const NO_PRIMITIVE = Object.freeze({ intact: false, primitive: null });

/**
 * Records that a primitive is being installed under a name.
 *
 * Every interpreter installs the same module-level functions, so installing a
 * second time is normally a no-op. A name installed with a *different*
 * function -- a primitive built per interpreter -- has no single primitive to
 * compare against, so its cell is marked not intact.
 *
 * @param {string} name - The name.
 * @param {Function} primitive - The primitive.
 * @returns {void}
 */
export function registerPrimitive(name, primitive) {
  const cell = cells.get(name);
  if (cell === undefined) {
    cells.set(name, { intact: true, primitive });
  } else if (cell.primitive !== primitive) {
    cell.intact = false;
  }
}

/**
 * Records a binding write. Every path that binds or assigns a name in an
 * environment frame calls this, so that a primitive's name rebound to anything
 * else is noticed wherever it happens.
 *
 * @param {string} name - The name written.
 * @param {*} value - The value it is now bound to.
 * @returns {void}
 */
export function noteBinding(name, value) {
  const cell = cells.get(name);
  if (cell !== undefined && value !== cell.primitive) cell.intact = false;
}

/**
 * The cell for a primitive's name, for compiled code to read.
 * @param {string} name - The name.
 * @returns {PrimitiveCell} Its cell, or one that is never intact if no
 *   primitive was installed under it.
 */
export function primitiveCell(name) {
  return cells.get(name) ?? NO_PRIMITIVE;
}
