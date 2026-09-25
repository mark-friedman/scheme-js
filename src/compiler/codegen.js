/**
 * @fileoverview Code generation, which is Scheme: the door into `emit.scm`.
 *
 * The emitter -- both forms of every procedure, lambda lifting, liveness for
 * frame spills, inline expansions -- is written in Scheme (`emit.scm`,
 * `lift.scm`, `liveness.scm`, `inline.scm`) and runs in the same interpreter as
 * the lowering, reached through `lowering.js`. What stays here is the one
 * question the emitter cannot answer from the IR: which globals the
 * environment being compiled for still binds to their primitives, since only
 * those may be expanded inline.
 *
 * Non-tail calls are ordinary JavaScript calls, so one live Scheme frame is one
 * live JavaScript frame and a debugger can see the Scheme call stack; a tail
 * call is made directly while the stack direct tail calls hold is inside a
 * budget, and otherwise returns a `TailCall` through a trampoline, so tail
 * recursion runs in bounded space. `emit.scm`'s header has the rest.
 */

import { currentBinding, primitiveCell } from './runtime.js';
import { emitUnit, inlineExpansionNames } from './lowering.js';

/**
 * Generates the JavaScript for a lowered procedure.
 *
 * @param {*} schemeIr - The procedure's IR, as `lower-lambda` returned it.
 * @param {Set<string>} globals - The globals it references.
 * @param {string} name - A display name for the generated function.
 * @param {Object} [env] - The environment it will run in. Without one nothing
 *   is expanded inline.
 * @returns {{source: string, constants: Array<*>, globals: Array<string>}} The
 *   generated source -- the body of a function of the runtime `R`, the
 *   environment `E` and the constant pool `K` -- its constants, and the globals
 *   it binds.
 */
export function generate(schemeIr, globals, name, env) {
  const globalNames = [...globals];
  // Expanded inline only while bound to the primitive the expansion
  // reproduces: a program that has already redefined `car` must not have its
  // `car` compiled as the primitive's.
  const expandable = new Set(inlineExpansionNames());
  const guarded = env
    ? globalNames.filter((g) => {
      if (!expandable.has(g)) return false;
      const cell = primitiveCell(g);
      return cell.primitive !== null && currentBinding(env, g) === cell.primitive;
    })
    : [];
  const { source, constants } = emitUnit(schemeIr, globalNames, name, guarded);
  return { source, constants, globals: globalNames };
}
