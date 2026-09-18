/**
 * The resumable half of convention B.
 *
 * Convention B's fast path is straight-line JavaScript: ordinary calls, one
 * identity check after each. That code cannot be re-entered part-way through,
 * so a second copy of each procedure is emitted as a state machine over its
 * call sites, used only while a captured continuation is being reinstated.
 *
 * This is Marshall's arrangement, where the pending computation of a frame
 * lives in a separate `Continue` method rather than in the procedure itself. It
 * costs code size, which is paid once at compile time, instead of costing the
 * normal path, which is paid on every call.
 */

import { BlockEmitter } from './backend_a.js';

/**
 * Emits the resumable form of a procedure.
 * @param {string} name - Identifier for the resumable function.
 * @param {Object} lambda - The lambda node.
 * @param {Object} ctx - Emitter state.
 * @returns {string} JavaScript source.
 */
export function emitResumable(name, lambda, ctx) {
  const emitter = new BlockEmitter(name, lambda, ctx);
  emitter.mode = 'resume';
  return emitter.emit();
}
