/**
 * @fileoverview Evaluator instrumentation.
 *
 * Counts what the evaluator actually does, as opposed to how long it takes.
 * Timings answer "is it faster?"; counts answer "did it stop doing the work?",
 * which is the question that matters when attributing an optimization. Counts
 * are deterministic, machine-independent and immune to JIT warm-up noise, so a
 * regression in step count is a real regression even when the wall clock is
 * ambiguous.
 *
 * Instrumentation is attached by wrapping the interpreter rather than by adding
 * checks inside it. The evaluator's dispatch loop is the hottest code in the
 * system, and a permanent `if (instrumenting)` branch there would be a cost
 * paid by every program forever in exchange for a facility used only by
 * benchmarks. Wrapping costs exactly nothing when nobody is measuring.
 */

import { CTL, FSTACK } from '../core/interpreter/stepables_base.js';

/**
 * @typedef {Object} InstrumentationStats
 * @property {number} totalSteps - Total evaluator dispatches.
 * @property {Object<string, number>} stepsByType - Dispatches per AST node or
 *   frame class name, descending by count.
 * @property {number} maxStackDepth - Deepest the continuation frame stack got.
 */

/**
 * @typedef {Object} InstrumentationHandle
 * @property {function(): InstrumentationStats} stop - Detaches instrumentation
 *   and returns the collected statistics. Idempotent.
 * @property {function(): InstrumentationStats} sample - Returns statistics so
 *   far without detaching.
 */

/**
 * Attaches step counting to an interpreter.
 *
 * The interpreter's `step` method is replaced for the lifetime of the handle
 * and restored by `stop()`. Nested instrumentation of the same interpreter is
 * supported, since each handle restores whatever it displaced.
 *
 * @param {Object} interpreter - The interpreter to instrument.
 * @returns {InstrumentationHandle} A handle used to collect results and detach.
 */
export function instrumentInterpreter(interpreter) {
  const stepsByType = new Map();
  let totalSteps = 0;
  let maxStackDepth = 0;
  let stopped = false;

  const original = interpreter.step;
  // `step` normally lives on the prototype. Restoring by plain assignment would
  // leave an own property shadowing it forever, so remember which it was.
  const hadOwnStep = Object.prototype.hasOwnProperty.call(interpreter, 'step');

  /**
   * Counting wrapper around the evaluator's dispatch.
   * @param {Array} registers - The interpreter register array.
   * @returns {boolean} Whatever the underlying step returned.
   */
  const counting = function (registers) {
    const control = registers[CTL];
    // Frames and AST nodes are plain class instances; anonymous or primitive
    // control values are bucketed rather than dropped so totals always agree.
    const name = (control && control.constructor && control.constructor.name)
      ? control.constructor.name
      : '(unknown)';

    stepsByType.set(name, (stepsByType.get(name) || 0) + 1);
    totalSteps++;

    const stack = registers[FSTACK];
    if (stack && stack.length > maxStackDepth) {
      maxStackDepth = stack.length;
    }

    return original.call(this, registers);
  };

  interpreter.step = counting;

  /**
   * Builds a snapshot of the statistics collected so far.
   * @returns {InstrumentationStats} The statistics.
   */
  const snapshot = () => {
    const sorted = [...stepsByType.entries()].sort((a, b) => b[1] - a[1]);
    return {
      totalSteps,
      stepsByType: Object.fromEntries(sorted),
      maxStackDepth
    };
  };

  return {
    sample: snapshot,
    stop() {
      if (!stopped) {
        stopped = true;
        // Restore only if nobody else has wrapped us since; otherwise leave the
        // outer wrapper in place so its own stop() can unwind correctly.
        if (interpreter.step === counting) {
          if (hadOwnStep) {
            interpreter.step = original;
          } else {
            delete interpreter.step;
          }
        }
      }
      return snapshot();
    }
  };
}

/**
 * Formats instrumentation statistics as a readable table.
 * @param {InstrumentationStats} stats - Statistics from a handle.
 * @param {number} [top=20] - How many node types to list.
 * @returns {string} A printable report.
 */
export function formatStats(stats, top = 20) {
  const lines = [];
  lines.push(`total steps: ${stats.totalSteps.toLocaleString()}`);
  lines.push(`max frame stack depth: ${stats.maxStackDepth.toLocaleString()}`);
  lines.push('');
  lines.push('  share |        steps | node / frame type');
  lines.push('--------|--------------|------------------------------------');

  const entries = Object.entries(stats.stepsByType).slice(0, top);
  for (const [name, count] of entries) {
    const share = stats.totalSteps > 0 ? (100 * count / stats.totalSteps) : 0;
    lines.push(
      `${share.toFixed(1).padStart(6)}% | ${count.toLocaleString().padStart(12)} | ${name}`
    );
  }
  return lines.join('\n');
}
