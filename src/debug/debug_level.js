/**
 * @fileoverview DebugLevel and DebugLevelStack for nested debugger contexts.
 *
 * Each DebugLevel represents a paused execution context (e.g., breakpoint,
 * step, exception, manual pause). The DebugLevelStack manages nested levels
 * for recursive debugger entry (e.g., eval during breakpoint hits error).
 */

/**
 * Represents a single debugger pause context.
 * Each debug level captures the state at the point of pause.
 */
export class DebugLevel {
  /**
   * @param {number} level - 0-based nesting depth
   * @param {string} reason - 'breakpoint', 'step', 'exception', 'manual'
   * @param {Object|null} source - Source location where paused
   * @param {Array} stack - Stack snapshot at pause point
   * @param {Object} env - Environment at pause point
   * @param {DebugLevel|null} [parentLevel=null] - Parent level (null for top-level)
   */
  constructor(level, reason, source, stack, env, parentLevel = null) {
    /** @type {number} */
    this.level = level;
    /** @type {string} */
    this.reason = reason;
    /** @type {Object|null} */
    this.source = source;
    /** @type {Array} */
    this.stack = stack;
    /** @type {Object} */
    this.env = env;
    /** @type {DebugLevel|null} */
    this.parentLevel = parentLevel;
    /** @type {number} */
    this.selectedFrameIndex = 0;
  }
}

/**
 * Manages a stack of nested debug levels.
 * Supports push/pop/popAll for entering and exiting debug contexts.
 */
export class DebugLevelStack {
  constructor() {
    /** @type {DebugLevel[]} */
    this.levels = [];
  }

  /**
   * Pushes a new debug level onto the stack.
   * @param {DebugLevel} level
   */
  push(level) {
    this.levels.push(level);
  }

  /**
   * Pops the topmost debug level (used by :abort).
   * @returns {DebugLevel|null}
   */
  pop() {
    if (this.levels.length === 0) return null;
    return this.levels.pop();
  }

  /**
   * Pops all debug levels (used by :toplevel).
   * @returns {DebugLevel[]}
   */
  popAll() {
    const popped = [...this.levels];
    this.levels = [];
    return popped;
  }

  /**
   * Returns the current (topmost) debug level.
   * @returns {DebugLevel|null}
   */
  current() {
    if (this.levels.length === 0) return null;
    return this.levels[this.levels.length - 1];
  }

  /**
   * Returns the current nesting depth (0 = not debugging).
   * @returns {number}
   */
  depth() {
    return this.levels.length;
  }
}
