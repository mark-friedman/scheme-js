/**
 * @fileoverview BreakpointManager for the Scheme debugger.
 *
 * Manages setting, removing, and querying breakpoints with support for
 * line-level and column-level precision, conditional breakpoints, and
 * hit count breakpoints. Uses an O(1) location index for fast lookups.
 */

/**
 * @typedef {Object} Breakpoint
 * @property {string} id - Unique breakpoint identifier
 * @property {string} filename - Source file path
 * @property {number} line - Line number (1-indexed)
 * @property {number|null} column - Column number (1-indexed), or null for line-level
 * @property {string|null} condition - Condition expression string, or null
 * @property {number|null} hitCount - Required hit count before breaking, or null
 * @property {number} currentHitCount - Current number of hits
 * @property {boolean} enabled - Whether the breakpoint is active
 */

/**
 * Manages breakpoints for the debug runtime.
 *
 * Provides O(1) lookup by location via an internal index keyed by
 * "filename:line". The primary breakpoint store is a Map keyed by
 * breakpoint ID.
 */
export class BreakpointManager {
  constructor() {
    /** @type {Map<string, Breakpoint>} */
    this.breakpoints = new Map();

    /**
     * Location index for O(1) lookups.
     * Key: "filename:line", Value: Set of breakpoint IDs at that location.
     * @type {Map<string, Set<string>>}
     */
    this.locationIndex = new Map();

    /** @type {number} */
    this.nextId = 1;
  }

  /**
   * Generates a location key for the index.
   * @param {string} filename - Source file path
   * @param {number} line - Line number
   * @returns {string} Location key
   */
  _locationKey(filename, line) {
    return `${filename}:${line}`;
  }

  /**
   * Sets a breakpoint at the specified location.
   * @param {string} filename - Source file path
   * @param {number} line - Line number (1-indexed)
   * @param {number|null} [column=null] - Optional column number (1-indexed)
   * @param {Object} [options={}] - Optional breakpoint options
   * @param {string|null} [options.condition=null] - Condition expression string
   * @param {number|null} [options.hitCount=null] - Hit count threshold
   * @returns {string} Unique breakpoint ID
   */
  setBreakpoint(filename, line, column = null, options = {}) {
    const id = `bp-${this.nextId++}`;
    const condition = options.condition ?? null;
    const hitCount = options.hitCount ?? null;

    /** @type {Breakpoint} */
    const bp = {
      id,
      filename,
      line,
      column,
      condition,
      hitCount,
      currentHitCount: 0,
      enabled: true,
    };

    this.breakpoints.set(id, bp);

    // Update location index
    const key = this._locationKey(filename, line);
    let ids = this.locationIndex.get(key);
    if (!ids) {
      ids = new Set();
      this.locationIndex.set(key, ids);
    }
    ids.add(id);

    return id;
  }

  /**
   * Removes a breakpoint by ID.
   * @param {string} id - Breakpoint ID
   * @returns {boolean} True if removed, false if not found
   */
  removeBreakpoint(id) {
    const bp = this.breakpoints.get(id);
    if (!bp) return false;

    // Remove from location index
    const key = this._locationKey(bp.filename, bp.line);
    const ids = this.locationIndex.get(key);
    if (ids) {
      ids.delete(id);
      if (ids.size === 0) {
        this.locationIndex.delete(key);
      }
    }

    return this.breakpoints.delete(id);
  }

  /**
   * Checks if there's a breakpoint at the given source location.
   * Uses the O(1) location index for fast lookup.
   * @param {Object} source - Source location info with filename, line, column
   * @returns {boolean} True if a breakpoint matches this location
   */
  hasBreakpoint(source) {
    if (!source) return false;

    const key = this._locationKey(source.filename, source.line);
    const ids = this.locationIndex.get(key);
    if (!ids || ids.size === 0) return false;

    // Check column-level precision for each breakpoint at this location
    for (const id of ids) {
      const bp = this.breakpoints.get(id);
      if (bp.column !== null) {
        if (source.column === bp.column) return true;
      } else {
        // Line-level breakpoint matches any column
        return true;
      }
    }
    return false;
  }

  /**
   * Returns whether any breakpoints are currently set.
   * Useful as a fast-path check to skip breakpoint evaluation entirely.
   * @returns {boolean} True if at least one breakpoint exists
   */
  hasAny() {
    return this.breakpoints.size > 0;
  }

  /**
   * Gets all active breakpoints.
   * @returns {Array<Breakpoint>}
   */
  getAllBreakpoints() {
    return Array.from(this.breakpoints.values());
  }

  /**
   * Gets a breakpoint by ID.
   * @param {string} id - Breakpoint ID
   * @returns {Breakpoint|null}
   */
  getBreakpoint(id) {
    return this.breakpoints.get(id) || null;
  }

  /**
   * Clears all breakpoints and the location index.
   */
  clearAll() {
    this.breakpoints.clear();
    this.locationIndex.clear();
  }
}
