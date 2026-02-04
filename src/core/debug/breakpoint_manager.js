/**
 * @fileoverview BreakpointManager for the Scheme debugger.
 * 
 * Manages setting, removing, and querying breakpoints with support for
 * line-level and column-level precision.
 */

/**
 * Manages breakpoints for the debug runtime.
 */
export class BreakpointManager {
    constructor() {
        /** @type {Map<string, {filename: string, line: number, column: number|null, id: string}>} */
        this.breakpoints = new Map();
        this.nextId = 1;
    }

    /**
     * Sets a breakpoint at the specified location.
     * @param {string} filename - Source file path
     * @param {number} line - Line number (1-indexed)
     * @param {number} [column] - Optional column number (1-indexed)
     * @returns {string} Unique breakpoint ID
     */
    setBreakpoint(filename, line, column = null) {
        const id = `bp-${this.nextId++}`;
        this.breakpoints.set(id, { filename, line, column, id });
        return id;
    }

    /**
     * Removes a breakpoint by ID.
     * @param {string} id - Breakpoint ID
     * @returns {boolean} True if removed, false if not found
     */
    removeBreakpoint(id) {
        return this.breakpoints.delete(id);
    }

    /**
     * Checks if there's a breakpoint at the given source location.
     * @param {Object} source - Source location info with filename, line, column
     * @returns {boolean} True if a breakpoint matches this location
     */
    hasBreakpoint(source) {
        if (!source) return false;
        for (const bp of this.breakpoints.values()) {
            if (bp.filename === source.filename && bp.line === source.line) {
                // If column specified on breakpoint, require exact match
                if (bp.column !== null) {
                    if (source.column === bp.column) return true;
                } else {
                    // Line-level breakpoint matches any column
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Gets all active breakpoints.
     * @returns {Array<{filename: string, line: number, column: number|null, id: string}>}
     */
    getAllBreakpoints() {
        return Array.from(this.breakpoints.values());
    }

    /**
     * Gets a breakpoint by ID.
     * @param {string} id - Breakpoint ID
     * @returns {{filename: string, line: number, column: number|null, id: string}|null}
     */
    getBreakpoint(id) {
        return this.breakpoints.get(id) || null;
    }

    /**
     * Clears all breakpoints.
     */
    clearAll() {
        this.breakpoints.clear();
    }
}
