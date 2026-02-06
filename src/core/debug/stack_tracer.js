/**
 * @fileoverview StackTracer for the Scheme debugger.
 * 
 * Tracks call stack with TCO (Tail Call Optimization) awareness.
 * Maintains frame information for stack inspection during debugging.
 */

/**
 * @typedef {Object} StackFrame
 * @property {string} name - Function/procedure name
 * @property {Object|null} source - Source location info
 * @property {Object} env - Environment at this frame
 * @property {number} tcoCount - Number of TCO replacements at this frame
 */

/**
 * Tracks call stack for debugging purposes.
 * TCO-aware: can replace frames for tail calls instead of pushing.
 */
export class StackTracer {
    constructor() {
        /** @type {StackFrame[]} */
        this.frames = [];
    }

    /**
     * Enters a new call frame.
     * @param {Object} frameInfo - Frame information
     * @param {string} frameInfo.name - Function name
     * @param {Object|null} frameInfo.source - Source location
     * @param {Object} frameInfo.env - Environment
     */
    enterFrame(frameInfo) {
        this.frames.push({
            name: frameInfo.name,
            source: frameInfo.source,
            env: frameInfo.env,
            tcoCount: 0
        });
    }

    /**
     * Exits the current call frame.
     */
    exitFrame() {
        if (this.frames.length > 0) {
            this.frames.pop();
        }
    }

    /**
     * Replaces the current frame (for tail call optimization).
     * Instead of pushing a new frame, replaces the current one.
     * @param {Object} frameInfo - New frame information
     */
    replaceFrame(frameInfo) {
        if (this.frames.length > 0) {
            const current = this.frames[this.frames.length - 1];
            const tcoCount = current.tcoCount + 1;
            this.frames[this.frames.length - 1] = {
                name: frameInfo.name,
                source: frameInfo.source,
                env: frameInfo.env,
                tcoCount
            };
        } else {
            // No frame to replace, just enter
            this.enterFrame(frameInfo);
        }
    }

    /**
     * Gets the current stack depth.
     * @returns {number}
     */
    getDepth() {
        return this.frames.length;
    }

    /**
     * Gets a copy of the current stack.
     * Frames are ordered bottom (oldest) to top (newest).
     * @returns {StackFrame[]}
     */
    getStack() {
        return [...this.frames];
    }

    /**
     * Gets the current (top) frame.
     * @returns {StackFrame|null}
     */
    getCurrentFrame() {
        if (this.frames.length === 0) return null;
        return this.frames[this.frames.length - 1];
    }

    /**
     * Clears all frames.
     */
    clear() {
        this.frames = [];
    }

    /**
     * Formats the stack for CDP (Chrome DevTools Protocol) output.
     * @returns {Array} CDP-formatted call frames
     */
    toCDPFormat() {
        // Reverse so top frame is first (CDP convention)
        return this.frames.slice().reverse().map((frame, index) => ({
            callFrameId: `frame-${this.frames.length - 1 - index}`,
            functionName: frame.name,
            location: frame.source ? {
                scriptId: frame.source.filename,
                lineNumber: frame.source.line - 1, // CDP uses 0-indexed
                columnNumber: (frame.source.column || 1) - 1
            } : null,
            scopeChain: [], // Will be populated by StateInspector
            this: null
        }));
    }
}
