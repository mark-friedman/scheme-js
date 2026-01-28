/**
 * Stepables Base Module
 * 
 * Contains the foundational elements shared by all stepable objects:
 * - Register constants (ANS, CTL, ENV, FSTACK)
 * - Executable base class
 * - ensureExecutable helper
 * 
 * This module is imported by both ast_nodes.js and frames.js.
 */

// =============================================================================
// Register Constants
// =============================================================================

/** Answer register - holds result of last computation */
export const ANS = 0;
/** Control register - holds next AST node or Frame to execute */
export const CTL = 1;
/** Environment register - holds current lexical environment */
export const ENV = 2;
/** Frame stack register - holds continuation frames */
export const FSTACK = 3;
/** This context register - holds current JavaScript 'this' context */
export const THIS = 4;

/**
 * @typedef {[any, Executable, Environment, Array<Executable>]} Registers
 * The register array: [ans, ctl, env, fstack]
 */

// =============================================================================
// Base Class
// =============================================================================

/**
 * Base class for all executable objects (AST nodes and Frames).
 * All must have a `step` method.
 */
export class Executable {
    /**
     * Executes a single step of this node's computation.
     * @param {Registers} registers - The [ans, ctl, env, fstack] registers array.
     * @param {Interpreter} interpreter - The interpreter instance.
     * @returns {boolean} `true` to continue trampoline, `false` to halt.
     */
    step(registers, interpreter) {
        throw new Error("step() must be implemented in subclasses");
    }
}

// Note: ensureExecutable is defined in ast_nodes.js because it depends on Literal
