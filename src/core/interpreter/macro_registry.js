/**
 * Registry for macro transformers.
 * Maps symbol names to transformer functions.
 */
class MacroRegistry {
    constructor() {
        this.macros = new Map();
    }

    /**
     * Registers a macro transformer.
     * @param {string} name - The name of the macro.
     * @param {Function} transformer - The transformer function.
     *                                 Takes an S-expression, returns an S-expression.
     */
    define(name, transformer) {
        this.macros.set(name, transformer);
    }

    /**
     * Checks if a name is bound to a macro.
     * @param {string} name
     * @returns {boolean}
     */
    isMacro(name) {
        return this.macros.has(name);
    }

    /**
     * Retrieves the transformer for a macro.
     * @param {string} name
     * @returns {Function | undefined}
     */
    lookup(name) {
        return this.macros.get(name);
    }

    /**
     * Clears all macros (useful for testing).
     */
    clear() {
        this.macros.clear();
    }
}

// Export a singleton instance
export const globalMacroRegistry = new MacroRegistry();
