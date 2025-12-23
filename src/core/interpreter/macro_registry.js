/**
 * Registry for macro transformers.
 * Maps symbol names to transformer functions.
 */
class MacroRegistry {
    constructor(parent = null) {
        this.macros = new Map();
        this.parent = parent;
    }

    /**
     * Registers a macro transformer.
     * @param {string} name - The name of the macro.
     * @param {Function} transformer - The transformer function.
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
        if (this.macros.has(name)) return true;
        if (this.parent) return this.parent.isMacro(name);
        return false;
    }

    /**
     * Retrieves the transformer for a macro.
     * @param {string} name
     * @returns {Function | undefined}
     */
    lookup(name) {
        if (this.macros.has(name)) return this.macros.get(name);
        if (this.parent) return this.parent.lookup(name);
        return undefined;
    }

    /**
     * Clears all macros (useful for testing).
     */
    clear() {
        this.macros.clear();
        this.parent = null;
    }
}

// Export a singleton instance and the class itself for scoped registries
export { MacroRegistry };
export const globalMacroRegistry = new MacroRegistry();
