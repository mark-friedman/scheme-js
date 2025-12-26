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

// Track the baseline set of macros (after std lib loading)
let baselineMacroNames = null;

/**
 * Takes a snapshot of the current macro registry state as the baseline.
 * Call this after loading standard libraries but before running user code.
 */
export function snapshotMacroRegistry() {
    baselineMacroNames = new Set(globalMacroRegistry.macros.keys());
}

/**
 * Resets the global macro registry to the baseline state,
 * removing any macros added after the snapshot was taken.
 */
export function resetGlobalMacroRegistry() {
    if (baselineMacroNames === null) {
        // No baseline - just clear everything (fallback behavior)
        globalMacroRegistry.macros.clear();
    } else {
        // Remove only macros added after the baseline
        for (const name of globalMacroRegistry.macros.keys()) {
            if (!baselineMacroNames.has(name)) {
                globalMacroRegistry.macros.delete(name);
            }
        }
    }
}
