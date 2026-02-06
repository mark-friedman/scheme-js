import { SchemeUnboundError } from './errors.js';

/**
 * Manages lexical scope via a chain of maps.
 */
export class Environment {
    /**
     * @param {Environment | null} parent - The parent environment.
     * @param {Map<string, *>} [bindings] - Local bindings for this frame.
     */
    constructor(parent = null, bindings = new Map()) {
        this.parent = parent;
        this.bindings = bindings;
        /** @type {Map<string, string>} Mapping from original name to renamed name */
        this.nameMap = new Map();
    }

    /**
     * Creates a new child environment.
     * @param {string} name - The variable name to bind.
     * @param {*} value - The value to bind.
     * @param {string} [originalName] - The original name before alpha-renaming.
     * @returns {Environment} A new child environment.
     */
    extend(name, value, originalName = null) {
        const env = new Environment(this, new Map([[name, value]]));
        if (originalName) env.nameMap.set(originalName, name);
        return env;
    }

    /**
     * Creates a new child environment with multiple bindings.
     * @param {Array<string>} names - The variable names.
     * @param {Array<*>} values - The corresponding values.
     * @param {Array<string>} [originalNames] - The original names before alpha-renaming.
     * @returns {Environment} A new child environment.
     */
    extendMany(names, values, originalNames = null) {
        const newBindings = new Map();
        const newNameMap = new Map();
        for (let i = 0; i < names.length; i++) {
            newBindings.set(names[i], values[i]);
            if (originalNames && originalNames[i]) {
                newNameMap.set(originalNames[i], names[i]);
            }
        }
        const env = new Environment(this, newBindings);
        env.nameMap = newNameMap;
        return env;
    }

    /**
     * Finds a variable's value by searching up the scope chain.
     * @param {string} name - The variable name to look up.
     * @returns {*} The bound value.
     * @throws {SchemeUnboundError} If the variable is not bound.
     */
    lookup(name) {
        if (this.bindings.has(name)) {
            return this.bindings.get(name);
        }
        if (this.parent) {
            return this.parent.lookup(name);
        }
        // Fallback to JS global environment
        if (Reflect.has(globalThis, name)) {
            return globalThis[name];
        }
        throw new SchemeUnboundError(name);
    }

    /**
     * Finds the environment where a variable is defined.
     * @param {string} name - The variable name.
     * @returns {Environment | null} The environment, or null if not found.
     */
    findEnv(name) {
        if (this.bindings.has(name)) {
            return this;
        }
        if (this.parent) {
            return this.parent.findEnv(name);
        }
        return null;
    }

    /**
   * Updates an existing variable in the scope chain.
   * Throws an error if the variable is not bound.
   * @param {string} name - The variable name.
   * @param {*} value - The new value.
   * @returns {*} The new value.
   * @throws {SchemeUnboundError} If the variable is not bound.
   */
    set(name, value) {
        const env = this.findEnv(name);
        if (!env) {
            // Check JS global environment
            if (Reflect.has(globalThis, name)) {
                globalThis[name] = value;
                return value;
            }
            throw new SchemeUnboundError(name, true);
        }
        env.bindings.set(name, value);
        return value;
    }

    /**
     * Defines a variable in the *current* environment frame.
     * Shadows any outer bindings of the same name.
     * @param {string} name - The variable name.
     * @param {*} value - The value to bind.
     * @returns {*} The value.
     */
    define(name, value) {
        this.bindings.set(name, value);
        return value;
    }
}
