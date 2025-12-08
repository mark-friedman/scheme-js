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
    }

    /**
     * Creates a new child environment.
     * @param {string} name - The variable name to bind.
     * @param {*} value - The value to bind.
     * @returns {Environment} A new child environment.
     */
    extend(name, value) {
        return new Environment(this, new Map([[name, value]]));
    }

    /**
     * Creates a new child environment with multiple bindings.
     * @param {Array<string>} names - The variable names.
     * @param {Array<*>} values - The corresponding values.
     * @returns {Environment} A new child environment.
     */
    extendMany(names, values) {
        const newBindings = new Map();
        for (let i = 0; i < names.length; i++) {
            newBindings.set(names[i], values[i]);
        }
        return new Environment(this, newBindings);
    }

    /**
     * Finds a variable's value by searching up the scope chain.
     * @param {string} name - The variable name to look up.
     * @returns {*} The bound value.
     */
    lookup(name) {
        if (this.bindings.has(name)) {
            return this.bindings.get(name);
        }
        if (this.parent) {
            return this.parent.lookup(name);
        }
        throw new Error(`Unbound variable: ${name}`);
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
   * @throws {Error} If the variable is not bound.
   */
    set(name, value) {
        const env = this.findEnv(name);
        if (!env) {
            throw new Error(`set!: unbound variable: ${name}`);
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
