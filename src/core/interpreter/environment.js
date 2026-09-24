import { SchemeUnboundError } from './errors.js';
import { noteBinding } from './primitive_bindings.js';

/**
 * Manages lexical scope via a chain of maps.
 */
export class Environment {
    /**
     * @param {Environment | null} parent - The parent environment.
     * @param {Map<string, *>} [bindings] - Local bindings for this frame.
     * @param {Map<string, string> | null} [nameMap] - Mapping from original name
     *   to alpha-renamed name. Allocated lazily: it exists only for the
     *   debugger's `:eval`, which reconstructs the analyzer's renaming from it,
     *   and a `Map` per frame per call is a cost every program was paying for a
     *   facility almost no program uses.
     */
    constructor(parent = null, bindings = new Map(), nameMap = null) {
        this.parent = parent;
        this.bindings = bindings;
        /** @type {Map<string, string> | null} */
        this.nameMap = nameMap;
        /**
         * Cells compiled code reads this frame's bindings through, by name.
         * Created only when compiled code first asks for one, so a frame the
         * interpreter makes for a call carries no more than a null.
         * @type {Map<string, {v: *}> | null}
         */
        this.cells = null;
    }

    /**
     * Creates a new child environment.
     * @param {string} name - The variable name to bind.
     * @param {*} value - The value to bind.
     * @param {string} [originalName] - The original name before alpha-renaming.
     * @returns {Environment} A new child environment.
     */
    extend(name, value, originalName = null) {
        const nameMap = originalName ? new Map([[originalName, name]]) : null;
        return new Environment(this, new Map([[name, value]]), nameMap);
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
        let newNameMap = null;
        for (let i = 0; i < names.length; i++) {
            newBindings.set(names[i], values[i]);
            if (originalNames && originalNames[i]) {
                if (newNameMap === null) newNameMap = new Map();
                newNameMap.set(originalNames[i], names[i]);
            }
        }
        return new Environment(this, newBindings, newNameMap);
    }

    /**
     * Creates a child environment binding `names` to a slice of `values`.
     *
     * Exists so the common closure-application path does not have to allocate
     * an argument array just to drop the operator off the front: the evaluated
     * operator and operands arrive in one array, and this reads the operands
     * directly out of it.
     *
     * @param {Array<string>} names - The variable names.
     * @param {Array<*>} values - Array containing the values.
     * @param {number} offset - Index in `values` of the value for `names[0]`.
     * @param {Array<string>} [originalNames] - Names before alpha-renaming.
     * @returns {Environment} A new child environment.
     */
    extendManyFrom(names, values, offset, originalNames = null) {
        const newBindings = new Map();
        let newNameMap = null;
        for (let i = 0; i < names.length; i++) {
            newBindings.set(names[i], values[offset + i]);
            if (originalNames && originalNames[i]) {
                if (newNameMap === null) newNameMap = new Map();
                newNameMap.set(originalNames[i], names[i]);
            }
        }
        return new Environment(this, newBindings, newNameMap);
    }

    /**
     * Finds a variable's value by searching up the scope chain.
     * @param {string} name - The variable name to look up.
     * @returns {*} The bound value.
     * @throws {SchemeUnboundError} If the variable is not bound.
     */
    lookup(name) {
        // Walk iteratively rather than recursively, and probe each frame with a
        // single hash lookup instead of has()-then-get(). A binding whose value
        // is genuinely `undefined` -- which `letrec` creates before its
        // initializers run -- is the only case needing the second probe, so the
        // extra `has` is paid only there rather than on every reference.
        let env = this;
        do {
            const value = env.bindings.get(name);
            if (value !== undefined || env.bindings.has(name)) {
                return value;
            }
            env = env.parent;
        } while (env !== null);

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
        let env = this;
        do {
            if (env.bindings.has(name)) return env;
            env = env.parent;
        } while (env !== null);
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
        noteBinding(name, value);
        env.bindings.set(name, value);
        syncCell(env, name, value);
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
        noteBinding(name, value);
        this.bindings.set(name, value);
        syncCell(this, name, value);
        return value;
    }

    /**
     * Replaces a binding in this frame with an equivalent value.
     *
     * For installing a compiled procedure over the interpreted closure it was
     * compiled from, or over a library's copy of one. That is not a program
     * binding the name to something new, so the primitive-binding record is
     * not told, as a `define` would tell it.
     *
     * @param {string} name - A name bound in this frame.
     * @param {*} value - Its new value.
     * @returns {void}
     */
    rebind(name, value) {
        this.bindings.set(name, value);
        syncCell(this, name, value);
    }

    /**
     * The cell compiled code reads one of this frame's bindings through.
     *
     * A global read by compiled code cannot be taken at compile time, since
     * the binding may be defined later or changed, and looking it up in this
     * frame's map on every read costs a hash lookup each time -- up to 1.5x of
     * compiled run time on call-heavy code. So the frame hands out a cell per
     * name, which every write to the binding here keeps current, and compiled
     * code reads the cell. Scheme has no way to remove a binding, so a cell
     * never has to be retired.
     *
     * @param {string} name - A name bound in this frame.
     * @returns {{v: *}} Its cell, the same one every time.
     */
    cellFor(name) {
        if (this.cells === null) this.cells = new Map();
        let cell = this.cells.get(name);
        if (cell === undefined) {
            cell = { v: this.bindings.get(name) };
            this.cells.set(name, cell);
        }
        return cell;
    }
}

/**
 * Brings a binding's cell, if compiled code has asked for one, up to date.
 * @param {Environment} env - The frame written to.
 * @param {string} name - The name written.
 * @param {*} value - Its new value.
 * @returns {void}
 */
function syncCell(env, name, value) {
    if (env.cells !== null) {
        const cell = env.cells.get(name);
        if (cell !== undefined) cell.v = value;
    }
}
