/**
 * @fileoverview StateInspector for debugger state inspection.
 *
 * Provides scope chain traversal and CDP-compatible value serialization.
 * Used by the debug runtime to inspect variables and values during pause.
 */

import { Cons } from '../core/interpreter/cons.js';
import { Symbol as SchemeSymbol } from '../core/interpreter/symbol.js';
import { Char } from '../core/primitives/char_class.js';
import { Rational } from '../core/primitives/rational.js';
import { Complex } from '../core/primitives/complex.js';
import { isSchemeClosure, isSchemeContinuation } from '../core/interpreter/values.js';

/**
 * StateInspector provides debugger state inspection capabilities.
 * It converts Scheme environments and values to CDP-compatible formats.
 */
export class StateInspector {
    constructor() {
        /** @type {number} Counter for generating unique object IDs */
        this.objectIdCounter = 1;
        /** @type {Map<number, *>} Registry of objects by ID for later inspection */
        this.objectRegistry = new Map();
    }

    /**
     * Gets the scope chain for an environment.
     * Returns an array of scope objects, from innermost to outermost.
     *
     * @param {Environment} env - The environment to inspect
     * @returns {Array<Object>} Array of scope descriptors
     */
    getScopes(env) {
        const scopes = [];
        let current = env;
        let isFirst = true;

        while (current) {
            const isGlobal = current.parent === null;
            const type = isGlobal ? 'global' : (isFirst ? 'local' : 'closure');

            const scope = {
                type,
                name: this._getScopeName(type),
                object: {
                    type: 'object',
                    objectId: this._registerObject(current),
                    className: 'Object',
                    description: `${type} scope`
                }
            };

            scopes.push(scope);
            current = current.parent;
            isFirst = false;
        }

        return scopes;
    }

    /**
     * Gets the properties (bindings) of a single scope/environment.
     *
     * @param {Environment} env - The environment to inspect
     * @returns {Array<Object>} Array of property descriptors
     */
    getScopeProperties(env) {
        const properties = [];

        for (const [name, value] of env.bindings) {
            properties.push({
                name,
                value: this.serializeValue(value),
                writable: true,
                configurable: true,
                enumerable: true
            });
        }

        return properties;
    }

    /**
     * Serializes a Scheme value to CDP RemoteObject format.
     *
     * @param {*} value - The value to serialize
     * @param {number} [depth=0] - Current recursion depth (for limiting)
     * @returns {Object} CDP RemoteObject representation
     */
    serializeValue(value, depth = 0) {
        // Handle null
        if (value === null) {
            return { type: 'object', subtype: 'null', value: null };
        }

        // Handle undefined
        if (value === undefined) {
            return { type: 'undefined' };
        }

        // Handle primitive types
        if (typeof value === 'number') {
            return { type: 'number', value, description: String(value) };
        }

        if (typeof value === 'bigint') {
            return {
                type: 'bigint',
                value: value.toString(),
                description: value.toString() + 'n',
                unserializableValue: value.toString() + 'n'
            };
        }

        if (typeof value === 'string') {
            return { type: 'string', value, description: `"${value}"` };
        }

        if (typeof value === 'boolean') {
            return { type: 'boolean', value, description: String(value) };
        }

        // Handle Scheme-specific types
        if (value instanceof SchemeSymbol) {
            return {
                type: 'symbol',
                description: value.name,
                objectId: this._registerObject(value)
            };
        }

        if (value instanceof Char) {
            return {
                type: 'object',
                subtype: 'character',
                className: 'Char',
                description: this._formatChar(value),
                objectId: this._registerObject(value)
            };
        }

        if (value instanceof Rational) {
            return {
                type: 'object',
                subtype: 'rational',
                className: 'Rational',
                description: value.toString(),
                objectId: this._registerObject(value)
            };
        }

        if (value instanceof Complex) {
            return {
                type: 'object',
                subtype: 'complex',
                className: 'Complex',
                description: value.toString(),
                objectId: this._registerObject(value)
            };
        }

        if (value instanceof Cons) {
            return {
                type: 'object',
                subtype: 'pair',
                className: 'Pair',
                description: this._describePair(value),
                objectId: this._registerObject(value)
            };
        }

        // Handle functions (closures and continuations)
        if (typeof value === 'function') {
            if (isSchemeContinuation(value)) {
                return {
                    type: 'function',
                    subtype: 'continuation',
                    className: 'Continuation',
                    description: '#<continuation>',
                    objectId: this._registerObject(value)
                };
            }

            if (isSchemeClosure(value) || value.__schemeClosure) {
                const params = value.__params || [];
                return {
                    type: 'function',
                    subtype: 'closure',
                    className: 'Closure',
                    description: `#<procedure (${params.join(' ')})>`,
                    objectId: this._registerObject(value)
                };
            }

            // Regular JS function
            return {
                type: 'function',
                className: 'Function',
                description: value.name ? `function ${value.name}` : 'function',
                objectId: this._registerObject(value)
            };
        }

        // Handle arrays (vectors in Scheme)
        if (Array.isArray(value)) {
            return {
                type: 'object',
                subtype: 'vector',
                className: 'Vector',
                description: `#(... ${value.length} elements)`,
                objectId: this._registerObject(value)
            };
        }

        // Handle Scheme records
        if (value && typeof value === 'object' && value.__schemeRecord) {
            const rtdName = value.__rtd?.name || 'record';
            return {
                type: 'object',
                subtype: 'record',
                className: rtdName,
                description: `#<${rtdName}>`,
                objectId: this._registerObject(value)
            };
        }

        // Handle plain objects
        if (typeof value === 'object') {
            const className = value.constructor?.name || 'Object';
            return {
                type: 'object',
                className,
                description: this._describeObject(value),
                objectId: this._registerObject(value)
            };
        }

        // Fallback
        return {
            type: 'object',
            description: String(value)
        };
    }

    /**
     * Gets an object by its ID (for property inspection).
     *
     * @param {string} objectId - The object ID
     * @returns {*} The registered object, or undefined
     */
    getObjectById(objectId) {
        const id = parseInt(objectId.replace('obj-', ''), 10);
        return this.objectRegistry.get(id);
    }

    /**
     * Gets the properties of a registered object.
     *
     * @param {string} objectId - The object ID
     * @returns {Array<Object>} Array of property descriptors
     */
    getObjectProperties(objectId) {
        const obj = this.getObjectById(objectId);
        if (!obj) return [];

        const properties = [];

        // Handle Cons cells
        if (obj instanceof Cons) {
            properties.push({
                name: 'car',
                value: this.serializeValue(obj.car)
            });
            properties.push({
                name: 'cdr',
                value: this.serializeValue(obj.cdr)
            });
            return properties;
        }

        // Handle arrays
        if (Array.isArray(obj)) {
            for (let i = 0; i < Math.min(obj.length, 100); i++) {
                properties.push({
                    name: String(i),
                    value: this.serializeValue(obj[i])
                });
            }
            if (obj.length > 100) {
                properties.push({
                    name: '...',
                    value: { type: 'string', value: `${obj.length - 100} more elements` }
                });
            }
            return properties;
        }

        // Handle Environment
        if (obj.bindings instanceof Map) {
            return this.getScopeProperties(obj);
        }

        // Handle plain objects
        if (typeof obj === 'object') {
            for (const key of Object.keys(obj)) {
                if (!key.startsWith('__')) {
                    properties.push({
                        name: key,
                        value: this.serializeValue(obj[key])
                    });
                }
            }
        }

        return properties;
    }

    /**
     * Resets the object registry.
     */
    reset() {
        this.objectIdCounter = 1;
        this.objectRegistry.clear();
    }

    // =========================================================================
    // Private Helpers
    // =========================================================================

    /**
     * Registers an object and returns its ID.
     * @private
     */
    _registerObject(obj) {
        const id = this.objectIdCounter++;
        this.objectRegistry.set(id, obj);
        return `obj-${id}`;
    }

    /**
     * Gets a display name for a scope type.
     * @private
     */
    _getScopeName(type) {
        switch (type) {
            case 'local': return 'Local';
            case 'closure': return 'Closure';
            case 'global': return 'Global';
            default: return 'Scope';
        }
    }

    /**
     * Creates a description for a pair/list.
     * @private
     */
    _describePair(pair) {
        // Try to detect if it's a proper list
        let length = 0;
        let current = pair;
        while (current instanceof Cons) {
            length++;
            current = current.cdr;
            if (length > 10) break;
        }

        if (current === null) {
            return `(list ... ${length} elements)`;
        } else {
            return '(pair)';
        }
    }

    /**
     * Creates a description for a plain object.
     * @private
     */
    _describeObject(obj) {
        const className = obj.constructor?.name || 'Object';
        const keys = Object.keys(obj).filter(k => !k.startsWith('__'));
        if (keys.length <= 3) {
            return `${className} {${keys.join(', ')}}`;
        }
        return `${className} {...}`;
    }

    /**
     * Formats a Char value in Scheme reader notation.
     * @private
     */
    _formatChar(char) {
        const codePoint = char.codePoint;
        // Handle named characters
        switch (codePoint) {
            case 0: return '#\\null';
            case 7: return '#\\alarm';
            case 8: return '#\\backspace';
            case 9: return '#\\tab';
            case 10: return '#\\newline';
            case 13: return '#\\return';
            case 27: return '#\\escape';
            case 32: return '#\\space';
            case 127: return '#\\delete';
        }
        // Printable ASCII
        if (codePoint >= 33 && codePoint <= 126) {
            return `#\\${String.fromCodePoint(codePoint)}`;
        }
        // Use hex notation for other characters
        return `#\\x${codePoint.toString(16)}`;
    }
}
