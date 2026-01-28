/**
 * Class Primitives for Scheme.
 * 
 * Provides operations to create and manipulate JS-compatible classes from Scheme.
 */

import { toArray } from '../interpreter/cons.js';
import { assertString, assertSymbol } from '../interpreter/type_check.js';
import { SchemeTypeError } from '../interpreter/errors.js';

/**
 * Class primitives exported to Scheme.
 */
export const classPrimitives = {
    'make-class': (name, parent, fieldTags, constructorTags) => {
        const typeName = typeof name === 'string' ? name : name.name;
        const ownFieldNames = toArray(fieldTags).map(s => s.name);
        const constructorParamNames = toArray(constructorTags).map(s => s.name);
        const jsClassName = typeName.replace(/[^a-zA-Z0-9_$]/g, '_');

        // Dynamically create the class
        let InternalClass;
        if (parent) {
            const classSrc = `
                return class ${jsClassName} extends parent {
                    static get schemeName() { return ${JSON.stringify(typeName)}; }
                    constructor(${constructorParamNames.join(', ')}) {
                        super(${constructorParamNames.join(', ')});
                        ${ownFieldNames.map(f => `if (${f} !== undefined) this.${f} = ${f};`).join('\n')}
                    }
                }
            `;
            InternalClass = new Function('parent', classSrc)(parent);
        } else {
            const classSrc = `
                return class ${jsClassName} {
                    static get schemeName() { return ${JSON.stringify(typeName)}; }
                    constructor(${constructorParamNames.join(', ')}) {
                        ${ownFieldNames.map(f => `if (${f} !== undefined) this.${f} = ${f};`).join('\n')}
                    }
                }
            `;
            InternalClass = new Function(classSrc)();
        }

        // Return a wrapper that allows calling without 'new' in Scheme/JS
        const Wrapper = function (...args) {
            if (new.target) {
                return Reflect.construct(InternalClass, args, new.target);
            }
            return new InternalClass(...args);
        };

        // Ensure instanceof and static members work
        Object.setPrototypeOf(Wrapper, InternalClass);
        Wrapper.prototype = InternalClass.prototype;
        // Mark it so we can identify it's a class wrapper
        Wrapper.isSchemeClass = true;

        return Wrapper;
    },

    /**
     * Sets a method on a class's prototype.
     * @param {Function} cls - The class constructor.
     * @param {string|Symbol} name - The method name.
     * @param {Function} proc - The Scheme procedure (callable closure).
     */
    'class-method-set!': (cls, name, proc) => {
        const methodName = typeof name === 'string' ? name : name.name;
        if (typeof proc !== 'function') {
            throw new SchemeTypeError('class-method-set!', 3, 'procedure', proc);
        }
        cls.prototype[methodName] = proc;
    },

    /**
     * Calls a parent method on the current instance.
     * @param {Object} instance - The current instance (this).
     * @param {string|Symbol} methodName - The method name to call.
     * @param {...*} args - Arguments to pass to the method.
     * @returns {*} The result of calling the parent method.
     */
    'class-super-call': (instance, methodName, ...args) => {
        const name = typeof methodName === 'string' ? methodName : methodName.name;
        const cls = instance.constructor;
        const parent = cls._schemeParent;
        if (!parent) {
            throw new Error('class-super-call: no parent class');
        }
        const method = parent.prototype[name];
        if (typeof method !== 'function') {
            throw new Error(`class-super-call: parent has no method '${name}'`);
        }
        return method.call(instance, ...args);
    },

    /**
     * Creates a class with custom super call and initialization.
     * @param {string|Symbol} name - The class name.
     * @param {Function|null} parent - The parent class or null.
     * @param {Cons} fieldTags - List of field symbols.
     * @param {Cons} constructorTags - List of constructor parameter symbols.
     * @param {Function|null} superArgsFn - Scheme procedure that returns args for super call, or null.
     * @param {Function|null} initFn - Scheme procedure for post-super initialization, or null.
     * @returns {Function} The class constructor wrapper.
     */
    'make-class-with-init': (name, parent, fieldTags, constructorTags, superArgsFn, initFn) => {
        const typeName = typeof name === 'string' ? name : name.name;

        let InternalClass;
        if (parent) {
            // Two-phase construction: get super args, construct parent, then init
            const CustomClass = function (...args) {
                // Phase 1: Get super args (call with null this, just computing args)
                let superArgs = args; // Default: pass all args to super
                if (superArgsFn) {
                    const result = superArgsFn(...args);
                    // Result should be a list/array of args
                    superArgs = Array.isArray(result) ? result : [result];
                }

                // Phase 2: Construct with parent using computed super args
                const instance = Reflect.construct(parent, superArgs, CustomClass);

                // Phase 3: Run init with this bound to instance
                if (initFn) {
                    initFn.call(instance, ...args);
                }

                return instance;
            };

            // Set up prototype chain
            CustomClass.prototype = Object.create(parent.prototype);
            CustomClass.prototype.constructor = CustomClass;
            Object.setPrototypeOf(CustomClass, parent);
            Object.defineProperty(CustomClass, 'schemeName', { get: () => typeName });

            // Store parent for super.method access
            CustomClass._schemeParent = parent;

            InternalClass = CustomClass;
        } else {
            // No parent - simpler case
            InternalClass = class {
                static get schemeName() { return typeName; }
                constructor(...args) {
                    if (initFn) {
                        initFn.call(this, ...args);
                    }
                }
            };
        }

        // Return wrapper that allows calling without 'new'
        const Wrapper = function (...args) {
            if (new.target) {
                return Reflect.construct(InternalClass, args, new.target);
            }
            return new InternalClass(...args);
        };

        Object.setPrototypeOf(Wrapper, InternalClass);
        Wrapper.prototype = InternalClass.prototype;
        Wrapper.isSchemeClass = true;
        if (parent) {
            Wrapper._schemeParent = parent;
        }

        return Wrapper;
    }
};
