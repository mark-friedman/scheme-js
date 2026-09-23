/**
 * Class Primitives for Scheme.
 * 
 * Provides operations to create and manipulate JS-compatible classes from Scheme.
 */

import { toArray } from '../interpreter/cons.js';
import { assertString, assertSymbol } from '../interpreter/type_check.js';
import { SchemeTypeError } from '../interpreter/errors.js';
import { SCHEME_PRIMITIVE, takesSchemeValues, callSchemeMethod } from '../interpreter/values.js';
import { noteSchemeStore } from '../interpreter/js_interop.js';

// ============================================================================
// Construction from Scheme
// ============================================================================

// A class made here is both a Scheme procedure and a JavaScript constructor,
// and the two kinds of caller pass different kinds of value: Scheme passes
// Scheme values, JavaScript passes JavaScript ones, whose integers must become
// exact. They are told apart by `new`. The interpreter applies a class as a
// plain function, so a class called without `new` is being constructed from
// Scheme; one called with `new` -- from JavaScript, from `js-new`, or as the
// parent under a subclass's constructor -- from JavaScript. JavaScript code
// that calls a class without `new` is therefore taken for Scheme.
//
// The class's JavaScript constructor needs to know which kind of construction
// it is running, to run the Scheme constructor body without conversion and to
// pass the knowledge on to its parent's constructor. It is handed over here,
// naming the class whose constructor may take it: set just before that
// constructor runs, taken by it before anything else happens, and cleared
// when the construction returns. Naming the class keeps an unrelated
// construction -- one a JavaScript parent's constructor performs, say -- from
// taking it.
let schemeConstruction = null;

/**
 * Constructs an instance of a class for a Scheme caller.
 * @param {Function} cls - The class's wrapper, as `make-class` returns it.
 * @param {Function} InternalClass - The class's JavaScript constructor.
 * @param {Array} args - The constructor's arguments, as Scheme values.
 * @returns {Object} The new instance.
 */
function constructFromScheme(cls, InternalClass, args) {
    schemeConstruction = cls;
    try {
        return new InternalClass(...args);
    } finally {
        schemeConstruction = null;
    }
}

/**
 * Called first by a class's JavaScript constructor: whether this construction
 * is for a Scheme caller.
 * @param {Function} cls - The class's wrapper.
 * @returns {boolean} True if the construction was handed to `cls` from Scheme.
 */
function takeSchemeConstruction(cls) {
    if (schemeConstruction === cls) {
        schemeConstruction = null;
        return true;
    }
    return false;
}

/**
 * Runs a Scheme procedure a class was given -- a constructor body, or the
 * computation of a parent's constructor arguments -- converting nothing for
 * a Scheme construction and converting as for any JavaScript caller otherwise.
 * @param {Function} proc - The Scheme procedure.
 * @param {*} thisArg - The value `this` is bound to.
 * @param {Array} args - The constructor's arguments.
 * @param {boolean} fromScheme - Whether the construction is for Scheme.
 * @returns {*} The procedure's result.
 */
function runConstructorProcedure(proc, thisArg, args, fromScheme) {
    return fromScheme ? callSchemeMethod(proc, thisArg, args) : proc.apply(thisArg, args);
}

// ============================================================================
// Class Primitives
// ============================================================================

/**
 * Class primitives exported to Scheme.
 */
export const classPrimitives = {
    /**
     * Creates a class whose constructor sets each own field from the
     * constructor parameter of the same name.
     * @param {string|Symbol} name - The class name.
     * @param {Function|null} parent - The parent class or null.
     * @param {Cons} fieldTags - List of own field symbols.
     * @param {Cons} constructorTags - List of constructor parameter symbols;
     *   with a parent, all of them are also passed to the parent's constructor.
     * @returns {Function} The class constructor wrapper.
     */
    'make-class': (name, parent, fieldTags, constructorTags) => {
        const typeName = typeof name === 'string' ? name : name.name;
        const ownFieldNames = toArray(fieldTags).map(s => s.name);
        const constructorParamNames = toArray(constructorTags).map(s => s.name);
        const jsClassName = typeName.replace(/[^a-zA-Z0-9_$]/g, '_');

        // What the generated constructor calls back into. `Wrapper` is
        // assigned below, before any construction can run.
        let Wrapper;
        const hooks = {
            take: () => takeSchemeConstruction(Wrapper),
            handToParent: () => { schemeConstruction = parent; },
            note: noteSchemeStore
        };
        // Each own field is set, and for a Scheme construction noted so that
        // an integer-valued flonum reads back as a flonum.
        const setFields = ownFieldNames.map(f =>
            `if (${f} !== undefined) { this.${f} = ${f}; if (fromScheme) hooks.note(this, ${JSON.stringify(f)}, ${f}); }`
        ).join('\n');

        // Dynamically create the class
        let InternalClass;
        if (parent) {
            // The parent's constructor runs inside `super`, so a Scheme
            // construction is handed on to it first.
            const classSrc = `
                return class ${jsClassName} extends parent {
                    static get schemeName() { return ${JSON.stringify(typeName)}; }
                    constructor(${constructorParamNames.join(', ')}) {
                        const fromScheme = hooks.take();
                        if (fromScheme) hooks.handToParent();
                        super(${constructorParamNames.join(', ')});
                        ${setFields}
                    }
                }
            `;
            InternalClass = new Function('parent', 'hooks', classSrc)(parent, hooks);
        } else {
            const classSrc = `
                return class ${jsClassName} {
                    static get schemeName() { return ${JSON.stringify(typeName)}; }
                    constructor(${constructorParamNames.join(', ')}) {
                        const fromScheme = hooks.take();
                        ${setFields}
                    }
                }
            `;
            InternalClass = new Function('hooks', classSrc)(hooks);
        }

        // Return a wrapper that allows calling without 'new' in Scheme/JS
        Wrapper = function (...args) {
            if (new.target) {
                return Reflect.construct(InternalClass, args, new.target);
            }
            return constructFromScheme(Wrapper, InternalClass, args);
        };

        // Ensure instanceof and static members work
        Object.setPrototypeOf(Wrapper, InternalClass);
        Wrapper.prototype = InternalClass.prototype;
        // Mark it so we can identify it's a class wrapper
        Wrapper.isSchemeClass = true;
        Wrapper[SCHEME_PRIMITIVE] = true;

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
        // A Scheme method gets the caller's values unconverted, as through
        // dot notation.
        if (takesSchemeValues(method)) {
            return callSchemeMethod(method, instance, args);
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
        let Wrapper;
        if (parent) {
            // Two-phase construction: get super args, construct parent, then init
            const CustomClass = function (...args) {
                const fromScheme = takeSchemeConstruction(Wrapper);

                // Phase 1: Get super args (call with null this, just computing args)
                let superArgs = args; // Default: pass all args to super
                if (superArgsFn) {
                    const result = runConstructorProcedure(superArgsFn, undefined, args, fromScheme);
                    // Result should be a list/array of args
                    superArgs = Array.isArray(result) ? result : [result];
                }

                // Phase 2: Construct with parent using computed super args,
                // handing a Scheme construction on to the parent's constructor.
                let instance;
                if (fromScheme) {
                    schemeConstruction = parent;
                    try {
                        instance = Reflect.construct(parent, superArgs, CustomClass);
                    } finally {
                        schemeConstruction = null;
                    }
                } else {
                    instance = Reflect.construct(parent, superArgs, CustomClass);
                }

                // Phase 3: Run init with this bound to instance
                if (initFn) {
                    runConstructorProcedure(initFn, instance, args, fromScheme);
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
                    const fromScheme = takeSchemeConstruction(Wrapper);
                    if (initFn) {
                        runConstructorProcedure(initFn, this, args, fromScheme);
                    }
                }
            };
        }

        // Return wrapper that allows calling without 'new'
        Wrapper = function (...args) {
            if (new.target) {
                return Reflect.construct(InternalClass, args, new.target);
            }
            return constructFromScheme(Wrapper, InternalClass, args);
        };

        Object.setPrototypeOf(Wrapper, InternalClass);
        Wrapper.prototype = InternalClass.prototype;
        Wrapper.isSchemeClass = true;
        Wrapper[SCHEME_PRIMITIVE] = true;
        if (parent) {
            Wrapper._schemeParent = parent;
        }

        return Wrapper;
    }
};
