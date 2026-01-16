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
    }
};
