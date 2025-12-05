import { toArray } from '../cons.js';

export const recordPrimitives = {
    // (make-record-type name fields)
    // name: string
    // fields: list of symbols
    'make-record-type': (name, fields) => {
        const fieldNames = toArray(fields).map(s => s.name);

        // Dynamically create a named class
        // We use new Function to get a named class constructor
        // This ensures 'instanceof' works and the class name is correct in JS.
        const classSrc = `
            return class ${name} {
                constructor(${fieldNames.join(', ')}) {
                    ${fieldNames.map(f => `this.${f} = ${f};`).join('\n')}
                }
            }
        `;

        const ClassConstructor = new Function(classSrc)();
        return ClassConstructor;
    },

    // (record-constructor rtd)
    // rtd: The Class constructor returned by make-record-type
    'record-constructor': (rtd) => {
        // Return a function that creates a new instance
        return (...args) => new rtd(...args);
    },

    // (record-predicate rtd)
    'record-predicate': (rtd) => {
        return (obj) => obj instanceof rtd;
    },

    // (record-accessor rtd field)
    'record-accessor': (rtd, field) => {
        const fieldName = field.name;
        return (obj) => {
            if (!(obj instanceof rtd)) {
                throw new Error(`Accessor ${fieldName}: expected instance of ${rtd.name}`);
            }
            return obj[fieldName];
        };
    },

    // (record-modifier rtd field)
    'record-modifier': (rtd, field) => {
        const fieldName = field.name;
        return (obj, val) => {
            if (!(obj instanceof rtd)) {
                throw new Error(`Modifier ${fieldName}: expected instance of ${rtd.name}`);
            }
            obj[fieldName] = val;
        };
    }
};
