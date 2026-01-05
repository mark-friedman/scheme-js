
import { schemeToJs, schemeToJsDeep, jsToScheme, jsToSchemeDeep, registerJsObjectRecord } from '../interpreter/js_interop.js';

export const jsInteropPrimitives = {
    'scheme->js': schemeToJs,
    'scheme->js-deep': schemeToJsDeep,
    'js->scheme': jsToScheme,
    'js->scheme-deep': jsToSchemeDeep,

    // JS Object Record Helpers
    // We expect the Scheme side to define the record type, then pass the constructor here
    'register-js-object-record': (ctor) => {
        registerJsObjectRecord(ctor);
    },

    // Generic Accessors for the js-object (which are just JS objects)
    'js-ref': (obj, key) => {
        return obj[key];
    },

    'js-set!': (obj, key, val) => {
        obj[key] = val;
    }
};
