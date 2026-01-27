
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
    }
    // Note: js-ref and js-set! are defined in extras/primitives/interop.js
    // with proper jsToScheme conversion. Do not duplicate them here!
};

