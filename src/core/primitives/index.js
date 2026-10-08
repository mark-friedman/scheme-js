
import { Environment } from '../interpreter/environment.js';
import { globalScopeRegistry, GLOBAL_SCOPE_ID } from '../interpreter/syntax_object.js';
import { SCHEME_PRIMITIVE, SCHEME_RAW_CALL } from '../interpreter/values.js';
import { registerPrimitive } from '../interpreter/primitive_bindings.js';

import { mathPrimitives } from './math.js';
import { ioPrimitives } from './io/index.js';
import { printerPrimitives } from './io/printer.js';
import { listPrimitives } from './list.js';
import { vectorPrimitives } from './vector.js';
import { recordPrimitives } from './record.js';
import { stringPrimitives } from './string.js';
import { charPrimitives } from './char.js';
import { eqPrimitives } from './eq.js';
import { getAsyncPrimitives } from './async.js';
import { getControlPrimitives } from './control.js';
import { GCPrimitives } from './gc.js';
import { interopPrimitives } from '../../extras/primitives/interop.js';
import { getExceptionPrimitives } from './exception.js';
import { errorObjectPrimitives } from './error_object.js';
import { procedurePrimitives } from './apply.js';
import { timePrimitives } from './time.js';
import { processContextPrimitives } from './process_context.js';
import { bytevectorPrimitives } from './bytevector.js';
import { syntaxPrimitives } from './syntax.js';
import { promisePrimitives } from '../../extras/primitives/promise.js';
import { hashTablePrimitives } from '../../extras/primitives/hash_table.js';
import { bitwisePrimitives } from '../../extras/primitives/bitwise.js';
import { jsInteropPrimitives } from './js_interop_primitives.js';
import { libraryPrimitives } from './library.js';
import { readerPrimitives } from './reader_support.js';
import { expanderPrimitives } from './expander_support.js';
import { classPrimitives } from './class.js';
import { windPrimitives } from './winds.js';
import { handlerPrimitives } from './handlers.js';

/**
 * Creates the global environment with built-in primitives.
 * @param {Interpreter} interpreter - A reference to the interpreter for async callbacks.
 * @returns {Environment} The global environment.
 */
export function createGlobalEnvironment(interpreter) {
    const bindings = new Map();

    // Clear registry to ensure fresh state for tests
    globalScopeRegistry.clear();

    // Helper to add primitives
    const addPrimitives = (prims) => {
        for (const [name, fn] of Object.entries(prims)) {
            // Mark as Scheme-aware so interpreter doesn't auto-convert args
            if (typeof fn === 'function') {
                fn[SCHEME_PRIMITIVE] = true;
                // Its own entry for callers holding Scheme values, so that
                // compiled code, which looks for that entry first, calls it
                // with no second look at what it is.
                fn[SCHEME_RAW_CALL] = fn;
                // Its Scheme name, which the printer writes it by: the name
                // it is first bound to, not the JavaScript function's.
                if (!('schemeName' in fn)) fn.schemeName = name;
                registerPrimitive(name, fn);
            }
            bindings.set(name, fn); // No wrapper needed!

            // Register for hygienic macro expansion
            globalScopeRegistry.bind(name, new Set([GLOBAL_SCOPE_ID]), fn);
        }
    };

    addPrimitives(mathPrimitives);
    addPrimitives(ioPrimitives);
    addPrimitives(printerPrimitives);
    addPrimitives(listPrimitives);
    addPrimitives(vectorPrimitives);
    addPrimitives(recordPrimitives);
    addPrimitives(stringPrimitives);
    addPrimitives(charPrimitives);
    addPrimitives(eqPrimitives);
    addPrimitives(getAsyncPrimitives(interpreter));
    addPrimitives(getControlPrimitives(interpreter));
    addPrimitives(procedurePrimitives);
    addPrimitives(GCPrimitives);
    addPrimitives(interopPrimitives);
    addPrimitives(getExceptionPrimitives(interpreter));
    addPrimitives(errorObjectPrimitives);
    addPrimitives(timePrimitives);
    addPrimitives(processContextPrimitives);
    addPrimitives(bytevectorPrimitives);
    addPrimitives(syntaxPrimitives);
    addPrimitives(promisePrimitives);
    addPrimitives(hashTablePrimitives);
    addPrimitives(bitwisePrimitives);
    addPrimitives(jsInteropPrimitives);
    addPrimitives(classPrimitives);
    addPrimitives(windPrimitives);
    addPrimitives(handlerPrimitives);
    addPrimitives(libraryPrimitives);
    addPrimitives(readerPrimitives);
    addPrimitives(expanderPrimitives);

    return new Environment(null, bindings);
}
