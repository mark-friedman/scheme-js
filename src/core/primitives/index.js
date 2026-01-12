import { Environment } from '../interpreter/environment.js';
import { globalScopeRegistry, GLOBAL_SCOPE_ID } from '../interpreter/syntax_object.js';

import { mathPrimitives } from './math.js';
import { ioPrimitives } from './io.js';
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
import { timePrimitives } from './time.js';
import { processContextPrimitives } from './process_context.js';
import { bytevectorPrimitives } from './bytevector.js';
import { syntaxPrimitives } from './syntax.js';
import { getPromisePrimitives } from '../../extras/primitives/promise.js';
import { classPrimitives } from './class.js';

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
            bindings.set(name, fn); // No wrapper needed!

            // Register for hygienic macro expansion
            globalScopeRegistry.bind(name, new Set([GLOBAL_SCOPE_ID]), fn);
        }
    };

    addPrimitives(mathPrimitives);
    addPrimitives(ioPrimitives);
    addPrimitives(listPrimitives);
    addPrimitives(vectorPrimitives);
    addPrimitives(recordPrimitives);
    addPrimitives(stringPrimitives);
    addPrimitives(charPrimitives);
    addPrimitives(eqPrimitives);
    addPrimitives(getAsyncPrimitives(interpreter));
    addPrimitives(getControlPrimitives(interpreter));
    addPrimitives(GCPrimitives);
    addPrimitives(interopPrimitives);
    addPrimitives(getExceptionPrimitives(interpreter));
    addPrimitives(timePrimitives);
    addPrimitives(processContextPrimitives);
    addPrimitives(bytevectorPrimitives);
    addPrimitives(syntaxPrimitives);
    addPrimitives(getPromisePrimitives(interpreter));
    addPrimitives(classPrimitives);

    return new Environment(null, bindings);
}
