import { Environment } from '../environment.js';

import { mathPrimitives } from './math.js';
import { ioPrimitives } from './io.js';
import { listPrimitives } from './list.js';
import { vectorPrimitives } from './vector.js';
import { recordPrimitives } from './record.js';
import { stringPrimitives } from './string.js';
import { eqPrimitives } from './eq.js';
import { getAsyncPrimitives } from './async.js';
import { getControlPrimitives } from './control.js';
import { GCPrimitives } from './gc.js';
import { interopPrimitives } from './interop.js';

/**
 * Creates the global environment with built-in primitives.
 * @param {Interpreter} interpreter - A reference to the interpreter for async callbacks.
 * @returns {Environment} The global environment.
 */
export function createGlobalEnvironment(interpreter) {
    const bindings = new Map();

    // Helper to add primitives
    const addPrimitives = (prims) => {
        for (const [name, fn] of Object.entries(prims)) {
            bindings.set(name, fn); // No wrapper needed!
        }
    };

    addPrimitives(mathPrimitives);
    addPrimitives(ioPrimitives);
    addPrimitives(listPrimitives);
    addPrimitives(vectorPrimitives);
    addPrimitives(recordPrimitives);
    addPrimitives(stringPrimitives);
    addPrimitives(eqPrimitives);
    addPrimitives(getAsyncPrimitives(interpreter));
    addPrimitives(getControlPrimitives(interpreter));
    addPrimitives(GCPrimitives);
    addPrimitives(interopPrimitives);

    return new Environment(null, bindings);
}
