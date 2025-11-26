import { Environment } from '../core/environment.js';

import { mathPrimitives } from './math.js';
import { ioPrimitives } from './io.js';
import { listPrimitives } from './list.js';
import { vectorPrimitives } from './vector.js';
import { getAsyncPrimitives } from './async.js';

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
    addPrimitives(getAsyncPrimitives(interpreter));

    return new Environment(null, bindings);
}
