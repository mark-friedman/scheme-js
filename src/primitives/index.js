import { Environment } from '../core/environment.js';
import { NativeJsFunction } from '../data/values.js';
import { mathPrimitives } from './math.js';
import { ioPrimitives } from './io.js';
import { listPrimitives } from './list.js';
import { getAsyncPrimitives } from './async.js';

/**
 * Creates the global environment with built-in primitives.
 * @param {Interpreter} interpreter - A reference to the interpreter for async callbacks.
 * @returns {Environment} The global environment.
 */
export function createGlobalEnvironment(interpreter) {
    // Helper to wrap native JS functions
    const wrap = (fn) => new NativeJsFunction(fn, interpreter);

    const bindings = new Map();

    // Helper to add primitives
    const addPrimitives = (prims) => {
        for (const [name, fn] of Object.entries(prims)) {
            bindings.set(name, wrap(fn));
        }
    };

    addPrimitives(mathPrimitives);
    addPrimitives(ioPrimitives);
    addPrimitives(listPrimitives);
    addPrimitives(getAsyncPrimitives(interpreter));

    return new Environment(null, bindings);
}
