import { Literal, TailApp } from '../syntax/ast.js';

// Side effects
if (typeof window !== 'undefined') {
    window.fetchData = (successCallback) => {
        setTimeout(() => {
            console.log("JavaScript is executing the callback.");
            // The successCallback is a Scheme closure wrapped by NativeJsFunction
            successCallback("Fetched data from JS");
        }, 1000); // 1 second delay
    };
    window.globalK = null;
}

export function getAsyncPrimitives(interpreter) {
    return {
        'js-set-timeout': (cb, ms) => setTimeout(cb, ms),
        'js-fetch-data': (typeof window !== 'undefined') ? window.fetchData : () => { },

        // Stores a value (e.g., a continuation) in a global JS variable.
        'js-store-k': (k) => {
            if (typeof window !== 'undefined') window.globalK = k; // k will be a Continuation object
            return true;
        },

        // Simulates a JS event invoking a stored continuation.
        'js-invoke-k-from-js': (val) => {
            if (typeof window !== 'undefined' && window.globalK) {
                const k = window.globalK;
                // To invoke the continuation from JS, we must
                // start a *new* interpreter run that
                // has (k val) as its AST.
                const ast = new TailApp(new Literal(k), [new Literal(val)]);
                // This run will "jump" stacks.
                interpreter.run(ast, interpreter.globalEnv);
                return true;
            }
            console.error("JS: No k stored to invoke!");
            return false;
        },

        // Runs a Scheme callback from a native context.
        'js-run-callback': (cb) => {
            // cb is a Scheme Closure. The wrapper
            // in NativeJsFunction.call will turn it
            // into a callable JS function.
            cb(); // This cb() is the JS wrapper which calls interpreter.run
            return true;
        }
    };
}
