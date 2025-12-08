

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
            if (typeof window !== 'undefined') window.globalK = k; // k will be a Bridge function (if passed from Scheme)
            return true;
        }
    };
}
