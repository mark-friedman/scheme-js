
export const interopPrimitives = {
    'js-eval': (code) => {
        // Evaluate the string as JavaScript code in the global scope
        // We use Function constructor to avoid direct eval usage if possible, 
        // but for "evaluating code in global context" indirect eval is often used.
        // However, standard JS 'eval' is what we want for "execute this string".
        // To run in global scope, we can use (1, eval)(code).
        return (1, eval)(code);
    }
};
