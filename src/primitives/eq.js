export const eqPrimitives = {
    'eq?': (a, b) => a === b,
    'eqv?': (a, b) => Object.is(a, b)
};
