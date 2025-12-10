export const mathPrimitives = {
    '+': (...args) => args.reduce((a, b) => a + b, 0),
    '-': (first, ...rest) => {
        if (rest.length === 0) return -first;
        return rest.reduce((a, b) => a - b, first);
    },
    '*': (...args) => args.reduce((a, b) => a * b, 1),
    '/': (first, ...rest) => {
        if (rest.length === 0) return 1 / first;
        return rest.reduce((a, b) => a / b, first);
    },
    '=': (a, b) => a === b,
    '<': (a, b) => a < b,
    '>': (a, b) => a > b,
    'modulo': (a, b) => a % b,
};
