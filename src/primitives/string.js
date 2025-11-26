export const stringPrimitives = {
    'string?': (obj) => typeof obj === 'string',
    'string-append': (...args) => {
        return args.join('');
    },
    'number->string': (num) => {
        if (typeof num !== 'number') {
            throw new Error(`number->string: expected number, got ${num}`);
        }
        return String(num);
    }
};
