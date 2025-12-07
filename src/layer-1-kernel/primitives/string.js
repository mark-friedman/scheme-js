import { intern, Symbol } from '../symbol.js';

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
    },
    'string->symbol': (str) => {
        if (typeof str !== 'string') throw new Error(`string->symbol: expected string, got ${str}`);
        return intern(str);
    },
    'symbol->string': (sym) => {
        if (!(sym instanceof Symbol)) throw new Error(`symbol->string: expected symbol, got ${sym}`);
        return sym.name;
    }
};
