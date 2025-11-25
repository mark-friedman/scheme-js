export const listPrimitives = {
    'cons': (a, b) => [a, b],
    'car': (p) => p[0],
    'cdr': (p) => p[1],
    'list': (...args) => args,
    'null?': (a) => a === null, // Using null for '()
    'append': (...args) => {
        if (args.length === 0) return null;
        let res = args[0];
        for (let i = 1; i < args.length; i++) {
            const val = args[i];
            if (res === null) {
                res = val;
            } else if (val !== null) {
                res = res.concat(val);
            }
        }
        return res;
    }
};
