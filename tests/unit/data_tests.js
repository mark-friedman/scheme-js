import { Cons, cons, list } from '../../src/layer-1-kernel/cons.js';
import { Symbol, intern } from '../../src/layer-1-kernel/symbol.js';
import { assert } from '../helpers.js';

export function runDataTests(logger) {
    logger.title('Running Data Structure Unit Tests...');

    // --- Symbol Tests ---
    try {
        const s1 = intern('foo');
        const s2 = intern('foo');
        const s3 = intern('bar');

        assert(logger, "Symbol: intern uniqueness", s1 === s2, true);
        assert(logger, "Symbol: distinct symbols", s1 !== s3, true);
        assert(logger, "Symbol: name property", s1.name, 'foo');
        assert(logger, "Symbol: toString", s1.toString(), 'foo');
    } catch (e) {
        logger.fail(`Symbol tests failed: ${e.message}`);
    }

    // --- Cons Tests ---
    try {
        const c1 = cons(1, 2);
        assert(logger, "Cons: car", c1.car, 1);
        assert(logger, "Cons: cdr", c1.cdr, 2);
        assert(logger, "Cons: is pair", c1 instanceof Cons, true);

        const l1 = list(1, 2, 3);
        assert(logger, "List: construction", l1 instanceof Cons, true);
        assert(logger, "List: structure car", l1.car, 1);
        assert(logger, "List: structure cadr", l1.cdr.car, 2);
        assert(logger, "List: structure caddr", l1.cdr.cdr.car, 3);
        assert(logger, "List: structure null termination", l1.cdr.cdr.cdr, null);

        const arr = l1.toArray();
        assert(logger, "Cons: toArray", arr, [1, 2, 3]);

        // Improper list toArray
        const improper = cons(1, 2);
        const improperArr = improper.toArray();
        // [1, Symbol(.), 2]
        assert(logger, "Cons: toArray improper list length", improperArr.length, 3);
        assert(logger, "Cons: toArray improper list dot", improperArr[1].name, '.');
        assert(logger, "Cons: toArray improper list tail", improperArr[2], 2);

    } catch (e) {
        logger.fail(`Cons tests failed: ${e.message}`);
    }
}
