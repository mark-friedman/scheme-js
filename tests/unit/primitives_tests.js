import { listPrimitives } from '../../src/primitives/list.js';
import { Cons, cons, list } from '../../src/data/cons.js';
import { assert } from '../helpers.js';

export function runPrimitiveTests(logger) {
    logger.title('Running List Primitive Unit Tests...');

    const {
        car, cdr, cons: pCons, list: pList,
        'pair?': isPair, 'null?': isNull,
        'set-car!': setCar, 'set-cdr!': setCdr,
        append
    } = listPrimitives;

    try {
        // Basic accessors
        const p1 = pCons(1, 2);
        assert(logger, "Primitives: cons", p1 instanceof Cons, true);
        assert(logger, "Primitives: car", car(p1), 1);
        assert(logger, "Primitives: cdr", cdr(p1), 2);

        // Type checks
        assert(logger, "Primitives: pair? true", isPair(p1), true);
        assert(logger, "Primitives: pair? false", isPair(null), false);
        assert(logger, "Primitives: pair? false (atom)", isPair(1), false);
        assert(logger, "Primitives: null? true", isNull(null), true);
        assert(logger, "Primitives: null? false", isNull(p1), false);

        // Mutation
        setCar(p1, 10);
        assert(logger, "Primitives: set-car!", p1.car, 10);
        setCdr(p1, 20);
        assert(logger, "Primitives: set-cdr!", p1.cdr, 20);

        // List construction
        const l1 = pList(1, 2, 3);
        assert(logger, "Primitives: list", l1.toArray(), [1, 2, 3]);

        // Append
        const l2 = pList(4, 5);
        const l3 = append(l1, l2);
        assert(logger, "Primitives: append", l3.toArray(), [1, 2, 3, 4, 5]);

        // Append empty
        assert(logger, "Primitives: append empty", append(l1, null).toArray(), [1, 2, 3]);
        assert(logger, "Primitives: append null first", append(null, l1).toArray(), [1, 2, 3]);

        // Append improper
        const l4 = append(l1, 99);
        // (1 2 3 . 99)
        assert(logger, "Primitives: append improper car", l4.car, 1);
        assert(logger, "Primitives: append improper cdr chain", l4.cdr.cdr.cdr, 99);

    } catch (e) {
        logger.fail(`Primitive tests failed: ${e.message}`);
    }
}
