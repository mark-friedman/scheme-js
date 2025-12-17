import { listPrimitives } from '../../../src/core/primitives/list.js';
import { vectorPrimitives } from '../../../src/core/primitives/vector.js';
import { Cons, cons, list } from '../../../src/core/interpreter/cons.js';
import { assert } from '../../helpers.js';

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
        logger.fail(`List primitive tests failed: ${e.message}`);
    }

    // --- Vector Primitives ---
    logger.title('Running Vector Primitive Unit Tests...');
    const {
        vector, 'make-vector': makeVector, 'vector?': isVector,
        'vector-length': vectorLength, 'vector-ref': vectorRef,
        'vector-set!': vectorSet, 'vector->list': vectorToList,
        'list->vector': listToVector
    } = vectorPrimitives;

    try {
        // Construction
        const v1 = vector(1, 2, 3);
        assert(logger, "Primitives: vector", Array.isArray(v1) && v1.length === 3, true);
        assert(logger, "Primitives: vector content", v1[0] === 1 && v1[2] === 3, true);

        const v2 = makeVector(3, 'a');
        assert(logger, "Primitives: make-vector", Array.isArray(v2) && v2.length === 3, true);
        assert(logger, "Primitives: make-vector content", v2[0] === 'a' && v2[2] === 'a', true);

        // Predicates
        assert(logger, "Primitives: vector? true", isVector(v1), true);
        assert(logger, "Primitives: vector? false (list)", isVector(list(1, 2)), false);
        assert(logger, "Primitives: vector? false (number)", isVector(123), false);

        // Accessors
        assert(logger, "Primitives: vector-length", vectorLength(v1), 3);
        assert(logger, "Primitives: vector-ref", vectorRef(v1, 1), 2);

        // Mutation
        vectorSet(v1, 1, 99);
        assert(logger, "Primitives: vector-set!", vectorRef(v1, 1), 99);

        // Conversions
        const l1 = vectorToList(v1);
        assert(logger, "Primitives: vector->list", l1.toArray(), [1, 99, 3]);

        const v3 = listToVector(l1);
        assert(logger, "Primitives: list->vector", v3[1], 99);

        // Error handling
        try {
            vectorRef([1], 5);
            logger.fail("Primitives: vector-ref out of bounds failed to throw");
        } catch (e) {
            assert(logger, "Primitives: vector-ref out of bounds", e.message.includes("out of range"), true);
        }

        try {
            makeVector(-1, 0);
            logger.fail("Primitives: make-vector negative size failed to throw");
        } catch (e) {
            assert(logger, "Primitives: make-vector negative size", e.message.includes("out of range"), true);
        }

    } catch (e) {
        logger.fail(`Vector primitive tests failed: ${e.message}`);
    }
}
