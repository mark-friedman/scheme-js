/**
 * Unit Tests for SchemeError Classes and Type Checking Utilities
 */

import {
    SchemeError,
    SchemeTypeError,
    SchemeArityError,
    SchemeRangeError,
    getTypeName
} from '../../../src/core/interpreter/errors.js';

import {
    isPair, isList, isNumber, isInteger, isString, isSymbol, isVector, isProcedure,
    assertType, assertPair, assertList, assertNumber, assertInteger, assertString,
    assertArity, assertRange, assertIndex
} from '../../../src/core/interpreter/type_check.js';

import { Cons, cons, list } from '../../../src/core/interpreter/cons.js';
import { Symbol } from '../../../src/core/interpreter/symbol.js';
import { assert } from '../../harness/helpers.js';

export function runErrorTests(logger) {
    logger.title('Running Error Class and Type Check Unit Tests...');

    // === Error Class Tests ===

    // SchemeError
    {
        const err = new SchemeError('test message', [1, 2, 3]);
        const pass = err.message === 'test message' &&
            err.irritants.length === 3 &&
            err instanceof Error;
        assert(logger, 'SchemeError captures message and irritants', pass, true);
    }

    // SchemeTypeError
    {
        const err = new SchemeTypeError('car', 1, 'pair', 42);
        const pass = err.message.includes('car') &&
            err.message.includes('pair') &&
            err.message.includes('number') &&
            err.argPos === 1;
        assert(logger, 'SchemeTypeError includes proc, expected, got', pass, true);
    }

    // SchemeArityError
    {
        const err = new SchemeArityError('cons', 2, 2, 1);
        const pass = err.message.includes('cons') &&
            err.message.includes('expected 2') &&
            err.message.includes('got 1');
        assert(logger, 'SchemeArityError reports expected vs actual', pass, true);
    }

    // SchemeArityError variadic
    {
        const err = new SchemeArityError('list', 0, Infinity, 0);
        const pass = err.message.includes('at least 0');
        assert(logger, 'SchemeArityError variadic message', pass, true);
    }

    // SchemeRangeError
    {
        const err = new SchemeRangeError('vector-ref', 'index', 0, 5, 10);
        const pass = err.message.includes('vector-ref') &&
            err.message.includes('out of range') &&
            err.message.includes('10');
        assert(logger, 'SchemeRangeError includes bounds and actual', pass, true);
    }

    // getTypeName
    {
        const tests = [
            [null, 'null'],
            [42, 'number'],
            ['hello', 'string'],
            [true, 'boolean'],
            [new Symbol('x'), 'symbol'],
            [cons(1, 2), 'pair'],
            [[1, 2, 3], 'vector'],
            [() => { }, 'procedure']
        ];
        let allPass = true;
        for (const [val, expected] of tests) {
            if (getTypeName(val) !== expected) allPass = false;
        }
        assert(logger, 'getTypeName returns correct type names', allPass, true);
    }

    // === Type Predicate Tests ===

    // isPair
    {
        const pass = isPair(cons(1, 2)) && !isPair(null) && !isPair([1, 2]);
        assert(logger, 'isPair identifies cons cells', pass, true);
    }

    // isList
    {
        const pass = isList(null) &&
            isList(list(1, 2, 3)) &&
            !isList(cons(1, 2)); // improper list
        assert(logger, 'isList accepts null and proper lists', pass, true);
    }

    // isNumber
    {
        const pass = isNumber(42) && isNumber(3.14) && !isNumber('42');
        assert(logger, 'isNumber identifies numbers', pass, true);
    }

    // isInteger
    {
        const pass = isInteger(42) && !isInteger(3.14) && !isInteger('42');
        assert(logger, 'isInteger identifies integers', pass, true);
    }

    // isString
    {
        const pass = isString('hello') && !isString(42) && !isString(new Symbol('x'));
        assert(logger, 'isString identifies strings', pass, true);
    }

    // isSymbol
    {
        const pass = isSymbol(new Symbol('x')) && !isSymbol('x');
        assert(logger, 'isSymbol identifies symbols', pass, true);
    }

    // isVector (vectors are JS arrays in this implementation)
    {
        const pass = isVector([1, 2, 3]) && !isVector(cons(1, 2));
        assert(logger, 'isVector identifies arrays', pass, true);
    }

    // isProcedure
    {
        const pass = isProcedure(() => { }) && !isProcedure(42);
        assert(logger, 'isProcedure identifies functions', pass, true);
    }

    // === Assertion Helper Tests ===

    // assertPair throws on non-pair
    {
        let threw = false;
        try {
            assertPair('car', 1, 42);
        } catch (e) {
            threw = e instanceof SchemeTypeError && e.procName === 'car';
        }
        assert(logger, 'assertPair throws SchemeTypeError for non-pair', threw, true);
    }

    // assertPair returns pair
    {
        const p = cons(1, 2);
        const result = assertPair('car', 1, p);
        assert(logger, 'assertPair returns pair on success', result === p, true);
    }

    // assertList accepts null
    {
        let threw = false;
        try {
            assertList('length', 1, null);
        } catch (e) {
            threw = true;
        }
        assert(logger, 'assertList accepts null as empty list', threw, false);
    }

    // assertArity throws on wrong count
    {
        let threw = false;
        try {
            assertArity('cons', [1], 2, 2);
        } catch (e) {
            threw = e instanceof SchemeArityError;
        }
        assert(logger, 'assertArity throws SchemeArityError', threw, true);
    }

    // assertRange throws on out of range
    {
        let threw = false;
        try {
            assertRange('vector-ref', 'index', 10, 0, 5);
        } catch (e) {
            threw = e instanceof SchemeRangeError;
        }
        assert(logger, 'assertRange throws SchemeRangeError', threw, true);
    }

    // assertIndex
    {
        let threw = false;
        try {
            assertIndex('vector-ref', 10, 5);
        } catch (e) {
            threw = e instanceof SchemeRangeError;
        }
        assert(logger, 'assertIndex throws on invalid index', threw, true);
    }
}
