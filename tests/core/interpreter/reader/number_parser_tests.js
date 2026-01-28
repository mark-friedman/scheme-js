/**
 * @fileoverview Unit tests for reader/number_parser.js
 */

import { parseNumber, parsePrefixedNumber } from '../../../../src/core/interpreter/reader/number_parser.js';
import { Rational } from '../../../../src/core/primitives/rational.js';
import { Complex } from '../../../../src/core/primitives/complex.js';
import { assert } from '../../../harness/helpers.js';

export function runNumberParserTests(logger) {
    logger.title('parseNumber - integers');

    assert(logger, 'positive integer', parseNumber('42'), 42);
    assert(logger, 'negative integer', parseNumber('-42'), -42);
    assert(logger, 'zero', parseNumber('0'), 0);

    logger.title('parseNumber - decimals');

    assert(logger, 'decimal', parseNumber('3.14'), 3.14);
    assert(logger, 'negative decimal', parseNumber('-0.5'), -0.5);
    assert(logger, 'leading dot', parseNumber('.5'), 0.5);
    assert(logger, 'trailing dot', parseNumber('2.'), 2);

    logger.title('parseNumber - scientific');

    assert(logger, 'lowercase e', parseNumber('1e2'), 100);
    assert(logger, 'uppercase E', parseNumber('1E2'), 100);
    assert(logger, 'positive exponent', parseNumber('1e+2'), 100);
    assert(logger, 'negative exponent', parseNumber('1e-2'), 0.01);
    assert(logger, 'short precision s', parseNumber('1s2'), 100);
    assert(logger, 'single precision f', parseNumber('1f2'), 100);
    assert(logger, 'double precision d', parseNumber('1d2'), 100);
    assert(logger, 'long precision l', parseNumber('1l2'), 100);

    logger.title('parseNumber - special values');

    assert(logger, 'positive infinity', parseNumber('+inf.0'), Infinity);
    assert(logger, 'negative infinity', parseNumber('-inf.0'), -Infinity);
    assert(logger, 'positive NaN', Number.isNaN(parseNumber('+nan.0')), true);
    assert(logger, 'negative NaN', Number.isNaN(parseNumber('-nan.0')), true);

    logger.title('parseNumber - rationals');

    {
        const rat = parseNumber('1/2');
        assert(logger, '1/2 is Rational', rat instanceof Rational, true);
        assert(logger, '1/2 numerator', rat.numerator, 1);
        assert(logger, '1/2 denominator', rat.denominator, 2);
    }

    {
        const rat = parseNumber('-3/4');
        assert(logger, '-3/4 is Rational', rat instanceof Rational, true);
        assert(logger, '-3/4 numerator', rat.numerator, -3);
        assert(logger, '-3/4 denominator', rat.denominator, 4);
    }

    logger.title('parseNumber - complex');

    {
        const c = parseNumber('1+2i');
        assert(logger, '1+2i is Complex', c instanceof Complex, true);
        assert(logger, '1+2i real', c.real, 1);
        assert(logger, '1+2i imag', c.imag, 2);
    }

    {
        const c = parseNumber('3-4i');
        assert(logger, '3-4i is Complex', c instanceof Complex, true);
        assert(logger, '3-4i real', c.real, 3);
        assert(logger, '3-4i imag', c.imag, -4);
    }

    {
        const c = parseNumber('+i');
        assert(logger, '+i is Complex', c instanceof Complex, true);
        assert(logger, '+i real', c.real, 0);
        assert(logger, '+i imag', c.imag, 1);
    }

    {
        const c = parseNumber('-i');
        assert(logger, '-i is Complex', c instanceof Complex, true);
        assert(logger, '-i real', c.real, 0);
        assert(logger, '-i imag', c.imag, -1);
    }

    logger.title('parseNumber - non-numbers');

    assert(logger, 'symbol returns null', parseNumber('hello'), null);

    logger.title('parsePrefixedNumber - radix');

    assert(logger, 'hex ff = 255', parsePrefixedNumber('#xff'), 255);
    assert(logger, 'hex 10 = 16', parsePrefixedNumber('#x10'), 16);
    assert(logger, 'octal 10 = 8', parsePrefixedNumber('#o10'), 8);
    assert(logger, 'octal 77 = 63', parsePrefixedNumber('#o77'), 63);
    assert(logger, 'binary 10 = 2', parsePrefixedNumber('#b10'), 2);
    assert(logger, 'binary 1111 = 15', parsePrefixedNumber('#b1111'), 15);
    assert(logger, 'decimal prefix', parsePrefixedNumber('#d42'), 42);

    logger.title('parsePrefixedNumber - exactness');

    assert(logger, 'exact integer', parsePrefixedNumber('#e10'), 10);
    assert(logger, 'inexact integer', parsePrefixedNumber('#i10'), 10);

    logger.title('parsePrefixedNumber - combined');

    assert(logger, 'exact hex', parsePrefixedNumber('#e#x10'), 16);
    assert(logger, 'hex exact (reversed)', parsePrefixedNumber('#x#e10'), 16);

    logger.title('parsePrefixedNumber - special');

    assert(logger, 'decimal inf', parsePrefixedNumber('#d+inf.0'), Infinity);
    assert(logger, 'decimal nan', Number.isNaN(parsePrefixedNumber('#d+nan.0')), true);
}

export default runNumberParserTests;
