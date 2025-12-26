/**
 * Rational Number Support for Scheme.
 * 
 * Implements exact rational numbers (fractions) per R7RS §6.2.
 * Rationals are represented as numerator/denominator pairs, always reduced.
 */

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Computes the greatest common divisor using Euclidean algorithm.
 * @param {bigint|number} a 
 * @param {bigint|number} b 
 * @returns {bigint|number}
 */
function gcdNum(a, b) {
    a = Math.abs(a);
    b = Math.abs(b);
    while (b !== 0) {
        const t = b;
        b = a % b;
        a = t;
    }
    return a;
}

// =============================================================================
// Rational Class
// =============================================================================

/**
 * Represents an exact rational number (fraction).
 * 
 * Invariants:
 * - denominator is always positive
 * - numerator and denominator are always coprime (reduced form)
 * - denominator is never zero
 */
export class Rational {
    /**
     * Creates a new Rational number.
     * @param {number} numerator - The numerator (integer)
     * @param {number} denominator - The denominator (positive integer)
     */
    constructor(numerator, denominator = 1) {
        if (!Number.isInteger(numerator)) {
            throw new Error('Rational: numerator must be an integer');
        }
        if (!Number.isInteger(denominator)) {
            throw new Error('Rational: denominator must be an integer');
        }
        if (denominator === 0) {
            throw new Error('Rational: division by zero');
        }

        // Ensure denominator is positive
        if (denominator < 0) {
            numerator = -numerator;
            denominator = -denominator;
        }

        // Reduce to lowest terms
        const g = gcdNum(numerator, denominator);
        this.numerator = numerator / g;
        this.denominator = denominator / g;
    }

    /**
     * Creates a Rational from a number (if exact integer).
     * @param {number} n 
     * @returns {Rational}
     */
    static fromNumber(n) {
        if (Number.isInteger(n)) {
            return new Rational(n, 1);
        }
        throw new Error('Cannot convert inexact number to exact rational');
    }

    /**
     * Returns true if this rational represents an integer.
     * @returns {boolean}
     */
    isInteger() {
        return this.denominator === 1;
    }

    /**
     * Converts to JavaScript number (may lose precision).
     * @returns {number}
     */
    toNumber() {
        return this.numerator / this.denominator;
    }

    /**
     * Returns the rational's string representation.
     * @returns {string}
     */
    toString() {
        if (this.denominator === 1) {
            return String(this.numerator);
        }
        return `${this.numerator}/${this.denominator}`;
    }

    /**
     * Adds two rationals.
     * @param {Rational} other 
     * @returns {Rational}
     */
    add(other) {
        const num = this.numerator * other.denominator + other.numerator * this.denominator;
        const den = this.denominator * other.denominator;
        return new Rational(num, den);
    }

    /**
     * Subtracts another rational from this one.
     * @param {Rational} other 
     * @returns {Rational}
     */
    subtract(other) {
        const num = this.numerator * other.denominator - other.numerator * this.denominator;
        const den = this.denominator * other.denominator;
        return new Rational(num, den);
    }

    /**
     * Multiplies two rationals.
     * @param {Rational} other 
     * @returns {Rational}
     */
    multiply(other) {
        return new Rational(
            this.numerator * other.numerator,
            this.denominator * other.denominator
        );
    }

    /**
     * Divides this rational by another.
     * @param {Rational} other 
     * @returns {Rational}
     */
    divide(other) {
        if (other.numerator === 0) {
            throw new Error('Rational: division by zero');
        }
        return new Rational(
            this.numerator * other.denominator,
            this.denominator * other.numerator
        );
    }

    /**
     * Returns the negation of this rational.
     * @returns {Rational}
     */
    negate() {
        return new Rational(-this.numerator, this.denominator);
    }

    /**
     * Returns the absolute value.
     * @returns {Rational}
     */
    abs() {
        return new Rational(Math.abs(this.numerator), this.denominator);
    }

    /**
     * Compares this rational to another.
     * @param {Rational} other 
     * @returns {number} -1, 0, or 1
     */
    compareTo(other) {
        const diff = this.numerator * other.denominator - other.numerator * this.denominator;
        if (diff < 0) return -1;
        if (diff > 0) return 1;
        return 0;
    }

    /**
     * Checks equality with another rational.
     * @param {Rational} other 
     * @returns {boolean}
     */
    equals(other) {
        return this.numerator === other.numerator &&
            this.denominator === other.denominator;
    }
}

/**
 * Checks if a value is a Rational.
 * @param {*} val 
 * @returns {boolean}
 */
export function isRational(val) {
    return val instanceof Rational;
}

/**
 * Creates a Rational from numerator and denominator.
 * @param {number} num 
 * @param {number} den 
 * @returns {Rational}
 */
export function makeRational(num, den) {
    return new Rational(num, den);
}
