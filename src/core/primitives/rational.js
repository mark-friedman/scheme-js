/**
 * Rational Number Support for Scheme.
 * 
 * Implements exact rational numbers (fractions) per R7RS §6.2.
 * Rationals are represented as numerator/denominator pairs using BigInt for
 * arbitrary precision. They maintain an `exact` flag for R7RS compliance.
 */

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Computes the greatest common divisor using Euclidean algorithm.
 * Works with BigInt.
 * @param {bigint} a 
 * @param {bigint} b 
 * @returns {bigint}
 */
function gcdBigInt(a, b) {
    a = a < 0n ? -a : a;
    b = b < 0n ? -b : b;
    while (b !== 0n) {
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
 * Represents a rational number (fraction).
 * 
 * Invariants:
 * - numerator and denominator are always BigInt
 * - denominator is always positive
 * - numerator and denominator are always coprime (reduced form)
 * - denominator is never zero
 */
export class Rational {
    /**
     * Creates a new Rational number.
     * @param {bigint|number} numerator - The numerator (will be converted to BigInt)
     * @param {bigint|number} denominator - The denominator (will be converted to BigInt)
     * @param {boolean} exact - Whether this rational is exact (default: true)
     */
    constructor(numerator, denominator = 1n, exact = true) {
        // Convert to BigInt if needed
        if (typeof numerator === 'number') {
            if (!Number.isInteger(numerator)) {
                throw new Error('Rational: numerator must be an integer');
            }
            numerator = BigInt(numerator);
        }
        if (typeof denominator === 'number') {
            if (!Number.isInteger(denominator)) {
                throw new Error('Rational: denominator must be an integer');
            }
            denominator = BigInt(denominator);
        }

        if (typeof numerator !== 'bigint') {
            throw new Error('Rational: numerator must be an integer or BigInt');
        }
        if (typeof denominator !== 'bigint') {
            throw new Error('Rational: denominator must be an integer or BigInt');
        }
        if (denominator === 0n) {
            throw new Error('Rational: division by zero');
        }

        // Ensure denominator is positive
        if (denominator < 0n) {
            numerator = -numerator;
            denominator = -denominator;
        }

        // Reduce to lowest terms
        const g = gcdBigInt(numerator, denominator);
        this.numerator = numerator / g;
        this.denominator = denominator / g;
        this.exact = exact;
    }

    /**
     * Creates a Rational from a number (if exact integer).
     * @param {number|bigint} n 
     * @returns {Rational}
     */
    static fromNumber(n) {
        if (typeof n === 'bigint') {
            return new Rational(n, 1n, true);
        }
        if (Number.isInteger(n)) {
            return new Rational(BigInt(n), 1n, true);
        }
        throw new Error('Cannot convert inexact number to exact rational');
    }

    /**
     * Returns true if this rational represents an integer.
     * @returns {boolean}
     */
    isInteger() {
        return this.denominator === 1n;
    }

    /**
     * Converts to JavaScript number (may lose precision).
     * @returns {number}
     */
    toNumber() {
        return Number(this.numerator) / Number(this.denominator);
    }

    /**
     * Returns the rational's string representation.
     * @returns {string}
     */
    toString() {
        if (this.denominator === 1n) {
            return String(this.numerator);
        }
        return `${this.numerator}/${this.denominator}`;
    }

    /**
     * Adds two rationals.
     * @param {Rational|bigint|number} other 
     * @returns {Rational}
     */
    add(other) {
        other = Rational._coerce(other);
        const num = this.numerator * other.denominator + other.numerator * this.denominator;
        const den = this.denominator * other.denominator;
        return new Rational(num, den, this.exact && other.exact);
    }

    /**
     * Subtracts another rational from this one.
     * @param {Rational|bigint|number} other 
     * @returns {Rational}
     */
    subtract(other) {
        other = Rational._coerce(other);
        const num = this.numerator * other.denominator - other.numerator * this.denominator;
        const den = this.denominator * other.denominator;
        return new Rational(num, den, this.exact && other.exact);
    }

    /**
     * Multiplies two rationals.
     * @param {Rational|bigint|number} other 
     * @returns {Rational}
     */
    multiply(other) {
        other = Rational._coerce(other);
        return new Rational(
            this.numerator * other.numerator,
            this.denominator * other.denominator,
            this.exact && other.exact
        );
    }

    /**
     * Divides this rational by another.
     * @param {Rational|bigint|number} other 
     * @returns {Rational}
     */
    divide(other) {
        other = Rational._coerce(other);
        if (other.numerator === 0n) {
            throw new Error('Rational: division by zero');
        }
        return new Rational(
            this.numerator * other.denominator,
            this.denominator * other.numerator,
            this.exact && other.exact
        );
    }

    /**
     * Returns the negation of this rational.
     * @returns {Rational}
     */
    negate() {
        return new Rational(-this.numerator, this.denominator, this.exact);
    }

    /**
     * Returns the absolute value.
     * @returns {Rational}
     */
    abs() {
        const absNum = this.numerator < 0n ? -this.numerator : this.numerator;
        return new Rational(absNum, this.denominator, this.exact);
    }

    /**
     * Compares this rational to another.
     * @param {Rational|bigint|number} other 
     * @returns {number} -1, 0, or 1
     */
    compareTo(other) {
        other = Rational._coerce(other);
        const diff = this.numerator * other.denominator - other.numerator * this.denominator;
        if (diff < 0n) return -1;
        if (diff > 0n) return 1;
        return 0;
    }

    /**
     * Checks equality with another rational.
     * @param {Rational|bigint|number} other 
     * @returns {boolean}
     */
    equals(other) {
        other = Rational._coerce(other);
        return this.numerator === other.numerator &&
            this.denominator === other.denominator;
    }

    /**
     * Coerces a value to Rational.
     * @param {Rational|bigint|number} val
     * @returns {Rational}
     * @private
     */
    static _coerce(val) {
        if (val instanceof Rational) return val;
        if (typeof val === 'bigint') return new Rational(val, 1n, true);
        if (typeof val === 'number' && Number.isInteger(val)) {
            return new Rational(BigInt(val), 1n, false); // Number is inexact
        }
        throw new Error('Rational: cannot coerce non-integer to Rational');
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
 * @param {bigint|number} num 
 * @param {bigint|number} den 
 * @returns {Rational}
 */
export function makeRational(num, den) {
    return new Rational(num, den);
}
