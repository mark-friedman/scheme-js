/**
 * Complex Number Support for Scheme.
 * 
 * Implements complex numbers per R7RS §6.2 and (scheme complex) library.
 * Complex numbers have real and imaginary parts.
 */

// =============================================================================
// Complex Class
// =============================================================================

/**
 * Represents a complex number with real and imaginary parts.
 */
export class Complex {
    /**
     * Creates a new Complex number.
     * @param {number} real - The real part
     * @param {number} imag - The imaginary part
     */
    constructor(real, imag) {
        if (typeof real !== 'number' || typeof imag !== 'number') {
            throw new Error('Complex: real and imaginary parts must be numbers');
        }
        this.real = real;
        this.imag = imag;
    }

    /**
     * Creates a Complex from rectangular coordinates.
     * @param {number} x - Real part
     * @param {number} y - Imaginary part
     * @returns {Complex}
     */
    static fromRectangular(x, y) {
        return new Complex(x, y);
    }

    /**
     * Creates a Complex from polar coordinates.
     * @param {number} magnitude - Magnitude (r)
     * @param {number} angle - Angle in radians (θ)
     * @returns {Complex}
     */
    static fromPolar(magnitude, angle) {
        return new Complex(
            magnitude * Math.cos(angle),
            magnitude * Math.sin(angle)
        );
    }

    /**
     * Returns true if this is a real number (imaginary part is 0).
     * @returns {boolean}
     */
    isReal() {
        return this.imag === 0;
    }

    /**
     * Returns true if this is purely imaginary (real part is 0).
     * @returns {boolean}
     */
    isImaginary() {
        return this.real === 0 && this.imag !== 0;
    }

    /**
     * Returns the real part.
     * @returns {number}
     */
    realPart() {
        return this.real;
    }

    /**
     * Returns the imaginary part.
     * @returns {number}
     */
    imagPart() {
        return this.imag;
    }

    /**
     * Returns the magnitude (absolute value).
     * @returns {number}
     */
    magnitude() {
        return Math.sqrt(this.real * this.real + this.imag * this.imag);
    }

    /**
     * Returns the angle (argument/phase) in radians.
     * @returns {number}
     */
    angle() {
        return Math.atan2(this.imag, this.real);
    }

    /**
     * Converts to JavaScript number (only if real).
     * @returns {number}
     */
    toNumber() {
        if (this.imag === 0) {
            return this.real;
        }
        throw new Error('Cannot convert complex with non-zero imaginary part to real');
    }

    /**
     * Returns the string representation with R7RS formatting for special values.
     * @param {number} [radix=10]
     * @returns {string}
     */
    toString(radix = 10) {
        const formatComp = (n) => {
            if (n === Infinity) return '+inf.0';
            if (n === -Infinity) return '-inf.0';
            if (Number.isNaN(n)) return '+nan.0';
            return n.toString(radix);
        };

        if (this.imag === 0) {
            return formatComp(this.real);
        }
        if (this.real === 0) {
            // Pure imaginary
            if (this.imag === 1) return '+i';
            if (this.imag === -1) return '-i';
            return `${formatComp(this.imag)}i`;
        }

        const realStr = formatComp(this.real);
        const imagStr = formatComp(Math.abs(this.imag));
        const sign = this.imag >= 0 ? '+' : '-';

        if (this.imag === 1) return `${realStr}+i`;
        if (this.imag === -1) return `${realStr}-i`;
        return `${realStr}${sign}${imagStr}i`;
    }

    /**
     * Returns the complex conjugate.
     * @returns {Complex}
     */
    conjugate() {
        return new Complex(this.real, -this.imag);
    }

    /**
     * Adds two complex numbers.
     * @param {Complex} other 
     * @returns {Complex}
     */
    add(other) {
        return new Complex(
            this.real + other.real,
            this.imag + other.imag
        );
    }

    /**
     * Subtracts another complex from this one.
     * @param {Complex} other 
     * @returns {Complex}
     */
    subtract(other) {
        return new Complex(
            this.real - other.real,
            this.imag - other.imag
        );
    }

    /**
     * Multiplies two complex numbers.
     * (a + bi)(c + di) = (ac - bd) + (ad + bc)i
     * @param {Complex} other 
     * @returns {Complex}
     */
    multiply(other) {
        return new Complex(
            this.real * other.real - this.imag * other.imag,
            this.real * other.imag + this.imag * other.real
        );
    }

    /**
     * Divides this complex by another.
     * @param {Complex} other 
     * @returns {Complex}
     */
    divide(other) {
        const denom = other.real * other.real + other.imag * other.imag;
        if (denom === 0) {
            throw new Error('Complex: division by zero');
        }
        return new Complex(
            (this.real * other.real + this.imag * other.imag) / denom,
            (this.imag * other.real - this.real * other.imag) / denom
        );
    }

    /**
     * Returns the negation.
     * @returns {Complex}
     */
    negate() {
        return new Complex(-this.real, -this.imag);
    }

    /**
     * Checks equality with another complex.
     * @param {Complex} other 
     * @returns {boolean}
     */
    equals(other) {
        return this.real === other.real && this.imag === other.imag;
    }
}

/**
 * Checks if a value is a Complex number.
 * @param {*} val 
 * @returns {boolean}
 */
export function isComplex(val) {
    return val instanceof Complex;
}

/**
 * Creates a Complex from rectangular coordinates (make-rectangular).
 * @param {number} x - Real part
 * @param {number} y - Imaginary part
 * @returns {Complex}
 */
export function makeRectangular(x, y) {
    // If imaginary part is 0, could return just the real number
    // but R7RS says make-rectangular always returns a complex
    return new Complex(x, y);
}

/**
 * Creates a Complex from polar coordinates (make-polar).
 * @param {number} magnitude 
 * @param {number} angle - In radians
 * @returns {Complex}
 */
export function makePolar(magnitude, angle) {
    return Complex.fromPolar(magnitude, angle);
}
