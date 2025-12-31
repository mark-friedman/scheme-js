/**
 * Complex Number Support for Scheme.
 * 
 * Implements complex numbers per R7RS §6.2 and (scheme complex) library.
 * Complex numbers have real and imaginary parts.
 */

import { Rational } from './rational.js';

// Helper for component arithmetic (Number/Rational/BigInt)
function add(a, b) {
    // Basic types (BigInt/Number)
    if (typeof a === 'bigint' && typeof b === 'bigint') return a + b;
    if (typeof a === 'number' && typeof b === 'number') return a + b;

    // Rational handling
    if (a instanceof Rational) return a.add(b instanceof Rational ? b : Rational.fromNumber(b));
    if (b instanceof Rational) return b.add(a instanceof Rational ? a : Rational.fromNumber(a));

    // Mixed BigInt/Number -> Inexact
    if (typeof a === 'bigint') return Number(a) + b;
    if (typeof b === 'bigint') return a + Number(b);

    return a + b;
}

function sub(a, b) {
    if (typeof a === 'bigint' && typeof b === 'bigint') return a - b;
    if (typeof a === 'number' && typeof b === 'number') return a - b;

    if (a instanceof Rational) return a.subtract(b instanceof Rational ? b : Rational.fromNumber(b));
    if (b instanceof Rational) return Rational.fromNumber(a).subtract(b);

    if (typeof a === 'bigint') return Number(a) - b;
    if (typeof b === 'bigint') return a - Number(b);
    return a - b;
}

function mul(a, b) {
    if (typeof a === 'bigint' && typeof b === 'bigint') return a * b;
    if (typeof a === 'number' && typeof b === 'number') return a * b;

    if (a instanceof Rational) return a.multiply(b instanceof Rational ? b : Rational.fromNumber(b));
    if (b instanceof Rational) return b.multiply(a instanceof Rational ? a : Rational.fromNumber(a));

    if (typeof a === 'bigint') return Number(a) * b;
    if (typeof b === 'bigint') return a * Number(b);
    return a * b;
}

function div(a, b) {
    if (a instanceof Rational) return a.divide(b instanceof Rational ? b : Rational.fromNumber(b));
    if (b instanceof Rational) return Rational.fromNumber(a).divide(b);

    if (typeof a === 'bigint' && typeof b === 'bigint') {
        return new Rational(a, b);
    }

    if (typeof a === 'bigint') a = Number(a);
    if (typeof b === 'bigint') b = Number(b);
    return a / b;
}

function toNum(n) {
    if (n instanceof Rational) return n.toNumber();
    if (typeof n === 'bigint') return Number(n);
    return n;
}

// =============================================================================
// Complex Class
// =============================================================================

/**
 * Represents a complex number with real and imaginary parts.
 * Parts can be Number, BigInt, or Rational.
 */
export class Complex {
    /**
     * Creates a new Complex number.
     * @param {number|bigint|Rational} real - The real part
     * @param {number|bigint|Rational} imag - The imaginary part
     * @param {boolean} [exact] - Whether this complex number is exact. 
     *                           Calculated from parts if omitted.
     */
    constructor(real, imag, exact) {
        const validType = (x) => typeof x === 'number' || typeof x === 'bigint' || x instanceof Rational;
        if (!validType(real) || !validType(imag)) {
            throw new Error('Complex: real and imaginary parts must be numbers, BigInts, or Rationals');
        }

        // Determine exactness if not provided
        // Is exact if and only if both parts are exact.
        const partsExact = (typeof real !== 'number') && (typeof imag !== 'number') &&
            (!(real instanceof Rational) || real.exact !== false) &&
            (!(imag instanceof Rational) || imag.exact !== false);

        this.exact = exact === undefined ? partsExact : exact;

        // If inexact, coerce parts to Number (JavaScript floats) for consistency
        if (!this.exact) {
            this.real = toNum(real);
            this.imag = toNum(imag);
        } else {
            this.real = real;
            this.imag = imag;
        }
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
     * @param {number|bigint|Rational} magnitude - Magnitude (r)
     * @param {number|bigint|Rational} angle - Angle in radians (θ)
     * @returns {Complex}
     */
    static fromPolar(magnitude, angle) {
        // Convert to Number for trigonometric operations
        const r = toNum(magnitude);
        const theta = toNum(angle);
        return new Complex(
            r * Math.cos(theta),
            r * Math.sin(theta)
        );
    }

    /**
     * Returns true if this is a real number (imaginary part is 0).
     * @returns {boolean}
     */
    isReal() {
        return this.imag === 0 || this.imag === 0n ||
            (this.imag instanceof Rational && this.imag.numerator === 0n);
    }

    isImaginary() {
        const rZero = this.real === 0 || this.real === 0n ||
            (this.real instanceof Rational && this.real.numerator === 0n);
        const iZero = this.imag === 0 || this.imag === 0n ||
            (this.imag instanceof Rational && this.imag.numerator === 0n);
        return rZero && !iZero;
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
        const r = toNum(this.real); // Convert exact components to float if needed
        const i = toNum(this.imag);
        return Math.sqrt(r * r + i * i);
    }

    angle() {
        return Math.atan2(toNum(this.imag), toNum(this.real));
    }

    toNumber() {
        const iZero = this.imag === 0 || (this.imag instanceof Rational && this.imag.numerator === 0);
        if (iZero) {
            return toNum(this.real);
        }
        throw new Error('Cannot convert complex with non-zero imaginary part to real');
    }

    /**
     * Returns the string representation with R7RS formatting for special values.
     * @param {number} [radix=10]
     * @returns {string}
     */
    toString(radix = 10) {
        // Format a component with R7RS special value formatting
        const formatComp = (val, isImag, includeSign = true) => {
            let s;
            if (typeof val === 'bigint') {
                s = val.toString(radix);
            } else if (val instanceof Rational) {
                s = val.toString(radix);
            } else if (typeof val === 'number') {
                // Always include + for positive infinity in R7RS format
                if (val === Infinity) return '+inf.0';
                if (val === -Infinity) return '-inf.0';
                if (Number.isNaN(val)) return '+nan.0';
                // Handle negative zero - JS toString() loses the sign
                if (Object.is(val, -0)) return '-0.0';

                s = val.toString(radix);
                // Ensure inexactness is visible
                if (Number.isInteger(val) && !s.includes('.') && !s.includes('e')) {
                    s += '.0';
                }
            } else {
                s = String(val);
            }

            if (includeSign && !s.startsWith('-') && !s.startsWith('+')) {
                return '+' + s;
            }
            return s;
        };

        const realStr = formatComp(this.real, false, false);

        let imagVal = this.imag;
        let signStr = '';

        // Check if imaginary part is a special value (inf/nan) - they carry their own sign
        const isSpecialImag = typeof imagVal === 'number' &&
            (imagVal === Infinity || imagVal === -Infinity || Number.isNaN(imagVal));

        if (isSpecialImag) {
            // Special values format themselves with their sign
            const imagStr = formatComp(imagVal, true, false);
            // imagStr will be "+inf.0", "-inf.0", or "+nan.0"
            return `${realStr}${imagStr}i`;
        }

        // Regular handling for non-special values
        if (typeof imagVal === 'number' && imagVal < 0) {
            imagVal = -imagVal;
            signStr = '-';
        } else if (imagVal instanceof Rational && imagVal.numerator < 0n) {
            imagVal = imagVal.negate();
            signStr = '-';
        } else if (typeof imagVal === 'bigint' && imagVal < 0n) {
            imagVal = -imagVal;
            signStr = '-';
        } else {
            signStr = '+';
        }

        let imagStr = formatComp(imagVal, true, false);

        // Suppress 1 for pure imaginary unit i/1.0i/etc if it's exact integer 1
        if (imagStr === '1') {
            imagStr = '';
        }

        return `${realStr}${signStr}${imagStr}i`;
    }


    /**
     * Returns the complex conjugate.
     * @returns {Complex}
     */
    conjugate() {
        // -imag
        let negImag;
        if (this.imag instanceof Rational) negImag = this.imag.negate();
        else negImag = -this.imag;

        return new Complex(this.real, negImag);
    }

    add(other) {
        if (other instanceof Complex) {
            return new Complex(add(this.real, other.real), add(this.imag, other.imag));
        }
        return new Complex(add(this.real, other), this.imag);
    }

    subtract(other) {
        if (other instanceof Complex) {
            return new Complex(sub(this.real, other.real), sub(this.imag, other.imag));
        }
        return new Complex(sub(this.real, other), this.imag);
    }

    multiply(other) {
        if (other instanceof Complex) {
            // (a+bi)(c+di) = (ac-bd) + (ad+bc)i
            const ac = mul(this.real, other.real);
            const bd = mul(this.imag, other.imag);
            const ad = mul(this.real, other.imag);
            const bc = mul(this.imag, other.real);
            return new Complex(sub(ac, bd), add(ad, bc));
        }
        return new Complex(mul(this.real, other), mul(this.imag, other));
    }

    divide(other) {
        if (other instanceof Complex) {
            // (a+bi)/(c+di) = [(ac+bd) + (bc-ad)i] / (c^2+d^2)
            const ac = mul(this.real, other.real);
            const bd = mul(this.imag, other.imag);
            const bc = mul(this.imag, other.real);
            const ad = mul(this.real, other.imag);
            const den = add(mul(other.real, other.real), mul(other.imag, other.imag));
            return new Complex(
                div(add(ac, bd), den),
                div(sub(bc, ad), den)
            );
        }
        return new Complex(div(this.real, other), div(this.imag, other));
    }

    negate() {
        let nr, ni;
        if (this.real instanceof Rational) nr = this.real.negate(); else nr = -this.real;
        if (this.imag instanceof Rational) ni = this.imag.negate(); else ni = -this.imag;
        return new Complex(nr, ni);
    }

    equals(other) {
        if (!(other instanceof Complex)) return false;
        const eq = (a, b) => {
            if (a === b) return true;
            if (a instanceof Rational && b instanceof Rational) return a.equals(b);
            if (a instanceof Rational) return a.equals(Rational.fromNumber(b));
            if (b instanceof Rational) return Rational.fromNumber(a).equals(b);

            // Numeric comparison for mixed BigInt/Number
            if (typeof a === 'bigint' && typeof b === 'number') return Number(a) === b;
            if (typeof a === 'number' && typeof b === 'bigint') return a === Number(b);

            // NaN handling
            if (typeof a === 'number' && typeof b === 'number' && isNaN(a) && isNaN(b)) return true;

            return a === b;
        };
        return (this.exact === other.exact) && eq(this.real, other.real) && eq(this.imag, other.imag);
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
