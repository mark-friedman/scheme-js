/**
 * Complex Number Support for Scheme.
 * 
 * Implements complex numbers per R7RS §6.2 and (scheme complex) library.
 * Complex numbers have real and imaginary parts.
 */

import { Rational } from './rational.js';

// Helper for component arithmetic (Number/Rational)
function add(a, b) {
    if (a instanceof Rational) return a.add(b);
    if (b instanceof Rational) return b.add(a);
    return a + b;
}

function sub(a, b) {
    if (a instanceof Rational) return a.subtract(b);
    if (b instanceof Rational) return b.negate().add(a);
    return a - b;
}

function mul(a, b) {
    if (a instanceof Rational) return a.multiply(b);
    if (b instanceof Rational) return b.multiply(a);
    return a * b;
}

function div(a, b) {
    if (a instanceof Rational) return a.divide(b);
    if (b instanceof Rational) return Rational.fromNumber(a).divide(b); // a is number
    return a / b;
}

function toNum(n) {
    if (n instanceof Rational) return n.toNumber();
    return n;
}

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
        if ((typeof real !== 'number' && !(real instanceof Rational)) ||
            (typeof imag !== 'number' && !(imag instanceof Rational))) {
            throw new Error('Complex: real and imaginary parts must be numbers or Rationals');
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
        return this.imag === 0 || (this.imag instanceof Rational && this.imag.numerator === 0);
    }

    isImaginary() {
        const rZero = this.real === 0 || (this.real instanceof Rational && this.real.numerator === 0);
        const iZero = this.imag === 0 || (this.imag instanceof Rational && this.imag.numerator === 0);
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
        const formatComp = (n, includeSign = true) => {
            if (typeof n === 'number') {
                if (n === Infinity) return includeSign ? '+inf.0' : 'inf.0';
                if (n === -Infinity) return '-inf.0';
                if (Number.isNaN(n)) return includeSign ? '+nan.0' : 'nan.0';
                if (n >= 0 && includeSign) return '+' + n.toString(radix);
                return n.toString(radix);
            }
            if (n instanceof Rational) {
                const str = n.toString(radix);
                // Rational toString is "n/d" or "-n/d"
                if (!str.startsWith('-') && includeSign) return '+' + str;
                return str;
            }
            return String(n);
        };

        // For the real part, we force the sign if it's a special value (inf/nan),
        // because R7RS requires +inf.0 / +nan.0, but formatComp(..., false) gives inf.0
        const realIsSpecial = typeof this.real === 'number' && !Number.isFinite(this.real);
        const realStr = formatComp(this.real, realIsSpecial);

        // For imaginary part, we calculate the sign separate from the magnitude
        // so we don't want formatComp to include the sign for the magnitude.

        let imagVal = this.imag;
        let signStr = '+';

        // Normalize sign for imaginary part
        if (typeof imagVal === 'number') {
            if (imagVal < 0) {
                imagVal = -imagVal;
                signStr = '-';
            }
        } else if (imagVal instanceof Rational) {
            if (imagVal.numerator < 0) {
                imagVal = imagVal.negate();
                signStr = '-';
            }
        }

        // Handle pure imaginary unit 'i'
        let imagStr = formatComp(imagVal, false);

        // If the magnitude is 1, suppress the '1' before 'i'
        // But be careful: '1' vs '1.0' vs '1/1'
        // R7RS: 1i -> +i. 1.0i -> +1.0i (retains exactness/precision marking if we had it)
        // With Rational(1,1), toString says "1".
        // With Number 1, toString says "1".
        // With Number 1.0, toString says "1" (JS limitation).

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
            if (a instanceof Rational && b instanceof Rational) return a.equals(b);
            if (a instanceof Rational) return a.equals(Rational.fromNumber(b));
            if (b instanceof Rational) return Rational.fromNumber(a).equals(b);
            return a === b;
        };
        return eq(this.real, other.real) && eq(this.imag, other.imag);
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
