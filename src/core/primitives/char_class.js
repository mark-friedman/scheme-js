/**
 * Represents a Scheme character.
 * 
 * R7RS requires characters to be a distinct type from strings.
 * This class provides a wrapper around the character's numeric code point.
 */
export class Char {
    /**
     * @param {number} codePoint - Unicode code point of the character.
     */
    constructor(codePoint) {
        if (!Number.isInteger(codePoint) || codePoint < 0 || codePoint > 0x10FFFF) {
            throw new Error(`Invalid character code point: ${codePoint}`);
        }
        this.codePoint = codePoint;
    }

    /**
     * Returns the character as a single-character string.
     * @returns {string}
     */
    toString() {
        return String.fromCodePoint(this.codePoint);
    }

    /**
     * Returns the numeric code point.
     * @returns {number}
     */
    valueOf() {
        return this.codePoint;
    }

    /**
     * Comparison for eqv? support.
     * @param {*} other 
     * @returns {boolean}
     */
    equals(other) {
        return other instanceof Char && this.codePoint === other.codePoint;
    }
}
