/**
 * The characters made so far, by code point: ASCII in an array, the rest in
 * a map.
 */
const ASCII_CHARS = new Array(128);
const OTHER_CHARS = new Map();

/**
 * Represents a Scheme character.
 * 
 * R7RS requires characters to be a distinct type from strings.
 * This class provides a wrapper around the character's numeric code point.
 *
 * There is one object per code point: the constructor returns the character
 * already made for one, so that `eq?` on characters is `eqv?`. R7RS leaves
 * `eq?` on characters unspecified, but most implementations make it so, and
 * code relies on it -- finding characters with `memq` and `assq`, or
 * comparing them with `eq?`, as chibi's string library does. Doing it here
 * covers every way a character is made, the constants compiled code makes
 * among them.
 */
export class Char {
    /**
     * @param {number} codePoint - Unicode code point of the character.
     */
    constructor(codePoint) {
        if (codePoint >= 0 && codePoint < 128) {
            const made = ASCII_CHARS[codePoint];
            if (made !== undefined) return made;
        } else {
            const made = OTHER_CHARS.get(codePoint);
            if (made !== undefined) return made;
        }
        if (!Number.isInteger(codePoint) || codePoint < 0 || codePoint > 0x10FFFF) {
            throw new Error(`Invalid character code point: ${codePoint}`);
        }
        this.codePoint = codePoint;
        if (codePoint < 128) ASCII_CHARS[codePoint] = this;
        else OTHER_CHARS.set(codePoint, this);
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
