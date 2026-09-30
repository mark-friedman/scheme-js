/**
 * @fileoverview Mutable Scheme strings.
 *
 * R7RS lets a program change a string with `string-set!`, `string-fill!` and
 * `string-copy!`, and every string a procedure newly allocates may be changed
 * -- `make-string`'s, `string-append`'s, `substring`'s. A JavaScript string is
 * a value with no identity: it cannot be changed in place for everyone holding
 * it, and two strings with the same characters are the same value, so which
 * of them a `string-set!` meant could not be told. So a string that may be
 * changed is an object from the moment it is made.
 *
 * Inside, it keeps a JavaScript string until it is first changed, so a string
 * that never is -- nearly all of them -- costs one small object and keeps V8's
 * representation of it, which makes `string-append` close to free. The first
 * change splits it into an array of UTF-16 code units, and the string is joined
 * again only when it is next needed whole.
 *
 * Literals, the strings `symbol->string` returns, and strings that come from
 * JavaScript stay JavaScript strings, which R7RS allows to be immutable: a
 * `string-set!` on one is an error. A Scheme string crossing into JavaScript
 * becomes a JavaScript string holding its characters at that moment
 * (`schemeToJs` in `src/core/interpreter/js_interop.js`): JavaScript has no
 * mutable strings, so a string crosses as its value, as a number does.
 *
 * Positions count UTF-16 code units, as for every string here: a character
 * beyond the Basic Multilingual Plane takes two positions, and storing one
 * replaces one position with two.
 */

/**
 * A string that may be changed.
 */
export class SchemeString {
    /**
     * @param {string} text - Its characters.
     */
    constructor(text) {
        /**
         * Its characters as a JavaScript string, valid unless `stale`.
         * @type {string}
         */
        this.text = text;

        /**
         * Its code units, one to an element, once it has been changed.
         * @type {Array<string>|null}
         */
        this.units = null;

        /**
         * Whether `units` holds changes `text` does not.
         * @type {boolean}
         */
        this.stale = false;
    }

    /**
     * Its characters as a JavaScript string.
     * @returns {string}
     */
    toString() {
        if (this.stale) {
            this.text = this.units.join('');
            this.stale = false;
        }
        return this.text;
    }

    /**
     * Its length in code units.
     * @returns {number}
     */
    get length() {
        return this.units !== null ? this.units.length : this.text.length;
    }

    /**
     * The code point starting at a position, as `String.prototype.codePointAt`
     * reads it: a surrogate pair's two units as one character.
     * @param {number} index - A position.
     * @returns {number} The code point.
     */
    codePointAt(index) {
        if (this.units === null) return this.text.codePointAt(index);
        const high = this.units[index].charCodeAt(0);
        if (high >= 0xD800 && high <= 0xDBFF && index + 1 < this.units.length) {
            const low = this.units[index + 1].charCodeAt(0);
            if (low >= 0xDC00 && low <= 0xDFFF) return (high - 0xD800) * 0x400 + (low - 0xDC00) + 0x10000;
        }
        return high;
    }

    /**
     * The code units, split out on the first change.
     * @returns {Array<string>}
     */
    unitsForChange() {
        if (this.units === null) this.units = this.text.split('');
        this.stale = true;
        return this.units;
    }

    /**
     * Stores characters at a position, replacing as many code units as they
     * take.
     * @param {number} index - Where the first goes.
     * @param {string} chars - The characters, as a JavaScript string.
     */
    replace(index, chars) {
        const units = this.unitsForChange();
        if (chars.length === 1) {
            units[index] = chars;
        } else {
            units.splice(index, 1, ...chars.split(''));
        }
    }

    /**
     * Stores one character at every position in a range.
     * @param {string} char - The character, as a JavaScript string.
     * @param {number} start - The first position.
     * @param {number} end - The position after the last.
     */
    fill(char, start, end) {
        const units = this.unitsForChange();
        if (char.length === 1) {
            for (let i = start; i < end; i++) units[i] = char;
        } else {
            // Two units each: the range keeps its length in characters.
            units.splice(start, end - start, ...char.repeat(end - start).split(''));
        }
    }
}

/**
 * Whether a value is a Scheme string: a JavaScript string or a `SchemeString`.
 * @param {*} x - The value.
 * @returns {boolean}
 */
export function isString(x) {
    return typeof x === 'string' || x instanceof SchemeString;
}

/**
 * A Scheme string's characters as a JavaScript string.
 * @param {string|SchemeString} s - A Scheme string.
 * @returns {string}
 */
export function stringValue(s) {
    return typeof s === 'string' ? s : s.toString();
}

/**
 * A newly allocated Scheme string, which may be changed.
 * @param {string} text - Its characters.
 * @returns {SchemeString}
 */
export function freshString(text) {
    return new SchemeString(text);
}
