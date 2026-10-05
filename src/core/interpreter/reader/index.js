/**
 * @fileoverview The reader's door: `parse`, which reads text into data with
 * `(scheme-js reader)` (src/core/scheme/reader.scm), on the library system's
 * own interpreter, where it is loaded with the library system's seed.
 *
 * The number parser is still here, as `string->number`'s core.
 */

import { systemLibrary } from '../library_seed.js';
import { callSchemeProcedure } from '../values.js';
import { toArray } from '../cons.js';

export { parseNumber, parsePrefixedNumber } from './number_parser.js';

/** The reader's exports, once found. @type {Map<string, Function>|null} */
let reader = null;

/**
 * Reads a text into data.
 * @param {string} input - The text.
 * @param {Object} [options] - How to read it.
 * @param {boolean} [options.caseFold=false] - Whether symbols are read
 *   folding case at first, as `#!fold-case` would have them (for include-ci).
 * @param {boolean} [options.dotAccess=true] - Whether dot notation applies at
 *   first, `a.b` read as `(js-ref a "b")`: off for a library's files, which
 *   are written in R7RS, where a dot is a character of an identifier. The text
 *   can say otherwise, with `#!dot-notation` and `#!no-dot-notation`.
 * @param {string} [options.filename='<unknown>'] - The name each list's and
 *   vector's span gives the text. The debugger matches breakpoints on it, so
 *   callers that know which file they are reading should supply it.
 * @param {boolean} [options.suppressLog=false] - Whether to say nothing on
 *   the console when the text cannot be read.
 * @param {{caseFold: boolean, dotAccess: boolean}} [options.state] - How the
 *   text read before this one, of which it is the continuation, left case
 *   folding and dot notation, which reading it updates: as a port's reads do.
 * @returns {Array} The data.
 */
export function parse(input, options = {}) {
    if (reader === null) reader = systemLibrary(['scheme-js', 'reader']);
    const filename = options.filename ?? '<unknown>';
    try {
        const state = options.state;
        if (state === undefined) {
            return toArray(callSchemeProcedure(reader.get('read-source'),
                [input, filename, options.caseFold === true, options.dotAccess !== false]));
        }
        const [data, caseFold, dotAccess] = toArray(callSchemeProcedure(reader.get('read-source-continuing'),
            [input, filename, state.caseFold === true, state.dotAccess !== false]));
        state.caseFold = caseFold;
        state.dotAccess = dotAccess;
        return toArray(data);
    } catch (e) {
        if (!options.suppressLog) {
            console.error(`Parse error in input: "${input.substring(0, 100)}${input.length > 100 ? '...' : ''}"`);
        }
        throw e;
    }
}
