/**
 * @fileoverview String and symbol escape processing for Scheme reader.
 */

/**
 * Process escape sequences in vertical bar delimited symbols.
 * @param {string} str - Inner content of |...|
 * @returns {string} Processed string
 */
export function processSymbolEscapes(str) {
    let result = '';
    let i = 0;
    while (i < str.length) {
        if (str[i] === '\\' && i + 1 < str.length) {
            const next = str[i + 1];
            if (next === '|') {
                result += '|';
                i += 2;
            } else if (next === '\\') {
                result += '\\';
                i += 2;
            } else if (next === 'x') {
                // Hex escape: \xNN; or \xNNNN;
                const semicolonIdx = str.indexOf(';', i + 2);
                if (semicolonIdx !== -1) {
                    const hexStr = str.slice(i + 2, semicolonIdx);
                    const codePoint = parseInt(hexStr, 16);
                    if (!isNaN(codePoint)) {
                        result += String.fromCodePoint(codePoint);
                        i = semicolonIdx + 1;
                        continue;
                    }
                }
                // Invalid hex escape - keep as-is
                result += str[i];
                i++;
            } else {
                // Other escapes - keep the character after backslash
                result += next;
                i += 2;
            }
        } else {
            result += str[i];
            i++;
        }
    }
    return result;
}

/**
 * Process R7RS string escape sequences.
 * \a - alarm (bell)
 * \b - backspace
 * \t - tab
 * \n - newline
 * \r - return
 * \" - double quote
 * \\ - backslash
 * \| - vertical bar
 * \xN...N; - hex escape
 * \<newline><intraline-whitespace> - line continuation
 * @param {string} str - String content without quotes
 * @returns {string} Processed string
 */
export function processStringEscapes(str) {
    let result = '';
    let i = 0;
    while (i < str.length) {
        if (str[i] === '\\' && i + 1 < str.length) {
            const next = str[i + 1];
            switch (next) {
                case 'a':
                    result += '\x07'; // alarm (bell)
                    i += 2;
                    break;
                case 'b':
                    result += '\x08'; // backspace
                    i += 2;
                    break;
                case 't':
                    result += '\t';
                    i += 2;
                    break;
                case 'n':
                    result += '\n';
                    i += 2;
                    break;
                case 'r':
                    result += '\r';
                    i += 2;
                    break;
                case '"':
                    result += '"';
                    i += 2;
                    break;
                case '\\':
                    result += '\\';
                    i += 2;
                    break;
                case '|':
                    result += '|';
                    i += 2;
                    break;
                case 'x':
                    // Hex escape: \xN...N;
                    const semicolonIdx = str.indexOf(';', i + 2);
                    if (semicolonIdx !== -1) {
                        const hexStr = str.slice(i + 2, semicolonIdx);
                        const codePoint = parseInt(hexStr, 16);
                        if (!isNaN(codePoint)) {
                            result += String.fromCodePoint(codePoint);
                            i = semicolonIdx + 1;
                            break;
                        }
                    }
                    // Invalid hex escape - keep as-is
                    result += str[i];
                    i++;
                    break;
                case '\n':
                case '\r':
                case ' ':
                case '\t':
                    // Line continuation: skip backslash, skip whitespace including newline
                    i += 1; // skip backslash
                    // Skip leading whitespace before newline
                    while (i < str.length && (str[i] === ' ' || str[i] === '\t')) {
                        i++;
                    }
                    // Skip newline (could be \n, \r, or \r\n)
                    if (i < str.length && str[i] === '\r') {
                        i++;
                    }
                    if (i < str.length && str[i] === '\n') {
                        i++;
                    }
                    // Skip trailing whitespace after newline
                    while (i < str.length && (str[i] === ' ' || str[i] === '\t')) {
                        i++;
                    }
                    break;
                default:
                    // Unknown escape - keep the character
                    result += next;
                    i += 2;
            }
        } else {
            result += str[i];
            i++;
        }
    }
    return result;
}
