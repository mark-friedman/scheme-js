import { parse } from '../../interpreter/reader.js';
import { SchemeReadError } from '../../interpreter/errors.js';
import { EOF_OBJECT } from './ports.js';

/**
 * Reads and parses a single S-expression from port.
 * @param {Port} port - Input port.
 * @returns {*} Parsed S-expression or EOF.
 */
export function readExpressionFromPort(port) {
    // Initialize or retrieve persistent reader state for this port
    // This ensures directives like #!fold-case persist across calls
    if (!port._readerState) {
        port._readerState = {
            caseFold: false,
            labels: new Map()
        };
    }

    let buffer = '';

    while (true) {
        let hitEOF = false;
        let parenDepth = 0;
        let inString = false;
        let inVerticalBar = false;
        let escaped = false;
        let started = false;

        // Collect one "chunk" of input (atom or balanced expression)
        // Note: we don't clear buffer here because we might be retrying after a partial read
        while (true) {
            // PEEK first to check for delimiters if we are in an atom
            const next = port.peekChar();

            if (next === EOF_OBJECT) {
                hitEOF = true;
                if (started || buffer.length > 0) {
                    // We have content to process
                    break;
                }
                return EOF_OBJECT;
            }

            // Check for atom delimiters when we are reading a top-level atom
            // Delimiters: whitespace, (, ), ", ;
            if (started && parenDepth === 0 && !inString && !inVerticalBar) {
                if (/\s/.test(next) ||
                    next === '(' || next === ')' ||
                    next === '"' || next === ';') {
                    break;
                }
            }

            // Now consume the character
            const ch = port.readChar();

            // Handle comments: skip until newline
            // But only if we are not inside string/symbol
            // Special case: #; is a datum comment, not a line comment.
            // If buffer ends with #, then ; is part of #; token.
            if (ch === ';' && !inString && !inVerticalBar && !buffer.endsWith('#')) {
                while (true) {
                    const nextCh = port.peekChar();
                    if (nextCh === EOF_OBJECT || nextCh === '\n' || nextCh === '\r') {
                        break;
                    }
                    port.readChar();
                }

                // If we haven't started an atom/expr, we just skipped whitespace/comment.
                // Loop continues.
                // If we HAD started (e.g. invalid state?), we would have broken above at delimiter check.
                continue;
            }

            buffer += ch;

            // Track escape state for strings and |...| symbols
            if (escaped) {
                escaped = false;
                continue;
            } else if (ch === '\\' && (inString || inVerticalBar)) {
                escaped = true;
                continue;
            }

            // Track vertical bar symbol state
            if (ch === '|' && !inString) {
                inVerticalBar = !inVerticalBar;
                started = true;
                if (!inVerticalBar && parenDepth === 0) {
                    // Finished |...| symbol at top level
                    break;
                }
                continue;
            }

            // Skip content inside |...| symbols
            if (inVerticalBar) {
                continue;
            }

            // Track string state
            if (ch === '"') {
                inString = !inString;
                started = true;
                if (!inString && parenDepth === 0) {
                    // Finished string at top level
                    break;
                }
            } else if (!inString) {
                if (ch === '(') {
                    parenDepth++;
                    started = true;
                } else if (ch === ')') {
                    parenDepth--;
                    if (parenDepth <= 0 && started) {
                        break;
                    }
                } else if (ch === '#') {
                    // Check what follows the #
                    const n = port.peekChar();

                    if (n === '(') {
                        // Vector start: #(
                        port.readChar();
                        buffer += '(';
                        parenDepth++;
                        started = true;
                    } else if (n === 'u' || n === 'U') {
                        // Possibly #u8( bytevector
                        port.readChar(); // Consume 'u'
                        const ch2 = port.peekChar();
                        if (ch2 === '8') {
                            port.readChar(); // Consume '8'
                            const ch3 = port.peekChar();
                            if (ch3 === '(') {
                                port.readChar(); // Consume '('
                                buffer += 'u8(';
                                parenDepth++;
                                started = true;
                            } else {
                                buffer += 'u8'; // 'u8' then loop continues
                            }
                        } else {
                            buffer += 'u'; // 'u' + something else (next iter reads ch2)
                        }
                    }
                } else if (!started && !/\s/.test(ch)) {
                    // Started a hidden atom (symbol, number, etc)
                    started = true;
                } else if (started && parenDepth === 0 && /\s/.test(ch)) {
                    // Ended an atom at top level (Should be caught by peek check above)
                    break;
                }
            }
        }

        if (buffer.trim() === '') {
            return EOF_OBJECT;
        }

        try {
            // Pass the persistent state
            const result = parse(buffer, {
                suppressLog: true,
                state: port._readerState
            });

            if (result.length > 0) {
                // If the result is undefined (e.g. from #!fold-case which returns nothing),
                // we should loop again to read the next thing!

                if (result[0] === undefined) {
                    // E.g. found a directive or comment that returned no value
                    buffer = ''; // Clear buffer for next read
                    continue;
                }

                return result[0];
            }
            // If result is empty (comment/whitespace/directive handled by parse), loop.
            buffer = ''; // Clear buffer for next read
            continue;
        } catch (e) {
            // Check for "unexpected end of input" to retry reading more
            if (e.message && e.message.includes('unexpected end of input')) {
                if (hitEOF) {
                    throw e; // Cannot read more input, so this is a real error
                }
                // We need more input. Keep buffer and continue reading next chunk.
                continue;
            }
            // For debugging: warn if we're rethrowing an unexpected end error that didn't match
            // console.warn('Reader bridge rethrowing:', e.message);
            throw e;
        }
    }
}
