import { parse } from '../../interpreter/reader.js';
import { SchemeReadError } from '../../interpreter/errors.js';
import { EOF_OBJECT } from './ports.js';

/**
 * Reads and parses a single S-expression from port.
 * @param {Port} port - Input port.
 * @returns {*} Parsed S-expression or EOF.
 */
export function readExpressionFromPort(port) {
    let buffer = '';

    while (true) {
        let hitEOF = false;
        let parenDepth = 0;
        let inString = false;
        let inVerticalBar = false;
        let escaped = false;
        let started = false;

        // Collect one "chunk" of input (atom or balanced expression)
        while (true) {
            const ch = port.readChar();
            if (ch === EOF_OBJECT) {
                hitEOF = true;
                break;
            }

            // Handle comments: skip until newline
            // But only if we are not inside string/symbol
            if (ch === ';' && !inString && !inVerticalBar) {
                while (true) {
                    const next = port.peekChar();
                    if (next === EOF_OBJECT || next === '\n' || next === '\r') {
                        break;
                    }
                    port.readChar();
                }
                // Continue loop without adding ; to buffer
                // But we must handle the boundary cleanly.
                // If we were reading an atom "abc;", the ; ends it.
                // "abc" is in buffer. `started` is true. `parenDepth` 0.
                // The next char will be newline (handled in next iter) or EOF.
                // If EOF/newline runs, it will break.
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
                    const next = port.peekChar();
                    if (next === '(') {
                        // Vector start: #(
                        port.readChar();
                        buffer += '(';
                        parenDepth++;
                        started = true;
                    } else if (next === 'u' || next === 'U') {
                        // Possibly #u8( bytevector
                        const ch2 = port.readChar();
                        // Check if 8 and ( follows
                        if (ch2 === '8') {
                            const ch3 = port.peekChar();
                            if (ch3 === '(') {
                                port.readChar();
                                buffer += '8(';
                                parenDepth++;
                                started = true;
                            } else {
                                buffer += ch2;
                            }
                        } else {
                            buffer += ch2;
                        }
                    }
                } else if (!started && !/\s/.test(ch)) {
                    // Started a hidden atom (symbol, number, etc)
                    started = true;
                } else if (started && parenDepth === 0 && /\s/.test(ch)) {
                    // Ended an atom at top level
                    break;
                }
            }
        }

        if (hitEOF && buffer.trim() === '') {
            return EOF_OBJECT;
        }

        try {
            const result = parse(buffer, { suppressLog: true });
            if (result.length > 0) {
                return result[0];
            }
            // If result is empty (comment/whitespace), we loop again.
            // But we can reset buffer because the previous buffer was effectively just whitespace/comments.
            if (hitEOF) return EOF_OBJECT;
            buffer = '';
            continue;
        } catch (e) {
            if (hitEOF) {
                // If we hit EOF and parse failed, it's a real error (incomplete input)
                throw e;
            }
            // Check for "unexpected end of input" to retry reading more
            // Note: parse throws SchemeReadError(message, context)
            if (e.message && e.message.includes('unexpected end of input')) {
                // We need more input. Keep buffer and continue reading next chunk.
                // Reset parser logic state is implied by `parse` being stateless w.r.t our loop (new call)
                continue;
            }
            throw e;
        }
    }
}
