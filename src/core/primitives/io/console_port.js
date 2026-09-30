import { Port } from './ports.js';

/**
 * Console output port - writes each line to `console.log`, or to
 * `console.error` for the error port (for default ports).
 */
export class ConsoleOutputPort extends Port {
    constructor(name = 'stdout') {
        super('output');
        this._name = name;
        this._lineBuffer = '';
    }

    writeChar(ch) {
        if (!this._open) {
            throw new Error('write-char: port is closed');
        }
        if (ch === '\n') {
            this._emit(this._lineBuffer);
            this._lineBuffer = '';
        } else {
            this._lineBuffer += ch;
        }
    }

    writeString(str, start = 0, end = str.length) {
        if (!this._open) {
            throw new Error('write-string: port is closed');
        }
        const slice = str.slice(start, end);
        for (const ch of slice) {
            this.writeChar(ch);
        }
    }

    /**
     * Flushes any remaining buffered output.
     */
    flush() {
        if (this._lineBuffer.length > 0) {
            this._emit(this._lineBuffer);
            this._lineBuffer = '';
        }
    }

    /**
     * Writes a line to the console. `console` is looked up at each line, since
     * the harnesses that capture a program's output replace `console.log`.
     * @param {string} line - The line, without its newline.
     */
    _emit(line) {
        if (this._name === 'stderr') console.error(line);
        else console.log(line);
    }

    toString() {
        return `#<console-${this._name}-port>`;
    }
}
