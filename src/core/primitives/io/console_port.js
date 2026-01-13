import { Port } from './ports.js';

/**
 * Console output port - writes to console.log (for default ports).
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
            console.log(this._lineBuffer);
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
            console.log(this._lineBuffer);
            this._lineBuffer = '';
        }
    }

    toString() {
        return `#<console-${this._name}-port>`;
    }
}
