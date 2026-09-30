import { EOF_OBJECT } from './ports.js';
import { StringInputPort, passCharacters } from './string_port.js';
import { flushStandardOutput } from './stdout_port.js';

// ============================================================================
// Environment Detection
// ============================================================================

/**
 * Node.js's `fs`, or null in a browser, which has no standard input.
 * @type {Object|null}
 */
const fs = typeof process !== 'undefined' && process.versions?.node != null
    ? await import('node:fs')
    : null;

/**
 * Something to wait on: a read of standard input that Node has made
 * non-blocking fails with EAGAIN while there is no input, and the port waits
 * on this for a moment before trying again.
 * @type {Int32Array|null}
 */
const PAUSE = fs ? new Int32Array(new SharedArrayBuffer(4)) : null;

// ============================================================================
// Standard Input Port (Node.js Only)
// ============================================================================

/**
 * Textual input port over the process's standard input, or over any file
 * descriptor, decoded as UTF-8.
 *
 * The CLI runs a program synchronously, and a Scheme read returns its
 * character as its value, so the port reads the descriptor with `fs.readSync`,
 * which blocks until input arrives, rather than through `process.stdin`,
 * whose data arrives in callbacks the running program would never return to.
 * It reads only when a read needs more than it holds, and takes what is
 * there, so a program in a pipeline answers each line as it arrives and one
 * at a terminal reads each line as it is typed. Reading all the input when
 * the program starts would be simpler, and would wait for the end of the
 * input before the program could do anything.
 *
 * The characters read and not yet consumed are a string, read as a string
 * port reads its string; all this class adds is reading more into the string
 * when a read needs more than it holds.
 * The descriptor is never closed: closing the port stops reads through it,
 * and standard input stays open for the rest of the process.
 */
export class StandardInputPort extends StringInputPort {
    /**
     * @param {number} [fd=0] - The file descriptor to read.
     * @param {number} [chunkSize=65536] - The most bytes one read takes.
     */
    constructor(fd = 0, chunkSize = 65536) {
        super('');
        if (!fs) {
            throw new Error('standard-input-port: there is no standard input in a browser');
        }
        this._fd = fd;
        this._bytes = new Uint8Array(chunkSize);
        this._decoder = new TextDecoder('utf-8');
        this._atEnd = false;
        this._isFile = null;
    }

    /**
     * Reads more input into the string until it answers a test, or the input
     * ends.
     * @param {function(): boolean} holdsEnough - Whether the string holds
     *   what the read needs.
     */
    _readUntil(holdsEnough) {
        while (this._open && !this._atEnd && !holdsEnough()) this._readMore();
    }

    /**
     * Reads what input there is, waiting for some if there is none, and adds
     * it to the string, dropping what has already been consumed. Standard
     * output is written first, so that a prompt is seen before the read waits
     * for its answer.
     */
    _readMore() {
        flushStandardOutput();
        const count = readAvailable(this._fd, this._bytes);
        const rest = this._string.slice(this._pos);
        if (count === 0) {
            // A character cut short by the end of the input decodes as U+FFFD.
            this._atEnd = true;
            this._string = rest + this._decoder.decode();
        } else {
            // `stream` holds back the bytes of a character the read cut in two
            // until the next read brings the rest.
            this._string = rest + this._decoder.decode(this._bytes.subarray(0, count), { stream: true });
        }
        this._pos = 0;
    }

    /**
     * Whether the string holds a whole line: a "\n", or a "\r" with what
     * follows it, since a "\r\n" is one line ending.
     * @returns {boolean}
     */
    _holdsLine() {
        for (let i = this._pos; i < this._string.length; i++) {
            if (this._string[i] === '\n') return true;
            if (this._string[i] === '\r') return i + 1 < this._string.length;
        }
        return false;
    }

    peekChar() {
        this._readUntil(() => this._pos < this._string.length);
        return super.peekChar();
    }

    readLine() {
        this._readUntil(() => this._holdsLine());
        return super.readLine();
    }

    readString(k) {
        this._readUntil(() => passCharacters(this._string, this._pos, k).passed === k);
        return super.readString(k);
    }

    /**
     * Whether a read would return without waiting: when characters are
     * waiting in the string, at the end of the input, and always from a
     * regular file. From a pipe or a terminal with nothing read ahead there
     * is no way to ask without a read that might wait, so the answer is #f,
     * even if input has in fact arrived.
     * @returns {boolean}
     */
    charReady() {
        if (!this._open) return false;
        if (this._pos < this._string.length || this._atEnd) return true;
        if (this._isFile === null) this._isFile = fs.fstatSync(this._fd).isFile();
        return this._isFile;
    }

    toString() {
        return `#<standard-input-port:${this._open ? 'open' : 'closed'}>`;
    }
}

/**
 * Reads what bytes a descriptor has, waiting until it has some or its input
 * ends.
 * @param {number} fd - The descriptor.
 * @param {Uint8Array} bytes - Where to put them.
 * @returns {number} How many were read, 0 at the end of the input.
 * @throws {Error} If the descriptor cannot be read.
 */
function readAvailable(fd, bytes) {
    for (;;) {
        try {
            return fs.readSync(fd, bytes, 0, bytes.length, null);
        } catch (e) {
            // Non-blocking, as standard input is once anything has touched
            // `process.stdin`, and nothing there yet.
            if (e.code === 'EAGAIN') {
                Atomics.wait(PAUSE, 0, 0, 10);
                continue;
            }
            // Windows reports the end of a pipe as an error.
            if (e.code === 'EOF') return 0;
            throw new Error(`cannot read standard input: ${e.message}`);
        }
    }
}

/**
 * The port over the process's standard input. There is one, made when first
 * asked for: two ports each reading ahead would each take input the other
 * should have had.
 * @type {StandardInputPort|null}
 */
let standardInput = null;

/**
 * The port over the process's standard input.
 * @returns {StandardInputPort} The port.
 * @throws {Error} In a browser, which has no standard input.
 */
export function standardInputPort() {
    if (!standardInput) standardInput = new StandardInputPort(0);
    return standardInput;
}
