import { Port } from './ports.js';

// ============================================================================
// Environment Detection
// ============================================================================

/**
 * Node.js's `fs`, or null in a browser, which has no standard output. Got
 * synchronously, not by a top-level `await`, which a bundler that wraps
 * modules in functions cannot keep.
 * @type {Object|null}
 */
const fs = typeof process !== 'undefined' && process.versions?.node != null
    ? process.getBuiltinModule('node:fs')
    : null;

/**
 * Something to wait on: a write to standard output that Node has made
 * non-blocking fails with EAGAIN while the pipe is full, and the port waits
 * on this for a moment before trying again.
 * @type {Int32Array|null}
 */
const PAUSE = fs ? new Int32Array(new SharedArrayBuffer(4)) : null;

/**
 * The exit status a shell reports for a process that SIGPIPE killed: 128 and
 * the signal's number, 13.
 * @type {number}
 */
const BROKEN_PIPE_STATUS = 141;

/**
 * The errors a write fails with when nothing reads what it writes any more.
 * A pipe reports EPIPE. A socket -- which is what a child process's standard
 * output is on macOS, where Node makes its pipes of socket pairs -- reports
 * EPIPE, or ENOTCONN if the write comes as the reader shuts its end, or
 * ECONNRESET if the reader reset the connection rather than closing it.
 * @type {Set<string>}
 */
const READER_GONE = new Set(['EPIPE', 'ENOTCONN', 'ECONNRESET']);

// ============================================================================
// Standard Output Port (Node.js Only)
// ============================================================================

/**
 * Textual output port over the process's standard output or standard error,
 * or over any file descriptor, written as UTF-8.
 *
 * It writes with `fs.writeSync`, so what a write sends has reached the
 * descriptor when the write returns: the process can exit straight after,
 * and output to standard output and standard error keeps the order it was
 * written in. `process.stdout` would queue writes to a pipe on some systems,
 * and a program that loops without returning to Node's event loop would
 * never see them go.
 *
 * It holds text until a line ends, a line grows past a limit, or it is
 * flushed, so a program writing a character at a time costs a write a line,
 * not one a character. The CLI flushes standard output when the program ends
 * and before a read of standard input waits, so that a prompt with no newline
 * is seen before the program waits for its answer. An unbuffered port, as
 * standard error is, writes at once. The descriptor is never closed: closing
 * the port writes what it holds and stops writes through it.
 */
export class StandardOutputPort extends Port {
    /**
     * @param {number} [fd=1] - The file descriptor to write.
     * @param {Object} [options]
     * @param {boolean} [options.unbuffered=false] - Write each write at once.
     * @param {number} [options.limit=65536] - The most code units held before
     *   they are written, a line ended or not.
     * @param {function(): void} [options.beforeWrite] - Called before each
     *   write to the descriptor, as standard error flushes standard output.
     */
    constructor(fd = 1, { unbuffered = false, limit = 65536, beforeWrite = null } = {}) {
        super('output');
        if (!fs) {
            throw new Error('standard-output-port: there is no standard output in a browser');
        }
        this._fd = fd;
        this._unbuffered = unbuffered;
        this._limit = limit;
        this._beforeWrite = beforeWrite;
        this._buffer = '';
    }

    writeChar(ch) {
        if (!this._open) throw new Error('write-char: port is closed');
        this._hold(ch);
    }

    writeString(str, start = 0, end = str.length) {
        if (!this._open) throw new Error('write-string: port is closed');
        this._hold(str.slice(start, end));
    }

    /**
     * Adds text to what the port holds, and writes it all if the text ends a
     * line, the port holds more than its limit, or the port is unbuffered.
     * @param {string} text - The text.
     */
    _hold(text) {
        this._buffer += text;
        if (this._unbuffered || this._buffer.length >= this._limit || text.includes('\n')) this.flush();
    }

    /**
     * Writes what the port holds.
     */
    flush() {
        if (this._buffer === '') return;
        if (this._beforeWrite) this._beforeWrite();
        // Taken before it is written, so that a write that ends the process
        // does not leave it to be written again as the process exits.
        const text = this._buffer;
        this._buffer = '';
        writeAll(this._fd, new TextEncoder().encode(text));
    }

    /**
     * Writes what the port holds, and closes it.
     */
    close() {
        if (this._open) {
            this.flush();
            this._open = false;
        }
    }

    toString() {
        return `#<standard-output-port:${this._fd}:${this._open ? 'open' : 'closed'}>`;
    }
}

/**
 * Writes bytes to a descriptor, all of them, waiting while a pipe is full.
 * @param {number} fd - The descriptor.
 * @param {Uint8Array} bytes - The bytes.
 * @throws {Error} If the descriptor cannot be written.
 */
function writeAll(fd, bytes) {
    let written = 0;
    while (written < bytes.length) {
        try {
            written += fs.writeSync(fd, bytes, written, bytes.length - written);
        } catch (e) {
            // Non-blocking, as standard output is once anything has touched
            // `process.stdout`, and the pipe full.
            if (e.code === 'EAGAIN') {
                Atomics.wait(PAUSE, 0, 0, 10);
                continue;
            }
            // Nothing reads the pipe any more, as when the output goes to
            // `head` and it has had its lines. SIGPIPE would end the process,
            // silently, but Node ignores it, so the process ends here.
            if (READER_GONE.has(e.code)) process.exit(BROKEN_PIPE_STATUS);
            throw new Error(`cannot write file descriptor ${fd}: ${e.message}`);
        }
    }
}

// ============================================================================
// The Process's Own Ports
// ============================================================================

/**
 * The port over standard output, made when first asked for.
 * @type {StandardOutputPort|null}
 */
let standardOutput = null;

/**
 * The port over standard error, made when first asked for.
 * @type {StandardOutputPort|null}
 */
let standardError = null;

/**
 * The port over the process's standard output. There is one, so that what it
 * holds is written once, in order, when the process exits: however it exits,
 * the program returning, `exit`, or an error.
 * @returns {StandardOutputPort} The port.
 * @throws {Error} In a browser, which has no standard output.
 */
export function standardOutputPort() {
    if (!standardOutput) {
        standardOutput = new StandardOutputPort(1);
        process.on('exit', () => {
            // Nothing is left to report a failure to.
            try { standardOutput.flush(); } catch (e) { /* the process is ending */ }
        });
    }
    return standardOutput;
}

/**
 * The port over the process's standard error: unbuffered, and writing
 * standard output first, so that an error is seen after what came before it.
 * @returns {StandardOutputPort} The port.
 * @throws {Error} In a browser, which has no standard error.
 */
export function standardErrorPort() {
    if (!standardError) {
        standardError = new StandardOutputPort(2, { unbuffered: true, beforeWrite: flushStandardOutput });
    }
    return standardError;
}

/**
 * Writes what the port over standard output holds, if there is one: before
 * anything that should be seen after it.
 */
export function flushStandardOutput() {
    if (standardOutput) standardOutput.flush();
}
