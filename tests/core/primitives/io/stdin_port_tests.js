/**
 * Unit tests of the port over standard input, run over files and a FIFO
 * rather than the test process's own standard input.
 *
 * Only JavaScript can choose how many bytes each read takes, and reading one
 * byte at a time splits every multi-byte character and every "\r\n" across
 * two reads, which is where a port that decodes as it goes can go wrong. The
 * CLI's own use of the port is tested in `tests/functional/cli_stdin_tests.js`.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { spawn } from 'child_process';
import { StandardInputPort } from '../../../../src/core/primitives/io/stdin_port.js';
import { EOF_OBJECT } from '../../../../src/core/primitives/io/ports.js';
import { assert } from '../../../harness/helpers.js';

/**
 * Runs the standard input port's unit tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>} Resolves when every test has run.
 */
export async function runStandardInputPortTests(logger) {
  if (typeof process === 'undefined') {
    logger.skip('Standard input port tests (Node.js only)');
    return;
  }

  logger.title('Standard Input Port');

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-js-stdin-'));
  const opened = [];

  /**
   * A port over a file holding the given contents.
   * @param {string|Uint8Array} contents - What the file holds.
   * @param {number} [chunkSize] - Bytes each read takes.
   * @returns {StandardInputPort} The port.
   */
  const portOver = (contents, chunkSize) => {
    const file = path.join(dir, `input-${opened.length}`);
    fs.writeFileSync(file, contents);
    const fd = fs.openSync(file, 'r');
    opened.push(fd);
    return new StandardInputPort(fd, chunkSize);
  };

  /**
   * Reads with a method until the end of the input.
   * @param {StandardInputPort} port - The port.
   * @param {function(StandardInputPort): *} read - One read.
   * @returns {Array<*>} What each read returned, the end-of-file object last.
   */
  const readAll = (port, read) => {
    const results = [];
    for (;;) {
      const result = read(port);
      results.push(result);
      if (result === EOF_OBJECT) return results;
    }
  };

  try {
    // Characters, one read per byte
    {
      const port = portOver('aé✓😀b', 1);
      assert(logger, 'readChar decodes characters split across reads',
        readAll(port, (p) => p.readChar()), ['a', 'é', '✓', '😀', 'b', EOF_OBJECT]);
    }
    {
      const port = portOver('😀x', 1);
      assert(logger, 'peekChar sees a character split across reads whole', port.peekChar(), '😀');
      assert(logger, 'peekChar does not consume it', port.readChar(), '😀');
      assert(logger, 'readChar reads the next', port.readChar(), 'x');
      assert(logger, 'peekChar at the end', port.peekChar(), EOF_OBJECT);
      assert(logger, 'readChar at the end', port.readChar(), EOF_OBJECT);
      assert(logger, 'the end stays the end', port.readChar(), EOF_OBJECT);
    }

    // Lines
    {
      const port = portOver('héllo ✓\nab\r\ncd\rlast', 1);
      assert(logger, 'readLine ends lines at \\n, \\r\\n and \\r split across reads',
        readAll(port, (p) => p.readLine()), ['héllo ✓', 'ab', 'cd', 'last', EOF_OBJECT]);
    }
    {
      const port = portOver('\n\nx\n', 1);
      assert(logger, 'readLine reads empty lines',
        readAll(port, (p) => p.readLine()), ['', '', 'x', EOF_OBJECT]);
    }
    {
      const lines = Array.from({ length: 5000 }, (_, i) => `line ${i} ${'x'.repeat(i % 97)}`);
      const port = portOver(lines.join('\n') + '\n');
      const read = readAll(port, (p) => p.readLine());
      assert(logger, 'readLine over many reads of the default size reads every line, then the end',
        read.length === lines.length + 1 && read.every((line, i) => line === (lines[i] ?? EOF_OBJECT)), true);
    }

    // Strings of characters
    {
      const port = portOver('a😀bcd', 1);
      assert(logger, 'readString counts characters, not code units', port.readString(3), 'a😀b');
      assert(logger, 'readString returns what is left', port.readString(10), 'cd');
      assert(logger, 'readString at the end', port.readString(1), EOF_OBJECT);
    }

    // Mixed reads share one buffer
    {
      const port = portOver('ab\ncd', 2);
      assert(logger, 'readChar then readLine', [port.readChar(), port.readLine()], ['a', 'b']);
      assert(logger, 'peekChar then readString', [port.peekChar(), port.readString(5)], ['c', 'cd']);
    }

    // Empty input
    {
      const port = portOver('');
      assert(logger, 'every read of empty input is the end',
        [port.peekChar(), port.readChar(), port.readLine(), port.readString(1)],
        [EOF_OBJECT, EOF_OBJECT, EOF_OBJECT, EOF_OBJECT]);
    }

    // Bytes that are not UTF-8 decode as U+FFFD, as Node decodes a file
    {
      const port = portOver(new Uint8Array([0x61, 0xff, 0x62, 0xc3]), 1);
      assert(logger, 'an invalid byte and a truncated sequence decode as U+FFFD',
        port.readLine(), 'a�b�');
    }

    // char-ready? on a file never has to wait
    {
      const port = portOver('ab');
      assert(logger, 'charReady on a file before reading', port.charReady(), true);
      port.readString(2);
      assert(logger, 'charReady on a file with nothing buffered', port.charReady(), true);
      port.readChar();
      assert(logger, 'charReady at the end', port.charReady(), true);
      port.close();
      assert(logger, 'charReady on a closed port', port.charReady(), false);
    }

    await testWaitingForInput(logger, dir);
  } finally {
    for (const fd of opened) fs.closeSync(fd);
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

/**
 * Reads a descriptor whose reads fail with EAGAIN when nothing is there, as
 * standard input's do once Node has made it non-blocking, which it does when
 * anything touches `process.stdin`. The port must wait for the input, not
 * take the error for an end or a failure. A non-blocking FIFO is such a
 * descriptor. Another process opens it for writing, so that a read finds it
 * empty rather than at its end, and writes a line once the port is already
 * waiting. The writer, not this process, holds the only write end and closes
 * it, so a port that wrongly read on to the end would still finish.
 * @param {Object} logger - Test logger.
 * @param {string} dir - A directory to make the FIFO in.
 * @returns {Promise<void>} Resolves when the writer has exited.
 */
async function testWaitingForInput(logger, dir) {
  const name = 'a read waits for input that is not there yet';
  if (process.platform === 'win32') {
    logger.skip(`${name} (no FIFOs on Windows)`);
    return;
  }
  const fifo = path.join(dir, 'fifo');
  await new Promise((resolve, reject) => {
    spawn('mkfifo', [fifo]).on('exit', (code) => (code === 0 ? resolve() : reject(new Error('mkfifo failed'))))
      .on('error', reject);
  });

  const readFd = fs.openSync(fifo, fs.constants.O_RDONLY | fs.constants.O_NONBLOCK);
  const writer = spawn(process.execPath, ['-e', `
    const fs = require('fs');
    const fd = fs.openSync(${JSON.stringify(fifo)}, 'w');
    process.stdout.write('open');
    setTimeout(() => { fs.writeSync(fd, 'late\\n'); fs.closeSync(fd); }, 300);
  `]);
  const exited = new Promise((resolve) => writer.on('exit', resolve));
  const opened = await Promise.race([
    new Promise((resolve) => writer.stdout.once('data', () => resolve(true))),
    exited.then(() => false)
  ]);
  try {
    if (!opened) {
      logger.fail(`${name}: the writer exited without opening the FIFO`);
      return;
    }
    const port = new StandardInputPort(readFd);
    assert(logger, name, port.readLine(), 'late');
  } catch (e) {
    logger.fail(`${name}: ${e.message}`);
  } finally {
    await exited;
    fs.closeSync(readFd);
  }
}
