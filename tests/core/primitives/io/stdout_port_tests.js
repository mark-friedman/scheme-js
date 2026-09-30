/**
 * Unit tests of the ports over standard output and standard error, run over
 * files and a FIFO rather than the test process's own descriptors.
 *
 * When a buffered port writes, and what, is only visible from outside the
 * process, by reading the file it writes to. What a program the CLI runs sees
 * is tested in `tests/functional/cli_stdout_tests.js`.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { spawn, spawnSync } from 'child_process';
import { StandardOutputPort } from '../../../../src/core/primitives/io/stdout_port.js';
import { assert } from '../../../harness/helpers.js';

/**
 * Runs the standard output port's unit tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>} Resolves when every test has run.
 */
export async function runStandardOutputPortTests(logger) {
  if (typeof process === 'undefined') {
    logger.skip('Standard output port tests (Node.js only)');
    return;
  }

  logger.title('Standard Output Port');

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-js-stdout-'));
  const opened = [];

  /**
   * A new, empty file, open for writing.
   * @returns {{fd: number, written: function(): string}} Its descriptor, and
   *   a way to read what has reached it.
   */
  const newFile = () => {
    const file = path.join(dir, `output-${opened.length}`);
    const fd = fs.openSync(file, 'w');
    opened.push(fd);
    return { fd, written: () => fs.readFileSync(file, 'utf8') };
  };

  try {
    // Line buffering
    {
      const { fd, written } = newFile();
      const port = new StandardOutputPort(fd);
      port.writeString('abc');
      assert(logger, 'a partial line stays in the port', written(), '');
      port.writeChar('\n');
      assert(logger, 'a newline writes the line', written(), 'abc\n');
      port.writeString('de\nf');
      assert(logger, 'a write holding a newline writes all of itself', written(), 'abc\nde\nf');
      port.writeString('g');
      port.flush();
      assert(logger, 'flush writes a partial line', written(), 'abc\nde\nfg');
      port.writeString('hello\n', 1, 3);
      port.flush();
      assert(logger, 'writeString writes the part asked for', written(), 'abc\nde\nfgel');
    }
    {
      const { fd, written } = newFile();
      const port = new StandardOutputPort(fd, { limit: 4 });
      port.writeString('123');
      assert(logger, 'a partial line under the limit stays', written(), '');
      port.writeString('45');
      assert(logger, 'a partial line reaching the limit is written', written(), '12345');
    }

    // Encoding
    {
      const { fd } = newFile();
      const port = new StandardOutputPort(fd);
      port.writeString('é😀\n');
      const file = path.join(dir, `output-${opened.length - 1}`);
      assert(logger, 'text is written as UTF-8',
        Array.from(fs.readFileSync(file)), [0xc3, 0xa9, 0xf0, 0x9f, 0x98, 0x80, 0x0a]);
    }

    // No buffering, and what is written first
    {
      const { fd, written } = newFile();
      const out = new StandardOutputPort(fd);
      const err = new StandardOutputPort(fd, { unbuffered: true, beforeWrite: () => out.flush() });
      out.writeString('partial ');
      err.writeString('E');
      assert(logger, 'an unbuffered port writes at once, after beforeWrite', written(), 'partial E');
    }

    // Closing
    {
      const { fd, written } = newFile();
      const port = new StandardOutputPort(fd);
      port.writeString('last');
      port.close();
      assert(logger, 'close writes what is buffered', written(), 'last');
      assert(logger, 'the port is closed', port.isOpen, false);
      assert(logger, 'writeString on a closed port', errorOf(() => port.writeString('x')),
        'write-string: port is closed');
      assert(logger, 'writeChar on a closed port', errorOf(() => port.writeChar('x')),
        'write-char: port is closed');
      assert(logger, 'the descriptor is left open', errorOf(() => fs.fstatSync(fd)), null);
    }

    await testWaitingToWrite(logger, dir);
    testReaderGone(logger);
  } finally {
    for (const fd of opened) fs.closeSync(fd);
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

/**
 * What becomes of a process whose standard output port finds its write
 * failing with an error: in a child process whose `fs.writeSync` is made to
 * throw it, since a descriptor that fails on demand with a given error cannot
 * be made. `syncBuiltinESMExports` passes the replacement on to the `node:fs`
 * the port imports.
 * @param {string} code - The error's code, such as `EPIPE`.
 * @returns {{status: number, stderr: string}} How the child exited, and what
 *   it wrote to standard error.
 */
function afterWriteFailing(code) {
  const port = new URL('../../../../src/core/primitives/io/stdout_port.js', import.meta.url).href;
  const child = spawnSync(process.execPath, ['--input-type=module', '-e', `
    import { createRequire } from 'module';
    const require = createRequire(import.meta.url);
    const fs = require('fs');
    fs.writeSync = () => {
      const error = new Error('the write failed');
      error.code = ${JSON.stringify(code)};
      throw error;
    };
    require('module').syncBuiltinESMExports();
    const { StandardOutputPort } = await import(${JSON.stringify(port)});
    new StandardOutputPort(1).writeString('a line\\n');
    console.error('the program went on');
  `], { encoding: 'utf8' });
  return { status: child.status, stderr: child.stderr };
}

/**
 * A write whose reader has gone ends the process quietly, with the status a
 * shell gives a process SIGPIPE killed, whatever error the system reports it
 * with: EPIPE for a pipe, and for a socket -- which is what a child process's
 * standard output is on macOS -- EPIPE, ENOTCONN or ECONNRESET. Any other
 * failure is an error the program sees.
 * @param {Object} logger - Test logger.
 */
function testReaderGone(logger) {
  for (const code of ['EPIPE', 'ENOTCONN', 'ECONNRESET']) {
    const { status, stderr } = afterWriteFailing(code);
    assert(logger, `a write failing with ${code} ends the process quietly, as SIGPIPE would`,
      [status, stderr], [141, '']);
  }
  const { status, stderr } = afterWriteFailing('EBADF');
  assert(logger, 'a write failing with any other error is an error',
    [status, stderr.includes('cannot write file descriptor 1'), stderr.includes('the program went on')],
    [1, true, false]);
}

/**
 * The message of the error a call throws.
 * @param {function(): *} call - The call.
 * @returns {string|null} The message, or null if it threw nothing.
 */
function errorOf(call) {
  try {
    call();
    return null;
  } catch (e) {
    return e.message;
  }
}

/**
 * Writes to a descriptor whose writes fail with EAGAIN while the pipe is
 * full, as standard output's do once Node has made it non-blocking, which it
 * does when anything touches `process.stdout`. The port must wait until there
 * is room, not take the error for a failure, and write everything. A
 * non-blocking FIFO filled until a write would wait is such a descriptor.
 * Another process opens it for reading at once, and starts reading once the
 * port is already waiting, until the end; it opens it at once so that a port
 * that wrongly wrote nothing would leave it to find the end, not waiting
 * forever for a writer.
 * @param {Object} logger - Test logger.
 * @param {string} dir - A directory to make the FIFO in.
 * @returns {Promise<void>} Resolves when the reader has exited.
 */
async function testWaitingToWrite(logger, dir) {
  const name = 'a write waits for room in a full pipe';
  if (process.platform === 'win32') {
    logger.skip(`${name} (no FIFOs on Windows)`);
    return;
  }
  const fifo = path.join(dir, 'fifo');
  await new Promise((resolve, reject) => {
    spawn('mkfifo', [fifo]).on('exit', (code) => (code === 0 ? resolve() : reject(new Error('mkfifo failed'))))
      .on('error', reject);
  });

  // Open for reading, which a non-blocking open for writing needs, and never
  // read: the other process does the reading.
  const readFd = fs.openSync(fifo, fs.constants.O_RDONLY | fs.constants.O_NONBLOCK);
  const writeFd = fs.openSync(fifo, fs.constants.O_WRONLY | fs.constants.O_NONBLOCK);
  let writeOpen = true;
  let filled = 0;
  const chunk = new Uint8Array(4096).fill(0x61);
  for (;;) {
    try {
      filled += fs.writeSync(writeFd, chunk);
    } catch (e) {
      if (e.code === 'EAGAIN') break;
      throw e;
    }
  }
  const reader = spawn(process.execPath, ['-e', `
    const fs = require('fs');
    const fd = fs.openSync(${JSON.stringify(fifo)}, 'r');
    process.stdout.write('open\\n');
    setTimeout(() => {
      const bytes = new Uint8Array(65536);
      let count = 0;
      for (let n; (n = fs.readSync(fd, bytes)) > 0;) count += n;
      process.stdout.write(String(count));
    }, 300);
  `]);
  let said = '';
  reader.stdout.on('data', (data) => { said += data; });
  const exited = new Promise((resolve) => reader.on('exit', resolve));
  const opened = await Promise.race([
    new Promise((resolve) => reader.stdout.once('data', () => resolve(true))),
    exited.then(() => false)
  ]);
  try {
    if (!opened) {
      logger.fail(`${name}: the reader exited without opening the FIFO`);
      return;
    }
    const port = new StandardOutputPort(writeFd);
    port.writeString('more\n');
    fs.closeSync(writeFd);
    writeOpen = false;
    await exited;
    assert(logger, name, Number(said.split('\n')[1]), filled + 5);
  } catch (e) {
    logger.fail(`${name}: ${e.message}`);
  } finally {
    if (writeOpen) fs.closeSync(writeFd);
    await exited;
    fs.closeSync(readFd);
  }
}
