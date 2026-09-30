/**
 * What a program the CLI runs writes reaches standard output and standard
 * error, all of it, in order, and when it is written.
 *
 * A program run as `node repl.js prog.scm` or `node repl.js -e ...` has the
 * process's standard output and standard error as its current output and
 * error ports: a line is written when it ends, what is left when the program
 * ends, a prompt before the program waits to read, and standard output before
 * anything written to standard error. The interactive REPL writes what an
 * evaluation displayed when the evaluation ends. Each test runs `repl.js` in a
 * child process, so these are Node-only.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { assert } from '../harness/helpers.js';
import { runCli, inSteps, outputOf } from '../harness/cli_process.js';

/**
 * Runs the tests of what the CLI's programs write.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>} Resolves when every run has finished.
 */
export async function runCliStdoutTests(logger) {
  if (typeof process === 'undefined') {
    logger.skip('CLI standard output tests (Node.js only)');
    return;
  }

  logger.title('CLI - programs write standard output and standard error');

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-js-cli-stdout-'));
  const opened = [];

  /**
   * Writes a program to a file.
   * @param {string} name - The file's name.
   * @param {string} source - The program.
   * @returns {string} The file's path.
   */
  const program = (name, source) => {
    const file = path.join(dir, name);
    fs.writeFileSync(file, source);
    return file;
  };

  /**
   * A new, empty file to give a run as both standard output and standard
   * error.
   * @returns {{fd: number, written: function(): string}} Its descriptor, and
   *   a way to read what the run wrote to it.
   */
  const sharedOutput = () => {
    const file = path.join(dir, `output-${opened.length}`);
    const fd = fs.openSync(file, 'w');
    opened.push(fd);
    return { fd, written: () => fs.readFileSync(file, 'utf8') };
  };

  try {
    const noNewline = program('no_newline.scm', '(display "no newline at end")');
    const prompt = program('prompt.scm', `
      (display "Name: ")
      (let ((name (read-line)))
        (display "Hello, ")
        (display name)
        (newline))
    `);
    const bothStreams = program('both_streams.scm', `
      (display "to stdout")
      (newline)
      (display "to stderr" (current-error-port))
      (newline (current-error-port))
    `);
    const interleaved = program('interleaved.scm', `
      (display "partial ")
      (display "err" (current-error-port))
      (newline (current-error-port))
      (display "rest")
      (newline)
    `);
    const exits = program('exits.scm', '(display "before exit") (exit 3) (display "after exit")');
    const outputFile = path.join(dir, 'written.txt');
    const toFile = program('to_file.scm', `
      (with-output-to-file ${JSON.stringify(outputFile)} (lambda () (display "in the file")))
      (display "on stdout")
    `);
    const fails = program('fails.scm', `(display "partial") (car '())`);
    const whichPorts = program('which_ports.scm', `
      (write (list (eq? (current-output-port) (standard-output-port))
                   (eq? (current-error-port) (standard-error-port))
                   (textual-port? (current-output-port))))
      (newline)
    `);
    const merged = sharedOutput();
    const failed = sharedOutput();

    // Closes its end of the pipe once the program has written something, as
    // `head -1` would.
    const closeAfterFirstOutput = (child, stdout) => {
      if (stdout === '') child.stdin.end();
      else child.stdout.destroy();
    };

    const runs = await Promise.all([
      runCli([noNewline]),
      runCli(['--no-compile', noNewline]),
      runCli(['-e', '(write 1)']),
      runCli(['-e', '(display "x")']),
      runCli(['-e', '(+ 1 2 3)']),
      runCli(['-e', '(list 1.5 "a" #\\b)']),
      runCli(['-e', '(display "out") 42']),
      runCli([prompt], { onOutput: inSteps([{ after: 'Name: ', write: 'World\n' }]) }),
      runCli([bothStreams]),
      runCli([interleaved], { output: merged.fd }),
      runCli([exits]),
      runCli([toFile]),
      runCli([fails], { output: failed.fd }),
      runCli([whichPorts]),
      runCli(['-e', '(let loop ((i 0)) (display i) (newline) (loop (+ i 1)))'],
        { onOutput: closeAfterFirstOutput }),
      runCli([], {
        onOutput: inSteps([{ after: '', write: '(display "hi")\n' }, { after: '> hi', write: '(+ 1 2)\n' }])
      })
    ]);
    const [lastLine, lastLineInterpreted, eWrite, eDisplay, eNumber, eList, eOrder, prompted,
      streams, interleavedRun, exited, toFileRun, failedRun, ports, cutOff, repl] = runs;

    assert(logger, 'a last line with no newline is written (compiled)',
      outputOf(lastLine), 'no newline at end');
    assert(logger, 'a last line with no newline is written (interpreted)',
      outputOf(lastLineInterpreted), 'no newline at end');
    assert(logger, '-e writes what the program wrote, and no result for an unspecified one',
      outputOf(eWrite), '1');
    assert(logger, '-e displays with no newline', outputOf(eDisplay), 'x');
    assert(logger, '-e writes its result as Scheme writes it: an exact integer',
      outputOf(eNumber), '6\n');
    assert(logger, '-e writes its result as Scheme writes it: a list of a flonum, a string and a character',
      outputOf(eList), '(1.5 "a" #\\b)\n');
    assert(logger, '-e writes its result after what the program wrote',
      outputOf(eOrder), 'out42\n');
    assert(logger, 'a prompt with no newline is written before the program waits to read',
      outputOf(prompted), 'Name: Hello, World\n');
    assert(logger, 'current-error-port writes standard error',
      [streams.status, streams.stdout, streams.stderr], [0, 'to stdout\n', 'to stderr\n']);
    assert(logger, 'standard output is written before what is written to standard error',
      [interleavedRun.status, merged.written()], [0, 'partial err\nrest\n']);
    assert(logger, 'exit writes what the program wrote before it',
      [exited.status, exited.stdout], [3, 'before exit']);
    assert(logger, 'with-output-to-file puts standard output back',
      [outputOf(toFileRun), fs.readFileSync(outputFile, 'utf8')], ['on stdout', 'in the file']);
    assert(logger, 'what a failing program wrote comes before the error',
      failedRun.status === 1 && failed.written().startsWith('partial') && failed.written().includes('car')
        ? true : failed.written(), true);
    assert(logger, 'the current output and error ports are the standard ones',
      outputOf(ports), '(#t #t #t)\n');
    assert(logger, 'a program writing to a pipe whose reader has gone ends quietly',
      [cutOff.status, cutOff.stderr], [141, '']);
    assert(logger, 'the interactive REPL writes what an evaluation displayed when it ends',
      repl.status === 0 && repl.stdout.includes('> hi\n') && repl.stdout.includes('> 3\n')
        ? true : outputOf(repl), true);
  } finally {
    for (const fd of opened) fs.closeSync(fd);
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
