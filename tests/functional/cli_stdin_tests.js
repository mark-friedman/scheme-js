/**
 * The CLI's programs read the process's standard input.
 *
 * A program run as `node repl.js prog.scm` or `node repl.js -e ...` has
 * standard input as its current input port, so that a Scheme program can
 * stand in a shell pipeline; the interactive REPL, whose own input is standard
 * input, keeps the empty port it always had. Each test runs `repl.js` in a
 * child process with input piped into it, so these are Node-only. The
 * programs `write` what they read.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { assert } from '../harness/helpers.js';
import { runCli, inSteps, outputOf } from '../harness/cli_process.js';

/**
 * Runs the tests of the CLI reading standard input.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>} Resolves when every run has finished.
 */
export async function runCliStdinTests(logger) {
  if (typeof process === 'undefined') {
    logger.skip('CLI standard input tests (Node.js only)');
    return;
  }

  logger.title('CLI - programs read standard input');

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-js-cli-stdin-'));
  let fileStdin;

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

  try {
    const numberLines = program('number_lines.scm', `
      ;; Writes each line of standard input upcased, numbered from 1.
      (define (number-lines n)
        (let ((line (read-line)))
          (if (not (eof-object? line))
              (begin
                (write n) (display " ") (display (string-upcase line)) (newline)
                (number-lines (+ n 1))))))
      (number-lines 1)
    `);
    const characters = program('characters.scm', `
      (let* ((peeked (peek-char))
             (first (read-char))
             (second (read-char))
             (end (read-char))
             (end-peeked (peek-char)))
        (write (list peeked first second end end-peeked (char? first)))
        (newline))
    `);
    const data = program('data.scm', `
      (let* ((a (read)) (b (read)) (c (read)) (d (read)))
        (write (list a b c d))
        (newline))
    `);
    const strings = program('strings.scm', `
      (let* ((a (read-string 5)) (b (read-string 100)) (c (read-string 1)))
        (write (list a b c))
        (newline))
    `);
    const empty = program('empty.scm', `
      (let* ((c (read-char))
             (p (peek-char))
             (l (read-line))
             (s (read-string 1))
             (d (read))
             (ready (char-ready?)))
        (write (list c p l s d ready))
        (newline))
    `);
    const ready = program('ready.scm', `
      (let* ((before (char-ready?))
             (line (read-line))
             (after (char-ready?)))
        (write (list before line after))
        (newline))
    `);
    const unicode = program('unicode.scm', `
      (let ((line (read-line)))
        (write (string->list line))
        (newline)
        (display line)
        (newline))
    `);
    const inputFile = program('input.txt', 'from file\n');
    const fromFile = program('from_file.scm', `
      (let* ((from-file (with-input-from-file ${JSON.stringify(inputFile)} read-line))
             (from-stdin (read-line)))
        (write (list from-file from-stdin))
        (newline))
    `);
    const whichPort = program('which_port.scm', `
      (write (list (eq? (current-input-port) (standard-input-port))
                   (input-port? (current-input-port))
                   (textual-port? (current-input-port))))
      (newline)
    `);
    const echo = program('echo.scm', `
      ;; Answers each line as it arrives.
      (define (echo)
        (let ((line (read-line)))
          (if (eof-object? line)
              (begin (display "done") (newline))
              (begin (display "got ") (display line) (newline) (echo)))))
      (echo)
    `);
    fileStdin = fs.openSync(program('stdin.txt', 'line\nmore'), 'r');

    const runs = await Promise.all([
      runCli(['--no-compile', '-e', '(list (read-line) (read-line) (read-line))'], { input: 'a\nb\n' }),
      runCli(['-e', '(list (read-line) (read-line) (read-line))'], { input: 'a\nb\n' }),
      runCli(['--no-compile', numberLines], { input: 'one\ntwo\nthree' }),
      runCli([numberLines], { input: 'one\ntwo\nthree' }),
      runCli([characters], { input: 'xy' }),
      runCli([data], { input: '(1 "two" #\\3) sym\n42' }),
      runCli([strings], { input: 'hello world' }),
      runCli([empty], { input: '' }),
      runCli([ready], { stdin: fileStdin }),
      runCli([unicode], { input: 'héllo ✓ 😀\n' }),
      runCli([fromFile], { input: 'from stdin\n' }),
      runCli([whichPort]),
      // The second line is written only once the program has answered the
      // first, so it must read what has arrived rather than all its input.
      runCli([echo], {
        onOutput: inSteps([{ after: '', write: 'first\n' }, { after: 'got first\n', write: 'second\n' }])
      }),
      // The second line is typed only once the REPL has answered the first,
      // so a `(read-line)` reading standard input would wait for it.
      runCli([], {
        onOutput: inSteps([{ after: '', write: '(read-line)\n' }, { after: '#<eof>', write: '(+ 1 2)\n' }])
      })
    ]);
    const [eInterpreted, eCompiled, linesInterpreted, linesCompiled, chars, datum, string,
      atEnd, fileReady, utf8, restored, port, streamed, repl] = runs;

    assert(logger, '-e reads lines, then the end of the input (interpreted)',
      outputOf(eInterpreted), '("a" "b" #<eof>)\n');
    assert(logger, '-e reads lines, then the end of the input (compiled)',
      outputOf(eCompiled), '("a" "b" #<eof>)\n');
    assert(logger, 'a program file reads every line, the last unterminated (interpreted)',
      outputOf(linesInterpreted), '1 ONE\n2 TWO\n3 THREE\n');
    assert(logger, 'a program file reads every line, the last unterminated (compiled)',
      outputOf(linesCompiled), '1 ONE\n2 TWO\n3 THREE\n');
    assert(logger, 'peek-char and read-char return characters, then the end',
      outputOf(chars), '(#\\x #\\x #\\y #<eof> #<eof> #t)\n');
    assert(logger, 'read reads data across lines, then the end',
      outputOf(datum), '((1 "two" #\\3) sym 42 #<eof>)\n');
    assert(logger, 'read-string reads k characters, what is left, then the end',
      outputOf(string), '("hello" " world" #<eof>)\n');
    assert(logger, 'every read of empty input is the end, and char-ready? is #t there',
      outputOf(atEnd), '(#<eof> #<eof> #<eof> #<eof> #<eof> #t)\n');
    assert(logger, 'char-ready? is #t when standard input is a file',
      outputOf(fileReady), '(#t "line" #t)\n');
    assert(logger, 'input is decoded as UTF-8',
      outputOf(utf8), '(#\\h #\\é #\\l #\\l #\\o #\\space #\\✓ #\\space #\\😀)\nhéllo ✓ 😀\n');
    assert(logger, 'with-input-from-file puts standard input back',
      outputOf(restored), '("from file" "from stdin")\n');
    assert(logger, 'the current input port is the standard input port',
      outputOf(port), '(#t #t #t)\n');
    assert(logger, 'a program answers each line as it arrives',
      outputOf(streamed), 'got first\ngot second\ndone\n');

    // The REPL reads its own input from standard input, so `(read-line)`
    // typed into it reads the empty port, and the next line typed is still
    // the REPL's.
    assert(logger, 'the interactive REPL answers (read-line) and then what is typed next',
      repl.status === 0 && repl.stdout.includes('> #<eof>\n') && repl.stdout.includes('> 3\n')
        ? true : outputOf(repl), true);
  } finally {
    if (fileStdin !== undefined) fs.closeSync(fileStdin);
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
