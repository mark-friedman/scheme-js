/**
 * I/O Primitive Tests
 * 
 * Tests R7RS §6.13 I/O operations.
 */

import { assert, run, createTestLogger, createTestEnv } from '../helpers.js';

/**
 * Runs I/O primitive tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runIOTests(interpreter, logger) {
    // ===========================================================================
    logger.title("I/O Tests - Port Predicates");

    let result = run(interpreter, `(port? (open-input-string "hello"))`);
    assert(logger, "port? on input string port", result, true);

    result = run(interpreter, `(port? (open-output-string))`);
    assert(logger, "port? on output string port", result, true);

    result = run(interpreter, `(port? 42)`);
    assert(logger, "port? on number", result, false);

    result = run(interpreter, `(input-port? (open-input-string "x"))`);
    assert(logger, "input-port? on input port", result, true);

    result = run(interpreter, `(input-port? (open-output-string))`);
    assert(logger, "input-port? on output port", result, false);

    result = run(interpreter, `(output-port? (open-output-string))`);
    assert(logger, "output-port? on output port", result, true);

    result = run(interpreter, `(output-port? (open-input-string "x"))`);
    assert(logger, "output-port? on input port", result, false);

    result = run(interpreter, `(textual-port? (open-input-string "x"))`);
    assert(logger, "textual-port?", result, true);

    result = run(interpreter, `(binary-port? (open-input-string "x"))`);
    assert(logger, "binary-port? (false for textual)", result, false);

    // ===========================================================================
    logger.title("I/O Tests - Port Open State");

    result = run(interpreter, `
    (let ((p (open-input-string "abc")))
      (input-port-open? p))
  `);
    assert(logger, "input-port-open? on open port", result, true);

    result = run(interpreter, `
    (let ((p (open-input-string "abc")))
      (close-port p)
      (input-port-open? p))
  `);
    assert(logger, "input-port-open? after close", result, false);

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (output-port-open? p))
  `);
    assert(logger, "output-port-open? on open port", result, true);

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (close-output-port p)
      (output-port-open? p))
  `);
    assert(logger, "output-port-open? after close", result, false);

    // ===========================================================================
    logger.title("I/O Tests - Current Ports");

    result = run(interpreter, `(port? (current-input-port))`);
    assert(logger, "current-input-port is a port", result, true);

    result = run(interpreter, `(input-port? (current-input-port))`);
    assert(logger, "current-input-port is input", result, true);

    result = run(interpreter, `(port? (current-output-port))`);
    assert(logger, "current-output-port is a port", result, true);

    result = run(interpreter, `(output-port? (current-output-port))`);
    assert(logger, "current-output-port is output", result, true);

    result = run(interpreter, `(output-port? (current-error-port))`);
    assert(logger, "current-error-port is output", result, true);

    // ===========================================================================
    logger.title("I/O Tests - String Input Port");

    result = run(interpreter, `
    (let ((p (open-input-string "hello")))
      (read-char p))
  `);
    assert(logger, "read-char returns first char", result, 'h');

    result = run(interpreter, `
    (let ((p (open-input-string "hello")))
      (read-char p)
      (read-char p))
  `);
    assert(logger, "read-char second char", result, 'e');

    result = run(interpreter, `
    (let ((p (open-input-string "ab")))
      (peek-char p))
  `);
    assert(logger, "peek-char first", result, 'a');

    result = run(interpreter, `
    (let ((p (open-input-string "ab")))
      (peek-char p)
      (peek-char p))
  `);
    assert(logger, "peek-char doesn't consume", result, 'a');

    result = run(interpreter, `
    (let ((p (open-input-string "ab")))
      (peek-char p)
      (read-char p))
  `);
    assert(logger, "peek then read same char", result, 'a');

    result = run(interpreter, `
    (let ((p (open-input-string "hello")))
      (read-string 3 p))
  `);
    assert(logger, "read-string 3", result, 'hel');

    result = run(interpreter, `
    (let ((p (open-input-string "hi")))
      (read-string 10 p))
  `);
    assert(logger, "read-string more than available", result, 'hi');

    result = run(interpreter, `
    (let ((p (open-input-string "hello")))
      (char-ready? p))
  `);
    assert(logger, "char-ready? on string port", result, true);

    // ===========================================================================
    logger.title("I/O Tests - read-line");

    result = run(interpreter, `
    (let ((p (open-input-string "line1")))
      (read-line p))
  `);
    assert(logger, "read-line single line no newline", result, 'line1');

    result = run(interpreter, `
    (let ((p (open-input-string "line1\nline2")))
      (read-line p))
  `);
    assert(logger, "read-line first of two lines", result, 'line1');

    result = run(interpreter, `
    (let ((p (open-input-string "line1\nline2")))
      (read-line p)
      (read-line p))
  `);
    assert(logger, "read-line second line", result, 'line2');

    result = run(interpreter, `
    (let ((p (open-input-string "line1\r\nline2")))
      (read-line p))
  `);
    assert(logger, "read-line handles CRLF", result, 'line1');

    // ===========================================================================
    logger.title("I/O Tests - EOF Handling");

    result = run(interpreter, `(eof-object? (eof-object))`);
    assert(logger, "eof-object? on eof-object", result, true);

    result = run(interpreter, `(eof-object? 'eof)`);
    assert(logger, "eof-object? on symbol", result, false);

    result = run(interpreter, `(eof-object? #f)`);
    assert(logger, "eof-object? on #f", result, false);

    result = run(interpreter, `
    (let ((p (open-input-string "")))
      (eof-object? (read-char p)))
  `);
    assert(logger, "read-char from empty returns eof", result, true);

    result = run(interpreter, `
    (let ((p (open-input-string "x")))
      (read-char p)
      (eof-object? (read-char p)))
  `);
    assert(logger, "read-char past end returns eof", result, true);

    result = run(interpreter, `
    (let ((p (open-input-string "")))
      (eof-object? (read-line p)))
  `);
    assert(logger, "read-line from empty returns eof", result, true);

    result = run(interpreter, `
    (let ((p (open-input-string "")))
      (eof-object? (read-string 5 p)))
  `);
    assert(logger, "read-string from empty returns eof", result, true);

    // ===========================================================================
    logger.title("I/O Tests - String Output Port");

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (write-char #\\H p)
      (get-output-string p))
  `);
    assert(logger, "write-char to string port", result, 'H');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (write-string "Hello" p)
      (get-output-string p))
  `);
    assert(logger, "write-string to string port", result, 'Hello');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (write-string "Hello" p)
      (write-string " World" p)
      (get-output-string p))
  `);
    assert(logger, "multiple write-string calls", result, 'Hello World');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (display "test" p)
      (get-output-string p))
  `);
    assert(logger, "display to string port", result, 'test');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (newline p)
      (get-output-string p))
  `);
    assert(logger, "newline to string port", result, '\n');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (display "Hi")
      (display "!" p)
      (get-output-string p))
  `);
    // First display goes to current-output-port, second to p
    assert(logger, "display with explicit port", result, '!');

    // ===========================================================================
    logger.title("I/O Tests - write vs display");

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (display "hello" p)
      (get-output-string p))
  `);
    assert(logger, "display string unquoted", result, 'hello');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (write "hello" p)
      (get-output-string p))
  `);
    assert(logger, "write string quoted", result, '"hello"');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (display #t p)
      (get-output-string p))
  `);
    assert(logger, "display boolean", result, '#t');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (display 42 p)
      (get-output-string p))
  `);
    assert(logger, "display number", result, '42');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (display '() p)
      (get-output-string p))
  `);
    assert(logger, "display empty list", result, '()');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (display '(1 2 3) p)
      (get-output-string p))
  `);
    assert(logger, "display list", result, '(1 2 3)');

    result = run(interpreter, `
    (let ((p (open-output-string)))
      (display '#(a b c) p)
      (get-output-string p))
  `);
    assert(logger, "display vector", result, '#(a b c)');

    // ===========================================================================
    logger.title("I/O Tests - Error Cases");

    try {
        run(interpreter, `(open-input-string 42)`);
        logger.fail("open-input-string with non-string - should throw");
    } catch (e) {
        assert(logger, "open-input-string type check",
            e.message.includes('string'), true);
    }

    try {
        run(interpreter, `(get-output-string (open-input-string "x"))`);
        logger.fail("get-output-string on input port - should throw");
    } catch (e) {
        assert(logger, "get-output-string port type check",
            e.message.includes('output'), true);
    }

    try {
        run(interpreter, `
      (let ((p (open-input-string "x")))
        (close-port p)
        (read-char p))
    `);
        logger.fail("read-char on closed port - should throw");
    } catch (e) {
        assert(logger, "read-char closed port check",
            e.message.includes('closed'), true);
    }

    try {
        run(interpreter, `
      (let ((p (open-output-string)))
        (close-port p)
        (write-char #\\x p))
    `);
        logger.fail("write-char on closed port - should throw");
    } catch (e) {
        assert(logger, "write-char closed port check",
            e.message.includes('closed'), true);
    }

    try {
        run(interpreter, `(write-char "ab" (open-output-string))`);
        logger.fail("write-char with string - should throw");
    } catch (e) {
        assert(logger, "write-char expects character",
            e.message.includes('character'), true);
    }

    try {
        run(interpreter, `(read-string -1 (open-input-string "x"))`);
        logger.fail("read-string with negative - should throw");
    } catch (e) {
        assert(logger, "read-string negative check",
            e.message.includes('non-negative') || e.message.includes('integer'), true);
    }

    try {
        run(interpreter, `(input-port-open? 42)`);
        logger.fail("input-port-open? on non-port - should throw");
    } catch (e) {
        assert(logger, "input-port-open? type check",
            e.message.includes('input port'), true);
    }

    try {
        run(interpreter, `(output-port-open? "x")`);
        logger.fail("output-port-open? on non-port - should throw");
    } catch (e) {
        assert(logger, "output-port-open? type check",
            e.message.includes('output port'), true);
    }
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runIOTests(interpreter, logger);
}
