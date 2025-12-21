/**
 * String Primitive Tests
 * 
 * Tests R7RS §6.7 string operations.
 */

import { assert, run, createTestLogger, createTestEnv } from '../helpers.js';

/**
 * Runs string primitive tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runStringTests(interpreter, logger) {
    logger.title("String Tests - Constructors");

    let result = run(interpreter, `(make-string 5 #\\a)`);
    assert(logger, "make-string 5 a", result, 'aaaaa');

    result = run(interpreter, `(make-string 3)`);
    assert(logger, "make-string 3 (default fill)", result, '   ');

    result = run(interpreter, `(make-string 0)`);
    assert(logger, "make-string 0", result, '');

    result = run(interpreter, `(string #\\h #\\i)`);
    assert(logger, "string from chars", result, 'hi');

    result = run(interpreter, `(string)`);
    assert(logger, "string (empty)", result, '');

    // -------------------------------------------------------------------------
    logger.title("String Tests - Accessors");

    result = run(interpreter, `(string-length "hello")`);
    assert(logger, "string-length", result, 5);

    result = run(interpreter, `(string-length "")`);
    assert(logger, "string-length empty", result, 0);

    result = run(interpreter, `(string-ref "hello" 0)`);
    assert(logger, "string-ref 0", result, 'h');

    result = run(interpreter, `(string-ref "hello" 4)`);
    assert(logger, "string-ref 4", result, 'o');

    // -------------------------------------------------------------------------
    logger.title("String Tests - Comparison");

    result = run(interpreter, `(string=? "abc" "abc")`);
    assert(logger, "string=? equal", result, true);

    result = run(interpreter, `(string=? "abc" "abd")`);
    assert(logger, "string=? not equal", result, false);

    result = run(interpreter, `(string<? "abc" "abd")`);
    assert(logger, "string<? abc < abd", result, true);

    result = run(interpreter, `(string<? "abd" "abc")`);
    assert(logger, "string<? abd < abc", result, false);

    result = run(interpreter, `(string>? "abd" "abc")`);
    assert(logger, "string>? abd > abc", result, true);

    result = run(interpreter, `(string<=? "abc" "abc")`);
    assert(logger, "string<=? equal", result, true);

    result = run(interpreter, `(string>=? "abc" "abc")`);
    assert(logger, "string>=? equal", result, true);

    // Variadic
    result = run(interpreter, `(string<? "a" "b" "c")`);
    assert(logger, "string<? variadic (a b c)", result, true);

    result = run(interpreter, `(string<? "a" "c" "b")`);
    assert(logger, "string<? variadic (a c b) false", result, false);

    // -------------------------------------------------------------------------
    logger.title("String Tests - Case-Insensitive Comparison");

    result = run(interpreter, `(string-ci=? "ABC" "abc")`);
    assert(logger, "string-ci=? ABC abc", result, true);

    result = run(interpreter, `(string-ci<? "ABC" "abd")`);
    assert(logger, "string-ci<? ABC abd", result, true);

    // -------------------------------------------------------------------------
    logger.title("String Tests - Substring and Copy");

    result = run(interpreter, `(substring "hello" 1 4)`);
    assert(logger, "substring 1 4", result, 'ell');

    result = run(interpreter, `(substring "hello" 0 5)`);
    assert(logger, "substring 0 5 (full)", result, 'hello');

    result = run(interpreter, `(string-append "hello" " " "world")`);
    assert(logger, "string-append", result, 'hello world');

    result = run(interpreter, `(string-append)`);
    assert(logger, "string-append empty", result, '');

    result = run(interpreter, `(string-copy "hello")`);
    assert(logger, "string-copy", result, 'hello');

    result = run(interpreter, `(string-copy "hello" 1 4)`);
    assert(logger, "string-copy with range", result, 'ell');

    // -------------------------------------------------------------------------
    logger.title("String Tests - Conversion");

    result = run(interpreter, `(string->list "abc")`);
    // Check it's a list with correct elements
    const listResult = run(interpreter, `(car (string->list "abc"))`);
    assert(logger, "string->list car", listResult, 'a');

    const listLen = run(interpreter, `(length (string->list "hello"))`);
    assert(logger, "string->list length", listLen, 5);

    result = run(interpreter, `(list->string (list #\\a #\\b #\\c))`);
    assert(logger, "list->string", result, 'abc');

    result = run(interpreter, `(number->string 42)`);
    assert(logger, "number->string 42", result, '42');

    result = run(interpreter, `(number->string 255 16)`);
    assert(logger, "number->string 255 hex", result, 'ff');

    result = run(interpreter, `(string->number "42")`);
    assert(logger, "string->number 42", result, 42);

    result = run(interpreter, `(string->number "ff" 16)`);
    assert(logger, "string->number ff hex", result, 255);

    result = run(interpreter, `(string->number "not-a-number")`);
    assert(logger, "string->number invalid", result, false);

    result = run(interpreter, `(string->number "+inf.0")`);
    assert(logger, "string->number +inf.0", result, Infinity);

    // -------------------------------------------------------------------------
    logger.title("String Tests - Case Conversion");

    result = run(interpreter, `(string-upcase "hello")`);
    assert(logger, "string-upcase", result, 'HELLO');

    result = run(interpreter, `(string-downcase "HELLO")`);
    assert(logger, "string-downcase", result, 'hello');

    result = run(interpreter, `(string-foldcase "HeLLo")`);
    assert(logger, "string-foldcase", result, 'hello');

    // -------------------------------------------------------------------------
    logger.title("String Tests - Immutability Errors");

    try {
        run(interpreter, `(string-set! "hello" 0 #\\H)`);
        logger.fail("string-set! should throw");
    } catch (e) {
        assert(logger, "string-set! throws immutability error",
            e.message.includes("immutable") || e.message.includes("Immutable"), true);
    }

    try {
        run(interpreter, `(string-fill! "hello" #\\x)`);
        logger.fail("string-fill! should throw");
    } catch (e) {
        assert(logger, "string-fill! throws immutability error",
            e.message.includes("immutable") || e.message.includes("Immutable"), true);
    }

    // -------------------------------------------------------------------------
    logger.title("String Tests - Error Cases");

    try {
        run(interpreter, `(string-ref "hello" 10)`);
        logger.fail("string-ref out of bounds - should throw");
    } catch (e) {
        assert(logger, "string-ref bounds check", e.message.includes("range") || e.message.includes("index"), true);
    }

    try {
        run(interpreter, `(string-ref "hello" -1)`);
        logger.fail("string-ref negative - should throw");
    } catch (e) {
        assert(logger, "string-ref negative check", e.message.includes("range") || e.message.includes("index"), true);
    }

    try {
        run(interpreter, `(make-string -1)`);
        logger.fail("make-string negative - should throw");
    } catch (e) {
        assert(logger, "make-string negative check", e.message.includes("range") || e.message.includes("length"), true);
    }

    try {
        run(interpreter, `(substring "hello" 3 1)`);
        logger.fail("substring invalid range - should throw");
    } catch (e) {
        assert(logger, "substring range check", e.message.includes("range") || e.message.includes("end"), true);
    }
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runStringTests(interpreter, logger);
}
