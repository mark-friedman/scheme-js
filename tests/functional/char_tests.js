/**
 * Character Primitive Tests
 * 
 * Tests R7RS §6.6 character operations.
 */

import { assert, run, createTestLogger, createTestEnv } from '../harness/helpers.js';

/**
 * Runs character primitive tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runCharTests(interpreter, logger) {
    logger.title("Character Tests - Reader");

    // Character literal parsing
    let result = run(interpreter, `#\\a`);
    assert(logger, "Char literal: #\\a", result, 'a');

    result = run(interpreter, `#\\A`);
    assert(logger, "Char literal: #\\A", result, 'A');

    result = run(interpreter, `#\\space`);
    assert(logger, "Char literal: #\\space", result, ' ');

    result = run(interpreter, `#\\newline`);
    assert(logger, "Char literal: #\\newline", result, '\n');

    result = run(interpreter, `#\\tab`);
    assert(logger, "Char literal: #\\tab", result, '\t');

    result = run(interpreter, `#\\x41`);
    assert(logger, "Char literal: #\\x41 (hex A)", result, 'A');

    result = run(interpreter, `#\\x7a`);
    assert(logger, "Char literal: #\\x7a (hex z)", result, 'z');

    // -------------------------------------------------------------------------
    logger.title("Character Tests - Predicates");

    result = run(interpreter, `(char? #\\a)`);
    assert(logger, "char? on char", result, true);

    result = run(interpreter, `(char? "a")`);
    // Single-char strings ARE characters in this implementation
    assert(logger, "char? on single-char string", result, true);

    result = run(interpreter, `(char? "ab")`);
    assert(logger, "char? on multi-char string", result, false);

    result = run(interpreter, `(char? 65)`);
    assert(logger, "char? on number", result, false);

    // -------------------------------------------------------------------------
    logger.title("Character Tests - Comparison");

    result = run(interpreter, `(char=? #\\a #\\a)`);
    assert(logger, "char=? equal", result, true);

    result = run(interpreter, `(char=? #\\a #\\b)`);
    assert(logger, "char=? not equal", result, false);

    result = run(interpreter, `(char<? #\\a #\\b)`);
    assert(logger, "char<? a < b", result, true);

    result = run(interpreter, `(char<? #\\b #\\a)`);
    assert(logger, "char<? b < a", result, false);

    result = run(interpreter, `(char>? #\\b #\\a)`);
    assert(logger, "char>? b > a", result, true);

    result = run(interpreter, `(char<=? #\\a #\\a)`);
    assert(logger, "char<=? equal", result, true);

    result = run(interpreter, `(char<=? #\\a #\\b)`);
    assert(logger, "char<=? less", result, true);

    result = run(interpreter, `(char>=? #\\b #\\a)`);
    assert(logger, "char>=? greater", result, true);

    // Variadic
    result = run(interpreter, `(char<? #\\a #\\b #\\c)`);
    assert(logger, "char<? variadic (a b c)", result, true);

    result = run(interpreter, `(char<? #\\a #\\c #\\b)`);
    assert(logger, "char<? variadic (a c b) false", result, false);

    // -------------------------------------------------------------------------
    logger.title("Character Tests - Case-Insensitive Comparison");

    result = run(interpreter, `(char-ci=? #\\a #\\A)`);
    assert(logger, "char-ci=? a A", result, true);

    result = run(interpreter, `(char-ci<? #\\A #\\b)`);
    assert(logger, "char-ci<? A b", result, true);

    // -------------------------------------------------------------------------
    logger.title("Character Tests - Class Predicates");

    result = run(interpreter, `(char-alphabetic? #\\a)`);
    assert(logger, "char-alphabetic? a", result, true);

    result = run(interpreter, `(char-alphabetic? #\\5)`);
    assert(logger, "char-alphabetic? 5", result, false);

    result = run(interpreter, `(char-numeric? #\\5)`);
    assert(logger, "char-numeric? 5", result, true);

    result = run(interpreter, `(char-numeric? #\\a)`);
    assert(logger, "char-numeric? a", result, false);

    result = run(interpreter, `(char-whitespace? #\\space)`);
    assert(logger, "char-whitespace? space", result, true);

    result = run(interpreter, `(char-whitespace? #\\a)`);
    assert(logger, "char-whitespace? a", result, false);

    result = run(interpreter, `(char-upper-case? #\\A)`);
    assert(logger, "char-upper-case? A", result, true);

    result = run(interpreter, `(char-upper-case? #\\a)`);
    assert(logger, "char-upper-case? a", result, false);

    result = run(interpreter, `(char-lower-case? #\\a)`);
    assert(logger, "char-lower-case? a", result, true);

    // -------------------------------------------------------------------------
    logger.title("Character Tests - Conversion");

    result = run(interpreter, `(char->integer #\\A)`);
    assert(logger, "char->integer A", result, 65);

    result = run(interpreter, `(integer->char 65)`);
    assert(logger, "integer->char 65", result, 'A');

    result = run(interpreter, `(char-upcase #\\a)`);
    assert(logger, "char-upcase a", result, 'A');

    result = run(interpreter, `(char-downcase #\\A)`);
    assert(logger, "char-downcase A", result, 'a');

    result = run(interpreter, `(char-foldcase #\\A)`);
    assert(logger, "char-foldcase A", result, 'a');

    // -------------------------------------------------------------------------
    logger.title("Character Tests - Digit Value");

    result = run(interpreter, `(digit-value #\\5)`);
    assert(logger, "digit-value 5", result, 5);

    result = run(interpreter, `(digit-value #\\0)`);
    assert(logger, "digit-value 0", result, 0);

    result = run(interpreter, `(digit-value #\\a)`);
    assert(logger, "digit-value a (not digit)", result, false);

    // -------------------------------------------------------------------------
    logger.title("Character Tests - Error Cases");

    try {
        run(interpreter, `(char=? #\\a)`);
        logger.fail("char=? arity check - should throw");
    } catch (e) {
        assert(logger, "char=? requires 2+ args", e.message.includes("arity") || e.message.includes("argument"), true);
    }

    try {
        run(interpreter, `(char-upcase "ab")`);
        logger.fail("char-upcase type check - should throw");
    } catch (e) {
        assert(logger, "char-upcase type check", e.message.includes("character") || e.message.includes("type"), true);
    }

    try {
        run(interpreter, `(integer->char -1)`);
        logger.fail("integer->char range check - should throw");
    } catch (e) {
        assert(logger, "integer->char range check", e.message.includes("range") || e.message.includes("code point"), true);
    }
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runCharTests(interpreter, logger);
}
