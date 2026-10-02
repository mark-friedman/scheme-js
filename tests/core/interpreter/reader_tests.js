import { assert, createTestLogger } from '../../harness/helpers.js';
import { parse } from '../../../src/core/interpreter/reader.js';
import { Cons } from '../../../src/core/interpreter/cons.js';
import { Symbol } from '../../../src/core/interpreter/symbol.js';

export function runReaderTests(logger) {
    logger.title('Running Reader Unit Tests...');

    // 1. Comments
    const c1 = parse("; comment\n123");
    assert(logger, "Skip comment line", c1[0], 123);

    const c2 = parse("123; comment");  // No space before semicolon
    assert(logger, "Skip comment end of line (no space)", c2[0], 123);

    const c3 = parse("123 ; comment");  // Space before semicolon
    assert(logger, "Skip comment end of line (with space)", c3[0], 123);

    // 2. Strings with escapes
    const s1 = parse(`"foo \\" bar"`);
    assert(logger, "Escaped quote", s1[0], 'foo " bar');

    const s2 = parse(`"foo \\n bar"`);
    assert(logger, "Escaped newline", s2[0], 'foo \n bar');

    // 3. Vectors
    const v1 = parse("#(1 2 3)");
    assert(logger, "Vector array", Array.isArray(v1[0]), true);
    assert(logger, "Vector content", v1[0][0], 1);

    // 4. Improper Lists
    const i1 = parse("(1 . 2)");
    assert(logger, "Improper pair car", i1[0].car, 1);
    assert(logger, "Improper pair cdr", i1[0].cdr, 2);

    const i2 = parse("(1 2 . 3)");
    assert(logger, "Improper list 2nd element", i2[0].cdr.car, 2);
    assert(logger, "Improper list tail", i2[0].cdr.cdr, 3); // last cdr is atom

    // 5. Numbers
    assert(logger, "Number float", parse("1.5")[0], 1.5);
    assert(logger, "Number negative", parse("-10")[0], -10);
    // Nan/Inf
    assert(logger, "+inf.0", parse("+inf.0")[0], Infinity);
    assert(logger, "-inf.0", parse("-inf.0")[0], -Infinity);
    assert(logger, "+nan.0", isNaN(parse("+nan.0")[0]), true);

    // 6. Booleans
    assert(logger, "Bool #t", parse("#t")[0], true);
    assert(logger, "Bool #f", parse("#f")[0], false);

    // 7. Whitespace
    assert(logger, "Multiple spaces", parse("   1   ")[0], 1);
    assert(logger, "Newlines", parse("\n1\n")[0], 1);

    // 8. Bytevector elements are any exact integer datum from 0 to 255,
    // written in any radix or with an exactness prefix (R7RS 6.9).
    const bytes = (text) => Array.from(parse(text)[0]).join(' ');
    assert(logger, "Bytevector decimal", bytes("#u8(0 65 255)"), "0 65 255");
    assert(logger, "Bytevector hex, binary, octal", bytes("#u8(#x41 #b1000010 #o103)"), "65 66 67");
    assert(logger, "Bytevector exactness prefix", bytes("#u8(#e65 #e1e2)"), "65 100");
    const rejects = (text) => { try { parse(text); return false; } catch (e) { return true; } };
    assert(logger, "Bytevector rejects an inexact element", rejects("#u8(65.5)"), true);
    assert(logger, "Bytevector rejects an inexact integer", rejects("#u8(1e2)"), true);
    assert(logger, "Bytevector rejects 256", rejects("#u8(256)"), true);
    assert(logger, "Bytevector rejects a negative", rejects("#u8(-1)"), true);
    assert(logger, "Bytevector rejects a symbol", rejects("#u8(a)"), true);

    // 9. A line comment ends at CR LF and at CR, not only LF (R7RS 2.2)
    assert(logger, "Comment ending in CR LF", parse("; comment\r\n1 ; more\r\n2").length, 2);
    assert(logger, "Comment ending in CR", parse("; comment\r1").length, 1);

    // 10. Square brackets are reserved for future extensions (R7RS 2.3):
    // reading one is an error naming it and where it is, not a reader that
    // never returns. In a string, a |symbol| or a character they are text.
    const readError = (text) => {
        try {
            parse(text, { suppressLog: true });
            return null;
        } catch (e) {
            return e.message;
        }
    };
    assert(logger, "Open bracket is an error", /'\[' is reserved/.test(readError("[a b]")), true);
    assert(logger, "Close bracket is an error", /'\]' is reserved/.test(readError("(a ] b)")), true);
    assert(logger, "Bracketed let binding is an error", /'\[' is reserved/.test(readError("(let ([x 1]) x)")), true);
    assert(logger, "Bracket after an identifier is an error", /'\[' is reserved/.test(readError("a[0]")), true);
    assert(logger, "Bracket error gives its line", /at line 2/.test(readError("(a\n [b])")), true);
    assert(logger, "Bracket after #; is an error", /'\[' is reserved/.test(readError("#; [a] b")), true);
    assert(logger, "Brackets in a string", parse('"[a]"')[0], "[a]");
    assert(logger, "Brackets in a |symbol|", parse("|[a]|")[0].name, "[a]");
    const brackets = parse("(#\\[ #\\])")[0];
    assert(logger, "Bracket characters", [brackets.car.codePoint, brackets.cdr.car.codePoint], [0x5b, 0x5d]);
    assert(logger, "Brackets in comments", parse("; [a]\n#| ] |# 1")[0], 1);

    // 11. An error for input that ends inside a datum is marked incomplete,
    // so that a REPL asks for another line instead of reporting it; an error
    // more input cannot mend is not.
    const incomplete = (text) => {
        try {
            parse(text, { suppressLog: true });
            return 'read';
        } catch (e) {
            return e.incomplete;
        }
    };
    for (const text of ["(a", "(a (b)", "#(1", "#u8(1", "#{(a 1)", "#{(a", "'", "(a '", ",@",
        "(a .", "(a . b", "(a . #;", "#;", "(a #;", "#0=", "\"abc", "\"a\\\"", "|abc", "|",
        "#| a", "#\\"]) {
        assert(logger, `Incomplete: ${text}`, incomplete(text), true);
    }
    for (const text of [")", "(a . )", "(1 . 2 3)", "( #; )", "#u8(256)", "#u8(a)", "#\\nosuchname",
        "[a]", "#1#", "(. a)"]) {
        assert(logger, `Not incomplete: ${text}`, incomplete(text), false);
    }

    // 12. A string or |symbol| whose last delimiter is escaped is not ended
    assert(logger, "A string of one escaped quote is unterminated",
        /unterminated string/.test(readError('"\\"')), true);
    assert(logger, "A string ending in an escaped quote is unterminated",
        /unterminated string/.test(readError('(display "a\\")')), true);
    assert(logger, "A string ending in an escaped backslash ends", parse('"a\\\\"')[0], "a\\");
    assert(logger, "An unterminated |symbol| is an error", /unterminated \|symbol\|/.test(readError("|abc")), true);
    assert(logger, "A lone | is an unterminated |symbol|", /unterminated \|symbol\|/.test(readError("|")), true);
    assert(logger, "A |symbol| ending in an escaped | is unterminated",
        /unterminated \|symbol\|/.test(readError("|a\\|")), true);
    assert(logger, "An unterminated string gives its line", /at line 2/.test(readError('(a\n "b)')), true);
    assert(logger, "#\\ at the end is an error", /end of input after #\\/.test(readError("(a #\\")), true);

}
