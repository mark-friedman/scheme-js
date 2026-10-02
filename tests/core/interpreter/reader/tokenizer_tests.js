/**
 * @fileoverview Unit tests for reader/tokenizer.js
 */

import { tokenize } from '../../../../src/core/interpreter/reader/tokenizer.js';
import { assert } from '../../../harness/helpers.js';

export function runTokenizerTests(logger) {
    logger.title('tokenize - block comments');

    /**
     * The values of the tokens a string is read into.
     * @param {string} input - Source code.
     * @returns {string} The token values, separated by spaces.
     */
    const values = (input) => tokenize(input).map((t) => t.value).join(' ');

    /**
     * The message of the error tokenizing a string raises, or null if none.
     * @param {string} input - Source code.
     * @returns {string|null}
     */
    const tokenizeError = (input) => {
        try {
            tokenize(input);
            return null;
        } catch (e) {
            return e.message;
        }
    };

    // A block comment is skipped like whitespace, nested ones with it
    assert(logger, 'a block comment is skipped', values('(define x #| comment |# 10)'), '( define x 10 )');
    assert(logger, 'nested block comments are skipped', values('(a #| outer #| inner |# outer |# b)'), '( a b )');
    assert(logger, 'a block comment over several lines', values('a #| one\ntwo\r\nthree |# b'), 'a b');
    assert(logger, 'only a block comment', values('#| nothing |#'), '');
    assert(logger, 'a token after a block comment follows space',
        tokenize('a#|c|#b').map((t) => t.hasPrecedingSpace), [true, true]);

    // R7RS 2.2: #| starts a comment only where a token can start, so what is
    // in a string, a |symbol| or a character is never one
    assert(logger, '#| in a string', values('"#|"'), '"#|"');
    assert(logger, '#| and |# in strings', values('(a "#|x" "y|#" b)'), '( a "#|x" "y|#" b )');
    assert(logger, '#| in a string after an escaped quote', values('"\\"#|"'), '"\\"#|"');
    assert(logger, 'a |symbol| ending in #', values('(|a#| b)'), '( |a#| b )');
    assert(logger, 'the |symbol| #', values('(|#| b)'), '( |#| b )');
    assert(logger, 'a |symbol| holding #|', values('|a#\\|b|'), '|a#\\|b|');
    assert(logger, '#\\# then a |symbol|', values('#\\#|a|'), '#\\# |a|');
    assert(logger, '#\\| then #\\#', values('#\\| #\\#'), '#\\| #\\#');
    assert(logger, '#| in a line comment', values('a ; #| not a comment\nb'), 'a b');
    // An identifier ends where a comment starts, as at whitespace
    assert(logger, 'a block comment ends an identifier', values('(a#|c|#b)'), '( a b )');
    // Outside a comment, |# is not special: an identifier may hold it
    assert(logger, '|# in an identifier', values('a|# b'), 'a|# b');

    // Square brackets are tokens of their own, as parentheses are, for the
    // parser to report: R7RS 2.3 reserves them
    assert(logger, 'brackets are tokens', values('(let ([x 1]) x)'), '( let ( [ x 1 ] ) x )');
    assert(logger, 'a bracket ends an identifier', values('a[0]'), 'a [ 0 ]');
    assert(logger, 'brackets in a string, a |symbol| and characters',
        values('"[a]" |[b]| #\\[ #\\]'), '"[a]" |[b]| #\\[ #\\]');
    {
        const tokens = tokenize('(a\n [b])');
        assert(logger, 'a bracket token, line', tokens[2].source.line, 2);
        assert(logger, 'a bracket token, column', tokens[2].source.column, 2);
    }

    // Unterminated: an error, rather than the rest of the input commented out
    assert(logger, 'an unterminated block comment is an error',
        /unterminated block comment/.test(tokenizeError('a #| b')), true);
    assert(logger, 'an unterminated nested block comment is an error',
        /unterminated block comment/.test(tokenizeError('#|#||')), true);
    assert(logger, 'an unterminated block comment in a list is an error',
        /unterminated block comment/.test(tokenizeError('(a #| b #| c |# d)')), true);

    logger.title('tokenize - input ending inside a token');

    /**
     * The error tokenizing a string raises, or null if none.
     * @param {string} input - Source code.
     * @returns {Error|null}
     */
    const errorOf = (input) => {
        try {
            tokenize(input);
            return null;
        } catch (e) {
            return e;
        }
    };

    // A string, a |symbol| or a character the input ends in is an error, and
    // one more input could mend, as an unterminated block comment is
    for (const [input, pattern, description] of [
        ['(a "bc', /unterminated string/, 'a string'],
        ['"a\\"', /unterminated string/, 'a string whose last quote is escaped'],
        ['"a\\', /unterminated string/, 'a string ending in a backslash'],
        ['(a |bc', /unterminated \|symbol\|/, 'a |symbol|'],
        ['|a\\|', /unterminated \|symbol\|/, 'a |symbol| whose last | is escaped'],
        ['|', /unterminated \|symbol\|/, 'a lone |'],
        ['(a #\\', /end of input after #\\/, 'a #\\ with no character'],
        ['a #| b', /unterminated block comment/, 'a block comment'],
    ]) {
        const error = errorOf(input);
        assert(logger, `${description} the input ends in is an error`, pattern.test(error?.message), true);
        assert(logger, `${description} the input ends in is incomplete`, error?.incomplete, true);
    }
    {
        const error = errorOf('(a\n  "bc');
        assert(logger, 'an unterminated string gives the line it starts on', error?.line, 2);
        assert(logger, 'an unterminated string gives the column it starts at', error?.column, 3);
    }
    // Ended, they are no errors
    assert(logger, 'a string ending in an escaped backslash', values('"a\\\\" b'), '"a\\\\" b');
    assert(logger, 'a |symbol| holding an escaped |', values('|a\\|| b'), '|a\\|| b');
    assert(logger, '#\\ then a space is the space character', values('#\\ '), '#\\ ');

    logger.title('tokenize - offsets');

    // Each token records the index in the input where it starts, counting a
    // CR LF as the two characters it is
    {
        const offsets = (input) => tokenize(input).map((t) => t.offset);
        assert(logger, 'token offsets', offsets('(a "b c" #\\( |d e|)'), [0, 1, 3, 9, 13, 18]);
        assert(logger, 'token offsets past comments', offsets('a ; c\n#| d |# b #;c'), [0, 14, 16, 18]);
        assert(logger, 'token offsets past CR LF', offsets('a\r\nb\rc'), [0, 3, 5]);
        assert(logger, 'token offsets of #( and #u8(', offsets('#(1) #u8(2)'), [0, 2, 3, 5, 9, 10]);
    }

    logger.title('tokenize - basic');

    // Basic tokens
    {
        const tokens = tokenize('(+ 1 2)');
        assert(logger, 'tokenizes basic expression', tokens.length, 5);
        assert(logger, 'first token is open paren', tokens[0].value, '(');
        assert(logger, 'second token is plus', tokens[1].value, '+');
        assert(logger, 'last token is close paren', tokens[4].value, ')');
    }

    // hasPrecedingSpace tracking
    {
        const tokens = tokenize('a b');
        assert(logger, 'first token has space (start)', tokens[0].hasPrecedingSpace, true);
        assert(logger, 'second token has space', tokens[1].hasPrecedingSpace, true);
    }

    {
        const tokens = tokenize('a.b');
        assert(logger, 'a.b is single token', tokens.length, 1);
        assert(logger, 'dot notation as single token', tokens[0].value, 'a.b');
    }

    logger.title('tokenize - literals');

    // String tokens
    {
        const tokens = tokenize('"hello world"');
        assert(logger, 'string is single token', tokens.length, 1);
        assert(logger, 'string preserved', tokens[0].value, '"hello world"');
    }

    // Character literals
    {
        const tokens = tokenize('#\\space #\\a');
        assert(logger, 'two character tokens', tokens.length, 2);
        assert(logger, 'named character', tokens[0].value, '#\\space');
        assert(logger, 'single character', tokens[1].value, '#\\a');
    }

    logger.title('tokenize - special forms');

    // Special tokens
    {
        const tokens = tokenize("' ` , ,@");
        assert(logger, 'has quote token', tokens.some(t => t.value === "'"), true);
        assert(logger, 'has quasiquote token', tokens.some(t => t.value === '`'), true);
        assert(logger, 'has unquote token', tokens.some(t => t.value === ','), true);
        assert(logger, 'has unquote-splicing token', tokens.some(t => t.value === ',@'), true);
    }

    // Vector and bytevector starts
    {
        const tokens = tokenize('#() #u8()');
        assert(logger, 'vector start token', tokens.some(t => t.value === '#('), true);
        assert(logger, 'bytevector start token', tokens.some(t => t.value === '#u8('), true);
    }

    // Line comments are skipped
    {
        const tokens = tokenize('a ; this is a comment\nb');
        assert(logger, 'comments not tokenized', tokens.length, 2);
        assert(logger, 'token before comment', tokens[0].value, 'a');
        assert(logger, 'token after comment', tokens[1].value, 'b');
    }

    // A line comment ends at any of R7RS's line endings (2.2): LF, CR LF or CR
    {
        for (const [ending, label] of [['\r\n', 'CR LF'], ['\r', 'CR']]) {
            const tokens = tokenize(`a ; comment${ending}b ; another${ending}c`);
            assert(logger, `a comment ends at ${label}`, tokens.map((t) => t.value).join(' '), 'a b c');
            assert(logger, `the line after a comment ending in ${label} is counted`, tokens[1].source.line, 2);
        }
    }

    // Vertical bar symbols
    {
        const tokens = tokenize('|hello world|');
        assert(logger, 'bar symbol is single token', tokens.length, 1);
        assert(logger, 'bar symbol preserved', tokens[0].value, '|hello world|');
    }
}

export default runTokenizerTests;
