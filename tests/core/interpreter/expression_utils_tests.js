/**
 * @fileoverview Unit tests for expression_utils.js: the browser REPL's
 * questions about its input -- whether Enter submits it, and which
 * parenthesis the one at the cursor matches.
 */

import { isCompleteExpression, findMatchingDelimiter } from '../../../src/core/interpreter/expression_utils.js';
import { assert } from '../../harness/helpers.js';

/**
 * Runs the expression_utils tests.
 * @param {Object} logger - Test logger.
 */
export function runExpressionUtilsTests(logger) {
    logger.title('isCompleteExpression - complete data');

    // [input, description]: each is one or more whole data
    const complete = [
        ['(+ 1 2)', 'a list'],
        ['42', 'an atom'],
        ['(define x 1) (display x)', 'two data'],
        ["'a", 'a quoted symbol'],
        ['#(1 2)', 'a vector'],
        ['#u8(1 2)', 'a bytevector'],
        ['(a . b)', 'a pair'],
        ['"a string"', 'a string'],
        ['"a \\" quote"', 'a string holding an escaped quote'],
        ['"a\\\\"', 'a string ending in an escaped backslash'],
        ['; only a comment', 'a line comment'],
        ['#| c |# 1', 'a block comment then a datum'],
        ['(a #| ( " |# b)', 'a block comment holding ( and "'],
        ['(a ; (\n b)', 'a line comment holding ('],
        ['#;a b', 'a datum comment then a datum'],
        // After #\ the next character is the character, whatever it is
        ['(display #\\")', '#\\" opens no string'],
        ['(list #\\( 1)', '#\\( opens no list'],
        ['(list #\\) #\\()', '#\\) closes nothing'],
        ['(f #\\;)', '#\\; starts no comment'],
        ['(f #\\|)', '#\\| starts no |symbol|'],
        ['#\\space', 'a named character'],
        // Inside |...| nothing is special but \|
        ["'|a#|", '#| in a |symbol| opens no comment'],
        ['(f |(|)', '( in a |symbol|'],
        ['(f |a"b|)', '" in a |symbol|'],
        ["'|a\\|#|", 'an escaped | in a |symbol|'],
        // Inside a string #| is text
        ['"#|"', '#| in a string'],
        ['(a "#|x" "y|#" b)', '#| and |# in strings'],
        // Errors more input cannot mend are complete: Enter submits them and
        // the reader reports them
        [')', 'an unbalanced )'],
        ['(a . )', 'a dot with no datum before )'],
        ['(1 . 2 3)', 'two data after a dot'],
        ['#u8(256)', 'a byte out of range'],
        ['#\\nosuchname', 'an unknown character name'],
        ['(let ([x 1]) x)', 'a reserved bracket'],
    ];
    for (const [input, description] of complete) {
        assert(logger, `complete: ${description}: ${input}`, isCompleteExpression(input), true);
    }

    logger.title('isCompleteExpression - incomplete data');

    // [input, description]: each ends inside a datum
    const incomplete = [
        ['(+ 1', 'an open list'],
        ['(define (f x)\n  (g x)', 'an open list over two lines'],
        ['#(1 2', 'an open vector'],
        ['#u8(1', 'an open bytevector'],
        ['#{(a 1)', 'an open object literal'],
        ['#{(a', 'an open object literal entry'],
        ['(display "abc', 'an open string'],
        ['"abc\\"', 'a string whose last quote is escaped'],
        ['"\\"', 'a string holding only an escaped quote'],
        ['"abc\\', 'a string ending in a backslash'],
        ['|abc', 'an open |symbol|'],
        ['|', 'a lone |'],
        ["'|a\\|", 'a |symbol| whose last | is escaped'],
        ['#| abc', 'an open block comment'],
        ['#| a #| b |#', 'an open nested block comment'],
        ['(f #| ) |#', 'an open list after a block comment holding )'],
        ["'", 'a quote'],
        ['`', 'a quasiquote'],
        [',', 'an unquote'],
        [',@', 'an unquote-splicing'],
        ["(a '", 'a quote in a list'],
        ['(a .', 'a dot'],
        ['(a . b', 'a dotted tail'],
        ['#;', 'a datum comment'],
        ['(a #;', 'a datum comment in a list'],
        ['#0=', 'a datum label'],
        ['#\\', 'a #\\ with no character'],
        ['(display #\\', 'a #\\ with no character in a list'],
        // The blind spots of a scanner that knows only strings and comments
        ['(f #\\"', 'an open list after #\\"'],
        ['(f #\\) ', 'an open list after #\\)'],
        ["'(|a)b|", 'an open list after a |symbol| holding )'],
        ['(f "a)"', 'an open list after a string holding )'],
    ];
    for (const [input, description] of incomplete) {
        assert(logger, `incomplete: ${description}: ${input}`, isCompleteExpression(input), false);
    }

    // Nothing to submit
    assert(logger, 'the empty string is not complete', isCompleteExpression(''), false);
    assert(logger, 'whitespace is not complete', isCompleteExpression('  \n '), false);

    logger.title('findMatchingDelimiter');

    // [text, position, expected, description]
    const matches = [
        ['(a (b) c)', 0, 8, 'an outer ( to its )'],
        ['(a (b) c)', 8, 0, 'an outer ) to its ('],
        ['(a (b) c)', 3, 5, 'an inner ( to its )'],
        ['(a (b) c)', 5, 3, 'an inner ) to its ('],
        ['(a\n (b)\n c)', 10, 0, 'over lines'],
        ['(a\r\n b)', 6, 0, 'over a CR LF'],
        ['#(1 2)', 5, 1, 'a vector\'s ) to the ( of #('],
        ['#(1 2)', 1, 5, 'the ( of #( to its )'],
        ['#u8(1)', 5, 3, 'a bytevector\'s ) to the ( of #u8('],
        // Parentheses that are no delimiters are passed over
        ['(list #\\( 1)', 11, 0, 'past #\\('],
        ['(list #\\( 1)', 0, 11, 'past #\\( forward'],
        ['(display #\\")', 12, 0, 'past #\\"'],
        ['(f #\\) x)', 8, 0, 'past #\\)'],
        ['(f "a(" |b)|)', 12, 0, 'past a string and a |symbol|'],
        ['(f "a(" |b)|)', 0, 12, 'past a string and a |symbol| forward'],
        ['(f \'|a#| (g))', 12, 0, 'past a |symbol| holding #|'],
        ['(a #| ( |# b)', 12, 0, 'past a block comment'],
        ['(a #| ( |# b)', 0, 12, 'past a block comment forward'],
        ['(a ; (\n b)', 9, 0, 'past a line comment'],
        // A parenthesis that is not a delimiter has no match
        ['(list #\\( 1)', 8, null, 'the ( of #\\('],
        ['(f "a(")', 5, null, 'a ( in a string'],
        ['(f |a)b|)', 5, null, 'a ) in a |symbol|'],
        ['(a #| ( |# b)', 6, null, 'a ( in a block comment'],
        ['(a ; )\n b)', 5, null, 'a ) in a line comment'],
        // Nor does an unbalanced one, a character that is no parenthesis, or
        // a position outside the text
        ['(a (b)', 0, null, 'an unclosed ('],
        ['a)', 1, null, 'an unopened )'],
        ['(a b)', 1, null, 'a symbol'],
        ['(a b)', 5, null, 'past the end'],
        ['(a b)', -1, null, 'before the start'],
    ];
    for (const [text, position, expected, description] of matches) {
        assert(logger, `match ${description}: ${JSON.stringify(text)} at ${position}`,
            findMatchingDelimiter(text, position), expected);
    }

}

export default runExpressionUtilsTests;
