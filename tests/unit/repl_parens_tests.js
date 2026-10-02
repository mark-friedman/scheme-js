/**
 * @fileoverview Unit tests of the browser REPL's parentheses: how it colours
 * them by depth, and how deep a new line is indented. Both go by the
 * delimiter parentheses the reader finds, so a parenthesis in a string, a
 * |symbol|, a character or a comment is text.
 */

import { renderRainbowParens, nestingDepth } from '../../web/repl.js';
import { delimiterParens } from '../../src/core/interpreter/expression_utils.js';
import { assert } from '../harness/helpers.js';

/**
 * Runs the tests of the browser REPL's parentheses.
 * @param {Object} logger - Test logger.
 */
export function runReplParensTests(logger) {
    logger.title('REPL - colouring parentheses');

    /**
     * A parenthesis as the REPL renders it.
     * @param {string} paren - `(` or `)`.
     * @param {string} cls - Its classes.
     * @returns {string} HTML.
     */
    const span = (paren, cls) => `<span class="${cls}">${paren}</span>`;
    const open = (depth) => span('(', `paren-${depth}`);
    const close = (depth) => span(')', `paren-${depth}`);
    const render = (text, matched) => renderRainbowParens(text, delimiterParens(text), matched);

    // [text, expected HTML, description]
    const renderings = [
        ['(a (b))', `${open(0)}a ${open(1)}b${close(1)}${close(0)}`, 'nested lists by depth'],
        ['#(1 #u8(2))', `#${open(0)}1 #u8${open(1)}2${close(1)}${close(0)}`, 'the ( of #( and #u8('],
        ['(((((((', [0, 1, 2, 3, 4, 5, 0].map(open).join(''), 'the colours cycle'],
        // The parentheses the old scanner miscounted
        ['(list #\\( 1)', `${open(0)}list #\\( 1${close(0)}`, '#\\( is text'],
        ['(f #\\) x)', `${open(0)}f #\\) x${close(0)}`, '#\\) is text'],
        ['(f |a)b| "c(" #| ( |# ; )\n)', `${open(0)}f |a)b| "c(" #| ( |# ; )\n${close(0)}`,
            'a |symbol|, a string and comments are text'],
        ['(a "x\n)" b)', `${open(0)}a "x\n)" b${close(0)}`, 'a string over two lines is text'],
        // An unbalanced ) is marked, and closes nothing
        ['a)', `a${span(')', 'paren-mismatch')}`, 'an unopened )'],
        ['()) (', `${open(0)}${close(0)}${span(')', 'paren-mismatch')} ${open(0)}`, 'a ) too many'],
        // Text the input ends inside: those before it are coloured
        ['(a "b(', `${open(0)}a "b(`, 'an unfinished string'],
        ['(a) #| (', `${open(0)}a${close(0)} #| (`, 'an unfinished block comment'],
        // The rest is escaped
        ['(< a "&")', `${open(0)}&lt; a "&amp;"${close(0)}`, 'HTML escaped'],
    ];
    for (const [text, expected, description] of renderings) {
        assert(logger, `colours: ${description}: ${JSON.stringify(text)}`, render(text), expected);
    }
    assert(logger, 'colours: a matching pair marked',
        render('(a)', new Set([0, 2])), `${span('(', 'paren-0 paren-match')}a${span(')', 'paren-0 paren-match')}`);

    // A history entry is rendered whole and split into lines, so a string or
    // block comment over lines is text on each: the HTML has a line for each
    // of the text's, a parenthesis never spanning two
    {
        const text = '(define s "a\n(b")\n#| c\n) |# (f s)';
        const lines = render(text).split('\n');
        assert(logger, 'colours: as many lines of HTML as of text', lines.length, text.split('\n').length);
        assert(logger, 'colours: a string\'s second line', lines[1], `(b"${close(0)}`);
        assert(logger, 'colours: a block comment\'s second line', lines[3], `) |# ${open(0)}f s${close(0)}`);
    }

    logger.title('REPL - indenting a new line');

    // [text before the cursor, depth, description]
    const depths = [
        ['', 0, 'no text'],
        ['(a (b', 2, 'two lists open'],
        ['(define (f x)\n  (g x)', 1, 'one open after two lines'],
        ['(a #\\( b', 1, '#\\( opens nothing'],
        ['(a |(| "(" #| ( |# ; (\n', 1, 'nor do a |symbol|, a string or comments'],
        ['(a) )', 0, 'an unopened ) closes nothing'],
        [')) (', 1, 'a ) too many, then one open'],
        ['(define (f x)\n  "(', 1, 'one open before an unfinished string'],
    ];
    for (const [text, depth, description] of depths) {
        assert(logger, `depth: ${description}: ${JSON.stringify(text)}`, nestingDepth(delimiterParens(text)), depth);
    }
}

export default runReplParensTests;
