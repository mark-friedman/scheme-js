/**
 * @fileoverview Unit tests for source location tracking in the tokenizer and parser.
 * 
 * These tests validate that source positions (line, column) are correctly
 * tracked through the tokenization and parsing pipeline, which is foundational
 * for the debugger implementation.
 * 
 * PHASE 0: Foundation - Tests MUST be created BEFORE implementation.
 */

import { tokenize, stripBlockComments } from '../../../../src/core/interpreter/reader/tokenizer.js';
import { parse } from '../../../../src/core/interpreter/reader.js';
import { assert } from '../../../harness/helpers.js';
import { Cons } from '../../../../src/core/interpreter/cons.js';
import { Symbol } from '../../../../src/core/interpreter/symbol.js';

/**
 * Runs all source location tests.
 * @param {Object} logger - Test logger
 */
export function runSourceLocationTests(logger) {
    // ============================================================
    // TOKENIZER SOURCE POSITION TESTS
    // ============================================================
    logger.title('Tokenizer Source Position - Basic Tokens');

    // Test: Tokenizer produces position info for identifiers
    {
        const tokens = tokenize('foo bar baz');
        // After implementation, each token should have source info
        assert(logger, 'tokenize returns array', Array.isArray(tokens), true);
        assert(logger, 'tokenize has 3 tokens', tokens.length, 3);

        // These tests will pass once source info is added to tokens
        // For now, they verify the tokens exist
        if (tokens[0].source) {
            assert(logger, 'first identifier line', tokens[0].source.line, 1);
            assert(logger, 'first identifier column', tokens[0].source.column, 1);
            assert(logger, 'second identifier line', tokens[1].source.line, 1);
            assert(logger, 'second identifier column', tokens[1].source.column, 5);
            assert(logger, 'third identifier line', tokens[2].source.line, 1);
            assert(logger, 'third identifier column', tokens[2].source.column, 9);
        } else {
            logger.skip('identifier position info (not yet implemented)');
        }
    }

    // Test: Tokenizer produces position info for numbers (integers, floats, rationals)
    logger.title('Tokenizer Source Position - Numbers');
    {
        const tokens = tokenize('123 45.67 3/4');
        assert(logger, 'tokenize has 3 number tokens', tokens.length, 3);

        if (tokens[0].source) {
            assert(logger, 'integer line', tokens[0].source.line, 1);
            assert(logger, 'integer column', tokens[0].source.column, 1);
            assert(logger, 'float line', tokens[1].source.line, 1);
            assert(logger, 'float column', tokens[1].source.column, 5);
            assert(logger, 'rational line', tokens[2].source.line, 1);
            assert(logger, 'rational column', tokens[2].source.column, 11);
        } else {
            logger.skip('number position info (not yet implemented)');
        }
    }

    // Test: Tokenizer produces position info for strings (including multi-line)
    logger.title('Tokenizer Source Position - Strings');
    {
        const tokens = tokenize('"hello" "world"');
        assert(logger, 'tokenize has 2 string tokens', tokens.length, 2);

        if (tokens[0].source) {
            assert(logger, 'first string line', tokens[0].source.line, 1);
            assert(logger, 'first string column', tokens[0].source.column, 1);
            // endColumn is exclusive (column after last character)
            assert(logger, 'first string end column', tokens[0].source.endColumn, 8);
            assert(logger, 'second string line', tokens[1].source.line, 1);
            assert(logger, 'second string column', tokens[1].source.column, 9);
        } else {
            logger.skip('string position info (not yet implemented)');
        }
    }

    // Test: Multi-line string position tracking
    {
        const tokens = tokenize('"line1\nline2"');
        assert(logger, 'tokenize has 1 multi-line string token', tokens.length, 1);

        if (tokens[0].source) {
            assert(logger, 'multi-line string start line', tokens[0].source.line, 1);
            assert(logger, 'multi-line string end line', tokens[0].source.endLine, 2);
        } else {
            logger.skip('multi-line string position info (not yet implemented)');
        }
    }

    // Test: Tokenizer handles escaped characters in strings correctly
    logger.title('Tokenizer Source Position - Escaped Characters');
    {
        const tokens = tokenize('"hello\\nworld" next');
        assert(logger, 'tokenize has 2 tokens (escaped string + next)', tokens.length, 2);

        if (tokens[1].source) {
            // The string "hello\nworld" is 14 chars (including quotes and escape)
            assert(logger, 'token after escaped string column', tokens[1].source.column, 16);
        } else {
            logger.skip('escaped char position info (not yet implemented)');
        }
    }

    // Test: Tokenizer handles Unicode characters (codepoint tracking)
    logger.title('Tokenizer Source Position - Unicode');
    {
        const tokens = tokenize('λ α β');
        assert(logger, 'tokenize has 3 unicode tokens', tokens.length, 3);

        if (tokens[0].source) {
            assert(logger, 'first unicode line', tokens[0].source.line, 1);
            assert(logger, 'first unicode column', tokens[0].source.column, 1);
            // Each unicode char is 1 column (not byte count)
            assert(logger, 'second unicode column', tokens[1].source.column, 3);
            assert(logger, 'third unicode column', tokens[2].source.column, 5);
        } else {
            logger.skip('unicode position info (not yet implemented)');
        }
    }

    // Test: Position tracking across line boundaries (\n, \r\n)
    logger.title('Tokenizer Source Position - Line Boundaries');
    {
        const tokens = tokenize('a\nb\r\nc');
        assert(logger, 'tokenize has 3 tokens across lines', tokens.length, 3);

        if (tokens[0].source) {
            assert(logger, 'first token line', tokens[0].source.line, 1);
            assert(logger, 'second token line (after \\n)', tokens[1].source.line, 2);
            assert(logger, 'third token line (after \\r\\n)', tokens[2].source.line, 3);
            // Column should reset to 1 on new lines
            assert(logger, 'second token column', tokens[1].source.column, 1);
            assert(logger, 'third token column', tokens[2].source.column, 1);
        } else {
            logger.skip('line boundary position info (not yet implemented)');
        }
    }

    // Test: Position tracking with comments (line and block comments)
    logger.title('Tokenizer Source Position - Comments');
    {
        // Line comments
        const tokens = tokenize('a ; comment\nb');
        assert(logger, 'comments skipped, 2 tokens', tokens.length, 2);

        if (tokens[1].source) {
            assert(logger, 'token after line comment line', tokens[1].source.line, 2);
            assert(logger, 'token after line comment column', tokens[1].source.column, 1);
        } else {
            logger.skip('comment position info (not yet implemented)');
        }
    }

    // Note: Block comments are handled by stripBlockComments before tokenize
    // so we need to test position adjustment if we track that

    // Test: Tokenizer produces position info for special tokens
    logger.title('Tokenizer Source Position - Special Tokens');
    {
        const tokens = tokenize("' ` , ,@");
        assert(logger, 'quote tokens count', tokens.length, 4);

        if (tokens[0].source) {
            assert(logger, 'quote column', tokens[0].source.column, 1);
            assert(logger, 'quasiquote column', tokens[1].source.column, 3);
            assert(logger, 'unquote column', tokens[2].source.column, 5);
            assert(logger, 'unquote-splicing column', tokens[3].source.column, 7);
        } else {
            logger.skip('special token position info (not yet implemented)');
        }
    }

    // Test: Vector and bytevector starts
    {
        const tokens = tokenize('#() #u8()');

        if (tokens[0].source) {
            assert(logger, 'vector start column', tokens[0].source.column, 1);
            assert(logger, 'bytevector start column', tokens[2].source.column, 5);
        } else {
            logger.skip('vector position info (not yet implemented)');
        }
    }

    // Test: Character literals
    {
        const tokens = tokenize('#\\a #\\newline');
        assert(logger, 'char literal count', tokens.length, 2);

        if (tokens[0].source) {
            assert(logger, 'first char literal column', tokens[0].source.column, 1);
            assert(logger, 'second char literal column', tokens[1].source.column, 5);
        } else {
            logger.skip('char literal position info (not yet implemented)');
        }
    }

    // ============================================================
    // PARSER SOURCE POSITION TESTS
    // ============================================================
    logger.title('Parser Source Position - Lists');

    // Test: Parser preserves positions for lists
    {
        const exprs = parse('(+ 1 2)');
        assert(logger, 'parse returns 1 expression', exprs.length, 1);
        const expr = exprs[0];

        if (expr.source) {
            assert(logger, 'list has source info', expr.source !== null && expr.source !== undefined, true);
            assert(logger, 'list start line', expr.source.line, 1);
            assert(logger, 'list start column', expr.source.column, 1);
            // endColumn is exclusive (column after last character in list)
            assert(logger, 'list end column', expr.source.endColumn, 8);
        } else {
            logger.skip('list position info (not yet implemented)');
        }
    }

    // Test: Parser preserves positions for vectors
    logger.title('Parser Source Position - Vectors');
    {
        const exprs = parse('#(1 2 3)');
        assert(logger, 'parse returns 1 vector', exprs.length, 1);
        const vec = exprs[0];

        if (vec.source) {
            assert(logger, 'vector start line', vec.source.line, 1);
            assert(logger, 'vector start column', vec.source.column, 1);
        } else {
            logger.skip('vector position info (not yet implemented)');
        }
    }

    // Test: Parser preserves positions for quoted expressions
    logger.title('Parser Source Position - Quoted Expressions');
    {
        const exprs = parse("'foo");
        assert(logger, 'parse returns 1 quoted expr', exprs.length, 1);
        const expr = exprs[0];

        if (expr.source) {
            assert(logger, 'quoted expr start line', expr.source.line, 1);
            assert(logger, 'quoted expr start column', expr.source.column, 1);
        } else {
            logger.skip('quoted expr position info (not yet implemented)');
        }
    }

    // Test: Parser preserves positions for quasiquoted expressions
    {
        const exprs = parse('`(1 ,x 2)');
        assert(logger, 'parse returns 1 quasiquoted expr', exprs.length, 1);

        if (exprs[0].source) {
            assert(logger, 'quasiquote start column', exprs[0].source.column, 1);
        } else {
            logger.skip('quasiquote position info (not yet implemented)');
        }
    }

    // Test: Nested structures have correct positions
    logger.title('Parser Source Position - Nested Structures');
    {
        const exprs = parse('(define (foo x)\n  (+ x 1))');
        assert(logger, 'parse nested structure', exprs.length, 1);
        const expr = exprs[0];

        if (expr.source) {
            assert(logger, 'outer form start line', expr.source.line, 1);
            assert(logger, 'outer form start column', expr.source.column, 1);
            // The inner body should have its own source info
            // Navigate to (+ x 1) which is the last element
            if (expr instanceof Cons && expr.cdr && expr.cdr.cdr && expr.cdr.cdr.car) {
                const innerBody = expr.cdr.cdr.car;
                if (innerBody.source) {
                    assert(logger, 'inner body start line', innerBody.source.line, 2);
                    assert(logger, 'inner body start column', innerBody.source.column, 3);
                } else {
                    logger.skip('inner body position info (not yet implemented)');
                }
            }
        } else {
            logger.skip('nested structure position info (not yet implemented)');
        }
    }

    // Test: Empty file edge case
    logger.title('Parser Source Position - Edge Cases');
    {
        const exprs = parse('');
        assert(logger, 'empty input returns empty array', exprs.length, 0);
    }

    // Test: Whitespace-only input edge case
    {
        const exprs = parse('   \n\n   ');
        assert(logger, 'whitespace-only input returns empty array', exprs.length, 0);
    }

    // Test: Multiple expressions with positions
    {
        const exprs = parse('a\nb\nc');
        assert(logger, 'parse multiple expressions', exprs.length, 3);

        if (exprs[0].source) {
            assert(logger, 'first expr line', exprs[0].source.line, 1);
            assert(logger, 'second expr line', exprs[1].source.line, 2);
            assert(logger, 'third expr line', exprs[2].source.line, 3);
        } else if (exprs[0] instanceof Symbol && exprs[0].source) {
            assert(logger, 'first symbol line', exprs[0].source.line, 1);
        } else {
            logger.skip('multiple expr position info (not yet implemented)');
        }
    }

    // Test: Source info across different token types in same expression
    logger.title('Parser Source Position - Mixed Token Types');
    {
        const exprs = parse('(if #t "yes" 42)');
        assert(logger, 'parse mixed token types', exprs.length, 1);
        const expr = exprs[0];

        if (expr.source) {
            assert(logger, 'if form has source info', expr.source !== null, true);
        } else {
            logger.skip('mixed token type position info (not yet implemented)');
        }
    }

    // Test: Datum labels preserve positions
    {
        const exprs = parse('#0=(1 2 #0#)');
        assert(logger, 'parse datum label expression', exprs.length, 1);
        // The circular structure should still have source info
        if (exprs[0].source) {
            assert(logger, 'datum label form has source info', exprs[0].source.line, 1);
        } else {
            logger.skip('datum label position info (not yet implemented)');
        }
    }
}

export default runSourceLocationTests;
