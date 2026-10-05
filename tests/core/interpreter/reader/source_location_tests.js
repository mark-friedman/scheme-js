/**
 * @fileoverview The spans `parse` gives the lists and vectors it reads, which
 * the debugger and the source maps place code by: JavaScript asking the
 * reader, `(scheme-js reader)`, through its door. The reader's own tests are
 * Scheme (`read_source_tests.scm`).
 */

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

    // Test: a list holding a block comment ends where its close paren does
    {
        const [expr] = parse('(a #| c |# b) #| d |# (e)');
        assert(logger, 'list holding a block comment, end column', expr.source.endColumn, 14);
        const [, second] = parse('(a #| c |# b) #| d |# (e)');
        assert(logger, 'list after block comments, start column', second.source.column, 23);
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

    // ============================================================
    // FILENAME PROPAGATION
    // ============================================================
    // The debugger matches breakpoints on `source.filename`. If parse() cannot
    // be told which file it is reading, every expression in the system claims
    // to come from '<unknown>' and file-scoped breakpoints can never match.
    logger.title('Source Filename Propagation');

    {
        const exprs = parse('(+ 1 2)', { filename: 'arith.scm' });
        assert(logger, 'parse threads filename through to expressions',
            exprs[0].source.filename, 'arith.scm');
    }

    {
        const exprs = parse('(define (f x)\n  (* x 2))', { filename: 'nested.scm' });
        assert(logger, 'nested forms carry the filename',
            exprs[0].source.filename, 'nested.scm');
        assert(logger, 'line numbers still correct with a filename',
            exprs[0].source.line, 1);
    }

    {
        const exprs = parse('(+ 1 2)');
        assert(logger, 'filename defaults to <unknown> when not supplied',
            exprs[0].source.filename, '<unknown>');
    }
}

export default runSourceLocationTests;
