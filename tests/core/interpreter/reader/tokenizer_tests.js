/**
 * @fileoverview Unit tests for reader/tokenizer.js
 */

import { tokenize, stripBlockComments } from '../../../../src/core/interpreter/reader/tokenizer.js';
import { assert } from '../../../harness/helpers.js';

export function runTokenizerTests(logger) {
    logger.title('stripBlockComments');

    // Simple block comment
    {
        const result = stripBlockComments('(define x #| comment |# 10)');
        assert(logger, 'preserves code before comment', result.includes('define'), true);
        assert(logger, 'preserves code after comment', result.includes('10'), true);
        assert(logger, 'removes comment content', result.includes('comment'), false);
    }

    // Nested block comments
    {
        const result = stripBlockComments('(a #| outer #| inner |# outer |# b)');
        assert(logger, 'preserves before nested', result.includes('a'), true);
        assert(logger, 'preserves after nested', result.includes('b'), true);
        assert(logger, 'removes nested comments', result.includes('inner'), false);
    }

    // No comments
    {
        const result = stripBlockComments('(+ 1 2)');
        assert(logger, 'unchanged without comments', result, '(+ 1 2)');
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

    // Vertical bar symbols
    {
        const tokens = tokenize('|hello world|');
        assert(logger, 'bar symbol is single token', tokens.length, 1);
        assert(logger, 'bar symbol preserved', tokens[0].value, '|hello world|');
    }
}

export default runTokenizerTests;
