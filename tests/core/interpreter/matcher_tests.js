
import { findMatchingParen } from '../../../src/core/interpreter/expression_utils.js';

export function test(logger) {
    if (!logger) {
        // Fallback if run standalone (though run_all.js should provide logger)
        const { createTestLogger } = require('../../harness/helpers.js');
        logger = createTestLogger();
    }
    logger.title('Testing Matcher');

    testSimpleMatching(logger);
    testNestedMatching(logger);
    testStringSkipping(logger);
    testCommentSkipping(logger);
    testMultiLineMatching(logger);
    testQuoteMatching(logger);
}

function assert(logger, cond, message) {
    if (cond) {
        logger.pass(message);
    } else {
        logger.fail(message);
    }
}

function testSimpleMatching(logger) {
    // Index: 01234
    // Code:  (foo)
    const code = '(foo)';
    const closeParenIndex = 4;
    const match = findMatchingParen(code, closeParenIndex);
    assert(logger, match !== null, 'Should find match for simple paren');
    if (match) {
        assert(logger, match.index === 0, `Expected match at 0, got ${match.index}`);
        assert(logger, match.line === 0, `Expected line 0, got ${match.line}`);
        assert(logger, match.column === 0, `Expected column 0, got ${match.column}`);
    }
}

function testNestedMatching(logger) {
    // Index: 012345678
    // Code:  (a (b) c)
    // Matches:  ^ ^
    // Indices:  3 5
    const code = '(a (b) c)';
    const closeParenIndex = 5;
    const match = findMatchingParen(code, closeParenIndex);
    assert(logger, match !== null, 'Should find match for nested paren');
    if (match) {
        assert(logger, match.index === 3, `Expected match at 3, got ${match.index}`);
    }
}

function testStringSkipping(logger) {
    // Code:  (foo ")")
    // Strings should be ignored
    const code = '(foo ")")';
    const closeParenIndex = 8; // The last )
    const match = findMatchingParen(code, closeParenIndex);
    assert(logger, match !== null, 'Should find match around string');
    if (match) {
        assert(logger, match.index === 0, `Expected match at 0, got ${match.index}`);
    }
}

function testCommentSkipping(logger) {
    // Code:  (foo ;)\n)
    const code = '(foo ;)\n)';
    const closeParenIndex = 8;
    const match = findMatchingParen(code, closeParenIndex);
    assert(logger, match !== null, 'Should find match around comment');
    if (match) {
        assert(logger, match.index === 0, `Expected match at 0, got ${match.index}`);
    }
}

function testMultiLineMatching(logger) {
    // Line 0: (foo
    // Line 1:   bar)
    const code = '(foo\n  bar)';
    const closeParenIndex = 10; // ')' is at index 10 (4 + 1 + 5)

    const match = findMatchingParen(code, closeParenIndex);
    assert(logger, match !== null, 'Should find match for multiline');
    if (match) {
        assert(logger, match.index === 0, `Expected match at 0, got ${match.index}`);
        assert(logger, match.line === 0, `Expected line 0, got ${match.line}`);
        assert(logger, match.column === 0, `Expected column 0, got ${match.column}`);
    }
}

function testQuoteMatching(logger) {
    // Code:  "foo"
    const code = '"foo"';
    const closeQuoteIndex = 4;
    const match = findMatchingParen(code, closeQuoteIndex);
    assert(logger, match !== null, 'Should find match for quote');
    if (match) {
        assert(logger, match.index === 0, `Expected match at 0, got ${match.index}`);
    }
}
