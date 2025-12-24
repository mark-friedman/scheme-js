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

}
