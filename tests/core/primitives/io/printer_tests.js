import { displayString, writeString, writeStringShared } from '../../../../src/core/primitives/io/printer.js';
import { Cons } from '../../../../src/core/interpreter/cons.js';
import { intern } from '../../../../src/core/interpreter/symbol.js';
import { assert } from '../../../harness/helpers.js';

export function runPrinterTests(logger) {
    logger.title("Printer (Display/Write)");

    // Basics
    assert(logger, "display null", displayString(null), "()");
    assert(logger, "display true", displayString(true), "#t");
    assert(logger, "display number", displayString(42), "42");
    assert(logger, "display string", displayString("hello"), "hello");
    assert(logger, "write string", writeString("hello"), '"hello"');

    // Symbols
    const sym = intern("foo");
    assert(logger, "display symbol", displayString(sym), "foo");
    assert(logger, "write symbol", writeString(sym), "foo");
    const oddSym = intern("foo bar");
    assert(logger, "write symbol escaped", writeString(oddSym), "|foo bar|");

    // Lists
    const list = new Cons(1, new Cons(2, null));
    assert(logger, "display list", displayString(list), "(1 2)");

    // Vectors
    const vec = [1, 2, 3];
    assert(logger, "display vector", displayString(vec), "#(1 2 3)");

    // Bytevectors
    const bv = new Uint8Array([10, 20]);
    // Expecting #u8(10 20) as per R7RS
    // NOTE: If this fails, it means printer.js needs updating
    assert(logger, "display bytevector", displayString(bv), "#u8(10 20)");

    // Shared
    const cell = new Cons(1, null);
    cell.cdr = cell; // cycle
    assert(logger, "write shared cycle", writeStringShared(cell), "#0=(1 . #0#)");

    const shared = new Cons(100, null);
    const dag = new Cons(shared, new Cons(shared, null));
    assert(logger, "write shared dag", writeStringShared(dag), "(#0=(100) #0#)");
}

// Allow direct execution
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { createTestLogger } = await import('../../../harness/helpers.js');
    const logger = createTestLogger();
    runPrinterTests(logger);
    logger.summary();
}
