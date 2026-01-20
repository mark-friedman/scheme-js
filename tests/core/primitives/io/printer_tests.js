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

    // Objects
    logger.title("Object Printing");

    // Simple object
    const simpleObj = { a: 1, b: 2 };
    assert(logger, "display simple object", displayString(simpleObj), "#{(a 1) (b 2)}");
    assert(logger, "write simple object", writeString(simpleObj), "#{(a 1) (b 2)}");

    // Empty object
    const emptyObj = {};
    assert(logger, "display empty object", displayString(emptyObj), "#{}");

    // Object with string value
    const objWithString = { name: "alice" };
    assert(logger, "display object with string", displayString(objWithString), '#{(name alice)}');
    assert(logger, "write object with string", writeString(objWithString), '#{(name "alice")}');

    // Nested object
    const nestedObj = { outer: { inner: 1 } };
    assert(logger, "display nested object", displayString(nestedObj), "#{(outer #{(inner 1)})}");

    // Object with special key (needs quoting)
    const specialKeyObj = { "foo bar": 1 };
    assert(logger, "write object with special key", writeString(specialKeyObj), '#{("foo bar" 1)}');

    // Object with numeric-looking key
    const numKeyObj = { "123": 456 };
    assert(logger, "write object with numeric key", writeString(numKeyObj), '#{("123" 456)}');

    // Circular object (write-shared)
    const circObj = { name: "root" };
    circObj.self = circObj;
    assert(logger, "write shared circular object", writeStringShared(circObj), '#0=#{(name "root") (self #0#)}');

    // Object containing shared sub-object
    const innerObj = { x: 1 };
    const objWithShared = { a: innerObj, b: innerObj };
    assert(logger, "write shared nested object", writeStringShared(objWithShared), "#{(a #0=#{(x 1)}) (b #0#)}");
}

// Allow direct execution
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { createTestLogger } = await import('../../../harness/helpers.js');
    const logger = createTestLogger();
    runPrinterTests(logger);
    logger.summary();
}
