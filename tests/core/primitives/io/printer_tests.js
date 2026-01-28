import { displayString, writeString, writeStringShared } from '../../../../src/core/primitives/io/printer.js';
import { Cons } from '../../../../src/core/interpreter/cons.js';
import { intern } from '../../../../src/core/interpreter/symbol.js';
import { Rational } from '../../../../src/core/primitives/rational.js';
import { Complex } from '../../../../src/core/primitives/complex.js';
import { Char } from '../../../../src/core/primitives/char_class.js';
import { assert } from '../../../harness/helpers.js';

export function runPrinterTests(logger) {
    logger.title("Printer (Display/Write)");

    // Basics & Numbers
    assert(logger, "display null", displayString(null), "()");
    assert(logger, "write null", writeString(null), "()");
    assert(logger, "display true", displayString(true), "#t");
    assert(logger, "write true", writeString(true), "#t");

    assert(logger, "display inexact number", displayString(42), "42.0");
    assert(logger, "write inexact number", writeString(42), "42.0");
    assert(logger, "display exact integer", displayString(42n), "42");
    assert(logger, "write exact integer", writeString(42n), "42");

    assert(logger, "display +inf.0", displayString(Infinity), "+inf.0");
    assert(logger, "write +inf.0", writeString(Infinity), "+inf.0");
    assert(logger, "display +nan.0", displayString(NaN), "+nan.0");
    assert(logger, "write +nan.0", writeString(NaN), "+nan.0");

    // Rationals
    assert(logger, "display rational", displayString(new Rational(1n, 2n)), "1/2");
    assert(logger, "write rational", writeString(new Rational(1n, 2n)), "1/2");

    // Complex
    assert(logger, "display complex (inexact)", displayString(new Complex(1, 2)), "1.0+2.0i");
    assert(logger, "write complex (inexact)", writeString(new Complex(1, 2)), "1.0+2.0i");
    assert(logger, "display complex (exact)", displayString(new Complex(1n, 2n)), "1+2i");
    assert(logger, "write complex (exact)", writeString(new Complex(1n, 2n)), "1+2i");

    // Strings
    assert(logger, "display string", displayString("hello world"), "hello world");
    assert(logger, "write string", writeString("hello world"), '"hello world"');
    assert(logger, "write string escaped", writeString('a "b" \\ c\n'), '"a \\"b\\" \\\\ c\\n"');

    // Symbols
    const sym = intern("foo");
    assert(logger, "display symbol", displayString(sym), "foo");
    assert(logger, "write symbol", writeString(sym), "foo");

    const oddSym = intern("foo bar");
    assert(logger, "display symbol with space", displayString(oddSym), "foo bar");
    assert(logger, "write symbol escaped", writeString(oddSym), "|foo bar|");

    const numSym = intern("123");
    assert(logger, "write numeric-looking symbol", writeString(numSym), "|123|");

    // Characters
    const spaceChar = new Char(' '.codePointAt(0));
    assert(logger, "display space char", displayString(spaceChar), " ");
    assert(logger, "write space char", writeString(spaceChar), "#\\space");

    const aChar = new Char('a'.codePointAt(0));
    assert(logger, "display 'a' char", displayString(aChar), "a");
    assert(logger, "write 'a' char", writeString(aChar), "#\\a");

    // Lists
    const list = new Cons(1, new Cons(2n, null));
    assert(logger, "display list", displayString(list), "(1.0 2)");
    assert(logger, "write list", writeString(list), "(1.0 2)");

    const improper = new Cons(1, 2);
    assert(logger, "display improper", displayString(improper), "(1.0 . 2.0)");
    assert(logger, "write improper", writeString(improper), "(1.0 . 2.0)");

    // Vectors
    const vec = [1, 2n, "3"];
    assert(logger, "display vector", displayString(vec), "#(1.0 2 3)");
    assert(logger, "write vector", writeString(vec), '#(1.0 2 "3")');

    // Bytevectors
    const bv = new Uint8Array([10, 20]);
    assert(logger, "display bytevector", displayString(bv), "#u8(10 20)");
    assert(logger, "write bytevector", writeString(bv), "#u8(10 20)");

    // Shared Structure (writeShared)
    logger.title("Shared Structure Detection");
    const cell = new Cons(1, null);
    cell.cdr = cell; // cycle
    assert(logger, "write shared cycle", writeStringShared(cell), "#0=(1.0 . #0#)");

    const shared = new Cons(100n, null);
    const dag = new Cons(shared, new Cons(shared, null));
    assert(logger, "write shared dag", writeStringShared(dag), "(#0=(100) #0#)");

    // Objects
    logger.title("Object Printing");
    const simpleObj = { a: 1, b: 2n };
    assert(logger, "display simple object", displayString(simpleObj), "#{(a 1.0) (b 2)}");
    assert(logger, "write simple object", writeString(simpleObj), "#{(a 1.0) (b 2)}");

    const objWithString = { name: "alice" };
    assert(logger, "display object with string", displayString(objWithString), '#{(name alice)}');
    assert(logger, "write object with string", writeString(objWithString), '#{(name "alice")}');

    const specialKeyObj = { "foo bar": 1 };
    assert(logger, "write object with special key", writeString(specialKeyObj), '#{("foo bar" 1.0)}');

    const circObj = { name: "root" };
    circObj.self = circObj;
    assert(logger, "write shared circular object", writeStringShared(circObj), '#0=#{(name "root") (self #0#)}');
}

// Allow direct execution
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { createTestLogger } = await import('../../../harness/helpers.js');
    const logger = createTestLogger();
    runPrinterTests(logger);
    logger.summary();
}
