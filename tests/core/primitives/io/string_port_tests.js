import { StringInputPort, StringOutputPort } from '../../../../src/core/primitives/io/string_port.js';
import { EOF_OBJECT } from '../../../../src/core/primitives/io/ports.js';
import { assert } from '../../../harness/helpers.js';

export function runStringPortTests(logger) {
    logger.title("String Input Port");

    // Test readChar
    {
        const p = new StringInputPort("abc");
        assert(logger, "readChar 1", p.readChar(), "a");
        assert(logger, "readChar 2", p.readChar(), "b");
        assert(logger, "readChar 3", p.readChar(), "c");
        assert(logger, "readChar EOF", p.readChar(), EOF_OBJECT);
    }

    // Test peekChar
    {
        const p = new StringInputPort("x");
        assert(logger, "peekChar 1", p.peekChar(), "x");
        assert(logger, "readChar after peek", p.readChar(), "x");
    }

    // Test readLine
    {
        const p = new StringInputPort("line1\nline2\r\nline3");
        assert(logger, "readLine 1", p.readLine(), "line1");
        assert(logger, "readLine 2", p.readLine(), "line2");
        assert(logger, "readLine 3", p.readLine(), "line3");
        assert(logger, "readLine EOF", p.readLine(), EOF_OBJECT);
    }

    // Test readString
    {
        const p = new StringInputPort("abcdef");
        assert(logger, "readString 2", p.readString(2), "ab");
        assert(logger, "readString 3", p.readString(3), "cde");
        assert(logger, "readString remainder", p.readString(10), "f");
        assert(logger, "readString EOF", p.readString(1), EOF_OBJECT);
    }

    // Test closing
    {
        const p = new StringInputPort("a");
        p.close();
        try {
            p.readChar();
            logger.fail("readChar on closed port should throw");
        } catch (e) {
            assert(logger, "readChar closed error", e.message.includes("closed"), true);
        }
    }

    logger.title("String Output Port");

    // Test write
    {
        const p = new StringOutputPort();
        p.writeChar("a");
        p.writeString("bc");
        assert(logger, "getString", p.getString(), "abc");
        p.writeString("hello", 1, 3); // "el"
        assert(logger, "getString append", p.getString(), "abcel");
    }

    // Test closing
    {
        const p = new StringOutputPort();
        p.close();
        try {
            p.writeChar("a");
            logger.fail("writeChar on closed port should throw");
        } catch (e) {
            assert(logger, "writeChar closed error", e.message.includes("closed"), true);
        }
    }
}

// Allow direct execution
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { createTestLogger } = await import('../../../harness/helpers.js');
    const logger = createTestLogger();
    runStringPortTests(logger);
    logger.summary();
}
