import { BytevectorInputPort, BytevectorOutputPort } from '../../../../src/core/primitives/io/bytevector_port.js';
import { EOF_OBJECT } from '../../../../src/core/primitives/io/ports.js';
import { assert } from '../../../harness/helpers.js';

export function runBytevectorPortTests(logger) {
    logger.title("Bytevector Input Port");

    // Test readU8
    {
        const bv = new Uint8Array([10, 20, 30]);
        const p = new BytevectorInputPort(bv);
        assert(logger, "readU8 1", p.readU8(), 10);
        assert(logger, "readU8 2", p.readU8(), 20);
        assert(logger, "readU8 3", p.readU8(), 30);
        assert(logger, "readU8 EOF", p.readU8(), EOF_OBJECT);
    }

    // Test peekU8
    {
        const bv = new Uint8Array([42]);
        const p = new BytevectorInputPort(bv);
        assert(logger, "peekU8 1", p.peekU8(), 42);
        assert(logger, "readU8 after peek", p.readU8(), 42);
    }

    // Test readBytevector
    {
        const bv = new Uint8Array([1, 2, 3, 4, 5]);
        const p = new BytevectorInputPort(bv);

        const chunk1 = p.readBytevector(2);
        assert(logger, "readBytevector 2 length", chunk1.length, 2);
        assert(logger, "readBytevector 2 val[0]", chunk1[0], 1);
        assert(logger, "readBytevector 2 val[1]", chunk1[1], 2);

        const chunk2 = p.readBytevector(2);
        assert(logger, "readBytevector next 2 length", chunk2.length, 2);
        assert(logger, "readBytevector next 2 val[0]", chunk2[0], 3);

        const chunk3 = p.readBytevector(10);
        assert(logger, "readBytevector remainder length", chunk3.length, 1);
        assert(logger, "readBytevector remainder val[0]", chunk3[0], 5);

        assert(logger, "readBytevector EOF", p.readBytevector(1), EOF_OBJECT);
    }

    logger.title("Bytevector Output Port");

    // Test write
    {
        const p = new BytevectorOutputPort();
        p.writeU8(10);
        p.writeU8(20);

        const bv = p.getBytevector();
        assert(logger, "getBytevector length", bv.length, 2);
        assert(logger, "getBytevector[0]", bv[0], 10);
        assert(logger, "getBytevector[1]", bv[1], 20);

        p.writeBytevector(new Uint8Array([30, 40]));
        const bv2 = p.getBytevector();
        assert(logger, "getBytevector appended length", bv2.length, 4);
        assert(logger, "getBytevector[3]", bv2[3], 40);
    }

    // Test invalid write
    {
        const p = new BytevectorOutputPort();
        try {
            p.writeU8(256);
            logger.fail("writeU8(256) should throw");
        } catch (e) {
            assert(logger, "writeU8 overflow error", e.message.includes("byte"), true);
        }
    }
}

// Allow direct execution
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { createTestLogger } = await import('../../../harness/helpers.js');
    const logger = createTestLogger();
    runBytevectorPortTests(logger);
    logger.summary();
}
