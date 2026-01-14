import fs from 'fs';
import path from 'path';
import { FileInputPort, FileOutputPort, fileExists, deleteFile } from '../../../../src/core/primitives/io/file_port.js';
import { EOF_OBJECT } from '../../../../src/core/primitives/io/ports.js';
import { assert } from '../../../harness/helpers.js';

export function runFilePortTests(logger) {
    if (typeof process === 'undefined') {
        logger.skip("FilePort tests (Node.js only)");
        return;
    }

    logger.title("File I/O Ports");

    // Create a temp file path
    const testDir = process.cwd();
    const testFile = path.join(testDir, 'unit_test_file_port.txt');

    // Ensure cleanup
    try {
        if (fs.existsSync(testFile)) fs.unlinkSync(testFile);
    } catch (e) { }

    try {
        // Test FileOutputPort
        {
            const p = new FileOutputPort(testFile);
            p.writeString("Hello\n");
            p.writeChar("W");
            p.writeString("orld");
            p.close();

            assert(logger, "File created", fs.existsSync(testFile), true);
            const content = fs.readFileSync(testFile, 'utf8');
            assert(logger, "File content", content, "Hello\nWorld");
        }

        // Test FileInputPort
        {
            const p = new FileInputPort(testFile);
            assert(logger, "readLine 1", p.readLine(), "Hello");
            assert(logger, "readChar 1", p.readChar(), "W");
            assert(logger, "peekChar 1", p.peekChar(), "o");
            assert(logger, "readChar 2", p.readChar(), "o");
            assert(logger, "readString 3", p.readString(3), "rld");
            assert(logger, "readChar EOF", p.readChar(), EOF_OBJECT);
            p.close();
        }

        // Test Helpers
        {
            assert(logger, "fileExists", fileExists(testFile), true);
            deleteFile(testFile);
            assert(logger, "deleteFile", fileExists(testFile), false);
        }

    } finally {
        // Cleanup
        try {
            if (fs.existsSync(testFile)) fs.unlinkSync(testFile);
        } catch (e) { }
    }
}

// Allow direct execution
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { createTestLogger } = await import('../../../harness/helpers.js');
    const logger = createTestLogger();
    runFilePortTests(logger);
    logger.summary();
}
