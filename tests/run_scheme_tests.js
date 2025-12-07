import { createTestEnv, run, createTestLogger } from './helpers.js';
import { fileURLToPath } from 'url';
import path from 'path';
import fs from 'fs';
import { runSchemeTests } from './run_scheme_tests_lib.js';


// CLI Entry point
if (process.argv[1] === fileURLToPath(import.meta.url)) {
    const logger = createTestLogger();
    const { interpreter } = createTestEnv();

    // Mock window for interop tests
    if (typeof global !== 'undefined' && !global.window) {
        global.window = {};
    }

    // Simple Node file loader
    const nodeFileLoader = async (relativePath) => {
        // Run from CWD
        const filePath = path.resolve(process.cwd(), relativePath);
        return fs.readFileSync(filePath, 'utf8');
    };

    const args = process.argv.slice(2);
    if (args.length === 0) {
        console.error("Usage: node run_scheme_tests.js <test_file_path> ...");
        process.exit(1);
    }

    runSchemeTests(interpreter, logger, args, nodeFileLoader)
        .then(() => console.log("All Scheme tests passed"))
        .catch(e => {
            console.error(e);
            process.exit(1);
        });
}
