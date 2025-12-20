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
        // Handle library name arrays
        if (Array.isArray(relativePath)) {
            const fileName = relativePath[relativePath.length - 1];
            const baseDir = path.resolve(process.cwd(), 'src', 'core', 'scheme');

            // Try as-is first (for includes that already have .scm)
            const asIsPath = path.resolve(baseDir, fileName);
            if (fs.existsSync(asIsPath)) {
                return fs.readFileSync(asIsPath, 'utf8');
            }

            // Try .sld
            const libPath = path.resolve(baseDir, `${fileName}.sld`);
            if (fs.existsSync(libPath)) {
                return fs.readFileSync(libPath, 'utf8');
            }

            // Try .scm
            const scmPath = path.resolve(baseDir, `${fileName}.scm`);
            return fs.readFileSync(scmPath, 'utf8');
        }
        // Run from CWD
        const filePath = path.resolve(process.cwd(), relativePath);
        return fs.readFileSync(filePath, 'utf8');
    };

    const { setFileResolver } = await import('../src/core/interpreter/library_loader.js');
    setFileResolver(nodeFileLoader);

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
