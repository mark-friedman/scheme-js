/**
 * Chibi R7RS Compliance Test Runner - Node.js
 * 
 * Run with: node tests/core/scheme/compliance/run_chibi_tests.js
 */

import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { createComplianceRunner, sectionFiles } from './chibi_runner_lib.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const projectRoot = path.resolve(__dirname, '../../../..');

function fileLoader(relativePath) {
    const fullPath = path.join(projectRoot, relativePath);
    return fs.promises.readFile(fullPath, 'utf-8');
}

// Simple logger for compliance tests
const logger = {
    pass: (msg) => console.log(`✅ PASS: ${msg}`),
    fail: (msg) => console.error(`❌ FAIL: ${msg}`),
    log: (msg) => console.log(msg)
};

async function main() {
    console.log('=== Running R7RS Compliance Suite ===\n');

    const runner = await createComplianceRunner(fileLoader, logger);

    console.log(`Found ${sectionFiles.length} test sections\n`);

    let passedSections = 0;
    let failedSections = 0;
    const errors = [];

    for (const sectionFile of sectionFiles) {
        console.log(`\n--- ${sectionFile} ---`);
        const result = await runner.runSectionTest(sectionFile);
        if (result.success) {
            console.log(`✅ Section ${sectionFile} completed`);
            passedSections++;
        } else {
            console.log(`❌ Section ${sectionFile}: ${result.error}`);
            failedSections++;
            errors.push({ file: sectionFile, error: result.error });
        }
    }

    console.log('\n========================================');
    console.log(`SECTIONS: ${passedSections} passed, ${failedSections} failed`);
    console.log('========================================');

    if (errors.length > 0) {
        console.log('\nFailed sections:');
        for (const e of errors) {
            console.log(`  ${e.file}: ${e.error}`);
        }
        process.exitCode = 1;
    }
}

main().catch(err => {
    console.error('Fatal error:', err);
    process.exitCode = 1;
});
