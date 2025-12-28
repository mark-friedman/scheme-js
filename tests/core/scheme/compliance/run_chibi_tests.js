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

    const args = process.argv.slice(2);
    const filteredSections = args.length > 0
        ? sectionFiles.filter(f => args.some(arg => f.includes(arg)))
        : sectionFiles;

    console.log(`Found ${filteredSections.length} test sections (filtered from ${sectionFiles.length})\n`);

    let passedSections = 0;
    let failedSections = 0;
    const errors = [];

    // Track overall test counts
    let totalPasses = 0;
    let totalFailures = 0;
    let totalSkips = 0;

    for (const sectionFile of filteredSections) {
        console.log(`\n--- ${sectionFile} ---`);

        // Create a fresh runner for each section to prevent macro pollution
        const runner = await createComplianceRunner(fileLoader, logger);
        const result = await runner.runSectionTest(sectionFile);

        // Accumulate test counts
        totalPasses += result.passes;
        totalFailures += result.failures;
        totalSkips += result.skips;

        if (result.success) {
            console.log(`✅ Section ${sectionFile} completed (${result.passes} passed, ${result.failures} failed, ${result.skips} skipped)`);
            passedSections++;
        } else if (result.error) {
            console.log(`❌ Section ${sectionFile} crashed: ${result.error}`);
            failedSections++;
            errors.push({ file: sectionFile, error: result.error });
        } else {
            console.log(`❌ Section ${sectionFile} had failures (${result.passes} passed, ${result.failures} failed, ${result.skips} skipped)`);
            failedSections++;
            errors.push({ file: sectionFile, error: `${result.failures} test failures` });
        }
    }

    console.log('\n========================================');
    console.log(`SECTIONS: ${passedSections} passed, ${failedSections} failed`);
    console.log(`TESTS: ${totalPasses} passed, ${totalFailures} failed, ${totalSkips} skipped`);
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
