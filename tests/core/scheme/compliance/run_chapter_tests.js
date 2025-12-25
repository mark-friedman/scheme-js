/**
 * R7RS Chapter Compliance Test Runner - Node.js
 * 
 * Run with: node tests/core/scheme/compliance/run_chapter_tests.js
 */

import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { createChapterComplianceRunner, chapterFiles } from './chapter_runner_lib.js';

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
    console.log('=== Running R7RS Chapter Compliance Suite ===\n');

    const runner = await createChapterComplianceRunner(fileLoader, logger);

    console.log(`Found ${chapterFiles.length} chapter files\n`);

    let passedChapters = 0;
    let failedChapters = 0;
    const errors = [];

    for (const chapterFile of chapterFiles) {
        console.log(`\n--- ${chapterFile} ---`);
        const result = await runner.runChapterTest(chapterFile);
        if (result.success) {
            console.log(`✅ Chapter ${chapterFile} completed`);
            passedChapters++;
        } else {
            console.log(`❌ Chapter ${chapterFile}: ${result.error}`);
            failedChapters++;
            errors.push({ file: chapterFile, error: result.error });
        }
    }

    console.log('\n========================================');
    console.log(`CHAPTERS: ${passedChapters} passed, ${failedChapters} failed`);
    console.log('========================================');

    if (errors.length > 0) {
        console.log('\nFailed chapters:');
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
