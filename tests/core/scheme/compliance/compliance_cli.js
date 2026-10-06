/**
 * @fileoverview Runs a conformance suite from the command line, for
 * `run_chapter_tests.js` and `run_chibi_tests.js`.
 *
 * Arguments: `--compiled` installs the standard library compiled, from the
 * prebuilt tables, as the browser does; any other argument keeps only the files
 * whose names contain it. `npm test` runs both suites in both configurations
 * (`compliance_tests.js`); this is for running one on its own.
 */

import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { SUITES, loadSuiteSources, runSuite } from './compliance_suite.js';

const projectRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../../../..');

/**
 * Reads a file by its path from the project root.
 * @param {string} relativePath - The path.
 * @returns {Promise<string>} Its text.
 */
function fileLoader(relativePath) {
    return fs.promises.readFile(path.join(projectRoot, relativePath), 'utf-8');
}

/**
 * Prints each test as it runs.
 * @type {Object}
 */
const logger = {
    pass: (msg) => console.log(`✅ PASS: ${msg}`),
    fail: (msg) => console.error(`❌ FAIL: ${msg}`),
    skip: (msg) => console.log(`⏭️ SKIP: ${msg}`),
    title: (title) => console.log(`\n=== ${title} ===`)
};

/**
 * Runs one suite as the command line asks, prints a summary, and sets the
 * exit code if anything failed.
 * @param {string} suiteName - A key of `SUITES`.
 * @returns {Promise<void>}
 */
export async function runFromCommandLine(suiteName) {
    const suite = SUITES[suiteName];
    const args = process.argv.slice(2);
    const compiledLibraries = args.includes('--compiled');
    const filters = args.filter((a) => a !== '--compiled');
    const files = filters.length > 0 ? suite.files.filter((f) => filters.some((a) => f.includes(a))) : suite.files;

    console.log(`=== ${suite.title}, standard library ${compiledLibraries ? 'compiled' : 'interpreted'}: `
        + `${files.length} of ${suite.files.length} files ===`);
    const sources = await loadSuiteSources(suite, fileLoader);
    const { results, installation, fromSource } = runSuite(suite, sources, logger, { compiledLibraries, files });

    let passes = 0, failures = 0, skips = 0;
    const failed = [];
    for (const r of results) {
        passes += r.passes; failures += r.failures; skips += r.skips;
        if (r.error !== undefined) failed.push(`${r.file}: crashed: ${r.error}`);
        else if (r.failures > 0) failed.push(`${r.file}: ${r.failures} failed`);
    }
    if (compiledLibraries) {
        for (const [name, outcome] of installation) {
            console.log(`${name}: ${outcome.restored.length} procedures restored, ${outcome.installed.length} installed`
                + `${outcome.stale ? ', STALE' : ''}`);
            if (outcome.stale) failed.push(`${name}: stale table, left interpreted`);
        }
        for (const name of fromSource) failed.push(`${name}: read from its source, where a page restores it`);
    }
    console.log('\n========================================');
    console.log(`FILES: ${results.length - failed.length} passed, ${failed.length} failed`);
    console.log(`TESTS: ${passes} passed, ${failures} failed, ${skips} skipped`);
    console.log('========================================');
    if (failed.length > 0) {
        console.log('\nFailed:');
        for (const f of failed) console.log(`  ${f}`);
        process.exitCode = 1;
    }
}
