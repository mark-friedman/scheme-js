/**
 * @fileoverview The R7RS conformance suites, in both library configurations.
 *
 * Each suite runs twice: with the standard library interpreted, and with it
 * compiled, installed from the prebuilt tables as every browser page installs
 * it. The second is the configuration users run in a browser, and before these
 * ran inside `npm test` it had never been checked for conformance at all.
 *
 * The compiled run also asserts that the tables were installed. A table built
 * against other sources is stale, and a stale table installs nothing and leaves
 * the library interpreted -- which would make the compiled run pass by testing
 * the interpreter a second time.
 */

import { SUITES, loadSuiteSources, runSuite } from './compliance_suite.js';
import { globalMacroRegistry } from '../../../../src/core/interpreter/macro_registry.js';

/**
 * The configurations each suite runs in.
 * @type {Array<{label: string, compiledLibraries: boolean}>}
 */
const CONFIGURATIONS = [
    { label: 'standard library interpreted', compiledLibraries: false },
    { label: 'standard library compiled', compiledLibraries: true }
];

/**
 * A logger that names the configuration in every line it passes on, so a
 * failure says which configuration it came from.
 * @param {Object} logger - The test logger.
 * @param {string} label - The configuration.
 * @returns {Object} The prefixing logger.
 */
function labelled(logger, label) {
    return {
        pass: (message) => logger.pass(`[${label}] ${message}`),
        fail: (message) => logger.fail(`[${label}] ${message}`),
        skip: (message) => logger.skip(`[${label}] ${message}`),
        title: (title) => logger.title(`${title} -- ${label}`)
    };
}

/**
 * Runs both conformance suites in both configurations.
 * @param {Object} logger - Test logger.
 * @param {Function} loader - Reads a file by its path from the project root.
 * @returns {Promise<void>}
 */
export async function runComplianceTests(logger, loader) {
    const macrosBefore = new Map(globalMacroRegistry.macros);
    for (const suite of Object.values(SUITES)) {
        const sources = await loadSuiteSources(suite, loader);
        for (const { label, compiledLibraries } of CONFIGURATIONS) {
            logger.title(`${suite.title} -- ${label}`);
            const { results, installation } = runSuite(suite, sources, labelled(logger, label), { compiledLibraries });
            for (const result of results) {
                if (result.error !== undefined) logger.fail(`[${label}] ${result.file} crashed: ${result.error}`);
            }
            if (compiledLibraries) {
                const core = installation.get('scheme core');
                if (core && core.installed.length > 0) {
                    logger.pass(`[${label}] the standard library's table installed ${core.installed.length} procedures`);
                } else {
                    logger.fail(`[${label}] the standard library's table installed nothing, so this run tested the interpreter`);
                }
                const stale = [...installation].filter(([, outcome]) => outcome.stale).map(([name]) => name);
                if (stale.length === 0) {
                    logger.pass(`[${label}] no library's table is stale`);
                } else {
                    logger.fail(`[${label}] stale, so left interpreted: ${stale.join(', ')}`);
                }
            }
        }
    }
    // The suites define macros of their own, and the Chibi suite starts from an
    // empty registry; the registry the rest of the tests share is given back.
    const macrosAfter = globalMacroRegistry.macros;
    const unchanged = macrosAfter.size === macrosBefore.size
        && [...macrosBefore].every(([name, macro]) => macrosAfter.get(name) === macro);
    if (unchanged) logger.pass('the conformance suites leave the shared macro registry as they found it');
    else logger.fail('the conformance suites changed the shared macro registry');
}
