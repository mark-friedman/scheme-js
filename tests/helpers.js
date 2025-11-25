import { parse } from '../src/syntax/reader.js';
import { analyze } from '../src/syntax/analyzer.js';
import { Variable, Literal } from '../src/syntax/ast.js';
import { Environment } from '../src/core/environment.js';
import { createGlobalEnvironment } from '../src/primitives/index.js';
import { Interpreter } from '../src/core/interpreter.js';

/**
 * Helper to run code from strings.
 * @param {Interpreter} interpreter
 * @param {string} code
 * @returns {*}
 */
export function run(interpreter, code) {
    // Parse the code, which may contain multiple expressions
    const asts = parse(code);
    if (asts.length === 0) return undefined;

    let ast;
    if (asts.length === 1) {
        ast = analyze(asts[0]);
    } else {
        // Construct a 'begin' S-exp and analyze it
        // The parser returns AST nodes (Literal/Variable) for atoms, and Arrays for lists.
        // analyze() handles both.
        ast = analyze([new Variable('begin'), ...asts]);
    }

    return interpreter.run(ast);
}

/**
 * Simple assertion helper
 * @param {object} logger - The logger object.
 * @param {string} description - Test description.
 * @param {*} actual - Actual result.
 * @param {*} expected - Expected result.
 */
export function assert(logger, description, actual, expected) {
    // Simple deep equal for arrays
    let isEqual = actual === expected;
    if (Array.isArray(actual) && Array.isArray(expected)) {
        isEqual = actual.length === expected.length &&
            actual.every((val, i) => val === expected[i]);
    }

    if (isEqual) {
        logger.pass(`${description} (Expected: ${expected}, Got: ${actual})`);
    } else {
        logger.fail(`${description} (Expected: ${expected}, Got: ${actual})`);
    }
}

export function createTestLogger() {
    return {
        log: (message, type = 'info') => console.log(`[${type.toUpperCase()}] ${message}`),
        title: (message) => console.log(`\n=== ${message} ===`),
        pass: (message) => console.log(`✅ PASS: ${message}`),
        fail: (message) => {
            console.error(`❌ FAIL: ${message}`);
            if (typeof process !== 'undefined') process.exitCode = 1;
        },
    };
}

export function createTestEnv() {
    const interpreter = new Interpreter();
    const globalEnv = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(globalEnv);
    return { interpreter, globalEnv };
}
