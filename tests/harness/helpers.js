import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { VariableNode, LiteralNode } from '../../src/core/interpreter/ast.js';
import { Environment } from '../../src/core/interpreter/environment.js';
import { createGlobalEnvironment } from '../../src/core/primitives/index.js';
import { Interpreter } from '../../src/core/interpreter/interpreter.js';
import { Cons, list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';

/**
 * Helper to run code from strings.
 * @param {Interpreter} interpreter
 * @param {string} code
 * @param {Object} [options={}] - Options for interpreter.run
 * @returns {*}
 */
export function run(interpreter, code, options = {}) {
    // Parse the code, which may contain multiple expressions
    const asts = parse(code);
    if (asts.length === 0) return undefined;

    let ast;
    if (asts.length === 1) {
        ast = analyze(asts[0]);
    } else {
        // Construct a 'begin' S-exp and analyze it
        // asts is an array of S-expressions (Cons, Symbol, etc.)
        // We need to create a Cons list: (begin ...asts)
        ast = analyze(list(intern('begin'), ...asts));
    }

    return interpreter.run(ast, undefined, [], undefined, options);
}

/**
 * Simple assertion helper
 * @param {object} logger - The logger object.
 * @param {string} description - Test description.
 * @param {*} actual - Actual result.
 * @param {*} expected - Expected result.
 */
export function assert(logger, description, actual, expected) {
    const actualJS = toJS(actual);
    const expectedJS = toJS(expected);

    if (deepEqual(actualJS, expectedJS)) {
        logger.pass(`${description} (Expected: ${safeStringify(expectedJS)}, Got: ${safeStringify(actualJS)})`);
    } else {
        logger.fail(`${description} (Expected: ${safeStringify(expectedJS)}, Got: ${safeStringify(actualJS)})`);
    }
}

/**
 * Skip a test
 * @param {object} logger - The logger object.
 * @param {string} description - Test description.
 * @param {string} reason - Reason for skipping.
 */
export function skip(logger, description, reason) {
    logger.skip(`${description} (Reason: ${reason})`);
}

export function toJS(val) {
    if (val === null) return null;
    if (val instanceof Cons || (val && val.car !== undefined && val.cdr !== undefined)) {
        // Handle Cons (duck typing to be safe against module loading issues)
        const arr = [];
        let curr = val;
        while (curr !== null && (curr instanceof Cons || (curr.car !== undefined && curr.cdr !== undefined))) {
            arr.push(toJS(curr.car));
            curr = curr.cdr;
        }
        // Handle improper list tail
        if (curr !== null) {
            // How to represent improper list in JS array?
            // Maybe just push the tail?
            // But [1, 2, . 3] is not valid JS.
            // Let's push a special marker or just the tail?
            // For tests expecting [1, 2], we assume proper lists.
            // If improper, let's push it.
            arr.push(toJS(curr));
        }
        return arr;
    }
    if (val instanceof Symbol || (val && val.name !== undefined && val.constructor.name === 'Symbol')) {
        return val.name;
    }
    if (Array.isArray(val)) {
        return val.map(toJS);
    }
    if (val && typeof val === 'object' && val.constructor &&
        (val.constructor.name === 'Char' || val.constructor.name === 'Rational' || val.constructor.name === 'Complex')) {
        return val.toString();
    }
    return val;
}

export function deepEqual(a, b) {
    if (a === b) return true;

    // Handle BigInt vs Number comparison
    if (typeof a === 'bigint' && typeof b === 'number') {
        return Number(a) === b;
    }
    if (typeof a === 'number' && typeof b === 'bigint') {
        return a === Number(b);
    }

    if (typeof a !== 'object' || a === null || typeof b !== 'object' || b === null) return false;

    if (Array.isArray(a) && Array.isArray(b)) {
        if (a.length !== b.length) return false;
        return a.every((val, i) => deepEqual(val, b[i]));
    }

    return false; // Objects not supported for now
}

export function safeStringify(obj) {
    try {
        // Custom replacer to handle BigInt
        return JSON.stringify(obj, (key, value) =>
            typeof value === 'bigint' ? value.toString() : value
        );
    } catch (e) {
        return String(obj);
    }
}

/**
 * Creates a test logger for tracking test results.
 * @param {Object|boolean} [options={}] - Options object or boolean for verbosity.
 * @returns {Object} Logger object.
 */
export function createTestLogger(options = {}) {
    let verbose = false;
    if (typeof options === 'boolean') {
        verbose = options;
    } else if (options && typeof options.verbose !== 'undefined') {
        verbose = options.verbose;
    } else if (typeof process !== 'undefined' && process.argv) {
        verbose = process.argv.includes('--verbose') || process.argv.includes('-v');
    }

    let passCount = 0;
    let failCount = 0;
    let skipCount = 0;
    const failures = [];

    return {
        log: (message, type = 'info') => {
            if (verbose) console.log(`[${type.toUpperCase()}] ${message}`);
        },
        title: (message) => console.log(`\n=== ${message} ===`),
        pass: (message) => {
            passCount++;
            if (verbose) {
                console.log(`✅ PASS: ${message}`);
            }
        },
        skip: (message) => {
            skipCount++;
            if (verbose) {
                console.log(`⚠️ SKIP: ${message}`);
            }
        },
        fail: (message) => {
            failCount++;
            failures.push(message);
            console.error(`❌ FAIL: ${message}`);
            if (typeof process !== 'undefined') process.exitCode = 1;
        },
        summary: () => {
            console.log(`\n========================================`);
            console.log(`TEST SUMMARY: ${passCount} passed, ${failCount} failed, ${skipCount} skipped`);
            if (failCount > 0) {
                console.log(`\nFailed tests:`);
                failures.forEach((f, i) => console.log(`  ${i + 1}. ${f}`));
            }
            console.log(`========================================\n`);
            return { passCount, failCount, failures };
        },
        getStats: () => ({ passCount, failCount, skipCount, failures }),
    };
}


import { createInterpreter } from '../../src/core/interpreter/index.js';

export function createTestEnv() {
    return createInterpreter();
}
