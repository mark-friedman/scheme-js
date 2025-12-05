import { parse } from '../src/layer-1-kernel/reader.js';
import { analyze } from '../src/layer-1-kernel/analyzer.js';
import { Variable, Literal } from '../src/layer-1-kernel/ast.js';
import { Environment } from '../src/layer-1-kernel/environment.js';
import { createGlobalEnvironment } from '../src/layer-1-kernel/primitives/index.js';
import { Interpreter } from '../src/layer-1-kernel/interpreter.js';
import { Cons, list } from '../src/layer-1-kernel/cons.js';
import { intern } from '../src/layer-1-kernel/symbol.js';

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
        // asts is an array of S-expressions (Cons, Symbol, etc.)
        // We need to create a Cons list: (begin ...asts)
        ast = analyze(list(intern('begin'), ...asts));
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
    const actualJS = toJS(actual);
    const expectedJS = toJS(expected);

    if (deepEqual(actualJS, expectedJS)) {
        logger.pass(`${description} (Expected: ${safeStringify(expectedJS)}, Got: ${safeStringify(actualJS)})`);
    } else {
        logger.fail(`${description} (Expected: ${safeStringify(expectedJS)}, Got: ${safeStringify(actualJS)})`);
    }
}

function toJS(val) {
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
    return val;
}

function deepEqual(a, b) {
    if (a === b) return true;
    if (typeof a !== 'object' || a === null || typeof b !== 'object' || b === null) return false;

    if (Array.isArray(a) && Array.isArray(b)) {
        if (a.length !== b.length) return false;
        return a.every((val, i) => deepEqual(val, b[i]));
    }

    return false; // Objects not supported for now
}

function safeStringify(obj) {
    try {
        return JSON.stringify(obj);
    } catch (e) {
        return String(obj);
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
