/**
 * Promise Interop Tests
 * 
 * Tests for JavaScript <-> Scheme Promise interoperability.
 * Verifies that:
 * 1. Scheme-created promises work correctly when consumed by JavaScript
 * 2. JavaScript-created promises work correctly when consumed by Scheme
 */

import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { loadLibrary, applyImports, setFileResolver, registerBuiltinLibrary, createPrimitiveExports } from '../../src/core/interpreter/library_loader.js';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * Helper to run Scheme code and return the result.
 */
function runScheme(interpreter, env, code) {
    const forms = parse(code);
    let result;
    for (const form of forms) {
        const ast = analyze(form);
        result = interpreter.run(ast, env);
    }
    return result;
}

/**
 * Promise interop test suite.
 * @param {Object} interpreter - Scheme interpreter
 * @param {Object} logger - Test logger
 */
export async function runPromiseInteropTests(interpreter, logger) {
    logger.title('Promise Interop Tests');

    const env = interpreter.globalEnv;

    // Setup file resolver
    setFileResolver((libraryName) => {
        const parts = libraryName;
        const fileName = parts[parts.length - 1];

        const searchDirs = [
            path.join(__dirname, '../../src/core/scheme'),
            path.join(__dirname, '../../src/extras/scheme')
        ];

        for (const dir of searchDirs) {
            // Check .sld
            let p = path.join(dir, fileName + '.sld');
            if (fs.existsSync(p)) return fs.readFileSync(p, 'utf8');

            // Check .scm
            p = path.join(dir, fileName + '.scm');
            if (fs.existsSync(p)) return fs.readFileSync(p, 'utf8');

            // Check exact
            p = path.join(dir, fileName);
            if (fs.existsSync(p)) return fs.readFileSync(p, 'utf8');
        }

        throw new Error(`Library not found: ${libraryName.join('/')}`);
    });

    // Re-register primitives and load promise library
    const primitiveExports = createPrimitiveExports(env);
    registerBuiltinLibrary(['scheme', 'primitives'], primitiveExports, env);

    await loadLibrary(['scheme', 'base'], analyze, interpreter, env);
    const promiseExports = await loadLibrary(['scheme-js', 'promise'], analyze, interpreter, env);
    applyImports(env, promiseExports, { libraryName: ['scheme-js', 'promise'] });

    // =========================================================================
    // Test 1: Scheme-created Promise consumed by JavaScript
    // =========================================================================
    logger.log('Test: Scheme-created Promise consumed by JavaScript');
    {
        // Create a promise in Scheme
        const schemePromise = runScheme(interpreter, env, `
            (make-js-promise 
                (lambda (resolve reject)
                    (resolve 42)))
        `);

        // Verify it's a real JS Promise
        if (!(schemePromise instanceof Promise)) {
            logger.fail('Scheme-created promise is not a JS Promise');
        } else {
            // Consume it with JavaScript .then()
            const result = await schemePromise;
            if (result === 42) {
                logger.pass('Scheme-created promise resolved correctly in JS (got 42)');
            } else {
                logger.fail(`Scheme-created promise resolved to wrong value: ${result}`);
            }
        }
    }

    // =========================================================================
    // Test 2: Scheme callback with JavaScript Promise
    // =========================================================================
    logger.log('Test: Scheme callback attached to JS Promise');
    {
        // Create a JS promise
        const jsPromise = Promise.resolve(100);

        // Define it in Scheme environment
        env.define('js-test-promise', jsPromise);

        // Attach a Scheme callback
        const resultPromise = runScheme(interpreter, env, `
            (js-promise-then js-test-promise 
                (lambda (x) (* x 2)))
        `);

        // Verify the chained promise works
        const result = await resultPromise;
        if (result === 200) {
            logger.pass('Scheme callback doubled JS Promise value correctly (100 -> 200)');
        } else {
            logger.fail(`Scheme callback returned wrong value: ${result}`);
        }
    }

    // =========================================================================
    // Test 3: Complex chain - JS -> Scheme -> JS -> Scheme
    // =========================================================================
    logger.log('Test: Complex JS <-> Scheme promise chain');
    {
        // Start with JS Promise
        const initial = Promise.resolve(5);
        env.define('chain-start', initial);

        // Chain through Scheme callbacks
        const chained = runScheme(interpreter, env, `
            (js-promise-chain chain-start
                (lambda (x) (js-promise-resolve (* x 2)))    ; 5 -> 10
                (lambda (x) (js-promise-resolve (+ x 3))))   ; 10 -> 13
        `);

        // Consume result in JS
        const result = await chained;
        if (result === 13) {
            logger.pass('Complex chain computed correctly: 5 -> 10 -> 13');
        } else {
            logger.fail(`Complex chain returned wrong value: ${result} (expected 13)`);
        }
    }

    // =========================================================================
    // Test 4: Scheme procedure used as JS Promise executor
    // =========================================================================
    logger.log('Test: Scheme procedure as Promise executor');
    {
        // Define a Scheme procedure that computes something
        runScheme(interpreter, env, `
            (define (compute-async resolve reject)
                (resolve (+ 10 20 30)))
        `);

        // Get the procedure and use it with new Promise()
        const computeAsync = env.lookup('compute-async');
        const wrappedCompute = interpreter.createJsBridge(computeAsync);

        const p = new Promise((resolve, reject) => {
            wrappedCompute(resolve, reject);
        });

        const result = await p;
        if (result === 60) {
            logger.pass('Scheme executor computed correctly: 10+20+30 = 60');
        } else {
            logger.fail(`Scheme executor returned wrong value: ${result}`);
        }
    }

    // =========================================================================
    // Test 5: Scheme promise-all with mixed Scheme/JS promises
    // =========================================================================
    logger.log('Test: promise-all with mixed Scheme/JS promises');
    {
        // Create a JS promise
        const jsP = Promise.resolve('js');
        env.define('mixed-js-promise', jsP);

        // Create mixed array and use js-promise-all
        const result = runScheme(interpreter, env, `
            (js-promise-all (list 
                mixed-js-promise
                (js-promise-resolve 'scheme)
                (js-promise-resolve 42)))
        `);

        const values = await result;
        if (Array.isArray(values) && values[0] === 'js' && values[2] === 42) {
            logger.pass('promise-all with mixed sources worked correctly');
        } else {
            logger.fail(`promise-all returned unexpected: ${JSON.stringify(values)}`);
        }
    }

    // =========================================================================
    // Test 6: Error handling across the boundary
    // =========================================================================
    logger.log('Test: Error handling across JS/Scheme boundary');
    {
        // Create a rejected JS promise
        const rejected = Promise.reject(new Error('test-error'));
        env.define('rejected-promise', rejected);

        // Catch it with Scheme
        const caught = runScheme(interpreter, env, `
            (js-promise-catch rejected-promise
                (lambda (err) 'caught))
        `);

        const result = await caught;
        if (result.name === 'caught') {
            logger.pass('Scheme caught JS rejection correctly');
        } else {
            logger.fail(`Unexpected catch result: ${result}`);
        }
    }
}
