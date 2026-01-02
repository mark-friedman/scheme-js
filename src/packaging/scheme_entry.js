import { createInterpreter } from '../core/interpreter/index.js';
import { parse } from '../core/interpreter/reader.js';
import { analyze } from '../core/interpreter/analyzer.js';
import { list } from '../core/interpreter/cons.js';
import { intern } from '../core/interpreter/symbol.js';

// Create a single shared interpreter and environment instance
const { interpreter, env } = createInterpreter();

function evalCode(code) {
    const asts = parse(code);
    if (asts.length === 0) return undefined;

    let ast;
    if (asts.length === 1) {
        ast = analyze(asts[0]);
    } else {
        ast = analyze(list(intern('begin'), ...asts));
    }

    return interpreter.run(ast);
}

/**
 * Evaluates Scheme code synchronously.
 * @param {string} code - The Scheme source code.
 * @returns {*} The result of the evaluation.
 */
export function schemeEval(code) {
    return evalCode(code);
}

/**
 * Evaluates Scheme code asynchronously.
 * Returns a Promise that resolves to the result.
 * @param {string} code - The Scheme source code.
 * @returns {Promise<*>} A promise resolving to the result.
 */
export function schemeEvalAsync(code) {
    return new Promise((resolve, reject) => {
        try {
            resolve(evalCode(code));
        } catch (e) {
            reject(e);
        }
    });
}

// Export the interpreter and environment for advanced usage (e.g. testing, extending)
export { interpreter, env };
