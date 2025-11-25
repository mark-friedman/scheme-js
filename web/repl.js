import { Closure, Continuation } from '../src/data/values.js';
import { Variable, Literal } from '../src/syntax/ast.js';
import { parse } from '../src/syntax/reader.js';
import { analyze } from '../src/syntax/analyzer.js';

/**
 * Pretty-prints a Scheme value for the REPL.
 * @param {*} val - The value from the interpreter.
 * @returns {string}
 */
export function prettyPrint(val) {
    if (val instanceof Literal) {
        return prettyPrint(val.value);
    }
    if (val instanceof Closure) {
        return "#<procedure>";
    }
    if (val instanceof Continuation) {
        return "#<continuation>";
    }
    if (val instanceof Variable) {
        return val.name;
    }
    if (val === null) {
        return "'()";
    }
    if (val === true) {
        return "#t";
    }
    if (val === false) {
        return "#f";
    }
    if (typeof val === 'string') {
        // Check if it's a display string or a symbol-like string
        if (val.startsWith('[Native Error:')) return val;
        return `"${val.replace(/"/g, '\\"')}"`; // Show as string
    }
    if (Array.isArray(val)) {
        return `'(${val.map(prettyPrint).join(' ')})`;
    }
    // Numbers
    return `${val}`;
}

/**
 * Sets up the REPL UI event listeners.
 * @param {Interpreter} interpreter
 * @param {Environment} globalEnv
 */
export function setupRepl(interpreter, globalEnv) {
    const replInput = document.getElementById('repl-input');
    const replOutput = document.getElementById('repl-output');
    const replRunBtn = document.getElementById('repl-run-btn');

    function printToRepl(message, type = 'result') {
        const pre = document.createElement('pre');
        pre.className = `repl-${type}`;
        pre.textContent = message;
        replOutput.appendChild(pre);
        replOutput.scrollTop = replOutput.scrollHeight;
    }

    replRunBtn.addEventListener('click', () => {
        const code = replInput.value;
        if (code.trim() === '') return;

        printToRepl(`> ${code}`, 'prompt');

        try {
            // 1. Read: Parse code into S-expressions
            const s_exps = parse(code);

            // 2. Eval: Analyze and run each expression
            let result;
            for (const s_exp of s_exps) {
                const ast = analyze(s_exp);
                result = interpreter.run(ast, globalEnv);
            }

            // 3. Print: Show the *last* result
            printToRepl(prettyPrint(result), 'result');

        } catch (e) {
            console.error("REPL Error:", e);
            printToRepl(`Error: ${e.message}`, 'error');
        }

        replInput.value = ''; // Clear input
    });
}
