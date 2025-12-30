import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { prettyPrint } from '../src/core/interpreter/printer.js';

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
