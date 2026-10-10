/**
 * @fileoverview The browser REPL (web/repl.js) while a program is paused.
 *
 * A line typed at the paused REPL is the debugger's: a debug command, or else
 * an expression to evaluate in the selected frame, as the CLI's REPL takes
 * it. These tests mount the REPL on an element of their own, type lines into
 * it and press Enter as a person would, and read what it shows, so they need
 * a DOM and run only in the browser. The rule itself is tested in both, by
 * tests/core/scheme/debugger_tests.scm and tests/unit/repl_debug_commands_tests.js.
 */

import { assert } from '../harness/helpers.js';
import { setupRepl, replTemplate } from '../../web/repl.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { prettyPrint } from '../../src/core/interpreter/printer.js';
import {
  isCompleteExpression, findMatchingDelimiter, delimiterParens
} from '../../src/core/interpreter/expression_utils.js';
import { SchemeDebugRuntime, ReplDebugBackend, ReplDebugCommands } from '../../src/debug/index.js';

/**
 * Settles once a condition holds, checking it between turns of the event
 * loop, or fails after a time.
 * @param {Function} holds - The condition.
 * @param {string} what - What is waited for, to say if it never comes.
 * @param {number} [ms=5000] - How long to wait.
 * @returns {Promise<void>}
 */
async function until(holds, what, ms = 5000) {
  const deadline = Date.now() + ms;
  while (!holds()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
}

/**
 * A browser REPL on an element of its own, not in the document, with a fresh
 * interpreter, wired as web/main.js wires the page's.
 * @returns {{enter: Function, prompt: Function, shown: Function}} Ways to
 *   type a line at it and to read its prompt and what it has shown.
 */
function mountRepl() {
  const { interpreter, env } = createInterpreter();
  const root = document.createElement('div');
  root.innerHTML = replTemplate;
  setupRepl(interpreter, env, root, {
    parse, analyze, prettyPrint, isCompleteExpression, findMatchingDelimiter, delimiterParens,
    ReplDebugBackend, ReplDebugCommands, SchemeDebugRuntime
  });
  const history = root.querySelector('#repl-history');
  const inputArea = root.querySelector('#repl-input-area');
  const currentLine = root.querySelector('#repl-current-line');
  const shown = () => [...history.children].map((entry) => entry.textContent);

  return {
    /**
     * Types a line and presses Enter, and waits until the REPL takes input
     * again: when what it ran ends, or the program pauses.
     * @param {string} line - The line.
     * @returns {Promise<string[]>} What it showed after the line, an entry
     *   at a time.
     */
    async enter(line) {
      const before = history.children.length;
      inputArea.textContent = line;
      inputArea.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true, cancelable: true }));
      await until(() => currentLine.style.display !== 'none', `the REPL to take input after ${line}`);
      // The first entry is the line itself.
      return shown().slice(before + 1);
    },
    /** @returns {string} The prompt. */
    prompt: () => root.querySelector('#repl-prompt-column .repl-prompt').textContent,
    shown,
  };
}

/**
 * Runs the tests of the browser REPL while a program is paused.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runWebReplDebugTests(logger) {
  logger.title('Browser REPL - lines typed while a program is paused');

  const repl = mountRepl();
  assert(logger, 'debugging turned on', (await repl.enter(':debug on'))[0], ';; Debugging enabled');
  await repl.enter('(define (f x) (raise x))');

  const pausing = await repl.enter('(f 5)');
  assert(logger, 'an uncaught exception pauses the program',
    pausing.some((entry) => entry.includes(';; Paused: exception')) ? true : pausing, true);
  assert(logger, 'and the prompt says so', repl.prompt(), 'debug>');

  assert(logger, 'a variable typed alone is evaluated in the frame, as :eval evaluates it',
    await repl.enter('x'), [';; result: 5']);
  assert(logger, 'so is an expression', await repl.enter('(+ x 1)'), [';; result: 6']);
  assert(logger, 'a debug command is still one',
    (await repl.enter(':bt')).some((entry) => /#0 f at/.test(entry)), true);

  // Continued, the exception goes on to the top level, ending the run.
  await repl.enter(':c');
  await until(() => repl.shown().some((entry) => entry.startsWith('Error: ')), 'the run to end');
  assert(logger, 'once the run has ended, the prompt is the usual one', repl.prompt(), '>');
  assert(logger, 'and a line is evaluated as usual', await repl.enter('(+ 1 2)'), ['3']);
}

export default runWebReplDebugTests;
