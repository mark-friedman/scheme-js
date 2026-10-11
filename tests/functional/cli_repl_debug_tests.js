/**
 * The interactive REPL goes on answering after a program it was debugging
 * pauses and then resumes, or is aborted.
 *
 * `node repl.js` with no arguments reads lines from standard input. While a
 * program is paused there its prompt is `debug>`, and every line is the
 * debugger's; once the run resumes and ends, or is aborted, the prompt is `>`
 * again and a line is evaluated as before. Each step is written once the
 * REPL has answered the one before, and waits for an answer found nowhere
 * earlier in the output. These run `repl.js` in a child process with its
 * input piped, so they are Node-only.
 */

import { assert } from '../harness/helpers.js';
import { runCli, inSteps, outputOf } from '../harness/cli_process.js';

/**
 * Runs the tests of the interactive REPL around a pause.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>} Resolves when the run has finished.
 */
export async function runCliReplDebugTests(logger) {
  if (typeof process === 'undefined') {
    logger.skip('CLI REPL debugging tests (Node.js only)');
    return;
  }

  logger.title('CLI - the REPL after a pause, resumed or aborted');

  // `pause` is the CLI's own primitive; given a reason, the pause message
  // names it, so that each pause can be waited for apart. With debugging off,
  // as it starts, a line is evaluated before the next is read, so the first
  // lines go together.
  const run = await runCli([], {
    onOutput: inSteps([
      {
        after: '',
        write: '(define (g x why) (pause #f #f why) (* x 10))\n'
          + '(define (h x) (pause #f #f "pause in h") (* x 100))\n'
          + '(g 1 "not debugging")\n'
          + ':debug on\n'
      },
      { after: 'Debugging enabled', write: '(g 5 "pause in g")\n' },
      { after: 'Paused: pause in g', write: 'x\n' },
      { after: ';; result: 5', write: ':c\n' },
      { after: '50\n', write: '(+ 1000 1)\n' },
      // The third call of `g`: the tier compiles a procedure at its second,
      // but leaves one that pauses interpreted.
      { after: '1001\n', write: '(g 6 "third call")\n' },
      { after: 'Paused: third call', write: 'x\n' },
      { after: ';; result: 6', write: ':s\n' },
      { after: 'Paused: step complete', write: ':c\n' },
      { after: '60\n', write: '(h 7)\n' },
      { after: 'Paused: pause in h', write: ':abort\n' },
      { after: 'SchemeError: Evaluation aborted', write: '(+ 2000 2)\n' },
      { after: '2002\n', write: ':abort\n' },
      { after: ';; Not paused', write: '(+ 3000 3)\n' },
    ])
  });

  const { stdout } = run;
  const answered = (text) => stdout.includes(text) ? true : outputOf(run);
  assert(logger, 'with debugging off, a pause does nothing: the call returns, the prompt the usual one',
    !stdout.includes('not debugging') && /> 10\n> /.test(stdout) ? true : outputOf(run), true);
  assert(logger, 'paused, the prompt says so', answered('debug> '), true);
  assert(logger, 'and the pause says where the call to pause is', answered('Paused: pause in g at <unknown>:1\n'), true);
  assert(logger, 'and a line is evaluated in the paused frame', answered(';; result: 5'), true);
  // The command that lets the run go on says so, and the run's result or
  // error follows on lines of its own, then the usual prompt.
  assert(logger, 'continued, the run goes on to its end, its result after the command\'s answer',
    answered(';; Continuing...\n50\n> '), true);
  assert(logger, 'after which a line is evaluated as before', answered('1001\n'), true);
  assert(logger, 'a procedure that pauses still pauses inside at its third call', answered(';; result: 6'), true);
  assert(logger, 'a step command answers before the pause it leads to',
    answered(';; Stepping into...\n\n;; Paused: step complete'), true);
  assert(logger, 'aborted, the run stops, its error after the command\'s answer',
    answered(';; Evaluation aborted\nUncaught SchemeError: Evaluation aborted\n> '), true);
  assert(logger, 'after which a line is evaluated as before', answered('2002\n'), true);
  assert(logger, 'with nothing paused, an abort is refused', answered(';; Not paused'), true);
  assert(logger, 'and the REPL still answers', answered('3003\n'), true);
  assert(logger, 'the prompt after the abort is the usual one',
    /2002\n> /.test(stdout) ? true : outputOf(run), true);
}

export default runCliReplDebugTests;
