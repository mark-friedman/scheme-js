/**
 * The interactive REPL reads an expression over as many lines as it takes.
 *
 * `node repl.js` with no arguments reads expressions from standard input. A
 * line that ends inside a datum -- a list not closed, a string or block
 * comment not ended -- is continued by the next line rather than reported as
 * an error; one with an error more input cannot mend is reported at once.
 * Each test runs `repl.js` in a child process with input piped into it, so
 * these are Node-only. Each expression is written whole, its lines together,
 * once the REPL has answered the one before, and gives a result found nowhere
 * earlier in the output, so that the next step can wait for it.
 */

import { assert } from '../harness/helpers.js';
import { runCli, inSteps, outputOf } from '../harness/cli_process.js';

/**
 * Runs the tests of the interactive REPL's input.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>} Resolves when every run has finished.
 */
export async function runCliReplInputTests(logger) {
  if (typeof process === 'undefined') {
    logger.skip('CLI REPL input tests (Node.js only)');
    return;
  }

  logger.title('CLI - the REPL continues an expression over lines');

  const run = await runCli([], {
    onOutput: inSteps([
      { after: '', write: '(+ 1000\n1)\n' },
      { after: '1001\n', write: '(string-length "ab\ncde")\n' },
      { after: '6\n', write: '#| a comment\nover two lines |# (* 6 7)\n' },
      // Characters and |symbols| holding a parenthesis, a double quote or a
      // bar are complete on one line
      { after: '42\n', write: '(+ 100 (length (list #\\( #\\" #\\| \'|a)#|)))\n' },
      // An error more input cannot mend is reported, and the next line is a
      // new expression
      { after: '104\n', write: ')\n' },
      { after: 'unbalanced', write: '(+ 2000 2)\n' },
      // So is a read error the evaluation raises, though its input ended
      // inside a datum: it is not the REPL's input that is incomplete
      { after: '2002\n', write: '(read (open-input-string "(a"))\n' },
      { after: "missing ')'", write: '(+ 3000 3)\n' },
    ])
  });

  const { stdout, stderr } = run;
  const answered = (result) => stdout.includes(`${result}\n`) ? true : outputOf(run);
  assert(logger, 'a list continued on the next line', answered(1001), true);
  assert(logger, 'a string continued on the next line, the line break in it', answered(6), true);
  assert(logger, 'a block comment continued on the next line', answered(42), true);
  assert(logger, '#\\(, #\\", #\\| and a |symbol| holding ) and #| on one line', answered(104), true);
  assert(logger, 'an unbalanced ) is reported at once',
    /unbalanced parentheses/.test(stdout) && answered(2002) === true ? true : outputOf(run), true);
  assert(logger, 'a read error from evaluating is reported, not continued',
    /missing '\)'/.test(stdout) && answered(3003) === true ? true : outputOf(run), true);
  assert(logger, 'no parse error is logged for a line that is continued',
    stderr.includes('Parse error') ? stderr : '', '');
  assert(logger, 'the REPL ends when its input does', run.status, 0);
}

export default runCliReplInputTests;
