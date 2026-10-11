/**
 * @fileoverview An abort ends the paused run it is given, and only that run.
 *
 * `:abort` at a pause releases the run, which stops with "Evaluation aborted"
 * wherever it paused -- at a breakpoint, at an exception a handler will
 * catch, or at one none will. The backend is told the pause ended, so a REPL
 * keeping whether the program is paused does not go on treating every line
 * as the debugger's; and the abort is over once its run has stopped, so the
 * next run gives its result, and one that pauses and is resumed goes on.
 * Each is a real asynchronous run, which only JavaScript can observe; what
 * the debugger records of an abort is tested in tests/core/scheme/debugger_tests.scm.
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from '../../src/debug/repl_debug_backend.js';

/**
 * Waits until a condition holds, checking it between turns of the event
 * loop, for at most five seconds.
 * @param {Function} holds - The condition.
 * @returns {Promise<boolean>} Whether it came to hold.
 */
async function until(holds) {
  const deadline = Date.now() + 5000;
  while (!holds()) {
    if (Date.now() > deadline) return false;
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
  return true;
}

/**
 * Runs the tests of an abort.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runAbortTests(logger) {
  logger.title('Debugger - an abort ends the paused run, and only that run');

  const { interpreter, env } = createInterpreter();
  const runtime = new SchemeDebugRuntime();
  const shown = [];
  const backend = new ReplDebugBackend((line) => shown.push(line));
  runtime.setBackend(backend);
  interpreter.setDebugRuntime(runtime);
  runtime.enable();

  /**
   * Runs a form, attributed to abort.scm, asynchronously as a REPL debugging
   * it runs it.
   * @param {string} code - The form.
   * @returns {Promise<string>} What it returned, as JavaScript writes it, or
   *   `raised: ` and the message of what it raised.
   */
  const run = (code) => interpreter.runAsync(analyze(parse(code, { filename: 'abort.scm' })[0], env), env,
    { jsAutoConvert: 'raw' }).then((value) => String(value), (error) => `raised: ${error.message}`);
  // Each run below is to pause; one that does not is a failure, and leaves
  // the rest untested, since what they test needs a pause.
  const paused = async (what) => {
    const did = await until(() => runtime.isPaused());
    if (!did) logger.fail(`${what} did not pause`);
    return did;
  };

  // Line 2 of abort.scm is inside `g`.
  await run('(define (g x)\n  (+ x 1))');
  await run('(define handled #f)');

  try {
    // --- At a breakpoint ---------------------------------------------------
    let bp = runtime.setBreakpoint('abort.scm', 2);
    let pending = run('(g 5)');
    if (!await paused('(g 5), at a breakpoint')) return;
    runtime.abort();
    assert(logger, 'aborted at a breakpoint, the run stops', await pending, 'raised: Evaluation aborted');
    assert(logger, 'the backend is told the pause ended', backend.isPaused(), false);
    assert(logger, 'and says, as it ends, that it ended by an abort', shown.at(-1), ';; Evaluation aborted');
    runtime.removeBreakpoint(bp);

    // --- The runs after it -------------------------------------------------
    assert(logger, 'the next run gives its result', await run('(+ 1 2)'), '3');
    bp = runtime.setBreakpoint('abort.scm', 2);
    pending = run('(g 10)');
    if (!await paused('(g 10), at a breakpoint')) return;
    runtime.resume();
    assert(logger, 'one that pauses and is resumed goes on to its end', await pending, '11');
    runtime.removeBreakpoint(bp);

    // --- At an exception a handler will catch ------------------------------
    runtime.breakOnCaughtException = true;
    pending = run(`(with-exception-handler (lambda (e) (set! handled #t) 99)
                     (lambda () (+ 1 (raise-continuable 'oops))))`);
    if (!await paused('a raise a handler will catch')) return;
    runtime.abort();
    assert(logger, 'aborted at an exception a handler will catch, the run stops',
      await pending, 'raised: Evaluation aborted');
    assert(logger, 'without calling the handler', await run('handled'), 'false');
    runtime.breakOnCaughtException = false;

    // --- At an exception none will -----------------------------------------
    pending = run("(raise 'oops)");
    if (!await paused('a raise none will')) return;
    runtime.abort();
    assert(logger, 'aborted at an exception no handler will catch, the run stops as aborted',
      await pending, 'raised: Evaluation aborted');
    assert(logger, 'and the run after it gives its result', await run('(* 6 7)'), '42');
  } finally {
    interpreter.setDebugRuntime(null);
  }
}

export default runAbortTests;
