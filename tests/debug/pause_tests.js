/**
 * @fileoverview A program's own pause, and what a run leaves the debugger
 * when it ends.
 *
 * The CLI and the REPL page bind `pause` (`pauseProcedure`), which asks the
 * debugger to stop the program before its next step, and says it stopped
 * where the call is: there, in an interpreted procedure, the asynchronous
 * run can wait; beneath compiled code the step is taken again by the run
 * that can, as at a breakpoint; and with debugging off, when no run can
 * wait, it does nothing. A run that ends takes its calls and any step still
 * in progress with it, so the next run neither shows the first's frames nor
 * stops where the step would have. And how a pause ended is said as it ends,
 * before the run goes on, so that a REPL shows it before the next pause.
 * Each is a real run, which only JavaScript can observe; what the debugger
 * records is tested in tests/core/scheme/debugger_tests.scm, and the
 * compiler's leaving a procedure that pauses interpreted in
 * tests/compiler/driver_tests.scm.
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from '../../src/debug/repl_debug_backend.js';
import { ReplDebugCommands } from '../../src/debug/repl_debug_commands.js';
import { pauseProcedure } from '../../src/debug/scheme_debug_runtime.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';

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
 * An interpreter debugged as the CLI's REPL debugs it, `pause` bound as the
 * CLI binds it.
 * @param {{interpreter: Object, env: Object}} [pair] - The interpreter and
 *   its environment; a fresh one by default.
 * @returns {Object} The interpreter, environment, runtime, commands, what the
 *   backend has shown, and `run`, which runs a form of pause.scm
 *   asynchronously and settles with what it returned, written, or
 *   `raised: ` and the message.
 */
function debugged(pair = createInterpreter()) {
  const { interpreter, env } = pair;
  const runtime = new SchemeDebugRuntime();
  const shown = [];
  const backend = new ReplDebugBackend((line) => shown.push(line));
  runtime.setBackend(backend);
  interpreter.setDebugRuntime(runtime);
  const commands = new ReplDebugCommands(interpreter, runtime, backend);
  env.define('pause', pauseProcedure(interpreter));
  const run = (code) => interpreter.runAsync(analyze(parse(code, { filename: 'pause.scm' })[0], env), env,
    { jsAutoConvert: 'raw' }).then((value) => writeString(value), (error) => `raised: ${error.message}`);
  return { interpreter, env, runtime, backend, commands, shown, run };
}

/**
 * Runs the tests of a program's own pause and of the end of a run.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runPauseTests(logger) {
  logger.title('Debugger - a pause the program asks for, and the end of a run');

  // --- With debugging off -----------------------------------------------
  {
    const { interpreter, runtime, backend, shown, run } = debugged();
    try {
      await run('(define (g x)\n  (pause)\n  (* x 10))');
      assert(logger, 'with debugging off, a pause does nothing: the run goes on to its end',
        await run('(g 5)'), '50');
      assert(logger, 'and leaves nothing paused', [runtime.isPaused(), backend.isPaused(), shown.length],
        [false, false, 0]);
    } finally {
      interpreter.setDebugRuntime(null);
    }
  }

  // --- With it on --------------------------------------------------------
  {
    const { interpreter, runtime, commands, shown, run } = debugged();
    runtime.enable();
    try {
      await run('(define (g x)\n  (pause)\n  (* x 10))');
      let pending = run('(g 5)');
      if (!await until(() => runtime.isPaused())) return logger.fail('(g 5) did not pause');
      assert(logger, 'it stops at the step after it, saying why, and where the call is',
        shown[0], '\n;; Paused: manual pause at pause.scm:2');
      assert(logger, 'in the procedure', runtime.getStack().map((frame) => String(frame.name)), ['g']);
      assert(logger, 'where the procedure\'s variables are', await commands.execute('x'), ';; result: 5');
      runtime.resume();
      assert(logger, 'resumed, the run goes on to its end', await pending, '50');

      // A step still in progress when the run ends is not taken into the next.
      pending = run('(g 6)');
      if (!await until(() => runtime.isPaused())) return logger.fail('(g 6) did not pause');
      runtime.stepOut();
      assert(logger, 'a run stepped out of to its end', await pending, '60');
      let paused = false;
      let settled = false;
      pending = run('(+ 1 2)').then((value) => { settled = true; return value; });
      await until(() => (paused = runtime.isPaused()) || settled);
      if (paused) runtime.resume();
      assert(logger, 'the next run does not stop at its first step', [paused, await pending], [false, '3']);

      // Nor are the calls of an aborted run.
      await run('(define (k y)\n  (pause)\n  y)');
      pending = run('(g 7)');
      if (!await until(() => runtime.isPaused())) return logger.fail('(g 7) did not pause');
      runtime.abort();
      await pending;
      pending = run('(k 1)');
      if (!await until(() => runtime.isPaused())) return logger.fail('(k 1) did not pause');
      assert(logger, 'the next pause shows only its own run\'s calls',
        runtime.getStack().map((frame) => String(frame.name)), ['k']);
      runtime.resume();
      await pending;

      // How a pause ended is said as it ends, before the run goes on.
      pending = run('(g 8)');
      if (!await until(() => runtime.isPaused())) return logger.fail('(g 8) did not pause');
      const before = shown.length;
      const answer = await commands.execute(':s');
      await until(() => runtime.isPaused());
      const after = shown.slice(before);
      assert(logger, 'a step command says nothing itself', answer, '');
      assert(logger, 'the backend says how the pause ended, before the pause the step leads to',
        [after[0], after[1]?.startsWith('\n;; Paused: step complete')], [';; Stepping into...', true]);
      runtime.resume();
      assert(logger, 'which then goes on to its end', await pending, '80');
    } finally {
      interpreter.setDebugRuntime(null);
    }
  }

  // --- Beneath compiled code -----------------------------------------------
  // `tick`, which pauses, is interpreted; the compiled `map` of the standard
  // library calls it, in a synchronous run that cannot wait, so the step it
  // stops at is taken again by the run that can, as at a breakpoint.
  {
    const pair = interpretedLibrary();
    installStandardLibrary(pair.env);
    const { interpreter, runtime, run } = debugged(pair);
    runtime.enable();
    const pauses = [];
    runtime.onPause = (info) => {
      pauses.push(`${runtime.getStack().map((frame) => String(frame.name)).join(' ')} at ${info.source?.line}`);
      setTimeout(() => runtime.resume(), 2);
    };
    try {
      assert(logger, 'setup: map is compiled', pair.env.lookup('map').$compiled, true);
      await run('(define (tick x)\n  (pause)\n  (* x 10))');
      assert(logger, 'beneath compiled code, a pause stops each call, in it',
        [await run('(map tick (list 1 2 3))'), pauses.length, pauses.every((stack) => stack.endsWith('tick at 2'))],
        ['(10 20 30)', 3, true]);
    } finally {
      interpreter.setDebugRuntime(null);
    }
  }
}

export default runPauseTests;
