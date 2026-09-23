/**
 * @fileoverview Breakpoints set inside `define-macro` transformers.
 *
 * A transformer runs while code is being expanded, and expansion happens
 * inside the analyzer, which is synchronous: it has finished before the
 * interpreter runs a single step of the program being expanded. The debugger
 * can only stop execution between the steps of an asynchronous run, so there
 * is no point inside a transformer where it could wait for the user. A
 * breakpoint there is therefore accepted and never fires. These tests pin the
 * things that make that failure loud and harmless instead of silent or
 * misleading:
 *
 *  1. the debugger can ask whether a location is inside a macro transformer;
 *  2. the REPL says so when a breakpoint lands there, whenever the macro was
 *     defined;
 *  3. such a breakpoint does not disturb expansion or leave the debugger in a
 *     paused state that would then stop the program somewhere else.
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from '../../src/debug/repl_debug_backend.js';
import { ReplDebugCommands } from '../../src/debug/repl_debug_commands.js';

/**
 * A file with a shorthand macro on lines 1-3, an ordinary procedure on lines
 * 4-5, and a macro whose transformer is an explicit lambda, on lines 7-8 of
 * the `define-macro` form that begins on line 6.
 */
const MACROS = [
  '(define-macro (bp-swap! a b)',
  "  (list 'let (list (list 'tmp a))",
  "        (list 'set! a b) (list 'set! b 'tmp)))",
  '(define (bp-plain x)',
  '  (+ x 1))',
  '(define-macro bp-twice',
  '  (lambda (x)',
  "    (list 'begin x x)))",
  ''
].join('\n');

/**
 * Evaluates source in a fresh interpreter, attributing it to a filename.
 * @param {Object} interpreter - The interpreter to evaluate in.
 * @param {Object} env - Its global environment.
 * @param {string} source - Scheme source.
 * @param {string} filename - The name the reader records in source spans.
 */
function evaluateIn(interpreter, env, source, filename) {
  for (const form of parse(source, { filename })) {
    interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
  }
}

/**
 * Wires a debug runtime and REPL command handler to an interpreter.
 * @param {Object} interpreter - The interpreter to debug.
 * @param {Function} [onPause] - Called when the runtime reports a pause.
 * @returns {{runtime: SchemeDebugRuntime, commands: ReplDebugCommands}} The pair.
 */
function debuggerFor(interpreter, onPause = null) {
  const runtime = new SchemeDebugRuntime();
  const backend = new ReplDebugBackend(() => { });
  const commands = new ReplDebugCommands(interpreter, runtime, backend);
  runtime.setBackend(backend);
  if (onPause) {
    runtime.onPause = onPause;
  }
  interpreter.setDebugRuntime(runtime);
  return { runtime, commands };
}

/**
 * Runs the macro-breakpoint tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runMacroBreakpointTests(logger) {
  logger.title('Breakpoints Inside Macro Transformers');

  // --- The debugger can ask -------------------------------------------------

  {
    const { interpreter, env } = createInterpreter();
    evaluateIn(interpreter, env, MACROS, 'macros.scm');
    const { runtime } = debuggerFor(interpreter);

    const inside = runtime.macroTransformerAt('macros.scm', 2);
    assert(logger, 'a line inside a shorthand transformer is reported',
      inside && inside.name, 'bp-swap!');
    assert(logger, 'so is its first line',
      runtime.macroTransformerAt('macros.scm', 1) !== null, true);
    assert(logger, 'and its last', runtime.macroTransformerAt('macros.scm', 3) !== null, true);
    assert(logger, 'a line inside an ordinary procedure is not',
      runtime.macroTransformerAt('macros.scm', 5), null);
    assert(logger, 'a line inside an explicit-lambda transformer is reported',
      runtime.macroTransformerAt('macros.scm', 8)?.name, 'bp-twice');
    assert(logger, 'the explicit form reports the lambda, not the define-macro line',
      runtime.macroTransformerAt('macros.scm', 6), null);
    assert(logger, 'a line outside any transformer is not',
      runtime.macroTransformerAt('macros.scm', 40), null);
    assert(logger, 'the same line in another file is not',
      runtime.macroTransformerAt('elsewhere.scm', 2), null);
    assert(logger, 'a column past the end of the last line is not',
      runtime.macroTransformerAt('macros.scm', 3, 60), null);
  }

  // --- The REPL says so -----------------------------------------------------

  {
    const { interpreter, env } = createInterpreter();
    evaluateIn(interpreter, env, MACROS, 'macros.scm');
    const { runtime, commands } = debuggerFor(interpreter);
    runtime.enable();

    const inTransformer = await commands.execute(':break macros.scm 2');
    assert(logger, ':break still sets the breakpoint',
      inTransformer.includes('Breakpoint bp-1 set at macros.scm:2'), true);
    assert(logger, ':break warns that it will not fire',
      inTransformer.includes('will not fire'), true);
    assert(logger, 'and names the macro', inTransformer.includes("'bp-swap!'"), true);

    const inProcedure = await commands.execute(':break macros.scm 5');
    assert(logger, ':break inside an ordinary procedure does not warn',
      inProcedure, ';; Breakpoint bp-2 set at macros.scm:5');

    const list = await commands.execute(':breakpoints');
    assert(logger, ':breakpoints marks the transformer breakpoint as not firing',
      list.includes("bp-1: macros.scm:2 (enabled -- will not fire: inside macro transformer 'bp-swap!')"),
      true);
    assert(logger, 'and leaves the ordinary one alone',
      list.includes('bp-2: macros.scm:5 (enabled)'), true);
  }

  // The status is worked out when asked, so a breakpoint placed before its
  // macro was defined -- say, restored from a previous session -- is reported.
  {
    const { interpreter, env } = createInterpreter();
    const { runtime, commands } = debuggerFor(interpreter);
    runtime.enable();
    await commands.execute(':break later.scm 2');
    evaluateIn(interpreter, env, MACROS, 'later.scm');

    const list = await commands.execute(':breakpoints');
    assert(logger, 'a breakpoint set before its macro was defined is reported',
      list.includes('will not fire'), true);
  }

  // --- It does no harm ------------------------------------------------------

  // Expansion runs to completion inside the analyzer, so a breakpoint that
  // fired there could not stop anything: it would report a pause, leave the
  // runtime marked as paused, and the program would then stop at its first
  // step instead -- somewhere the report did not name. The breakpoint must
  // simply not fire.
  //
  // The debugger is attached before the macro is defined, as it is when a
  // REPL session with debugging on defines one, so that neither the
  // definition nor the expansion runs without it.
  {
    const { interpreter, env } = createInterpreter();
    const pauses = [];
    const { runtime } = debuggerFor(interpreter, (info) => pauses.push(info));
    runtime.enable();
    runtime.setBreakpoint('harm.scm', 2);
    evaluateIn(interpreter, env, MACROS, 'harm.scm');
    evaluateIn(interpreter, env, '(define bp-p 1)\n(define bp-q 2)\n', 'bindings.scm');

    const use = parse('(begin (bp-swap! bp-p bp-q) (list bp-p bp-q))', { filename: 'use.scm' })[0];
    const ast = analyze(use);
    assert(logger, 'expanding past a transformer breakpoint does not pause',
      pauses.length, 0);
    assert(logger, 'and leaves the debugger running', runtime.isPaused(), false);

    const result = await interpreter.runAsync(ast, env, { jsAutoConvert: 'raw' });
    assert(logger, 'the expanded program runs to completion', result, [2, 1]);
    assert(logger, 'without pausing anywhere else', pauses.length, 0);
  }
}

export default runMacroBreakpointTests;
