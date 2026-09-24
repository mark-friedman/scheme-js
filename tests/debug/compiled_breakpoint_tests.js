/**
 * @fileoverview Breakpoints set inside compiled procedures.
 *
 * The debugger's only hook sits in the interpreter's step loop, and compiled
 * procedures never enter it -- so a breakpoint set inside one is accepted and
 * then never fires. These tests pin the three things that make that failure
 * loud instead of silent:
 *
 *  1. a procedure knows the source it came from, including the common
 *     `(define (f x) ...)` form, which used to lose its span entirely;
 *  2. a compiled procedure keeps that span, on every path that produces one;
 *  3. the debugger can ask whether a location is inside compiled code, and the
 *     REPL says so when a breakpoint lands there.
 *
 * Making compiled code actually stop at breakpoints is separate work. This is
 * about the debugger telling the truth in the meantime.
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from '../../src/debug/repl_debug_backend.js';
import { ReplDebugCommands } from '../../src/debug/repl_debug_commands.js';
import {
  tryCompileDefinition, tryCompileClosure, compileEnvironment
} from '../../src/compiler/index.js';
import { interpretedLibrary as standardLibrary, installStandardLibrary } from '../harness/standard_library.js';

/**
 * A definition spread over several lines, so that a span can be seen to cover
 * more than its first line.
 *
 * Lines: 1 `(define (area w h)`, 2 `(let ((a (* w h)))`, 3 `a))`.
 */
const AREA = '(define (area w h)\n  (let ((a (* w h)))\n    a))\n';

/**
 * Evaluates source in a fresh interpreter, attributing it to a filename.
 * @param {string} source - Scheme source.
 * @param {string} filename - The name the reader records in source spans.
 * @returns {{interpreter: Object, env: Object}} The interpreter and its global
 *   environment, after evaluation.
 */
function evaluate(source, filename) {
  const { interpreter, env } = createInterpreter();
  for (const form of parse(source, { filename })) {
    interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
  }
  return { interpreter, env };
}

/**
 * Analyzes the first form of a source text, attributing it to a filename.
 * @param {string} source - Scheme source.
 * @param {string} filename - The name the reader records in source spans.
 * @returns {Object} The analyzed node.
 */
function analyzeFirst(source, filename) {
  return analyze(parse(source, { filename })[0]);
}

/**
 * Loads the standard library interpreted, with each file's name recorded in
 * its source spans, as loading the library records it.
 * @returns {{interpreter: Object, env: Object}} The interpreter and environment.
 */
function interpretedLibrary() {
  return standardLibrary({ filenames: true });
}

/**
 * Wires a debug runtime and REPL command handler to an interpreter.
 * @param {Object} interpreter - The interpreter to debug.
 * @returns {{runtime: SchemeDebugRuntime, commands: ReplDebugCommands}} The pair.
 */
function debuggerFor(interpreter) {
  const runtime = new SchemeDebugRuntime();
  const backend = new ReplDebugBackend(() => { });
  const commands = new ReplDebugCommands(interpreter, runtime, backend);
  runtime.setBackend(backend);
  interpreter.setDebugRuntime(runtime);
  return { runtime, commands };
}

/**
 * Runs the compiled-breakpoint tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runCompiledBreakpointTests(logger) {
  logger.title('Compiled Breakpoints');

  // --- A procedure knows where it came from --------------------------------

  // The `(define (f x) ...)` form is desugared into a lambda built by the
  // analyzer rather than read from source, and that lambda used to carry no
  // span. It is how almost every procedure is written, so without this the
  // debugger could not place most procedures at all.
  {
    const { env } = evaluate(AREA, 'area.scm');
    const source = env.lookup('area').source;
    assert(logger, 'a (define (f x) ...) closure has a source span',
      source !== null && source !== undefined, true);
    assert(logger, 'the span names the file', source && source.filename, 'area.scm');
    assert(logger, 'the span starts at the definition', source && source.line, 1);
    assert(logger, 'the span reaches the last line of the definition',
      source && source.endLine, 3);
  }
  {
    const { env } = evaluate('(define area\n  (lambda (w h)\n    (* w h)))\n', 'area.scm');
    const source = env.lookup('area').source;
    assert(logger, 'an explicit lambda still has its own span',
      source && source.line, 2);
  }

  // The same missing span is what a user saw: the debugger records each
  // frame's location from the procedure being called, so the backtrace said
  // "unknown location" for every procedure written the ordinary way.
  {
    let pauseInfo = null;
    const { interpreter, env } = evaluate(
      '(define (inner x)\n  (* x 2))\n(define (outer y)\n  (+ 1 (inner y)))\n', 'bt.scm');
    const runtime = new SchemeDebugRuntime({ onPause: (info) => { pauseInfo = info; } });
    interpreter.setDebugRuntime(runtime);
    runtime.enable();
    runtime.setBreakpoint('bt.scm', 2);
    interpreter.run(analyze(parse('(outer 5)', { filename: 'call.scm' })[0]), env);
    interpreter.setDebugRuntime(null);

    const frames = pauseInfo ? pauseInfo.stack : [];
    const where = (name) => {
      const frame = frames.find((f) => f.name === name);
      return frame && frame.source ? `${frame.source.filename}:${frame.source.line}` : null;
    };
    assert(logger, 'the breakpoint paused', pauseInfo !== null, true);
    assert(logger, 'the stack locates the caller', where('outer'), 'bt.scm:3');
    assert(logger, 'and the callee', where('inner'), 'bt.scm:1');
  }

  // --- A compiled procedure keeps it, on every path ------------------------

  {
    const ast = analyzeFirst(AREA, 'area.scm');
    const { env } = createInterpreter();
    const result = tryCompileDefinition(ast, env);
    assert(logger, 'the definition compiles', result.compiled, true);
    const source = result.procedure && result.procedure.source;
    assert(logger, 'tryCompileDefinition keeps the span', source && source.filename, 'area.scm');
    assert(logger, 'with its extent', source && source.endLine, 3);
  }
  {
    const { env } = evaluate(AREA, 'area.scm');
    const result = tryCompileClosure(env.lookup('area'), 'area');
    assert(logger, 'the closure compiles', result.compiled, true);
    assert(logger, 'tryCompileClosure keeps the span',
      result.procedure && result.procedure.source && result.procedure.source.line, 1);
  }
  {
    const { env } = evaluate(AREA, 'area.scm');
    compileEnvironment(env);
    const compiled = env.lookup('area');
    assert(logger, 'compileEnvironment compiled it', compiled.$compiled, true);
    assert(logger, 'compileEnvironment keeps the span',
      compiled.source && compiled.source.filename, 'area.scm');
  }
  {
    const { env } = interpretedLibrary();
    installStandardLibrary(env);
    const map = env.lookup('map');
    assert(logger, 'map is installed from the prebuilt table', map.$compiled, true);
    assert(logger, 'installPrebuilt keeps the span of the closure it replaced',
      map.source && map.source.filename, 'list.scm');
  }

  // --- The debugger can ask whether a location is compiled -----------------

  {
    // `area` on lines 1-3, `perimeter` on lines 4-5.
    const forms = parse(AREA + '(define (perimeter w h)\n  (* 2 (+ w h)))\n',
      { filename: 'shapes.scm' });
    const { interpreter, env } = createInterpreter();
    const run = (form) =>
      interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    forms.forEach(run);
    compileEnvironment(env);
    // Put `perimeter` back as an interpreted closure, so one file holds one
    // compiled procedure and one interpreted one. Re-running the same parsed
    // form, rather than re-reading its text, keeps its span on lines 4-5; read
    // on its own it would claim lines 1-2 and overlap `area`.
    run(forms[1]);
    assert(logger, 'setup: area is compiled', env.lookup('area').$compiled, true);
    assert(logger, 'setup: perimeter is interpreted',
      env.lookup('perimeter').$compiled === undefined, true);
    const { runtime } = debuggerFor(interpreter);

    const inside = runtime.compiledProcedureAt('shapes.scm', 2);
    assert(logger, 'a line inside a compiled procedure is reported',
      inside && inside.name, 'area');
    assert(logger, 'so is its first line', runtime.compiledProcedureAt('shapes.scm', 1) !== null, true);
    assert(logger, 'and its last', runtime.compiledProcedureAt('shapes.scm', 3) !== null, true);

    assert(logger, 'a line inside an interpreted procedure is not',
      runtime.compiledProcedureAt('shapes.scm', 5), null);
    assert(logger, 'nor is its first line',
      runtime.compiledProcedureAt('shapes.scm', 4), null);
    assert(logger, 'a line outside any procedure is not',
      runtime.compiledProcedureAt('shapes.scm', 40), null);
    assert(logger, 'the same line in another file is not',
      runtime.compiledProcedureAt('elsewhere.scm', 2), null);

    // Column precision at the edges of the span: `(define` opens at column 1
    // of line 1, and `a))` closes on line 3.
    assert(logger, 'a column inside the first line is reported',
      runtime.compiledProcedureAt('shapes.scm', 1, 5) !== null, true);
    assert(logger, 'a column past the end of the last line is not',
      runtime.compiledProcedureAt('shapes.scm', 3, 40), null);
  }

  // --- The REPL says so -----------------------------------------------------

  {
    const { interpreter, env } = evaluate(AREA, 'area.scm');
    compileEnvironment(env);
    const { runtime, commands } = debuggerFor(interpreter);
    runtime.enable();

    const inCompiled = await commands.execute(':break area.scm 2');
    assert(logger, ':break still sets the breakpoint',
      inCompiled.includes('Breakpoint bp-1 set at area.scm:2'), true);
    assert(logger, ':break warns that it will not fire',
      inCompiled.includes('will not fire'), true);
    assert(logger, 'and names the compiled procedure', inCompiled.includes('area'), true);

    const list = await commands.execute(':breakpoints');
    assert(logger, ':breakpoints marks it as not firing',
      list.includes('will not fire'), true);
  }
  {
    const { interpreter } = evaluate(AREA, 'area.scm');
    const { runtime, commands } = debuggerFor(interpreter);
    runtime.enable();

    const inInterpreted = await commands.execute(':break area.scm 2');
    assert(logger, ':break inside interpreted code does not warn',
      inInterpreted, ';; Breakpoint bp-1 set at area.scm:2');

    // Every breakpoint used to list as "disabled", because the listing read an
    // `enabled` field that nothing ever set.
    const list = await commands.execute(':breakpoints');
    assert(logger, 'an ordinary breakpoint lists as enabled',
      list.includes('(enabled)'), true);
    assert(logger, 'and not as disabled', list.includes('disabled'), false);
  }

  // The status is worked out when asked, not when the breakpoint is set, so a
  // breakpoint placed first and compiled over afterwards is still reported.
  {
    const { interpreter, env } = evaluate(AREA, 'area.scm');
    const { runtime, commands } = debuggerFor(interpreter);
    runtime.enable();
    await commands.execute(':break area.scm 2');
    compileEnvironment(env);

    const list = await commands.execute(':breakpoints');
    assert(logger, 'a breakpoint compiled over after it was set is reported',
      list.includes('will not fire'), true);
  }
}
