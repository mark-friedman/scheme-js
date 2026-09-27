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
 * A procedure compiled over an interpreted closure -- the standard library's
 * prebuilt code, or `compileEnvironment` -- does stop at breakpoints: while any
 * breakpoint is set, or the program is paused or stepping, it runs as the
 * closure it replaced (`Interpreter.interpretForDebugger`). So the warning is
 * left for code compiled with nothing to go back to, and the last section tests
 * that breakpoints fire -- including inside a callback that compiled code
 * called, which used to be reached and not stop.
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
import { writeString } from '../../src/core/primitives/io/printer.js';
import { loadLibrarySync, applyImports } from '../../src/core/interpreter/library_loader.js';
import { withPrivateLibraries, getLibraryEnv } from '../../src/core/interpreter/library_registry.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { installLibraryTable } from '../../src/compiler/prebuilt.js';
import prebuiltLibraries from '../../src/packaging/compiled_libraries.js';

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
    // `area` compiled from its definition, with no interpreted closure to go
    // back to; `perimeter` interpreted.
    const compiledArea = tryCompileDefinition(analyze(forms[0]), env);
    env.define(compiledArea.name, compiledArea.procedure);
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
    const { interpreter, env } = createInterpreter();
    const compiledArea = tryCompileDefinition(analyzeFirst(AREA, 'area.scm'), env);
    env.define(compiledArea.name, compiledArea.procedure);
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
    const { interpreter, env } = createInterpreter();
    const { runtime, commands } = debuggerFor(interpreter);
    runtime.enable();
    await commands.execute(':break area.scm 2');
    const compiledArea = tryCompileDefinition(analyzeFirst(AREA, 'area.scm'), env);
    env.define(compiledArea.name, compiledArea.procedure);

    const list = await commands.execute(':breakpoints');
    assert(logger, 'a breakpoint compiled over after it was set is reported',
      list.includes('will not fire'), true);
  }
  // Compiled over an interpreted closure, it is not: it runs as that closure
  // while there is a breakpoint.
  {
    const { interpreter, env } = evaluate(AREA, 'area.scm');
    compileEnvironment(env);
    const { runtime, commands } = debuggerFor(interpreter);
    runtime.enable();
    const set = await commands.execute(':break area.scm 2');
    assert(logger, 'a procedure compiled over its closure gets no warning', set.includes('will not fire'), false);
    assert(logger, 'since it runs as the closure while there is a breakpoint',
      env.lookup('area').$compiled === undefined, true);
    await commands.execute(':unbreak bp-1');
    assert(logger, 'and compiled again once there is none', env.lookup('area').$compiled, true);
  }
  // Nor before debugging is turned on, as the command-line REPL starts: the
  // procedure is still compiled when the breakpoint is set, and will run as its
  // closure once debugging is on, which is the only time a breakpoint can fire.
  {
    const { interpreter, env } = evaluate(AREA, 'area.scm');
    compileEnvironment(env);
    const { commands } = debuggerFor(interpreter);
    const set = await commands.execute(':break area.scm 2');
    assert(logger, 'setup: before debugging is on, the procedure is still compiled', env.lookup('area').$compiled, true);
    assert(logger, 'a procedure compiled over its closure gets no warning before debugging is on',
      set.includes('will not fire'), false);
    const list = await commands.execute(':breakpoints');
    assert(logger, 'nor in the listing', list.includes('will not fire'), false);
  }
  {
    const { interpreter, env } = evaluate(AREA, 'area.scm');
    const { runtime, commands } = debuggerFor(interpreter);
    runtime.enable();
    await commands.execute(':break area.scm 2');
    compileEnvironment(env);
    assert(logger, 'compiled while there is a breakpoint, it runs as its closure at once',
      env.lookup('area').$compiled === undefined, true);
  }

  // --- Breakpoints fire in code compiled over interpreted closures ------------

  logger.title('Compiled Breakpoints - Firing Through Compiled Code');
  {
    // The standard library the browser installs, compiled, and a program
    // whose procedure, on line 2, is called back by the compiled `map`. The
    // callback ran in a synchronous nested run of the interpreter, which
    // cannot wait, so the breakpoint was reached on every element and the
    // program stopped only when `map` returned. Now `map` runs as its closure
    // while a breakpoint is set, and the session is the one the interpreted
    // library gives: every pause answered by a resume before the next.
    const session = async (compiledLibrary) => {
      const pair = interpretedLibrary();
      if (compiledLibrary) installStandardLibrary(pair.env);
      const compiledBefore = pair.env.lookup('map').$compiled === true;
      const source = '(define hits 0)\n(define (tick x) (set! hits (+ hits 1)) (* x 10))\n(map tick (list 1 2 3))\n';
      const forms = parse(source, { filename: 'cb.scm' });
      pair.interpreter.run(analyze(forms[0]), pair.env, [], undefined, { jsAutoConvert: 'raw' });
      pair.interpreter.run(analyze(forms[1]), pair.env, [], undefined, { jsAutoConvert: 'raw' });
      const events = [];
      const runtime = new SchemeDebugRuntime({
        onPause: () => {
          events.push(`pause ${pair.env.lookup('hits')}`);
          setTimeout(() => { events.push('resume'); runtime.resume(); }, 2);
        }
      });
      pair.interpreter.setDebugRuntime(runtime);
      runtime.enable();
      const id = runtime.setBreakpoint('cb.scm', 2);
      let result = null;
      try {
        result = await pair.interpreter.runAsync(analyze(forms[2]), pair.env, { stepsPerYield: 1000 });
      } finally {
        runtime.removeBreakpoint(id);
      }
      const compiledAfter = pair.env.lookup('map').$compiled === true;
      pair.interpreter.setDebugRuntime(null);
      return { events: events.join(', '), result: writeString(result), compiledBefore, compiledAfter };
    };
    const reference = await session(false);
    const compiled = await session(true);
    assert(logger, 'setup: the compiled library is compiled', compiled.compiledBefore, true);
    assert(logger, 'setup: the interpreted session pauses and resumes in turn',
      /^(pause \d, resume, )*pause \d, resume$/.test(reference.events) && reference.events.includes('pause 2'), true);
    assert(logger, 'a breakpoint in a callback of the compiled map pauses as with the interpreted library',
      compiled.events, reference.events);
    assert(logger, 'and the program then finishes', compiled.result, '(10 20 30)');
    assert(logger, 'and map is compiled again once no breakpoint is set', compiled.compiledAfter, true);
  }
  {
    // Libraries loaded through the library system, as the browser loads them:
    // `map` lives in `(scheme core)`'s own environment, where the library's
    // other procedures find it, and the program imports it. Both switch.
    const bundled = (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] ?? BUNDLED_SOURCES[name[name.length - 1]];
    const install = (name, env) => {
      if (BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] !== undefined && env) {
        installLibraryTable(prebuiltLibraries, name, env, (file) => BUNDLED_SOURCES[file]);
      }
    };
    const seen = withPrivateLibraries({ resolver: bundled, hook: install }, () => {
      const { interpreter } = createInterpreter();
      const env = interpreter.globalEnv;
      applyImports(env, loadLibrarySync(['scheme', 'base'], analyze, interpreter, env), { libraryName: ['scheme', 'base'] });
      const core = getLibraryEnv(['scheme', 'core']);
      const compiled = () => [core.bindings.get('map').$compiled === true, env.bindings.get('map').$compiled === true];
      const before = compiled();
      const runtime = new SchemeDebugRuntime();
      interpreter.setDebugRuntime(runtime);
      runtime.enable();
      const id = runtime.setBreakpoint('anywhere.scm', 1);
      const during = compiled();
      runtime.removeBreakpoint(id);
      const after = compiled();
      interpreter.setDebugRuntime(null);
      return { before, during, after };
    });
    assert(logger, 'setup: map is compiled in the library and in the program', seen.before.join(' '), 'true true');
    assert(logger, 'with a breakpoint set, both run the closure', seen.during.join(' '), 'false false');
    assert(logger, 'and both are compiled again once there is none', seen.after.join(' '), 'true true');

    // A library imported while a breakpoint is set arrives compiled, by value;
    // the next run switches it too.
    const late = withPrivateLibraries({ resolver: bundled, hook: install }, () => {
      const { interpreter } = createInterpreter();
      const env = interpreter.globalEnv;
      applyImports(env, loadLibrarySync(['scheme', 'base'], analyze, interpreter, env), { libraryName: ['scheme', 'base'] });
      const runtime = new SchemeDebugRuntime();
      interpreter.setDebugRuntime(runtime);
      runtime.enable();
      runtime.setBreakpoint('anywhere.scm', 1);
      applyImports(env, loadLibrarySync(['srfi', '1'], analyze, interpreter, env), { libraryName: ['srfi', '1'] });
      const imported = env.bindings.get('fold').$compiled === true;
      // Its first steps, which is where the switch happens, run now.
      interpreter.runAsync(analyze(parse('1')[0]), env, {});
      const run = env.bindings.get('fold').$compiled === true;
      interpreter.setDebugRuntime(null);
      return { imported, run };
    });
    assert(logger, 'setup: a library imported while a breakpoint is set arrives compiled', late.imported, true);
    assert(logger, 'and runs as its closures from the next run', late.run, false);
  }
  {
    // Paused with no breakpoint set -- on an uncaught error -- the program may
    // be stepped from there, so compiled code runs as its closures while it
    // is paused, and compiled again once it runs on.
    const pair = interpretedLibrary();
    installStandardLibrary(pair.env);
    let whilePaused = null;
    const runtime = new SchemeDebugRuntime({
      onPause: () => {
        whilePaused = pair.env.lookup('map').$compiled === true;
        setTimeout(() => runtime.resume(), 2);
      }
    });
    runtime.breakOnUncaughtException = true;
    pair.interpreter.setDebugRuntime(runtime);
    runtime.enable();
    try {
      await pair.interpreter.runAsync(analyze(parse('(error "boom")')[0]), pair.env, {});
    } catch (e) {
      // The error still propagates after the pause.
    }
    const afterwards = pair.env.lookup('map').$compiled === true;
    pair.interpreter.setDebugRuntime(null);
    assert(logger, 'paused on an error, compiled code runs as its closures', whilePaused, false);
    assert(logger, 'and compiled again once the program runs on', afterwards, true);
  }
  {
    // A breakpoint inside the library itself, in `map`'s definition.
    const pair = interpretedLibrary();
    installStandardLibrary(pair.env);
    let paused = null;
    const runtime = new SchemeDebugRuntime({
      onPause: (info) => { paused = paused ?? info; setTimeout(() => runtime.resume(), 5); }
    });
    pair.interpreter.setDebugRuntime(runtime);
    runtime.enable();
    const id = runtime.setBreakpoint('list.scm', 26);
    try {
      await pair.interpreter.runAsync(analyze(parse('(map (lambda (x) x) (list 1))')[0]), pair.env, {});
    } finally {
      runtime.removeBreakpoint(id);
      pair.interpreter.setDebugRuntime(null);
    }
    assert(logger, 'a breakpoint inside the compiled library fires', paused !== null, true);
  }
}
