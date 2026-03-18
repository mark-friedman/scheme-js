/**
 * @fileoverview End-to-end tests for the cooperative breakpoint/pause flow.
 *
 * These tests exercise the complete path that runs in the browser:
 *   1. Create interpreter with debug runtime
 *   2. Set breakpoints via the __schemeDebug API
 *   3. Run code via runDebug() (async cooperative path)
 *   4. Verify that onPause fires with correct info
 *   5. Resume and verify execution completes
 *
 * This allows debugging the pause flow in Node.js without a browser.
 */

import { assert, run } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { SchemeSourceRegistry } from '../../src/debug/devtools/source_registry.js';
import { DevToolsDebugIntegration } from '../../src/debug/devtools/devtools_debug.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';

/**
 * Sets up a fresh interpreter + debug runtime, mirroring html_adapter.js.
 *
 * @returns {{interpreter, env, debugRuntime, devtools, sourceRegistry, schemeDebug}}
 */
function createDebugInterpreter() {
  const { interpreter, env } = createInterpreter();

  const sourceRegistry = new SchemeSourceRegistry();
  const debugRuntime = new SchemeDebugRuntime();
  debugRuntime.enable();
  interpreter.setDebugRuntime(debugRuntime);

  const devtools = new DevToolsDebugIntegration(sourceRegistry);
  devtools.enable();
  interpreter.devtoolsDebug = devtools;
  debugRuntime.setDevToolsIntegration(devtools);

  // Install __schemeDebug API (just like html_adapter does)
  devtools.installSchemeDebugAPI(interpreter);
  const schemeDebug = globalThis.__schemeDebug;

  return { interpreter, env, debugRuntime, devtools, sourceRegistry, schemeDebug };
}

/**
 * Creates a debug interpreter with the full Scheme bootstrap loaded.
 * This gives access to standard procedures (>, <, not, equal?, etc.)
 * and macros (let, cond, when, unless, etc.) — needed for testing
 * user-defined macros that use standard Scheme.
 *
 * @returns {{interpreter, env, debugRuntime, devtools, sourceRegistry, schemeDebug}}
 */
async function createFullDebugInterpreter() {
  const ctx = createDebugInterpreter();

  // Load bootstrap files in dependency order (same as run_all.js)
  if (typeof process !== 'undefined') {
    const fs = await import('fs');
    const bootstrapFiles = [
      'src/core/scheme/macros.scm',
      'src/core/scheme/equality.scm',
      'src/core/scheme/cxr.scm',
      'src/core/scheme/numbers.scm',
      'src/core/scheme/list.scm',
      'src/core/scheme/control.scm',
      'src/core/scheme/case_lambda.scm',
    ];
    for (const file of bootstrapFiles) {
      const code = fs.readFileSync(file, 'utf8');
      run(ctx.interpreter, code);
    }
  }

  return ctx;
}

/**
 * Parses, analyzes, and registers a piece of Scheme code.
 *
 * @param {Object} ctx - Context from createDebugInterpreter()
 * @param {string} code - Scheme source
 * @param {string} sourceUrl - Source URL to use
 * @returns {Object} Analyzed AST
 */
function prepareCode(ctx, code, sourceUrl) {
  const expressions = parse(code, { filename: sourceUrl, wrapLiterals: true });
  ctx.sourceRegistry.register(sourceUrl, code, 'test', expressions);

  let ast;
  if (expressions.length === 1) {
    ast = analyze(expressions[0]);
  } else {
    ast = analyze(list(intern('begin'), ...expressions));
  }
  return ast;
}

/**
 * Runs all cooperative pause flow tests.
 *
 * @param {Object} logger - Test logger with pass/fail/title methods
 */
export async function runCooperativePauseTests(logger) {

  // =====================================================================
  // Diagnostic: verify setup
  // =====================================================================

  logger.title('Cooperative Pause - Setup Verification');

  {
    const ctx = createDebugInterpreter();
    assert(logger, 'debug runtime is enabled',
      ctx.debugRuntime.enabled, true);
    assert(logger, 'interpreter.debugRuntime is set',
      ctx.interpreter.debugRuntime === ctx.debugRuntime, true);
    assert(logger, '__schemeDebug is installed',
      ctx.schemeDebug !== undefined, true);
  }

  // =====================================================================
  // Breakpoint management
  // =====================================================================

  logger.title('Cooperative Pause - Breakpoint Management');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/bp.scm';
    const id = ctx.schemeDebug.setBreakpoint(url, 3);
    assert(logger, 'setBreakpoint returns string id',
      typeof id, 'string');

    const bps = ctx.schemeDebug.getAllBreakpoints();
    assert(logger, 'getAllBreakpoints returns 1 bp',
      bps.length, 1);
    assert(logger, 'breakpoint filename matches',
      bps[0].filename, url);
    assert(logger, 'breakpoint line matches',
      bps[0].line, 3);
  }

  // =====================================================================
  // hasBreakpoint matching
  // =====================================================================

  logger.title('Cooperative Pause - hasBreakpoint Matching');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/match.scm';
    ctx.schemeDebug.setBreakpoint(url, 2);

    const match = ctx.debugRuntime.breakpointManager.hasBreakpoint({
      filename: url, line: 2, column: 1
    });
    assert(logger, 'hasBreakpoint matches correct file:line', match, true);

    const noMatch = ctx.debugRuntime.breakpointManager.hasBreakpoint({
      filename: url, line: 5, column: 1
    });
    assert(logger, 'hasBreakpoint rejects wrong line', noMatch, false);

    const wrongFile = ctx.debugRuntime.breakpointManager.hasBreakpoint({
      filename: 'scheme://other/file.scm', line: 2, column: 1
    });
    assert(logger, 'hasBreakpoint rejects wrong file', wrongFile, false);
  }

  // =====================================================================
  // shouldPause
  // =====================================================================

  logger.title('Cooperative Pause - shouldPause');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/should-pause.scm';
    ctx.schemeDebug.setBreakpoint(url, 1);

    const result = ctx.debugRuntime.shouldPause(
      { filename: url, line: 1, column: 1 }, {}
    );
    assert(logger, 'shouldPause returns true for matching bp', result, true);

    const noMatch = ctx.debugRuntime.shouldPause(
      { filename: url, line: 99, column: 1 }, {}
    );
    assert(logger, 'shouldPause returns false for no bp', noMatch, false);
  }

  // =====================================================================
  // AST source info verification
  // =====================================================================

  logger.title('Cooperative Pause - AST Source Info');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/source-info.scm';
    const code = '(+ 1 2)';
    const expressions = parse(code, { filename: url });

    assert(logger, 'parsed expression has source',
      expressions[0].source !== undefined, true);
    assert(logger, 'source.filename matches url',
      expressions[0].source.filename, url);
    assert(logger, 'source.line is 1',
      expressions[0].source.line, 1);
  }

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/analyzed-info.scm';
    const ast = prepareCode(ctx, '(+ 1 2)', url);

    assert(logger, 'analyzed AST has source',
      ast.source !== undefined, true);
    assert(logger, 'AST source.filename matches url',
      ast.source.filename, url);
  }

  // =====================================================================
  // End-to-end: breakpoint pauses execution
  // =====================================================================

  logger.title('Cooperative Pause - Breakpoint Pauses Execution');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/pause.scm';
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);

    // panelConnected must be true for handlePause to block
    ctx.debugRuntime.panelConnected = true;

    let paused = false;
    let pauseInfo = null;
    ctx.debugRuntime.onPause = (info) => {
      paused = true;
      pauseInfo = info;
      // Resume immediately so the test completes
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'onPause was called', paused, true);
    assert(logger, 'pause reason is breakpoint',
      pauseInfo?.reason, 'breakpoint');
    assert(logger, 'paused at correct filename',
      pauseInfo?.source?.filename, url);
    assert(logger, 'paused at correct line',
      pauseInfo?.source?.line, 2);
  }

  // =====================================================================
  // Breakpoint at correct line in multi-line code
  // =====================================================================

  logger.title('Cooperative Pause - Multi-line Breakpoint');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/multiline.scm';
    const code = '(define a 10)\n(define b 20)\n(define c 30)\n(+ a b c)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 3);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'paused on line 3', pausedLine, 3);
  }

  // =====================================================================
  // Breakpoint on a bare variable reference line
  // =====================================================================

  logger.title('Cooperative Pause - Variable Reference Breakpoint');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/var-bp.scm';
    // Line 1: define x
    // Line 2: bare reference to x (just "x" on its own line)
    const code = '(define x 42)\nx';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on bare variable pauses on line 2', pausedLine, 2);
  }

  // =====================================================================
  // Breakpoint on a bare literal line
  // =====================================================================

  logger.title('Cooperative Pause - Literal Breakpoint');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/literal-bp.scm';
    // Line 1: define x
    // Line 2: bare number literal on its own line
    const code = '(define x 42)\n99';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on bare literal pauses on line 2', pausedLine, 2);
  }

  // String literal
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/string-bp.scm';
    const code = '(define x 1)\n"hello"';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on string literal pauses on line 2', pausedLine, 2);
  }

  // Boolean literal
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/bool-bp.scm';
    const code = '(define x 1)\n#t';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on boolean literal pauses on line 2', pausedLine, 2);
  }

  // =====================================================================
  // No pause when no breakpoints set
  // =====================================================================

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/no-bp.scm';
    const ast = prepareCode(ctx, '(+ 1 2)', url);

    let paused = false;
    ctx.debugRuntime.onPause = () => {
      paused = true;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'no pause when no breakpoints set', paused, false);
  }

  // =====================================================================
  // No pause when breakpoint is on different file
  // =====================================================================

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/file-a.scm';
    const ast = prepareCode(ctx, '(+ 1 2)', url);
    ctx.schemeDebug.setBreakpoint('scheme://test/file-b.scm', 1);

    let paused = false;
    ctx.debugRuntime.onPause = () => {
      paused = true;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'no pause for bp on different file', paused, false);
  }

  // =====================================================================
  // Breakpoint inside function body (let macro expansion)
  // =====================================================================

  logger.title('Cooperative Pause - In-Function Breakpoint (let macro)');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/in-func-bp.scm';
    // Line 1: define a function
    // Line 2: function body uses let (a Scheme macro)
    // Line 4: call the function
    const code = '(define (f x)\n  (let ((d (* x 2)))\n    d))\n(f 5)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on let inside function body fires', pausedLine, 2);
  }

  // Breakpoint on nested let inside function
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/nested-let-bp.scm';
    const code = '(define (g x)\n  (let ((a 1))\n    (let ((b 2))\n      (+ a b x))))\n(g 10)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 3);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on nested let fires at correct line', pausedLine, 3);
  }

  // =====================================================================
  // Breakpoint on user-defined macros
  // =====================================================================

  logger.title('Cooperative Pause - User-Defined Macro Breakpoints');

  // User-defined my-when macro — breakpoint on the macro call line
  {
    const ctx = await createFullDebugInterpreter();
    const url = 'scheme://test/user-macro-when.scm';
    const code = [
      '(define-syntax my-when',
      '  (syntax-rules ()',
      '    ((my-when test body ...)',
      '     (if test (begin body ...)))))',
      '(define (f x)',
      '  (my-when (> x 0)',
      '    (* x 2)))',
      '(f 5)'
    ].join('\n');
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 6); // breakpoint on (my-when ...)
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on user-defined my-when macro fires', pausedLine, 6);
  }

  // User-defined my-let macro — breakpoint on the macro call line inside function
  {
    const ctx = await createFullDebugInterpreter();
    const url = 'scheme://test/user-macro-let.scm';
    const code = [
      '(define-syntax my-let',
      '  (syntax-rules ()',
      '    ((my-let ((name val) ...) body ...)',
      '     ((lambda (name ...) body ...) val ...))))',
      '(define (g x)',
      '  (my-let ((a (* x 2)))',
      '    (+ a 1)))',
      '(g 10)'
    ].join('\n');
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 6); // breakpoint on (my-let ...)
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on user-defined my-let macro fires', pausedLine, 6);
  }

  // User-defined macro — breakpoint on macro at top level
  {
    const ctx = await createFullDebugInterpreter();
    const url = 'scheme://test/user-macro-toplevel.scm';
    const code = [
      '(define-syntax my-swap!',
      '  (syntax-rules ()',
      '    ((my-swap! a b)',
      '     (let ((tmp a))',
      '       (set! a b)',
      '       (set! b tmp)))))',
      '(define x 1)',
      '(define y 2)',
      '(my-swap! x y)'
    ].join('\n');
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 9); // breakpoint on (my-swap! x y)
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on user-defined macro at top level fires', pausedLine, 9);
  }

  // Nested user-defined macros — breakpoint on inner macro
  {
    const ctx = await createFullDebugInterpreter();
    const url = 'scheme://test/user-macro-nested.scm';
    // my-unless expands through my-when, which expands to if+begin
    const code = [
      '(define-syntax my-when',
      '  (syntax-rules ()',
      '    ((my-when test body ...)',
      '     (if test (begin body ...)))))',
      '(define-syntax my-unless',
      '  (syntax-rules ()',
      '    ((my-unless test body ...)',
      '     (my-when (not test) body ...))))',
      '(define (h x)',
      '  (my-unless (< x 0)',
      '    (+ x 100)))',
      '(h 5)'
    ].join('\n');
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 10); // breakpoint on (my-unless ...)
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on nested user-defined macro fires', pausedLine, 10);
  }

  // =====================================================================
  // Step Into
  // =====================================================================

  logger.title('Cooperative Pause - Stepping');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/step.scm';
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 1);
    ctx.debugRuntime.panelConnected = true;

    let pauseCount = 0;
    ctx.debugRuntime.onPause = (info) => {
      pauseCount++;
      if (pauseCount === 1) {
        ctx.debugRuntime.stepInto();
      } else {
        ctx.debugRuntime.resume();
      }
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'step into causes at least 2 pauses',
      pauseCount >= 2, true);
  }

  // =====================================================================
  // Full panel flow: set bp → "reload" → restore → pause
  // =====================================================================

  logger.title('Cooperative Pause - Panel Reload Flow');

  {
    // Session 1: set a breakpoint
    const ctx1 = createDebugInterpreter();
    const url = 'scheme://test/panel-flow.scm';
    ctx1.schemeDebug.setBreakpoint(url, 2);
    const persisted = ctx1.schemeDebug.getAllBreakpoints()
      .map(bp => ({ url: bp.filename, line: bp.line }));

    // Session 2: fresh interpreter (simulates reload)
    const ctx2 = createDebugInterpreter();
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx2, code, url);

    assert(logger, 'fresh interpreter has no breakpoints',
      ctx2.schemeDebug.getAllBreakpoints().length, 0);

    // Restore persisted breakpoints
    for (const { url: bpUrl, line } of persisted) {
      ctx2.schemeDebug.setBreakpoint(bpUrl, line);
    }

    assert(logger, 'after restore has 1 breakpoint',
      ctx2.schemeDebug.getAllBreakpoints().length, 1);

    let paused = false;
    let pausedLine = null;
    ctx2.debugRuntime.panelConnected = true;
    ctx2.debugRuntime.onPause = (info) => {
      paused = true;
      pausedLine = info.source?.line;
      ctx2.debugRuntime.resume();
    };

    await ctx2.interpreter.runDebug(ast, ctx2.env);

    assert(logger, 'pauses at restored breakpoint', paused, true);
    assert(logger, 'pauses on correct line', pausedLine, 2);
  }

  // =====================================================================
  // Pre-loaded breakpoints (simulates activate_debug.js + html_adapter.js)
  // =====================================================================

  logger.title('Cooperative Pause - Pre-loaded Breakpoints');

  {
    // Simulate activate_debug.js setting __SCHEME_JS_BREAKPOINTS
    // BEFORE the interpreter is created
    const preLoaded = [
      { url: 'scheme://test/preloaded.scm', line: 2 }
    ];

    // Create interpreter + debug runtime (like html_adapter.js does)
    const ctx = createDebugInterpreter();

    // Simulate html_adapter.js restoring pre-loaded breakpoints
    for (const { url, line } of preLoaded) {
      ctx.debugRuntime.setBreakpoint(url, line);
    }

    // Verify breakpoint is set before any code runs
    assert(logger, 'pre-loaded bp is set before code runs',
      ctx.debugRuntime.breakpointManager.hasBreakpoint({
        filename: 'scheme://test/preloaded.scm', line: 2, column: 1
      }), true);

    // Now run code (like html_adapter.js does AFTER restoring breakpoints)
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx, code, 'scheme://test/preloaded.scm');

    let paused = false;
    let pausedLine = null;
    ctx.debugRuntime.panelConnected = true;
    ctx.debugRuntime.onPause = (info) => {
      paused = true;
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'pre-loaded bp pauses execution', paused, true);
    assert(logger, 'pre-loaded bp pauses at correct line', pausedLine, 2);
  }

  // =====================================================================
  // panelConnected flag and activate()
  // =====================================================================

  logger.title('Cooperative Pause - Panel Connection');

  // panelConnected defaults to false
  {
    const ctx = createDebugInterpreter();
    assert(logger, 'panelConnected defaults to false',
      ctx.debugRuntime.panelConnected, false);
  }

  // activate() sets panelConnected
  {
    const ctx = createDebugInterpreter();
    ctx.schemeDebug.activate();
    assert(logger, 'activate() sets panelConnected to true',
      ctx.debugRuntime.panelConnected, true);
  }

  // Without panelConnected, breakpoints fire onPause but don't block
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/no-panel.scm';
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    // panelConnected is false (default) — should NOT block

    let pauseFired = false;
    ctx.debugRuntime.onPause = (info) => {
      pauseFired = true;
      // Do NOT call resume — if it blocks, this test will hang forever
    };

    const result = await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'onPause fires even without panel', pauseFired, true);
    assert(logger, 'execution completes without panel connected',
      result !== undefined, true);
  }

  // With panelConnected + async resume (simulates browser panel)
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/async-panel.scm';
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    ctx.debugRuntime.panelConnected = true;

    let pauseCount = 0;
    ctx.debugRuntime.onPause = (info) => {
      pauseCount++;
      // Simulate async panel response (like browser setTimeout)
      setTimeout(() => ctx.debugRuntime.resume(), 10);
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'async panel resume completes execution', pauseCount, 1);
  }

  // =====================================================================
  // panelConnected from localStorage (simulates activate_debug.js flow)
  // =====================================================================

  logger.title('Cooperative Pause - panelConnected from localStorage');

  // Simulates the full flow: activate_debug.js reads localStorage and sets
  // __SCHEME_JS_PANELCONNECTED, then html_adapter.js applies it to the runtime.
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/localStorage-flow.scm';
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);

    // Simulate html_adapter.js reading __SCHEME_JS_PANELCONNECTED (set by activate_debug.js)
    // and setting panelConnected on the runtime BEFORE scripts run.
    ctx.debugRuntime.panelConnected = true;

    let paused = false;
    ctx.debugRuntime.onPause = (info) => {
      paused = true;
      // Simulate panel response
      setTimeout(() => ctx.debugRuntime.resume(), 5);
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'panelConnected=true from "localStorage" causes blocking pause',
      paused, true);
  }

  // =====================================================================
  // Timing: immediate resume completes fast
  // =====================================================================

  logger.title('Cooperative Pause - Timing');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/timing-fast.scm';
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    ctx.debugRuntime.panelConnected = true;
    ctx.debugRuntime.onPause = () => ctx.debugRuntime.resume();

    const start = Date.now();
    await ctx.interpreter.runDebug(ast, ctx.env);
    const elapsed = Date.now() - start;
    assert(logger, 'immediate resume completes fast (<500ms)',
      elapsed < 500, true);
  }

  // Timeout fires at roughly resumeTimeout ms
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/timing-timeout.scm';
    const code = '(define x 1)\n(+ x 2)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 1);
    ctx.debugRuntime.panelConnected = true;
    ctx.debugRuntime.resumeTimeout = 100; // 100ms for test
    ctx.debugRuntime.onPause = () => { /* no resume — let timeout fire */ };

    const start = Date.now();
    await ctx.interpreter.runDebug(ast, ctx.env);
    const elapsed = Date.now() - start;
    assert(logger, 'timeout fires near resumeTimeout (80-500ms)',
      elapsed >= 80 && elapsed < 500, true);
  }

  // =====================================================================
  // Pause info structure survives JSON serialization (postMessage compat)
  // =====================================================================

  logger.title('Cooperative Pause - Serialization');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/serialize.scm';
    const code = '(define x 1)\n(define y 2)\n(+ x y)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 2);
    ctx.debugRuntime.panelConnected = true;

    let capturedInfo = null;
    ctx.debugRuntime.onPause = (info) => {
      capturedInfo = info;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    // Verify the info can survive JSON round-trip (postMessage uses structuredClone)
    const roundTripped = JSON.parse(JSON.stringify(capturedInfo));
    assert(logger, 'pauseInfo survives serialization (has source)',
      roundTripped.source !== null && roundTripped.source !== undefined, true);
    assert(logger, 'pauseInfo.source.line survives serialization',
      roundTripped.source.line, 2);
    assert(logger, 'pauseInfo.source.filename survives serialization',
      roundTripped.source.filename, url);
    assert(logger, 'pauseInfo has stack array',
      Array.isArray(roundTripped.stack), true);
    assert(logger, 'pauseInfo.reason survives serialization',
      roundTripped.reason, 'breakpoint');
  }

  // =====================================================================
  // ackPause cancels the safety timeout
  // =====================================================================

  logger.title('Cooperative Pause - ackPause');

  // ackPause prevents timeout from firing
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/ack-pause.scm';
    const code = '(define x 1)\n(+ x 2)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 1);
    ctx.debugRuntime.panelConnected = true;
    ctx.debugRuntime.resumeTimeout = 50; // 50ms — would fire fast without ack

    let pauseFired = false;
    ctx.debugRuntime.onPause = (info) => {
      pauseFired = true;
      // Acknowledge the pause (like the panel does)
      ctx.debugRuntime.ackPause();
      // Resume after 200ms (well after the 50ms timeout would have fired)
      setTimeout(() => ctx.debugRuntime.resume(), 200);
    };

    const start = Date.now();
    await ctx.interpreter.runDebug(ast, ctx.env);
    const elapsed = Date.now() - start;

    assert(logger, 'ackPause: pause fired', pauseFired, true);
    // Should take ~200ms (the explicit resume delay), not ~50ms (the timeout)
    assert(logger, 'ackPause: resumed by explicit resume, not timeout (>100ms)',
      elapsed >= 100, true);
    // panelConnected should still be true (timeout didn't fire and clear it)
    assert(logger, 'ackPause: panelConnected preserved',
      ctx.debugRuntime.panelConnected, true);
  }

  // Without ackPause, timeout fires and clears panelConnected
  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/no-ack.scm';
    const code = '(define x 1)\n(+ x 2)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 1);
    ctx.debugRuntime.panelConnected = true;
    ctx.debugRuntime.resumeTimeout = 50;

    let resumeReason = null;
    ctx.debugRuntime.onPause = () => {
      // Do NOT ack — simulates panel not responding
    };
    ctx.debugRuntime.onResume = (reason) => {
      resumeReason = reason;
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'no ack: timeout fires and clears panelConnected',
      ctx.debugRuntime.panelConnected, false);
    assert(logger, 'no ack: onResume called with timeout reason',
      resumeReason, 'timeout');
  }

  // =====================================================================
  // Timeout safety (stale panelConnected flag, panel not responding)
  // =====================================================================

  logger.title('Cooperative Pause - Timeout Safety');

  {
    const ctx = createDebugInterpreter();
    const url = 'scheme://test/timeout-safety.scm';
    const code = '(define x 1)\n(+ x 2)';
    const ast = prepareCode(ctx, code, url);
    ctx.schemeDebug.setBreakpoint(url, 1);

    // Set panelConnected=true (stale flag — panel "closed")
    ctx.debugRuntime.panelConnected = true;
    // Use a very short timeout for testing (1ms instead of 30s)
    ctx.debugRuntime.resumeTimeout = 1;

    let pauseFired = false;
    ctx.debugRuntime.onPause = () => {
      pauseFired = true;
      // Do NOT call resume — simulates panel not responding
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'timeout safety: onPause fired', pauseFired, true);
    assert(logger, 'timeout safety: execution completed despite no resume call',
      ctx.debugRuntime.pauseController.getState(), 'running');
    assert(logger, 'timeout safety: panelConnected cleared after timeout',
      ctx.debugRuntime.panelConnected, false);
  }
}
