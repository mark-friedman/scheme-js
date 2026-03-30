/**
 * @fileoverview Tests for source location propagation through macro expansion.
 *
 * Verifies that `flipScopeInExpression`, `addScopeToExpression`, and
 * `unwrapSyntax` preserve the `.source` property on Cons cells, ensuring
 * that breakpoints inside macro-expanded bodies (e.g. `when`, `unless`)
 * fire correctly.
 */

import { assert } from '../harness/helpers.js';
import {
  flipScopeInExpression,
  addScopeToExpression,
  unwrapSyntax,
  freshScope,
  resetScopeCounter,
} from '../../src/core/interpreter/syntax_object.js';
import { Cons, list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { SchemeSourceRegistry } from '../../src/debug/devtools/source_registry.js';
import { DevToolsDebugIntegration } from '../../src/debug/devtools/devtools_debug.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { run } from '../harness/helpers.js';

// =============================================================================
// Helpers
// =============================================================================

/**
 * Creates a Cons cell with source info for testing.
 * @param {*} car
 * @param {*} cdr
 * @param {number} line
 * @param {number} col
 * @returns {Cons}
 */
function consWithSource(car, cdr, line, col) {
  return new Cons(car, cdr, { filename: 'test.scm', line, column: col, endLine: line, endColumn: col + 10 });
}

/**
 * Sets up a debug interpreter with bootstrap loaded.
 * @returns {Promise<{interpreter, env, debugRuntime, devtools, sourceRegistry, schemeDebug}>}
 */
async function createFullDebugInterpreter() {
  const { interpreter, env } = createInterpreter();

  const sourceRegistry = new SchemeSourceRegistry();
  const debugRuntime = new SchemeDebugRuntime();
  debugRuntime.enable();
  interpreter.setDebugRuntime(debugRuntime);

  const devtools = new DevToolsDebugIntegration(sourceRegistry);
  devtools.enable();
  interpreter.devtoolsDebug = devtools;
  debugRuntime.setDevToolsIntegration(devtools);

  devtools.installSchemeDebugAPI(interpreter);
  const schemeDebug = globalThis.__schemeDebug;

  // Load bootstrap so that standard macros (when, unless, let) are available
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
      run(interpreter, code);
    }
  }

  return { interpreter, env, debugRuntime, devtools, sourceRegistry, schemeDebug };
}

/**
 * Parses, registers, and analyzes Scheme code.
 * @param {{sourceRegistry: SchemeSourceRegistry}} ctx
 * @param {string} code
 * @param {string} url
 * @returns {*} analyzed AST
 */
function prepareCode(ctx, code, url) {
  const expressions = parse(code, { filename: url, wrapLiterals: true });
  ctx.sourceRegistry.register(url, code, 'test', expressions);

  if (expressions.length === 1) {
    return analyze(expressions[0]);
  }
  return analyze(list(intern('begin'), ...expressions));
}

// =============================================================================
// Unit tests: source preservation in scope functions
// =============================================================================

/**
 * Runs macro source propagation unit and functional tests.
 * @param {Object} logger
 */
export async function runMacroSourcePropagationTests(logger) {
  resetScopeCounter();

  // =========================================================================
  // flipScopeInExpression preserves .source
  // =========================================================================

  logger.title('flipScopeInExpression - source preservation');

  {
    const scope = freshScope();
    const inner = consWithSource(intern('x'), null, 5, 3);
    const outer = consWithSource(inner, null, 5, 1);

    const result = flipScopeInExpression(outer, scope);

    assert(logger, 'flipScopeInExpression: outer Cons preserves source.line',
      result.source?.line, 5);
    assert(logger, 'flipScopeInExpression: outer Cons preserves source.column',
      result.source?.column, 1);
    assert(logger, 'flipScopeInExpression: inner Cons preserves source.line',
      result.car?.source?.line, 5);
    assert(logger, 'flipScopeInExpression: inner Cons preserves source.column',
      result.car?.source?.column, 3);
  }

  // =========================================================================
  // addScopeToExpression preserves .source
  // =========================================================================

  logger.title('addScopeToExpression - source preservation');

  {
    const scope = freshScope();
    const inner = consWithSource(intern('y'), null, 10, 7);
    const outer = consWithSource(inner, null, 10, 1);

    const result = addScopeToExpression(outer, scope);

    assert(logger, 'addScopeToExpression: outer Cons preserves source.line',
      result.source?.line, 10);
    assert(logger, 'addScopeToExpression: outer Cons preserves source.column',
      result.source?.column, 1);
    assert(logger, 'addScopeToExpression: inner Cons preserves source.line',
      result.car?.source?.line, 10);
    assert(logger, 'addScopeToExpression: inner Cons preserves source.column',
      result.car?.source?.column, 7);
  }

  // =========================================================================
  // unwrapSyntax preserves .source
  // =========================================================================

  logger.title('unwrapSyntax - source preservation');

  {
    const inner = consWithSource(intern('z'), null, 3, 5);
    const outer = consWithSource(inner, null, 3, 1);

    const result = unwrapSyntax(outer);

    assert(logger, 'unwrapSyntax: outer Cons preserves source.line',
      result.source?.line, 3);
    assert(logger, 'unwrapSyntax: outer Cons preserves source.column',
      result.source?.column, 1);
    assert(logger, 'unwrapSyntax: inner Cons preserves source.line',
      result.car?.source?.line, 3);
    assert(logger, 'unwrapSyntax: inner Cons preserves source.column',
      result.car?.source?.column, 5);
  }

  // =========================================================================
  // Nested Cons source preserved through deep structures
  // =========================================================================

  logger.title('Nested source preservation through deep Cons structures');

  {
    const scope = freshScope();
    // Build a list with source at each level
    const leaf = consWithSource(intern('a'), null, 7, 9);
    const mid = consWithSource(leaf, null, 7, 5);
    const root = consWithSource(mid, null, 7, 1);

    const result = flipScopeInExpression(root, scope);

    assert(logger, 'deep flip: root source.line', result.source?.line, 7);
    assert(logger, 'deep flip: mid source.line', result.car?.source?.line, 7);
    assert(logger, 'deep flip: leaf source.line', result.car?.car?.source?.line, 7);
    assert(logger, 'deep flip: root source.column', result.source?.column, 1);
    assert(logger, 'deep flip: mid source.column', result.car?.source?.column, 5);
    assert(logger, 'deep flip: leaf source.column', result.car?.car?.source?.column, 9);
  }

  // =========================================================================
  // Functional: breakpoint inside (when #t ...) body fires
  // =========================================================================

  logger.title('Functional - breakpoint inside (when) body fires');

  // Skip functional tests in browser (no fs access for bootstrap)
  if (typeof process === 'undefined') {
    logger.skip('Functional macro breakpoint tests require Node.js');
    return;
  }

  {
    const ctx = await createFullDebugInterpreter();
    const url = 'scheme://test/when-bp.scm';
    // Line 1: (define result 0)
    // Line 2: (when #t
    // Line 3:   (set! result 42))
    const code = '(define result 0)\n(when #t\n  (set! result 42))';
    const ast = prepareCode(ctx, code, url);

    ctx.schemeDebug.setBreakpoint(url, 3);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint inside (when) body pauses on line 3', pausedLine, 3);
  }

  // =========================================================================
  // Functional: breakpoint on 2nd expression in (when) body fires
  // =========================================================================

  logger.title('Functional - breakpoint on 2nd expression in (when) body');

  {
    const ctx = await createFullDebugInterpreter();
    const url = 'scheme://test/when-multi.scm';
    // Line 1: (define result 0)
    // Line 2: (when #t
    // Line 3:   (set! result 1)
    // Line 4:   (set! result 2))
    const code = '(define result 0)\n(when #t\n  (set! result 1)\n  (set! result 2))';
    const ast = prepareCode(ctx, code, url);

    ctx.schemeDebug.setBreakpoint(url, 4);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint on 2nd expr in (when) pauses on line 4', pausedLine, 4);
  }

  // =========================================================================
  // Functional: breakpoint inside (unless) body fires
  // =========================================================================

  logger.title('Functional - breakpoint inside (unless) body fires');

  {
    const ctx = await createFullDebugInterpreter();
    const url = 'scheme://test/unless-bp.scm';
    // Line 1: (define x 0)
    // Line 2: (unless #f
    // Line 3:   (set! x 99))
    const code = '(define x 0)\n(unless #f\n  (set! x 99))';
    const ast = prepareCode(ctx, code, url);

    ctx.schemeDebug.setBreakpoint(url, 3);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint inside (unless) body pauses on line 3', pausedLine, 3);
  }

  // =========================================================================
  // Functional: breakpoint inside (when) nested in (let) fires
  // =========================================================================

  logger.title('Functional - breakpoint inside nested macros (when inside let)');

  {
    const ctx = await createFullDebugInterpreter();
    const url = 'scheme://test/nested-macro.scm';
    // Line 1: (define result 0)
    // Line 2: (let ((flag #t))
    // Line 3:   (when flag
    // Line 4:     (set! result 77)))
    const code = '(define result 0)\n(let ((flag #t))\n  (when flag\n    (set! result 77)))';
    const ast = prepareCode(ctx, code, url);

    ctx.schemeDebug.setBreakpoint(url, 4);
    ctx.debugRuntime.panelConnected = true;

    let pausedLine = null;
    ctx.debugRuntime.onPause = (info) => {
      pausedLine = info.source?.line;
      ctx.debugRuntime.resume();
    };

    await ctx.interpreter.runDebug(ast, ctx.env);

    assert(logger, 'breakpoint inside (when inside let) pauses on line 4', pausedLine, 4);
  }
}
