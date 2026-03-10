/**
 * @fileoverview Unit tests for expression-level breakpoints and expression spans.
 *
 * Tests the getExpressions() API on SourceRegistry, column-level breakpoints
 * on BreakpointManager, and the __schemeDebug.getExpressions() page API.
 * Part of Phase 3: Expression-Level Breakpoints.
 */

import { assert } from '../harness/helpers.js';
import { SchemeSourceRegistry } from '../../src/debug/devtools/source_registry.js';
import { BreakpointManager } from '../../src/debug/breakpoint_manager.js';
import { parse } from '../../src/core/interpreter/reader.js';

/**
 * Helper to register a source with the registry.
 * @param {SchemeSourceRegistry} registry
 * @param {string} url
 * @param {string} content
 * @param {string} origin
 */
function regSource(registry, url, content, origin) {
  const exprs = parse(content, { filename: url });
  registry.register(url, content, origin, exprs);
}

/**
 * Runs expression breakpoint and expression span tests.
 * @param {Object} logger - Test logger
 */
export function runExpressionBreakpointTests(logger) {
  logger.title('Expression Spans - getExpressions()');

  // Test: getExpressions returns [] for unknown URL
  {
    const registry = new SchemeSourceRegistry();
    const spans = registry.getExpressions('scheme://unknown/file.scm');
    assert(logger, 'getExpressions returns [] for unknown URL', spans.length, 0);
  }

  // Test: getExpressions returns spans after register()
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/test.scm', '(define x 1)\n(+ x 2)\n', 'external');

    const spans = registry.getExpressions('scheme://app/test.scm');
    assert(logger, 'getExpressions returns spans', spans.length > 0, true);

    // Each span should have the required fields
    const first = spans[0];
    assert(logger, 'span has exprId', typeof first.exprId, 'number');
    assert(logger, 'span has line', typeof first.line, 'number');
    assert(logger, 'span has column', typeof first.column, 'number');
    assert(logger, 'span has endLine', first.endLine !== undefined, true);
    assert(logger, 'span has endColumn', first.endColumn !== undefined, true);
  }

  // Test: Spans include correct line/column for known expressions
  {
    const registry = new SchemeSourceRegistry();
    const code = '(+ 1 2)\n(* 3 4)\n';
    regSource(registry, 'scheme://app/arith.scm', code, 'external');

    const spans = registry.getExpressions('scheme://app/arith.scm');

    // Should have spans for both top-level expressions and sub-expressions
    // (+ 1 2) starts at line 1, col 1
    const plusExpr = spans.find(s => s.line === 1 && s.column === 1);
    assert(logger, 'found (+ 1 2) span at line 1, col 1', !!plusExpr, true);

    // (* 3 4) starts at line 2, col 1
    const mulExpr = spans.find(s => s.line === 2 && s.column === 1);
    assert(logger, 'found (* 3 4) span at line 2, col 1', !!mulExpr, true);
  }

  // Test: Spans persist through multiple registrations
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/a.scm', '(+ 1 2)\n', 'external');
    regSource(registry, 'scheme://app/b.scm', '(* 3 4)\n', 'external');

    const spansA = registry.getExpressions('scheme://app/a.scm');
    const spansB = registry.getExpressions('scheme://app/b.scm');

    assert(logger, 'spans for a.scm exist', spansA.length > 0, true);
    assert(logger, 'spans for b.scm exist', spansB.length > 0, true);
  }

  // Test: REPL pruning cleans up expression spans
  {
    const registry = new SchemeSourceRegistry();
    registry.replSourceLimit = 2;

    regSource(registry, 'scheme://repl/eval-0.scm', '(+ 1 2)', 'repl');
    regSource(registry, 'scheme://repl/eval-1.scm', '(+ 3 4)', 'repl');
    regSource(registry, 'scheme://repl/eval-2.scm', '(+ 5 6)', 'repl');

    // eval-0 should have been pruned
    const pruned = registry.getExpressions('scheme://repl/eval-0.scm');
    assert(logger, 'pruned REPL source has no expression spans', pruned.length, 0);

    // eval-2 should still have spans
    const kept = registry.getExpressions('scheme://repl/eval-2.scm');
    assert(logger, 'kept REPL source has expression spans', kept.length > 0, true);
  }

  // Test: Re-registering updates expression spans
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/test.scm', '(+ 1 2)\n', 'external');
    const spans1 = registry.getExpressions('scheme://app/test.scm');

    regSource(registry, 'scheme://app/test.scm', '(+ 1 2)\n(* 3 4)\n', 'external');
    const spans2 = registry.getExpressions('scheme://app/test.scm');

    assert(logger, 're-registration updates spans count',
      spans2.length > spans1.length, true);
  }

  logger.title('Expression Spans - JSON serializable');

  // Test: getExpressions output is JSON-serializable (simulating __schemeDebug bridge)
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/test.scm', '(define (f x) (+ x 1))\n', 'external');

    const spans = registry.getExpressions('scheme://app/test.scm');
    const serialized = JSON.parse(JSON.stringify(spans.map(s => ({
      exprId: s.exprId,
      line: s.line,
      column: s.column,
      endLine: s.endLine,
      endColumn: s.endColumn,
    }))));

    assert(logger, 'serialized spans have correct structure',
      serialized.length > 0 && typeof serialized[0].exprId === 'number', true);
  }

  logger.title('Column Breakpoints - BreakpointManager');

  // Test: setBreakpoint with column creates column-level breakpoint
  {
    const bm = new BreakpointManager();
    const id = bm.setBreakpoint('test.scm', 1, 5);

    const bp = bm.getBreakpoint(id);
    assert(logger, 'column breakpoint has correct column', bp.column, 5);
    assert(logger, 'column breakpoint has correct line', bp.line, 1);
  }

  // Test: Column breakpoint matches only exact column
  {
    const bm = new BreakpointManager();
    bm.setBreakpoint('test.scm', 1, 5);

    // Should match exact column
    assert(logger, 'matches exact column',
      bm.hasBreakpoint({ filename: 'test.scm', line: 1, column: 5 }), true);

    // Should NOT match different column on same line
    assert(logger, 'does not match different column',
      bm.hasBreakpoint({ filename: 'test.scm', line: 1, column: 10 }), false);
  }

  // Test: Line-level and column-level breakpoints coexist on same line
  {
    const bm = new BreakpointManager();
    bm.setBreakpoint('test.scm', 1, null);  // Line-level
    bm.setBreakpoint('test.scm', 1, 5);     // Column-level

    // Line-level matches any column
    assert(logger, 'line-level matches column 1',
      bm.hasBreakpoint({ filename: 'test.scm', line: 1, column: 1 }), true);

    // Column-level also matches
    assert(logger, 'column-level matches column 5',
      bm.hasBreakpoint({ filename: 'test.scm', line: 1, column: 5 }), true);

    // Check both are in getAllBreakpoints
    const all = bm.getAllBreakpoints();
    const onLine1 = all.filter(bp => bp.line === 1);
    assert(logger, 'two breakpoints on line 1', onLine1.length, 2);
  }

  // Test: Removing column breakpoint doesn't affect line breakpoint
  {
    const bm = new BreakpointManager();
    const lineId = bm.setBreakpoint('test.scm', 1, null);
    const colId = bm.setBreakpoint('test.scm', 1, 5);

    bm.removeBreakpoint(colId);

    // Line-level should still work
    assert(logger, 'line-level still matches after column removed',
      bm.hasBreakpoint({ filename: 'test.scm', line: 1, column: 1 }), true);

    // Column-level should be gone
    assert(logger, 'column-level gone after removal',
      bm.getBreakpoint(colId), null);
  }
}
