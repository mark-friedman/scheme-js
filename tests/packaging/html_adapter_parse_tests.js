/**
 * @fileoverview Tests for html_adapter.js parse/analyze pipeline edge cases.
 *
 * Covers two bugs found when running Scheme web apps in debug mode:
 *
 *  1. parse() returns null for a bare '()' top-level expression (the Scheme
 *     empty list), which was being passed directly to analyze(), causing a
 *     "cannot analyze null (empty list)" error.  The fix filters out null
 *     entries before calling analyze().
 *
 *  2. When rollup bundles scheme-html.js it creates a separate copy of
 *     library_registry.js with fileResolver = null, disconnected from the
 *     resolver set in scheme.js (scheme_entry.js).  The fix makes
 *     html_adapter.js import parse/analyze/list/intern/setFileResolver from
 *     ./scheme_entry.js (external) so all uses share one module instance.
 *     These tests exercise the underlying library-loader behaviour directly.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import { setFileResolver, getFileResolver, loadLibrarySync } from '../../src/core/interpreter/library_loader.js';
import { SchemeLibraryError } from '../../src/core/interpreter/errors.js';

/**
 * Runs html_adapter parse/analyze pipeline tests.
 * @param {Object} logger - Test logger
 */
export function runHtmlAdapterParseTests(logger) {
  logger.title('html_adapter — parse() null-expression handling');

  // -----------------------------------------------------------------------
  // Issue 1: parse() returns null for a bare '()' top-level expression
  // -----------------------------------------------------------------------

  // Bare '()' as the only top-level expression produces a [null] array.
  {
    const exprs = parse('()');
    assert(logger, 'parse("()") returns array of length 1', exprs.length, 1);
    assert(logger, 'parse("()")[0] is null (empty list)', exprs[0], null);
  }

  // Bare '()' mixed with a real expression.
  {
    const exprs = parse('()\n(define x 1)');
    assert(logger, 'parse with null + define returns 2 expressions', exprs.length, 2);
    assert(logger, 'first expression is null', exprs[0], null);
    assert(logger, 'second expression is non-null', exprs[1] !== null, true);
  }

  // Filtering null before analyze() allows the remaining forms to be analyzed.
  {
    const raw = parse('()\n(define x 42)');
    const filtered = raw.filter(e => e !== null);
    assert(logger, 'filtered array has one entry', filtered.length, 1);

    let threw = false;
    let ast = null;
    try {
      ast = analyze(filtered[0]);
    } catch (e) {
      threw = true;
    }
    assert(logger, 'analyze() does not throw after null is filtered', threw, false);
    assert(logger, 'analyze() produces a non-null AST node', ast !== null, true);
  }

  // Calling analyze(null) directly still throws the expected error.
  {
    let threw = false;
    let msg = '';
    try {
      analyze(null);
    } catch (e) {
      threw = true;
      msg = e.message || '';
    }
    assert(logger, 'analyze(null) throws', threw, true);
    assert(logger, 'analyze(null) error mentions empty list', msg.includes('null') || msg.includes('empty'), true);
  }

  // A file containing only '()' produces an empty filtered array → no AST.
  {
    const raw = parse('  ()  \n  ()  ');
    const filtered = raw.filter(e => e !== null);
    assert(logger, 'file with only () expressions produces empty filtered array', filtered.length, 0);
  }

  // Normal code (no bare '()') is unaffected by filtering.
  {
    const raw = parse('(define y 10)\n(+ y 1)');
    const filtered = raw.filter(e => e !== null);
    assert(logger, 'normal code: no entries removed by filter', filtered.length, raw.length);
    assert(logger, 'normal code: filtered length is 2', filtered.length, 2);
  }

  logger.title('html_adapter — library-loader file-resolver safety');

  // -----------------------------------------------------------------------
  // Issue 2: loadLibrarySync throws "no file resolver set" when resolver is
  // null.  After the fix, html_adapter.js imports setFileResolver from
  // scheme_entry.js (the same module instance as the interpreter), so the
  // resolver set during bootstrap is always visible.
  // These tests verify the expected behaviour of the resolver API itself.
  // -----------------------------------------------------------------------

  // Save and restore the current resolver to avoid polluting other tests.
  const savedResolver = getFileResolver();

  try {
    // When no resolver is set, loadLibrarySync should throw.
    setFileResolver(null);
    {
      let threw = false;
      let msg = '';
      try {
        loadLibrarySync(['scheme', 'base'], analyze, null, null);
      } catch (e) {
        threw = true;
        msg = e.message || '';
      }
      assert(logger, 'loadLibrarySync throws when resolver is null', threw, true);
      assert(logger,
        'error message mentions "no file resolver"',
        msg.includes('no file resolver'), true);
    }

    // When a resolver is installed, getFileResolver() returns it.
    const mockResolver = (name) => `; mock library for ${name.join('/')}`;
    setFileResolver(mockResolver);
    {
      const resolver = getFileResolver();
      assert(logger, 'getFileResolver() returns installed resolver', resolver, mockResolver);
    }

    // setFileResolver(null) clears the resolver; getFileResolver() returns null.
    setFileResolver(null);
    {
      assert(logger, 'setFileResolver(null) clears resolver', getFileResolver(), null);
    }

  } finally {
    // Always restore the original resolver so subsequent tests are unaffected.
    setFileResolver(savedResolver);
  }

  // Confirm the original resolver is restored.
  assert(logger, 'original file resolver restored after test', getFileResolver(), savedResolver);
}
