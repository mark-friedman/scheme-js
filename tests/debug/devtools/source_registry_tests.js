/**
 * @fileoverview Unit tests for SchemeSourceRegistry.
 *
 * Tests source registration, probe lookup, and URL conventions.
 * Part of Phase 1: Foundation.
 */

import { assert } from '../../harness/helpers.js';
import { SchemeSourceRegistry } from '../../../src/debug/devtools/source_registry.js';
import { parse } from '../../../src/core/interpreter/reader.js';

function regSource(registry, url, content, origin) {
  const exprs = parse(content, { filename: url });
  registry.register(url, content, origin, exprs);
}

/**
 * Runs all source registry tests.
 * @param {Object} logger - Test logger
 */
export function runSourceRegistryTests(logger) {
  logger.title('SchemeSourceRegistry - Registration');

  // Test: Register a source
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/test.scm', '(define x 1)\n(+ x 2)\n', 'external');

    assert(logger, 'source is registered', registry.hasSource('scheme://app/test.scm'), true);
    assert(logger, 'unregistered source returns false', registry.hasSource('scheme://app/other.scm'), false);
  }

  // Test: Get source info
  {
    const registry = new SchemeSourceRegistry();
    const content = '(define x 1)\n(+ x 2)\n';
    regSource(registry, 'scheme://app/test.scm', content, 'external');

    const info = registry.getSourceInfo('scheme://app/test.scm');
    assert(logger, 'source info has content', info.content, content);
    assert(logger, 'source info has lines', info.lines, 2);
    assert(logger, 'source info has origin', info.origin, 'external');
  }

  logger.title('SchemeSourceRegistry - Probe Lookup');

  // Test: getProbe returns functions after registration
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/test.scm', '(define x 1)\n(+ x 2)\n', 'external');

    // the first specific expression from AST:
    // (define x 1) is line 1, col 1
    const exprId1 = registry.getExprId('scheme://app/test.scm', 1, 1);
    const probe1 = registry.getProbe('scheme://app/test.scm', exprId1);

    assert(logger, 'probe for first expression is a function', typeof probe1, 'function');
  }

  // Test: getProbe returns null for invalid exprId
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/test.scm', '(+ 1 2)\n', 'external');

    const probe = registry.getProbe('scheme://app/test.scm', 99);
    assert(logger, 'probe for invalid exprId returns null', probe, null);
  }

  // Test: getProbe returns null for unregistered source
  {
    const registry = new SchemeSourceRegistry();

    const probe = registry.getProbe('scheme://app/unknown.scm', 1);
    assert(logger, 'probe for unregistered source returns null', probe, null);
  }

  // Test: Probe functions are callable and accept envProxy
  {
    globalThis.__schemeProbeRuntime = { hit: () => { } };
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/test.scm', '(+ 1 2)\n', 'external');

    const exprId1 = registry.getExprId('scheme://app/test.scm', 1, 1);
    const probe = registry.getProbe('scheme://app/test.scm', exprId1);
    // Calling with a simple object should not throw
    let threw = false;
    try {
      probe({});
    } catch (e) {
      threw = true;
    }
    assert(logger, 'probe function is callable with envProxy', threw, false);
  }

  logger.title('SchemeSourceRegistry - Multiple Sources');

  // Test: Multiple sources can be registered
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/a.scm', '(define a 1)\n', 'external');
    regSource(registry, 'scheme://app/b.scm', '(define b 2)\n(+ b 1)\n', 'external');

    assert(logger, 'source a is registered', registry.hasSource('scheme://app/a.scm'), true);
    assert(logger, 'source b is registered', registry.hasSource('scheme://app/b.scm'), true);

    const exprIdA = registry.getExprId('scheme://app/a.scm', 1, 1);
    const exprIdB = registry.getExprId('scheme://app/b.scm', 1, 1);

    const probeA = registry.getProbe('scheme://app/a.scm', exprIdA);
    const probeB = registry.getProbe('scheme://app/b.scm', exprIdB);

    assert(logger, 'probe from source a is a function', typeof probeA, 'function');
    assert(logger, 'probe from source b is a function', typeof probeB, 'function');
  }

  // Test: Re-registering same URL overwrites
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/test.scm', '(+ 1 2)\n', 'external');
    regSource(registry, 'scheme://app/test.scm', '(+ 3 4)\n(* 5 6)\n', 'external');

    const info = registry.getSourceInfo('scheme://app/test.scm');
    assert(logger, 're-registered source has updated lines', info.lines, 2);
  }

  logger.title('SchemeSourceRegistry - getAllSources');

  // Test: getAllSources returns registered URLs
  {
    const registry = new SchemeSourceRegistry();
    regSource(registry, 'scheme://app/a.scm', '(+ 1 2)\n', 'external');
    regSource(registry, 'scheme://inline/script-0.scm', '(+ 3 4)\n', 'inline');

    const sources = registry.getAllSources();
    assert(logger, 'getAllSources returns correct count', sources.length, 2);

    const urls = sources.map(s => s.url);
    assert(logger, 'getAllSources includes first URL', urls.includes('scheme://app/a.scm'), true);
    assert(logger, 'getAllSources includes second URL', urls.includes('scheme://inline/script-0.scm'), true);
  }
}
