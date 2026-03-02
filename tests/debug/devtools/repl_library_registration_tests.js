/**
 * @fileoverview Tests for Phase 7 features:
 *   - 7.2: REPL source registration (dynamic eval probes)
 *   - 7.3: Library source registration
 *
 * Verifies that REPL eval inputs and library sources are registered
 * with the SchemeSourceRegistry when DevTools debugging is enabled,
 * generating probe scripts with correct URLs.
 */

import { assert } from '../../harness/helpers.js';
import { DevToolsDebugIntegration } from '../../../src/debug/devtools/devtools_debug.js';
import { SchemeSourceRegistry } from '../../../src/debug/devtools/source_registry.js';
import { parse } from '../../../src/core/interpreter/reader.js';

// =========================================================================
// Helper
// =========================================================================

/**
 * Creates a DevToolsDebugIntegration with an empty registry.
 * @returns {{devtools: DevToolsDebugIntegration, registry: SchemeSourceRegistry}}
 */
function createTestSetup() {
  const registry = new SchemeSourceRegistry();
  const devtools = new DevToolsDebugIntegration(registry);
  devtools.enable();
  return { devtools, registry };
}

// =========================================================================
// 7.2: REPL Source Registration Tests
// =========================================================================

/**
 * @param {Object} logger - Test logger
 */
export function runReplRegistrationTests(logger) {
  logger.title('REPL Source Registration — URL Generation');

  // Test: getNextReplSourceId returns incrementing URLs
  {
    const { devtools } = createTestSetup();

    const id0 = devtools.getNextReplSourceId();
    const id1 = devtools.getNextReplSourceId();
    const id2 = devtools.getNextReplSourceId();

    assert(logger, 'first REPL source ID', id0, 'scheme://repl/eval-0.scm');
    assert(logger, 'second REPL source ID', id1, 'scheme://repl/eval-1.scm');
    assert(logger, 'third REPL source ID', id2, 'scheme://repl/eval-2.scm');
  }

  // Test: Counter resets after disable/enable cycle
  {
    const { devtools } = createTestSetup();

    devtools.getNextReplSourceId(); // eval-0
    devtools.getNextReplSourceId(); // eval-1
    devtools.disable();
    devtools.enable();

    // Counter should NOT reset — eval IDs must stay unique across sessions
    // to avoid URL collisions with persisted breakpoints.
    const id = devtools.getNextReplSourceId();
    assert(logger, 'counter persists across disable/enable', id, 'scheme://repl/eval-2.scm');
  }

  logger.title('REPL Source Registration — Source Registration');

  // Test: registerReplEval registers source and probes
  {
    const { devtools, registry } = createTestSetup();
    const code = '(+ 1 2)';

    const { sourceId, expressions } = devtools.registerReplEval(code);

    assert(logger, 'returned sourceId follows REPL URL pattern',
      sourceId.startsWith('scheme://repl/eval-'), true);
    assert(logger, 'source is registered in registry',
      registry.hasSource(sourceId), true);
    assert(logger, 'expressions are parsed S-expressions',
      Array.isArray(expressions), true);
    assert(logger, 'expressions are non-empty', expressions.length > 0, true);

    // Verify source info
    const info = registry.getSourceInfo(sourceId);
    assert(logger, 'registered source has correct content', info.content, code);
    assert(logger, 'registered source has origin repl', info.origin, 'repl');
  }

  // Test: registerReplEval generates probes
  {
    const { devtools, registry } = createTestSetup();
    const code = '(define x 42)\n(+ x 1)';

    const { sourceId } = devtools.registerReplEval(code);

    // Check that probes were generated for the expressions
    const exprId = registry.getExprId(sourceId, 1, 1);
    assert(logger, 'expression ID assigned for first expression', exprId !== null, true);

    const probe = registry.getProbe(sourceId, exprId);
    assert(logger, 'probe function generated', typeof probe, 'function');
  }

  // Test: multiple registerReplEval calls get unique URLs
  {
    const { devtools, registry } = createTestSetup();

    const r1 = devtools.registerReplEval('(+ 1 2)');
    const r2 = devtools.registerReplEval('(* 3 4)');

    assert(logger, 'different evals get different sourceIds',
      r1.sourceId !== r2.sourceId, true);
    assert(logger, 'both sources registered',
      registry.hasSource(r1.sourceId) && registry.hasSource(r2.sourceId), true);
  }

  logger.title('REPL Source Registration — LRU Pruning');

  // Test: REPL sources are pruned when limit is exceeded
  {
    const { devtools, registry } = createTestSetup();

    // Set a small limit for testing
    registry.replSourceLimit = 5;

    const sourceIds = [];
    for (let i = 0; i < 8; i++) {
      const { sourceId } = devtools.registerReplEval(`(+ ${i} 1)`);
      sourceIds.push(sourceId);
    }

    // The oldest 3 should have been pruned (8 - 5 = 3)
    assert(logger, 'oldest REPL source pruned',
      registry.hasSource(sourceIds[0]), false);
    assert(logger, 'second oldest REPL source pruned',
      registry.hasSource(sourceIds[1]), false);
    assert(logger, 'third oldest REPL source pruned',
      registry.hasSource(sourceIds[2]), false);

    // The newest 5 should still exist
    assert(logger, 'newest REPL sources still registered',
      registry.hasSource(sourceIds[7]), true);
    assert(logger, 'fifth newest still registered',
      registry.hasSource(sourceIds[3]), true);
  }

  // Test: Non-REPL sources are not affected by pruning
  {
    const { devtools, registry } = createTestSetup();
    registry.replSourceLimit = 3;

    // Register a non-REPL source
    const externalUrl = 'scheme://app/test.scm';
    const exprs = parse('(+ 1 2)', { filename: externalUrl });
    registry.register(externalUrl, '(+ 1 2)', 'external', exprs);

    // Register REPL sources past the limit
    for (let i = 0; i < 5; i++) {
      devtools.registerReplEval(`(+ ${i} 1)`);
    }

    // External source should not be pruned
    assert(logger, 'external source not pruned by REPL LRU',
      registry.hasSource(externalUrl), true);
  }
}

// =========================================================================
// 7.3: Library Source Registration Tests
// =========================================================================

/**
 * @param {Object} logger - Test logger
 */
export function runLibraryRegistrationTests(logger) {
  logger.title('Library Source Registration');

  // Test: registerLibrarySource registers a library source
  {
    const { devtools, registry } = createTestSetup();
    const code = '(define-library (scheme test)\n  (export foo)\n  (begin (define foo 42)))';
    const libUrl = 'scheme://lib/scheme/test.sld';

    devtools.registerLibrarySource(libUrl, code);

    assert(logger, 'library source is registered',
      registry.hasSource(libUrl), true);

    const info = registry.getSourceInfo(libUrl);
    assert(logger, 'library source has correct content', info.content, code);
    assert(logger, 'library source has origin library', info.origin, 'library');
  }

  // Test: registerLibrarySource generates probes
  {
    const { devtools, registry } = createTestSetup();
    const code = '(define foo 42)\n(define bar (+ foo 1))';
    const libUrl = 'scheme://lib/scheme/base.scm';

    devtools.registerLibrarySource(libUrl, code);

    const exprId = registry.getExprId(libUrl, 1, 1);
    assert(logger, 'expression ID assigned for library expression',
      exprId !== null, true);
  }

  // Test: Library URL convention
  {
    const { devtools } = createTestSetup();

    const url = devtools.libraryNameToUrl(['scheme', 'base']);
    assert(logger, 'library URL follows convention',
      url, 'scheme://lib/scheme/base.sld');

    const url2 = devtools.libraryNameToUrl(['scheme-js', 'interop']);
    assert(logger, 'scheme-js library URL',
      url2, 'scheme://lib/scheme-js/interop.sld');
  }

  // Test: Library registration is idempotent (re-register same URL)
  {
    const { devtools, registry } = createTestSetup();
    const code1 = '(define x 1)';
    const code2 = '(define x 2)\n(define y 3)';
    const libUrl = 'scheme://lib/scheme/test.sld';

    devtools.registerLibrarySource(libUrl, code1);
    devtools.registerLibrarySource(libUrl, code2);

    const info = registry.getSourceInfo(libUrl);
    assert(logger, 're-registered library has updated content', info.content, code2);
    assert(logger, 're-registered library has updated line count', info.lines, 2);
  }

  // Test: Library sources are not subject to REPL LRU pruning
  {
    const { devtools, registry } = createTestSetup();
    registry.replSourceLimit = 2;

    const libUrl = 'scheme://lib/scheme/test.sld';
    devtools.registerLibrarySource(libUrl, '(define x 1)');

    // Register several REPL sources to trigger pruning
    for (let i = 0; i < 5; i++) {
      devtools.registerReplEval(`(+ ${i} 1)`);
    }

    assert(logger, 'library source survives REPL LRU pruning',
      registry.hasSource(libUrl), true);
  }
}
