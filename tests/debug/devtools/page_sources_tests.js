/**
 * @fileoverview Unit tests for page source registration in SchemeSourceRegistry.
 *
 * Verifies that registerPageSource() stores HTML/JS sources without generating
 * probes/spans, and that getAllSources() returns both Scheme and page sources.
 */

import { assert } from '../../harness/helpers.js';
import { SchemeSourceRegistry } from '../../../src/debug/devtools/source_registry.js';
import { parse } from '../../../src/core/interpreter/reader.js';

/**
 * Runs page sources unit tests.
 * @param {Object} logger
 */
export function runPageSourcesTests(logger) {
  // =========================================================================
  // registerPageSource stores HTML source
  // =========================================================================

  logger.title('Page Sources - HTML source registration');

  {
    const registry = new SchemeSourceRegistry();
    const url = 'http://localhost:8080/index.html';
    const content = '<!DOCTYPE html><html><body></body></html>';

    registry.registerPageSource(url, content, 'page');

    assert(logger, 'HTML source is registered', registry.hasSource(url), true);
    const info = registry.getSourceInfo(url);
    assert(logger, 'HTML source content matches', info.content, content);
    assert(logger, 'HTML source origin is page', info.origin, 'page');
  }

  // =========================================================================
  // registerPageSource stores JS source
  // =========================================================================

  logger.title('Page Sources - JS source registration');

  {
    const registry = new SchemeSourceRegistry();
    const url = 'http://localhost:8080/app.js';
    const content = 'function hello() { return 42; }';

    registry.registerPageSource(url, content, 'js');

    assert(logger, 'JS source is registered', registry.hasSource(url), true);
    const info = registry.getSourceInfo(url);
    assert(logger, 'JS source content matches', info.content, content);
    assert(logger, 'JS source origin is js', info.origin, 'js');
  }

  // =========================================================================
  // registerPageSource does NOT generate probes
  // =========================================================================

  logger.title('Page Sources - no probes generated for page sources');

  {
    const registry = new SchemeSourceRegistry();
    const url = 'http://localhost:8080/index.html';
    registry.registerPageSource(url, '<html></html>', 'page');

    // No expression spans should be generated
    const spans = registry.getExpressions(url);
    assert(logger, 'page source has no expression spans', spans.length, 0);

    // No probes should be registered
    const probe = registry.getProbe(url, 1);
    assert(logger, 'page source has no probes', probe, null);
  }

  // =========================================================================
  // getAllSources returns both Scheme and page sources
  // =========================================================================

  logger.title('Page Sources - getAllSources returns Scheme + page sources');

  {
    const registry = new SchemeSourceRegistry();

    // Register a Scheme source
    const schemeUrl = 'scheme://app/test.scm';
    const schemeExprs = parse('(define x 1)', { filename: schemeUrl });
    registry.register(schemeUrl, '(define x 1)', 'external', schemeExprs);

    // Register an HTML page source
    const htmlUrl = 'http://localhost:8080/index.html';
    registry.registerPageSource(htmlUrl, '<html></html>', 'page');

    const all = registry.getAllSources();
    const urls = all.map(s => s.url);

    assert(logger, 'getAllSources includes Scheme source', urls.includes(schemeUrl), true);
    assert(logger, 'getAllSources includes page source', urls.includes(htmlUrl), true);
    assert(logger, 'getAllSources returns 2 sources', all.length, 2);
  }

  // =========================================================================
  // __schemeDebug getSources() returns page sources via API
  // =========================================================================

  logger.title('Page Sources - getSources API includes page sources');

  {
    // Save and replace global __schemeDebug if it exists
    const savedDebug = globalThis.__schemeDebug;

    // Directly test the registry's getAllSources which backs the API
    const registry = new SchemeSourceRegistry();
    const htmlUrl = 'http://localhost:8080/test.html';
    const jsUrl = 'http://localhost:8080/test.js';

    registry.registerPageSource(htmlUrl, '<!DOCTYPE html>', 'page');
    registry.registerPageSource(jsUrl, 'const x = 1;', 'js');

    const sources = registry.getAllSources();
    const htmlSrc = sources.find(s => s.url === htmlUrl);
    const jsSrc = sources.find(s => s.url === jsUrl);

    assert(logger, 'HTML source appears in getAllSources', htmlSrc !== undefined, true);
    assert(logger, 'JS source appears in getAllSources', jsSrc !== undefined, true);
    assert(logger, 'HTML source has correct origin', htmlSrc?.origin, 'page');
    assert(logger, 'JS source has correct origin', jsSrc?.origin, 'js');

    if (savedDebug !== undefined) globalThis.__schemeDebug = savedDebug;
  }
}
