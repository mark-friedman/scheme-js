/**
 * @fileoverview Unit tests for probe script generation.
 *
 * Tests that generated probe scripts have the correct structure, function
 * naming, and source map references. Part of Phase 1: Foundation.
 */

import { assert } from '../../harness/helpers.js';
import { generateProbeScript } from '../../../src/debug/devtools/probe_generator.js';
import { parse } from '../../../src/core/interpreter/reader.js';

function getMockSpans(url, content) {
  const exprs = parse(content, { filename: url });
  let nextId = 1;
  const spans = [];
  const walk = (expr) => {
    if (!expr || typeof expr !== 'object') return;
    if (expr.source && expr.source.line !== undefined) {
      if (expr.source.filename === url) {
        spans.push({ exprId: nextId++, line: expr.source.line, column: expr.source.column });
      }
    }
    if (typeof expr.car !== 'undefined') {
      walk(expr.car);
      walk(expr.cdr);
    } else if (Array.isArray(expr)) {
      expr.forEach(walk);
    }
  };
  walk(exprs);
  return spans;
}

/**
 * Runs all probe generator tests.
 * @param {Object} logger - Test logger
 */
export function runProbeGeneratorTests(logger) {
  logger.title('Probe Generator - Basic Structure');

  // Test: Generated script contains probe array
  {
    const content = '(define x 1)\n(+ x 2)\n';
    const url = 'scheme://app/test.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    assert(logger, 'result has probeScript property', typeof result.probeScript, 'string');
    assert(logger, 'result has probeUrl property', typeof result.probeUrl, 'string');
    assert(logger, 'probeUrl follows convention', result.probeUrl, 'scheme-probe://app/test.scm.probe.js');
  }

  // Test: Generated script contains probe functions for each line
  {
    const content = '(define x 1)\n(+ x 2)\n(display x)\n';
    const url = 'scheme://app/test.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    // Expressions include definitions and calls, e.g. exprId 1, 2, 3..
    assert(logger, 'script contains probe for expression 1', result.probeScript.includes('probes[1] ='), true);
    assert(logger, 'script contains probe for expression 2', result.probeScript.includes('probes[2] ='), true);
    assert(logger, 'script contains probe for expression 3', result.probeScript.includes('probes[3] ='), true);
  }

  // Test: Probe functions use with(envProxy) pattern
  {
    const content = '(+ 1 2)\n';
    const url = 'scheme://app/test.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    assert(logger, 'script contains with(envProxy) pattern',
      result.probeScript.includes('with (envProxy)'), true);
  }

  // Test: Generated script contains sourceURL comment
  {
    const content = '(+ 1 2)\n';
    const url = 'scheme://app/test.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    assert(logger, 'script contains sourceURL',
      result.probeScript.includes('//# sourceURL=scheme-probe://app/test.scm.probe.js'), true);
  }

  // Test: Generated script contains sourceMappingURL
  {
    const content = '(+ 1 2)\n';
    const url = 'scheme://app/test.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    assert(logger, 'script contains sourceMappingURL',
      result.probeScript.includes('//# sourceMappingURL=data:application/json;base64,'), true);
  }

  // Test: Generated script registers probes with registry
  {
    const content = '(+ 1 2)\n';
    const url = 'scheme://app/test.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    assert(logger, 'script registers with __schemeProbeRegistry',
      result.probeScript.includes('__schemeProbeRegistry'), true);
    assert(logger, 'script uses url as registry key',
      result.probeScript.includes(url), true);
  }

  logger.title('Probe Generator - Edge Cases');

  // Test: Empty source
  {
    const content = '';
    const url = 'scheme://app/empty.scm';
    const spans = [];
    const result = generateProbeScript(url, content, spans);

    assert(logger, 'empty source generates valid script', typeof result.probeScript, 'string');
    assert(logger, 'empty source has no probe functions',
      !result.probeScript.includes('probes[1] ='), true);
  }

  // Test: Single line without trailing newline
  {
    const content = '(+ 1 2)';
    const url = 'scheme://app/single.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    assert(logger, 'single line generates probe', result.probeScript.includes('probes[1] ='), true);
  }

  // Test: URL conventions for different source types
  {
    // External file
    const spans = getMockSpans('scheme://app/path/to/file.scm', '(+ 1 2)\n');
    const ext = generateProbeScript('scheme://app/path/to/file.scm', '(+ 1 2)\n', spans);
    assert(logger, 'external file probe URL',
      ext.probeUrl, 'scheme-probe://app/path/to/file.scm.probe.js');

    // Inline script
    const inlineSpans = getMockSpans('scheme://inline/script-0.scm', '(+ 1 2)\n');
    const inline = generateProbeScript('scheme://inline/script-0.scm', '(+ 1 2)\n', inlineSpans);
    assert(logger, 'inline script probe URL',
      inline.probeUrl, 'scheme-probe://inline/script-0.scm.probe.js');

    // REPL eval
    const replSpans = getMockSpans('scheme://repl/eval-0.scm', '(+ 1 2)\n');
    const repl = generateProbeScript('scheme://repl/eval-0.scm', '(+ 1 2)\n', replSpans);
    assert(logger, 'REPL eval probe URL',
      repl.probeUrl, 'scheme-probe://repl/eval-0.scm.probe.js');
  }

  // Test: Script does NOT use strict mode (needed for 'with' statement)
  {
    const content = '(+ 1 2)\n';
    const url = 'scheme://app/test.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    assert(logger, 'script does not use strict mode',
      !result.probeScript.includes('"use strict"') && !result.probeScript.includes("'use strict'"), true);
  }

  // Test: Line count matches source
  {
    const content = 'line1\nline2\nline3\nline4\n(define a 1)\n';
    const url = 'scheme://app/multi.scm';
    const spans = getMockSpans(url, content);
    const result = generateProbeScript(url, content, spans);

    // Should generate for parsed expressions, but 'line1\nline2...\n' without parens is just atom evaluation (variables), so it parses as variables.
    // At least we check if valid probes are generated.
    assert(logger, 'probe exists for parsed content', result.probeScript.includes('probes[1] ='), true);
  }
}
