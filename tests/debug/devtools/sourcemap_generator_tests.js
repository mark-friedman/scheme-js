/**
 * @fileoverview Unit tests for VLQ encoding and source map generation.
 *
 * Tests for the VLQ encoder and the probe source map generator used by the
 * Chrome DevTools integration. Part of Phase 1: Foundation.
 */

import { assert } from '../../harness/helpers.js';
import { encodeVLQ, generateProbeSourceMap } from '../../../src/debug/devtools/sourcemap_generator.js';
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
 * Runs all source map generator tests.
 * @param {Object} logger - Test logger
 */
export function runSourceMapGeneratorTests(logger) {
  logger.title('VLQ Encoding - Basic Values');

  // Test: VLQ encoding of 0
  {
    const result = encodeVLQ(0);
    assert(logger, 'VLQ encode 0', result, 'A');
  }

  // Test: VLQ encoding of 1
  {
    const result = encodeVLQ(1);
    assert(logger, 'VLQ encode 1', result, 'C');
  }

  // Test: VLQ encoding of -1
  {
    const result = encodeVLQ(-1);
    assert(logger, 'VLQ encode -1', result, 'D');
  }

  // Test: VLQ encoding of 5
  {
    const result = encodeVLQ(5);
    assert(logger, 'VLQ encode 5', result, 'K');
  }

  // Test: VLQ encoding of -5
  {
    const result = encodeVLQ(-5);
    assert(logger, 'VLQ encode -5', result, 'L');
  }

  // Test: VLQ encoding of larger number (16) requiring continuation
  {
    const result = encodeVLQ(16);
    // 16 << 1 = 32, first chunk: 32 & 31 = 0 | 32 = 32 -> 'g', then 1 -> 'C'
    // Actually: vlq = 32, digit = 0 | 32 = 32 -> BASE64[32] = 'g', vlq = 1, digit = 1 -> BASE64[2] = 'C'
    // Wait: vlq = 32, digit = 32 & 31 = 0, vlq >>>= 5 = 1, 1 > 0 so digit |= 32 -> 32 -> BASE64[32] = 'g'
    // then vlq = 1, digit = 1 & 31 = 1, vlq >>>= 5 = 0, no continuation -> BASE64[1] = 'B'
    // Hmm, let me recalculate
    // vlq = 16 << 1 = 32
    // digit = 32 & 0x1F = 0, vlq >>>= 5 = 1
    // vlq > 0, so digit |= 32 -> 32 -> 'g'
    // digit = 1 & 0x1F = 1, vlq >>>= 5 = 0
    // vlq == 0, no continuation -> 'B'
    // Result: 'gB'
    assert(logger, 'VLQ encode 16', result, 'gB');
  }

  logger.title('Source Map Generation - Structure');

  // Test: Source map has correct version
  {
    const schemeUrl = 'scheme://app/test.scm';
    const probeUrl = 'scheme-probe://app/test.scm.probe.js';
    const content = '(define x 1)\n(+ x 2)\n';
    const spans = getMockSpans(schemeUrl, content);

    const base64 = generateProbeSourceMap(schemeUrl, probeUrl, content, spans);
    const json = JSON.parse(atob(base64));

    assert(logger, 'source map version is 3', json.version, 3);
    assert(logger, 'source map file matches probe URL', json.file, probeUrl);
    assert(logger, 'source map has one source', json.sources.length, 1);
    assert(logger, 'source map source matches scheme URL', json.sources[0], schemeUrl);
    assert(logger, 'source map sourcesContent includes original', json.sourcesContent[0], content);
  }

  // Test: Source map mappings are non-empty
  {
    const content = '(define x 1)\n(+ x 2)\n(display x)\n';
    const schemeUrl = 'scheme://app/test.scm';
    const spans = getMockSpans(schemeUrl, content);
    const base64 = generateProbeSourceMap(
      schemeUrl,
      'scheme-probe://app/test.scm.probe.js',
      content,
      spans
    );
    const json = JSON.parse(atob(base64));

    assert(logger, 'source map mappings is non-empty string', typeof json.mappings === 'string' && json.mappings.length > 0, true);

    // Mappings should have semicolons separating generated lines
    // The probe script has a 3-line header, then 3 lines per Scheme expression
    // (function decl, function body, function close).
    const lines = json.mappings.split(';');
    assert(logger, 'source map has correct generated line count',
      lines.length, 3 + spans.length * 3);
  }

  // Test: Single line source
  {
    const content = '(+ 1 2)\n';
    const schemeUrl = 'scheme://app/one.scm';
    const spans = getMockSpans(schemeUrl, content);
    const base64 = generateProbeSourceMap(
      schemeUrl,
      'scheme-probe://app/one.scm.probe.js',
      content,
      spans
    );
    const json = JSON.parse(atob(base64));

    assert(logger, 'single line source map is valid', json.version, 3);
    assert(logger, 'single line mappings not empty', json.mappings.length > 0, true);
  }

  // Test: Empty source (0 lines)
  {
    const content = '';
    const schemeUrl = 'scheme://app/empty.scm';
    const spans = [];
    const base64 = generateProbeSourceMap(
      schemeUrl,
      'scheme-probe://app/empty.scm.probe.js',
      content,
      spans
    );
    const json = JSON.parse(atob(base64));

    assert(logger, 'empty source map is valid', json.version, 3);
  }
}
