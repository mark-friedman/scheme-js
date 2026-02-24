/**
 * @fileoverview Source map V3 generator for probe scripts.
 *
 * Generates V3 source maps that map each probe function's `void 0;` statement
 * back to the corresponding Scheme source line. Uses a lightweight VLQ encoder
 * with no external dependencies.
 */

// =============================================================================
// VLQ Encoding
// =============================================================================

/** @const {number} Base for VLQ encoding (2^5) */
const VLQ_BASE = 32;

/** @const {number} Mask for extracting 5-bit chunks */
const VLQ_BASE_MASK = VLQ_BASE - 1;

/** @const {number} Continuation bit flag */
const VLQ_CONTINUATION_BIT = VLQ_BASE;

/** @const {string} Base64 character set for source map encoding */
const BASE64_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

/**
 * Encodes a single integer value using Variable-Length Quantity (VLQ) encoding
 * as specified by the source map V3 format.
 *
 * @param {number} value - The integer to encode (can be negative)
 * @returns {string} The VLQ-encoded string
 */
export function encodeVLQ(value) {
  // Convert to unsigned with sign bit in LSB
  let vlq = value < 0 ? ((-value) << 1) + 1 : (value << 1);
  let encoded = '';

  do {
    let digit = vlq & VLQ_BASE_MASK;
    vlq >>>= 5;
    if (vlq > 0) {
      digit |= VLQ_CONTINUATION_BIT;
    }
    encoded += BASE64_CHARS[digit];
  } while (vlq > 0);

  return encoded;
}

// =============================================================================
// Source Map Generation
// =============================================================================

/**
 * The number of header lines in the generated probe script before the first
 * probe entry. This must match the structure in probe_generator.js:
 *
 *   Line 1: (function(__schemeProbeRegistry, __schemeProbeRuntime) {
 *   Line 2:   const probes = {};
 *   Line 3:   (blank line)
 *   Line 4:   // Expr 1: <comment>        <- first Scheme line starts here
 *   Line 5:   probes[1] = new Function ...
 *
 * @const {number}
 */
const PROBE_HEADER_LINES = 3;

/**
 * Generates a V3 source map for a probe script.
 *
 * Each probe function in the generated JS maps to one Scheme source expression.
 * The source map ensures that when V8 pauses inside a probe, DevTools shows
 * the correct Scheme file, line, and column.
 *
 * @param {string} schemeUrl - Original Scheme source URL (e.g., 'scheme://app/factorial.scm')
 * @param {string} probeUrl - Generated probe script URL (e.g., 'scheme-probe://app/factorial.scm.probe.js')
 * @param {string} schemeContent - Original Scheme source text
 * @param {Array<{exprId: number, line: number, column: number, text?: string}>} expressions - Array of expression spans
 * @param {number} [lineOffset=0] - Offset for inline scripts
 * @returns {string} Base64-encoded source map JSON
 */
export function generateProbeSourceMap(schemeUrl, probeUrl, schemeContent, expressions, lineOffset = 0) {
  // Build mappings string
  const mappingSegments = [];

  // Empty mappings for header lines (no mapping needed)
  for (let i = 0; i < PROBE_HEADER_LINES; i++) {
    mappingSegments.push('');
  }

  // Track previous values for delta encoding
  let prevGeneratedCol = 0;
  let prevSourceIndex = 0;
  let prevOriginalLine = 0;
  let prevOriginalCol = 0;

  // Each Scheme source expression produces 3 generated JS lines:
  //   1. Function declaration line (empty segment)
  //   2. Probe function body line (mapped to the Scheme source expression's line/col)
  //   3. Function closing line (empty segment)
  for (const expr of expressions) {
    // Function declaration line — no source mapping
    mappingSegments.push('');

    // Probe function body line — map to original Scheme line and column
    const generatedCol = 4; // '    with...'
    const sourceIndex = 0; // Only one source file
    const originalLine = (expr.line - 1) + lineOffset; // 0-indexed in source map
    const originalCol = expr.column - 1;

    // Delta encode from previous values
    const segment =
      encodeVLQ(generatedCol - prevGeneratedCol) +
      encodeVLQ(sourceIndex - prevSourceIndex) +
      encodeVLQ(originalLine - prevOriginalLine) +
      encodeVLQ(originalCol - prevOriginalCol);

    mappingSegments.push(segment);

    // Update previous values (generated column resets per line)
    prevGeneratedCol = generatedCol;
    prevSourceIndex = sourceIndex;
    prevOriginalLine = originalLine;
    prevOriginalCol = originalCol;

    // Function close line — no source mapping
    mappingSegments.push('');
  }

  // Build the source map object
  const sourceMap = {
    version: 3,
    file: probeUrl,
    sources: [schemeUrl],
    sourcesContent: [schemeContent],
    names: [],
    mappings: mappingSegments.join(';')
  };

  // Encode as base64
  const json = JSON.stringify(sourceMap);

  // Use btoa if available (browser), otherwise Buffer (Node.js)
  if (typeof btoa === 'function') {
    return btoa(json);
  }
  return Buffer.from(json).toString('base64');
}
