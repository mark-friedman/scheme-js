/**
 * @fileoverview Probe script generator for Chrome DevTools integration.
 *
 * Generates JavaScript probe scripts for each Scheme source file. Each probe
 * script contains one function per Scheme source expression, source-mapped back to
 * the original Scheme code. When the interpreter's trampoline reaches a new
 * Scheme expression, it calls the corresponding probe function, making Chrome DevTools
 * believe real JS execution is happening at that Scheme expression.
 */

import { generateProbeSourceMap } from './sourcemap_generator.js';

/**
 * Converts a Scheme source URL to a probe script URL.
 *
 * @param {string} schemeUrl - The canonical Scheme source URL (e.g., 'scheme://app/factorial.scm')
 * @returns {string} The probe script URL (e.g., 'scheme-probe://app/factorial.scm.probe.js')
 */
export function schemeUrlToProbeUrl(schemeUrl) {
  // Replace 'scheme://' prefix with 'scheme-probe://' and append '.probe.js'
  return schemeUrl.replace(/^scheme:\/\//, 'scheme-probe://') + '.probe.js';
}

/**
 * Generates a probe script for a Scheme source file.
 *
 * The probe script contains:
 * - An IIFE that creates a probes object
 * - One function per Scheme source expression using `with(envProxy)` and `hit(exprId)`
 * - Re-wrapping to inject the registry/runtime into scope safely
 * - Registration of the probes object with the global probe registry
 * - A sourceURL comment for DevTools identification
 * - An inline source map (base64 data URL) mapping probes to Scheme expressions
 *
 * @param {string} schemeUrl - Canonical URL for the Scheme source (e.g., 'scheme://app/factorial.scm')
 * @param {string} content - The Scheme source code
 * @param {Array<{exprId: number, line: number, column: number, text?: string}>} expressions - The expressions array
 * @param {number} [lineOffset=0] - Offset for inline scripts
 * @returns {{probeScript: string, probeUrl: string}} The generated probe script JS and its URL
 */
export function generateProbeScript(schemeUrl, content, expressions, lineOffset = 0) {
  const probeUrl = schemeUrlToProbeUrl(schemeUrl);

  // Generate the source map using the expressions
  const sourceMapBase64 = generateProbeSourceMap(schemeUrl, probeUrl, content, expressions, lineOffset);

  // Build the probe script
  const parts = [];

  // Header: IIFE wrapper
  parts.push('(function(__schemeProbeRegistry, __schemeProbeRuntime) {');
  parts.push('  const probes = {};');
  parts.push('');

  // One probe function per Scheme source expression
  for (const expr of expressions) {
    // We use a multi-line function so DevTools maps breakpoints to its execution, not creation.
    // The function name __scheme_E(id) is required for background.js to identify it as a probe pause.
    // The `debugger;` fires HERE (inside the probe), not inside hit(), so that V8 pauses in
    // the source-mapped probe function and DevTools shows Scheme code, not probe_runtime.js.
    parts.push(`  probes[${expr.exprId}] = function __scheme_E${expr.exprId}(envProxy) {`);
    parts.push(`    with (envProxy) { if (__schemeProbeRuntime.hit(${expr.exprId})) { debugger; } }`);
    parts.push(`  };`);
  }

  // Footer: register probes and close IIFE
  parts.push('');
  parts.push(`  __schemeProbeRegistry.set("${schemeUrl}", probes);`);
  parts.push('})(globalThis.__schemeProbeRegistry, globalThis.__schemeProbeRuntime);');
  parts.push('');

  // Source URL and source map
  parts.push(`//# sourceURL=${probeUrl}`);
  parts.push(`//# sourceMappingURL=data:application/json;base64,${sourceMapBase64}`);

  return {
    probeScript: parts.join('\n'),
    probeUrl
  };
}
