/**
 * @fileoverview SchemeSourceRegistry for managing loaded Scheme sources
 * and their generated probe scripts.
 *
 * This registry is the central coordination point for the Chrome DevTools
 * integration's source and probe management. It:
 * - Stores Scheme source content and metadata
 * - Extracts expression spans from parsed AST
 * - Generates and caches probe scripts for each source
 * - Provides O(1) probe function lookup by URL and exprId
 * - Maps source locations to expression IDs
 * - Handles probe script injection into the browser or evaluation in Node.js
 */

import { generateProbeScript } from './probe_generator.js';
import './probe_runtime.js';

/**
 * Manages loaded Scheme sources and their probe scripts.
 */
export class SchemeSourceRegistry {
  constructor() {
    /**
     * Map of scheme URL → source metadata.
     * @type {Map<string, {content: string, lines: number, origin: string}>}
     */
    this.sources = new Map();

    /**
     * Map of scheme URL → Map of exprId → probe function.
     * @type {Map<string, Object<number, Function>>}
     */
    this.probes = new Map();

    /**
     * Map of "filename:line:column" → exprId for fast location resolution.
     * @type {Map<string, number>}
     */
    this.locationToExprId = new Map();

    /**
     * Map of scheme URL → array of expression span objects.
     * Each span has { exprId, line, column, endLine, endColumn }.
     * Used by the panel for expression-level breakpoints and highlighting.
     * @type {Map<string, Array<{exprId: number, line: number, column: number, endLine: number, endColumn: number}>>}
     */
    this.expressionSpans = new Map();

    /**
     * Global expression ID counter.
     * @type {number}
     */
    this._nextExprId = 1;

    /**
     * Ordered list of REPL source URLs for LRU pruning.
     * Oldest entries are at the front.
     * @type {string[]}
     */
    this._replSourceQueue = [];

    /**
     * Maximum number of REPL sources to retain.
     * When exceeded, the oldest entries are pruned.
     * Set to a reasonable default; can be overridden for testing.
     * @type {number}
     */
    this.replSourceLimit = 50;
  }

  /**
   * Registers a Scheme source and generates + installs its probe script.
   *
   * @param {string} url - Canonical URL for this source (e.g., 'scheme://app/factorial.scm')
   * @param {string} content - The Scheme source code
   * @param {string} origin - Source origin: 'inline', 'external', or 'repl'
   * @param {Array<*>} expressions - Parsed S-expressions with source info
   */
  register(url, content, origin, expressions) {
    const splitLines = content.length > 0 ? content.split('\n') : [];
    if (splitLines.length > 0 && splitLines[splitLines.length - 1] === '') {
      splitLines.pop();
    }

    this.sources.set(url, { content, lines: splitLines.length, origin });

    // Extract expression spans and assign exprIds
    const spans = this._extractSourceSpans(expressions, url, splitLines);

    // Store expression spans for panel access (expression-level breakpoints/highlighting)
    this.expressionSpans.set(url, spans);

    // Clear any stale location mappings for this URL before populating new ones
    const prefix = `${url}:`;
    for (const key of this.locationToExprId.keys()) {
      if (key.startsWith(prefix)) {
        this.locationToExprId.delete(key);
      }
    }

    // Populate translation map
    for (const span of spans) {
      const key = `${span.filename}:${span.line}:${span.column}`;
      this.locationToExprId.set(key, span.exprId);
    }

    // Generate the probe script for these expressions
    const { probeScript } = generateProbeScript(url, content, spans);

    // Install the probe script and capture the registered probes
    this._installProbeScript(url, probeScript);

    // Track REPL sources for LRU pruning
    if (origin === 'repl') {
      this._replSourceQueue.push(url);
      this._pruneReplSources();
    }
  }

  /**
   * Registers a non-Scheme page source (HTML document or JS file) for display
   * in the DevTools sources pane. No probes or expression spans are generated.
   *
   * @param {string} url - Canonical URL for this source (e.g. 'http://localhost/index.html')
   * @param {string} content - The source content
   * @param {string} origin - Source origin: 'page' for HTML, 'js' for JS files
   */
  registerPageSource(url, content, origin) {
    const splitLines = content.length > 0 ? content.split('\n') : [];
    if (splitLines.length > 0 && splitLines[splitLines.length - 1] === '') {
      splitLines.pop();
    }
    this.sources.set(url, { content, lines: splitLines.length, origin });
    // No expression spans, no probes — just metadata for panel display
  }

  /**
   * Prunes the oldest REPL sources when the queue exceeds the limit.
   * Removes source metadata, probes, and location mappings for pruned entries.
   * Non-REPL sources are never affected.
   *
   * @private
   */
  _pruneReplSources() {
    while (this._replSourceQueue.length > this.replSourceLimit) {
      const oldUrl = this._replSourceQueue.shift();
      this.sources.delete(oldUrl);
      this.probes.delete(oldUrl);
      this.expressionSpans.delete(oldUrl);

      // Remove location mappings for this URL
      const prefix = `${oldUrl}:`;
      for (const key of this.locationToExprId.keys()) {
        if (key.startsWith(prefix)) {
          this.locationToExprId.delete(key);
        }
      }
    }
  }

  /**
   * Walks the parsed S-expressions to extract all unique source locations.
   * Assigns a global exprId to each unique location.
   *
   * @param {Array<*>} expressions - Parsed S-expressions
   * @param {string} defaultUrl - The URL to filter source locations for
   * @param {string[]} splitLines - The source content split by lines (for snippet extraction)
   * @returns {Array<{exprId: number, filename: string, line: number, column: number, endLine: number, endColumn: number, text: string}>}
   * @private
   */
  _extractSourceSpans(expressions, defaultUrl, splitLines) {
    const spansMap = new Map(); // key -> span object

    const walk = (expr) => {
      if (!expr || typeof expr !== 'object') return;

      if (expr.source && expr.source.line !== undefined) {
        // Only collect spans for the file we are registering
        if (expr.source.filename === defaultUrl) {
          const key = `${expr.source.filename}:${expr.source.line}:${expr.source.column}`;
          if (!spansMap.has(key)) {
            // Extract a snippet of text for readability in generated comments
            let textSnippet = '';
            if (expr.source.line > 0 && expr.source.line <= splitLines.length) {
              const lineStr = splitLines[expr.source.line - 1];
              textSnippet = lineStr.substring(expr.source.column - 1, expr.source.column - 1 + 80);
            }

            spansMap.set(key, {
              exprId: this._nextExprId++,
              filename: expr.source.filename,
              line: expr.source.line,
              column: expr.source.column,
              endLine: expr.source.endLine,
              endColumn: expr.source.endColumn,
              text: textSnippet
            });
          }
        }
      }

      // Traverse Cons pairs
      if (typeof expr.car !== 'undefined' && typeof expr.cdr !== 'undefined') {
        walk(expr.car);
        walk(expr.cdr);
      } else if (Array.isArray(expr)) {
        // Traverse arrays (vectors or the top-level expressions array)
        for (const child of expr) {
          walk(child);
        }
      }
    };

    walk(expressions);
    return Array.from(spansMap.values());
  }

  /**
   * Checks if a source URL is registered.
   *
   * @param {string} url - Source URL to check
   * @returns {boolean} True if the source is registered
   */
  hasSource(url) {
    return this.sources.has(url);
  }

  /**
   * Gets the source metadata for a URL.
   *
   * @param {string} url - Source URL
   * @returns {{content: string, lines: number, origin: string}|null} Source info, or null
   */
  getSourceInfo(url) {
    return this.sources.get(url) || null;
  }

  /**
   * Resolves a source location to an expression ID.
   * Fallback logic for nearest enclosing span will be added here in the future
   * if exact column matches aren't found.
   *
   * @param {string} filename
   * @param {number} line
   * @param {number} column
   * @returns {number|null} The exprId or null
   */
  getExprId(filename, line, column) {
    const key = `${filename}:${line}:${column}`;
    return this.locationToExprId.get(key) || null;
  }

  /**
   * Gets the probe function for a specific exprId in a file.
   *
   * @param {string} url - Source URL
   * @param {number} exprId - Expression ID
   * @returns {Function|null} The probe function, or null if not found
   */
  getProbe(url, exprId) {
    const probes = this.probes.get(url);
    if (!probes) return null;
    return probes[exprId] || null;
  }

  /**
   * Gets the expression spans for a source URL.
   * Each span describes a single AST expression with its location range.
   *
   * @param {string} url - Source URL
   * @returns {Array<{exprId: number, line: number, column: number, endLine: number, endColumn: number}>}
   */
  getExpressions(url) {
    return this.expressionSpans.get(url) || [];
  }

  /**
   * Gets all registered source URLs with their metadata.
   *
   * @returns {Array<{url: string, content: string, lines: number, origin: string}>}
   */
  getAllSources() {
    const result = [];
    for (const [url, info] of this.sources) {
      result.push({ url, ...info });
    }
    return result;
  }

  /**
   * Installs a probe script by evaluating it, making the probe functions
   * available for lookup.
   *
   * @param {string} url - The Scheme source URL (used as registry key)
   * @param {string} probeScript - The generated probe script JS
   * @private
   */
  _installProbeScript(url, probeScript) {
    // Ensure the global probe registry exists
    if (!globalThis.__schemeProbeRegistry) {
      globalThis.__schemeProbeRegistry = new Map();
    }

    // Evaluate the probe script
    try {
      if (typeof document !== 'undefined' && document.head) {
        // Browser: inject as <script> tag for proper source map support
        const script = document.createElement('script');
        script.textContent = probeScript;
        document.head.appendChild(script);
        document.head.removeChild(script);
      } else {
        // Node.js: use indirect eval
        (0, eval)(probeScript);
      }
    } catch (e) {
      console.warn(`Failed to install probe script for ${url}:`, e);
    }

    // Capture the probes that were registered by the script
    const registeredProbes = globalThis.__schemeProbeRegistry.get(url);
    if (registeredProbes) {
      this.probes.set(url, registeredProbes);
    }
  }
}
