/**
 * @fileoverview Breakpoint state management for the Scheme-JS DevTools panel.
 *
 * Encapsulates the breakpointIds map and all key-related logic using
 * structured JSON keys instead of fragile string concatenation with
 * colon delimiters (which collide with colons in URLs).
 */

import { evalInPage } from './protocol/scheme-bridge.js';

// =========================================================================
// Internal state
// =========================================================================

/**
 * Maps structured JSON key -> breakpoint ID (from interpreter) for removal.
 * Key format: JSON.stringify([url, line, column]) where column is null
 * for line-level breakpoints.
 * @type {Map<string, string>}
 */
const breakpointIds = new Map();

// =========================================================================
// Key helpers
// =========================================================================

/**
 * Creates a structured breakpoint key from components.
 *
 * @param {string} url - Source URL
 * @param {number} line - 1-indexed line number
 * @param {number|null} column - Column number or null for line-level
 * @returns {string} JSON-stringified key
 */
export function bpKey(url, line, column = null) {
  return JSON.stringify([url, line, column]);
}

/**
 * Parses a structured breakpoint key back into components.
 *
 * @param {string} key - JSON-stringified key from bpKey()
 * @returns {{url: string, line: number, column: number|null}}
 */
export function parseBpKey(key) {
  const [url, line, column] = JSON.parse(key);
  return { url, line, column };
}

// =========================================================================
// Accessors
// =========================================================================

/**
 * Returns true if a breakpoint exists at the given location.
 *
 * @param {string} url
 * @param {number} line
 * @param {number|null} [column=null]
 * @returns {boolean}
 */
export function has(url, line, column = null) {
  return breakpointIds.has(bpKey(url, line, column));
}

/**
 * Gets the breakpoint ID at the given location.
 *
 * @param {string} url
 * @param {number} line
 * @param {number|null} [column=null]
 * @returns {string|undefined}
 */
export function getId(url, line, column = null) {
  return breakpointIds.get(bpKey(url, line, column));
}

/**
 * Stores a breakpoint ID at the given location.
 *
 * @param {string} url
 * @param {number} line
 * @param {number|null} column
 * @param {string} id - Breakpoint ID from the interpreter
 */
export function set(url, line, column, id) {
  breakpointIds.set(bpKey(url, line, column), id);
}

/**
 * Removes a breakpoint at the given location.
 *
 * @param {string} url
 * @param {number} line
 * @param {number|null} [column=null]
 */
export function remove(url, line, column = null) {
  breakpointIds.delete(bpKey(url, line, column));
}

/**
 * Removes a breakpoint by its JSON key.
 *
 * @param {string} key - JSON-stringified key from bpKey()
 */
export function removeByKey(key) {
  breakpointIds.delete(key);
}

/**
 * Checks if a breakpoint exists by its JSON key.
 *
 * @param {string} key - JSON-stringified key from bpKey()
 * @returns {boolean}
 */
export function hasByKey(key) {
  return breakpointIds.has(key);
}

/**
 * Gets the breakpoint ID by its JSON key.
 *
 * @param {string} key - JSON-stringified key from bpKey()
 * @returns {string|undefined}
 */
export function getByKey(key) {
  return breakpointIds.get(key);
}

// =========================================================================
// Queries
// =========================================================================

/**
 * Returns the set of line-level breakpoint line numbers for a specific URL.
 *
 * @param {string} url - Source URL to filter by
 * @returns {Set<number>} Line numbers with line-level breakpoints
 */
export function getLinesForUrl(url) {
  const lines = new Set();
  for (const key of breakpointIds.keys()) {
    const bp = parseBpKey(key);
    if (bp.url === url && bp.column === null) {
      lines.add(bp.line);
    }
  }
  return lines;
}

/**
 * Returns expression breakpoint spans for a URL, looked up from the
 * provided expression spans array.
 *
 * @param {string} url - Source URL
 * @param {Array<{line: number, column: number, endLine: number, endColumn: number}>} expressions
 * @returns {Array<{line: number, column: number, endLine: number, endColumn: number}>}
 */
export function getExpressionBreakpointsForUrl(url, expressions) {
  const spans = [];
  for (const key of breakpointIds.keys()) {
    const bp = parseBpKey(key);
    if (bp.url === url && bp.column !== null) {
      const span = expressions.find(
        e => e.line === bp.line && e.column === bp.column
      );
      if (span) {
        spans.push({
          line: span.line,
          column: span.column,
          endLine: span.endLine,
          endColumn: span.endColumn
        });
      }
    }
  }
  return spans;
}

/**
 * Returns all breakpoints as a flat array for the breakpoints panel.
 *
 * @returns {Array<{id: string, filename: string, line: number, column: number|null, key: string}>}
 */
export function getAllBreakpoints() {
  const bps = [];
  for (const [key, id] of breakpointIds.entries()) {
    const bp = parseBpKey(key);
    bps.push({ id, filename: bp.url, line: bp.line, column: bp.column, key });
  }
  return bps;
}

// =========================================================================
// Persistence
// =========================================================================

/**
 * Persists the current breakpoint set to the page's localStorage.
 * Uses evalInPage to write into the inspected page's storage, where
 * activate_debug.js can read it at document_start on next reload.
 */
export async function saveToPage() {
  const entries = [];
  for (const key of breakpointIds.keys()) {
    const bp = parseBpKey(key);
    const entry = { url: bp.url, line: bp.line };
    if (bp.column !== null) entry.column = bp.column;
    entries.push(entry);
  }
  try {
    await evalInPage(
      `localStorage.setItem('schemeJS_breakpoints', ${JSON.stringify(JSON.stringify(entries))})`
    );
  } catch {
    console.warn('[breakpoint-state] Failed to persist breakpoints to page localStorage');
  }
}

/**
 * Loads the breakpoint list from the interpreter's current state.
 * Called after activation to sync the panel's local map with
 * whatever breakpoints were pre-loaded by activate_debug.js.
 */
export async function syncFromInterpreter() {
  try {
    const json = await evalInPage('JSON.stringify(__schemeDebug.getAllBreakpoints())');
    const bps = JSON.parse(json);
    for (const bp of bps) {
      breakpointIds.set(bpKey(bp.filename, bp.line, bp.column), bp.id);
    }
  } catch {
    console.warn('[breakpoint-state] Failed to sync breakpoints from interpreter');
  }
}

/**
 * Clears all breakpoint state (used for testing).
 */
export function clear() {
  breakpointIds.clear();
}

/**
 * Returns the number of stored breakpoints.
 * @returns {number}
 */
export function size() {
  return breakpointIds.size;
}
