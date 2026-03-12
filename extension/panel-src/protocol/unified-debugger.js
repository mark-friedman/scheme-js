/**
 * @fileoverview Unified Debugger — routes debugging commands to the correct
 * bridge (Scheme or CDP) based on file type and current pause context.
 *
 * Provides a single API for the panel to use regardless of whether the
 * current debugging session involves Scheme, JavaScript, or both.
 * Handles call stack merging for mixed Scheme/JS debugging sessions.
 */

import * as schemeBridge from './scheme-bridge.js';
import * as cdpBridge from './cdp-bridge.js';

// =========================================================================
// Pause Context Tracking
// =========================================================================

/**
 * Current pause context — which bridge is handling the active pause.
 * @type {'scheme'|'js'|null}
 */
let currentPauseContext = null;

/**
 * Stored CDP call frames from the most recent CDP pause event.
 * @type {Array<Object>}
 */
let cdpCallFrames = [];

/**
 * Stored Scheme call frames from the most recent pause event.
 * @type {Array<Object>}
 */
let schemeCallFrames = [];

/**
 * Combined unified callbacks for pause/resume events.
 * @type {{paused: Function[], resumed: Function[]}}
 */
const unifiedListeners = {
  paused: [],
  resumed: [],
};

// =========================================================================
// File Type Detection
// =========================================================================

/**
 * Determines whether a URL refers to a JavaScript file.
 * Returns true for `.js`, `.mjs`, `.ts` extensions and non-scheme:// URLs.
 * Returns false for `scheme://` URLs and `.scm`/`.sld` extensions.
 *
 * @param {string} url - The source URL to check
 * @returns {boolean} True if the URL is a JS file
 */
export function isJSFile(url) {
  if (!url) return false;
  if (url.startsWith('scheme://')) return false;
  // Check for known Scheme extensions
  const lower = url.toLowerCase();
  if (lower.endsWith('.scm') || lower.endsWith('.sld') || lower.endsWith('.ss')) return false;
  // HTML files are mixed, so they aren't strictly JS files.
  // We handle them specifically in setBreakpoint.
  if (lower.endsWith('.html') || lower.endsWith('.htm')) return false;
  // Check for JS extensions
  if (lower.endsWith('.js') || lower.endsWith('.mjs') || lower.endsWith('.ts') ||
      lower.endsWith('.jsx') || lower.endsWith('.tsx')) return true;
  // For URLs without clear extension (e.g., inline scripts), assume JS if not scheme://
  return !url.startsWith('scheme://');
}

/**
 * Checks if a specific line in an HTML file contains Scheme code.
 * @param {string} url - Source URL
 * @param {number} line - 1-indexed line number
 * @returns {Promise<boolean>}
 */
async function isSchemeLine(url, line) {
  const expressions = await schemeBridge.getExpressions(url);
  // If any expression spans this line, it's considered Scheme code
  for (const expr of expressions) {
    if (line >= expr.line && line <= expr.endLine) {
      return true;
    }
  }
  return false;
}

// =========================================================================
// Pause Context
// =========================================================================

/**
 * Gets the current pause context.
 * @returns {'scheme'|'js'|null}
 */
export function getPauseContext() {
  return currentPauseContext;
}

/**
 * Gets the stored unified call frames from the most recent pause.
 * @returns {Array<Object>}
 */
export function getUnifiedFrames() {
  return mergeCallStacks(schemeCallFrames, cdpCallFrames);
}

// =========================================================================
// Call Stack Merging
// =========================================================================

/**
 * Merges Scheme and CDP call frames into a unified, interleaved stack.
 *
 * The unified stack presents frames in top-to-bottom order (most recent first).
 * Each frame is annotated with `language: 'scheme'|'js'` for rendering.
 *
 * Interleaving strategy:
 * - When CDP is attached and we have both frame types, JS frames appear
 *   above Scheme frames at the boundary point (the interpreter trampoline).
 * - Pure-Scheme pauses: only Scheme frames are returned.
 * - Pure-JS pauses: only JS frames are returned.
 * - Mixed pauses: JS frames (from CDP) are placed on top, with Scheme frames
 *   below, filtered to exclude internal interpreter frames.
 *
 * @param {Array<Object>} schemeFrames - Scheme stack from __schemeDebug.getStack()
 * @param {Array<Object>} jsFrames - CDP call frames from Debugger.paused
 * @returns {Array<Object>} Unified frame array
 */
export function mergeCallStacks(schemeFrames, jsFrames) {
  const unified = [];

  if (jsFrames && jsFrames.length > 0) {
    // Add JS frames first (top of stack), filtering out internal chrome/extension frames
    for (const frame of jsFrames) {
      const url = frame.url || '';
      // Skip internal/extension frames
      if (url.startsWith('chrome-extension://')) continue;
      if (url.startsWith('scheme-probe://')) continue;
      // Skip Scheme interpreter internals (they appear as eval/trampoline frames)
      if (url.includes('/interpreter/') || url.includes('/debug/')) continue;

      unified.push({
        name: frame.functionName || '(anonymous)',
        source: {
          filename: url || '(inline)',
          line: (frame.location?.lineNumber ?? 0) + 1, // CDP is 0-indexed
          column: (frame.location?.columnNumber ?? 0) + 1,
        },
        tcoCount: 0,
        language: 'js',
        // Preserve CDP-specific data for fetching source/locals
        _cdpCallFrameId: frame.callFrameId,
        _cdpScriptId: frame.location?.scriptId,
        _cdpScopeChain: frame.scopeChain,
      });
    }
  }

  if (schemeFrames && schemeFrames.length > 0) {
    // Add Scheme frames below JS frames (or as the full stack if no JS frames)
    // Scheme stack is already in top-to-bottom order from getStack()
    for (const frame of schemeFrames) {
      unified.push({
        name: frame.name || '(anonymous)',
        source: frame.source || null,
        tcoCount: frame.tcoCount || 0,
        language: 'scheme',
      });
    }
  }

  return unified;
}

// =========================================================================
// Unified Resume / Step Commands
// =========================================================================

/**
 * Resumes execution using the appropriate bridge for the current pause context.
 * @returns {Promise<void>}
 */
export async function resume() {
  if (currentPauseContext === 'js') {
    await cdpBridge.resumeCDP();
  } else {
    await schemeBridge.resume();
  }
}

/**
 * Steps into using the appropriate bridge.
 * @returns {Promise<void>}
 */
export async function stepInto() {
  if (currentPauseContext === 'js') {
    await cdpBridge.stepIntoCDP();
  } else {
    await schemeBridge.stepInto();
  }
}

/**
 * Steps over using the appropriate bridge.
 * @returns {Promise<void>}
 */
export async function stepOver() {
  if (currentPauseContext === 'js') {
    await cdpBridge.stepOverCDP();
  } else {
    await schemeBridge.stepOver();
  }
}

/**
 * Steps out using the appropriate bridge.
 * @returns {Promise<void>}
 */
export async function stepOut() {
  if (currentPauseContext === 'js') {
    await cdpBridge.stepOutCDP();
  } else {
    await schemeBridge.stepOut();
  }
}

// =========================================================================
// Unified Breakpoint Commands
// =========================================================================

/**
 * Sets a breakpoint at the given location, routing to the correct bridge
 * based on whether the URL is a JS or Scheme file.
 *
 * @param {string} url - Source URL
 * @param {number} line - 1-indexed line number
 * @param {number|null} [column=null] - 1-indexed column, or null for line-level
 * @returns {Promise<string|null>} Breakpoint ID
 */
export async function setBreakpoint(url, line, column = null) {
  const lower = url.toLowerCase();
  // Handle mixed HTML files by analyzing the requested line
  if (lower.endsWith('.html') || lower.endsWith('.htm')) {
    const isScheme = await isSchemeLine(url, line);
    if (isScheme) {
      return schemeBridge.setBreakpoint(url, line, column);
    } else {
      return cdpBridge.setJSBreakpoint(url, line - 1);
    }
  }

  if (isJSFile(url)) {
    // CDP uses 0-indexed line numbers
    return cdpBridge.setJSBreakpoint(url, line - 1);
  } else {
    return schemeBridge.setBreakpoint(url, line, column);
  }
}

/**
 * Removes a breakpoint, routing to the correct bridge.
 *
 * @param {string} id - Breakpoint ID
 * @param {string} url - Source URL (used to determine which bridge to use)
 * @returns {Promise<boolean>} True if removed
 */
export async function removeBreakpoint(id, url) {
  const lower = url.toLowerCase();
  if (lower.endsWith('.html') || lower.endsWith('.htm')) {
    // Breakpoint IDs format differ between Scheme ("<url>:<line>:<col>") and CDP (opaque strings).
    // Try Scheme first; if it isn't found/removed, try CDP.
    const removedScheme = await schemeBridge.removeBreakpoint(id);
    if (removedScheme) return true;
    return cdpBridge.removeJSBreakpoint(id);
  } else if (isJSFile(url)) {
    return cdpBridge.removeJSBreakpoint(id);
  } else {
    return schemeBridge.removeBreakpoint(id);
  }
}

// =========================================================================
// Unified Eval Command
// =========================================================================

/**
 * Evaluates an expression within a specific call frame context.
 * Routes to CDP or Scheme bridge based on the frame's language.
 *
 * @param {Object} frame - A unified stack frame object (from getUnifiedFrames/onPaused)
 * @param {number} schemeFrameIndex - The index of this frame within the Scheme-only stack
 * @param {string} expression - The expression to evaluate
 * @returns {Promise<{success: boolean, result: any, error: string|null}>}
 */
export async function evalInFrame(frame, schemeFrameIndex, expression) {
  if (frame.language === 'js') {
    if (frame._cdpCallFrameId) {
      return cdpBridge.evalInJSFrame(frame._cdpCallFrameId, expression);
    }
    return { success: false, result: null, error: 'No CDP call frame ID available' };
  } else {
    return schemeBridge.evalInFrame(expression, schemeFrameIndex);
  }
}

// =========================================================================
// Unified Event Listeners
// =========================================================================

/**
 * Registers a callback for unified pause events (from either bridge).
 * @param {Function} callback - Called with { context, frames, reason, source }
 */
export function onPaused(callback) {
  unifiedListeners.paused.push(callback);
}

/**
 * Registers a callback for unified resume events.
 * @param {Function} callback - Called with no arguments
 */
export function onResumed(callback) {
  unifiedListeners.resumed.push(callback);
}

/**
 * Returns whether CDP is currently attached.
 * @returns {boolean}
 */
export function isCDPAttached() {
  return cdpBridge.isAttached();
}

// =========================================================================
// Wire up bridge events into unified stream
// =========================================================================

/**
 * Initializes the unified debugger by connecting bridge event listeners.
 * Should be called once during panel initialization.
 */
export function init() {
  // Listen for CDP pause events
  cdpBridge.onCDPPaused(async (event) => {
    currentPauseContext = 'js';
    cdpCallFrames = event.callFrames || [];

    // Try to get Scheme stack as well (for mixed debugging)
    try {
      const schemeStack = await schemeBridge.getStack();
      schemeCallFrames = schemeStack || [];
    } catch {
      schemeCallFrames = [];
    }

    const unified = mergeCallStacks(schemeCallFrames, cdpCallFrames);
    const topFrame = unified[0];

    for (const fn of unifiedListeners.paused) {
      fn({
        context: 'js',
        frames: unified,
        reason: event.reason || 'breakpoint',
        source: topFrame?.source || null,
      });
    }
  });

  // Listen for CDP resume events
  cdpBridge.onCDPResumed(() => {
    if (currentPauseContext === 'js') {
      currentPauseContext = null;
      cdpCallFrames = [];
      schemeCallFrames = [];

      for (const fn of unifiedListeners.resumed) {
        fn();
      }
    }
  });
}

/**
 * Handles a Scheme pause event from the content script relay.
 * Called by main.js when a 'scheme-debug-paused' message arrives.
 *
 * @param {Object} detail - Pause detail from the __schemeDebug API
 */
export async function handleSchemePause(detail) {
  if (detail.reason === 'boundary') {
    // 1. Attach CDP and set a native break on the boundary target
    const bpId = await cdpBridge.setBoundaryBreakpoint();
    
    // We successfully set a JS breakpoint (or failed, e.g. built-in native).
    // In either case, automatically resume the Scheme interpreter.
    // If successful, V8 will immediately pause at the new breakpoint.
    // If not, it acts as a "Step Over" since we can't step into built-ins.
    await schemeBridge.resume();
    return;
  }

  currentPauseContext = 'scheme';
  schemeCallFrames = detail.stack || [];
  cdpCallFrames = [];  // CDP frames not relevant for Scheme pauses

  const unified = mergeCallStacks(schemeCallFrames, cdpCallFrames);

  for (const fn of unifiedListeners.paused) {
    fn({
      context: 'scheme',
      frames: unified,
      reason: detail.reason || 'breakpoint',
      source: detail.source || null,
    });
  }
}

/**
 * Handles a Scheme resume event from the content script relay.
 * Called by main.js when a 'scheme-debug-resumed' message arrives.
 */
export function handleSchemeResume() {
  if (currentPauseContext === 'scheme') {
    currentPauseContext = null;
    schemeCallFrames = [];
    cdpCallFrames = [];

    for (const fn of unifiedListeners.resumed) {
      fn();
    }
  }
}

/**
 * Resets the unified debugger state. Used during panel refresh.
 */
export function reset() {
  currentPauseContext = null;
  cdpCallFrames = [];
  schemeCallFrames = [];
}
