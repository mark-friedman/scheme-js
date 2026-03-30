/**
 * @fileoverview Scheme bridge — evaluates __schemeDebug API methods in the
 * inspected page context. Supports two execution paths:
 *
 *   1. Standalone window: chrome.scripting.executeScript({world:'MAIN'})
 *   2. DevTools panel:    chrome.devtools.inspectedWindow.eval()
 *
 * The bridge auto-detects which API is available. Standalone window mode
 * reads the tabId from a URL parameter; DevTools mode reads it from
 * chrome.devtools.inspectedWindow.tabId.
 */

// =========================================================================
// Tab ID management
// =========================================================================

/**
 * Tab ID for the page being debugged.
 * Set from URL params (standalone window) or chrome.devtools (DevTools panel).
 * @type {number|null}
 */
let _tabId = null;

/**
 * Sets the tab ID for the inspected page.
 * @param {number} id - Chrome tab ID
 */
export function setTabId(id) {
  _tabId = id;
}

/**
 * Gets the tab ID. Falls back to chrome.devtools.inspectedWindow.tabId
 * if not explicitly set (backward compatibility with DevTools panel mode).
 * @returns {number|null}
 */
export function getTabId() {
  if (_tabId !== null) return _tabId;
  if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
    _tabId = chrome.devtools.inspectedWindow.tabId;
  }
  return _tabId;
}

// =========================================================================
// Page evaluation
// =========================================================================

/**
 * Evaluates a JavaScript expression in the inspected page context.
 * Uses chrome.scripting.executeScript when available (standalone window),
 * falls back to chrome.devtools.inspectedWindow.eval (DevTools panel).
 *
 * @param {string} expression - JavaScript expression to evaluate
 * @returns {Promise<*>} Resolved with the result value
 */
export function evalInPage(expression) {
  // Standalone window path: chrome.scripting.executeScript
  if (typeof chrome !== 'undefined' && chrome.scripting?.executeScript && _tabId !== null) {
    return chrome.scripting.executeScript({
      target: { tabId: _tabId },
      func: (expr) => eval(expr),
      args: [expression],
      world: 'MAIN',
    }).then((results) => {
      if (!results || results.length === 0) throw new Error('No script result');
      if (results[0].error) {
        throw new Error(results[0].error.message || 'eval error');
      }
      return results[0].result;
    });
  }

  // DevTools panel path: inspectedWindow.eval
  if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow?.eval) {
    return new Promise((resolve, reject) => {
      chrome.devtools.inspectedWindow.eval(expression, (result, exceptionInfo) => {
        if (exceptionInfo) {
          const msg = exceptionInfo.value || exceptionInfo.description || 'eval error';
          reject(new Error(msg));
        } else {
          resolve(result);
        }
      });
    });
  }

  return Promise.reject(new Error('No eval API available (need chrome.scripting or chrome.devtools)'));
}

/**
 * Checks whether __schemeDebug is available in the page.
 * @returns {Promise<boolean>}
 */
export async function isSchemeDebugAvailable() {
  try {
    return await evalInPage('typeof __schemeDebug !== "undefined"');
  } catch (e) {
    console.warn('[scheme-bridge] isSchemeDebugAvailable failed:', e.message);
    return false;
  }
}

/**
 * Fetches the list of registered Scheme sources.
 * Returns an array of { url, content, lines, origin } objects.
 *
 * @returns {Promise<Array<{url: string, content: string, lines: number, origin: string}>>}
 */
export async function getSources() {
  try {
    const json = await evalInPage('JSON.stringify(__schemeDebug.getSources())');
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] getSources failed:', e.message);
    return [];
  }
}

/**
 * Fetches the content of a specific source URL.
 *
 * @param {string} url - The scheme:// URL of the source
 * @returns {Promise<string|null>} The source content, or null if unavailable
 */
export async function getSourceContent(url) {
  try {
    const json = await evalInPage(
      `JSON.stringify(__schemeDebug.getSourceContent(${JSON.stringify(url)}))`
    );
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] getSourceContent failed:', e.message);
    return null;
  }
}

/**
 * Activates debug mode in the page.
 * @returns {Promise<{active: boolean, needsReload: boolean}>}
 */
export async function activate() {
  try {
    const json = await evalInPage('JSON.stringify(__schemeDebug.activate())');
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] activate failed:', e.message);
    return { active: false, needsReload: true };
  }
}

/**
 * Gets the current debugger status.
 * @returns {Promise<{state: string, reason: string|null, active: boolean}>}
 */
export async function getStatus() {
  try {
    const json = await evalInPage('JSON.stringify(__schemeDebug.getStatus())');
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] getStatus failed:', e.message);
    return { state: 'inactive', reason: null, active: false };
  }
}

/**
 * Gets the current Scheme call stack.
 * @returns {Promise<Array<{name: string, source: Object|null, tcoCount: number}>>}
 */
export async function getStack() {
  try {
    const json = await evalInPage('JSON.stringify(__schemeDebug.getStack())');
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] getStack failed:', e.message);
    return [];
  }
}

/**
 * Gets the local variable bindings for a specific stack frame.
 * @param {number} frameIndex - Index into the stack (0 = bottom, length-1 = top)
 * @returns {Promise<Array<{name: string, value: string, type: string, subtype: string|null}>>}
 */
export async function getLocals(frameIndex) {
  try {
    const json = await evalInPage(`JSON.stringify(__schemeDebug.getLocals(${frameIndex}))`);
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] getLocals failed:', e.message);
    return [];
  }
}

/**
 * Evaluates an expression within the context of a specific stack frame.
 * @param {string} expression - The Scheme expression to evaluate
 * @param {number} frameIndex - Index into the stack (0 = bottom, length-1 = top)
 * @returns {Promise<{success: boolean, result: string, error: string|null}>}
 */
export async function evalInFrame(expression, frameIndex) {
  try {
    const json = await evalInPage(
      `JSON.stringify(__schemeDebug.eval(${JSON.stringify(expression)}, ${frameIndex}))`
    );
    const result = JSON.parse(json);
    return { success: true, result: result, error: null };
  } catch (err) {
    return { success: false, result: null, error: err.message };
  }
}

/**
 * Acknowledges that the panel has received the pause event.
 * Cancels the safety timeout so the pause waits indefinitely
 * for an explicit resume/step/abort from the user.
 * @returns {Promise<void>}
 */
export async function ackPause() {
  try {
    await evalInPage('__schemeDebug.ackPause(); undefined');
  } catch (e) {
    console.warn('[scheme-bridge] ackPause failed:', e.message);
  }
}

/**
 * Resumes execution from a pause.
 * @returns {Promise<void>}
 */
export async function resume() {
  try {
    await evalInPage('__schemeDebug.resume(); undefined');
  } catch (e) {
    console.warn('[scheme-bridge] resume failed:', e.message);
  }
}

/**
 * Steps into the next expression.
 * @returns {Promise<void>}
 */
export async function stepInto() {
  try {
    await evalInPage('__schemeDebug.stepInto(); undefined');
  } catch (e) {
    console.warn('[scheme-bridge] stepInto failed:', e.message);
  }
}

/**
 * Steps over the current expression.
 * @returns {Promise<void>}
 */
export async function stepOver() {
  try {
    await evalInPage('__schemeDebug.stepOver(); undefined');
  } catch (e) {
    console.warn('[scheme-bridge] stepOver failed:', e.message);
  }
}

/**
 * Steps out of the current function.
 * @returns {Promise<void>}
 */
export async function stepOut() {
  try {
    await evalInPage('__schemeDebug.stepOut(); undefined');
  } catch (e) {
    console.warn('[scheme-bridge] stepOut failed:', e.message);
  }
}

/**
 * Fetches the expression spans for a source URL.
 * Each span describes a single AST expression with its location range,
 * enabling expression-level breakpoints and highlighting.
 *
 * @param {string} url - The scheme:// URL of the source
 * @returns {Promise<Array<{exprId: number, line: number, column: number, endLine: number, endColumn: number}>>}
 */
export async function getExpressions(url) {
  try {
    const json = await evalInPage(
      `JSON.stringify(__schemeDebug.getExpressions(${JSON.stringify(url)}))`
    );
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] getExpressions failed:', e.message);
    return [];
  }
}

/**
 * Sets a breakpoint at a specific source location.
 * @param {string} url - Source URL
 * @param {number} line - 1-indexed line number
 * @param {number|null} [column=null] - Column number (1-indexed), or null for line-level
 * @returns {Promise<string|null>} Breakpoint ID, or null on failure
 */
export async function setBreakpoint(url, line, column = null) {
  try {
    const colArg = column !== null ? `, ${column}` : '';
    const json = await evalInPage(
      `JSON.stringify(__schemeDebug.setBreakpoint(${JSON.stringify(url)}, ${line}${colArg}))`
    );
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] setBreakpoint failed:', e.message);
    return null;
  }
}

/**
 * Removes a breakpoint by ID.
 * @param {string} id - Breakpoint ID
 * @returns {Promise<boolean>}
 */
export async function removeBreakpoint(id) {
  try {
    const json = await evalInPage(
      `JSON.stringify(__schemeDebug.removeBreakpoint(${JSON.stringify(id)}))`
    );
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] removeBreakpoint failed:', e.message);
    return false;
  }
}

/**
 * Gets all currently set breakpoints.
 * @returns {Promise<Array<{id: string, filename: string, line: number, column: number|null}>>}
 */
export async function getAllBreakpoints() {
  try {
    const json = await evalInPage('JSON.stringify(__schemeDebug.getAllBreakpoints())');
    return JSON.parse(json);
  } catch (e) {
    console.warn('[scheme-bridge] getAllBreakpoints failed:', e.message);
    return [];
  }
}
