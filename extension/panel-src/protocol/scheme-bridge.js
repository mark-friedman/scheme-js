/**
 * @fileoverview Scheme bridge — wraps chrome.devtools.inspectedWindow.eval
 * to call __schemeDebug API methods in the inspected page context.
 */

/**
 * Evaluates a JavaScript expression in the inspected page context.
 * Returns the result or throws on error.
 *
 * @param {string} expression - JavaScript expression to evaluate
 * @returns {Promise<*>} Resolved with the result value
 */
export function evalInPage(expression) {
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

/**
 * Checks whether __schemeDebug is available in the page.
 * @returns {Promise<boolean>}
 */
export async function isSchemeDebugAvailable() {
  try {
    return await evalInPage('typeof __schemeDebug !== "undefined"');
  } catch {
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
  } catch {
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
  } catch {
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
  } catch {
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
  } catch {
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
  } catch {
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
  } catch {
    return [];
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
  } catch { /* ignore */ }
}

/**
 * Resumes execution from a pause.
 * @returns {Promise<void>}
 */
export async function resume() {
  try {
    await evalInPage('__schemeDebug.resume(); undefined');
  } catch { /* ignore */ }
}

/**
 * Steps into the next expression.
 * @returns {Promise<void>}
 */
export async function stepInto() {
  try {
    await evalInPage('__schemeDebug.stepInto(); undefined');
  } catch { /* ignore */ }
}

/**
 * Steps over the current expression.
 * @returns {Promise<void>}
 */
export async function stepOver() {
  try {
    await evalInPage('__schemeDebug.stepOver(); undefined');
  } catch { /* ignore */ }
}

/**
 * Steps out of the current function.
 * @returns {Promise<void>}
 */
export async function stepOut() {
  try {
    await evalInPage('__schemeDebug.stepOut(); undefined');
  } catch { /* ignore */ }
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
  } catch {
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
  } catch {
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
  } catch {
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
  } catch {
    return [];
  }
}
