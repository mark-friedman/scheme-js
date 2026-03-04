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
