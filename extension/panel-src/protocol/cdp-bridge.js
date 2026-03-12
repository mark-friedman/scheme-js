/**
 * @fileoverview CDP Bridge — manages Chrome DevTools Protocol communication
 * for JavaScript debugging via the background service worker.
 *
 * Provides lazy CDP attachment (only attaches when JS debugging is needed),
 * JS breakpoint management, source fetching, and CDP step/resume commands.
 *
 * All communication with CDP goes through chrome.runtime.sendMessage to
 * the background.js service worker, which holds the actual chrome.debugger
 * attachment.
 */

// =========================================================================
// State
// =========================================================================

/** Whether CDP is currently attached to the inspected tab. */
let attached = false;

/** Whether a CDP attach is in progress (avoids duplicate requests). */
let attaching = false;

/**
 * Cached script URL map from Debugger.scriptParsed events.
 * @type {Map<string, string>} scriptId → URL
 */
const scriptUrlMap = new Map();

/**
 * Registered callbacks for CDP events.
 * @type {{paused: Function[], resumed: Function[], scriptParsed: Function[]}}
 */
const listeners = {
  paused: [],
  resumed: [],
  scriptParsed: [],
};

/**
 * The inspected tab ID. Resolved lazily from chrome.devtools.
 * @type {number|null}
 */
let tabId = null;

/**
 * Gets the inspected tab ID, caching it for reuse.
 * @returns {number}
 */
function getTabId() {
  if (tabId === null && typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
    tabId = chrome.devtools.inspectedWindow.tabId;
  }
  return tabId;
}

// =========================================================================
// Message sending helper
// =========================================================================

/**
 * Sends a message to the background service worker and returns the response.
 *
 * @param {Object} message - Message to send
 * @returns {Promise<Object>} Response from background
 */
function sendToBackground(message) {
  return new Promise((resolve, reject) => {
    if (typeof chrome === 'undefined' || !chrome.runtime?.sendMessage) {
      reject(new Error('chrome.runtime.sendMessage not available'));
      return;
    }
    chrome.runtime.sendMessage(message, (response) => {
      if (chrome.runtime.lastError) {
        reject(new Error(chrome.runtime.lastError.message));
      } else {
        resolve(response || {});
      }
    });
  });
}

// =========================================================================
// CDP Attachment (lazy)
// =========================================================================

/**
 * Attaches to the inspected tab's debugger via the background service worker.
 * No-op if already attached. Shows the "started debugging" Chrome banner.
 *
 * @returns {Promise<boolean>} True if attached (or was already attached)
 */
export async function attachCDP() {
  if (attached) return true;
  if (attaching) {
    // Wait for in-progress attachment
    return new Promise((resolve) => {
      const check = setInterval(() => {
        if (!attaching) {
          clearInterval(check);
          resolve(attached);
        }
      }, 50);
    });
  }

  attaching = true;
  try {
    const response = await sendToBackground({
      type: 'attach-debugger',
      tabId: getTabId(),
    });
    attached = response.success === true;
    return attached;
  } catch {
    return false;
  } finally {
    attaching = false;
  }
}

/**
 * Detaches from the inspected tab's debugger.
 *
 * @returns {Promise<void>}
 */
export async function detachCDP() {
  if (!attached) return;
  try {
    await sendToBackground({
      type: 'detach-debugger',
      tabId: getTabId(),
    });
  } catch { /* ignore */ }
  attached = false;
  scriptUrlMap.clear();
}

/**
 * Returns whether CDP is currently attached.
 * @returns {boolean}
 */
export function isAttached() {
  return attached;
}

// =========================================================================
// CDP Step / Resume Commands
// =========================================================================

/**
 * Resumes execution via CDP Debugger.resume.
 * @returns {Promise<void>}
 */
export async function resumeCDP() {
  if (!attached) return;
  try {
    await sendToBackground({
      type: 'resume-debugger',
      tabId: getTabId(),
    });
  } catch { /* ignore */ }
}

/**
 * Steps into via native V8 Debugger.stepInto.
 * @returns {Promise<void>}
 */
export async function stepIntoCDP() {
  if (!attached) return;
  try {
    await sendToBackground({
      type: 'cdp-step-into',
      tabId: getTabId(),
    });
  } catch { /* ignore */ }
}

/**
 * Steps over via native V8 Debugger.stepOver.
 * @returns {Promise<void>}
 */
export async function stepOverCDP() {
  if (!attached) return;
  try {
    await sendToBackground({
      type: 'cdp-step-over',
      tabId: getTabId(),
    });
  } catch { /* ignore */ }
}

/**
 * Steps out via native V8 Debugger.stepOut.
 * @returns {Promise<void>}
 */
export async function stepOutCDP() {
  if (!attached) return;
  try {
    await sendToBackground({
      type: 'cdp-step-out',
      tabId: getTabId(),
    });
  } catch { /* ignore */ }
}

/**
 * Evaluates a JS expression in a specific call frame.
 * @param {string} callFrameId
 * @param {string} expression
 * @returns {Promise<{success: boolean, result: any, error: string|null}>}
 */
export async function evalInJSFrame(callFrameId, expression) {
  if (!attached) return { success: false, result: null, error: 'CDP not attached' };
  try {
    const response = await sendToBackground({
      type: 'eval-in-js-frame',
      tabId: getTabId(),
      callFrameId,
      expression,
    });
    return response;
  } catch (err) {
    return { success: false, result: null, error: err.message };
  }
}


// =========================================================================
// JS Breakpoints via CDP
// =========================================================================

/**
 * Sets a JS breakpoint by URL via CDP Debugger.setBreakpointByUrl.
 * Auto-attaches CDP if not yet attached.
 *
 * @param {string} url - JS file URL
 * @param {number} lineNumber - 0-indexed line number
 * @returns {Promise<string|null>} CDP breakpoint ID, or null on failure
 */
export async function setJSBreakpoint(url, lineNumber) {
  // Auto-attach on first JS breakpoint
  if (!attached) {
    const ok = await attachCDP();
    if (!ok) return null;
    // Notify listeners that CDP just attached
    for (const fn of listeners.scriptParsed) {
      fn({ type: 'cdp-attached' });
    }
  }

  try {
    const response = await sendToBackground({
      type: 'set-js-breakpoint',
      tabId: getTabId(),
      url,
      lineNumber,
    });
    if (response.success) {
      return response.breakpointId || null;
    }
    return null;
  } catch {
    return null;
  }
}

/**
 * Removes a JS breakpoint by CDP breakpoint ID.
 *
 * @param {string} breakpointId - CDP breakpoint ID
 * @returns {Promise<boolean>} True if removed
 */
export async function removeJSBreakpoint(breakpointId) {
  if (!attached) return false;
  try {
    const response = await sendToBackground({
      type: 'remove-js-breakpoint',
      tabId: getTabId(),
      breakpointId,
    });
    return response.success === true;
  } catch {
    return false;
  }
}

// =========================================================================
// Phase 5: Boundary Breakpoint
// =========================================================================

/**
 * Sets a native JS breakpoint on the function stored in `globalThis.__schemeBoundaryTarget`.
 * Auto-attaches CDP if not yet attached.
 *
 * @returns {Promise<string|null>} CDP breakpoint ID, or null on failure (e.g., built-in function)
 */
export async function setBoundaryBreakpoint() {
  if (!attached) {
    const ok = await attachCDP();
    if (!ok) return null;
    for (const fn of listeners.scriptParsed) {
      fn({ type: 'cdp-attached' });
    }
  }

  try {
    const response = await sendToBackground({
      type: 'set-boundary-breakpoint',
      tabId: getTabId(),
    });
    if (response.success) {
      return response.breakpointId || null;
    }
    return null;
  } catch {
    return null;
  }
}

// =========================================================================
// JS Source Fetching
// =========================================================================

/**
 * Fetches the source code of a JS script by scriptId via CDP.
 *
 * @param {string} scriptId - CDP script ID
 * @returns {Promise<string|null>} Source text, or null on failure
 */
export async function getJSSource(scriptId) {
  if (!attached) return null;
  try {
    const response = await sendToBackground({
      type: 'get-js-source',
      tabId: getTabId(),
      scriptId,
    });
    if (response.success) {
      return response.source || null;
    }
    return null;
  } catch {
    return null;
  }
}

/**
 * Looks up a script URL from the scriptParsed cache.
 *
 * @param {string} scriptId - CDP script ID
 * @returns {string|undefined} URL or undefined
 */
export function getScriptUrl(scriptId) {
  return scriptUrlMap.get(scriptId);
}

// =========================================================================
// Event Listeners
// =========================================================================

/**
 * Registers a callback for CDP pause events.
 * @param {Function} callback - Called with { callFrames, reason }
 */
export function onCDPPaused(callback) {
  listeners.paused.push(callback);
}

/**
 * Registers a callback for CDP resume events.
 * @param {Function} callback - Called with no arguments
 */
export function onCDPResumed(callback) {
  listeners.resumed.push(callback);
}

/**
 * Registers a callback for scriptParsed / cdp-attached events.
 * @param {Function} callback - Called with event data
 */
export function onScriptParsed(callback) {
  listeners.scriptParsed.push(callback);
}

// =========================================================================
// Message listener (from background.js)
// =========================================================================

if (typeof chrome !== 'undefined' && chrome.runtime?.onMessage) {
  chrome.runtime.onMessage.addListener((message) => {
    if (message.type === 'cdp-paused') {
      // If we're receiving CDP pause events, the debugger is attached
      attached = true;
      for (const fn of listeners.paused) {
        fn({
          callFrames: message.callFrames || [],
          reason: message.reason || 'other',
        });
      }
    } else if (message.type === 'cdp-resumed') {
      for (const fn of listeners.resumed) {
        fn();
      }
    } else if (message.type === 'cdp-script-parsed') {
      if (message.scriptId && message.url) {
        scriptUrlMap.set(message.scriptId, message.url);
      }
      for (const fn of listeners.scriptParsed) {
        fn(message);
      }
    }
  });
}
