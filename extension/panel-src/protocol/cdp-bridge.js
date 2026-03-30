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

import { getTabId } from './scheme-bridge.js';

// =========================================================================
// State
// =========================================================================

/** Whether CDP is currently attached to the inspected tab. */
let attached = false;

/** Whether a CDP attach is in progress (avoids duplicate requests). */
let attaching = false;

/**
 * Cached script URL map from Debugger.scriptParsed events.
 * Cleared on page navigation to prevent stale entries.
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
  } catch (e) {
    console.warn('[cdp-bridge] attachCDP failed:', e.message);
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
  } catch (e) {
    console.warn('[cdp-bridge] detachCDP failed:', e.message);
  }
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

/**
 * Marks CDP as attached without actually sending an attach command.
 * Used when background.js already has the debugger attached (e.g.,
 * during sync-path pauses where the debugger was attached by background.js
 * before the panel received the scheme-sync-paused message).
 */
export function markAttached() {
  attached = true;
}

/**
 * Clears cached state on page navigation.
 * Should be called when the inspected page reloads so that stale
 * scriptIds from the previous page don't linger.
 */
export function clearNavigationCache() {
  scriptUrlMap.clear();
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
  } catch (e) {
    console.warn('[cdp-bridge] resumeCDP failed:', e.message);
  }
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
  } catch (e) {
    console.warn('[cdp-bridge] stepIntoCDP failed:', e.message);
  }
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
  } catch (e) {
    console.warn('[cdp-bridge] stepOverCDP failed:', e.message);
  }
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
  } catch (e) {
    console.warn('[cdp-bridge] stepOutCDP failed:', e.message);
  }
}

/**
 * Evaluates a JS expression while the page is paused, using CDP
 * Debugger.evaluateOnCallFrame. Works during sync-path pauses when
 * normal page eval is unavailable because V8 is suspended.
 *
 * @param {string} expression - JS expression to evaluate
 * @returns {Promise<*>} The evaluation result value
 */
export async function evalWhilePaused(expression) {
  if (!attached) throw new Error('CDP not attached');
  try {
    const response = await sendToBackground({
      type: 'eval-paused',
      tabId: getTabId(),
      expression,
    });
    if (response.success) {
      return response.result;
    }
    throw new Error(response.error || 'eval-paused failed');
  } catch (e) {
    throw new Error(`evalWhilePaused: ${e.message}`);
  }
}

/**
 * Sends a Scheme step-into command via CDP evaluateOnCallFrame + resume.
 * Used during sync-path pauses where the probe runtime is on the V8 call stack.
 * @returns {Promise<void>}
 */
export async function schemeStepInto() {
  if (!attached) return;
  try {
    await sendToBackground({
      type: 'scheme-step-into',
      tabId: getTabId(),
    });
  } catch (e) {
    console.warn('[cdp-bridge] schemeStepInto failed:', e.message);
  }
}

/**
 * Sends a Scheme step-over command via CDP evaluateOnCallFrame + resume.
 * @returns {Promise<void>}
 */
export async function schemeStepOver() {
  if (!attached) return;
  try {
    await sendToBackground({
      type: 'scheme-step-over',
      tabId: getTabId(),
    });
  } catch (e) {
    console.warn('[cdp-bridge] schemeStepOver failed:', e.message);
  }
}

/**
 * Sends a Scheme step-out command via CDP evaluateOnCallFrame + resume.
 * @returns {Promise<void>}
 */
export async function schemeStepOut() {
  if (!attached) return;
  try {
    await sendToBackground({
      type: 'scheme-step-out',
      tabId: getTabId(),
    });
  } catch (e) {
    console.warn('[cdp-bridge] schemeStepOut failed:', e.message);
  }
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
  } catch (e) {
    console.warn('[cdp-bridge] setJSBreakpoint failed:', e.message);
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
  } catch (e) {
    console.warn('[cdp-bridge] removeJSBreakpoint failed:', e.message);
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
  } catch (e) {
    console.warn('[cdp-bridge] setBoundaryBreakpoint failed:', e.message);
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
  } catch (e) {
    console.warn('[cdp-bridge] getJSSource failed:', e.message);
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
