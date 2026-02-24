/**
 * @fileoverview Background service worker for the Scheme Stack extension.
 *
 * Attaches to the inspected tab's debugger to listen for CDP events
 * (Debugger.paused, Debugger.resumed) and forwards them to the sidebar
 * panel. Also handles auto-blackboxing of interpreter internals.
 */

// =========================================================================
// State
// =========================================================================

/** @type {Map<number, {attached: boolean}>} Tab state keyed by tab ID */
const tabState = new Map();

/**
 * Blackbox patterns for auto-hiding interpreter scripts.
 * These patterns match generated probe scripts and runtime internals.
 */
const BLACKBOX_PATTERNS = [
    '.*probe_runtime\\.js$',
    '.*devtools_debug\\.js$',
    '.*source_registry\\.js$',
    '.*env_proxy\\.js$',
    '.*interpreter\\.js$',
    '.*ast\\.js$',
    '.*values\\.js$'
];

// =========================================================================
// CDP Debugger Attachment
// =========================================================================

/**
 * Attaches to a tab's debugger if not already attached.
 * @param {number} tabId - Chrome tab ID
 */
async function attachToTab(tabId) {
    if (tabState.has(tabId) && tabState.get(tabId).attached) return;

    try {
        await chrome.debugger.attach({ tabId }, '1.3');
        await chrome.debugger.sendCommand({ tabId }, 'Debugger.enable');

        // Auto-blackbox interpreter internals
        await chrome.debugger.sendCommand({ tabId }, 'Debugger.setBlackboxPatterns', {
            patterns: BLACKBOX_PATTERNS
        });

        tabState.set(tabId, { attached: true });
        console.log(`[Scheme Stack] Attached to tab ${tabId}`);
    } catch (e) {
        console.warn(`[Scheme Stack] Failed to attach to tab ${tabId}:`, e.message);
    }
}

/**
 * Detaches from a tab's debugger.
 * @param {number} tabId - Chrome tab ID
 */
async function detachFromTab(tabId) {
    if (!tabState.has(tabId) || !tabState.get(tabId).attached) return;

    try {
        await chrome.debugger.detach({ tabId });
        tabState.delete(tabId);
        console.log(`[Scheme Stack] Detached from tab ${tabId}`);
    } catch (e) {
        // Tab may have already closed
        tabState.delete(tabId);
    }
}

// =========================================================================
// CDP Event Handling
// =========================================================================

/**
 * Checks if a paused event is from a Scheme probe.
 * Scheme probe functions have names starting with "__scheme_E".
 *
 * @param {Object} params - CDP Debugger.paused parameters
 * @returns {boolean} True if this is a Scheme probe pause
 */
function isSchemeProbe(params) {
    if (!params.callFrames || params.callFrames.length === 0) return false;
    const topFrame = params.callFrames[0];
    return topFrame.functionName && topFrame.functionName.startsWith('__scheme_E');
}

/**
 * Handles CDP debugger events.
 * Routes Debugger.paused and Debugger.resumed to the sidebar.
 */
chrome.debugger.onEvent.addListener((source, method, params) => {
    const tabId = source.tabId;

    if (method === 'Debugger.paused') {
        // Only notify sidebar for Scheme probe pauses
        if (isSchemeProbe(params)) {
            chrome.runtime.sendMessage({
                type: 'debugger-paused',
                tabId,
                callFrames: params.callFrames,
                reason: params.reason
            }).catch(() => {
                // Sidebar may not be open yet — ignore
            });
        }
    } else if (method === 'Debugger.resumed') {
        chrome.runtime.sendMessage({
            type: 'debugger-resumed',
            tabId
        }).catch(() => {
            // Sidebar may not be open yet — ignore
        });
    }
});

/**
 * Clean up when a tab closes.
 */
chrome.tabs.onRemoved.addListener((tabId) => {
    tabState.delete(tabId);
});

/**
 * Clean up when debugger is detached externally (e.g., DevTools closed).
 */
chrome.debugger.onDetach.addListener((source, reason) => {
    tabState.delete(source.tabId);
    console.log(`[Scheme Stack] Detached from tab ${source.tabId}: ${reason}`);
});

// =========================================================================
// Message Handling (from sidebar)
// =========================================================================

chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
    if (message.type === 'attach-debugger') {
        attachToTab(message.tabId).then(() => {
            sendResponse({ success: true });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true; // Indicates async response
    }

    if (message.type === 'detach-debugger') {
        detachFromTab(message.tabId).then(() => {
            sendResponse({ success: true });
        });
        return true;
    }
});
