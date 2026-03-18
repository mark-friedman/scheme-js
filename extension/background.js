/**
 * @fileoverview Background service worker for the Scheme-JS DevTools extension.
 *
 * Manages CDP debugger attachment and event handling:
 *   - Attaches to inspected tabs to intercept Debugger.paused/resumed events
 *   - Auto-resumes Scheme probe pauses (so Chrome's Sources tab never stays
 *     paused on `debugger;` statements in generated probe functions)
 *   - Forwards non-Scheme CDP pause events to the panel for JS debugging
 *   - Handles step commands, breakpoint management, and eval-while-paused
 *   - Auto-blackboxes interpreter internals
 *
 * The Scheme-JS panel receives pause notifications via a separate cooperative
 * channel (content script postMessage relay), independent of CDP events.
 */

// =========================================================================
// State
// =========================================================================

/** @type {Map<number, {attached: boolean}>} Tab state keyed by tab ID */
const tabState = new Map();

/**
 * Stores the most recent Debugger.paused params per tab.
 * Used by evaluateWhilePaused() to call Debugger.evaluateOnCallFrame,
 * which works reliably while paused (unlike inspectedWindow.eval /
 * Runtime.evaluate which may fail or return stale data).
 * @type {Map<number, Object>}
 */
const lastPauseParams = new Map();

/**
 * Tracks parsed script URLs by tab and scriptId.
 * Populated by Debugger.scriptParsed events, forwarded to the panel
 * so it can map scriptIds to URLs for JS source display.
 * @type {Map<number, Map<string, string>>} tabId → (scriptId → URL)
 */
const scriptUrlMaps = new Map();

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
    '.*values\\.js$',
    // Bundled distribution files (anchored to dist/ to avoid matching user files)
    '.*/dist/scheme\\.js$',
    '.*/dist/scheme\\.es\\.js$',
    '.*/dist/scheme-html\\.js$'
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
 * Scans the top few call frames for a probe function name starting
 * with "__scheme_E". We scan multiple frames (not just the top) because
 * instrumentation wrappers like task.run() can sometimes interpose frames.
 *
 * @param {Object} params - CDP Debugger.paused parameters
 * @returns {boolean} True if this is a Scheme probe pause
 */
function isSchemeProbe(params) {
    if (!params.callFrames || params.callFrames.length === 0) return false;
    // Check the top few frames for a probe function
    const framesToCheck = Math.min(params.callFrames.length, 5);
    for (let i = 0; i < framesToCheck; i++) {
        const frame = params.callFrames[i];
        if (frame.functionName && frame.functionName.startsWith('__scheme_E')) {
            return true;
        }
    }
    return false;
}

/**
 * Checks if a paused event is a Scheme exception.
 * Exception pauses won't have __scheme_E* probe frames at the top, but
 * they will have call frames whose URLs reference scheme:// or scheme-probe://
 * source-mapped files.
 *
 * @param {Object} params - CDP Debugger.paused parameters
 * @returns {boolean} True if this is a Scheme exception pause
 */
function isSchemeException(params) {
    if (params.reason !== 'exception') return false;
    if (!params.callFrames || params.callFrames.length === 0) return false;
    return params.callFrames.some(frame => {
        const url = frame.url || '';
        return url.startsWith('scheme://') || url.startsWith('scheme-probe://');
    });
}

/**
 * Evaluates a JS expression while the page is paused, using CDP
 * Debugger.evaluateOnCallFrame. This is reliable while paused, unlike
 * inspectedWindow.eval / Runtime.evaluate.
 *
 * Prefers the first probe call frame (__scheme_E*) since those are in the
 * probe IIFE scope where __schemeProbeRuntime is a closure parameter.
 * Falls back to the topmost frame if no probe frame is found (e.g.,
 * during exception pauses).
 *
 * @param {number} tabId - Chrome tab ID
 * @param {string} expression - JS expression to evaluate
 * @returns {Promise<*>} The evaluation result value
 */
async function evaluateWhilePaused(tabId, expression) {
    const pauseParams = lastPauseParams.get(tabId);
    if (!pauseParams || !pauseParams.callFrames || pauseParams.callFrames.length === 0) {
        throw new Error('Not paused or no call frames available');
    }

    // Prefer the probe frame (__scheme_E*) — it has the correct IIFE closure
    // scope. Fall back to the topmost frame if no probe frame is found.
    let callFrameId = pauseParams.callFrames[0].callFrameId;
    for (const frame of pauseParams.callFrames) {
        if (frame.functionName && frame.functionName.startsWith('__scheme_E')) {
            callFrameId = frame.callFrameId;
            break;
        }
    }

    const result = await chrome.debugger.sendCommand({ tabId }, 'Debugger.evaluateOnCallFrame', {
        callFrameId,
        expression,
        silent: true,
        returnByValue: true
    });

    if (result.exceptionDetails) {
        throw new Error(result.exceptionDetails.text || 'evaluation error');
    }

    return result.result?.value;
}

/**
 * Handles CDP debugger events.
 * Routes Debugger.paused and Debugger.resumed to the panel.
 * Also tracks Debugger.scriptParsed for JS source URL mapping.
 */
chrome.debugger.onEvent.addListener((source, method, params) => {
    const tabId = source.tabId;

    if (method === 'Debugger.paused') {
        // Store pause params for evaluateOnCallFrame
        lastPauseParams.set(tabId, params);

        if (isSchemeProbe(params) || isSchemeException(params)) {
            // Scheme probe/exception pause: auto-resume CDP immediately so
            // Chrome's Sources tab never stays paused on the `debugger;`
            // statement. The Scheme-JS panel receives its pause notification
            // via the cooperative channel (content script postMessage relay),
            // which is independent of CDP events.
            chrome.debugger.sendCommand({ tabId }, 'Debugger.resume').catch(() => {
                // Tab may have closed — ignore
            });
        } else {
            // Non-Scheme pause (JS breakpoint, exception, etc.)
            // Forward to panel as a CDP-specific pause event
            chrome.runtime.sendMessage({
                type: 'cdp-paused',
                tabId,
                callFrames: params.callFrames,
                reason: params.reason
            }).catch(() => {
                // Panel may not be open yet — ignore
            });
        }
    } else if (method === 'Debugger.resumed') {
        lastPauseParams.delete(tabId);

        chrome.runtime.sendMessage({
            type: 'debugger-resumed',
            tabId
        }).catch(() => {
            // Panel may not be open yet — ignore
        });
        chrome.runtime.sendMessage({
            type: 'cdp-resumed',
            tabId
        }).catch(() => {
            // Panel may not be open yet — ignore
        });
    } else if (method === 'Debugger.scriptParsed') {
        // Track script URLs for JS source display in the panel
        const scriptId = params.scriptId;
        const url = params.url || '';
        if (url && !url.startsWith('scheme://') && !url.startsWith('scheme-probe://')) {
            if (!scriptUrlMaps.has(tabId)) {
                scriptUrlMaps.set(tabId, new Map());
            }
            scriptUrlMaps.get(tabId).set(scriptId, url);

            // Forward to panel
            chrome.runtime.sendMessage({
                type: 'cdp-script-parsed',
                tabId,
                scriptId,
                url,
            }).catch(() => {
                // Panel may not be open yet — ignore
            });
        }
    }
});

/**
 * Clean up when a tab closes.
 */
chrome.tabs.onRemoved.addListener((tabId) => {
    tabState.delete(tabId);
    lastPauseParams.delete(tabId);
    scriptUrlMaps.delete(tabId);
});

/**
 * Clean up when debugger is detached externally (e.g., DevTools closed).
 */
chrome.debugger.onDetach.addListener((source, reason) => {
    tabState.delete(source.tabId);
    lastPauseParams.delete(source.tabId);
    scriptUrlMaps.delete(source.tabId);
    console.log(`[Scheme Stack] Detached from tab ${source.tabId}: ${reason}`);
});

// =========================================================================
// Message Handling (from panel)
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

    if (message.type === 'resume-debugger') {
        chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.resume').then(() => {
            sendResponse({ success: true });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // Evaluate an expression while paused using Debugger.evaluateOnCallFrame.
    // This is reliable while paused, unlike inspectedWindow.eval.
    if (message.type === 'eval-paused') {
        evaluateWhilePaused(message.tabId, message.expression).then(result => {
            sendResponse({ success: true, result });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // Evaluate an expression in a specific JS call frame context
    if (message.type === 'eval-in-js-frame') {
        chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.evaluateOnCallFrame', {
            callFrameId: message.callFrameId,
            expression: message.expression,
            silent: true,
            returnByValue: true
        }).then(result => {
            if (result.exceptionDetails) {
                sendResponse({ success: false, error: result.exceptionDetails.text || 'evaluation error' });
            } else {
                sendResponse({ success: true, result: result.result?.value });
            }
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // Step commands: evaluate step expression while paused, then resume.
    // Uses globalThis to avoid with(envProxy) scope interference in probe frames.
    // Always resumes even if the eval fails, to prevent the debugger from getting stuck.
    if (message.type === 'scheme-step-into' ||
        message.type === 'scheme-step-over' ||
        message.type === 'scheme-step-out') {
        const stepMethod = message.type.replace('scheme-', '');
        const camelCase = stepMethod.replace(/-([a-z])/g, (_, c) => c.toUpperCase());
        const expression = `globalThis.__schemeProbeRuntime.${camelCase}()`;

        (async () => {
            try {
                await evaluateWhilePaused(message.tabId, expression);
            } catch (e) {
                console.warn(`[Scheme Stack] Step eval failed (will resume anyway):`, e.message);
            }
            try {
                await chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.resume');
                sendResponse({ success: true });
            } catch (e) {
                sendResponse({ success: false, error: e.message });
            }
        })();
        return true;
    }

    // =====================================================================
    // Phase 4: Native V8 CDP step commands (for JS debugging)
    // =====================================================================

    // Native V8 Step Into — used when paused at a JS breakpoint
    if (message.type === 'cdp-step-into') {
        chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.stepInto').then(() => {
            sendResponse({ success: true });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // Native V8 Step Over — used when paused at a JS breakpoint
    if (message.type === 'cdp-step-over') {
        chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.stepOver').then(() => {
            sendResponse({ success: true });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // Native V8 Step Out — used when paused at a JS breakpoint
    if (message.type === 'cdp-step-out') {
        chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.stepOut').then(() => {
            sendResponse({ success: true });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // =====================================================================
    // Phase 4: JS Breakpoint management via CDP
    // =====================================================================

    // Set a JS breakpoint by URL and line number
    if (message.type === 'set-js-breakpoint') {
        chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.setBreakpointByUrl', {
            url: message.url,
            lineNumber: message.lineNumber,
        }).then(result => {
            sendResponse({ success: true, breakpointId: result.breakpointId });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // Remove a JS breakpoint by CDP breakpoint ID
    if (message.type === 'remove-js-breakpoint') {
        chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.removeBreakpoint', {
            breakpointId: message.breakpointId,
        }).then(() => {
            sendResponse({ success: true });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // =====================================================================
    // Phase 4: JS Source fetching via CDP
    // =====================================================================

    // Get the source of a JS script by scriptId
    if (message.type === 'get-js-source') {
        chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.getScriptSource', {
            scriptId: message.scriptId,
        }).then(result => {
            sendResponse({ success: true, source: result.scriptSource });
        }).catch(e => {
            sendResponse({ success: false, error: e.message });
        });
        return true;
    }

    // =====================================================================
    // Phase 5: Boundary Breakpoint management via CDP
    // =====================================================================

    // Set a one-shot breakpoint on the function stored in globalThis.__schemeBoundaryTarget
    if (message.type === 'set-boundary-breakpoint') {
        (async () => {
            try {
                // 1. Get the target function object
                const evalResult = await chrome.debugger.sendCommand({ tabId: message.tabId }, 'Runtime.evaluate', {
                    expression: 'globalThis.__schemeBoundaryTarget'
                });
                const objectId = evalResult.result?.objectId;
                if (!objectId) throw new Error("No boundary target found (not a function)");

                // 2. Get its internal properties to find [[FunctionLocation]]
                const props = await chrome.debugger.sendCommand({ tabId: message.tabId }, 'Runtime.getProperties', {
                    objectId: objectId,
                    ownProperties: false,
                    accessorPropertiesOnly: false,
                    generatePreview: false
                });

                let location = null;
                for (const prop of props.internalProperties || []) {
                    if (prop.name === '[[FunctionLocation]]' && prop.value && prop.value.value) {
                        location = prop.value.value;
                        break;
                    }
                }

                if (!location) throw new Error("Could not find function location");

                // 3. Set a breakpoint at that location
                const bpResult = await chrome.debugger.sendCommand({ tabId: message.tabId }, 'Debugger.setBreakpoint', {
                    location: location
                });

                sendResponse({ success: true, breakpointId: bpResult.breakpointId });
            } catch(e) {
                // If it fails (e.g. built-in native function without [[FunctionLocation]]), we just fail gracefully.
                sendResponse({ success: false, error: e.message });
            }
        })();
        return true;
    }
});
