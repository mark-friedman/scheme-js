/**
 * @fileoverview Sidebar panel logic for the Scheme Stack extension.
 *
 * Receives debugger-paused / debugger-resumed messages from the background
 * script and uses __schemeDebug page-side API (via inspectedWindow.eval)
 * to render the Scheme call stack, variables, and TCO info.
 */

// =========================================================================
// DOM References
// =========================================================================

const framesEl = document.getElementById('frames');
const variablesEl = document.getElementById('variables');
const statusEl = document.getElementById('status');

const btnStepInto = document.getElementById('btn-step-into');
const btnStepOver = document.getElementById('btn-step-over');
const btnStepOut = document.getElementById('btn-step-out');

// =========================================================================
// State
// =========================================================================

/** @type {Array|null} Current stack frames from __schemeDebug.getStack() */
let currentStack = null;

/** @type {number} Currently selected frame index */
let selectedFrameIndex = -1;

// =========================================================================
// Page Evaluation Helper
// =========================================================================

/**
 * Evaluates a JS expression in the inspected page context.
 * Wraps chrome.devtools.inspectedWindow.eval with a Promise.
 *
 * @param {string} expression - JavaScript expression to evaluate
 * @returns {Promise<*>} The eval result
 */
function evalInPage(expression) {
    return new Promise((resolve, reject) => {
        chrome.devtools.inspectedWindow.eval(expression, (result, exceptionInfo) => {
            if (exceptionInfo) {
                reject(new Error(exceptionInfo.value || exceptionInfo.description || 'eval error'));
            } else {
                resolve(result);
            }
        });
    });
}

// =========================================================================
// HTML Helpers (inline for extension — no module imports)
// =========================================================================

/**
 * Escapes HTML special characters.
 * @param {string} text
 * @returns {string}
 */
function escapeHtml(text) {
    return text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

/**
 * Formats a source location for display.
 * @param {Object|null} source - { filename, line, column }
 * @returns {string}
 */
function formatSource(source) {
    if (!source || !source.filename) return '<unknown>';
    const basename = source.filename.split('/').pop();
    return `${basename}:${source.line}`;
}

// =========================================================================
// Rendering
// =========================================================================

/**
 * Renders the stack frame list.
 * @param {Array<{name: string, source: Object|null, tcoCount: number}>} frames
 */
function renderFrameList(frames) {
    if (!frames || frames.length === 0) {
        framesEl.innerHTML = '<div class="empty-message">No Scheme frames</div>';
        return;
    }

    // Reverse: display top frame (most recent) first
    const reversed = [...frames].reverse();
    const parts = [];

    for (let i = 0; i < reversed.length; i++) {
        const frame = reversed[i];
        const originalIndex = frames.length - 1 - i;
        const isSelected = originalIndex === selectedFrameIndex;
        const selectedClass = isSelected ? ' selected' : '';

        let html = `<div class="frame${selectedClass}" data-index="${originalIndex}">`;
        html += `<span class="frame-name">${escapeHtml(frame.name)}</span>`;

        // TCO badge
        if (frame.tcoCount > 0) {
            html += ` <span class="tco-badge">TCO×${frame.tcoCount}</span>`;
        }

        // Source location
        const sourceText = formatSource(frame.source);
        html += ` <span class="frame-source">${escapeHtml(sourceText)}</span>`;

        html += '</div>';
        parts.push(html);
    }

    framesEl.innerHTML = parts.join('\n');

    // Add click handlers
    framesEl.querySelectorAll('.frame').forEach(el => {
        el.addEventListener('click', () => {
            const index = parseInt(el.dataset.index, 10);
            selectFrame(index);
        });
    });
}

/**
 * Renders the variables for the selected frame.
 * @param {Array<{name: string, value: string, type: string}>} locals
 */
function renderVariables(locals) {
    if (!locals || locals.length === 0) {
        variablesEl.innerHTML = '<div class="empty-message">No variables</div>';
        return;
    }

    const parts = [];
    for (const local of locals) {
        const cssClass = getTypeCssClass(local.type);
        parts.push(
            `<div class="variable">` +
            `<span class="var-name">${escapeHtml(local.name)}</span>` +
            `<span class="var-separator">: </span>` +
            `<span class="var-value ${cssClass}">${escapeHtml(local.value)}</span>` +
            `</div>`
        );
    }

    variablesEl.innerHTML = parts.join('\n');
}

/**
 * Maps a value type to a CSS class.
 * @param {string} type
 * @returns {string}
 */
function getTypeCssClass(type) {
    switch (type) {
        case 'number':
        case 'bigint':
            return 'number';
        case 'string':
            return 'string';
        case 'boolean':
            return 'boolean';
        case 'function':
            return 'function';
        case 'symbol':
            return 'symbol';
        default:
            return 'object';
    }
}

// =========================================================================
// Frame Selection
// =========================================================================

/**
 * Selects a frame: updates UI, fetches locals, navigates editor.
 * @param {number} frameIndex - Index into the stack (0 = bottom)
 */
async function selectFrame(frameIndex) {
    selectedFrameIndex = frameIndex;

    // Re-render frame list to update selection highlight
    renderFrameList(currentStack);

    // Fetch and render variables
    try {
        const locals = await evalInPage(
            `JSON.stringify(__schemeDebug.getLocals(${frameIndex}))`
        );
        renderVariables(JSON.parse(locals));
    } catch (e) {
        renderVariables([]);
    }

    // Navigate editor to source location
    try {
        const source = await evalInPage(
            `JSON.stringify(__schemeDebug.getSource(${frameIndex}))`
        );
        const loc = JSON.parse(source);
        if (loc && loc.filename) {
            // Open the file in the Sources panel editor
            chrome.devtools.panels.openResource(
                loc.filename,
                loc.line - 1, // openResource uses 0-indexed lines
                () => { }
            );
        }
    } catch (e) {
        // Source navigation is best-effort
    }
}

// =========================================================================
// Debugger Event Handling
// =========================================================================

/**
 * Handles a debugger-paused event.
 * Fetches the Scheme stack and renders the sidebar.
 */
async function onPaused() {
    statusEl.textContent = 'Paused';
    statusEl.classList.add('paused');

    try {
        // Check if __schemeDebug is available
        const hasApi = await evalInPage('typeof __schemeDebug !== "undefined"');
        if (!hasApi) {
            framesEl.innerHTML = '<div class="empty-message">scheme-js not detected</div>';
            return;
        }

        // Fetch the Scheme stack
        const stackJson = await evalInPage('JSON.stringify(__schemeDebug.getStack())');
        currentStack = JSON.parse(stackJson);

        if (currentStack.length > 0) {
            // Default to top frame
            selectedFrameIndex = currentStack.length - 1;
            renderFrameList(currentStack);

            // Fetch variables for top frame
            const localsJson = await evalInPage(
                `JSON.stringify(__schemeDebug.getLocals(${selectedFrameIndex}))`
            );
            renderVariables(JSON.parse(localsJson));
        } else {
            renderFrameList([]);
            renderVariables([]);
        }
    } catch (e) {
        framesEl.innerHTML = `<div class="empty-message">Error: ${escapeHtml(e.message)}</div>`;
    }
}

/**
 * Handles a debugger-resumed event.
 * Clears the sidebar.
 */
function onResumed() {
    statusEl.textContent = 'Running';
    statusEl.classList.remove('paused');
    currentStack = null;
    selectedFrameIndex = -1;
    framesEl.innerHTML = '';
    variablesEl.innerHTML = '';
}

// =========================================================================
// Step Button Handlers
// =========================================================================

btnStepInto.addEventListener('click', async () => {
    try {
        await evalInPage('__schemeDebug.stepInto()');
    } catch (e) { /* ignore */ }
});

btnStepOver.addEventListener('click', async () => {
    try {
        await evalInPage('__schemeDebug.stepOver()');
    } catch (e) { /* ignore */ }
});

btnStepOut.addEventListener('click', async () => {
    try {
        await evalInPage('__schemeDebug.stepOut()');
    } catch (e) { /* ignore */ }
});

// =========================================================================
// Message Listener
// =========================================================================

chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
    if (message.type === 'debugger-paused') {
        onPaused();
    } else if (message.type === 'debugger-resumed') {
        onResumed();
    }
});

// =========================================================================
// Initialization
// =========================================================================

// Request the background script to attach the debugger
const tabId = chrome.devtools.inspectedWindow.tabId;
chrome.runtime.sendMessage({
    type: 'attach-debugger',
    tabId
});
