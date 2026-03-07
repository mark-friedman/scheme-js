/**
 * @fileoverview Entry point for the Scheme-JS DevTools panel.
 *
 * Initializes the panel UI components and wires them together:
 *   - Toolbar: debug controls (Resume, Step Into/Over/Out) + status
 *   - Source list: file browser, click to load into editor
 *   - Editor: CodeMirror source viewer with breakpoint gutter
 *   - Call stack: displays Scheme frames when paused
 *   - Variables: displays locals for the selected frame
 *
 * Pause/resume notifications arrive via chrome.runtime.onMessage relayed
 * from the content script, which listens for CustomEvents on window.
 *
 * Breakpoints are persisted to chrome.storage.local so they survive page
 * reloads. On activation, stored breakpoints are re-sent to the interpreter.
 */

import { createEditor } from './components/editor.js';
import { createSourceList } from './components/source-list.js';
import { createToolbar } from './components/toolbar.js';
import { createCallStack } from './components/call-stack.js';
import { createVariables } from './components/variables.js';
import {
  evalInPage,
  getSourceContent,
  activate,
  getStatus,
  getStack,
  getLocals,
  ackPause,
  resume,
  stepInto,
  stepOver,
  stepOut,
  setBreakpoint,
  removeBreakpoint,
} from './protocol/scheme-bridge.js';

// =========================================================================
// DOM elements
// =========================================================================

const toolbarDebug       = document.getElementById('toolbar-debug');
const sourceListContainer = document.getElementById('source-list');
const editorContainer    = document.getElementById('editor-container');
const callStackContainer = document.getElementById('call-stack-container');
const variablesContainer = document.getElementById('variables-container');
const sidebar            = document.getElementById('sidebar');
const splitter           = document.getElementById('splitter');

// =========================================================================
// State
// =========================================================================

/** Currently displayed source URL. @type {string|null} */
let currentSourceUrl = null;

/**
 * Maps "${url}:${line}" -> breakpoint ID (from interpreter) for removal.
 * @type {Map<string, string>}
 */
const breakpointIds = new Map();

// =========================================================================
// Breakpoint persistence (localStorage in page context)
// =========================================================================

/**
 * Persists the current breakpoint set to the page's localStorage.
 * Uses evalInPage to write into the inspected page's storage, where
 * activate_debug.js can read it at document_start on next reload.
 */
async function saveBreakpointsToPage() {
  const entries = [];
  for (const key of breakpointIds.keys()) {
    const lastColon = key.lastIndexOf(':');
    const url = key.substring(0, lastColon);
    const line = parseInt(key.substring(lastColon + 1), 10);
    entries.push({ url, line });
  }
  try {
    await evalInPage(
      `localStorage.setItem('schemeJS_breakpoints', ${JSON.stringify(JSON.stringify(entries))})`
    );
  } catch { /* ignore */ }
}

/**
 * Loads the breakpoint list from the interpreter's current state.
 * Called after activation to sync the panel's local map with
 * whatever breakpoints were pre-loaded by activate_debug.js.
 */
async function syncBreakpointsFromInterpreter() {
  try {
    const json = await evalInPage('JSON.stringify(__schemeDebug.getAllBreakpoints())');
    const bps = JSON.parse(json);
    for (const bp of bps) {
      breakpointIds.set(`${bp.filename}:${bp.line}`, bp.id);
    }
  } catch { /* ignore */ }
}

/**
 * Returns the set of stored breakpoint line numbers for a specific URL.
 *
 * @param {string} url - Source URL to filter by
 * @returns {Set<number>} Line numbers with breakpoints for this URL
 */
function getBreakpointLinesForUrl(url) {
  const lines = new Set();
  for (const key of breakpointIds.keys()) {
    const lastColon = key.lastIndexOf(':');
    const bpUrl = key.substring(0, lastColon);
    if (bpUrl === url) {
      lines.add(parseInt(key.substring(lastColon + 1), 10));
    }
  }
  return lines;
}

// =========================================================================
// Initialize toolbar
// =========================================================================

const toolbar = createToolbar(toolbarDebug, {
  onResume:   () => resume(),
  onStepInto: () => stepInto(),
  onStepOver: () => stepOver(),
  onStepOut:  () => stepOut(),
});

// =========================================================================
// Initialize variables panel
// =========================================================================

const variables = createVariables(variablesContainer);

// =========================================================================
// Initialize call stack
// =========================================================================

/**
 * Called when a call stack frame is selected.
 * Loads the locals for that frame and navigates the editor to the source.
 *
 * @param {number} frameIndex - 0-indexed frame (0 = bottom, length-1 = top)
 * @param {import('./components/call-stack.js').StackFrame} frame
 */
async function onSelectFrame(frameIndex, frame) {
  // Load locals for this frame
  try {
    const locals = await getLocals(frameIndex);
    variables.setLocals(locals);
  } catch {
    variables.clear();
  }

  // Navigate editor to the frame's source location
  if (frame.source) {
    const url = frame.source.filename;
    if (url !== currentSourceUrl) {
      await loadSource(url);
    }
    editor.highlightLine(frame.source.line);
  } else {
    editor.highlightLine(null);
  }
}

const callStack = createCallStack(callStackContainer, onSelectFrame);

// =========================================================================
// Initialize editor
// =========================================================================

/**
 * Called when the user clicks a breakpoint gutter slot in the editor.
 * Adds or removes a breakpoint in the interpreter and persists the change.
 *
 * @param {number} line - 1-indexed line number
 * @param {boolean} isNowSet - true if breakpoint was just set, false if removed
 */
async function onBreakpointToggle(line, isNowSet) {
  if (!currentSourceUrl) return;
  const key = `${currentSourceUrl}:${line}`;
  if (isNowSet) {
    const id = await setBreakpoint(currentSourceUrl, line);
    if (id) {
      breakpointIds.set(key, id);
      saveBreakpointsToPage();
    }
  } else {
    const id = breakpointIds.get(key);
    if (id) {
      await removeBreakpoint(id);
      breakpointIds.delete(key);
      saveBreakpointsToPage();
    }
  }
}

const editor = createEditor(editorContainer, onBreakpointToggle);

// =========================================================================
// Source loading
// =========================================================================

/**
 * Loads a source file into the editor by URL.
 * Shows breakpoints from the local state for that URL in the gutter.
 *
 * @param {string} url - scheme:// URL of the source
 */
async function loadSource(url) {
  const content = await getSourceContent(url);
  if (content === null) return;
  currentSourceUrl = url;
  editor.setContent(content);

  // Show breakpoints for this file from our local persistent state
  const lines = getBreakpointLinesForUrl(url);
  if (lines.size > 0) {
    editor.setBreakpoints(lines);
  }
}

/**
 * Called when a source file is clicked in the file list.
 * Loads the content and syncs breakpoints for the selected file.
 *
 * @param {string} url - scheme:// URL of the selected source
 * @param {string} content - The source code
 */
function onSelectSource(url, content) {
  currentSourceUrl = url;
  editor.setContent(content || '');
  editor.highlightLine(null);

  // Sync breakpoints for this file from local state
  const lines = getBreakpointLinesForUrl(url);
  if (lines.size > 0) {
    editor.setBreakpoints(lines);
  }

  toolbar.setStatus(`Viewing: ${url.split('/').pop()}`);
}

// =========================================================================
// Initialize source list
// =========================================================================

const sourceList = createSourceList(sourceListContainer, onSelectSource);

// =========================================================================
// Pause / resume handlers
// =========================================================================

/**
 * Called when the interpreter pauses (breakpoint or step).
 *
 * @param {{reason: string, source: Object|null, stack: Array}} detail
 */
async function onPaused(detail) {
  const reason = detail.reason === 'step' ? 'Paused (step)' : 'Paused at breakpoint';
  toolbar.setStatus(reason);
  toolbar.setPaused();

  // Acknowledge the pause so the safety timeout is cancelled.
  // Without this, the interpreter auto-resumes after resumeTimeout ms
  // (safety net for stale panelConnected flags).
  ackPause();

  const stack = detail.stack || [];
  callStack.setFrames(stack, /* suppressAutoSelect */ true);

  // Use detail.source (the actual pause location) for highlighting,
  // NOT the top stack frame's source (which points to the function definition).
  const pauseSource = detail.source;
  if (pauseSource && pauseSource.filename) {
    if (pauseSource.filename !== currentSourceUrl) {
      await loadSource(pauseSource.filename);
    }
    editor.highlightLine(pauseSource.line);
  }

  // Load locals for the top frame (if any)
  if (stack.length > 0) {
    try {
      const locals = await getLocals(stack.length - 1);
      variables.setLocals(locals);
    } catch {
      variables.clear();
    }
  }
}

/**
 * Called when the interpreter resumes execution.
 */
function onResumed() {
  toolbar.setStatus('Running');
  toolbar.setRunning();
  callStack.clear();
  variables.clear();
  editor.highlightLine(null);
  // Re-query sources in case new scripts were processed while paused
  refresh();
}

// =========================================================================
// Chrome runtime message listener (relayed from content script)
// =========================================================================

if (typeof chrome !== 'undefined' && chrome.runtime?.onMessage) {
  chrome.runtime.onMessage.addListener((message) => {
    if (message.type === 'scheme-debug-paused') {
      onPaused(message.detail || {});
    } else if (message.type === 'scheme-debug-resumed') {
      onResumed();
    }
  });
}

// =========================================================================
// Refresh source list on panel show / page navigation
// =========================================================================

/**
 * Refreshes the source file list from the page.
 */
async function refresh() {
  toolbar.setStatus('Refreshing...');
  await sourceList.refresh();
  toolbar.setStatus('Ready');
}

/**
 * Activates debug mode and refreshes the panel state.
 * Called on initial panel load and after each page navigation.
 */
async function activateAndRefresh() {
  try {
    const result = await activate();
    if (result.needsReload) {
      toolbar.setStatus('Reload page to enable debugging');
    } else {
      // Sync panel state with breakpoints already set in the interpreter
      await syncBreakpointsFromInterpreter();
      await refresh();
      // Check if the page is already paused (we may have missed the event
      // due to timing — content_script runs at document_idle, but Scheme
      // scripts execute at DOMContentLoaded)
      const status = await getStatus();
      if (status.state === 'paused') {
        const stack = await getStack();
        onPaused({ reason: status.reason || 'breakpoint', source: status.source || null, stack });
      }
    }
  } catch {
    await refresh();
  }
}

// Activate debug mode and do initial refresh.
// activate() saves schemeJS_panelConnected to localStorage so that on the
// NEXT page reload, activate_debug.js can set panelConnected=true on the
// runtime BEFORE any Scheme scripts execute, enabling breakpoint pausing.
activateAndRefresh();

// Re-activate and refresh after each page navigation so the panel stays in
// sync with the new page's sources and breakpoints.
if (typeof chrome !== 'undefined' && chrome.devtools?.network) {
  chrome.devtools.network.onNavigated.addListener(() => {
    // Brief delay to let the page scripts finish before we query the interpreter
    setTimeout(() => activateAndRefresh(), 500);
  });
}

// Re-refresh when the panel regains focus (user switches back to this tab).
// Debounce to avoid re-rendering while the user is clicking within the panel.
let lastRefreshTime = 0;
if (typeof chrome !== 'undefined' && chrome.devtools?.panels) {
  window.addEventListener('focus', () => {
    const now = Date.now();
    if (now - lastRefreshTime > 2000) {
      lastRefreshTime = now;
      refresh();
    }
  });
}

// =========================================================================
// Splitter drag behavior (sidebar ↔ editor)
// =========================================================================

let dragging = false;
let dragStartX = 0;
let dragStartWidth = 0;

splitter.addEventListener('mousedown', (e) => {
  dragging = true;
  dragStartX = e.clientX;
  dragStartWidth = sidebar.offsetWidth;
  splitter.classList.add('dragging');
  document.body.style.cursor = 'col-resize';
  document.body.style.userSelect = 'none';
});

document.addEventListener('mousemove', (e) => {
  if (!dragging) return;
  const delta = e.clientX - dragStartX;
  const newWidth = Math.max(120, Math.min(600, dragStartWidth + delta));
  sidebar.style.width = `${newWidth}px`;
});

document.addEventListener('mouseup', () => {
  if (!dragging) return;
  dragging = false;
  splitter.classList.remove('dragging');
  document.body.style.cursor = '';
  document.body.style.userSelect = '';
});
