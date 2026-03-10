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
  getExpressions,
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
 * Expression spans for the currently loaded source.
 * Populated by loadSource/onSelectSource via getExpressions().
 * @type {Array<{exprId: number, line: number, column: number, endLine: number, endColumn: number}>}
 */
let currentExpressions = [];

/**
 * Maps "${url}:${line}:${column}" -> breakpoint ID (from interpreter) for removal.
 * Column is "null" for line-level breakpoints, or a number for expression breakpoints.
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
    // Key format: "${url}:${line}:${column}" where column may be "null"
    const parts = key.split(':');
    const column = parts.pop();
    const line = parts.pop();
    const url = parts.join(':');
    const entry = { url, line: parseInt(line, 10) };
    if (column !== 'null') entry.column = parseInt(column, 10);
    entries.push(entry);
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
      breakpointIds.set(`${bp.filename}:${bp.line}:${bp.column}`, bp.id);
    }
  } catch { /* ignore */ }
}

/**
 * Returns the set of stored line-level breakpoint line numbers for a specific URL.
 *
 * @param {string} url - Source URL to filter by
 * @returns {Set<number>} Line numbers with line-level breakpoints for this URL
 */
function getBreakpointLinesForUrl(url) {
  const lines = new Set();
  for (const key of breakpointIds.keys()) {
    // Key format: "${url}:${line}:${column}"
    const parts = key.split(':');
    const column = parts.pop();
    const line = parts.pop();
    const bpUrl = parts.join(':');
    // Only include line-level breakpoints (column === "null") in the gutter
    if (bpUrl === url && column === 'null') {
      lines.add(parseInt(line, 10));
    }
  }
  return lines;
}

/**
 * Returns expression breakpoint spans for a URL, looked up from currentExpressions.
 *
 * @param {string} url - Source URL
 * @returns {Array<{line: number, column: number, endLine: number, endColumn: number}>}
 */
function getExpressionBreakpointsForUrl(url) {
  const spans = [];
  for (const key of breakpointIds.keys()) {
    const parts = key.split(':');
    const column = parts.pop();
    const line = parts.pop();
    const bpUrl = parts.join(':');
    if (bpUrl === url && column !== 'null') {
      // Find the matching expression span
      const lineNum = parseInt(line, 10);
      const colNum = parseInt(column, 10);
      const span = currentExpressions.find(
        e => e.line === lineNum && e.column === colNum
      );
      if (span) {
        spans.push({ line: span.line, column: span.column, endLine: span.endLine, endColumn: span.endColumn });
      }
    }
  }
  return spans;
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

    // Highlight expression range if available
    if (frame.source.endLine != null && frame.source.endColumn != null) {
      editor.highlightExpression(
        frame.source.line, frame.source.column,
        frame.source.endLine, frame.source.endColumn
      );
    } else {
      editor.highlightExpression(null);
    }
  } else {
    editor.highlightLine(null);
    editor.highlightExpression(null);
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
  const key = `${currentSourceUrl}:${line}:null`;
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
  // Show/hide diamond markers on this line
  refreshDiamondMarkers();
}

/**
 * Called when the user clicks an inline diamond marker to toggle an expression breakpoint.
 *
 * @param {number} line - 1-indexed line number
 * @param {number} column - 1-indexed column number
 */
async function onDiamondClick(line, column) {
  if (!currentSourceUrl) return;

  const key = `${currentSourceUrl}:${line}:${column}`;
  const existingId = breakpointIds.get(key);

  if (existingId) {
    // Remove existing expression breakpoint
    await removeBreakpoint(existingId);
    breakpointIds.delete(key);
  } else {
    // Set new expression breakpoint
    const id = await setBreakpoint(currentSourceUrl, line, column);
    if (id) {
      breakpointIds.set(key, id);
    }
  }

  saveBreakpointsToPage();
  refreshDiamondMarkers();

  // Refresh expression breakpoint highlights
  const exprBps = getExpressionBreakpointsForUrl(currentSourceUrl);
  editor.setExpressionBreakpoints(exprBps);
}

const editor = createEditor(editorContainer, onBreakpointToggle, onDiamondClick);

/** The currently paused line (1-indexed), or null when running. @type {number|null} */
let currentPausedLine = null;

/**
 * Recomputes and updates the inline diamond markers in the editor.
 *
 * Diamonds appear at expression start positions on lines that:
 *   1. Have a line-level breakpoint (gutter dot), OR
 *   2. Are the current paused line
 *
 * Each diamond is either filled (◆, active expression breakpoint) or hollow (◇).
 * Clicking a diamond toggles an expression breakpoint at that position.
 */
function refreshDiamondMarkers() {
  if (!currentSourceUrl || currentExpressions.length === 0) {
    editor.setDiamondMarkers([]);
    return;
  }

  // Collect lines that should show diamonds
  const diamondLines = new Set();
  const bpLines = getBreakpointLinesForUrl(currentSourceUrl);
  for (const line of bpLines) diamondLines.add(line);
  if (currentPausedLine !== null) diamondLines.add(currentPausedLine);

  if (diamondLines.size === 0) {
    editor.setDiamondMarkers([]);
    return;
  }

  // Only show diamonds on lines with multiple expressions (otherwise the
  // line-level breakpoint is sufficient). Also skip the outermost expression
  // on a line since that's what the line breakpoint already covers.
  const exprsByLine = new Map();
  for (const expr of currentExpressions) {
    if (!diamondLines.has(expr.line)) continue;
    // Only include expressions that START on this line
    if (!exprsByLine.has(expr.line)) exprsByLine.set(expr.line, []);
    exprsByLine.get(expr.line).push(expr);
  }

  const markers = [];
  for (const [lineNum, exprs] of exprsByLine) {
    // Skip lines with only one expression (the line breakpoint handles it)
    if (exprs.length <= 1) continue;

    // Sort by column to show diamonds left-to-right
    exprs.sort((a, b) => a.column - b.column);

    // Skip the outermost (first/leftmost) expression — that's the line BP
    for (let i = 1; i < exprs.length; i++) {
      const expr = exprs[i];
      const key = `${currentSourceUrl}:${expr.line}:${expr.column}`;
      const active = breakpointIds.has(key);
      markers.push({ line: expr.line, column: expr.column, active });
    }
  }

  editor.setDiamondMarkers(markers);
}

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

  // Fetch expression spans for this source
  currentExpressions = await getExpressions(url);

  // Show line-level breakpoints in the gutter
  const lines = getBreakpointLinesForUrl(url);
  if (lines.size > 0) {
    editor.setBreakpoints(lines);
  }

  // Show expression-level breakpoints as inline highlights
  const exprBps = getExpressionBreakpointsForUrl(url);
  if (exprBps.length > 0) {
    editor.setExpressionBreakpoints(exprBps);
  }

  // Show diamond markers on breakpoint and paused lines
  refreshDiamondMarkers();
}

/**
 * Called when a source file is clicked in the file list.
 * Loads the content and syncs breakpoints for the selected file.
 *
 * @param {string} url - scheme:// URL of the selected source
 * @param {string} content - The source code
 */
async function onSelectSource(url, content) {
  currentSourceUrl = url;
  editor.setContent(content || '');
  editor.highlightLine(null);

  // Fetch expression spans for this source
  currentExpressions = await getExpressions(url);

  // Sync line-level breakpoints for this file from local state
  const lines = getBreakpointLinesForUrl(url);
  if (lines.size > 0) {
    editor.setBreakpoints(lines);
  }

  // Sync expression-level breakpoints
  const exprBps = getExpressionBreakpointsForUrl(url);
  if (exprBps.length > 0) {
    editor.setExpressionBreakpoints(exprBps);
  }

  // Show diamond markers on breakpoint lines
  refreshDiamondMarkers();

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

    // Highlight the exact expression range if available
    if (pauseSource.endLine != null && pauseSource.endColumn != null) {
      editor.highlightExpression(
        pauseSource.line, pauseSource.column,
        pauseSource.endLine, pauseSource.endColumn
      );
    } else {
      editor.highlightExpression(null);
    }

    // Show diamond markers on the paused line (+ any breakpoint lines)
    currentPausedLine = pauseSource.line;
    refreshDiamondMarkers();
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
  editor.highlightExpression(null);
  currentPausedLine = null;
  refreshDiamondMarkers();
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
