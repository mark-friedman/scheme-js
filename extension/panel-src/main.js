/**
 * @fileoverview Entry point for the Scheme-JS DevTools panel.
 *
 * Initializes the panel UI components and wires them together:
 *   - Toolbar: debug controls (Resume, Step Into/Over/Out) + status
 *   - Source list: file browser, click to load into editor
 *   - Editor: CodeMirror source viewer with breakpoint gutter
 *   - Call stack: displays Scheme and JS frames when paused
 *   - Variables: displays locals for the selected frame
 *
 * Pause/resume notifications arrive from two sources:
 *   1. Scheme: via chrome.runtime.onMessage relayed from the content script
 *   2. JS: via CDP events forwarded from background.js
 *
 * All step/resume/breakpoint commands are routed through the unified debugger,
 * which determines whether to use the Scheme bridge or CDP bridge based on
 * the current pause context and file type.
 *
 * Breakpoints are persisted to chrome.storage.local so they survive page
 * reloads. On activation, stored breakpoints are re-sent to the interpreter.
 */

import { createEditor } from './components/editor.js';
import { createSourceList } from './components/source-list.js';
import { createToolbar } from './components/toolbar.js';
import { createCallStack } from './components/call-stack.js';
import { createVariables } from './components/variables.js';
import { createBreakpointsList } from './components/breakpoints.js';
import { createConsole } from './components/console.js';
import {
  evalInPage,
  getSourceContent,
  getExpressions,
  activate,
  getStatus,
  getStack,
  getLocals,
  ackPause,
  setTabId,
  getTabId,
} from './protocol/scheme-bridge.js';
import * as unifiedDebugger from './protocol/unified-debugger.js';
import * as cdpBridge from './protocol/cdp-bridge.js';
import * as bpState from './breakpoint-state.js';
import { initSplitter } from './splitter.js';
import { MSG, PAUSE_CONTEXT } from './protocol/constants.js';

// =========================================================================
// Tab ID from URL parameter (standalone window) or DevTools API
// =========================================================================

const _urlParams = new URLSearchParams(window.location.search);
const _urlTabId = parseInt(_urlParams.get('tabId'), 10);
if (!isNaN(_urlTabId)) {
  setTabId(_urlTabId);
}

// =========================================================================
// DOM elements
// =========================================================================

const toolbarDebug       = document.getElementById('toolbar-debug');
const sourceListContainer = document.getElementById('source-list');
const editorContainer    = document.getElementById('editor-container');
const callStackContainer = document.getElementById('call-stack-container');
const variablesContainer = document.getElementById('variables-container');
const breakpointsContainer = document.getElementById('breakpoints-container');
const consoleContainer   = document.getElementById('console-container');
const sidebar            = document.getElementById('sidebar');
const splitter           = document.getElementById('splitter');

// =========================================================================
// State
// =========================================================================

// Expose unifiedDebugger on window for testing
if (typeof window !== 'undefined') {
  window.unifiedDebugger = unifiedDebugger;
}

/** Currently displayed source URL. @type {string|null} */
let currentSourceUrl = null;

/**
 * Safety timeout handle for step operations. If a step command doesn't
 * result in a pause or resume within 3 seconds, auto-resume the panel UI.
 * @type {number|null}
 */
let stepTimeoutHandle = null;

/** Whether CDP is attached (used for "JS debugging enabled" notification). */
let cdpAttached = false;

/**
 * Expression spans for the currently loaded source.
 * Populated by loadSource/onSelectSource via getExpressions().
 * @type {Array<{exprId: number, line: number, column: number, endLine: number, endColumn: number}>}
 */
let currentExpressions = [];

// =========================================================================
// Initialize toolbar
// =========================================================================

/**
 * Starts the step timeout safety net. If no pause/resume arrives
 * within 3 seconds after a step command, auto-resume the panel UI.
 */
function startStepTimeout() {
  clearStepTimeout();
  stepTimeoutHandle = setTimeout(() => {
    stepTimeoutHandle = null;
    onResumed();
  }, 3000);
}

/**
 * Clears any pending step timeout.
 */
function clearStepTimeout() {
  if (stepTimeoutHandle !== null) {
    clearTimeout(stepTimeoutHandle);
    stepTimeoutHandle = null;
  }
}

const toolbar = createToolbar(toolbarDebug, {
  onResume:   () => unifiedDebugger.resume(),
  onStepInto: () => { unifiedDebugger.stepInto(); startStepTimeout(); },
  onStepOver: () => { unifiedDebugger.stepOver(); startStepTimeout(); },
  onStepOut:  () => { unifiedDebugger.stepOut(); startStepTimeout(); },
});

// =========================================================================
// Initialize variables panel
// =========================================================================

const variables = createVariables(variablesContainer);

// =========================================================================
// Initialize breakpoints list
// =========================================================================

const breakpointsList = createBreakpointsList(breakpointsContainer, {
  onClickBreakpoint: async (url, line, column) => {
    try {
      if (url !== currentSourceUrl) {
        if (unifiedDebugger.isJSFile(url)) {
          await loadJSSource(url);
        } else {
          await loadSource(url);
        }
      }
      editor.highlightLine(line);
    } catch (err) {
      console.error('[breakpoint-click] navigation error:', err);
    }
  },
  onRemoveBreakpoint: async (id, url, line, column) => {
    if (bpState.has(url, line, column)) {
      await unifiedDebugger.removeBreakpoint(id, url);
      bpState.remove(url, line, column);
      persistBreakpointChange();

      // If the removed breakpoint was on the currently viewed source, update the editor
      if (url === currentSourceUrl) {
        if (!column) {
          const lines = bpState.getLinesForUrl(url);
          editor.setBreakpoints(lines);
        }
        refreshDiamondMarkers();
      }
    }
  }
});

/**
 * Refreshes the breakpoints panel from the current state.
 */
function refreshBreakpointsPanel() {
  breakpointsList.setBreakpoints(bpState.getAllBreakpoints());
}

// =========================================================================
// Initialize eval console
// =========================================================================

let selectedUnifiedFrame = null;
let selectedSchemeFrameIndex = 0;

const evalConsole = createConsole(consoleContainer, {
  onEvaluate: async (expression) => {
    if (!selectedUnifiedFrame) {
      return { success: false, error: 'Not paused', result: null };
    }
    return unifiedDebugger.evalInFrame(selectedUnifiedFrame, selectedSchemeFrameIndex, expression);
  }
});

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
  selectedUnifiedFrame = frame;

  // Calculate scheme frame index (bottom up, skipping JS frames)
  const unifiedFrames = unifiedDebugger.getUnifiedFrames();
  selectedSchemeFrameIndex = computeSchemeFrameIndex(unifiedFrames, frameIndex);

  // JS frames have their own variable inspection via CDP scope chain
  if (frame.language === 'js') {
    const jsLocals = formatJSLocals(frame._cdpScopeChain);
    if (jsLocals.length > 0) {
      variables.setLocals(jsLocals);
    } else {
      variables.clear();
    }

    // Navigate editor to JS source
    if (frame.source) {
      const url = frame.source.filename;
      if (url !== currentSourceUrl) {
        await loadJSSource(url, frame._cdpScriptId);
      }
      editor.highlightLine(frame.source.line);
      editor.highlightExpression(null);
    } else {
      editor.highlightLine(null);
      editor.highlightExpression(null);
    }
    return;
  }

  // Scheme frames — existing behavior
  // Load locals for this frame (context-aware: routes through CDP during sync pauses)
  try {
    const locals = await unifiedDebugger.getLocalsForContext(selectedSchemeFrameIndex);
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
  if (isNowSet) {
    const id = await unifiedDebugger.setBreakpoint(currentSourceUrl, line);
    if (id) {
      bpState.set(currentSourceUrl, line, null, id);
      persistBreakpointChange();
    }
  } else {
    const id = bpState.getId(currentSourceUrl, line, null);
    if (id) {
      await unifiedDebugger.removeBreakpoint(id, currentSourceUrl);
      bpState.remove(currentSourceUrl, line, null);
      persistBreakpointChange();
    }
  }
  // Show/hide diamond markers on this line
  refreshDiamondMarkers();

  // If CDP just auto-attached, update the notification
  if (cdpBridge.isAttached() && !cdpAttached) {
    cdpAttached = true;
    showCDPNotification();
  }
}

/**
 * Called when the user clicks an inline diamond marker to toggle an expression breakpoint.
 *
 * @param {number} line - 1-indexed line number
 * @param {number} column - 1-indexed column number
 */
async function onDiamondClick(line, column) {
  if (!currentSourceUrl) return;

  const existingId = bpState.getId(currentSourceUrl, line, column);

  if (existingId) {
    // Remove existing expression breakpoint
    await unifiedDebugger.removeBreakpoint(existingId, currentSourceUrl);
    bpState.remove(currentSourceUrl, line, column);
  } else {
    // Set new expression breakpoint
    const id = await unifiedDebugger.setBreakpoint(currentSourceUrl, line, column);
    if (id) {
      bpState.set(currentSourceUrl, line, column, id);
    }
  }

  persistBreakpointChange();
  refreshDiamondMarkers();

}

const editor = createEditor(editorContainer, onBreakpointToggle, onDiamondClick);

/** The currently paused line (1-indexed), or null when running. @type {number|null} */
let currentPausedLine = null;

/**
 * Formats CDP scope chain entries into the locals format used by the variables panel.
 * Extracts local and closure scopes from the CDP scope chain.
 *
 * @param {Array<{type: string, name: string}>} scopeChain - CDP scope chain entries
 * @returns {Array<{name: string, value: string, type: string, subtype: null}>}
 */
function formatJSLocals(scopeChain) {
  if (!scopeChain || scopeChain.length === 0) return [];
  const jsLocals = [];
  for (const scope of scopeChain) {
    if (scope.type === 'local' || scope.type === 'closure') {
      jsLocals.push({
        name: `[${scope.type}]`,
        value: scope.name || 'scope',
        type: 'other',
        subtype: null,
      });
    }
  }
  return jsLocals;
}

/**
 * Computes the Scheme-only frame index from a unified frame index by
 * subtracting the number of JS frames below the given position.
 *
 * @param {Array<{language: string}>} unifiedFrames - Merged call stack
 * @param {number} frameIndex - Index within the unified stack
 * @returns {number} Index within the Scheme-only stack
 */
function computeSchemeFrameIndex(unifiedFrames, frameIndex) {
  let jsFrameCountBelow = 0;
  for (let i = frameIndex - 1; i >= 0; i--) {
    if (unifiedFrames[i].language === 'js') {
      jsFrameCountBelow++;
    }
  }
  return frameIndex - jsFrameCountBelow;
}

/**
 * Persists breakpoint state to the page and refreshes the breakpoints panel.
 * Common operation after any breakpoint add/remove.
 */
function persistBreakpointChange() {
  bpState.saveToPage();
  refreshBreakpointsPanel();
}

/**
 * Recomputes and updates the inline diamond markers in the editor.
 *
 * Diamonds appear at expression start positions on lines that:
 *   1. Have a line-level breakpoint (gutter dot), OR
 *   2. Are the current paused line
 *
 * Each diamond is either filled (active expression breakpoint) or hollow.
 * Clicking a diamond toggles an expression breakpoint at that position.
 */
function refreshDiamondMarkers() {
  if (!currentSourceUrl || currentExpressions.length === 0) {
    editor.setDiamondMarkers([]);
    return;
  }

  // Collect lines that should show diamonds
  const diamondLines = new Set();
  const bpLines = bpState.getLinesForUrl(currentSourceUrl);
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
      const active = bpState.has(currentSourceUrl, expr.line, expr.column);
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
  // Use context-aware getters: routes through CDP during sync-path pauses
  const content = await unifiedDebugger.getSourceContentForContext(url);
  if (content === null) return;
  currentSourceUrl = url;
  sourceList.selectUrl(url);
  editor.setContent(content);

  // Fetch expression spans for this source
  currentExpressions = await unifiedDebugger.getExpressionsForContext(url);

  // Show line-level breakpoints in the gutter
  const lines = bpState.getLinesForUrl(url);
  if (lines.size > 0) {
    editor.setBreakpoints(lines);
  }

  // Show diamond markers on breakpoint and paused lines
  refreshDiamondMarkers();

  // Only update status if not currently paused (avoid overwriting pause status)
  if (currentPausedLine === null) {
    toolbar.setStatus(`Viewing: ${url.split('/').pop()}`);
  }
}

/**
 * Loads a JavaScript source file into the editor from CDP.
 * Switches the editor to JavaScript syntax highlighting mode.
 *
 * @param {string} url - JS file URL
 * @param {string} [scriptId] - CDP script ID (used to fetch source from CDP)
 */
async function loadJSSource(url, scriptId) {
  let content = null;

  // Try fetching via CDP if we have a scriptId
  if (scriptId && cdpBridge.isAttached()) {
    content = await cdpBridge.getJSSource(scriptId);
  }

  if (content === null) {
    content = `// Source not available: ${url}`;
  }

  currentSourceUrl = url;
  sourceList.selectUrl(url);
  // No expression spans for JS files
  currentExpressions = [];
  editor.setContent(content, 'javascript');

  // Show line-level breakpoints in the gutter (if any were set on this JS file)
  const lines = bpState.getLinesForUrl(url);
  if (lines.size > 0) {
    editor.setBreakpoints(lines);
  }

  // Only update status if not currently paused (avoid overwriting pause status)
  if (currentPausedLine === null) {
    toolbar.setStatus(`Viewing: ${url.split('/').pop()}`);
  }
}

/**
 * Shows a "JavaScript debugging enabled" notification in the toolbar.
 * Called once when CDP first attaches (triggered by setJSBreakpoint auto-attach).
 */
function showCDPNotification() {
  const toolbarEl = document.querySelector('.toolbar-controls');
  if (toolbarEl && !document.querySelector('.cdp-notification')) {
    const notification = document.createElement('span');
    notification.className = 'cdp-notification';
    notification.textContent = '\u26A1 JS debugging enabled';
    notification.title = 'Chrome Debugger Protocol is attached for JavaScript debugging';
    toolbarEl.appendChild(notification);
  }
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
  const lines = bpState.getLinesForUrl(url);
  if (lines.size > 0) {
    editor.setBreakpoints(lines);
  }

  // Show diamond markers on breakpoint lines
  refreshDiamondMarkers();

  // Only update status if not currently paused (avoid overwriting pause status)
  if (currentPausedLine === null) {
    toolbar.setStatus(`Viewing: ${url.split('/').pop()}`);
  }
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
 * Handles both Scheme and CDP pauses via the unified debugger.
 *
 * @param {{reason: string, source: Object|null, stack: Array, context: string}} detail
 */
async function onPaused(detail) {
  clearStepTimeout();
  const reason = detail.reason === 'step' ? 'Paused (step)' : 'Paused at breakpoint';
  const context = detail.context || 'scheme';
  const suffix = context === PAUSE_CONTEXT.JS ? ' [JS]' : '';
  // Show last expression result for step pauses (like Chrome's "← value")
  const lastResult = detail.lastResult;
  const resultSuffix = (lastResult != null && detail.reason === 'step')
    ? `  \u2190 ${lastResult}` : '';
  toolbar.setStatus(reason + suffix + resultSuffix);
  toolbar.setPaused();

  // Acknowledge the pause so the safety timeout is cancelled (Scheme pauses only)
  if (context === PAUSE_CONTEXT.SCHEME) {
    ackPause();
  }

  const stack = detail.frames || detail.stack || [];
  callStack.setFrames(stack, /* suppressAutoSelect */ true);

  // Use detail.source (the actual pause location) for highlighting
  const pauseSource = detail.source;
  if (pauseSource && pauseSource.filename) {
    const isJS = unifiedDebugger.isJSFile(pauseSource.filename);

    // Set paused line BEFORE loading source (so loadJSSource knows we're paused)
    currentPausedLine = pauseSource.line;

    if (pauseSource.filename !== currentSourceUrl) {
      if (isJS) {
        // Find the CDP frame with this source to get scriptId
        const jsFrame = stack.find(f => f.language === 'js' && f.source?.filename === pauseSource.filename);
        await loadJSSource(pauseSource.filename, jsFrame?._cdpScriptId);
      } else {
        await loadSource(pauseSource.filename);
      }
    }
    editor.highlightLine(pauseSource.line);

    // Highlight the exact expression range if available (Scheme only)
    if (!isJS && pauseSource.endLine != null && pauseSource.endColumn != null) {
      editor.highlightExpression(
        pauseSource.line, pauseSource.column,
        pauseSource.endLine, pauseSource.endColumn
      );
    } else {
      editor.highlightExpression(null);
    }

    // Show diamond markers on the paused line (+ any breakpoint lines)
    refreshDiamondMarkers();
  }

  // Set the selected frame to the top frame so eval console works immediately.
  // (callStack.setFrames uses suppressAutoSelect so onSelectFrame is not called.)
  if (stack.length > 0) {
    const topFrameIndex = stack.length - 1;
    selectedUnifiedFrame = stack[topFrameIndex];
    selectedSchemeFrameIndex = computeSchemeFrameIndex(stack, topFrameIndex);
  }

  // Load locals for the top frame
  if (stack.length > 0) {
    const topFrame = stack[stack.length - 1];
    if (topFrame.language === 'js') {
      const jsLocals = formatJSLocals(topFrame._cdpScopeChain);
      if (jsLocals.length > 0) {
        variables.setLocals(jsLocals);
      } else {
        variables.clear();
      }
    } else {
      try {
        // Use context-aware getter: routes through CDP during sync-path pauses
        const locals = await unifiedDebugger.getLocalsForContext(selectedSchemeFrameIndex);
        variables.setLocals(locals);
      } catch {
        variables.clear();
      }
    }
  }
}

/**
 * Called when the interpreter resumes execution.
 */
function onResumed() {
  clearStepTimeout();
  toolbar.setStatus('Running');
  toolbar.setRunning();
  callStack.clear();
  variables.clear();
  evalConsole.clear();
  selectedUnifiedFrame = null;
  selectedSchemeFrameIndex = 0;
  editor.highlightLine(null);
  editor.highlightExpression(null);
  currentPausedLine = null;
  refreshDiamondMarkers();
}

// =========================================================================
// Chrome runtime message listener (relayed from content script)
// =========================================================================

if (typeof chrome !== 'undefined' && chrome.runtime?.onMessage) {
  chrome.runtime.onMessage.addListener((message) => {
    // Filter messages by tabId to prevent cross-talk between multiple debugger windows
    const myTabId = getTabId();
    if (message.tabId && myTabId && message.tabId !== myTabId) return;

    if (message.type === MSG.SCHEME_DEBUG_PAUSED) {
      // Route through unified debugger for consistent handling
      unifiedDebugger.handleSchemePause(message.detail || {});
    } else if (message.type === MSG.SCHEME_DEBUG_RESUMED) {
      unifiedDebugger.handleSchemeResume();
    } else if (message.type === MSG.SCHEME_SYNC_PAUSED) {
      // Sync-path probe pause: page is halted at debugger; in a DOM callback.
      // Stack was pre-fetched by background.js. Route through unified debugger.
      unifiedDebugger.handleSyncSchemePause(message);
    } else if (message.type === MSG.CONSOLE_API_CALLED) {
      // Page console output forwarded from background.js via Runtime.consoleAPICalled
      evalConsole.addOutput(message.callType, message.args || []);
    }
  });
}

// Initialize the unified debugger and wire its events to the panel
unifiedDebugger.init();
unifiedDebugger.onPaused((event) => onPaused(event));
unifiedDebugger.onResumed(() => onResumed());

// Track CDP attachment for the UI notification
cdpBridge.onScriptParsed((event) => {
  if (event.type === MSG.CDP_ATTACHED && !cdpAttached) {
    cdpAttached = true;
    showCDPNotification();
  }
});

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
      // Attach CDP so background.js can intercept sync-path probe pauses
      // (debugger; statements in DOM callbacks). Without this, Chrome's
      // built-in Sources tab catches the pause instead of the panel.
      cdpBridge.attachCDP().catch(() => {});

      // Sync panel state with breakpoints already set in the interpreter
      await bpState.syncFromInterpreter();
      refreshBreakpointsPanel();
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
activateAndRefresh();

// Re-activate and refresh after each page navigation so the panel stays in
// sync with the new page's sources and breakpoints.
// Standalone window: listen for chrome.tabs.onUpdated
// DevTools panel: listen for chrome.devtools.network.onNavigated
if (typeof chrome !== 'undefined' && chrome.tabs?.onUpdated) {
  const myTabId = getTabId();
  chrome.tabs.onUpdated.addListener((updatedTabId, changeInfo) => {
    if (updatedTabId === myTabId && changeInfo.status === 'complete') {
      cdpBridge.clearNavigationCache();
      setTimeout(() => activateAndRefresh(), 500);
    }
  });
} else if (typeof chrome !== 'undefined' && chrome.devtools?.network) {
  chrome.devtools.network.onNavigated.addListener(() => {
    cdpBridge.clearNavigationCache();
    setTimeout(() => activateAndRefresh(), 500);
  });
}

// Re-refresh when the panel regains focus (user switches back to this tab).
// Debounce to avoid re-rendering while the user is clicking within the panel.
// Works in both standalone window and DevTools panel modes.
let lastRefreshTime = 0;
window.addEventListener('focus', () => {
  const now = Date.now();
  if (now - lastRefreshTime > 2000) {
    lastRefreshTime = now;
    refresh();
  }
});

// =========================================================================
// Splitter drag behavior
// =========================================================================

initSplitter(splitter, {
  direction: 'horizontal',
  target: sidebar,
  min: 120,
  max: 600,
});

const consoleSplitter = document.getElementById('console-splitter');
initSplitter(consoleSplitter, {
  direction: 'vertical',
  target: consoleContainer,
  min: 50,
  max: 600,
  invert: true,
});

// =========================================================================
// DevTools Theme Matching
// =========================================================================

// Theme detection: DevTools API when available, otherwise prefers-color-scheme.
if (typeof chrome !== 'undefined' && chrome.devtools?.panels) {
  const updateTheme = (themeName) => {
    if (themeName === 'default' || themeName === 'light') {
      document.documentElement.classList.add('theme-light');
      document.documentElement.classList.remove('theme-dark');
    } else {
      document.documentElement.classList.add('theme-dark');
      document.documentElement.classList.remove('theme-light');
    }
  };

  updateTheme(chrome.devtools.panels.themeName);

  if (chrome.devtools.panels.setThemeChangeHandler) {
    chrome.devtools.panels.setThemeChangeHandler(updateTheme);
  }
} else {
  // Standalone window: use prefers-color-scheme media query
  const darkQuery = window.matchMedia('(prefers-color-scheme: dark)');
  const updateThemeFromMedia = (isDark) => {
    document.documentElement.classList.toggle('theme-dark', isDark);
    document.documentElement.classList.toggle('theme-light', !isDark);
  };
  updateThemeFromMedia(darkQuery.matches);
  darkQuery.addEventListener('change', (e) => updateThemeFromMedia(e.matches));
}
