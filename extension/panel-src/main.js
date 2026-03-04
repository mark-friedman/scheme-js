/**
 * @fileoverview Entry point for the Scheme-JS DevTools panel.
 *
 * Initializes the panel UI: CodeMirror editor and source file list.
 * When a source is selected in the file list, it loads into the editor.
 * The source list is refreshed when the panel is shown.
 */

import { createEditor } from './components/editor.js';
import { createSourceList } from './components/source-list.js';

// =========================================================================
// DOM elements
// =========================================================================

const editorContainer = document.getElementById('editor-container');
const sourceListContainer = document.getElementById('source-list');
const statusBar = document.getElementById('status-bar');

// =========================================================================
// Initialize editor
// =========================================================================

const editor = createEditor(editorContainer);

// =========================================================================
// Initialize source list
// =========================================================================

/**
 * Called when a source file is selected in the file list.
 * Loads the content into the editor.
 *
 * @param {string} url - scheme:// URL of the selected source
 * @param {string} content - The source code
 */
function onSelectSource(url, content) {
  editor.setContent(content || '');
  setStatus(`Viewing: ${url.split('/').pop()}`);
}

const sourceList = createSourceList(sourceListContainer, onSelectSource);

// =========================================================================
// Status bar
// =========================================================================

/**
 * Updates the status bar text.
 * @param {string} text
 */
function setStatus(text) {
  statusBar.textContent = text;
}

// =========================================================================
// Refresh on navigation / show
// =========================================================================

/**
 * Refresh the source list. Called on panel show and when the page navigates.
 */
async function refresh() {
  setStatus('Refreshing...');
  await sourceList.refresh();
  setStatus('Ready');
}

// Refresh when the panel first becomes visible
refresh();

// Refresh when the DevTools panel is shown (re-activated)
if (typeof chrome !== 'undefined' && chrome.devtools?.panels) {
  // chrome.devtools.panels.onShown fires when the panel is opened/re-opened
  // We can't access this from inside the panel page directly, so we listen
  // for focus events as an approximation.
  window.addEventListener('focus', () => refresh());
}

// =========================================================================
// Splitter drag behavior
// =========================================================================

const sidebar = document.getElementById('sidebar');
const splitter = document.getElementById('splitter');

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
