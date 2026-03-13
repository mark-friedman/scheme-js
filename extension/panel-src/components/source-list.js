/**
 * @fileoverview Source file list component for the Scheme-JS panel.
 *
 * Fetches registered Scheme sources from the page via __schemeDebug.getSources()
 * and renders a clickable file list. Clicking a file loads it into the editor.
 */

import { getSources, isSchemeDebugAvailable } from '../protocol/scheme-bridge.js';

/**
 * Creates and manages the source file list sidebar.
 *
 * @param {HTMLElement} container - Element to render the file list into
 * @param {Function} onSelectSource - Callback when a source is selected: (url, content) => void
 * @returns {{refresh: Function}}
 */
export function createSourceList(container, onSelectSource) {
  /** @type {Array<{url: string, content: string, lines: number, origin: string}>} */
  let sources = [];

  /** @type {string|null} Currently selected URL */
  let selectedUrl = null;

  /**
   * Extracts a display name from a scheme:// URL.
   * @param {string} url
   * @returns {string}
   */
  function displayName(url) {
    // scheme://inline-scripts/script-0.scm -> script-0.scm (inline)
    // scheme://scheme-sources/factorial.scm -> factorial.scm
    const basename = url.split('/').pop() || url;
    const origin = url.includes('/inline-scripts/') ? 'inline'
      : url.includes('/repl/') ? 'repl'
      : url.includes('//lib/') ? 'lib'
      : null;
    return origin ? `${basename} [${origin}]` : basename;
  }

  /**
   * Renders the file list from current sources array.
   */
  function render() {
    if (sources.length === 0) {
      container.innerHTML = '<div class="source-empty">No Scheme sources loaded.<br>Run a page with &lt;script type="text/scheme"&gt;.</div>';
      return;
    }

    const items = sources.map(src => {
      const isSelected = src.url === selectedUrl;
      const name = displayName(src.url);
      return `<div class="source-item${isSelected ? ' selected' : ''}" data-testid="source-item" data-url="${escapeAttr(src.url)}" title="${escapeAttr(src.url)}">${escapeHtml(name)}</div>`;
    });

    container.innerHTML = items.join('');
  }

  /**
   * Refreshes the source list from the page.
   * Safe to call repeatedly; shows placeholder if not available.
   */
  async function refresh() {
    const available = await isSchemeDebugAvailable();
    if (!available) {
      container.innerHTML = '<div class="source-empty">scheme-js not detected on this page.</div>';
      return;
    }

    const fetched = await getSources();
    if (fetched.length > 0) {
      sources = fetched;
      render();

      // Auto-select first source if nothing selected yet
      if (!selectedUrl && sources.length > 0) {
        selectedUrl = sources[0].url;
        render();
        onSelectSource(sources[0].url, sources[0].content);
      }
    } else {
      render(); // shows "no sources" message
    }
  }

  // Use event delegation so we don't re-attach click listeners on every render
  container.addEventListener('click', (e) => {
    const el = e.target.closest('.source-item');
    if (!el) return;
    const url = el.dataset.url;
    const src = sources.find(s => s.url === url);
    if (src) {
      selectedUrl = url;
      render(); // re-render to update selection
      onSelectSource(url, src.content);
    }
  });

  // Initial render placeholder
  container.innerHTML = '<div class="source-empty">Loading sources...</div>';

  return { refresh };
}

/**
 * Escapes HTML special characters.
 * @param {string} text
 * @returns {string}
 */
function escapeHtml(text) {
  return String(text)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

/**
 * Escapes a string for use in an HTML attribute.
 * @param {string} text
 * @returns {string}
 */
function escapeAttr(text) {
  return escapeHtml(text).replace(/"/g, '&quot;');
}
