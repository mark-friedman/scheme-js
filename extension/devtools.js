/**
 * @fileoverview DevTools entry point for the Scheme-JS extension.
 *
 * Creates a top-level "Scheme-JS" panel in Chrome DevTools,
 * replacing the old Sources sidebar pane.
 */

// Create the top-level "Scheme-JS" panel
chrome.devtools.panels.create(
  'Scheme-JS',          // Panel title (shown as a tab in DevTools)
  '',                   // Icon URL (empty = no icon)
  'panel/panel.html',   // Panel page
  (_panel) => {
    // Panel created — nothing to do here for Phase 1.
    // Phase 2 will add onShown/onHidden listeners.
  }
);
