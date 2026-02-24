/**
 * @fileoverview DevTools entry point for the Scheme Stack extension.
 *
 * Creates a sidebar pane in the Sources panel that displays the
 * Scheme call stack when paused at a breakpoint.
 */

// Create the "Scheme Stack" sidebar pane in the Sources panel
chrome.devtools.panels.sources.createSidebarPane(
    'Scheme Stack',
    (sidebar) => {
        sidebar.setPage('panel/sidebar.html');
    }
);
