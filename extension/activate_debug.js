/**
 * @fileoverview Early activation script for Scheme-JS debugging.
 *
 * Injected into the page's MAIN world at document_start (before any
 * other scripts run) so that:
 *
 *   1. The HTML adapter's isDevToolsDebugRequested() sees __SCHEME_JS_DEBUG
 *      as true and creates the async debug runtime.
 *
 *   2. Any breakpoints persisted in localStorage by the DevTools panel are
 *      loaded into __SCHEME_JS_BREAKPOINTS so that html_adapter.js can
 *      install them in the BreakpointManager before Scheme scripts run.
 *
 * This runs in the page's own JavaScript context (world: "MAIN"),
 * not in the extension's isolated content script world.
 */
globalThis.__SCHEME_JS_DEBUG = true;

// Pre-load persisted breakpoints so they're in place before scripts execute.
try {
  const stored = localStorage.getItem('schemeJS_breakpoints');
  if (stored) {
    globalThis.__SCHEME_JS_BREAKPOINTS = JSON.parse(stored);
  }
} catch {
  // Ignore parse errors or restricted localStorage access
}

// If the DevTools panel was connected in the previous session, set the flag
// so html_adapter.js can enable blocking pauses before scripts run.
// This is the key timing fix: the panel calls activate() after page load,
// which saves this flag so the NEXT reload starts with panelConnected=true.
// sessionStorage is tab-scoped, so this only affects the tab where the
// DevTools panel is attached — not other tabs or diagnostic test pages.
try {
  if (sessionStorage.getItem('schemeJS_panelConnected') === 'true') {
    globalThis.__SCHEME_JS_PANELCONNECTED = true;
  }
} catch {
  // Ignore restricted sessionStorage access
}
