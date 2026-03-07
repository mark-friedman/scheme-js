/**
 * @fileoverview Content script for the Scheme-JS DevTools panel.
 *
 * Listens for postMessage events dispatched by the Scheme-JS interpreter in the
 * MAIN world and relays them to the extension panel via chrome.runtime.sendMessage.
 *
 * Uses window.postMessage (structured clone) instead of CustomEvent because
 * Chrome's CustomEvent.detail is null when read from a different world
 * (MAIN → ISOLATED). postMessage correctly copies data across the boundary.
 *
 * Events relayed:
 *   - 'scheme-debug-paused'  → { type: 'scheme-debug-paused',  detail: pauseInfo }
 *   - 'scheme-debug-resumed' → { type: 'scheme-debug-resumed' }
 *
 * The DevTools panel listens for these messages via chrome.runtime.onMessage.
 */

window.addEventListener('message', (e) => {
  // Only accept messages from the same window (page context)
  if (e.source !== window) return;

  if (e.data?.type === 'scheme-debug-paused') {
    chrome.runtime.sendMessage({
      type: 'scheme-debug-paused',
      detail: e.data.detail
    }).catch(() => {
      // Panel may not be open — ignore send failure
    });
  } else if (e.data?.type === 'scheme-debug-resumed') {
    chrome.runtime.sendMessage({ type: 'scheme-debug-resumed' }).catch(() => {
      // Panel may not be open — ignore send failure
    });
  }
});
