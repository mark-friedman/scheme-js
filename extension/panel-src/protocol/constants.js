/**
 * @fileoverview Shared constants for message types and pause contexts used
 * across the extension's protocol layer (background.js, cdp-bridge, scheme-bridge,
 * unified-debugger, and main.js).
 *
 * Centralizing these string literals prevents typo-induced bugs and makes it
 * easy to search for all message/context usage.
 */

// =========================================================================
// Message types exchanged between panel, background, and content script
// =========================================================================

/** @enum {string} Messages sent between panel and background service worker */
export const MSG = Object.freeze({
  // CDP lifecycle
  ATTACH_DEBUGGER:        'attach-debugger',
  DETACH_DEBUGGER:        'detach-debugger',
  RESUME_DEBUGGER:        'resume-debugger',

  // CDP evaluation
  EVAL_PAUSED:            'eval-paused',
  EVAL_IN_JS_FRAME:       'eval-in-js-frame',

  // Scheme step commands (via CDP evaluateOnCallFrame + resume)
  SCHEME_STEP_INTO:       'scheme-step-into',
  SCHEME_STEP_OVER:       'scheme-step-over',
  SCHEME_STEP_OUT:        'scheme-step-out',

  // Native V8 CDP step commands
  CDP_STEP_INTO:          'cdp-step-into',
  CDP_STEP_OVER:          'cdp-step-over',
  CDP_STEP_OUT:           'cdp-step-out',

  // Breakpoint management
  SET_JS_BREAKPOINT:      'set-js-breakpoint',
  REMOVE_JS_BREAKPOINT:   'remove-js-breakpoint',
  SET_BOUNDARY_BREAKPOINT:'set-boundary-breakpoint',

  // JS source fetching
  GET_JS_SOURCE:          'get-js-source',

  // CDP events forwarded from background → panel
  CDP_PAUSED:             'cdp-paused',
  CDP_RESUMED:            'cdp-resumed',
  CDP_SCRIPT_PARSED:      'cdp-script-parsed',
  CDP_ATTACHED:           'cdp-attached',
  DEBUGGER_RESUMED:       'debugger-resumed',

  // Scheme cooperative events (content script → panel)
  SCHEME_DEBUG_PAUSED:    'scheme-debug-paused',
  SCHEME_DEBUG_RESUMED:   'scheme-debug-resumed',
  SCHEME_SYNC_PAUSED:     'scheme-sync-paused',

  // Console output
  CONSOLE_API_CALLED:     'console-api-called',
});

// =========================================================================
// Pause contexts
// =========================================================================

/** @enum {string} Which debugging bridge is handling the current pause */
export const PAUSE_CONTEXT = Object.freeze({
  /** Cooperative async pause (runDebug path). Commands go through scheme-bridge. */
  SCHEME:      'scheme',
  /** CDP-level JS breakpoint or exception pause. Commands go through cdp-bridge. */
  JS:          'js',
  /** Sync DOM callback probe pause. V8 stays paused at `debugger;`. Commands go through CDP. */
  SCHEME_SYNC: 'scheme-sync',
});
