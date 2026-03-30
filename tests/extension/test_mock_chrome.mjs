/**
 * @fileoverview Shared mock chrome API infrastructure for Puppeteer E2E tests.
 *
 * Provides composable string-generating functions that produce JavaScript
 * to inject via evaluateOnNewDocument. This eliminates duplication of the
 * mock chrome API across test_panel_interactions.mjs, test_console.mjs,
 * and test_js_debugging.mjs.
 *
 * Usage:
 *   const script = buildMockChromeScript({ cdp: true, extraState: '...' });
 *   await page.evaluateOnNewDocument(script);
 */

import { INLINE_URL } from './test_harness.mjs';

// =========================================================================
// Default mock data
// =========================================================================

/**
 * Default inline source content (shared across tests).
 */
export const DEFAULT_INLINE_CONTENT = '(define (add a b) (+ a b))\\n(define (double x) (* x 2))\\n(define (compute n) (add (double n) n))\\n(define (factorial n)\\n  (if (<= n 1) 1 (* n (factorial (- n 1)))))\\n(define greeting "hello")\\ngreeting\\n42\\n(display (string-append "result=" (number->string (compute 5)) "\\\\n"))\\n(display (string-append "fact5=" (number->string (factorial 5)) "\\\\n"))';

/**
 * Default external source content.
 */
export const DEFAULT_EXTERNAL_CONTENT = ';; External test script\\n(define (fib n)\\n  (if (<= n 1) n\\n    (+ (fib (- n 1)) (fib (- n 2)))))\\n(display (fib 10))';

// =========================================================================
// Mock state generator
// =========================================================================

/**
 * Generates the default __mockState object as a JS string.
 *
 * @param {Object} [options]
 * @param {boolean} [options.cdp] - Include CDP-specific state fields
 * @param {string} [options.extraState] - Additional state fields to append
 * @returns {string} JS string for __mockState initialization
 */
function generateMockState(options = {}) {
  const cdpFields = options.cdp ? `
  // Phase 4 CDP-specific state
  cdpAttached: false,
  cdpMessages: [],
  jsBreakpointsSet: [],
  jsBreakpointsRemoved: [],
  cdpStepIntoCalled: false,
  cdpStepOverCalled: false,
  cdpStepOutCalled: false,
  cdpResumeCalled: false,
  cdpDetachCalled: false,
  jsSource: 'function jsAdd(a, b) {\\n  return a + b;\\n}\\n\\nfunction jsDouble(x) {\\n  return x * 2;\\n}\\n',` : '';

  const extraFields = options.extraState || '';

  return `
window.__mockState = {
  sources: [
    {
      url: '${INLINE_URL}',
      content: '${DEFAULT_INLINE_CONTENT}',
      lines: 10,
      origin: 'inline'
    },
    {
      url: 'scheme://scheme-sources/manual_script.scm',
      content: '${DEFAULT_EXTERNAL_CONTENT}',
      lines: 5,
      origin: 'external'
    }
  ],
  status: { state: 'running', reason: null, active: true },
  paused: false,
  stack: [],
  locals: [],
  breakpoints: [],
  expressions: {
    '${INLINE_URL}': [
      { exprId: 1, line: 1, column: 1, endLine: 1, endColumn: 26 },
      { exprId: 2, line: 1, column: 16, endLine: 1, endColumn: 22 },
      { exprId: 3, line: 2, column: 1, endLine: 2, endColumn: 28 },
      { exprId: 4, line: 3, column: 1, endLine: 3, endColumn: 41 },
      { exprId: 5, line: 4, column: 1, endLine: 5, endColumn: 46 },
    ],
    'scheme://scheme-sources/manual_script.scm': [
      { exprId: 10, line: 2, column: 1, endLine: 4, endColumn: 42 },
      { exprId: 11, line: 5, column: 1, endLine: 5, endColumn: 17 },
    ],
  },
  activateResult: { active: true, needsReload: false },
  evalResults: {},
  resumeCalled: false,
  stepIntoCalled: false,
  stepOverCalled: false,
  stepOutCalled: false,
  ackPauseCalled: false,
  breakpointsSet: [],
  breakpointsRemoved: [],
  nextBreakpointId: 1,${cdpFields}${extraFields}
};`;
}

// =========================================================================
// Message listener infrastructure
// =========================================================================

const MESSAGE_LISTENER_SCRIPT = `
// Captured message listeners from panel code
window.__messageListeners = [];

// Helper to fire a message to all registered listeners
window.__fireMessage = function(message) {
  for (const listener of window.__messageListeners) {
    listener(message);
  }
};`;

// =========================================================================
// Eval dispatch logic (shared between DevTools and standalone window mocks)
// =========================================================================

/**
 * Core eval dispatch logic as a standalone function string.
 * Dispatches __schemeDebug API calls against __mockState.
 * Used by both inspectedWindow.eval (DevTools) and
 * chrome.scripting.executeScript (standalone window) mocks.
 */
const EVAL_DISPATCH_FUNCTION = `
window.__evalDispatch = function(expression) {
  const state = window.__mockState;
  let result = undefined;
  let error = null;

  try {
    if (expression.includes('typeof __schemeDebug')) {
      result = true;
    } else if (expression.includes('getSources()')) {
      result = JSON.stringify(state.sources);
    } else if (expression.includes('getSourceContent(')) {
      const urlMatch = expression.match(/getSourceContent\\("([^"]+)"\\)/);
      if (urlMatch) {
        const src = state.sources.find(s => s.url === urlMatch[1]);
        result = JSON.stringify(src ? src.content : null);
      }
    } else if (expression.includes('activate()')) {
      result = JSON.stringify(state.activateResult);
    } else if (expression.includes('getStatus()')) {
      result = JSON.stringify(state.status);
    } else if (expression.includes('getStack()')) {
      result = JSON.stringify(state.stack);
    } else if (expression.includes('getLocals(')) {
      const idxMatch = expression.match(/getLocals\\((\\d+)\\)/);
      const idx = idxMatch ? parseInt(idxMatch[1]) : 0;
      const locals = Array.isArray(state.locals[idx]) ? state.locals[idx] : (state.locals || []);
      result = JSON.stringify(locals);
    } else if (expression.includes('getAllBreakpoints()')) {
      result = JSON.stringify(state.breakpoints);
    } else if (expression.includes('ackPause()')) {
      state.ackPauseCalled = true;
      state.lastAction = 'ackPause';
      result = undefined;
    } else if (expression.includes('resume()')) {
      state.resumeCalled = true;
      state.lastAction = 'resume';
      result = undefined;
    } else if (expression.includes('stepInto()')) {
      state.stepIntoCalled = true;
      state.lastAction = 'stepInto';
      result = undefined;
    } else if (expression.includes('stepOver()')) {
      state.stepOverCalled = true;
      state.lastAction = 'stepOver';
      result = undefined;
    } else if (expression.includes('stepOut()')) {
      state.stepOutCalled = true;
      state.lastAction = 'stepOut';
      result = undefined;
    } else if (expression.includes('getExpressions(')) {
      const urlMatch = expression.match(/getExpressions\\("([^"]+)"\\)/);
      if (urlMatch) {
        const exprs = state.expressions ? (state.expressions[urlMatch[1]] || []) : [];
        result = JSON.stringify(exprs);
      }
    } else if (expression.includes('setBreakpoint(')) {
      const bpMatch = expression.match(/setBreakpoint\\("([^"]+)",\\s*(\\d+)(?:,\\s*(\\d+))?\\)/);
      if (bpMatch) {
        const id = 'bp-' + (state.nextBreakpointId++);
        const col = bpMatch[3] ? parseInt(bpMatch[3]) : null;
        state.breakpointsSet.push({ url: bpMatch[1], line: parseInt(bpMatch[2]), column: col, id });
        state.breakpoints.push({ id, filename: bpMatch[1], line: parseInt(bpMatch[2]), column: col });
        result = JSON.stringify(id);
      }
    } else if (expression.includes('removeBreakpoint(')) {
      const rmMatch = expression.match(/removeBreakpoint\\("([^"]+)"\\)/);
      if (rmMatch) {
        state.breakpointsRemoved.push(rmMatch[1]);
        state.breakpoints = state.breakpoints.filter(bp => bp.id !== rmMatch[1]);
        result = JSON.stringify(true);
      }
    } else if (expression.includes('localStorage.setItem')) {
      result = undefined;
    } else {
      result = undefined;
    }
  } catch (e) {
    error = { value: e.message };
  }

  return { result, error };
};`;

// =========================================================================
// Eval dispatcher for DevTools mock (uses __evalDispatch)
// =========================================================================

const EVAL_DISPATCHER_SCRIPT = `
      eval: function(expression, callback) {
        const { result, error } = window.__evalDispatch(expression);
        setTimeout(() => callback(result, error), 10);
      }`;

// =========================================================================
// CDP sendMessage mock
// =========================================================================

const CDP_SEND_MESSAGE_SCRIPT = `
    sendMessage: function(message, callback) {
      const state = window.__mockState;
      state.cdpMessages.push(message);

      let response = { success: true };

      if (message.type === 'attach-debugger') {
        state.cdpAttached = true;
        response = { success: true };
      } else if (message.type === 'detach-debugger') {
        state.cdpAttached = false;
        state.cdpDetachCalled = true;
        response = { success: true };
      } else if (message.type === 'resume-debugger') {
        state.cdpResumeCalled = true;
        response = { success: true };
      } else if (message.type === 'cdp-step-into') {
        state.cdpStepIntoCalled = true;
        response = { success: true };
      } else if (message.type === 'cdp-step-over') {
        state.cdpStepOverCalled = true;
        response = { success: true };
      } else if (message.type === 'cdp-step-out') {
        state.cdpStepOutCalled = true;
        response = { success: true };
      } else if (message.type === 'set-js-breakpoint') {
        state.jsBreakpointsSet.push({ url: message.url, lineNumber: message.lineNumber });
        response = { success: true, breakpointId: 'jsbp-' + (state.nextBreakpointId++) };
      } else if (message.type === 'remove-js-breakpoint') {
        state.jsBreakpointsRemoved.push(message.breakpointId);
        response = { success: true };
      } else if (message.type === 'get-js-source') {
        response = { success: true, source: state.jsSource };
      } else if (message.type === 'set-boundary-breakpoint') {
        state.boundaryBreakpointsSet = (state.boundaryBreakpointsSet || 0) + 1;
        response = { success: true, breakpointId: 'boundary-bp-' + (state.nextBreakpointId++) };
      } else if (message.type === 'eval-paused') {
        // Route eval-paused through the same __evalDispatch mechanism
        // so tests can provide mock data via __mockState.locals etc.
        state.evalPausedCalled = true;
        const expr = message.expression;
        try {
          const dispatched = window.__evalDispatch(expr);
          if (dispatched.error) {
            response = { success: false, error: dispatched.error.value || 'eval error' };
          } else {
            response = { success: true, result: dispatched.result };
          }
        } catch(e) {
          response = { success: false, error: e.message };
        }
      } else if (message.type === 'scheme-step-into') {
        state.schemeStepIntoCalled = true;
        response = { success: true };
      } else if (message.type === 'scheme-step-over') {
        state.schemeStepOverCalled = true;
        response = { success: true };
      } else if (message.type === 'scheme-step-out') {
        state.schemeStepOutCalled = true;
        response = { success: true };
      }

      if (callback) setTimeout(() => callback(response), 10);
    },
    lastError: null,`;

// =========================================================================
// Chrome object assembly (DevTools panel version)
// =========================================================================

/**
 * Generates the mock chrome object for a DevTools panel context.
 *
 * @param {Object} [options]
 * @param {boolean} [options.cdp] - Include CDP support (sendMessage, tabId)
 * @returns {string} JS string for window.chrome initialization
 */
function generateChromeObject(options = {}) {
  const tabIdLine = options.cdp ? '\n      tabId: 42,' : '';
  const sendMessageBlock = options.cdp ? CDP_SEND_MESSAGE_SCRIPT : '';

  return `
window.chrome = {
  devtools: {
    inspectedWindow: {${tabIdLine}
${EVAL_DISPATCHER_SCRIPT}
    },
    network: {
      onNavigated: { addListener: function() {} }
    },
    panels: {}
  },
  runtime: {
    onMessage: {
      addListener: function(listener) {
        window.__messageListeners.push(listener);
      }
    },${sendMessageBlock}
  },
  storage: {
    local: {
      _data: {},
      get: function(keys, callback) {
        const result = {};
        for (const k of (Array.isArray(keys) ? keys : [keys])) {
          if (k in this._data) result[k] = this._data[k];
        }
        if (callback) callback(result);
      },
      set: function(items, callback) {
        Object.assign(this._data, items);
        if (callback) callback();
      }
    }
  }
};`;
}

// =========================================================================
// Chrome object assembly (Standalone window version — no chrome.devtools)
// =========================================================================

/**
 * Generates the mock chrome object for a standalone window context.
 * Uses chrome.scripting.executeScript instead of inspectedWindow.eval.
 * Has NO chrome.devtools property.
 *
 * @param {Object} [options]
 * @param {boolean} [options.cdp] - Include CDP support (sendMessage)
 * @returns {string} JS string for window.chrome initialization
 */
function generateStandaloneWindowChromeObject(options = {}) {
  const sendMessageBlock = options.cdp ? CDP_SEND_MESSAGE_SCRIPT : '';

  return `
window.chrome = {
  scripting: {
    executeScript: function(options) {
      return new Promise(function(resolve) {
        // Extract the expression from args (callers pass (expr) => eval(expr))
        var args = options.args || [];
        var expression = args[0];
        if (typeof expression === 'string') {
          var dispatched = window.__evalDispatch(expression);
          if (dispatched.error) {
            resolve([{ error: { message: dispatched.error.value || 'eval error' } }]);
          } else {
            resolve([{ result: dispatched.result }]);
          }
        } else {
          resolve([{ result: undefined }]);
        }
      });
    }
  },
  tabs: {
    onUpdated: {
      addListener: function(listener) {
        window.__tabsOnUpdatedListeners = window.__tabsOnUpdatedListeners || [];
        window.__tabsOnUpdatedListeners.push(listener);
      }
    }
  },
  runtime: {
    onMessage: {
      addListener: function(listener) {
        window.__messageListeners.push(listener);
      }
    },${sendMessageBlock}
  },
  storage: {
    local: {
      _data: {},
      get: function(keys, callback) {
        var result = {};
        var keyArr = Array.isArray(keys) ? keys : [keys];
        for (var i = 0; i < keyArr.length; i++) {
          if (keyArr[i] in this._data) result[keyArr[i]] = this._data[keyArr[i]];
        }
        if (callback) callback(result);
      },
      set: function(items, callback) {
        Object.assign(this._data, items);
        if (callback) callback();
      }
    }
  }
};

// Helper to simulate chrome.tabs.onUpdated events
window.__fireTabsUpdated = function(tabId, changeInfo) {
  var listeners = window.__tabsOnUpdatedListeners || [];
  for (var i = 0; i < listeners.length; i++) {
    listeners[i](tabId, changeInfo);
  }
};`;
}

// =========================================================================
// Public API
// =========================================================================

/**
 * Builds a complete mock chrome script for a DevTools panel context.
 * Ready for evaluateOnNewDocument.
 *
 * @param {Object} [options]
 * @param {boolean} [options.cdp] - Include CDP support (sendMessage, tabId, CDP state)
 * @param {string} [options.extraState] - Additional JS for __mockState fields
 * @param {string} [options.extraScript] - Additional JS to append after chrome setup
 * @returns {string} Complete JS string to inject
 */
export function buildMockChromeScript(options = {}) {
  return [
    generateMockState(options),
    MESSAGE_LISTENER_SCRIPT,
    EVAL_DISPATCH_FUNCTION,
    generateChromeObject(options),
    options.extraScript || '',
  ].join('\n');
}

/**
 * Builds a complete mock chrome script for a standalone window context.
 * Uses chrome.scripting.executeScript instead of inspectedWindow.eval.
 * Has NO chrome.devtools property — tests that the panel works without DevTools.
 *
 * @param {Object} [options]
 * @param {boolean} [options.cdp] - Include CDP support (sendMessage)
 * @param {string} [options.extraState] - Additional JS for __mockState fields
 * @param {string} [options.extraScript] - Additional JS to append after chrome setup
 * @returns {string} Complete JS string to inject
 */
export function buildStandaloneWindowMockScript(options = {}) {
  return [
    generateMockState(options),
    MESSAGE_LISTENER_SCRIPT,
    EVAL_DISPATCH_FUNCTION,
    generateStandaloneWindowChromeObject(options),
    options.extraScript || '',
  ].join('\n');
}

/**
 * Pre-built base mock (no CDP) for backward compatibility.
 * Equivalent to the original MOCK_CHROME_SCRIPT in test_panel_interactions.mjs.
 */
export const MOCK_CHROME_SCRIPT = buildMockChromeScript();

/**
 * Pre-built CDP mock for backward compatibility.
 * Equivalent to the original CDP_MOCK_CHROME_SCRIPT in test_js_debugging.mjs.
 */
export const CDP_MOCK_CHROME_SCRIPT = buildMockChromeScript({ cdp: true });

/**
 * Pre-built standalone window mock (with CDP) for standalone window tests.
 */
export const STANDALONE_WINDOW_MOCK_SCRIPT = buildStandaloneWindowMockScript({ cdp: true });
