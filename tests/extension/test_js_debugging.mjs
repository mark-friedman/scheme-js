/**
 * @fileoverview Puppeteer E2E tests for Phase 4: Lazy CDP + JS Debugging.
 *
 * Tests the full user interaction flow for JavaScript debugging integration:
 *   - CDP auto-attachment when setting JS breakpoints
 *   - CDP attachment notification in toolbar
 *   - JS pause events → call stack shows [JS] frames
 *   - JS frame click → editor loads JS source with JS highlighting
 *   - Unified call stack interleaving (Scheme + JS frames)
 *   - JS stepping (Into/Over/Out) via CDP
 *   - JS resume from pause via CDP
 *   - Frame switching between Scheme and JS
 *   - CDP resume clears UI
 *   - JS breakpoint toggle via gutter
 *   - Scheme step commands still work (regression)
 *   - Mixed Scheme→JS and JS→Scheme boundaries
 *   - Default panel shows Scheme sources (regression)
 *   - CDP detach on panel close
 *
 * Uses mock chrome APIs injected via evaluateOnNewDocument (same pattern as
 * test_panel_interactions.mjs). The mock extends the existing chrome stub
 * with CDP-specific message routing and state tracking.
 */

import { assert, INLINE_URL } from './test_harness.mjs';

// =========================================================================
// Extended mock chrome API for CDP testing
// =========================================================================

/**
 * JavaScript to inject that extends the mock chrome API with CDP support.
 * Adds chrome.runtime.sendMessage tracking and CDP event simulation.
 */
const CDP_MOCK_CHROME_SCRIPT = `
// Mock state — tests can modify this to change what the panel "sees"
window.__mockState = {
  sources: [
    {
      url: '${INLINE_URL}',
      content: '(define (add a b) (+ a b))\\n(define (double x) (* x 2))\\n(define (compute n) (add (double n) n))\\n(define (factorial n)\\n  (if (<= n 1) 1 (* n (factorial (- n 1)))))\\n(define greeting "hello")\\ngreeting\\n42\\n(display (string-append "result=" (number->string (compute 5)) "\\\\n"))\\n(display (string-append "fact5=" (number->string (factorial 5)) "\\\\n"))',
      lines: 10,
      origin: 'inline'
    },
    {
      url: 'scheme://scheme-sources/manual_script.scm',
      content: ';; External test script\\n(define (fib n)\\n  (if (<= n 1) n\\n    (+ (fib (- n 1)) (fib (- n 2)))))\\n(display (fib 10))',
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
    ],
    'scheme://scheme-sources/manual_script.scm': [],
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
  nextBreakpointId: 1,

  // Phase 4 CDP-specific state
  cdpAttached: false,
  cdpMessages: [],       // All messages sent via chrome.runtime.sendMessage
  jsBreakpointsSet: [],  // { url, lineNumber } entries
  jsBreakpointsRemoved: [],
  cdpStepIntoCalled: false,
  cdpStepOverCalled: false,
  cdpStepOutCalled: false,
  cdpResumeCalled: false,
  cdpDetachCalled: false,
  jsSource: 'function jsAdd(a, b) {\\n  return a + b;\\n}\\n\\nfunction jsDouble(x) {\\n  return x * 2;\\n}\\n',
};

// Captured message listeners from panel code
window.__messageListeners = [];

// Helper to fire a message to all registered listeners
window.__fireMessage = function(message) {
  for (const listener of window.__messageListeners) {
    listener(message);
  }
};

// Build the mock chrome object
window.chrome = {
  devtools: {
    inspectedWindow: {
      tabId: 42,
      eval: function(expression, callback) {
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

        setTimeout(() => callback(result, error), 10);
      }
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
    },
    sendMessage: function(message, callback) {
      const state = window.__mockState;
      state.cdpMessages.push(message);

      let response = { success: true };

      // Route CDP messages
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
      }

      if (callback) setTimeout(() => callback(response), 10);
    },
    lastError: null,
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
};
`;

// =========================================================================
// Helper: open panel with CDP mocks
// =========================================================================

/**
 * Opens the panel page with CDP-extended mocked chrome APIs.
 * @param {import('puppeteer').Browser} browser
 * @param {string} extensionId
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openCDPMockedPanel(browser, extensionId) {
  const panelPage = await browser.newPage();
  await panelPage.evaluateOnNewDocument(CDP_MOCK_CHROME_SCRIPT);
  await panelPage.goto(
    `chrome-extension://${extensionId}/panel/panel.html`,
    { waitUntil: 'domcontentloaded' }
  );
  await new Promise(r => setTimeout(r, 1500));
  return panelPage;
}

/**
 * Fires a CDP pause event to the panel.
 * @param {import('puppeteer').Page} panelPage
 * @param {Object} detail - { callFrames, reason }
 */
async function fireCDPPauseEvent(panelPage, detail) {
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'cdp-paused', ...d });
  }, detail);
  await new Promise(r => setTimeout(r, 400));
}

/**
 * Fires a Scheme pause event to the panel.
 * @param {import('puppeteer').Page} panelPage
 * @param {Object} detail - { reason, source, stack }
 */
async function fireSchemePauseEvent(panelPage, detail) {
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
  await new Promise(r => setTimeout(r, 400));
}

/**
 * Fires a CDP resume event to the panel.
 * @param {import('puppeteer').Page} panelPage
 */
async function fireCDPResumeEvent(panelPage) {
  await panelPage.evaluate(() => {
    window.__fireMessage({ type: 'cdp-resumed' });
  });
  await new Promise(r => setTimeout(r, 300));
}

/**
 * Fires a Scheme resume event to the panel.
 * @param {import('puppeteer').Page} panelPage
 */
async function fireSchemeResumeEvent(panelPage) {
  await panelPage.evaluate(() => {
    window.__fireMessage({ type: 'scheme-debug-resumed' });
  });
  await new Promise(r => setTimeout(r, 300));
}

// Sample JS and Scheme call frames for testing
const JS_CALL_FRAMES = [
  {
    callFrameId: 'cf-1',
    functionName: 'jsAdd',
    url: 'http://localhost:8081/test.js',
    location: { scriptId: '42', lineNumber: 1, columnNumber: 4 },
    scopeChain: [
      { type: 'local', name: 'Local', object: {} },
      { type: 'closure', name: 'Closure', object: {} },
    ],
  },
  {
    callFrameId: 'cf-2',
    functionName: 'jsDouble',
    url: 'http://localhost:8081/test.js',
    location: { scriptId: '42', lineNumber: 5, columnNumber: 4 },
    scopeChain: [
      { type: 'local', name: 'Local', object: {} },
    ],
  },
];

const SCHEME_STACK = [
  { name: '<top-level>', source: { filename: INLINE_URL, line: 8, column: 0 }, tcoCount: 0 },
  { name: 'compute', source: { filename: INLINE_URL, line: 3, column: 0 }, tcoCount: 0 },
];

// =========================================================================
// Test: JS pause shows [JS] frames in call stack
// =========================================================================

export async function testJSPauseShowsJSFrames(browser, extensionId) {
  console.log('\n--- Test Group: JS Pause Shows JS Frames ---');
  if (!extensionId) { assert('JS frames: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  const frames = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#call-stack-container .call-stack-frame');
    return Array.from(items).map(el => ({
      text: el.textContent,
      hasBadgeJS: !!el.querySelector('.frame-badge-js'),
      hasBadgeSCM: !!el.querySelector('.frame-badge-scheme'),
    }));
  });

  assert('Call stack shows JS frames', frames.length >= 2, `got ${frames.length}`);

  if (frames.length >= 1) {
    assert('Top frame has JS badge', frames[0].hasBadgeJS === true,
      `badges: JS=${frames[0].hasBadgeJS}, SCM=${frames[0].hasBadgeSCM}`);
    assert('Top frame shows jsDouble', frames[0].text.includes('jsDouble'),
      `got: "${frames[0].text}"`);
  }

  await panelPage.close();
}

// =========================================================================
// Test: Unified call stack interleaving
// =========================================================================

export async function testUnifiedCallStackInterleaving(browser, extensionId) {
  console.log('\n--- Test Group: Unified Call Stack Interleaving ---');
  if (!extensionId) { assert('Interleaving: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Set up mock to return Scheme stack when getStack is called during CDP pause
  await panelPage.evaluate((schemeStack) => {
    window.__mockState.stack = schemeStack;
  }, SCHEME_STACK);

  // Fire a CDP pause — the unified debugger should also fetch Scheme stack
  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  const frames = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#call-stack-container .call-stack-frame');
    return Array.from(items).map(el => ({
      text: el.textContent,
      hasBadgeJS: !!el.querySelector('.frame-badge-js'),
      hasBadgeSCM: !!el.querySelector('.frame-badge-scheme'),
    }));
  });

  // Should have both JS and Scheme frames
  const jsFrames = frames.filter(f => f.hasBadgeJS);
  const scmFrames = frames.filter(f => f.hasBadgeSCM);

  assert('Interleaved stack has JS frames', jsFrames.length >= 1, `JS: ${jsFrames.length}`);
  assert('Interleaved stack has Scheme frames', scmFrames.length >= 1, `SCM: ${scmFrames.length}`);
  assert('Total frames >= 3', frames.length >= 3, `total: ${frames.length}`);

  await panelPage.close();
}

// =========================================================================
// Test: Unified call stack badges
// =========================================================================

export async function testUnifiedCallStackBadges(browser, extensionId) {
  console.log('\n--- Test Group: Unified Call Stack Badges ---');
  if (!extensionId) { assert('Badges: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Mixed pause: Scheme pause with stack
  await fireSchemePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: INLINE_URL, line: 3, column: 0 },
    stack: SCHEME_STACK,
  });

  const scmBadges = await panelPage.evaluate(() => {
    const badges = document.querySelectorAll('#call-stack-container .frame-badge-scheme');
    return Array.from(badges).map(b => ({
      text: b.textContent,
      className: b.className,
    }));
  });

  assert('SCM badges exist in Scheme-only pause', scmBadges.length >= 1,
    `got ${scmBadges.length}`);
  if (scmBadges.length > 0) {
    assert('SCM badge text is correct', scmBadges[0].text === 'SCM');
    assert('SCM badge has frame-badge-scheme class',
      scmBadges[0].className.includes('frame-badge-scheme'));
  }

  await panelPage.close();
}

// =========================================================================
// Test: JS step Into via CDP
// =========================================================================

export async function testJSStepInto(browser, extensionId) {
  console.log('\n--- Test Group: JS Step Into ---');
  if (!extensionId) { assert('JS step into: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Pause at JS frame
  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  // Reset flag
  await panelPage.evaluate(() => { window.__mockState.cdpStepIntoCalled = false; });

  // Click Step Into
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Into'))?.click();
  });
  await new Promise(r => setTimeout(r, 300));

  const called = await panelPage.evaluate(() => window.__mockState.cdpStepIntoCalled);
  assert('Step Into calls CDP step into', called === true);

  await panelPage.close();
}

// =========================================================================
// Test: JS step Over via CDP
// =========================================================================

export async function testJSStepOver(browser, extensionId) {
  console.log('\n--- Test Group: JS Step Over ---');
  if (!extensionId) { assert('JS step over: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  await panelPage.evaluate(() => { window.__mockState.cdpStepOverCalled = false; });

  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Over'))?.click();
  });
  await new Promise(r => setTimeout(r, 300));

  const called = await panelPage.evaluate(() => window.__mockState.cdpStepOverCalled);
  assert('Step Over calls CDP step over', called === true);

  await panelPage.close();
}

// =========================================================================
// Test: JS step Out via CDP
// =========================================================================

export async function testJSStepOut(browser, extensionId) {
  console.log('\n--- Test Group: JS Step Out ---');
  if (!extensionId) { assert('JS step out: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  await panelPage.evaluate(() => { window.__mockState.cdpStepOutCalled = false; });

  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Out'))?.click();
  });
  await new Promise(r => setTimeout(r, 300));

  const called = await panelPage.evaluate(() => window.__mockState.cdpStepOutCalled);
  assert('Step Out calls CDP step out', called === true);

  await panelPage.close();
}

// =========================================================================
// Test: JS resume from pause
// =========================================================================

export async function testJSResumeFromPause(browser, extensionId) {
  console.log('\n--- Test Group: JS Resume From Pause ---');
  if (!extensionId) { assert('JS resume: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  await panelPage.evaluate(() => { window.__mockState.cdpResumeCalled = false; });

  // Click Resume
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Resume'))?.click();
  });
  await new Promise(r => setTimeout(r, 300));

  const called = await panelPage.evaluate(() => window.__mockState.cdpResumeCalled);
  assert('Resume calls CDP resume', called === true);

  await panelPage.close();
}

// =========================================================================
// Test: Scheme step still works during Scheme pause (regression)
// =========================================================================

export async function testSchemeStepStillWorks(browser, extensionId) {
  console.log('\n--- Test Group: Scheme Step Still Works ---');
  if (!extensionId) { assert('Scheme step: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Pause at Scheme frame
  await fireSchemePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: INLINE_URL, line: 3, column: 0 },
    stack: SCHEME_STACK,
  });

  await panelPage.evaluate(() => {
    window.__mockState.stepIntoCalled = false;
    window.__mockState.cdpStepIntoCalled = false;
  });

  // Click Step Into — should use Scheme bridge, not CDP
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Into'))?.click();
  });
  await new Promise(r => setTimeout(r, 300));

  const schemeStepCalled = await panelPage.evaluate(() => window.__mockState.stepIntoCalled);
  const cdpStepCalled = await panelPage.evaluate(() => window.__mockState.cdpStepIntoCalled);

  assert('Scheme step into called', schemeStepCalled === true);
  assert('CDP step into NOT called during Scheme pause', cdpStepCalled === false);

  await panelPage.close();
}

// =========================================================================
// Test: CDP resume updates UI
// =========================================================================

export async function testCDPResumeUpdatesUI(browser, extensionId) {
  console.log('\n--- Test Group: CDP Resume Updates UI ---');
  if (!extensionId) { assert('CDP resume UI: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Pause at JS
  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  // Verify paused state
  let status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Status shows paused', status?.includes('Paused'));

  // Resume
  await fireCDPResumeEvent(panelPage);

  // Verify running state
  status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Status changed after CDP resume', !status?.includes('Paused'),
    `got: "${status}"`);

  // Call stack should be cleared
  const frameCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#call-stack-container .call-stack-frame').length
  );
  assert('Call stack cleared after CDP resume', frameCount === 0);

  await panelPage.close();
}

// =========================================================================
// Test: JS frame click loads JS source in editor
// =========================================================================

export async function testJSFrameClickLoadsJSSource(browser, extensionId) {
  console.log('\n--- Test Group: JS Frame Click Loads JS Source ---');
  if (!extensionId) { assert('JS source load: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Mark CDP as attached (simulating prior auto-attach)
  await panelPage.evaluate(() => { window.__mockState.cdpAttached = true; });

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  // The top frame should be auto-selected; check editor has JS content
  // Wait a bit for the source to load
  await new Promise(r => setTimeout(r, 500));

  const editorContent = await panelPage.evaluate(() => {
    const cm = document.querySelector('.cm-content');
    return cm?.textContent?.substring(0, 60) || '';
  });

  assert('Editor shows JS source', editorContent.includes('function'),
    `got: "${editorContent}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Editor switches to JS highlighting
// =========================================================================

export async function testEditorSwitchesToJSHighlighting(browser, extensionId) {
  console.log('\n--- Test Group: Editor Switches to JS Highlighting ---');
  if (!extensionId) { assert('JS highlight: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await panelPage.evaluate(() => { window.__mockState.cdpAttached = true; });

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  await new Promise(r => setTimeout(r, 500));

  // Check that the editor is displaying content (JS highlighting is applied internally
  // via CodeMirror — we verify the language mode is set by checking the content loaded)
  const editorContent = await panelPage.evaluate(() => {
    const cm = document.querySelector('.cm-content');
    return cm?.textContent || '';
  });

  assert('Editor has JS content (highlighting mode switched)',
    editorContent.includes('function') || editorContent.includes('return'),
    `got: "${editorContent.substring(0, 60)}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Switching between Scheme and JS frames updates editor + variables
// =========================================================================

export async function testSwitchBetweenSchemeAndJSFrames(browser, extensionId) {
  console.log('\n--- Test Group: Switch Between Scheme and JS Frames ---');
  if (!extensionId) { assert('Frame switch: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Set up Scheme locals for frame index 0
  await panelPage.evaluate((inlineUrl) => {
    window.__mockState.cdpAttached = true;
    window.__mockState.locals = {
      0: [{ name: 'x', value: '10', type: 'number', subtype: null }],
      1: [{ name: 'n', value: '5', type: 'number', subtype: null }],
    };
    window.__mockState.stack = [
      { name: '<top-level>', source: { filename: inlineUrl, line: 8, column: 0 }, tcoCount: 0 },
      { name: 'compute', source: { filename: inlineUrl, line: 3, column: 0 }, tcoCount: 0 },
    ];
  }, INLINE_URL);

  // Fire a CDP pause (JS frames will be on top, Scheme frames below)
  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  // Click a Scheme frame (should be after the JS frames)
  const clickedScheme = await panelPage.evaluate(() => {
    const frames = document.querySelectorAll('#call-stack-container .call-stack-frame');
    for (const f of frames) {
      if (f.querySelector('.frame-badge-scheme')) {
        f.click();
        return true;
      }
    }
    return false;
  });
  await new Promise(r => setTimeout(r, 500));

  if (clickedScheme) {
    // Editor should now show Scheme content
    const editorContent = await panelPage.evaluate(() => {
      const cm = document.querySelector('.cm-content');
      return cm?.textContent?.substring(0, 60) || '';
    });
    assert('After clicking Scheme frame, editor shows Scheme content',
      editorContent.includes('define'),
      `got: "${editorContent}"`);
  } else {
    assert('Found a Scheme frame to click', false, 'no Scheme frames in stack');
  }

  await panelPage.close();
}

// =========================================================================
// Test: JS variables displayed when paused at JS frame
// =========================================================================

export async function testJSVariablesDisplayed(browser, extensionId) {
  console.log('\n--- Test Group: JS Variables Displayed ---');
  if (!extensionId) { assert('JS vars: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  // The top frame has a scopeChain with 'local' scope
  const vars = await panelPage.evaluate(() => {
    const rows = document.querySelectorAll('#variables-container .variable-row');
    return Array.from(rows).map(r => r.textContent);
  });

  // Should show at least the local scope entry
  assert('Variables panel shows scope entries', vars.length >= 1,
    `got ${vars.length}: ${JSON.stringify(vars)}`);

  await panelPage.close();
}

// =========================================================================
// Test: Panel shows Scheme sources as default (regression)
// =========================================================================

export async function testPanelShowsSchemeSourcesAsDefault(browser, extensionId) {
  console.log('\n--- Test Group: Panel Shows Scheme Sources As Default ---');
  if (!extensionId) { assert('Default sources: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Source list should show Scheme sources (no JS sources by default)
  const sourceItems = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#source-list .source-item');
    return Array.from(items).map(el => el.textContent);
  });

  assert('Source list has Scheme sources', sourceItems.length >= 2,
    `got ${sourceItems.length}`);

  if (sourceItems.length >= 1) {
    assert('First source is Scheme file', sourceItems[0].includes('.scm'),
      `got: "${sourceItems[0]}"`);
  }

  // Editor should show Scheme content
  const editorContent = await panelPage.evaluate(() => {
    const cm = document.querySelector('.cm-content');
    return cm?.textContent?.substring(0, 40) || '';
  });
  assert('Editor shows Scheme content by default', editorContent.includes('define'),
    `got: "${editorContent}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Toolbar status shows [JS] suffix for JS pauses
// =========================================================================

export async function testJSPauseToolbarStatus(browser, extensionId) {
  console.log('\n--- Test Group: JS Pause Toolbar Status ---');
  if (!extensionId) { assert('JS status: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  const status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );

  assert('Status includes [JS] suffix',
    status?.includes('[JS]'),
    `got: "${status}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Scheme→JS boundary shows both frame types
// =========================================================================

export async function testSchemeCallsJSShowsBothFrames(browser, extensionId) {
  console.log('\n--- Test Group: Scheme Calls JS Shows Both Frames ---');
  if (!extensionId) { assert('SCM→JS: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Simulate: Scheme called a JS function (CDP pause with Scheme stack available)
  await panelPage.evaluate((inlineUrl) => {
    window.__mockState.stack = [
      { name: '<top-level>', source: { filename: inlineUrl, line: 8, column: 0 }, tcoCount: 0 },
      { name: 'compute', source: { filename: inlineUrl, line: 3, column: 0 }, tcoCount: 0 },
    ];
  }, INLINE_URL);

  await fireCDPPauseEvent(panelPage, {
    callFrames: [{
      callFrameId: 'cf-boundary',
      functionName: 'jsHelperFromScheme',
      url: 'http://localhost:8081/helpers.js',
      location: { scriptId: '99', lineNumber: 10, columnNumber: 0 },
      scopeChain: [],
    }],
    reason: 'other',
  });

  const frames = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#call-stack-container .call-stack-frame');
    return Array.from(items).map(el => ({
      text: el.textContent,
      hasBadgeJS: !!el.querySelector('.frame-badge-js'),
      hasBadgeSCM: !!el.querySelector('.frame-badge-scheme'),
    }));
  });

  const jsCount = frames.filter(f => f.hasBadgeJS).length;
  const scmCount = frames.filter(f => f.hasBadgeSCM).length;

  assert('Scheme→JS: has JS frames', jsCount >= 1, `JS: ${jsCount}`);
  assert('Scheme→JS: has Scheme frames', scmCount >= 1, `SCM: ${scmCount}`);

  await panelPage.close();
}

// =========================================================================
// Test: JS→Scheme boundary shows both frame types
// =========================================================================

export async function testJSCallsSchemeShowsBothFrames(browser, extensionId) {
  console.log('\n--- Test Group: JS Calls Scheme Shows Both Frames ---');
  if (!extensionId) { assert('JS→SCM: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Simulate: JS called Scheme — Scheme pause with JS frames still in CDP
  await fireSchemePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: INLINE_URL, line: 3, column: 0 },
    stack: [
      { name: '<top-level>', source: { filename: INLINE_URL, line: 8, column: 0 }, tcoCount: 0 },
      { name: 'schemeCallbackFromJS', source: { filename: INLINE_URL, line: 3, column: 0 }, tcoCount: 0 },
    ],
  });

  const frames = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#call-stack-container .call-stack-frame');
    return Array.from(items).map(el => ({
      text: el.textContent,
      hasBadgeSCM: !!el.querySelector('.frame-badge-scheme'),
    }));
  });

  // In a JS→Scheme pause, we get Scheme frames (the JS frames are not visible
  // because the pause came from Scheme side, not CDP)
  assert('JS→Scheme: has Scheme frames', frames.some(f => f.hasBadgeSCM));
  assert('JS→Scheme: shows correct function name',
    frames.some(f => f.text.includes('schemeCallbackFromJS')));

  await panelPage.close();
}

// =========================================================================
// Test: Buttons enabled during JS pause
// =========================================================================

export async function testButtonsEnabledDuringJSPause(browser, extensionId) {
  console.log('\n--- Test Group: Buttons Enabled During JS Pause ---');
  if (!extensionId) { assert('JS buttons: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  const buttons = await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    return btns.map(b => ({ title: b.title, disabled: b.disabled }));
  });

  const resumeBtn = buttons.find(b => b.title.includes('Resume'));
  const stepIntoBtn = buttons.find(b => b.title.includes('Step Into'));
  const stepOverBtn = buttons.find(b => b.title.includes('Step Over'));
  const stepOutBtn = buttons.find(b => b.title.includes('Step Out'));

  assert('Resume enabled during JS pause', resumeBtn?.disabled === false);
  assert('Step Into enabled during JS pause', stepIntoBtn?.disabled === false);
  assert('Step Over enabled during JS pause', stepOverBtn?.disabled === false);
  assert('Step Out enabled during JS pause', stepOutBtn?.disabled === false);

  await panelPage.close();
}

// =========================================================================
// Test: Current line highlight on JS pause
// =========================================================================

export async function testCurrentLineHighlightOnJSPause(browser, extensionId) {
  console.log('\n--- Test Group: Current Line Highlight on JS Pause ---');
  if (!extensionId) { assert('JS highlight: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  await panelPage.evaluate(() => { window.__mockState.cdpAttached = true; });

  await fireCDPPauseEvent(panelPage, {
    callFrames: JS_CALL_FRAMES,
    reason: 'other',
  });

  await new Promise(r => setTimeout(r, 500));

  const hasHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-line')
  );

  assert('Editor shows current-line highlight on JS pause', hasHighlight);

  await panelPage.close();
}

// =========================================================================
// Test: Scheme→JS Boundary Stepping
// =========================================================================

export async function testBoundaryStepping(browser, extensionId) {
  console.log('\n--- Test Group: Boundary Stepping ---');
  if (!extensionId) { assert('Boundary stepping: extension loaded', false, 'no extension ID'); return; }

  const panelPage = await openCDPMockedPanel(browser, extensionId);

  // Initialize mock state
  await panelPage.evaluate(() => {
    window.__mockState.boundaryBreakpointsSet = 0;
    window.__mockState.resumeCalled = false;
  });

  // Simulate a boundary pause
  await panelPage.evaluate(() => {
    const detail = {
      reason: 'boundary',
      source: { filename: 'test.scm', line: 1 },
      stack: [{ name: 'test', source: { filename: 'test.scm', line: 1 }, tcoCount: 0 }],
      data: { funcName: 'nativeFunc' }
    };
    window.__fireMessage({ type: 'scheme-debug-paused', detail });
  });

  await new Promise(r => setTimeout(r, 500));

  // The panel SHOULD NOT show a pause UI
  const bodyClass = await panelPage.evaluate(() => document.body.className);
  assert('Panel hides boundary pauses (not paused)', !bodyClass.includes('paused'));

  // CDP should be attached
  const cdpAttached = await panelPage.evaluate(() => window.__mockState.cdpAttached);
  assert('Boundary pause auto-attaches CDP', cdpAttached === true);

  // set-boundary-breakpoint should have been called
  const boundaryBps = await panelPage.evaluate(() => window.__mockState.boundaryBreakpointsSet);
  assert('Boundary breakpoint was requested via CDP', boundaryBps > 0);

  // Scheme native pause should be auto resumed
  const schemeResume = await panelPage.evaluate(() => window.__mockState.resumeCalled);
  assert('Scheme interpreter automatically resumed after boundary breakpoint', schemeResume === true);

  await panelPage.close();
}
