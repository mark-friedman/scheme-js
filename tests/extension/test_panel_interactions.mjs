/**
 * @fileoverview Comprehensive panel UI interaction tests.
 *
 * Tests real user interactions with the DevTools panel by injecting mock
 * chrome.devtools and chrome.runtime APIs before the panel bundle loads.
 * This lets us simulate the full pause/resume/step flow, source loading,
 * call stack selection, variable display, and breakpoint toggling — all
 * the interactions that previously could only be tested manually.
 *
 * Mock strategy:
 *   - chrome.devtools.inspectedWindow.eval is stubbed to return mock data
 *     based on a configurable mock state object on window.__mockState
 *   - chrome.runtime.onMessage listeners are captured so we can fire
 *     pause/resume events programmatically
 *   - chrome.devtools.network.onNavigated is stubbed (no-op)
 *   - chrome.devtools.panels is stubbed (no-op)
 *   - chrome.storage.local is stubbed (in-memory)
 */

import { assert } from './test_harness.mjs';

// =========================================================================
// Mock chrome API injection script
// =========================================================================

/**
 * JavaScript to inject via evaluateOnNewDocument that creates a mock
 * chrome API before the panel bundle loads. The mock routes all
 * evalInPage calls through a dispatcher keyed on the expression string.
 */
const MOCK_CHROME_SCRIPT = `
// Mock state — tests can modify this to change what the panel "sees"
window.__mockState = {
  sources: [
    {
      url: 'scheme://inline-scripts/script-0.scm',
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
    // Default expressions matching the default inline source content
    'scheme://inline-scripts/script-0.scm': [
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
  evalResults: {},  // code -> result mapping for debugEval
  resumeCalled: false,
  stepIntoCalled: false,
  stepOverCalled: false,
  stepOutCalled: false,
  ackPauseCalled: false,
  breakpointsSet: [],     // { url, line } entries from setBreakpoint calls
  breakpointsRemoved: [], // IDs from removeBreakpoint calls
  nextBreakpointId: 1,
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
      eval: function(expression, callback) {
        const state = window.__mockState;
        let result = undefined;
        let error = null;

        try {
          // Route based on expression content
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
            // Return locals for the requested frame index
            const locals = Array.isArray(state.locals[idx]) ? state.locals[idx] : (state.locals || []);
            result = JSON.stringify(locals);
          } else if (expression.includes('getAllBreakpoints()')) {
            result = JSON.stringify(state.breakpoints);
          } else if (expression.includes('ackPause()')) {
            state.ackPauseCalled = true;
            result = undefined;
          } else if (expression.includes('resume()')) {
            state.resumeCalled = true;
            result = undefined;
          } else if (expression.includes('stepInto()')) {
            state.stepIntoCalled = true;
            result = undefined;
          } else if (expression.includes('stepOver()')) {
            state.stepOverCalled = true;
            result = undefined;
          } else if (expression.includes('stepOut()')) {
            state.stepOutCalled = true;
            result = undefined;
          } else if (expression.includes('getExpressions(')) {
            const urlMatch = expression.match(/getExpressions\\("([^"]+)"\\)/);
            if (urlMatch) {
              const exprs = state.expressions ? (state.expressions[urlMatch[1]] || []) : [];
              result = JSON.stringify(exprs);
            }
          } else if (expression.includes('setBreakpoint(')) {
            // Match both setBreakpoint("url", line) and setBreakpoint("url", line, col)
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
            // breakpoint persistence — ignore in tests
            result = undefined;
          } else {
            // Unknown expression — return undefined
            result = undefined;
          }
        } catch (e) {
          error = { value: e.message };
        }

        // Simulate async callback
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
    }
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
// Helper: open panel with mocks
// =========================================================================

/**
 * Opens the panel page with mocked chrome APIs injected before load.
 * @param {import('puppeteer').Browser} browser
 * @param {string} extensionId
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openMockedPanel(browser, extensionId) {
  const panelPage = await browser.newPage();
  await panelPage.evaluateOnNewDocument(MOCK_CHROME_SCRIPT);
  await panelPage.goto(
    `chrome-extension://${extensionId}/panel/panel.html`,
    { waitUntil: 'domcontentloaded' }
  );
  // Wait for panel to initialize (activateAndRefresh + source loading)
  await new Promise(r => setTimeout(r, 1500));
  return panelPage;
}

/**
 * Sends a pause event to the panel via the captured message listeners.
 * @param {import('puppeteer').Page} panelPage
 * @param {Object} detail - Pause detail (reason, source, stack)
 */
async function firePauseEvent(panelPage, detail) {
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
  // Let the panel process the event
  await new Promise(r => setTimeout(r, 300));
}

/**
 * Sends a resume event to the panel.
 * @param {import('puppeteer').Page} panelPage
 */
async function fireResumeEvent(panelPage) {
  await panelPage.evaluate(() => {
    window.__fireMessage({ type: 'scheme-debug-resumed' });
  });
  await new Promise(r => setTimeout(r, 300));
}

// =========================================================================
// Test: Sources load and display in source list
// =========================================================================

export async function testPanelSourcesLoad(browser, extensionId) {
  console.log('\n--- Test Group: Panel Sources Load ---');

  if (!extensionId) {
    assert('Panel sources: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Source list should show both files
  const sourceItems = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#source-list .source-item');
    return Array.from(items).map(el => ({
      text: el.textContent,
      url: el.dataset.url,
      selected: el.classList.contains('selected'),
    }));
  });

  assert('Source list has 2 items', sourceItems.length === 2,
    `got ${sourceItems.length}: ${JSON.stringify(sourceItems)}`);

  if (sourceItems.length >= 1) {
    assert('First source is inline script',
      sourceItems[0].text.includes('script-0.scm'),
      `got: "${sourceItems[0].text}"`);
    assert('First source is auto-selected', sourceItems[0].selected === true);
  }
  if (sourceItems.length >= 2) {
    assert('Second source is external script',
      sourceItems[1].text.includes('manual_script.scm'),
      `got: "${sourceItems[1].text}"`);
  }

  // Editor should have content from auto-selected first source
  const editorContent = await panelPage.evaluate(() => {
    const cm = document.querySelector('.cm-content');
    return cm?.textContent?.substring(0, 40) || '';
  });
  assert('Editor shows content from first source',
    editorContent.includes('define'),
    `got: "${editorContent}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Click source file switches editor content
// =========================================================================

export async function testPanelSourceSwitch(browser, extensionId) {
  console.log('\n--- Test Group: Panel Source Switch ---');

  if (!extensionId) {
    assert('Panel source switch: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Click the second source item
  await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#source-list .source-item');
    if (items[1]) items[1].click();
  });
  await new Promise(r => setTimeout(r, 300));

  // Editor should now show external script content
  const editorContent = await panelPage.evaluate(() => {
    const cm = document.querySelector('.cm-content');
    return cm?.textContent || '';
  });
  assert('Editor shows external script after click',
    editorContent.includes('fib'),
    `got: "${editorContent.substring(0, 60)}"`);

  // Second item should now be selected
  const selected = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#source-list .source-item');
    return Array.from(items).map(el => el.classList.contains('selected'));
  });
  assert('Second source is now selected',
    selected[1] === true, `selection: ${JSON.stringify(selected)}`);

  // Status should show viewing filename
  const status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Status shows viewing filename',
    status?.includes('manual_script.scm'),
    `got: "${status}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Pause event updates toolbar, call stack, variables
// =========================================================================

export async function testPanelPauseUpdatesUI(browser, extensionId) {
  console.log('\n--- Test Group: Panel Pause Updates UI ---');

  if (!extensionId) {
    assert('Panel pause UI: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Set up mock locals for the top frame
  await panelPage.evaluate(() => {
    window.__mockState.locals = {
      1: [
        { name: 'n', value: '5', type: 'number', subtype: null },
        { name: 'acc', value: '1', type: 'number', subtype: null },
      ]
    };
  });

  // Fire a pause event
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: {
      filename: 'scheme://inline-scripts/script-0.scm',
      line: 4,
      column: 0,
    },
    stack: [
      {
        name: '<top-level>',
        source: { filename: 'scheme://inline-scripts/script-0.scm', line: 10, column: 0 },
        tcoCount: 0,
      },
      {
        name: 'factorial',
        source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 },
        tcoCount: 0,
      },
    ],
  });

  // Check toolbar status
  const status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Status shows "Paused at breakpoint"',
    status === 'Paused at breakpoint', `got: "${status}"`);

  // Check buttons are enabled
  const buttons = await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    return btns.map(b => ({ title: b.title, disabled: b.disabled }));
  });
  const resumeBtn = buttons.find(b => b.title.includes('Resume'));
  assert('Resume button enabled during pause', resumeBtn?.disabled === false);
  const stepIntoBtn = buttons.find(b => b.title.includes('Step Into'));
  assert('Step Into button enabled during pause', stepIntoBtn?.disabled === false);

  // Check ackPause was called
  const ackCalled = await panelPage.evaluate(() => window.__mockState.ackPauseCalled);
  assert('ackPause was called on pause', ackCalled === true);

  // Check call stack frames
  const frames = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#call-stack-container .call-stack-frame');
    return Array.from(items).map(el => ({
      text: el.textContent,
      selected: el.classList.contains('selected'),
    }));
  });
  assert('Call stack shows 2 frames', frames.length === 2,
    `got ${frames.length}`);

  if (frames.length >= 1) {
    assert('Top frame shows factorial',
      frames[0].text.includes('factorial'),
      `got: "${frames[0].text}"`);
  }
  if (frames.length >= 2) {
    assert('Bottom frame shows top-level',
      frames[1].text.includes('top-level'),
      `got: "${frames[1].text}"`);
  }

  // Check current line highlight exists in editor
  const hasHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-line')
  );
  assert('Editor shows current-line highlight', hasHighlight);

  await panelPage.close();
}

// =========================================================================
// Test: Resume event clears UI
// =========================================================================

export async function testPanelResumeUpdatesUI(browser, extensionId) {
  console.log('\n--- Test Group: Panel Resume Updates UI ---');

  if (!extensionId) {
    assert('Panel resume UI: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // First, pause
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 },
    stack: [{ name: 'factorial', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 }, tcoCount: 0 }],
  });

  // Verify we're in paused state
  let status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Panel is paused before resume', status?.includes('Paused'));

  // Now resume
  await fireResumeEvent(panelPage);

  // Check toolbar reverts
  status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  // After resume, refresh() sets status to "Refreshing..." then "Ready"
  assert('Status changes after resume',
    !status?.includes('Paused'),
    `got: "${status}"`);

  // Buttons should be disabled again
  const buttons = await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    return btns.map(b => ({ title: b.title, disabled: b.disabled }));
  });
  const resumeBtn = buttons.find(b => b.title.includes('Resume'));
  assert('Resume button disabled after resume', resumeBtn?.disabled === true);

  // Call stack should be empty
  const hasFrames = await panelPage.evaluate(() => {
    return document.querySelectorAll('#call-stack-container .call-stack-frame').length;
  });
  assert('Call stack cleared after resume', hasFrames === 0);

  // Current line highlight should be gone
  const hasHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-line')
  );
  assert('Current-line highlight cleared after resume', !hasHighlight);

  await panelPage.close();
}

// =========================================================================
// Test: Clicking Resume button calls resume()
// =========================================================================

export async function testPanelResumeButtonClick(browser, extensionId) {
  console.log('\n--- Test Group: Panel Resume Button Click ---');

  if (!extensionId) {
    assert('Panel resume click: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Pause first to enable buttons
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 },
    stack: [],
  });

  // Reset the flag
  await panelPage.evaluate(() => { window.__mockState.resumeCalled = false; });

  // Click resume button
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    const resumeBtn = btns.find(b => b.title.includes('Resume'));
    if (resumeBtn) resumeBtn.click();
  });
  await new Promise(r => setTimeout(r, 200));

  const resumeCalled = await panelPage.evaluate(() => window.__mockState.resumeCalled);
  assert('Clicking Resume calls resume()', resumeCalled === true);

  await panelPage.close();
}

// =========================================================================
// Test: Clicking Step buttons calls correct API
// =========================================================================

export async function testPanelStepButtonClicks(browser, extensionId) {
  console.log('\n--- Test Group: Panel Step Button Clicks ---');

  if (!extensionId) {
    assert('Panel step clicks: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Pause to enable buttons
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 },
    stack: [],
  });

  // Reset all flags
  await panelPage.evaluate(() => {
    const s = window.__mockState;
    s.stepIntoCalled = false;
    s.stepOverCalled = false;
    s.stepOutCalled = false;
  });

  // Click Step Into
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Into'))?.click();
  });
  await new Promise(r => setTimeout(r, 200));

  let stepIntoCalled = await panelPage.evaluate(() => window.__mockState.stepIntoCalled);
  assert('Clicking Step Into calls stepInto()', stepIntoCalled === true);

  // Re-pause (step might have been consumed)
  await firePauseEvent(panelPage, {
    reason: 'step',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 5, column: 0 },
    stack: [],
  });

  // Click Step Over
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Over'))?.click();
  });
  await new Promise(r => setTimeout(r, 200));

  let stepOverCalled = await panelPage.evaluate(() => window.__mockState.stepOverCalled);
  assert('Clicking Step Over calls stepOver()', stepOverCalled === true);

  // Re-pause
  await firePauseEvent(panelPage, {
    reason: 'step',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 6, column: 0 },
    stack: [{ name: 'factorial', source: null, tcoCount: 0 }],
  });

  // Click Step Out
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Out'))?.click();
  });
  await new Promise(r => setTimeout(r, 200));

  let stepOutCalled = await panelPage.evaluate(() => window.__mockState.stepOutCalled);
  assert('Clicking Step Out calls stepOut()', stepOutCalled === true);

  await panelPage.close();
}

// =========================================================================
// Test: Call stack frame selection updates variables
// =========================================================================

export async function testPanelFrameSelectionUpdatesVariables(browser, extensionId) {
  console.log('\n--- Test Group: Panel Frame Selection -> Variables ---');

  if (!extensionId) {
    assert('Panel frame selection: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Set up per-frame locals
  await panelPage.evaluate(() => {
    window.__mockState.locals = {
      0: [{ name: 'x', value: '10', type: 'number', subtype: null }],
      1: [
        { name: 'n', value: '5', type: 'number', subtype: null },
        { name: 'acc', value: '1', type: 'number', subtype: null },
      ],
    };
  });

  // Pause with a 2-frame stack
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 },
    stack: [
      { name: '<top-level>', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 10, column: 0 }, tcoCount: 0 },
      { name: 'factorial', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 }, tcoCount: 0 },
    ],
  });

  // The panel auto-loads locals for the top frame (index 1)
  // Check variables show n and acc
  let vars = await panelPage.evaluate(() => {
    const rows = document.querySelectorAll('#variables-container .variable-row');
    return Array.from(rows).map(r => r.textContent);
  });
  assert('Top frame shows n and acc variables', vars.length >= 2,
    `got ${vars.length}: ${JSON.stringify(vars)}`);

  // Now click the bottom frame (top-level, shown second in DOM since stack is reversed)
  await panelPage.evaluate(() => {
    const frames = document.querySelectorAll('#call-stack-container .call-stack-frame');
    // frames[0] = factorial (top), frames[1] = top-level (bottom)
    if (frames[1]) frames[1].click();
  });
  await new Promise(r => setTimeout(r, 500));

  // Variables should now show x (frame 0's locals)
  vars = await panelPage.evaluate(() => {
    const rows = document.querySelectorAll('#variables-container .variable-row');
    return Array.from(rows).map(r => r.textContent);
  });
  assert('Bottom frame shows x variable',
    vars.length >= 1 && vars.some(v => v.includes('x')),
    `got: ${JSON.stringify(vars)}`);

  await panelPage.close();
}

// =========================================================================
// Test: Variables display types with correct CSS classes
// =========================================================================

export async function testPanelVariableTypeColors(browser, extensionId) {
  console.log('\n--- Test Group: Panel Variable Type Colors ---');

  if (!extensionId) {
    assert('Panel variable colors: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Set up locals with various types
  await panelPage.evaluate(() => {
    window.__mockState.locals = {
      0: [
        { name: 'count', value: '42', type: 'number', subtype: null },
        { name: 'name', value: '"hello"', type: 'string', subtype: null },
        { name: 'flag', value: '#t', type: 'boolean', subtype: null },
        { name: 'lst', value: '(1 2 3)', type: 'pair', subtype: null },
        { name: 'nothing', value: '()', type: 'null', subtype: null },
      ],
    };
  });

  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 1, column: 0 },
    stack: [{ name: 'test', source: null, tcoCount: 0 }],
  });

  const varInfo = await panelPage.evaluate(() => {
    const rows = document.querySelectorAll('#variables-container .variable-row');
    return Array.from(rows).map(row => {
      const name = row.querySelector('.var-name')?.textContent;
      const value = row.querySelector('.var-value')?.textContent;
      const classes = row.querySelector('.var-value')?.className;
      return { name, value, classes };
    });
  });

  assert('5 variables displayed', varInfo.length === 5, `got ${varInfo.length}`);

  const countVar = varInfo.find(v => v.name === 'count');
  assert('Number variable has correct class',
    countVar?.classes?.includes('var-type-number'),
    `classes: ${countVar?.classes}`);

  const nameVar = varInfo.find(v => v.name === 'name');
  assert('String variable has correct class',
    nameVar?.classes?.includes('var-type-string'),
    `classes: ${nameVar?.classes}`);

  const flagVar = varInfo.find(v => v.name === 'flag');
  assert('Boolean variable has correct class',
    flagVar?.classes?.includes('var-type-boolean'),
    `classes: ${flagVar?.classes}`);

  const lstVar = varInfo.find(v => v.name === 'lst');
  assert('Pair variable has correct class',
    lstVar?.classes?.includes('var-type-other'),
    `classes: ${lstVar?.classes}`);

  await panelPage.close();
}

// =========================================================================
// Test: Call stack TCO badge
// =========================================================================

export async function testPanelCallStackTCO(browser, extensionId) {
  console.log('\n--- Test Group: Panel Call Stack TCO Badge ---');

  if (!extensionId) {
    assert('Panel TCO badge: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 },
    stack: [
      { name: '<top-level>', source: null, tcoCount: 0 },
      { name: 'factorial', source: null, tcoCount: 3 },
    ],
  });

  const tcoBadge = await panelPage.evaluate(() => {
    const tcoEl = document.querySelector('#call-stack-container .frame-tco');
    return tcoEl ? { text: tcoEl.textContent, title: tcoEl.title } : null;
  });

  assert('TCO badge is displayed', tcoBadge !== null);
  assert('TCO badge shows correct count', tcoBadge?.text === '×4',
    `got: "${tcoBadge?.text}"`);
  assert('TCO badge has descriptive title',
    tcoBadge?.title?.includes('4 tail calls'),
    `got: "${tcoBadge?.title}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Breakpoint toggle in editor gutter
// =========================================================================

export async function testPanelBreakpointGutterToggle(browser, extensionId) {
  console.log('\n--- Test Group: Panel Breakpoint Gutter Toggle ---');

  if (!extensionId) {
    assert('Panel breakpoint toggle: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Reset breakpoint tracking
  await panelPage.evaluate(() => {
    window.__mockState.breakpointsSet = [];
    window.__mockState.breakpointsRemoved = [];
  });

  // Click the breakpoint gutter at a specific line.
  // CodeMirror gutter handlers use mousedown events via domEventHandlers.
  // We use the Puppeteer mouse to click at the right coordinates.
  // Wait for CodeMirror to render
  await new Promise(r => setTimeout(r, 500));

  // The breakpoint gutter has no child elements when empty (CM6 optimizes).
  // Use the line number gutter to determine the Y coordinate, then click
  // in the breakpoint gutter column (which is to the left of line numbers).
  const clickTarget = await panelPage.evaluate(() => {
    const bpGutter = document.querySelector('.cm-breakpoint-gutter');
    if (!bpGutter) return null;
    const bpRect = bpGutter.getBoundingClientRect();

    // Find the 3rd line number element to get the Y coordinate
    const lineNums = document.querySelectorAll('.cm-lineNumbers .cm-gutterElement');
    const visibleNums = Array.from(lineNums).filter(el => el.offsetHeight > 0);
    if (visibleNums.length < 3) return null;

    const lineRect = visibleNums[2].getBoundingClientRect();
    return {
      x: bpRect.x + bpRect.width / 2,
      y: lineRect.y + lineRect.height / 2,
    };
  });

  if (clickTarget) {
    await panelPage.mouse.click(clickTarget.x, clickTarget.y);
    await new Promise(r => setTimeout(r, 500));

    // Check if a breakpoint dot appeared
    const hasDot = await panelPage.evaluate(() =>
      !!document.querySelector('.cm-breakpoint-dot')
    );
    assert('Breakpoint dot appears after gutter click', hasDot);

    // Check if setBreakpoint was called on the mock
    const bpSet = await panelPage.evaluate(() => window.__mockState.breakpointsSet);
    assert('setBreakpoint was called', bpSet.length >= 1,
      `called ${bpSet.length} times`);
  } else {
    assert('Could find gutter to click', false,
      'breakpoint gutter or line numbers not found');
  }

  await panelPage.close();
}

// =========================================================================
// Test: Step reason shows in status
// =========================================================================

export async function testPanelStepReasonStatus(browser, extensionId) {
  console.log('\n--- Test Group: Panel Step Reason Status ---');

  if (!extensionId) {
    assert('Panel step reason: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Fire pause with reason 'step'
  await firePauseEvent(panelPage, {
    reason: 'step',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 5, column: 0 },
    stack: [],
  });

  const status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Status shows "Paused (step)" for step reason',
    status === 'Paused (step)', `got: "${status}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Pause navigates editor to correct source and line
// =========================================================================

export async function testPanelPauseNavigatesEditor(browser, extensionId) {
  console.log('\n--- Test Group: Panel Pause Navigates Editor ---');

  if (!extensionId) {
    assert('Panel pause navigation: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // The panel auto-loads the first source. Now pause in the SECOND source.
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: {
      filename: 'scheme://scheme-sources/manual_script.scm',
      line: 3,
      column: 0,
    },
    stack: [{ name: 'fib', source: { filename: 'scheme://scheme-sources/manual_script.scm', line: 3, column: 0 }, tcoCount: 0 }],
  });

  // Editor should now show the external script
  const editorContent = await panelPage.evaluate(() => {
    const cm = document.querySelector('.cm-content');
    return cm?.textContent || '';
  });
  assert('Editor switched to external script on pause',
    editorContent.includes('fib'),
    `got: "${editorContent.substring(0, 60)}"`);

  // Current line highlight should be on line 3
  const hasHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-line')
  );
  assert('Current line highlighted in editor', hasHighlight);

  await panelPage.close();
}

// =========================================================================
// Test: SCM badge in call stack frames
// =========================================================================

export async function testPanelCallStackBadges(browser, extensionId) {
  console.log('\n--- Test Group: Panel Call Stack Badges ---');

  if (!extensionId) {
    assert('Panel badges: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 0 },
    stack: [
      { name: '<top-level>', source: null, tcoCount: 0 },
      { name: 'factorial', source: null, tcoCount: 0 },
    ],
  });

  const badges = await panelPage.evaluate(() => {
    const els = document.querySelectorAll('#call-stack-container .frame-badge');
    return Array.from(els).map(el => ({
      text: el.textContent,
      className: el.className,
    }));
  });

  assert('Each frame has a badge', badges.length === 2, `got ${badges.length}`);
  assert('Badge shows SCM', badges[0]?.text === 'SCM');
  assert('Badge has scheme class',
    badges[0]?.className?.includes('frame-badge-scheme'));

  await panelPage.close();
}

// =========================================================================
// Test: Empty variables state shows message
// =========================================================================

export async function testPanelEmptyVariablesMessage(browser, extensionId) {
  console.log('\n--- Test Group: Panel Empty Variables Message ---');

  if (!extensionId) {
    assert('Panel empty vars: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Pause with empty locals
  await panelPage.evaluate(() => {
    window.__mockState.locals = { 0: [] };
  });

  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 1, column: 0 },
    stack: [{ name: 'test', source: null, tcoCount: 0 }],
  });

  const emptyMsg = await panelPage.evaluate(() => {
    const el = document.querySelector('#variables-container .variables-empty');
    return el?.textContent;
  });
  assert('Empty variables shows "No local bindings"',
    emptyMsg === 'No local bindings', `got: "${emptyMsg}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Expression highlight appears on pause with endLine/endColumn
// =========================================================================

export async function testPanelExpressionHighlightOnPause(browser, extensionId) {
  console.log('\n--- Test Group: Panel Expression Highlight on Pause ---');

  if (!extensionId) {
    assert('Panel expr highlight: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Fire a pause event with expression range (endLine/endColumn)
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: {
      filename: 'scheme://inline-scripts/script-0.scm',
      line: 1,
      column: 1,
      endLine: 1,
      endColumn: 26,
    },
    stack: [{ name: 'add', source: null, tcoCount: 0 }],
  });

  // Check that expression highlight exists
  const hasExprHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-expr')
  );
  assert('Expression highlight appears on pause', hasExprHighlight);

  // Also check line highlight exists
  const hasLineHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-line')
  );
  assert('Line highlight also appears on pause', hasLineHighlight);

  await panelPage.close();
}

// =========================================================================
// Test: Expression highlight clears on resume
// =========================================================================

export async function testPanelExpressionHighlightClears(browser, extensionId) {
  console.log('\n--- Test Group: Panel Expression Highlight Clears ---');

  if (!extensionId) {
    assert('Panel expr clear: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Pause with expression range
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: {
      filename: 'scheme://inline-scripts/script-0.scm',
      line: 1, column: 1, endLine: 1, endColumn: 26,
    },
    stack: [],
  });

  // Verify highlight exists
  let hasExpr = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-expr')
  );
  assert('Expression highlight present before resume', hasExpr);

  // Resume
  await fireResumeEvent(panelPage);

  // Verify highlight is gone
  hasExpr = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-expr')
  );
  assert('Expression highlight gone after resume', !hasExpr);

  await panelPage.close();
}

// =========================================================================
// Test: Diamond markers appear on breakpoint line with multiple expressions
// =========================================================================

export async function testPanelDiamondMarkersOnBreakpointLine(browser, extensionId) {
  console.log('\n--- Test Group: Panel Diamond Markers on Breakpoint Line ---');

  if (!extensionId) {
    assert('Panel diamonds: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Wait for initial load
  await new Promise(r => setTimeout(r, 500));

  // Click gutter to set a line breakpoint on line 1 (which has multiple expressions
  // in mock data: exprId 1 at col 1 and exprId 2 at col 16)
  const gutterTarget = await panelPage.evaluate(() => {
    const bpGutter = document.querySelector('.cm-breakpoint-gutter');
    if (!bpGutter) return null;
    const bpRect = bpGutter.getBoundingClientRect();
    const lineNums = document.querySelectorAll('.cm-lineNumbers .cm-gutterElement');
    const visibleNums = Array.from(lineNums).filter(el => el.offsetHeight > 0);
    if (visibleNums.length < 1) return null;
    // Click on the first visible line number gutter element (line 1)
    const lineRect = visibleNums[0].getBoundingClientRect();
    return { x: bpRect.x + bpRect.width / 2, y: lineRect.y + lineRect.height / 2 };
  });

  if (gutterTarget) {
    await panelPage.mouse.click(gutterTarget.x, gutterTarget.y);
    await new Promise(r => setTimeout(r, 500));

    // Check that a breakpoint dot appeared
    const hasDot = await panelPage.evaluate(() =>
      !!document.querySelector('.cm-breakpoint-dot')
    );
    assert('Gutter dot appears after click', hasDot);

    // Check that diamond markers appeared (hollow diamonds for sub-expressions)
    const diamonds = await panelPage.evaluate(() => {
      const els = document.querySelectorAll('.cm-expr-diamond');
      return Array.from(els).map(el => ({
        text: el.textContent,
        active: el.classList.contains('cm-expr-diamond-active'),
        line: el.dataset.line,
        column: el.dataset.column,
      }));
    });

    assert('Diamond markers appear on breakpoint line', diamonds.length >= 1,
      `found ${diamonds.length} diamonds: ${JSON.stringify(diamonds)}`);

    if (diamonds.length >= 1) {
      assert('Diamond is hollow (not active)', diamonds[0].active === false);
      assert('Diamond is ◇', diamonds[0].text === '◇');
    }
  } else {
    assert('Could find gutter to click', false, 'gutter not found');
  }

  await panelPage.close();
}

// =========================================================================
// Test: Clicking diamond sets expression breakpoint
// =========================================================================

export async function testPanelDiamondClickSetsBreakpoint(browser, extensionId) {
  console.log('\n--- Test Group: Panel Diamond Click Sets Breakpoint ---');

  if (!extensionId) {
    assert('Panel diamond click: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);
  await new Promise(r => setTimeout(r, 500));

  // Set a line breakpoint on line 1 to trigger diamond display
  const gutterTarget = await panelPage.evaluate(() => {
    const bpGutter = document.querySelector('.cm-breakpoint-gutter');
    if (!bpGutter) return null;
    const bpRect = bpGutter.getBoundingClientRect();
    const lineNums = document.querySelectorAll('.cm-lineNumbers .cm-gutterElement');
    const visibleNums = Array.from(lineNums).filter(el => el.offsetHeight > 0);
    if (visibleNums.length < 1) return null;
    const lineRect = visibleNums[0].getBoundingClientRect();
    return { x: bpRect.x + bpRect.width / 2, y: lineRect.y + lineRect.height / 2 };
  });

  if (!gutterTarget) {
    assert('Could find gutter to click', false, 'gutter not found');
    await panelPage.close();
    return;
  }

  await panelPage.mouse.click(gutterTarget.x, gutterTarget.y);
  await new Promise(r => setTimeout(r, 500));

  // Reset breakpoint tracking (the line BP was already recorded)
  await panelPage.evaluate(() => {
    window.__mockState.breakpointsSet = [];
  });

  // Now click the first diamond marker
  const clicked = await panelPage.evaluate(() => {
    const diamond = document.querySelector('.cm-expr-diamond');
    if (!diamond) return false;
    diamond.dispatchEvent(new MouseEvent('mousedown', { bubbles: true, cancelable: true }));
    return true;
  });
  await new Promise(r => setTimeout(r, 500));

  assert('Found and clicked a diamond', clicked);

  if (clicked) {
    // Check that setBreakpoint was called with a column
    const bpSet = await panelPage.evaluate(() => window.__mockState.breakpointsSet);
    assert('setBreakpoint called on diamond click', bpSet.length >= 1,
      `called ${bpSet.length} times: ${JSON.stringify(bpSet)}`);

    if (bpSet.length >= 1) {
      const bp = bpSet[bpSet.length - 1];
      assert('Expression breakpoint has column',
        bp.column !== null && bp.column !== undefined,
        `column: ${bp.column}`);
    }

    // Diamond should now be filled (active)
    const activeDiamonds = await panelPage.evaluate(() => {
      const els = document.querySelectorAll('.cm-expr-diamond-active');
      return els.length;
    });
    assert('Diamond is now active (filled)', activeDiamonds >= 1,
      `found ${activeDiamonds} active diamonds`);

    // Expression breakpoint highlight should also appear
    const hasExprBp = await panelPage.evaluate(() =>
      !!document.querySelector('.cm-expr-breakpoint')
    );
    assert('Expression breakpoint highlight appears', hasExprBp);
  }

  await panelPage.close();
}

// =========================================================================
// Test: Diamonds appear on paused line
// =========================================================================

export async function testPanelDiamondsOnPausedLine(browser, extensionId) {
  console.log('\n--- Test Group: Panel Diamonds on Paused Line ---');

  if (!extensionId) {
    assert('Panel paused diamonds: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Pause on line 1 (which has multiple expressions in mock data)
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: {
      filename: 'scheme://inline-scripts/script-0.scm',
      line: 1, column: 1, endLine: 1, endColumn: 26,
    },
    stack: [{ name: 'add', source: null, tcoCount: 0 }],
  });

  // Diamonds should appear on the paused line even without a line breakpoint
  const diamonds = await panelPage.evaluate(() => {
    const els = document.querySelectorAll('.cm-expr-diamond');
    return els.length;
  });
  assert('Diamonds appear on paused line', diamonds >= 1,
    `found ${diamonds} diamonds`);

  // Resume — diamonds should disappear (no breakpoints set)
  await fireResumeEvent(panelPage);

  const diamondsAfter = await panelPage.evaluate(() => {
    const els = document.querySelectorAll('.cm-expr-diamond');
    return els.length;
  });
  assert('Diamonds cleared after resume', diamondsAfter === 0,
    `found ${diamondsAfter} diamonds`);

  await panelPage.close();
}
