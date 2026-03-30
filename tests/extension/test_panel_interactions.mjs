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

import { assert, INLINE_URL, waitFor, waitForPage } from './test_harness.mjs';
import { MOCK_CHROME_SCRIPT } from './test_mock_chrome.mjs';

// Re-export for backward compatibility (test_console.mjs imports from here)
export { MOCK_CHROME_SCRIPT };

// The MOCK_CHROME_SCRIPT is now defined in test_mock_chrome.mjs.
// The following inline definition is kept commented as a reference for
// understanding the mock structure but is no longer used.

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
  // Wait for panel to initialize — source list items appear when ready
  await waitForPage(panelPage, `document.querySelectorAll('#source-list .source-item').length > 0`);
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
  // Wait for toolbar to show paused status
  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`);
}

/**
 * Sends a resume event to the panel.
 * @param {import('puppeteer').Page} panelPage
 */
async function fireResumeEvent(panelPage) {
  await panelPage.evaluate(() => {
    window.__fireMessage({ type: 'scheme-debug-resumed' });
  });
  // Wait for toolbar to show running status
  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent === 'Ready'`);
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
  // Wait for the status bar to reflect the new source
  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent?.includes('manual_script')`);

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
    source: { filename: INLINE_URL, line: 5 },
    stack: [{ name: 'compute', source: { filename: INLINE_URL, line: 5 } }]
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
    source: { filename: INLINE_URL, line: 4, column: 0 },
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
  await waitForPage(panelPage, `window.__mockState.resumeCalled === true`);

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
    source: { filename: INLINE_URL, line: 4, column: 0 },
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
  await waitForPage(panelPage, `window.__mockState.stepIntoCalled === true`);

  let stepIntoCalled = await panelPage.evaluate(() => window.__mockState.stepIntoCalled);
  assert('Clicking Step Into calls stepInto()', stepIntoCalled === true);

  // Re-pause (step might have been consumed)
  await firePauseEvent(panelPage, {
    reason: 'step',
    source: { filename: INLINE_URL, line: 5, column: 0 },
    stack: [],
  });

  // Click Step Over
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Over'))?.click();
  });
  await waitForPage(panelPage, `window.__mockState.stepOverCalled === true`);

  let stepOverCalled = await panelPage.evaluate(() => window.__mockState.stepOverCalled);
  assert('Clicking Step Over calls stepOver()', stepOverCalled === true);

  // Re-pause
  await firePauseEvent(panelPage, {
    reason: 'step',
    source: { filename: INLINE_URL, line: 6, column: 0 },
    stack: [{ name: 'factorial', source: null, tcoCount: 0 }],
  });

  // Click Step Out
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title.includes('Step Out'))?.click();
  });
  await waitForPage(panelPage, `window.__mockState.stepOutCalled === true`);

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
    source: { filename: INLINE_URL, line: 4, column: 0 },
    stack: [
      { name: '<top-level>', source: { filename: INLINE_URL, line: 10, column: 0 }, tcoCount: 0 },
      { name: 'factorial', source: { filename: INLINE_URL, line: 4, column: 0 }, tcoCount: 0 },
    ],
  });

  // The panel auto-loads locals for the top frame (index 1)
  // Wait for variables to populate
  await waitForPage(panelPage, `document.querySelectorAll('#variables-container .variable-row').length >= 2`);

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
  await waitForPage(panelPage, `(() => {
    const rows = document.querySelectorAll('#variables-container .variable-row');
    return Array.from(rows).some(r => r.textContent.includes('x'));
  })()`);

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
    source: { filename: INLINE_URL, line: 1, column: 0 },
    stack: [{ name: 'test', source: null, tcoCount: 0 }],
  });

  // Wait for variables to populate
  await waitForPage(panelPage, `document.querySelectorAll('#variables-container .variable-row').length === 5`);

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
    source: { filename: INLINE_URL, line: 4, column: 0 },
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
  await waitForPage(panelPage, `!!document.querySelector('.cm-lineNumbers .cm-gutterElement')`);

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
    await waitForPage(panelPage, `!!document.querySelector('.cm-breakpoint-dot')`);

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
    source: { filename: INLINE_URL, line: 5, column: 0 },
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

  // Wait for editor to switch to the external source
  await waitForPage(panelPage, `document.querySelector('.cm-content')?.textContent?.includes('fib')`);

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
    source: { filename: INLINE_URL, line: 4, column: 0 },
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
    source: { filename: INLINE_URL, line: 1, column: 0 },
    stack: [{ name: 'test', source: null, tcoCount: 0 }],
  });

  // Wait for variables component to render (empty state)
  await waitForPage(panelPage, `!!document.querySelector('#variables-container .variables-empty')`);

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
      filename: INLINE_URL,
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
      filename: INLINE_URL,
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

  // Wait for CodeMirror to render
  await waitForPage(panelPage, `!!document.querySelector('.cm-lineNumbers .cm-gutterElement')`);

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
    // Wait for breakpoint dot AND diamond markers to appear
    await waitForPage(panelPage, `!!document.querySelector('.cm-breakpoint-dot') && document.querySelectorAll('.cm-expr-diamond').length > 0`);

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
  await waitForPage(panelPage, `!!document.querySelector('.cm-lineNumbers .cm-gutterElement')`);

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
  // Wait for breakpoint dot AND diamond markers
  await waitForPage(panelPage, `!!document.querySelector('.cm-breakpoint-dot') && document.querySelectorAll('.cm-expr-diamond').length > 0`);

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
  await waitForPage(panelPage, `window.__mockState.breakpointsSet.length > 0`);

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

    // Wait for diamond to become active (filled ◆ is the only visual indicator)
    await waitForPage(panelPage, `document.querySelectorAll('.cm-expr-diamond-active').length >= 1`);

    // Diamond should now be filled (active)
    const activeDiamonds = await panelPage.evaluate(() => {
      const els = document.querySelectorAll('.cm-expr-diamond-active');
      return els.length;
    });
    assert('Diamond is now active (filled)', activeDiamonds >= 1,
      `found ${activeDiamonds} active diamonds`);
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
      filename: INLINE_URL,
      line: 1, column: 1, endLine: 1, endColumn: 26,
    },
    stack: [{ name: 'add', source: null, tcoCount: 0 }],
  });

  // Diamonds should appear on the paused line even without a line breakpoint.
  // Wait for them to render (refreshDiamondMarkers runs async after pause).
  const hasDiamonds = await waitForPage(panelPage,
    `document.querySelectorAll('.cm-expr-diamond').length >= 1`, 3000);
  const diamonds = hasDiamonds ? await panelPage.evaluate(() =>
    document.querySelectorAll('.cm-expr-diamond').length
  ) : 0;
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

// =========================================================================
// Test: Breakpoints Panel Population and Removal
// =========================================================================

export async function testPanelBreakpointsList(browser, extensionId) {
  console.log('\n--- Test Group: Panel Breakpoints List ---');

  if (!extensionId) {
    assert('Panel breakpoints list: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Reset breakpoint tracking
  await panelPage.evaluate(() => {
    window.__mockState.breakpointsSet = [];
    window.__mockState.breakpointsRemoved = [];
  });

  // Set a breakpoint via gutter click (the normal panel flow that populates breakpointIds)
  await waitForPage(panelPage, `!!document.querySelector('.cm-lineNumbers .cm-gutterElement')`);

  const clickTarget = await panelPage.evaluate(() => {
    const bpGutter = document.querySelector('.cm-breakpoint-gutter');
    if (!bpGutter) return null;
    const bpRect = bpGutter.getBoundingClientRect();

    // Find the 3rd line number element to get Y coordinate (same as gutter toggle test)
    const lineNums = document.querySelectorAll('.cm-lineNumbers .cm-gutterElement');
    const visibleNums = Array.from(lineNums).filter(el => el.offsetHeight > 0);
    if (visibleNums.length < 3) return null;

    const lineRect = visibleNums[2].getBoundingClientRect();
    return {
      x: bpRect.x + bpRect.width / 2,
      y: lineRect.y + lineRect.height / 2,
    };
  });

  if (!clickTarget) {
    assert('Could find gutter to click for breakpoint', false, 'gutter not found');
    await panelPage.close();
    return;
  }

  await panelPage.mouse.click(clickTarget.x, clickTarget.y);
  await waitForPage(panelPage, `!!document.querySelector('#breakpoints-container .breakpoint-item')`);

  // Check if it appeared in the breakpoints panel list
  let bpItems = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#breakpoints-container .breakpoint-item');
    return Array.from(items).map(el => el.textContent);
  });

  assert('Breakpoint panel has 1 item', bpItems.length === 1, `got ${bpItems.length}`);
  if (bpItems.length === 1) {
    assert('Breakpoint item shows filename',
      bpItems[0].includes('script-0.scm'),
      `got: "${bpItems[0]}"`);
  }

  // Click the remove button in the panel
  await panelPage.evaluate(() => {
    const removeBtn = document.querySelector('#breakpoints-container .breakpoint-remove');
    if (removeBtn) removeBtn.click();
  });
  await waitForPage(panelPage, `document.querySelectorAll('#breakpoints-container .breakpoint-item').length === 0`);

  // Verify the list is empty
  bpItems = await panelPage.evaluate(() => {
    return document.querySelectorAll('#breakpoints-container .breakpoint-item').length;
  });
  assert('Breakpoint panel is empty after removal', bpItems === 0, `got ${bpItems}`);

  // Verify unifiedDebugger was called to remove it
  const removedBps = await panelPage.evaluate(() => window.__mockState.breakpointsRemoved);
  assert('unifiedDebugger.removeBreakpoint was called', removedBps.length > 0, `got ${removedBps.length}`);

  await panelPage.close();
}

// =========================================================================
// Test: Breakpoints Panel Navigation
// =========================================================================

export async function testPanelBreakpointsNavigation(browser, extensionId) {
  console.log('\n--- Test Group: Panel Breakpoints Navigation ---');

  if (!extensionId) {
    assert('Panel breakpoints nav: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Set a breakpoint on the first source via gutter click (line 4)
  await waitForPage(panelPage, `!!document.querySelector('.cm-lineNumbers .cm-gutterElement')`);

  const clickTarget = await panelPage.evaluate(() => {
    const bpGutter = document.querySelector('.cm-breakpoint-gutter');
    if (!bpGutter) return null;
    const bpRect = bpGutter.getBoundingClientRect();

    const lineNums = document.querySelectorAll('.cm-lineNumbers .cm-gutterElement');
    const visibleNums = Array.from(lineNums).filter(el => el.offsetHeight > 0);
    if (visibleNums.length < 3) return null;

    const lineRect = visibleNums[2].getBoundingClientRect();
    return {
      x: bpRect.x + bpRect.width / 2,
      y: lineRect.y + lineRect.height / 2,
    };
  });

  if (!clickTarget) {
    assert('Could find gutter to click for breakpoint', false, 'gutter not found');
    await panelPage.close();
    return;
  }

  await panelPage.mouse.click(clickTarget.x, clickTarget.y);
  await waitForPage(panelPage, `document.querySelectorAll('#breakpoints-container .breakpoint-item').length >= 1`);

  // Verify breakpoint was set
  const bpCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#breakpoints-container .breakpoint-item').length
  );
  assert('Breakpoint was set in first source', bpCount >= 1, `got ${bpCount}`);

  // Switch to the second source
  await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#source-list .source-item');
    if (items[1]) items[1].click();
  });
  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent?.includes('manual_script')`);

  let status = await panelPage.evaluate(() => document.querySelector('.toolbar-status')?.textContent);
  assert('Now viewing second source', status?.includes('manual_script'), `got: "${status}"`);

  // Click the breakpoint item to navigate back
  await panelPage.evaluate(() => {
    const bpItem = document.querySelector('#breakpoints-container .breakpoint-item');
    if (bpItem) bpItem.click();
  });
  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent?.includes('script-0')`);

  // Verify we navigated back to the first source
  status = await panelPage.evaluate(() => document.querySelector('.toolbar-status')?.textContent);
  assert('Clicking breakpoint navigated back to first source', status?.includes('script-0'), `got: "${status}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Keyboard Shortcuts
// =========================================================================

export async function testKeyboardShortcuts(browser, extensionId) {
  console.log('\n--- Test Group: Keyboard Shortcuts ---');

  if (!extensionId) {
    assert('Keyboard shortcuts test: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // We are paused initially based on mock data in openMockedPanel
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: INLINE_URL, line: 1 },
    stack: [{ name: 'add', source: null, tcoCount: 0 }],
  });

  // Test F8 (Resume)
  await panelPage.evaluate(() => {
    window.__mockState.lastAction = null;
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'F8' }));
  });
  await waitForPage(panelPage, `window.__mockState.lastAction === 'resume'`);
  let lastAction = await panelPage.evaluate(() => window.__mockState.lastAction);
  assert('F8 triggers unifiedDebugger.resume', lastAction === 'resume', `got: ${lastAction}`);

  // Test F10 (Step Over)
  await panelPage.evaluate(() => {
    window.__mockState.lastAction = null;
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'F10' }));
  });
  await waitForPage(panelPage, `window.__mockState.lastAction === 'stepOver'`);
  lastAction = await panelPage.evaluate(() => window.__mockState.lastAction);
  assert('F10 triggers unifiedDebugger.stepOver', lastAction === 'stepOver', `got: ${lastAction}`);

  // Test F11 (Step Into)
  await panelPage.evaluate(() => {
    window.__mockState.lastAction = null;
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'F11', shiftKey: false }));
  });
  await waitForPage(panelPage, `window.__mockState.lastAction === 'stepInto'`);
  lastAction = await panelPage.evaluate(() => window.__mockState.lastAction);
  assert('F11 triggers unifiedDebugger.stepInto', lastAction === 'stepInto', `got: ${lastAction}`);

  // Test Shift+F11 (Step Out)
  await panelPage.evaluate(() => {
    window.__mockState.lastAction = null;
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'F11', shiftKey: true }));
  });
  await waitForPage(panelPage, `window.__mockState.lastAction === 'stepOut'`);
  lastAction = await panelPage.evaluate(() => window.__mockState.lastAction);
  assert('Shift+F11 triggers unifiedDebugger.stepOut', lastAction === 'stepOut', `got: ${lastAction}`);

  await panelPage.close();
}

// =========================================================================
// Test: Source list updates on pause navigation (Bug 1)
// =========================================================================

export async function testSourceListUpdatesOnPauseNavigation(browser, extensionId) {
  console.log('\n--- Test Group: Source List Updates on Pause Navigation ---');

  if (!extensionId) {
    assert('Source list navigation: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Verify initial selection is first source
  const initialSelected = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#source-list .source-item');
    return Array.from(items).map(el => ({
      url: el.dataset.url,
      selected: el.classList.contains('selected'),
    }));
  });
  assert('Initially first source is selected', initialSelected[0]?.selected === true);

  // Set up locals for pause
  await panelPage.evaluate(() => {
    window.__mockState.locals = {
      0: [{ name: 'x', value: '42', type: 'number', subtype: null }]
    };
  });

  // Fire a pause that points to the second source
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: {
      filename: 'scheme://scheme-sources/manual_script.scm',
      line: 3,
      column: 0,
    },
    stack: [{
      name: 'fib',
      source: { filename: 'scheme://scheme-sources/manual_script.scm', line: 3, column: 0 },
      tcoCount: 0,
    }],
  });

  // Wait for source list to update (loadSource is async)
  await waitForPage(panelPage,
    `document.querySelector('#source-list .source-item:nth-child(2)')?.classList?.contains('selected')`
  );

  // Source list should now have the second item selected
  const afterPause = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#source-list .source-item');
    return Array.from(items).map(el => ({
      url: el.dataset.url,
      selected: el.classList.contains('selected'),
    }));
  });
  assert('After pause, second source is selected',
    afterPause[1]?.selected === true,
    `selection: ${JSON.stringify(afterPause)}`);
  assert('After pause, first source is NOT selected',
    afterPause[0]?.selected === false,
    `selection: ${JSON.stringify(afterPause)}`);

  await panelPage.close();
}

// =========================================================================
// Test: Variables show scope sections (Bug 2)
// =========================================================================

export async function testVariableScopeHeaders(browser, extensionId) {
  console.log('\n--- Test Group: Variable Scope Headers ---');

  if (!extensionId) {
    assert('Scope headers: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Set up mock locals with scope information
  await panelPage.evaluate(() => {
    window.__mockState.locals = {
      0: [
        { name: 'x', value: '10', type: 'number', subtype: null, scope: 'local' },
        { name: 'y', value: '20', type: 'number', subtype: null, scope: 'local' },
        { name: 'parent-val', value: '"hello"', type: 'string', subtype: null, scope: 'closure' },
        { name: 'top-def', value: '42', type: 'number', subtype: null, scope: 'global' },
      ]
    };
  });

  // Fire pause
  await firePauseEvent(panelPage, {
    reason: 'breakpoint',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 5, column: 0 },
    stack: [{
      name: 'inner-fn',
      source: { filename: 'scheme://inline-scripts/script-0.scm', line: 5, column: 0 },
      tcoCount: 0,
    }],
  });

  // Wait for variables to render
  await waitForPage(panelPage, `document.querySelectorAll('#variables-container .variable-row').length >= 4`);

  // Check scope headers
  const headers = await panelPage.evaluate(() => {
    const els = document.querySelectorAll('#variables-container .variables-scope-header');
    return Array.from(els).map(el => el.textContent);
  });
  assert('Has Local scope header', headers.includes('Local'), `got: ${JSON.stringify(headers)}`);
  assert('Has Closure scope header', headers.includes('Closure'), `got: ${JSON.stringify(headers)}`);
  assert('Has Global scope header', headers.includes('Global'), `got: ${JSON.stringify(headers)}`);

  // Check variable count
  const varCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#variables-container .variable-row').length
  );
  assert('4 variables displayed across scopes', varCount === 4, `got ${varCount}`);

  await panelPage.close();
}

// =========================================================================
// Test: Step reason shows last result (Bug 3c)
// =========================================================================

export async function testStepLastResultDisplay(browser, extensionId) {
  console.log('\n--- Test Group: Step Last Result Display ---');

  if (!extensionId) {
    assert('Step last result: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Set up locals
  await panelPage.evaluate(() => {
    window.__mockState.locals = {
      0: [{ name: 'x', value: '5', type: 'number', subtype: null }]
    };
  });

  // Fire a step pause with lastResult
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, {
    reason: 'step',
    lastResult: '42',
    source: { filename: 'scheme://inline-scripts/script-0.scm', line: 6, column: 0 },
    stack: [{
      name: '<top-level>',
      source: { filename: 'scheme://inline-scripts/script-0.scm', line: 6, column: 0 },
      tcoCount: 0,
    }],
  });

  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`);

  const status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Status shows step with last result',
    status?.includes('← 42'),
    `got: "${status}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Console output from page (Feature 6)
// =========================================================================

export async function testConsolePageOutput(browser, extensionId) {
  console.log('\n--- Test Group: Console Page Output ---');

  if (!extensionId) {
    assert('Console page output: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Send console-api-called messages
  await panelPage.evaluate(() => {
    window.__fireMessage({
      type: 'console-api-called',
      callType: 'log',
      args: ['Hello', 'World'],
    });
    window.__fireMessage({
      type: 'console-api-called',
      callType: 'error',
      args: ['Something went wrong'],
    });
    window.__fireMessage({
      type: 'console-api-called',
      callType: 'warn',
      args: ['Be careful'],
    });
  });

  // Wait for console output
  await waitForPage(panelPage, `document.querySelectorAll('.console-page-output').length >= 3`);

  const outputs = await panelPage.evaluate(() => {
    const els = document.querySelectorAll('.console-page-output');
    return Array.from(els).map(el => ({
      text: el.querySelector('.console-text')?.textContent,
      type: el.dataset.messageType,
      isError: el.classList.contains('console-page-error'),
      isWarn: el.classList.contains('console-page-warn'),
    }));
  });

  assert('3 console outputs displayed', outputs.length === 3, `got ${outputs.length}`);
  assert('First is log', outputs[0]?.text === 'Hello World', `got: "${outputs[0]?.text}"`);
  assert('Second is error', outputs[1]?.isError === true);
  assert('Third is warn', outputs[2]?.isWarn === true);
  assert('Error text correct', outputs[1]?.text === 'Something went wrong');

  await panelPage.close();
}

// =========================================================================
// Test: CodeMirror Search
// =========================================================================

export async function testCodeMirrorSearch(browser, extensionId) {
  console.log('\n--- Test Group: CodeMirror Search ---');

  if (!extensionId) {
    assert('CodeMirror search test: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Press Meta+F (Mac) or Ctrl+F (Windows/Linux) to open search panel
  await panelPage.evaluate(() => {
    const isMac = navigator.platform.toUpperCase().indexOf('MAC') >= 0;
    const evt = new KeyboardEvent('keydown', {
      key: 'f',
      metaKey: isMac,
      ctrlKey: !isMac,
      bubbles: true,
      cancelable: true
    });
    document.querySelector('.cm-content').dispatchEvent(evt);
  });
  await waitForPage(panelPage, `document.querySelectorAll('.cm-search').length > 0`);

  // Check if search panel appeared
  const searchPanelMatches = await panelPage.evaluate(() => {
    return document.querySelectorAll('.cm-search').length;
  });
  assert('CodeMirror search panel is visible', searchPanelMatches > 0, `Search panels found: ${searchPanelMatches}`);

  await panelPage.close();
}
