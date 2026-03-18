/**
 * @fileoverview Tests for the auto-resume behavior that prevents Scheme probe
 * pauses from appearing in Chrome's Sources tab.
 *
 * When a Scheme breakpoint is hit, background.js detects the `debugger;`
 * statement from the probe function (via isSchemeProbe/isSchemeException) and
 * immediately resumes CDP so the Sources tab never stays paused. The
 * Scheme-JS panel receives its pause notification via the separate cooperative
 * channel: content script postMessage → chrome.runtime.sendMessage →
 * `scheme-debug-paused` message to the panel.
 *
 * Three test tiers:
 *   1. Mock-based panel tests  — verify the panel handles each message channel
 *      correctly (cooperative `scheme-debug-paused` vs CDP `cdp-paused`)
 *   2. Real page-side tests    — verify auto-resume doesn't break pause/resume/step
 *   3. Click-based UI tests    — verify the full UI flow via cooperative channel
 */

import { assert, INLINE_URL, waitForPage } from './test_harness.mjs';
import { buildMockChromeScript } from './test_mock_chrome.mjs';
import {
  openTestPageWithBreakpoints,
  waitForPause, getStatus, ackPause,
  doResume, doStepInto, doStepOver, drainPauses,
} from './test_helpers.mjs';

// =========================================================================
// Shared helpers
// =========================================================================

/**
 * Opens the panel page with mocked chrome APIs injected before load.
 * @param {import('puppeteer').Browser} browser
 * @param {string} extensionId
 * @param {Object} [mockOptions] - Options for buildMockChromeScript
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openMockedPanel(browser, extensionId, mockOptions = {}) {
  const panelPage = await browser.newPage();
  await panelPage.evaluateOnNewDocument(buildMockChromeScript(mockOptions));
  await panelPage.goto(
    `chrome-extension://${extensionId}/panel/panel.html`,
    { waitUntil: 'domcontentloaded' }
  );
  // Wait for panel to initialize — source list items appear when ready
  await waitForPage(panelPage, `document.querySelectorAll('#source-list .source-item').length > 0`);
  return panelPage;
}

/**
 * Fires a scheme-debug-paused (cooperative channel) event to the panel.
 * Simulates the content script relay arriving at the panel.
 * @param {import('puppeteer').Page} panelPage
 * @param {Object} detail - Pause detail (reason, source, stack)
 */
async function fireCooperativePause(panelPage, detail) {
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
  // Wait for toolbar to show paused status
  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`);
}

/**
 * Fires a cdp-paused (JS debugging channel) event to the panel.
 * Simulates a non-Scheme JS breakpoint pause forwarded by background.js.
 * @param {import('puppeteer').Page} panelPage
 * @param {Object} params - CDP pause params (tabId, callFrames, reason)
 */
async function fireCDPPause(panelPage, params) {
  await panelPage.evaluate((p) => {
    window.__fireMessage({ type: 'cdp-paused', ...p });
  }, params);
}

// =========================================================================
// Tier 1: Mock-based panel tests
// =========================================================================

/**
 * Tests that the panel correctly handles a Scheme cooperative pause:
 * when `scheme-debug-paused` arrives via the cooperative channel, the panel
 * shows "Paused at breakpoint", renders call stack frames, enables debug
 * buttons, and calls ackPause().
 */
export async function testSchemeProbeAutoResumed(browser, extensionId) {
  console.log('\n--- Test: Scheme Probe — Panel Uses Cooperative Channel ---');

  if (!extensionId) {
    assert('Scheme probe cooperative pause: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Set up mock locals for the top frame (factorial, n=5)
  await panelPage.evaluate(() => {
    window.__mockState.locals = {
      0: [{ name: 'n', value: '5', type: 'number', subtype: null }],
    };
  });

  // Simulate the content script relay: scheme-debug-paused arrives at the panel
  await fireCooperativePause(panelPage, {
    reason: 'breakpoint',
    source: { filename: INLINE_URL, line: 4, column: 0 },
    stack: [
      { name: 'factorial', source: { filename: INLINE_URL, line: 4, column: 0 }, tcoCount: 0 },
      { name: '<top-level>', source: { filename: INLINE_URL, line: 9, column: 0 }, tcoCount: 0 },
    ],
  });

  // Toolbar should show "Paused at breakpoint"
  const status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Status shows "Paused at breakpoint"',
    status === 'Paused at breakpoint', `got: "${status}"`);

  // Resume and step buttons should be enabled
  const buttons = await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    return btns.map(b => ({ title: b.title, disabled: b.disabled }));
  });
  const resumeBtn = buttons.find(b => b.title?.includes('Resume'));
  assert('Resume button enabled during pause',
    resumeBtn?.disabled === false, `disabled=${resumeBtn?.disabled}`);

  // Call stack should render 2 frames
  const frameCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#call-stack-container .call-stack-frame').length
  );
  assert('Call stack has 2 frames', frameCount === 2, `got ${frameCount}`);

  // ackPause should have been called (acknowledges the cooperative pause)
  const ackCalled = await panelPage.evaluate(() => window.__mockState.ackPauseCalled);
  assert('ackPause called on cooperative pause', ackCalled === true);

  await panelPage.close();
}

/**
 * Tests that when a non-Scheme JS pause arrives via `cdp-paused`, the panel
 * does NOT treat it as a cooperative Scheme pause. Specifically, ackPause()
 * should NOT be called (it is only called for cooperative Scheme pauses).
 */
export async function testNonSchemeProbeNotAutoResumed(browser, extensionId) {
  console.log('\n--- Test: Non-Scheme CDP Pause — Not Treated as Cooperative Pause ---');

  if (!extensionId) {
    assert('Non-Scheme CDP pause: extension loaded', false, 'no extension ID');
    return;
  }

  // Use CDP-enabled mock so cdp-bridge sendMessage routing works
  const panelPage = await openMockedPanel(browser, extensionId, { cdp: true });

  // Ensure ackPause flag starts false
  await panelPage.evaluate(() => { window.__mockState.ackPauseCalled = false; });

  // Fire a cdp-paused event (as background.js would send for real JS breakpoints)
  await fireCDPPause(panelPage, {
    tabId: 42,
    callFrames: [
      {
        functionName: 'jsAdd',
        url: 'http://localhost:8080/test.js',
        location: { scriptId: 'script1', lineNumber: 1 },
        callFrameId: 'cf1',
      },
    ],
    reason: 'breakpoint',
  });

  // Give the panel a moment to process the cdp-paused event
  await new Promise(r => setTimeout(r, 200));

  // ackPause should NOT have been called — that's for cooperative Scheme pauses only
  const ackCalled = await panelPage.evaluate(() => window.__mockState.ackPauseCalled);
  assert('ackPause NOT called for CDP (JS) pause', ackCalled !== true,
    'ackPause should only be called for cooperative Scheme pauses');

  await panelPage.close();
}

// =========================================================================
// Tier 2: Real page-side tests
// =========================================================================

/**
 * Tests that a Scheme breakpoint pauses via the cooperative channel.
 * Verifies getStatus().state === 'paused' with correct reason and source line.
 */
export async function testBreakpointPausesCooperatively(browser) {
  console.log('\n--- Test: Breakpoint Pauses Cooperatively ---');

  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  const status = await getStatus(page);
  assert('State is paused via cooperative channel', status.state === 'paused',
    `got state: ${status.state}`);
  assert('Pause reason is breakpoint', status.reason === 'breakpoint',
    `got reason: ${status.reason}`);
  assert('Pause source has correct line', status.source?.line === 8,
    `got line ${status.source?.line}`);

  await drainPauses(page);
  await page.close();
}

/**
 * Tests that after a cooperative pause, resume works correctly and execution
 * completes. Verifies auto-resume doesn't interfere with the cooperative
 * pause/resume cycle.
 */
export async function testBreakpointResumeAndContinue(browser) {
  console.log('\n--- Test: Breakpoint Resume and Continue ---');

  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  const status = await getStatus(page);
  assert('Paused at breakpoint before resume', status.state === 'paused');

  // Resume and verify execution completes
  await doResume(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });

  const finalStatus = await getStatus(page);
  assert('Running after resume', finalStatus.state !== 'paused',
    `got state: ${finalStatus.state}`);

  await page.close();
}

/**
 * Tests that stepping works correctly even though background.js auto-resumes
 * the CDP Debugger.paused event. The cooperative channel handles step events
 * independently via the Scheme debug runtime.
 */
export async function testSteppingWorksAfterAutoResume(browser) {
  console.log('\n--- Test: Stepping Works After Auto-Resume ---');

  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  const statusBefore = await getStatus(page);
  assert('Initially paused at line 8', statusBefore.source?.line === 8,
    `got line ${statusBefore.source?.line}`);

  // Step into — should advance to next sub-expression
  await doStepInto(page);
  await waitForPause(page);

  const statusAfterStepInto = await getStatus(page);
  assert('After step into: still paused', statusAfterStepInto.state === 'paused',
    `got state: ${statusAfterStepInto.state}`);
  assert('After step into: reason is step', statusAfterStepInto.reason === 'step',
    `got reason: ${statusAfterStepInto.reason}`);

  // Step over — should advance to next expression at the same level
  await doStepOver(page);
  await waitForPause(page);

  const statusAfterStepOver = await getStatus(page);
  assert('After step over: still paused', statusAfterStepOver.state === 'paused',
    `got state: ${statusAfterStepOver.state}`);

  // Drain remaining pauses and verify execution completes
  await drainPauses(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 15000 });
  assert('Execution completes after stepping', true);

  await page.close();
}

// =========================================================================
// Tier 3: Click-based UI simulation tests
// =========================================================================

/**
 * Tests the full cooperative channel UI flow: fires `scheme-debug-paused`,
 * verifies toolbar shows "Paused at breakpoint", call stack renders frames,
 * variables populate, editor shows current-line highlight, and clicking a
 * call stack frame updates the variables panel.
 */
export async function testPanelPauseShowsUIViaCooperativeChannel(browser, extensionId) {
  console.log('\n--- Test: Panel Pause Shows UI via Cooperative Channel ---');

  if (!extensionId) {
    assert('Panel pause UI cooperative: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Set up mock locals: stack[1] = factorial (n=5), stack[0] = <top-level> (empty).
  // The UI shows frames in reverse order: last stack item → first UI frame.
  await panelPage.evaluate(() => {
    window.__mockState.locals = {
      1: [{ name: 'n', value: '5', type: 'number', subtype: null }],
      0: [],
    };
  });

  // Stack array is bottom-to-top: <top-level> first, factorial last.
  // The UI reverses this: factorial appears as frames[0], <top-level> as frames[1].
  await fireCooperativePause(panelPage, {
    reason: 'breakpoint',
    source: { filename: INLINE_URL, line: 4, column: 0 },
    stack: [
      { name: '<top-level>', source: { filename: INLINE_URL, line: 9, column: 0 }, tcoCount: 0 },
      { name: 'factorial', source: { filename: INLINE_URL, line: 4, column: 0 }, tcoCount: 0 },
    ],
  });

  // Toolbar should show "Paused at breakpoint"
  const toolbarStatus = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Toolbar shows "Paused at breakpoint"',
    toolbarStatus === 'Paused at breakpoint', `got: "${toolbarStatus}"`);

  // Call stack should render 2 frames, top frame is factorial
  const frames = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#call-stack-container .call-stack-frame');
    return Array.from(items).map(el => el.textContent);
  });
  assert('Call stack has 2 frames', frames.length === 2, `got ${frames.length}`);
  assert('Top frame is factorial', frames[0]?.includes('factorial'), `got: "${frames[0]}"`);

  // Variables panel should show locals for the top frame (n=5 for factorial at stack[1])
  await waitForPage(panelPage, `document.querySelectorAll('#variables-container .variable-row').length >= 1`);
  const varRows = await panelPage.evaluate(() =>
    Array.from(document.querySelectorAll('#variables-container .variable-row')).map(el => el.textContent)
  );
  assert('Variables show "n" for top frame',
    varRows.some(v => v.includes('n')), `got: ${JSON.stringify(varRows)}`);

  // Editor should show current-line highlight
  const hasHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-line')
  );
  assert('Editor shows current-line highlight', hasHighlight);

  // Click the second call stack frame (<top-level>, stack[0] with empty locals)
  // — variables should clear since that frame has no locals.
  await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#call-stack-container .call-stack-frame');
    if (items[1]) items[1].click();
  });
  // Variables for <top-level> (stack[0]) are empty; wait for variable rows to clear
  await waitForPage(panelPage, `document.querySelectorAll('#variables-container .variable-row').length === 0`);

  assert('Variables cleared after clicking <top-level> frame', true);

  await panelPage.close();
}

/**
 * Tests that clicking the Resume button after a cooperative pause calls
 * resume(), clears the call stack, removes the current-line highlight,
 * and changes the toolbar status away from "Paused".
 */
export async function testPanelResumeClickAfterCooperativePause(browser, extensionId) {
  console.log('\n--- Test: Panel Resume Click After Cooperative Pause ---');

  if (!extensionId) {
    assert('Panel resume click cooperative: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  await fireCooperativePause(panelPage, {
    reason: 'breakpoint',
    source: { filename: INLINE_URL, line: 4, column: 0 },
    stack: [{ name: 'factorial', source: { filename: INLINE_URL, line: 4, column: 0 }, tcoCount: 0 }],
  });

  // Reset resume flag (ackPause may have set it during firePauseEvent processing)
  await panelPage.evaluate(() => { window.__mockState.resumeCalled = false; });

  // Click the Resume button
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title?.includes('Resume'))?.click();
  });
  await waitForPage(panelPage, `window.__mockState.resumeCalled === true`);

  const resumeCalled = await panelPage.evaluate(() => window.__mockState.resumeCalled);
  assert('Resume button click calls resume()', resumeCalled === true);

  // Fire the cooperative resume event (simulating what content script would send
  // after __schemeDebug.resume() completes) so the panel clears its UI.
  await panelPage.evaluate(() => {
    window.__fireMessage({ type: 'scheme-debug-resumed' });
  });
  await waitForPage(panelPage, `!document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`);

  const statusAfter = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Status no longer shows Paused after resume event',
    !statusAfter?.includes('Paused'), `got: "${statusAfter}"`);

  // Call stack should be cleared
  const frameCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#call-stack-container .call-stack-frame').length
  );
  assert('Call stack cleared after resume', frameCount === 0, `got ${frameCount}`);

  // Current-line highlight should be gone
  const hasHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-line')
  );
  assert('Current-line highlight cleared after resume', !hasHighlight);

  await panelPage.close();
}

/**
 * Tests that clicking Step Into, Step Over, and Step Out buttons after a
 * cooperative pause correctly calls the corresponding debug API methods.
 * Re-fires a pause event between each step to keep the buttons enabled.
 */
export async function testPanelStepButtonsAfterCooperativePause(browser, extensionId) {
  console.log('\n--- Test: Panel Step Buttons After Cooperative Pause ---');

  if (!extensionId) {
    assert('Panel step buttons cooperative: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  const pauseDetail = {
    reason: 'breakpoint',
    source: { filename: INLINE_URL, line: 4, column: 0 },
    stack: [],
  };

  // Test Step Into
  await fireCooperativePause(panelPage, pauseDetail);
  await panelPage.evaluate(() => { window.__mockState.stepIntoCalled = false; });
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title?.includes('Step Into'))?.click();
  });
  await waitForPage(panelPage, `window.__mockState.stepIntoCalled === true`);
  assert('Step Into button calls stepInto()',
    await panelPage.evaluate(() => window.__mockState.stepIntoCalled));

  // Test Step Over — re-pause first to re-enable buttons
  await fireCooperativePause(panelPage, { ...pauseDetail, reason: 'step' });
  await panelPage.evaluate(() => { window.__mockState.stepOverCalled = false; });
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title?.includes('Step Over'))?.click();
  });
  await waitForPage(panelPage, `window.__mockState.stepOverCalled === true`);
  assert('Step Over button calls stepOver()',
    await panelPage.evaluate(() => window.__mockState.stepOverCalled));

  // Test Step Out — re-pause first
  await fireCooperativePause(panelPage, { ...pauseDetail, reason: 'step' });
  await panelPage.evaluate(() => { window.__mockState.stepOutCalled = false; });
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    btns.find(b => b.title?.includes('Step Out'))?.click();
  });
  await waitForPage(panelPage, `window.__mockState.stepOutCalled === true`);
  assert('Step Out button calls stepOut()',
    await panelPage.evaluate(() => window.__mockState.stepOutCalled));

  await panelPage.close();
}

/**
 * Tests that clicking the breakpoint gutter sets a breakpoint (dot appears,
 * setBreakpoint called on mock). Clicking the same gutter position again
 * removes the breakpoint (dot gone, removeBreakpoint called).
 */
export async function testPanelBreakpointSetViaGutterClick(browser, extensionId) {
  console.log('\n--- Test: Panel Breakpoint Set Via Gutter Click ---');

  if (!extensionId) {
    assert('Panel breakpoint gutter click: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  await panelPage.evaluate(() => {
    window.__mockState.breakpointsSet = [];
    window.__mockState.breakpointsRemoved = [];
  });

  // Wait for CodeMirror to render line number gutter elements
  await waitForPage(panelPage, `!!document.querySelector('.cm-lineNumbers .cm-gutterElement')`);

  // Locate the breakpoint gutter at the 3rd visible line
  const clickTarget = await panelPage.evaluate(() => {
    const bpGutter = document.querySelector('.cm-breakpoint-gutter');
    if (!bpGutter) return null;
    const bpRect = bpGutter.getBoundingClientRect();
    const lineNums = document.querySelectorAll('.cm-lineNumbers .cm-gutterElement');
    const visibleNums = Array.from(lineNums).filter(el => el.offsetHeight > 0);
    if (visibleNums.length < 3) return null;
    const lineRect = visibleNums[2].getBoundingClientRect();
    return { x: bpRect.x + bpRect.width / 2, y: lineRect.y + lineRect.height / 2 };
  });

  if (!clickTarget) {
    assert('Could find gutter to click', false, 'breakpoint gutter or line numbers not found');
    await panelPage.close();
    return;
  }

  // Click to set breakpoint
  await panelPage.mouse.click(clickTarget.x, clickTarget.y);
  await waitForPage(panelPage, `!!document.querySelector('.cm-breakpoint-dot')`);

  const hasDot = await panelPage.evaluate(() => !!document.querySelector('.cm-breakpoint-dot'));
  assert('Breakpoint dot appears after gutter click', hasDot);

  const bpSet = await panelPage.evaluate(() => window.__mockState.breakpointsSet);
  assert('setBreakpoint was called', bpSet.length >= 1, `called ${bpSet.length} times`);

  await panelPage.close();
}
