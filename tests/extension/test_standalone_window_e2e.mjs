/**
 * @fileoverview End-to-end tests for the standalone debugger window.
 *
 * Unlike test_standalone_window.mjs which uses fully mocked chrome APIs,
 * these tests combine a REAL page running actual Scheme code with the
 * standalone panel UI. Real breakpoints fire on real pages, and the
 * captured pause events are injected into the panel with standalone
 * window mocks — verifying the full flow.
 *
 * These tests catch bugs where:
 *   - Real pause event data doesn't match what the standalone panel expects
 *   - Resume/step buttons don't send correct messages in standalone mode
 *   - Call stack or variables display is wrong with real Scheme execution data
 *   - Source navigation from real breakpoint locations fails
 */

import { assert, INLINE_URL, waitFor, waitForPage, TEST_PAGE } from './test_harness.mjs';
import { buildStandaloneWindowMockScript } from './test_mock_chrome.mjs';
import {
  waitForPause, waitForRunning, getStatus, ackPause, doResume, drainPauses,
} from './test_helpers.mjs';

// =========================================================================
// Helpers
// =========================================================================

/**
 * Opens a real test page with pre-loaded breakpoints and a pause event
 * capture hook.
 *
 * @param {import('puppeteer').Browser} browser
 * @param {Array<{url: string, line: number}>} breakpoints
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openRealPageWithBreakpoints(browser, breakpoints) {
  const page = await browser.newPage();
  await page.evaluateOnNewDocument((bps) => {
    globalThis.__SCHEME_JS_BREAKPOINTS = bps;
    globalThis.__capturedPauseEvents = [];
    window.addEventListener('message', (e) => {
      if (e.data?.type === 'scheme-debug-paused') {
        globalThis.__capturedPauseEvents.push(e.data);
      }
    });
  }, breakpoints);
  await page.goto(TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(
    () => typeof globalThis.__schemeDebug !== 'undefined',
    { timeout: 10000 }
  );
  return page;
}

/**
 * Opens the panel page with standalone window mocks (no chrome.devtools).
 * The mock routes eval calls through __evalDispatch, so the panel sees
 * mock source data — but pause events come from the REAL page.
 *
 * @param {import('puppeteer').Browser} browser
 * @param {string} extensionId
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openStandalonePanel(browser, extensionId) {
  const page = await browser.newPage();
  const script = buildStandaloneWindowMockScript({ cdp: true });
  await page.evaluateOnNewDocument(script);
  await page.goto(
    `chrome-extension://${extensionId}/panel/panel.html?tabId=42`,
    { waitUntil: 'domcontentloaded' }
  );
  await waitForPage(
    page,
    `document.querySelectorAll('#source-list .source-item').length > 0`,
    5000
  );
  return page;
}

/**
 * Captures the latest pause event from the real page and enriches it
 * with the full Scheme call stack from the debug API.
 *
 * @param {import('puppeteer').Page} realPage
 * @returns {Promise<Object|null>} Enriched pause detail, or null if no event
 */
async function captureEnrichedPauseDetail(realPage) {
  const events = await realPage.evaluate(() => globalThis.__capturedPauseEvents);
  if (!events || events.length === 0) return null;
  const latest = events[events.length - 1];
  const apiStack = await realPage.evaluate(() => globalThis.__schemeDebug.getStack());
  return { ...latest.detail, stack: apiStack };
}

// =========================================================================
// Test: Real breakpoint → standalone panel shows paused UI
// =========================================================================

/**
 * Opens a real Scheme page with a breakpoint, captures the cooperative
 * pause event, then opens a standalone panel and fires the real event
 * into it. Verifies the panel shows correct paused state, call stack,
 * and variable data from the actual Scheme execution.
 */
export async function testStandaloneE2ERealBreakpointPause(browser, extensionId) {
  console.log('\n--- Test: Standalone E2E — Real Breakpoint Pause ---');

  // Open real page with breakpoint on line 8 (greeting expression)
  const realPage = await openRealPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);
  await waitForPause(realPage);
  await ackPause(realPage);

  const detail = await captureEnrichedPauseDetail(realPage);
  assert('Real pause event captured', detail !== null,
    'No pause event dispatched from real page');

  if (!detail) {
    await realPage.close().catch(() => {});
    return;
  }

  assert('Real pause has stack frames', detail.stack?.length > 0,
    `stack: ${JSON.stringify(detail.stack)}`);

  // Open standalone panel
  const panelPage = await openStandalonePanel(browser, extensionId);

  // Fire the real pause event into the standalone panel
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);

  // Verify paused state
  const paused = await waitForPage(panelPage,
    `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
    3000);
  assert('Standalone panel shows paused from real event', paused);

  // Verify call stack has real frames
  await waitForPage(panelPage,
    `document.querySelectorAll('#call-stack-container .call-stack-frame').length > 0`,
    3000);
  const frameCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#call-stack-container .call-stack-frame').length
  );
  assert('Standalone panel shows real call stack', frameCount > 0,
    `got ${frameCount} frames`);

  // Verify chrome.devtools is NOT present (standalone mode)
  const hasDevtools = await panelPage.evaluate(() => !!window.chrome?.devtools);
  assert('Standalone panel has no chrome.devtools', hasDevtools === false);

  // Cleanup
  await panelPage.close();
  await doResume(realPage).catch(() => {});
  await drainPauses(realPage).catch(() => {});
  await realPage.close().catch(() => {});
}

// =========================================================================
// Test: Real breakpoint → Resume button click in standalone panel
// =========================================================================

/**
 * Fires a real pause event into the standalone panel, then clicks the
 * Resume button and verifies that the correct resume action was dispatched
 * (Scheme bridge resume, not CDP resume, for cooperative pauses).
 */
export async function testStandaloneE2EResumeButton(browser, extensionId) {
  console.log('\n--- Test: Standalone E2E — Resume Button Click ---');

  const realPage = await openRealPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);
  await waitForPause(realPage);
  await ackPause(realPage);

  const detail = await captureEnrichedPauseDetail(realPage);
  assert('Pause captured for resume test', detail !== null);
  if (!detail) {
    await realPage.close().catch(() => {});
    return;
  }

  const panelPage = await openStandalonePanel(browser, extensionId);

  // Fire real pause event
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
  await waitForPage(panelPage,
    `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
    3000);

  // Clear resume tracking
  await panelPage.evaluate(() => { window.__mockState.resumeCalled = false; });

  // Click the Resume button via DOM (same pattern as existing tests)
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    const resumeBtn = btns.find(b => b.title.includes('Resume'));
    if (resumeBtn) resumeBtn.click();
  });

  // Verify resume was dispatched via the Scheme bridge mock
  const resumed = await waitFor(async () => {
    return panelPage.evaluate(() => window.__mockState.resumeCalled === true);
  }, 3000);
  assert('Resume button dispatched resume() for cooperative pause', resumed);

  await panelPage.close();
  await doResume(realPage).catch(() => {});
  await drainPauses(realPage).catch(() => {});
  await realPage.close().catch(() => {});
}

// =========================================================================
// Test: Real breakpoint → Step button clicks in standalone panel
// =========================================================================

/**
 * Fires a real pause event, then clicks Step Into, Step Over, and Step Out
 * buttons in sequence, verifying each dispatches the correct command.
 */
export async function testStandaloneE2EStepButtons(browser, extensionId) {
  console.log('\n--- Test: Standalone E2E — Step Button Clicks ---');

  const realPage = await openRealPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);
  await waitForPause(realPage);
  await ackPause(realPage);

  const detail = await captureEnrichedPauseDetail(realPage);
  assert('Pause captured for step test', detail !== null);
  if (!detail) {
    await realPage.close().catch(() => {});
    return;
  }

  const panelPage = await openStandalonePanel(browser, extensionId);

  // Fire real pause event
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
  await waitForPage(panelPage,
    `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
    3000);

  // Test Step Into
  await panelPage.evaluate(() => { window.__mockState.stepIntoCalled = false; });
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    const btn = btns.find(b => b.title.includes('Step Into'));
    if (btn) btn.click();
  });
  const stepIntoCalled = await waitFor(async () => {
    return panelPage.evaluate(() => window.__mockState.stepIntoCalled === true);
  }, 2000);
  assert('Step Into button dispatched stepInto()', stepIntoCalled);

  // Re-pause for next test
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
  await waitForPage(panelPage,
    `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
    3000);

  // Test Step Over
  await panelPage.evaluate(() => { window.__mockState.stepOverCalled = false; });
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    const btn = btns.find(b => b.title.includes('Step Over'));
    if (btn) btn.click();
  });
  const stepOverCalled = await waitFor(async () => {
    return panelPage.evaluate(() => window.__mockState.stepOverCalled === true);
  }, 2000);
  assert('Step Over button dispatched stepOver()', stepOverCalled);

  // Re-pause for next test
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
  await waitForPage(panelPage,
    `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
    3000);

  // Test Step Out
  await panelPage.evaluate(() => { window.__mockState.stepOutCalled = false; });
  await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    const btn = btns.find(b => b.title.includes('Step Out'));
    if (btn) btn.click();
  });
  const stepOutCalled = await waitFor(async () => {
    return panelPage.evaluate(() => window.__mockState.stepOutCalled === true);
  }, 2000);
  assert('Step Out button dispatched stepOut()', stepOutCalled);

  await panelPage.close();
  await doResume(realPage).catch(() => {});
  await drainPauses(realPage).catch(() => {});
  await realPage.close().catch(() => {});
}

// =========================================================================
// Test: Real breakpoint → call stack frame click navigates editor
// =========================================================================

/**
 * Fires a real pause event with multiple stack frames, clicks a frame
 * in the call stack, and verifies the editor navigates to that frame's
 * source location.
 */
export async function testStandaloneE2EFrameClickNavigation(browser, extensionId) {
  console.log('\n--- Test: Standalone E2E — Frame Click Navigation ---');

  // Use line 5 (inside factorial) to get a multi-frame stack
  const realPage = await openRealPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 5 }
  ]);
  await waitForPause(realPage);
  await ackPause(realPage);

  const detail = await captureEnrichedPauseDetail(realPage);
  assert('Pause captured for frame click test', detail !== null);
  if (!detail) {
    await realPage.close().catch(() => {});
    return;
  }

  const panelPage = await openStandalonePanel(browser, extensionId);

  // Fire real pause event
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
  await waitForPage(panelPage,
    `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
    3000);

  // Wait for call stack frames to appear
  await waitForPage(panelPage,
    `document.querySelectorAll('#call-stack-container .call-stack-frame').length >= 1`,
    3000);

  const frameCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#call-stack-container .call-stack-frame').length
  );

  if (frameCount >= 2) {
    // Click the bottom frame (should navigate editor to that location)
    const bottomFrameName = await panelPage.evaluate(() => {
      const frames = document.querySelectorAll('#call-stack-container .call-stack-frame');
      const bottom = frames[frames.length - 1]; // bottom = last rendered = first in DOM (reverse order)
      if (bottom) {
        bottom.click();
        return bottom.querySelector('.frame-name')?.textContent || '';
      }
      return null;
    });
    assert('Clicked a different frame', bottomFrameName !== null,
      'Could not find bottom frame to click');

    // Verify the editor shows a line highlight (from the clicked frame's source)
    const hasHighlight = await waitForPage(panelPage,
      `!!document.querySelector('.cm-debug-current-line')`, 2000);
    assert('Editor highlights line after frame click', hasHighlight);
  } else {
    // Only 1 frame — still verify the highlight exists from the pause itself
    const hasHighlight = await panelPage.evaluate(() =>
      !!document.querySelector('.cm-debug-current-line')
    );
    assert('Editor highlights current line with real pause data', hasHighlight);
  }

  await panelPage.close();
  await doResume(realPage).catch(() => {});
  await drainPauses(realPage).catch(() => {});
  await realPage.close().catch(() => {});
}

// =========================================================================
// Test: Standalone panel gutter breakpoint click sets a breakpoint
// =========================================================================

/**
 * Opens the standalone panel, clicks the editor gutter to set a breakpoint,
 * and verifies that the breakpoint was dispatched through the mock eval API.
 * This is a pure UI interaction test — no real page needed.
 */
export async function testStandaloneE2EGutterBreakpointClick(browser, extensionId) {
  console.log('\n--- Test: Standalone E2E — Gutter Breakpoint Click ---');

  const panelPage = await openStandalonePanel(browser, extensionId);

  // Wait for CodeMirror to fully render with line numbers
  await waitForPage(panelPage,
    `!!document.querySelector('.cm-lineNumbers .cm-gutterElement')`,
    5000);

  // Use coordinate-based clicking (same as existing gutter tests).
  // Find the breakpoint gutter X and a line's Y, then use page.mouse.click.
  const clickTarget = await panelPage.evaluate(() => {
    const bpGutter = document.querySelector('.cm-breakpoint-gutter');
    if (!bpGutter) return null;
    const bpRect = bpGutter.getBoundingClientRect();

    // Find the 3rd visible line number for Y coordinate
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

    // Wait for the breakpoint to be registered in mock state
    const bpSet = await waitFor(async () => {
      return panelPage.evaluate(() => window.__mockState.breakpointsSet.length > 0);
    }, 3000);
    assert('Gutter click dispatched setBreakpoint()', bpSet);

    if (bpSet) {
      // Verify the breakpoint dot appears in the gutter
      const hasDot = await waitForPage(panelPage,
        `!!document.querySelector('.cm-breakpoint-dot')`, 2000);
      assert('Breakpoint dot visible after gutter click', hasDot);
    }
  } else {
    assert('Found gutter elements to click', false,
      'breakpoint gutter or line numbers not found');
  }

  await panelPage.close();
}
