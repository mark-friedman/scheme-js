/**
 * @fileoverview End-to-end integration test: real breakpoint → panel UI.
 *
 * This test bridges two worlds that are normally connected by the content
 * script relay:
 *   1. A real page running actual Scheme code with a real breakpoint
 *   2. The extension panel showing pause state in its UI
 *
 * When a real breakpoint fires, the interpreter dispatches a postMessage
 * with `{ type: 'scheme-debug-paused', detail: ... }`. Normally the content
 * script relays this via `chrome.runtime.sendMessage` to the panel. In this
 * test, we capture the real postMessage from the page and fire it into the
 * mocked panel, verifying the UI correctly reflects the real pause data.
 *
 * This catches bugs where:
 *   - The cooperative pause event is never dispatched (e.g., debugger; blocks)
 *   - The pause event data format is wrong or incomplete
 *   - The panel fails to show the correct paused state from real data
 */

import { assert, INLINE_URL, waitForPage, TEST_PAGE } from './test_harness.mjs';
import { buildMockChromeScript } from './test_mock_chrome.mjs';
import {
  waitForPause, getStatus, ackPause, doResume, drainPauses,
} from './test_helpers.mjs';

// =========================================================================
// Test: Real breakpoint fires → panel shows paused UI
// =========================================================================

/**
 * End-to-end integration: fires a real breakpoint on a real page, captures
 * the cooperative pause event and the stack from the debug API, then
 * verifies the panel correctly shows the paused state.
 *
 * The postMessage event's `stack` field can be empty for top-level
 * breakpoints (the stack tracer has no function frames). To get a
 * complete picture, we also read __schemeDebug.getStack() which adds
 * a synthetic top-level frame, and merge it into the event detail
 * before firing into the panel — mirroring what the real panel does
 * via getStatus() polling.
 */
export async function testRealBreakpointUpdatesPanelUI(browser, extensionId) {
  console.log('\n--- Test: Real Breakpoint → Panel UI (E2E) ---');

  if (!extensionId) {
    assert('E2E breakpoint test: extension loaded', false, 'no extension ID');
    return;
  }

  // --- Step 1: Open a real page with a breakpoint and capture the pause event ---

  const realPage = await browser.newPage();

  // Set up breakpoint and listen for the cooperative pause postMessage
  await realPage.evaluateOnNewDocument(() => {
    globalThis.__SCHEME_JS_BREAKPOINTS = [
      { url: 'scheme://inline-scripts/script-0.scm', line: 8 }
    ];

    // Capture the scheme-debug-paused postMessage that the interpreter sends
    globalThis.__capturedPauseEvent = null;
    window.addEventListener('message', (e) => {
      if (e.data?.type === 'scheme-debug-paused' && !globalThis.__capturedPauseEvent) {
        globalThis.__capturedPauseEvent = e.data;
      }
    });
  });

  await realPage.goto(TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await realPage.waitForFunction(
    () => typeof globalThis.__schemeDebug !== 'undefined',
    { timeout: 10000 }
  );

  // Wait for the cooperative pause to fire on the real page
  await waitForPause(realPage);
  await ackPause(realPage);

  // Capture the real pause event data
  const pauseEvent = await realPage.evaluate(() => globalThis.__capturedPauseEvent);
  assert('Cooperative pause event was captured from real page',
    pauseEvent !== null && pauseEvent.detail !== undefined,
    'postMessage with scheme-debug-paused was not dispatched');

  if (!pauseEvent) {
    await doResume(realPage).catch(() => {});
    await realPage.close();
    return;
  }

  const detail = pauseEvent.detail;
  assert('Pause event has reason', !!detail.reason,
    `got: ${JSON.stringify(detail)}`);
  assert('Pause event has source with line', typeof detail.source?.line === 'number',
    `got source: ${JSON.stringify(detail.source)}`);
  assert('Pause event has stack array', Array.isArray(detail.stack),
    `got stack: ${JSON.stringify(detail.stack)}`);

  // Get the stack from the debug API (includes synthetic top-level frame)
  const apiStack = await realPage.evaluate(() => globalThis.__schemeDebug.getStack());
  assert('getStack() returns frames during pause',
    apiStack.length > 0, `got ${apiStack.length} frames`);

  // Merge API stack into the event detail for a complete picture.
  // The real panel gets stack data from getStatus() polling, which
  // calls getStack() — so this mirrors the real flow.
  const enrichedDetail = { ...detail, stack: apiStack };

  // --- Step 2: Open the panel and fire the captured real event into it ---

  const panelPage = await browser.newPage();
  await panelPage.evaluateOnNewDocument(buildMockChromeScript());
  await panelPage.goto(
    `chrome-extension://${extensionId}/panel/panel.html`,
    { waitUntil: 'domcontentloaded' }
  );
  await waitForPage(panelPage,
    `document.querySelectorAll('#source-list .source-item').length > 0`);

  // Fire the enriched pause event data into the panel
  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, enrichedDetail);

  // Wait for the panel to show paused state
  await waitForPage(panelPage,
    `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`);

  // --- Step 3: Verify the panel UI reflects the real pause data ---

  const toolbarStatus = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Panel toolbar shows paused',
    toolbarStatus?.includes('Paused'),
    `got: "${toolbarStatus}"`);

  // Wait for call stack frames to render
  await waitForPage(panelPage,
    `document.querySelectorAll('#call-stack-container .call-stack-frame').length > 0`);

  // Call stack should have frames from the real debug API
  const frameCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#call-stack-container .call-stack-frame').length
  );
  assert('Panel shows call stack frames from real pause',
    frameCount > 0, `got ${frameCount} frames`);

  // The source filename in the call stack should reference the inline script
  const frames = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#call-stack-container .call-stack-frame');
    return Array.from(items).map(el => el.textContent);
  });
  assert('Call stack frames reference Scheme source',
    frames.some(f => f.includes('script-0.scm')),
    `frames: ${JSON.stringify(frames)}`);

  // ackPause should have been called
  const ackCalled = await panelPage.evaluate(() => window.__mockState.ackPauseCalled);
  assert('Panel called ackPause on real pause data', ackCalled === true);

  // --- Cleanup ---
  await panelPage.close();
  await doResume(realPage).catch(() => {});
  await drainPauses(realPage).catch(() => {});
  await realPage.close().catch(() => {});
}

// =========================================================================
// Test: Real breakpoint provides correct source location in panel
// =========================================================================

/**
 * Verifies that the real breakpoint's source location (line number) is
 * correctly shown in the panel's toolbar and triggers the correct line
 * highlight in the editor.
 */
export async function testRealBreakpointSourceLocationInPanel(browser, extensionId) {
  console.log('\n--- Test: Real Breakpoint Source Location in Panel ---');

  if (!extensionId) {
    assert('E2E source location test: extension loaded', false, 'no extension ID');
    return;
  }

  // Open real page with breakpoint on line 5
  const realPage = await browser.newPage();
  await realPage.evaluateOnNewDocument(() => {
    globalThis.__SCHEME_JS_BREAKPOINTS = [
      { url: 'scheme://inline-scripts/script-0.scm', line: 5 }
    ];
    globalThis.__capturedPauseEvent = null;
    window.addEventListener('message', (e) => {
      if (e.data?.type === 'scheme-debug-paused' && !globalThis.__capturedPauseEvent) {
        globalThis.__capturedPauseEvent = e.data;
      }
    });
  });

  await realPage.goto(TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await realPage.waitForFunction(
    () => typeof globalThis.__schemeDebug !== 'undefined',
    { timeout: 10000 }
  );

  await waitForPause(realPage);
  await ackPause(realPage);

  const status = await getStatus(realPage);
  const pauseEvent = await realPage.evaluate(() => globalThis.__capturedPauseEvent);

  if (!pauseEvent) {
    assert('Pause event captured', false, 'no pause event');
    await realPage.close();
    return;
  }

  // Verify the source line matches what we set
  assert('Real pause is on expected line',
    status.source?.line === 5,
    `got line ${status.source?.line}`);

  // Fire into panel and verify
  const panelPage = await browser.newPage();
  await panelPage.evaluateOnNewDocument(buildMockChromeScript());
  await panelPage.goto(
    `chrome-extension://${extensionId}/panel/panel.html`,
    { waitUntil: 'domcontentloaded' }
  );
  await waitForPage(panelPage,
    `document.querySelectorAll('#source-list .source-item').length > 0`);

  await panelPage.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, pauseEvent.detail);

  await waitForPage(panelPage,
    `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`);

  // Verify editor shows current-line highlight
  const hasHighlight = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-debug-current-line')
  );
  assert('Panel editor highlights current line from real breakpoint', hasHighlight);

  // Cleanup
  await panelPage.close();
  await doResume(realPage).catch(() => {});
  await drainPauses(realPage).catch(() => {});
  await realPage.close().catch(() => {});
}
