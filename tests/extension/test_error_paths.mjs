/**
 * @fileoverview Error path tests for the DevTools panel.
 *
 * Tests failure scenarios that the bridge layer and panel must handle
 * gracefully: eval timeouts, missing sources, broken CDP connections,
 * and malformed data from the page.
 */

import { assert, INLINE_URL, waitFor, waitForPage } from './test_harness.mjs';
import { buildMockChromeScript } from './test_mock_chrome.mjs';

// =========================================================================
// Helper: open panel with custom mock overrides
// =========================================================================

/**
 * Opens the panel with a custom mock chrome script.
 * @param {import('puppeteer').Browser} browser
 * @param {string} extensionId
 * @param {string} mockScript - The mock chrome script to inject
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openPanelWithMock(browser, extensionId, mockScript) {
  const panelPage = await browser.newPage();
  await panelPage.evaluateOnNewDocument(mockScript);
  await panelPage.goto(
    `chrome-extension://${extensionId}/panel/panel.html`,
    { waitUntil: 'domcontentloaded' }
  );
  return panelPage;
}

// =========================================================================
// Test: Panel handles eval timeout gracefully
// =========================================================================

export async function testEvalTimeout(browser, extensionId) {
  console.log('\n--- Test Group: Eval Timeout Handling ---');

  if (!extensionId) {
    assert('Eval timeout: extension loaded', false, 'no extension ID');
    return;
  }

  // Build a mock where getSources() hangs (never calls callback)
  const script = buildMockChromeScript({
    extraScript: `
      // Override eval to never respond for getSources
      const _origEval = window.chrome.devtools.inspectedWindow.eval;
      window.chrome.devtools.inspectedWindow.eval = function(expression, callback) {
        if (expression.includes('getSources()')) {
          // Never call callback — simulates a hung page
          return;
        }
        _origEval.call(this, expression, callback);
      };
    `,
  });

  const panelPage = await openPanelWithMock(browser, extensionId, script);

  // Wait for the panel to finish its initialization attempt
  // The source list should show an empty/error state, not crash
  await waitForPage(panelPage, `!!document.querySelector('.toolbar-status')`, 5000);

  const crashed = await panelPage.evaluate(() => {
    // Check that the panel didn't throw an unhandled error
    return !document.body.textContent.includes('Unhandled');
  });
  assert('Panel does not crash on eval timeout', crashed);

  // Source list should show empty or placeholder
  const sourceCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#source-list .source-item').length
  );
  assert('Source list is empty when eval hangs', sourceCount === 0);

  await panelPage.close();
}

// =========================================================================
// Test: Panel handles eval errors gracefully
// =========================================================================

export async function testEvalError(browser, extensionId) {
  console.log('\n--- Test Group: Eval Error Handling ---');

  if (!extensionId) {
    assert('Eval error: extension loaded', false, 'no extension ID');
    return;
  }

  // Build a mock where getSources() returns an error
  const script = buildMockChromeScript({
    extraScript: `
      const _origEval = window.chrome.devtools.inspectedWindow.eval;
      window.chrome.devtools.inspectedWindow.eval = function(expression, callback) {
        if (expression.includes('getSources()')) {
          setTimeout(() => callback(undefined, { value: 'Page crashed' }), 10);
          return;
        }
        _origEval.call(this, expression, callback);
      };
    `,
  });

  const panelPage = await openPanelWithMock(browser, extensionId, script);

  // Wait for the panel to finish initialization
  await waitForPage(panelPage, `!!document.querySelector('.toolbar-status')`, 5000);

  const crashed = await panelPage.evaluate(() =>
    !document.body.textContent.includes('Unhandled')
  );
  assert('Panel does not crash on eval error', crashed);

  // Source list should be empty
  const sourceCount = await panelPage.evaluate(() =>
    document.querySelectorAll('#source-list .source-item').length
  );
  assert('Source list is empty when eval errors', sourceCount === 0);

  await panelPage.close();
}

// =========================================================================
// Test: Panel handles missing __schemeDebug gracefully
// =========================================================================

export async function testMissingSchemeDebug(browser, extensionId) {
  console.log('\n--- Test Group: Missing __schemeDebug Handling ---');

  if (!extensionId) {
    assert('Missing debug: extension loaded', false, 'no extension ID');
    return;
  }

  // Build a mock where typeof __schemeDebug returns false
  const script = buildMockChromeScript({
    extraScript: `
      const _origEval = window.chrome.devtools.inspectedWindow.eval;
      window.chrome.devtools.inspectedWindow.eval = function(expression, callback) {
        if (expression.includes('typeof __schemeDebug')) {
          setTimeout(() => callback(false, null), 10);
          return;
        }
        _origEval.call(this, expression, callback);
      };
    `,
  });

  const panelPage = await openPanelWithMock(browser, extensionId, script);

  // Wait for panel to show the "not detected" message (refresh completes asynchronously)
  await waitForPage(panelPage, `document.querySelector('#source-list')?.textContent?.includes('not detected')`, 5000);

  const statusText = await panelPage.evaluate(() =>
    document.querySelector('#source-list')?.textContent || ''
  );
  assert('Panel shows "not detected" when __schemeDebug is missing',
    statusText.includes('not detected'),
    `got: "${statusText}"`);

  await panelPage.close();
}

// =========================================================================
// Test: Panel handles malformed pause event
// =========================================================================

export async function testMalformedPauseEvent(browser, extensionId) {
  console.log('\n--- Test Group: Malformed Pause Event Handling ---');

  if (!extensionId) {
    assert('Malformed pause: extension loaded', false, 'no extension ID');
    return;
  }

  const script = buildMockChromeScript();
  const panelPage = await openPanelWithMock(browser, extensionId, script);
  await waitForPage(panelPage, `document.querySelectorAll('#source-list .source-item').length > 0`);

  // Send a pause event with missing fields
  await panelPage.evaluate(() => {
    window.__fireMessage({
      type: 'scheme-debug-paused',
      detail: {
        reason: 'breakpoint',
        // Missing source and stack
      }
    });
  });

  // Panel should not crash
  await new Promise(r => setTimeout(r, 200));
  const crashed = await panelPage.evaluate(() =>
    !document.body.textContent.includes('Unhandled') &&
    !document.body.textContent.includes('Cannot read')
  );
  assert('Panel does not crash on malformed pause (missing source/stack)', crashed);

  // Send a pause with null detail
  await panelPage.evaluate(() => {
    window.__fireMessage({
      type: 'scheme-debug-paused',
      detail: null
    });
  });

  await new Promise(r => setTimeout(r, 200));
  const crashed2 = await panelPage.evaluate(() =>
    !document.body.textContent.includes('Unhandled')
  );
  assert('Panel does not crash on null pause detail', crashed2);

  await panelPage.close();
}

// =========================================================================
// Test: Panel handles getLocals failure gracefully
// =========================================================================

export async function testGetLocalsFailure(browser, extensionId) {
  console.log('\n--- Test Group: getLocals Failure Handling ---');

  if (!extensionId) {
    assert('Locals failure: extension loaded', false, 'no extension ID');
    return;
  }

  // Build a mock where getLocals throws
  const script = buildMockChromeScript({
    extraScript: `
      const _origEval = window.chrome.devtools.inspectedWindow.eval;
      window.chrome.devtools.inspectedWindow.eval = function(expression, callback) {
        if (expression.includes('getLocals(')) {
          setTimeout(() => callback(undefined, { value: 'getLocals exploded' }), 10);
          return;
        }
        _origEval.call(this, expression, callback);
      };
    `,
  });

  const panelPage = await openPanelWithMock(browser, extensionId, script);
  await waitForPage(panelPage, `document.querySelectorAll('#source-list .source-item').length > 0`);

  // Fire a pause event
  await panelPage.evaluate((url) => {
    window.__fireMessage({
      type: 'scheme-debug-paused',
      detail: {
        reason: 'breakpoint',
        source: { filename: url, line: 1, column: 0 },
        stack: [{ name: 'test-fn', source: { filename: url, line: 1, column: 0 }, tcoCount: 0 }],
      }
    });
  }, INLINE_URL);

  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`);

  // Variables should show empty or error state, not crash
  const varsState = await panelPage.evaluate(() => {
    const empty = document.querySelector('#variables-container .variables-empty');
    const rows = document.querySelectorAll('#variables-container .variable-row');
    return {
      hasEmptyMsg: !!empty,
      rowCount: rows.length,
      bodyOk: !document.body.textContent.includes('Unhandled'),
    };
  });

  assert('Panel shows empty variables on getLocals failure',
    varsState.hasEmptyMsg || varsState.rowCount === 0,
    `empty=${varsState.hasEmptyMsg}, rows=${varsState.rowCount}`);
  assert('Panel does not crash on getLocals failure', varsState.bodyOk);

  await panelPage.close();
}

// =========================================================================
// Test: CDP sendMessage failure does not crash panel
// =========================================================================

export async function testCDPSendMessageFailure(browser, extensionId) {
  console.log('\n--- Test Group: CDP sendMessage Failure ---');

  if (!extensionId) {
    assert('CDP failure: extension loaded', false, 'no extension ID');
    return;
  }

  // Build a CDP mock where sendMessage returns errors
  const script = buildMockChromeScript({
    cdp: true,
    extraScript: `
      window.chrome.runtime.sendMessage = function(message, callback) {
        if (callback) setTimeout(() => callback({ success: false, error: 'Connection refused' }), 10);
      };
    `,
  });

  const panelPage = await openPanelWithMock(browser, extensionId, script);
  await waitForPage(panelPage, `document.querySelectorAll('#source-list .source-item').length > 0`);

  // Try to trigger a CDP pause - this should not crash
  await panelPage.evaluate(() => {
    window.__fireMessage({
      type: 'cdp-paused',
      callFrames: [{
        callFrameId: 'cf-1',
        functionName: 'testFn',
        url: 'http://localhost/test.js',
        location: { scriptId: '1', lineNumber: 0, columnNumber: 0 },
        scopeChain: [],
      }],
      reason: 'other',
    });
  });

  await new Promise(r => setTimeout(r, 300));

  const crashed = await panelPage.evaluate(() =>
    !document.body.textContent.includes('Unhandled')
  );
  assert('Panel does not crash when CDP sendMessage fails', crashed);

  await panelPage.close();
}

// =========================================================================
// Test: getSourceContent returns null for unknown source
// =========================================================================

export async function testUnknownSourceContent(browser, extensionId) {
  console.log('\n--- Test Group: Unknown Source Content ---');

  if (!extensionId) {
    assert('Unknown source: extension loaded', false, 'no extension ID');
    return;
  }

  const script = buildMockChromeScript();
  const panelPage = await openPanelWithMock(browser, extensionId, script);
  await waitForPage(panelPage, `document.querySelectorAll('#source-list .source-item').length > 0`);

  // Fire a pause pointing to a source not in the sources list
  await panelPage.evaluate(() => {
    window.__fireMessage({
      type: 'scheme-debug-paused',
      detail: {
        reason: 'breakpoint',
        source: { filename: 'scheme://unknown/nonexistent.scm', line: 1, column: 0 },
        stack: [{ name: 'ghost', source: { filename: 'scheme://unknown/nonexistent.scm', line: 1, column: 0 }, tcoCount: 0 }],
      }
    });
  });

  await waitForPage(panelPage, `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`);

  // Panel should show the pause but not crash
  const statusText = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent || ''
  );
  assert('Panel shows paused even for unknown source', statusText.includes('Paused'));

  // Panel should not have crashed
  const bodyOk = await panelPage.evaluate(() =>
    !document.body.textContent.includes('Unhandled')
  );
  assert('Panel does not crash on unknown source', bodyOk);

  await panelPage.close();
}
