/**
 * @fileoverview Tests for the standalone debugger window architecture.
 *
 * These tests verify that the debugger panel works when hosted in a
 * standalone browser window (chrome.windows.create) instead of a
 * Chrome DevTools panel. The key differences:
 *
 * 1. Uses chrome.scripting.executeScript instead of inspectedWindow.eval
 * 2. Gets tabId from URL parameter instead of chrome.devtools.inspectedWindow.tabId
 * 3. Detects navigation via chrome.tabs.onUpdated instead of devtools.network.onNavigated
 * 4. No chrome.devtools property exists at all
 *
 * These tests should FAIL with the current DevTools-panel-based code
 * and PASS after the standalone window migration.
 */

import { assert, INLINE_URL, waitFor, waitForPage } from './test_harness.mjs';
import { buildStandaloneWindowMockScript } from './test_mock_chrome.mjs';

// =========================================================================
// Helpers
// =========================================================================

/**
 * Opens the panel page with standalone window mocks (no chrome.devtools).
 * Passes tabId=42 as a URL parameter, matching the mock setup.
 *
 * @param {import('puppeteer').Browser} browser
 * @param {string} extensionId
 * @param {Object} [mockOptions] - Options for buildStandaloneWindowMockScript
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openStandalonePanel(browser, extensionId, mockOptions = {}) {
  const page = await browser.newPage();
  const script = buildStandaloneWindowMockScript({ cdp: true, ...mockOptions });
  await page.evaluateOnNewDocument(script);
  await page.goto(
    `chrome-extension://${extensionId}/panel/panel.html?tabId=42`,
    { waitUntil: 'domcontentloaded' }
  );
  return page;
}

/**
 * Waits for the panel to finish initializing (source list populated).
 * @param {import('puppeteer').Page} page
 * @returns {Promise<boolean>}
 */
async function waitForPanelReady(page) {
  return waitForPage(
    page,
    `document.querySelectorAll('#source-list .source-item').length > 0`,
    5000
  );
}

/**
 * Sends a pause event to the panel via captured message listeners.
 * @param {import('puppeteer').Page} page
 * @param {Object} detail - Pause detail (reason, source, stack)
 */
async function firePauseEvent(page, detail) {
  await page.evaluate((d) => {
    window.__fireMessage({ type: 'scheme-debug-paused', detail: d });
  }, detail);
}

/**
 * Sends a sync-path pause event to the panel.
 * @param {import('puppeteer').Page} page
 * @param {Object} message - scheme-sync-paused message from background.js
 */
async function fireSyncPauseEvent(page, message) {
  await page.evaluate((msg) => {
    window.__fireMessage(msg);
  }, message);
}

// =========================================================================
// Test: Panel initializes and loads sources without chrome.devtools
// =========================================================================

/**
 * Verifies that the panel can initialize, read tabId from URL params,
 * and load sources via chrome.scripting.executeScript (no inspectedWindow).
 */
export async function testStandaloneWindowSourcesLoad(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Sources Load ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel initializes without chrome.devtools', ready,
    'Source list did not populate — panel likely failed to call chrome.scripting.executeScript');

  if (ready) {
    const sourceCount = await page.evaluate(
      () => document.querySelectorAll('#source-list .source-item').length
    );
    assert('Source list has 2 items', sourceCount === 2,
      `got ${sourceCount}`);

    // Verify chrome.devtools is NOT present (confirming we're testing the right path)
    const hasDevtools = await page.evaluate(() => !!window.chrome?.devtools);
    assert('chrome.devtools is absent', hasDevtools === false,
      `chrome.devtools exists: ${hasDevtools}`);

    // Verify chrome.scripting is present
    const hasScripting = await page.evaluate(() => !!window.chrome?.scripting);
    assert('chrome.scripting is present', hasScripting === true);
  }

  await page.close();
}

// =========================================================================
// Test: Navigation detected via chrome.tabs.onUpdated
// =========================================================================

/**
 * Verifies that the panel re-activates when a tabs.onUpdated event fires,
 * replacing the chrome.devtools.network.onNavigated listener.
 */
export async function testStandaloneWindowNavigationDetection(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Navigation Detection ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel ready for navigation test', ready);

  if (ready) {
    // Change the mock sources to simulate a page that loaded different content
    await page.evaluate(() => {
      window.__mockState.sources = [
        { url: 'scheme://inline-scripts/script-0.scm',
          content: '(define x 99)',
          lines: 1,
          origin: 'inline' }
      ];
    });

    // Fire tabs.onUpdated with status=complete for tabId 42
    await page.evaluate(() => {
      window.__fireTabsUpdated(42, { status: 'complete' });
    });

    // Wait for sources to refresh (the panel should re-query after navigation)
    const refreshed = await waitForPage(
      page,
      `document.querySelectorAll('#source-list .source-item').length === 1`,
      3000
    );

    assert('Sources refreshed after navigation event', refreshed,
      'Source list did not update — chrome.tabs.onUpdated handler may not be wired up');
  }

  await page.close();
}

// =========================================================================
// Test: Cooperative pause works in standalone window
// =========================================================================

/**
 * Verifies that a cooperative (async-path) pause event correctly updates
 * the panel UI when running in a standalone window.
 */
export async function testStandaloneWindowCooperativePause(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Cooperative Pause ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel ready for pause test', ready);

  if (ready) {
    // Set up mock state for a Scheme pause
    await page.evaluate(() => {
      window.__mockState.stack = [
        { name: 'factorial', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 3 }, tcoCount: 0 },
        { name: 'main', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 9, column: 1 }, tcoCount: 0 },
      ];
      window.__mockState.locals = [
        [],
        [{ name: 'n', value: '5', type: 'number' }],
      ];
    });

    // Fire cooperative pause
    await firePauseEvent(page, {
      reason: 'breakpoint',
      source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 3 },
      stack: [
        { name: 'factorial', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 4, column: 3 }, tcoCount: 0 },
        { name: 'main', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 9, column: 1 }, tcoCount: 0 },
      ],
    });

    // Verify paused UI
    const paused = await waitForPage(
      page,
      `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
      3000
    );
    assert('Toolbar shows paused state', paused);

    // Verify call stack
    const frameCount = await page.evaluate(
      () => document.querySelectorAll('#call-stack-container .call-stack-frame').length
    );
    assert('Call stack shows 2 frames', frameCount === 2,
      `got ${frameCount}`);

    // Verify variables loaded
    const hasVars = await waitForPage(
      page,
      `document.querySelectorAll('#variables-container .variable-row').length > 0`,
      2000
    );
    assert('Variables pane shows locals', hasVars);
  }

  await page.close();
}

// =========================================================================
// Test: Sync-path pause (scheme-sync-paused) shows in standalone panel
// =========================================================================

/**
 * Verifies that a sync-path breakpoint pause (scheme-sync-paused message)
 * correctly updates the standalone panel with stack, source, and variables.
 * This is the critical test — sync breakpoints caused Sources tab switching
 * in the DevTools panel, but should work seamlessly in the standalone window.
 */
export async function testStandaloneWindowSyncPause(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Sync-Path Pause ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel ready for sync pause test', ready);

  if (ready) {
    // Set up mock state — locals will be fetched via CDP evalWhilePaused
    await page.evaluate(() => {
      window.__mockState.locals = [
        [{ name: 'click-count', value: '3', type: 'number' },
         { name: 'event', value: '#<js-object>', type: 'object' }],
      ];
    });

    // Fire a scheme-sync-paused message (as background.js would send)
    await fireSyncPauseEvent(page, {
      type: 'scheme-sync-paused',
      tabId: 42,
      stack: [
        { name: 'handle-click', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 3, column: 1 }, tcoCount: 0 },
      ],
      callFrames: [],
      reason: 'breakpoint',
    });

    // Verify the panel shows paused state
    const paused = await waitForPage(
      page,
      `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
      3000
    );
    assert('Sync-path pause shows in standalone panel', paused,
      'Panel did not show paused state for scheme-sync-paused message');

    // Verify call stack has the Scheme frame
    const frameCount = await page.evaluate(
      () => document.querySelectorAll('#call-stack-container .call-stack-frame').length
    );
    assert('Sync pause shows call stack', frameCount >= 1,
      `got ${frameCount} frames`);

    // Verify variables are displayed
    const hasVars = await waitForPage(
      page,
      `document.querySelectorAll('#variables-container .variable-row').length > 0`,
      2000
    );
    assert('Sync pause shows variables', hasVars,
      'Variables not loaded — evalWhilePaused CDP routing may not be working');
  }

  await page.close();
}

// =========================================================================
// Test: Theme detection via prefers-color-scheme (no DevTools panels API)
// =========================================================================

/**
 * Verifies that the panel detects theme without chrome.devtools.panels.themeName.
 */
export async function testStandaloneWindowThemeDetection(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Theme Detection ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel ready for theme test', ready);

  if (ready) {
    // The panel should have applied a theme class even without chrome.devtools.panels
    const hasTheme = await page.evaluate(() => {
      const root = document.documentElement;
      return root.classList.contains('theme-dark') || root.classList.contains('theme-light');
    });
    assert('Panel applied theme class without DevTools API', hasTheme,
      'No theme-dark or theme-light class found — theme detection may require chrome.devtools.panels');
  }

  await page.close();
}

// =========================================================================
// Test: Resume button works during sync-path pause
// =========================================================================

/**
 * Verifies that clicking Resume during a sync-path pause sends the correct
 * CDP command (resume-debugger) instead of calling inspectedWindow.eval.
 */
export async function testStandaloneWindowSyncResumeClick(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Sync Resume Click ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel ready for sync resume test', ready);

  if (ready) {
    // Fire a sync pause
    await fireSyncPauseEvent(page, {
      type: 'scheme-sync-paused',
      tabId: 42,
      stack: [
        { name: 'on-click', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 2, column: 1 }, tcoCount: 0 },
      ],
      callFrames: [],
      reason: 'breakpoint',
    });

    const paused = await waitForPage(
      page,
      `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
      3000
    );
    assert('Panel paused before resume test', paused);

    if (paused) {
      // Click the Resume button
      await page.evaluate(() => {
        const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
        const resumeBtn = btns.find(b => b.title.includes('Resume'));
        if (resumeBtn) resumeBtn.click();
      });

      // Verify a CDP resume was sent (not a scheme-bridge resume)
      const cdpResumed = await waitFor(async () => {
        return page.evaluate(() => window.__mockState.cdpResumeCalled === true);
      }, 2000);

      assert('Resume during sync pause sends CDP resume',
        cdpResumed,
        'Expected cdpResumeCalled=true — resume may be routing through inspectedWindow.eval instead of CDP');
    }
  }

  await page.close();
}

// =========================================================================
// Test: Sync-path pause uses Scheme source, not JS frame (Bug B regression)
// =========================================================================

/**
 * Verifies that handleSyncSchemePause uses the Scheme top frame's source
 * (not unified[0] which may be an internal JS frame). Before the fix,
 * the code panel would show the wrong file during sync-path pauses when
 * JS CDP frames were present in the merged stack.
 */
export async function testSyncPauseUsesSchemeSource(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Sync Pause Uses Scheme Source ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel ready for scheme source test', ready);

  if (ready) {
    // Fire a sync pause with BOTH CDP JS frames and Scheme frames.
    // The JS frames come from callFrames (CDP), Scheme frames from stack.
    // mergeCallStacks puts JS frames first, so unified[0] would be the JS frame.
    await fireSyncPauseEvent(page, {
      type: 'scheme-sync-paused',
      tabId: 42,
      stack: [
        // Bottom-to-top: first frame is bottom, last is top
        { name: 'handle-click', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 3, column: 1 }, tcoCount: 0 },
      ],
      callFrames: [
        // CDP JS frames — these would appear in the merged stack
        {
          callFrameId: '{"ordinal":0}',
          functionName: 'hit',
          url: 'chrome-extension://abc/dist/scheme-html.js',
          location: { scriptId: '99', lineNumber: 500, columnNumber: 0 },
          scopeChain: [],
        },
      ],
      reason: 'breakpoint',
    });

    // Wait for the panel to show paused state
    const paused = await waitForPage(
      page,
      `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
      3000
    );
    assert('Sync pause with mixed frames shows paused', paused);

    if (paused) {
      // Check which source file the editor loaded. If Bug B is present,
      // it would load the JS frame's URL instead of the Scheme source.
      const editorSource = await page.evaluate(() => {
        // The source list should have the Scheme file selected/highlighted
        const selectedItem = document.querySelector('#source-list .source-item.selected');
        return selectedItem?.textContent || '';
      });

      // The editor should show the Scheme source, not the JS bundle
      const sourceContent = await page.evaluate(() => {
        // Check the editor content — should be Scheme code, not JS
        const cm = document.querySelector('.cm-content');
        return cm?.textContent?.substring(0, 50) || '';
      });

      // Verify it's not showing the JS bundle URL
      const showsScheme = !editorSource.includes('scheme-html.js');
      assert('Editor shows Scheme source (not JS bundle)',
        showsScheme,
        `selected source: "${editorSource}"`);
    }
  }

  await page.close();
}

// =========================================================================
// Test: Frame selection uses Scheme index for getLocals (Bug C regression)
// =========================================================================

/**
 * Verifies that clicking a Scheme frame in a mixed (JS + Scheme) stack
 * passes the correct Scheme frame index to getLocals, not the raw unified
 * index. Before the fix, the unified index was passed which exceeded the
 * Scheme stack length, causing getLocals() to return [].
 */
export async function testFrameSelectUsesSchemeIndex(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Frame Select Uses Scheme Index ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel ready for frame select test', ready);

  if (ready) {
    // Set up mock locals — index 0 should return variables
    await page.evaluate(() => {
      window.__mockState.locals = {
        0: [{ name: 'click-count', value: '3', type: 'number' }],
      };
    });

    // Fire sync pause with one Scheme frame (index 0 in Scheme stack)
    // and one JS CDP frame. In the unified stack, the Scheme frame will
    // be at a higher index than 0 due to JS frames below it.
    await fireSyncPauseEvent(page, {
      type: 'scheme-sync-paused',
      tabId: 42,
      stack: [
        { name: 'handle-click', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 3, column: 1 }, tcoCount: 0 },
      ],
      callFrames: [],
      reason: 'breakpoint',
    });

    const paused = await waitForPage(
      page,
      `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
      3000
    );
    assert('Panel paused for frame select test', paused);

    if (paused) {
      // Wait for initial variables to load
      const hasVars = await waitForPage(
        page,
        `document.querySelectorAll('#variables-container .variable-row').length > 0`,
        2000
      );
      assert('Initial pause shows variables', hasVars,
        'Variables not loaded on initial pause');

      if (hasVars) {
        // Now click the top frame in the call stack to simulate frame selection.
        // This triggers onSelectFrame. With the bug, it would pass the unified
        // index instead of the Scheme index, causing getLocals to fail.
        const frameClicked = await page.evaluate(() => {
          const frames = document.querySelectorAll('#call-stack-container .call-stack-frame');
          if (frames.length > 0) {
            // Click the last frame (top of stack)
            frames[frames.length - 1].click();
            return true;
          }
          return false;
        });
        assert('Clicked top stack frame', frameClicked);

        if (frameClicked) {
          // Wait a moment for the frame selection handler to complete
          await new Promise(r => setTimeout(r, 500));

          // Variables should still be visible after clicking
          const stillHasVars = await page.evaluate(
            () => document.querySelectorAll('#variables-container .variable-row').length > 0
          );
          assert('Variables still visible after frame click (correct Scheme index used)',
            stillHasVars,
            'Variables disappeared — onSelectFrame likely passed unified index instead of Scheme index');
        }
      }
    }
  }

  await page.close();
}

// =========================================================================
// Test: Resume does not show "Refreshing..." (Bug F regression)
// =========================================================================

/**
 * Verifies that clicking Resume during a sync-path pause transitions
 * cleanly to "Running" without showing "Refreshing..." in the toolbar.
 * Before the fix, onResumed() called refresh() which set the toolbar
 * status to "Refreshing..." and cleared console output.
 */
export async function testSyncResumeNoRefreshing(browser, extensionId) {
  console.log('\n--- Test: Standalone Window — Resume No Refreshing ---');

  const page = await openStandalonePanel(browser, extensionId);
  const ready = await waitForPanelReady(page);

  assert('Panel ready for no-refreshing test', ready);

  if (ready) {
    // Fire a sync pause
    await fireSyncPauseEvent(page, {
      type: 'scheme-sync-paused',
      tabId: 42,
      stack: [
        { name: 'handler', source: { filename: 'scheme://inline-scripts/script-0.scm', line: 3, column: 1 }, tcoCount: 0 },
      ],
      callFrames: [],
      reason: 'breakpoint',
    });

    const paused = await waitForPage(
      page,
      `document.querySelector('.toolbar-status')?.textContent?.includes('Paused')`,
      3000
    );
    assert('Panel paused for no-refreshing test', paused);

    if (paused) {
      // Install an observer to capture all toolbar status text changes
      await page.evaluate(() => {
        window.__statusHistory = [];
        const statusEl = document.querySelector('.toolbar-status');
        if (statusEl) {
          const observer = new MutationObserver(() => {
            window.__statusHistory.push(statusEl.textContent);
          });
          observer.observe(statusEl, { childList: true, characterData: true, subtree: true });
        }
      });

      // Click the Resume button
      await page.evaluate(() => {
        const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
        const resumeBtn = btns.find(b => b.title.includes('Resume'));
        if (resumeBtn) resumeBtn.click();
      });

      // Wait for status to change from Paused
      await new Promise(r => setTimeout(r, 500));

      // Check that "Refreshing..." never appeared in the status history
      const history = await page.evaluate(() => window.__statusHistory || []);
      const sawRefreshing = history.some(s => s.includes('Refreshing'));
      assert('Toolbar never showed "Refreshing..." during resume',
        !sawRefreshing,
        `status history: ${JSON.stringify(history)}`);
    }
  }

  await page.close();
}
