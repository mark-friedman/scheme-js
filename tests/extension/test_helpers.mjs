/**
 * @fileoverview Page interaction helpers for Puppeteer E2E tests.
 *
 * Provides wrappers around __schemeDebug API calls and page navigation
 * helpers used by all test modules.
 */

import { TEST_PAGE, waitForPage } from './test_harness.mjs';

/**
 * Waits for __schemeDebug API to be available on the page.
 * @param {import('puppeteer').Page} page
 * @param {number} [timeout=10000]
 */
export async function waitForDebugAPI(page, timeout = 10000) {
  await page.waitForFunction(
    () => typeof globalThis.__schemeDebug !== 'undefined',
    { timeout }
  );
}

/**
 * Waits for the page to enter paused state.
 * @param {import('puppeteer').Page} page
 * @param {number} [timeout=10000]
 */
export async function waitForPause(page, timeout = 10000) {
  await page.waitForFunction(
    () => globalThis.__schemeDebug?.getStatus()?.state === 'paused',
    { timeout }
  );
}

/**
 * Waits for the page to exit paused state (running or idle).
 * @param {import('puppeteer').Page} page
 * @param {number} [timeout=10000]
 */
export async function waitForRunning(page, timeout = 10000) {
  await page.waitForFunction(
    () => globalThis.__schemeDebug?.getStatus()?.state !== 'paused',
    { timeout }
  );
}

/**
 * Sets a breakpoint on the page and returns the breakpoint ID.
 * @param {import('puppeteer').Page} page
 * @param {string} url
 * @param {number} line
 * @returns {Promise<string|null>}
 */
export async function setBP(page, url, line) {
  return page.evaluate(
    (u, l) => globalThis.__schemeDebug.setBreakpoint(u, l),
    url, line
  );
}

/**
 * Removes a breakpoint by ID.
 * @param {import('puppeteer').Page} page
 * @param {string} id
 */
export async function removeBP(page, id) {
  await page.evaluate(bpId => globalThis.__schemeDebug.removeBreakpoint(bpId), id);
}

/**
 * Gets the current debug status.
 * @param {import('puppeteer').Page} page
 */
export async function getStatus(page) {
  return page.evaluate(() => globalThis.__schemeDebug.getStatus());
}

/**
 * Gets the call stack.
 * @param {import('puppeteer').Page} page
 */
export async function getStack(page) {
  return page.evaluate(() => JSON.parse(JSON.stringify(globalThis.__schemeDebug.getStack())));
}

/**
 * Gets locals for a frame.
 * @param {import('puppeteer').Page} page
 * @param {number} frameIndex
 */
export async function getLocals(page, frameIndex) {
  return page.evaluate(
    idx => JSON.parse(JSON.stringify(globalThis.__schemeDebug.getLocals(idx))),
    frameIndex
  );
}

/**
 * Gets registered sources.
 * @param {import('puppeteer').Page} page
 */
export async function getSources(page) {
  return page.evaluate(() => JSON.parse(JSON.stringify(globalThis.__schemeDebug.getSources())));
}

/**
 * Acknowledges a pause (cancels safety timeout).
 * @param {import('puppeteer').Page} page
 */
export async function ackPause(page) {
  await page.evaluate(() => globalThis.__schemeDebug.ackPause());
}

/**
 * Resumes execution.
 * @param {import('puppeteer').Page} page
 */
export async function doResume(page) {
  await page.evaluate(() => globalThis.__schemeDebug.resume());
}

/**
 * Steps into.
 * @param {import('puppeteer').Page} page
 */
export async function doStepInto(page) {
  await page.evaluate(() => globalThis.__schemeDebug.stepInto());
}

/**
 * Steps over.
 * @param {import('puppeteer').Page} page
 */
export async function doStepOver(page) {
  await page.evaluate(() => globalThis.__schemeDebug.stepOver());
}

/**
 * Steps out.
 * @param {import('puppeteer').Page} page
 */
export async function doStepOut(page) {
  await page.evaluate(() => globalThis.__schemeDebug.stepOut());
}

/**
 * Evaluates a Scheme expression in the context of a paused frame.
 * @param {import('puppeteer').Page} page
 * @param {string} code
 * @param {number} [frameIndex]
 * @returns {Promise<string>}
 */
export async function debugEval(page, code, frameIndex) {
  return page.evaluate(
    (c, fi) => globalThis.__schemeDebug.eval(c, fi),
    code, frameIndex
  );
}

/**
 * Opens a fresh page with the test HTML, waits for debug API.
 * Does NOT wait for execution to complete (caller may set breakpoints first).
 * @param {import('puppeteer').Browser} browser
 * @returns {Promise<import('puppeteer').Page>}
 */
export async function openTestPage(browser) {
  const page = await browser.newPage();
  await page.goto(TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await waitForDebugAPI(page);
  return page;
}

/**
 * Opens the test page with pre-loaded breakpoints (set via page globals BEFORE
 * scheme-html.js processes the scripts). This is needed because by the time
 * Puppeteer can interact, the scripts may have already finished.
 *
 * @param {import('puppeteer').Browser} browser
 * @param {Array<{url: string, line: number}>} breakpoints
 * @returns {Promise<import('puppeteer').Page>}
 */
export async function openTestPageWithBreakpoints(browser, breakpoints) {
  const page = await browser.newPage();

  // Inject breakpoints BEFORE navigating so the non-module <script> picks them up
  await page.evaluateOnNewDocument((bps) => {
    globalThis.__SCHEME_JS_BREAKPOINTS = bps;
  }, breakpoints);

  await page.goto(TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await waitForDebugAPI(page);
  return page;
}

/**
 * Opens the panel page directly in a new tab.
 * This gives us full access to the panel DOM without needing to navigate
 * the DevTools shadow DOM.
 *
 * @param {import('puppeteer').Browser} browser
 * @param {string} extensionId
 * @returns {Promise<import('puppeteer').Page>}
 */
export async function openPanelPage(browser, extensionId) {
  const panelPage = await browser.newPage();
  await panelPage.goto(
    `chrome-extension://${extensionId}/panel/panel.html`,
    { waitUntil: 'domcontentloaded' }
  );
  await waitForPage(panelPage, `!!document.querySelector('#source-list') || !!document.querySelector('.toolbar-status')`);
  return panelPage;
}

/**
 * Drains all remaining pauses by resuming until the page is no longer paused.
 * @param {import('puppeteer').Page} page
 * @param {number} [maxDrains=50]
 */
export async function drainPauses(page, maxDrains = 50) {
  let drainCount = 0;
  while (true) {
    const s = await getStatus(page);
    if (s.state !== 'paused') break;
    await doResume(page);
    drainCount++;
    if (drainCount > maxDrains) break;
    await new Promise(r => setTimeout(r, 50));
  }
}
