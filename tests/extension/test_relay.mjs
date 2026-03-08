/**
 * @fileoverview Tests for cross-world postMessage relay, pause persistence
 * (ackPause), and the self-contained breakpoint flow test page.
 */

import { assert, BASE_URL } from './test_harness.mjs';

// --- PostMessage Relay (cross-world) ---

export async function testPostMessageRelay(browser) {
  console.log('\n--- Test Group: PostMessage Relay ---');

  const page = await browser.newPage();
  await page.goto(`${BASE_URL}/tests/debug/postmessage_relay_test.html`, {
    waitUntil: 'domcontentloaded'
  });

  // Wait for test page to finish
  await page.waitForFunction(
    () => {
      const output = document.getElementById('output');
      return output && !output.textContent.includes('Running...');
    },
    { timeout: 10000 }
  );

  const postMessageWorked = await page.evaluate(() => window._postMessageDetail !== null);
  assert('postMessage detail received by page listener', postMessageWorked);

  const customEventWorked = await page.evaluate(() => window._customEventDetail !== null);
  assert('CustomEvent detail received by page listener', customEventWorked);

  const detailCheck = await page.evaluate(() => {
    const pm = window._postMessageDetail;
    const ce = window._customEventDetail;
    return {
      pmHasSource: pm && pm.source && typeof pm.source.line === 'number',
      ceHasSource: ce && ce.source && typeof ce.source.line === 'number',
      pmLine: pm?.source?.line,
      ceLine: ce?.source?.line,
    };
  });

  assert('postMessage detail has source line', detailCheck.pmHasSource === true);
  assert('CustomEvent detail has source line', detailCheck.ceHasSource === true);
  assert('Both channels report same line', detailCheck.pmLine === detailCheck.ceLine,
    `PM=${detailCheck.pmLine} CE=${detailCheck.ceLine}`);

  await page.close();
}

// --- Pause Persistence (ackPause) ---

export async function testPausePersistence(browser) {
  console.log('\n--- Test Group: Pause Persistence ---');

  const page = await browser.newPage();
  await page.goto(`${BASE_URL}/tests/debug/pause_persistence_test.html`, {
    waitUntil: 'domcontentloaded'
  });

  // Wait for PAUSED status
  await page.waitForFunction(
    () => document.getElementById('status')?.textContent === 'PAUSED',
    { timeout: 10000 }
  );
  assert('Page enters PAUSED state', true);

  // Wait 7 seconds — default resumeTimeout is 5s, ackPause should prevent it
  await new Promise(r => setTimeout(r, 7000));

  const statusAfterWait = await page.evaluate(() => ({
    status: document.getElementById('status')?.textContent,
    isPaused: globalThis.__schemeDebug?.getStatus()?.state === 'paused',
    resumeTime: window._resumeTime,
  }));

  assert('Still paused after 7s (ackPause prevents timeout)',
    statusAfterWait.status === 'PAUSED',
    `status=${statusAfterWait.status}`);
  assert('Interpreter in paused state', statusAfterWait.isPaused === true);
  assert('No resume event fired', statusAfterWait.resumeTime === null,
    `resumeTime=${statusAfterWait.resumeTime}`);

  // Explicit resume
  await page.evaluate(() => globalThis.__schemeDebug?.resume());
  await new Promise(r => setTimeout(r, 1000));

  const afterResume = await page.evaluate(() => ({
    status: document.getElementById('status')?.textContent,
    resumeTime: window._resumeTime,
  }));

  assert('Explicit resume works', afterResume.resumeTime !== null);
  assert('Page shows RESUMED after explicit resume',
    afterResume.status === 'RESUMED',
    `status=${afterResume.status}`);

  await page.close();
}

// --- Breakpoint Flow Test Page ---

export async function testBreakpointFlowPage(browser) {
  console.log('\n--- Test Group: Breakpoint Flow Test Page ---');

  const page = await browser.newPage();
  const startTime = Date.now();
  await page.goto(`${BASE_URL}/tests/debug/breakpoint_flow_test.html`, {
    waitUntil: 'domcontentloaded'
  });

  // Wait for the self-contained test page to show results
  await page.waitForFunction(
    () => {
      const output = document.getElementById('output');
      return output && output.textContent.includes('PASS');
    },
    { timeout: 15000 }
  );

  const elapsed = Date.now() - startTime;
  assert('Breakpoint flow test page loads in <10s', elapsed < 10000, `took ${elapsed}ms`);

  const results = await page.evaluate(() => document.getElementById('output')?.textContent || '');
  const passCount = (results.match(/PASS/g) || []).length;
  const failCount = (results.match(/FAIL/g) || []).length;
  assert('Breakpoint flow page: all tests pass',
    failCount === 0 && passCount > 0,
    `${passCount} passed, ${failCount} failed`);

  await page.close();
}
