/**
 * @fileoverview Tests for debug API activation, source registration, and execution timing.
 */

import { assert, TEST_PAGE } from './test_harness.mjs';
import { openTestPage, getSources, getStatus } from './test_helpers.mjs';

// --- Activation & Sources ---

export async function testActivationAndSources(browser) {
  console.log('\n--- Test Group: Activation & Sources ---');

  const page = await openTestPage(browser);

  // Wait for execution to complete (no breakpoints)
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });

  const hasAPI = await page.evaluate(() => typeof globalThis.__schemeDebug !== 'undefined');
  assert('__schemeDebug API is available', hasAPI);

  const status = await getStatus(page);
  assert('Debug runtime is active', status.active === true);

  const sources = await getSources(page);
  assert('At least 2 sources registered (inline + external)', sources.length >= 2,
    `got ${sources.length}: ${sources.map(s => s.url).join(', ')}`);

  const inlineSrc = sources.find(s => s.url.includes('inline'));
  const externalSrc = sources.find(s => s.url.includes('manual_script'));
  assert('Inline source is registered', !!inlineSrc, `urls: ${sources.map(s => s.url)}`);
  assert('External source is registered', !!externalSrc, `urls: ${sources.map(s => s.url)}`);

  if (inlineSrc) {
    assert('Inline source has content', inlineSrc.content?.length > 0);
  }
  if (externalSrc) {
    assert('External source has content with fib function',
      externalSrc.content?.includes('fib'), `content: ${externalSrc.content?.substring(0, 50)}`);
  }

  await page.close();
}

// --- Execution Timing ---

export async function testExecutionTiming(browser) {
  console.log('\n--- Test Group: Execution Timing ---');

  const page = await browser.newPage();
  const startTime = Date.now();
  await page.goto(TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });
  const elapsed = Date.now() - startTime;

  assert('No breakpoints: execution completes in <5s', elapsed < 5000, `took ${elapsed}ms`);

  await page.close();
}
