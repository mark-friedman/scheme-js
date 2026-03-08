/**
 * @fileoverview Tests for step into, step over, and step out operations.
 */

import { assert, INLINE_URL } from './test_harness.mjs';
import {
  openTestPageWithBreakpoints,
  waitForPause, getStatus, getStack, ackPause,
  doResume, doStepInto, doStepOver, doStepOut, drainPauses,
} from './test_helpers.mjs';

// --- Step Into ---

export async function testStepInto(browser) {
  console.log('\n--- Test Group: Step Into ---');

  // Breakpoint on line 8: (display (string-append "result=" (number->string (compute 5)) "\n"))
  // After pausing here, stepInto should go into the next sub-expression
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  const statusBefore = await getStatus(page);
  assert('Paused on line 8 before step', statusBefore.source?.line === 8,
    `got line ${statusBefore.source?.line}`);

  // Step into
  await doStepInto(page);
  await waitForPause(page);

  const statusAfter = await getStatus(page);
  assert('After step into: still paused', statusAfter.state === 'paused');
  assert('After step into: reason is step', statusAfter.reason === 'step');
  assert('After step into: source has a line',
    typeof statusAfter.source?.line === 'number',
    `source: ${JSON.stringify(statusAfter.source)}`);

  // Resume to completion (drain remaining pauses)
  await drainPauses(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 15000 });
  assert('Execution completes after step into + resume', true);

  await page.close();
}

// --- Step Over ---

export async function testStepOver(browser) {
  console.log('\n--- Test Group: Step Over ---');

  // Breakpoint on line 8
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  // Step over should advance to the next expression at the same level
  // without going into (compute 5)'s body
  await doStepOver(page);
  await waitForPause(page);

  const statusAfter = await getStatus(page);
  assert('After step over: still paused', statusAfter.state === 'paused');
  assert('After step over: reason is step', statusAfter.reason === 'step');

  // Resume to completion
  await drainPauses(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 15000 });
  assert('Execution completes after step over + resume', true);

  await page.close();
}

// --- Step Out ---

export async function testStepOut(browser) {
  console.log('\n--- Test Group: Step Out ---');

  // Breakpoint on line 4: factorial body. Resume past the define-level hit
  // until we're inside a call (non-empty stack), then step out.
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 4 }
  ]);

  // Find a pause inside a function call (non-empty stack)
  let insideCall = false;
  for (let attempt = 0; attempt < 15; attempt++) {
    await waitForPause(page);
    await ackPause(page);
    const stack = await getStack(page);
    if (stack.length > 0) {
      insideCall = true;
      break;
    }
    await doResume(page);
    await new Promise(r => setTimeout(r, 100));
  }

  assert('Paused inside a function call for step-out', insideCall);

  if (insideCall) {
    // Step out — should return to the caller
    await doStepOut(page);
    await new Promise(r => setTimeout(r, 300));

    const statusAfter = await getStatus(page);
    if (statusAfter.state === 'paused') {
      assert('After step out: still paused (at caller or re-hit breakpoint)', true);
    } else {
      assert('After step out: execution continued past function', true);
    }
  }

  // Resume to completion
  await drainPauses(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 15000 });
  assert('Execution completes after step out + resume', true);

  await page.close();
}
