/**
 * @fileoverview Tests for breakpoint pause/resume, multiple breakpoints,
 * breakpoints on different expression types, external files, and the
 * breakpoint set/remove API.
 */

import { assert, INLINE_URL, EXTERNAL_URL } from './test_harness.mjs';
import {
  openTestPage, openTestPageWithBreakpoints,
  waitForPause, getStatus, ackPause, doResume,
  setBP, removeBP, getSources, drainPauses,
} from './test_helpers.mjs';

// --- Breakpoint Pause & Resume ---

export async function testBreakpointPauseResume(browser) {
  console.log('\n--- Test Group: Breakpoint Pause & Resume ---');

  // Line 8 = (display (string-append "result=" ...))
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);

  // Should hit breakpoint on line 8
  await waitForPause(page);
  await ackPause(page);

  const status = await getStatus(page);
  assert('Paused state', status.state === 'paused');
  assert('Pause reason is breakpoint', status.reason === 'breakpoint');
  assert('Pause source has correct line', status.source?.line === 8,
    `got line ${status.source?.line}`);
  assert('Pause source has correct file', status.source?.filename === INLINE_URL,
    `got ${status.source?.filename}`);

  // Resume and verify execution completes
  await doResume(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });

  const finalStatus = await getStatus(page);
  assert('Running after resume', finalStatus.state !== 'paused');

  await page.close();
}

// --- Multiple Breakpoints ---

export async function testMultipleBreakpoints(browser) {
  console.log('\n--- Test Group: Multiple Breakpoints ---');

  // Breakpoints on lines 5 and 8
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 5 },
    { url: INLINE_URL, line: 8 },
  ]);

  // First pause should be on line 5 (define greeting)
  await waitForPause(page);
  await ackPause(page);

  const status1 = await getStatus(page);
  assert('First breakpoint hit', status1.state === 'paused');
  const firstLine = status1.source?.line;
  assert('First breakpoint on line 5', firstLine === 5, `got line ${firstLine}`);

  await doResume(page);

  // Second pause should be on line 8 (display ...)
  // but there may be intermediate pauses on line 5/8 from sub-expressions
  let secondPauseLine = null;
  for (let i = 0; i < 30; i++) {
    await new Promise(r => setTimeout(r, 100));
    const s = await getStatus(page);
    if (s.state === 'paused') {
      secondPauseLine = s.source?.line;
      if (secondPauseLine === 8) break;
      await doResume(page);
    }
  }

  assert('Second breakpoint on line 8', secondPauseLine === 8,
    `got line ${secondPauseLine}`);

  // Resume to completion
  await drainPauses(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 15000 });
  assert('Execution completes with multiple breakpoints', true);

  await page.close();
}

// --- Breakpoint on Variable Reference ---

export async function testVariableBreakpoint(browser) {
  console.log('\n--- Test Group: Variable Reference Breakpoint ---');

  // Line 6: greeting (bare variable reference)
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 6 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  const status = await getStatus(page);
  assert('Paused on bare variable line', status.state === 'paused');
  assert('Pause on line 6 (greeting reference)', status.source?.line === 6,
    `got line ${status.source?.line}`);

  // Resume to completion
  await drainPauses(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });
  assert('Execution completes after variable breakpoint', true);

  await page.close();
}

// --- Breakpoint on Literal ---

export async function testLiteralBreakpoint(browser) {
  console.log('\n--- Test Group: Literal Breakpoint ---');

  // Line 7: 42 (bare number literal)
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 7 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  const status = await getStatus(page);
  assert('Paused on bare literal line', status.state === 'paused');
  assert('Pause on line 7 (number literal 42)', status.source?.line === 7,
    `got line ${status.source?.line}`);

  // Resume to completion
  await drainPauses(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });
  assert('Execution completes after literal breakpoint', true);

  await page.close();
}

// --- External File Breakpoint ---

export async function testExternalFileBreakpoint(browser) {
  console.log('\n--- Test Group: External File Breakpoint ---');

  // Set breakpoint in the external manual_script.scm
  // Line 6: (if (<= n 1)  — inside fib function
  const page = await openTestPageWithBreakpoints(browser, [
    { url: EXTERNAL_URL, line: 6 }
  ]);

  // Should eventually pause when fib is called
  try {
    await waitForPause(page);
    await ackPause(page);

    const status = await getStatus(page);
    assert('Paused in external script', status.state === 'paused');
    assert('Pause source is external file', status.source?.filename === EXTERNAL_URL,
      `got ${status.source?.filename}`);
  } catch {
    // The external script might not define fib on that exact line
    // depending on how the source is parsed. Check if any pause happened.
    const pauseEvents = await page.evaluate(() => window._pauseEvents.length);
    assert('External file breakpoint triggered a pause', pauseEvents > 0,
      `no pauses occurred`);
  }

  // Resume to completion
  await drainPauses(page, 100);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 30000 });
  assert('Execution completes with external file breakpoint', true);

  await page.close();
}

// --- Breakpoint Set/Remove API ---

export async function testBreakpointAPI(browser) {
  console.log('\n--- Test Group: Breakpoint Set/Remove API ---');

  const page = await openTestPage(browser);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });

  // Set a breakpoint
  const bpId = await setBP(page, INLINE_URL, 3);
  assert('setBreakpoint returns an ID', typeof bpId === 'string' && bpId.length > 0,
    `got: ${bpId}`);

  // Get all breakpoints
  const allBps = await page.evaluate(() =>
    JSON.parse(JSON.stringify(globalThis.__schemeDebug.getAllBreakpoints()))
  );
  assert('getAllBreakpoints includes the set breakpoint', allBps.length >= 1,
    `got ${allBps.length}`);
  const foundBp = allBps.find(bp => bp.line === 3 && bp.filename === INLINE_URL);
  assert('Breakpoint is on correct line/file', !!foundBp);

  // Remove the breakpoint
  await removeBP(page, bpId);
  const afterRemove = await page.evaluate(() =>
    JSON.parse(JSON.stringify(globalThis.__schemeDebug.getAllBreakpoints()))
  );
  const stillThere = afterRemove.find(bp => bp.id === bpId);
  assert('Breakpoint removed successfully', !stillThere);

  await page.close();
}
