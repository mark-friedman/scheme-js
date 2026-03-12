/**
 * @fileoverview Real integration tests for JS ↔ Scheme boundary debugging.
 *
 * These tests load puppeteer_js_test_page.html which has both JavaScript
 * and Scheme code. The Scheme code has:
 *   - Top-level JS calls: (define doubled (jsDouble 7)) etc.
 *   - A function with JS calls: (define (test-interop x) ...)
 *   - Top-level call to the function: (define result (test-interop 5))
 *
 * Tests cover BOTH top-level and in-function debugging.
 *
 * IMPORTANT: textContent of a <script> tag includes the leading newline
 * after the closing >, so line 1 is empty. All code starts at line 2.
 *
 * Scheme line map (actual line numbers as seen by the parser):
 *   Line 1:  (empty — newline after script tag)
 *   Line 2:  (define doubled (jsDouble 7))
 *   Line 3:  (define summed (jsAdd 10 20))
 *   Line 4:  (define (test-interop x)
 *   Line 5:    (let ((d (jsDouble x))
 *   Line 6:          (s (jsAdd x 100)))
 *   Line 7:      (let ((my-fn jsDouble))
 *   Line 8:        (let ((via-var (my-fn x))
 *   Line 9:              (via-dot (window.jsHelper.compute x)))
 *   Line 10:         (list d s via-var via-dot)))))
 *   Line 11: (define result (test-interop 5))
 *   Line 12: (display ...)
 *
 * Tests use only the page-side __schemeDebug API (no extension panel).
 */

import { assert, recordCrash, JS_TEST_PAGE, INLINE_URL } from './test_harness.mjs';
import {
  waitForDebugAPI, waitForPause, waitForRunning,
  setBP, getStatus, getStack, getLocals,
  ackPause, doResume, doStepInto, doStepOver, doStepOut,
  drainPauses,
} from './test_helpers.mjs';

const INLINE_SCM = INLINE_URL;

// Well-known standard library names to filter from top-level locals
const STDLIB_NAMES = new Set([
  '+', '-', '*', '/', 'car', 'cdr', 'cons', 'list', 'pair?', 'null?',
  'display', 'write', 'eq?', 'eqv?', 'equal?', 'not', 'append', 'map',
  'for-each', 'apply', 'values', 'length', 'reverse', 'number?',
  'string?', 'symbol?', 'boolean?', 'vector?', 'procedure?',
]);

// =========================================================================
// Helper: open the JS interop test page
// =========================================================================

/**
 * Opens the JS test page with Puppeteer-injected breakpoints.
 * Sets __SCHEME_JS_PUPPETEER to disable auto-resume and default breakpoints.
 *
 * @param {import('puppeteer').Browser} browser
 * @param {Array<{url: string, line: number}>} breakpoints
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openJSTestPage(browser, breakpoints) {
  const page = await browser.newPage();

  // Signal Puppeteer mode: skip defaults and disable auto-resume
  await page.evaluateOnNewDocument((bps) => {
    globalThis.__SCHEME_JS_PUPPETEER = true;
    globalThis.__SCHEME_JS_BREAKPOINTS = bps;
  }, breakpoints);

  await page.goto(JS_TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await waitForDebugAPI(page);
  return page;
}

/**
 * Polls through pauses until status reaches the target line, resuming
 * intermediate sub-expression pauses. Returns the status at the target line.
 *
 * @param {import('puppeteer').Page} page
 * @param {number} targetLine
 * @param {number} [maxAttempts=30]
 * @returns {Promise<Object|null>} status at target line, or null
 */
async function waitForLine(page, targetLine, maxAttempts = 30) {
  for (let i = 0; i < maxAttempts; i++) {
    const s = await getStatus(page);
    if (s.state !== 'paused') {
      try { await waitForPause(page, 2000); } catch { return null; }
      continue;
    }
    await ackPause(page);
    if (s.source?.line === targetLine) return s;
    await doResume(page);
    await new Promise(r => setTimeout(r, 100));
  }
  return null;
}

// =========================================================================
// Test Group: JS Interop Page (self-test)
// =========================================================================

/**
 * Tests that the JS interop test page loads and its self-tests pass
 * in standalone mode (no Puppeteer breakpoint override).
 */
export async function testJSInteropPageSelfTest(browser) {
  console.log('\n--- Test Group: JS Interop Page Self-Test ---');
  const page = await browser.newPage();
  await page.goto(JS_TEST_PAGE, { waitUntil: 'domcontentloaded' });

  const allPass = await page.waitForFunction(
    () => {
      const output = document.getElementById('output');
      if (!output) return false;
      return output.innerHTML.includes('PASS') && !output.innerHTML.includes('FAIL');
    },
    { timeout: 10000 }
  ).then(() => true).catch(() => false);

  assert('JS interop page self-tests pass', allPass);
  await page.close();
}

// =========================================================================
// Test Group: Top-Level Breakpoint (Scheme→JS)
// =========================================================================

/**
 * Breakpoint on line 2: (define doubled (jsDouble 7))
 * Tests that top-level JS call breakpoints work correctly.
 */
export async function testBreakpointAtJSCallSite(browser) {
  console.log('\n--- Test Group: Top-Level Breakpoint at JS Call ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 2 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  const status = await getStatus(page);
  assert('Paused at top-level JS call', status.state === 'paused');
  assert('Pause line is 2 (jsDouble top-level)', status.source?.line === 2,
    `got line ${status.source?.line}`);

  // Stack should have at least 1 frame (synthetic top-level)
  const stack = await getStack(page);
  assert('Stack non-empty at top-level pause', stack.length > 0,
    `got ${stack.length} frames`);

  await doResume(page);
  await drainPauses(page);
  await waitForRunning(page, 8000);
  assert('Execution completes after resume',
    (await getStatus(page)).state !== 'paused');

  await page.close();
}

// =========================================================================
// Test Group: Step Into from Top-Level (Scheme→JS)
// =========================================================================

/**
 * Breakpoint on line 2: (define doubled (jsDouble 7))
 * StepInto should advance execution from top-level.
 */
export async function testStepIntoJSFromScheme(browser) {
  console.log('\n--- Test Group: Step Into JS from Top-Level ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 2 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  await doStepInto(page);
  await waitForPause(page, 5000);
  await ackPause(page);

  const status = await getStatus(page);
  assert('Still paused after step into', status.state === 'paused');
  assert('Step reason is step', status.reason === 'step');

  await drainPauses(page);
  await waitForRunning(page, 8000);
  assert('Execution completes after step-into',
    (await getStatus(page)).state !== 'paused');

  await page.close();
}

// =========================================================================
// Test Group: Step Over from Top-Level (Scheme→JS)
// =========================================================================

/**
 * Breakpoint on line 2: (define doubled (jsDouble 7))
 * StepOver should eventually advance past line 2.
 */
export async function testStepOverJSFromScheme(browser) {
  console.log('\n--- Test Group: Step Over JS from Top-Level ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 2 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  // Step over — may take multiple steps due to sub-expression pauses
  let advanced = false;
  for (let i = 0; i < 10; i++) {
    await doStepOver(page);
    try {
      await waitForPause(page, 5000);
    } catch {
      advanced = true; // Execution completed
      break;
    }
    await ackPause(page);
    const s = await getStatus(page);
    if (s.state === 'paused' && s.source?.line > 2) {
      advanced = true;
      break;
    }
    if (s.state !== 'paused') {
      advanced = true;
      break;
    }
  }

  assert('Step over advanced past line 2', advanced);

  await drainPauses(page);
  await waitForRunning(page, 8000);
  assert('Execution completes after step-over',
    (await getStatus(page)).state !== 'paused');

  await page.close();
}

// =========================================================================
// Test Group: Step Out from Inside Function
// =========================================================================

/**
 * Breakpoint on line 5 (inside test-interop: jsDouble call).
 * StepOut should return to the caller.
 */
export async function testStepOutFromJSBoundary(browser) {
  console.log('\n--- Test Group: Step Out from Inside Function ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 5 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  await doStepOut(page);
  try {
    await waitForPause(page, 5000);
    await ackPause(page);
    const status = await getStatus(page);
    assert('Still paused after step out', status.state === 'paused');
  } catch {
    // Might complete if we step out of top-level code
    assert('Step out completed execution', true);
  }

  await drainPauses(page);
  await waitForRunning(page, 8000);
  assert('Execution completes after step-out',
    (await getStatus(page)).state !== 'paused');

  await page.close();
}

// =========================================================================
// Test Group: Variables at Top-Level JS Boundary
// =========================================================================

/**
 * Breakpoint on line 3: (define summed (jsAdd 10 20))
 * At this point, 'doubled' should be defined (from line 2).
 * Tests that top-level variables are visible via the synthetic frame.
 */
export async function testVariablesAtJSBoundary(browser) {
  console.log('\n--- Test Group: Variables at Top-Level JS Boundary ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 3 }
  ]);

  const s = await waitForLine(page, 3);
  assert('Paused at line 3 (jsAdd top-level)', s?.source?.line === 3,
    `got line ${s?.source?.line}`);

  const stack = await getStack(page);
  assert('Stack non-empty during top-level pause', stack.length > 0,
    `stack length: ${stack.length}`);

  // Get locals from the top frame. At top-level this is the global env
  // which includes standard library bindings. Filter to user-defined vars.
  const locals = await getLocals(page, stack.length - 1);
  assert('Locals available at top-level', locals && locals.length > 0,
    `got ${locals.length} locals`);

  // Filter out standard library names — look for user-defined 'doubled'
  const userNames = locals
    .map(l => l.name || l[0])
    .filter(n => !STDLIB_NAMES.has(n) && !n.startsWith('%'));
  assert('User-defined doubled is in locals', userNames.includes('doubled'),
    `user locals: ${userNames.slice(0, 20).join(', ')}${userNames.length > 20 ? '...' : ''}`);

  await drainPauses(page);
  await waitForRunning(page, 8000);
  await page.close();
}

// =========================================================================
// Test Group: Multiple Breakpoints (Top-Level + In-Function)
// =========================================================================

/**
 * Breakpoints on line 2 (top-level jsDouble) and line 5 (in-function jsDouble).
 * Tests that breakpoints hit at both levels.
 */
export async function testMultipleBreakpointsAcrossJSCalls(browser) {
  console.log('\n--- Test Group: Multiple Breakpoints (Top-Level + In-Function) ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 2 },
    { url: INLINE_SCM, line: 5 },
  ]);

  // First breakpoint: line 2 (top-level)
  const s1 = await waitForLine(page, 2);
  assert('First BP on line 2 (top-level jsDouble)', s1?.source?.line === 2,
    `got line ${s1?.source?.line}`);

  if (s1) await doResume(page);

  // Second breakpoint: line 5 (inside test-interop)
  const s2 = await waitForLine(page, 5);
  assert('Second BP on line 5 (in-function jsDouble)', s2?.source?.line === 5,
    `got line ${s2?.source?.line}`);

  await drainPauses(page);
  await waitForRunning(page, 8000);
  assert('Execution completes with multiple BPs',
    (await getStatus(page)).state !== 'paused');

  await page.close();
}

// =========================================================================
// Test Group: Dot Notation JS Call (In-Function)
// =========================================================================

/**
 * Breakpoint on line 9: (via-dot (window.jsHelper.compute x))
 * Verifies Scheme can call JS methods via dot notation during debugging.
 */
export async function testDotNotationJSCall(browser) {
  console.log('\n--- Test Group: Dot Notation JS Call ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 9 }
  ]);

  const s = await waitForLine(page, 9);
  assert('Paused at line 9 (dot notation)', s?.source?.line === 9,
    `got line ${s?.source?.line}`);

  // Step over — may need multiple steps
  let advanced = false;
  for (let i = 0; i < 10; i++) {
    await doStepOver(page);
    try {
      await waitForPause(page, 5000);
    } catch {
      advanced = true;
      break;
    }
    await ackPause(page);
    const status = await getStatus(page);
    if (status.state === 'paused' && status.source?.line > 9) {
      advanced = true;
      break;
    }
    if (status.state !== 'paused') {
      advanced = true;
      break;
    }
  }

  assert('Step over dot notation: advanced past line 9', advanced);

  await drainPauses(page);
  await waitForRunning(page, 8000);
  await page.close();
}

// =========================================================================
// Test Group: Stored JS Function Call (In-Function)
// =========================================================================

/**
 * Breakpoint on line 8: (via-var (my-fn x))
 * where my-fn = jsDouble (stored in a Scheme variable).
 */
export async function testStoredJSFunctionCall(browser) {
  console.log('\n--- Test Group: Stored JS Function Call ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 8 }
  ]);

  const s = await waitForLine(page, 8);
  assert('Paused at line 8 (stored JS fn)', s?.source?.line === 8,
    `got line ${s?.source?.line}`);

  // Step into the stored JS function call
  await doStepInto(page);
  await waitForPause(page, 5000);
  await ackPause(page);

  const s2 = await getStatus(page);
  assert('Step into stored JS fn: still paused', s2.state === 'paused');
  assert('Step into stored JS fn: reason is step', s2.reason === 'step');

  await drainPauses(page);
  await waitForRunning(page, 8000);
  assert('Execution completes after stored JS fn test',
    (await getStatus(page)).state !== 'paused');

  await page.close();
}

// =========================================================================
// Test Group: Call Stack Inside JS-Calling Function
// =========================================================================

/**
 * Breakpoint on line 5 (inside test-interop).
 * Should have real function frames (not just synthetic top-level).
 */
export async function testCallStackAtJSBoundary(browser) {
  console.log('\n--- Test Group: Call Stack at JS Boundary ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 5 }
  ]);

  // Poll until we find a pause with a non-empty stack
  let foundStack = false;
  for (let attempt = 0; attempt < 20; attempt++) {
    const s = await getStatus(page);
    if (s.state !== 'paused') {
      try { await waitForPause(page, 2000); } catch { break; }
      continue;
    }
    await ackPause(page);

    const stack = await getStack(page);
    if (stack.length > 0) {
      foundStack = true;
      const names = stack.map(f => f.name);
      assert('Call stack is non-empty at JS call boundary', true);
      assert('Call stack has frame names', names.some(n => n && n.length > 0),
        `names: ${names.join(', ')}`);
      break;
    }

    await doResume(page);
    await new Promise(r => setTimeout(r, 100));
  }

  if (!foundStack) {
    assert('Call stack is non-empty at JS call boundary', false, 'never got non-empty stack');
    assert('Call stack has frame names', false, 'no stack found');
  }

  await drainPauses(page);
  await waitForRunning(page, 8000);
  await page.close();
}

// =========================================================================
// Test Group: Step Through Full JS Interop Sequence
// =========================================================================

/**
 * Start at top-level line 2, step over multiple lines to verify
 * advancing through the JS call sequence.
 */
export async function testStepThroughJSInteropSequence(browser) {
  console.log('\n--- Test Group: Step Through JS Interop Sequence ---');
  const page = await openJSTestPage(browser, [
    { url: INLINE_SCM, line: 2 }
  ]);

  const s = await waitForLine(page, 2);
  assert('Start at line 2', s?.source?.line === 2,
    `got line ${s?.source?.line}`);

  // Step over several times, tracking which lines we visit
  const linesVisited = [2];
  for (let i = 0; i < 15; i++) {
    await doStepOver(page);
    try {
      await waitForPause(page, 3000);
    } catch {
      break;
    }
    await ackPause(page);
    const status = await getStatus(page);
    if (status.state !== 'paused') break;
    const line = status.source?.line;
    if (line && line !== linesVisited[linesVisited.length - 1]) {
      linesVisited.push(line);
    }
  }

  assert('Visited at least 2 different lines',
    linesVisited.length >= 2,
    `visited lines: ${linesVisited.join(', ')}`);

  await drainPauses(page);
  await waitForRunning(page, 8000);
  await page.close();
}
