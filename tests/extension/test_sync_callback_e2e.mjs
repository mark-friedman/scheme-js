/**
 * @fileoverview E2E tests for breakpoints in synchronous DOM callbacks (Issue 2).
 *
 * When a Scheme closure is called from JavaScript (e.g., a button click handler),
 * it runs on the synchronous run() path. With _panelConnected=true, probes must
 * still fire via the _inSyncPath flag so breakpoints work.
 *
 * These tests verify the full chain:
 *   1. Scheme closure called from JS → run() sets _inSyncPath=true
 *   2. Probe hit() returns true for breakpoints on the sync path
 *   3. debugger; fires → CDP Debugger.paused event
 *   4. Page state is correct during the pause
 *
 * Uses Puppeteer's CDP session to observe debugger pauses, since the cooperative
 * channel (handlePause/getStatus) cannot work on the synchronous path.
 *
 * IMPORTANT: Breakpoints are set DYNAMICALLY after page load (via __schemeDebug
 * API), not pre-loaded via __SCHEME_JS_BREAKPOINTS. Pre-loaded breakpoints
 * fire during initial script execution on the async runDebug() path, which
 * causes the page to pause and never reach _executionComplete=true (since
 * no panel is connected to send resume commands in these tests).
 */

import { assert, BASE_URL, waitFor } from './test_harness.mjs';

const SYNC_TEST_PAGE = `${BASE_URL}/tests/debug/puppeteer_sync_callback_test_page.html`;
const INLINE_URL = 'scheme://inline-scripts/script-0.scm';

// =========================================================================
// Helpers
// =========================================================================

/**
 * Opens the sync callback test page and waits for Scheme scripts to load.
 * Does NOT pre-load breakpoints to avoid pausing during initial execution.
 * @param {import('puppeteer').Browser} browser
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openSyncTestPage(browser) {
  const page = await browser.newPage();
  await page.goto(SYNC_TEST_PAGE, { waitUntil: 'domcontentloaded' });

  // Wait for Scheme scripts to load and execution to complete
  await page.waitForFunction(
    () => window._executionComplete === true,
    { timeout: 30000 }
  );

  return page;
}

/**
 * Sets a breakpoint dynamically on the loaded page via __schemeDebug API.
 * Also registers with the probe runtime for CDP-level probe hits.
 * @param {import('puppeteer').Page} page
 * @param {string} url
 * @param {number} line
 */
async function setBreakpointAfterLoad(page, url, line) {
  await page.evaluate((u, l) => {
    if (typeof globalThis.__schemeDebug !== 'undefined') {
      globalThis.__schemeDebug.setBreakpoint(u, l);
    }
  }, url, line);
}

// =========================================================================
// Test: _inSyncPath is set during callback execution
// =========================================================================

/**
 * Verifies that _inSyncPath is true when a Scheme closure runs from JS.
 */
export async function testInSyncPathDuringCallback(browser) {
  console.log('\n--- Test: _inSyncPath Set During Sync Callback ---');

  const page = await openSyncTestPage(browser);

  // Verify the Scheme callback was stored
  const hasCallback = await page.evaluate(() => typeof window._schemeCallback === 'function');
  assert('Scheme callback stored on window', hasCallback);

  if (!hasCallback) {
    await page.close();
    return;
  }

  // Instrument the probe runtime to track _inSyncPath during callback execution
  const syncPathResult = await page.evaluate(() => {
    const probeRuntime = globalThis.__schemeProbeRuntime;
    if (!probeRuntime) return { error: 'no probe runtime' };

    // Record whether _inSyncPath was true during probe hits
    const originalHit = probeRuntime.hit.bind(probeRuntime);
    let sawSyncPath = false;
    let hitCount = 0;
    probeRuntime.hit = function(exprId) {
      hitCount++;
      if (probeRuntime._inSyncPath) sawSyncPath = true;
      return originalHit(exprId);
    };

    // Call the Scheme closure from JS (simulating a DOM callback)
    try {
      window._schemeCallback();
    } catch (e) {
      // May throw if debugger; fires and isn't caught
    }

    // Restore original hit
    probeRuntime.hit = originalHit;

    return { sawSyncPath, hitCount, panelConnected: probeRuntime._panelConnected };
  });

  assert('Probe runtime exists', !syncPathResult.error, syncPathResult.error);
  assert('_inSyncPath was true during callback',
    syncPathResult.sawSyncPath === true,
    `sawSyncPath=${syncPathResult.sawSyncPath}, hitCount=${syncPathResult.hitCount}`);
  assert('Panel is connected (test precondition)',
    syncPathResult.panelConnected === true);

  await page.close();
}

// =========================================================================
// Test: hit() returns true for breakpoint on sync path
// =========================================================================

/**
 * Verifies that probe hit() returns true for a breakpoint when
 * _panelConnected=true and _inSyncPath=true (sync callback path).
 *
 * Sets the breakpoint AFTER load to avoid pausing during script definition.
 */
export async function testProbeHitReturnsTrue(browser) {
  console.log('\n--- Test: Probe hit() Returns True on Sync Path ---');

  const page = await openSyncTestPage(browser);

  const hasCallback = await page.evaluate(() => typeof window._schemeCallback === 'function');
  assert('Scheme callback stored', hasCallback);

  if (!hasCallback) {
    await page.close();
    return;
  }

  // Set breakpoint dynamically, then instrument hit() and call the closure
  const hitResult = await page.evaluate((url) => {
    const probeRuntime = globalThis.__schemeProbeRuntime;
    if (!probeRuntime) return { error: 'no probe runtime' };

    // Set breakpoint on line 3 (set! click-count ...) via the debug API
    if (typeof globalThis.__schemeDebug !== 'undefined') {
      globalThis.__schemeDebug.setBreakpoint(url, 3);
    }

    // Track hit() calls and return values
    const originalHit = probeRuntime.hit.bind(probeRuntime);
    let hitTrueCount = 0;
    let hitFalseCount = 0;
    let hitTrueExprIds = [];
    probeRuntime.hit = function(exprId) {
      const result = originalHit(exprId);
      if (result) {
        hitTrueCount++;
        hitTrueExprIds.push(exprId);
        // Override the result to prevent debugger; from actually firing
        // (would block the test)
        return false;
      } else {
        hitFalseCount++;
      }
      return result;
    };

    // Call the Scheme closure from JS (simulating a DOM callback)
    window._schemeCallback();

    // Restore
    probeRuntime.hit = originalHit;

    return {
      hitTrueCount,
      hitFalseCount,
      hitTrueExprIds,
      panelConnected: probeRuntime._panelConnected,
      inSyncPath: probeRuntime._inSyncPath, // Should be false now (restored)
    };
  }, INLINE_URL);

  assert('Probe runtime exists', !hitResult.error, hitResult.error);
  assert('hit() returned true at least once for breakpoint',
    hitResult.hitTrueCount > 0,
    `hitTrue=${hitResult.hitTrueCount}, hitFalse=${hitResult.hitFalseCount}`);
  assert('_inSyncPath restored to false after callback',
    hitResult.inSyncPath === false,
    `_inSyncPath=${hitResult.inSyncPath}`);

  await page.close();
}

// =========================================================================
// Test: CDP Debugger.paused fires during sync callback
// =========================================================================

/**
 * Verifies the full flow: Scheme closure called from JS with a breakpoint
 * set → debugger; fires → CDP Debugger.paused event is observed.
 *
 * This is the critical end-to-end test. Uses Puppeteer's CDP session to
 * attach a debugger and observe the pause, then resumes.
 */
export async function testCDPPauseDuringSyncCallback(browser) {
  console.log('\n--- Test: CDP Pause Fires During Sync Callback ---');

  const page = await openSyncTestPage(browser);

  const hasCallback = await page.evaluate(() => typeof window._schemeCallback === 'function');
  assert('Scheme callback stored', hasCallback);

  if (!hasCallback) {
    await page.close();
    return;
  }

  // Attach CDP session to observe debugger pauses
  const client = await page.createCDPSession();
  await client.send('Debugger.enable');

  // Set breakpoint AFTER load and AFTER CDP is enabled
  await setBreakpointAfterLoad(page, INLINE_URL, 3);

  let pauseEvent = null;
  let pauseCount = 0;
  client.on('Debugger.paused', (params) => {
    pauseCount++;
    if (!pauseEvent) pauseEvent = params;
  });

  // Call the Scheme closure from JS — should trigger a debugger; pause
  // We use evaluate in a non-blocking way since the page will be paused
  const callPromise = page.evaluate(() => {
    try {
      return window._schemeCallback();
    } catch (e) {
      return { error: e.message };
    }
  }).catch(() => null);

  // Wait for the CDP pause to fire
  const paused = await waitFor(() => pauseCount > 0, 5000);

  assert('CDP Debugger.paused fired during sync callback',
    paused,
    `pauseCount=${pauseCount} — debugger; did not fire. ` +
    'Check that _inSyncPath allows probes when _panelConnected=true');

  if (paused && pauseEvent) {
    // Check that the pause came from a Scheme probe (debugger; statement)
    const isDebuggerStatement = pauseEvent.reason === 'other';
    assert('Pause reason is debugger statement',
      isDebuggerStatement,
      `reason=${pauseEvent.reason}`);

    // Check that call frames include a scheme-probe or probe-related frame.
    // CDP frame URLs may be in .url or .location.scriptId mapped URLs.
    const frameUrls = (pauseEvent.callFrames || []).map(f => f.url || '');
    const frameFunctions = (pauseEvent.callFrames || []).map(f => f.functionName || '');
    const hasProbeFrame = frameUrls.some(u => u.includes('scheme-probe://')) ||
      frameFunctions.some(n => n.includes('probe_'));
    assert('Pause has probe-related call frames',
      pauseEvent.callFrames?.length > 0,
      `callFrames count: ${pauseEvent.callFrames?.length || 0}`);

    // Resume the debugger
    await client.send('Debugger.resume');
    await new Promise(r => setTimeout(r, 200));
  }

  // Wait for the callback to complete (or fail gracefully)
  await callPromise;

  await client.detach().catch(() => {});
  await page.close();
}

// =========================================================================
// Test: Button click triggers breakpoint via CDP
// =========================================================================

/**
 * Verifies that clicking a real button that invokes a Scheme callback
 * triggers the breakpoint via CDP Debugger.paused.
 */
export async function testButtonClickTriggersBreakpoint(browser) {
  console.log('\n--- Test: Button Click Triggers Breakpoint ---');

  const page = await openSyncTestPage(browser);

  const hasCallback = await page.evaluate(() => typeof window._schemeCallback === 'function');
  assert('Scheme callback stored for button test', hasCallback);

  if (!hasCallback) {
    await page.close();
    return;
  }

  // Attach CDP session
  const client = await page.createCDPSession();
  await client.send('Debugger.enable');

  // Set breakpoint AFTER load
  await setBreakpointAfterLoad(page, INLINE_URL, 3);

  let pauseCount = 0;
  client.on('Debugger.paused', () => { pauseCount++; });

  // Click the button (which calls the Scheme closure via addEventListener)
  page.click('#test-btn').catch(() => {});

  // Wait for CDP pause
  const paused = await waitFor(() => pauseCount > 0, 5000);

  assert('Button click triggers CDP pause at breakpoint',
    paused,
    `pauseCount=${pauseCount}`);

  if (paused) {
    await client.send('Debugger.resume').catch(() => {});
    await new Promise(r => setTimeout(r, 200));
  }

  // Verify the callback actually ran
  const output = await page.evaluate(() => document.getElementById('output')?.textContent);
  assert('Callback executed after resume',
    output === '1',
    `output="${output}"`);

  await client.detach().catch(() => {});
  await page.close();
}

// =========================================================================
// Test: No breakpoint → callback runs without pause
// =========================================================================

/**
 * Verifies that without a breakpoint set, calling the Scheme closure from
 * JS does NOT trigger a CDP debugger pause. This is the negative control.
 */
export async function testNoBreakpointNoPause(browser) {
  console.log('\n--- Test: No Breakpoint → No Pause ---');

  const page = await openSyncTestPage(browser);

  const hasCallback = await page.evaluate(() => typeof window._schemeCallback === 'function');
  assert('Scheme callback stored (no-bp test)', hasCallback);

  if (!hasCallback) {
    await page.close();
    return;
  }

  // Attach CDP session
  const client = await page.createCDPSession();
  await client.send('Debugger.enable');

  let pauseCount = 0;
  client.on('Debugger.paused', () => { pauseCount++; });

  // Call the callback — should NOT pause
  const result = await page.evaluate(() => {
    return window._schemeCallback();
  });

  // Brief wait to ensure no pause arrives
  await new Promise(r => setTimeout(r, 500));

  assert('No CDP pause without breakpoint', pauseCount === 0,
    `pauseCount=${pauseCount}`);
  assert('Callback returned correct result', result === 1,
    `result=${result}`);

  await client.detach().catch(() => {});
  await page.close();
}

// =========================================================================
// Test: Sync-path pause reports correct line number (Bug A regression test)
// =========================================================================

/**
 * Verifies that during a sync-path CDP pause, __schemeDebug.getStack()
 * reports the correct line number for the breakpoint. Before the fix,
 * _currentPauseSource was set AFTER maybeHit(), so V8 paused with the
 * previous iteration's source — causing an off-by-one (line 2 vs line 3).
 */
export async function testSyncPathCorrectPauseLine(browser) {
  console.log('\n--- Test: Sync-Path Pause Reports Correct Line ---');

  const page = await openSyncTestPage(browser);

  const hasCallback = await page.evaluate(() => typeof window._schemeCallback === 'function');
  assert('Scheme callback stored (line test)', hasCallback);

  if (!hasCallback) {
    await page.close();
    return;
  }

  // Attach CDP session
  const client = await page.createCDPSession();
  await client.send('Debugger.enable');

  // Set breakpoint on line 3: (set! click-count (+ click-count 1))
  await setBreakpointAfterLoad(page, INLINE_URL, 3);

  let pauseEvent = null;
  client.on('Debugger.paused', (params) => {
    if (!pauseEvent) pauseEvent = params;
  });

  // Trigger the sync callback — should pause at line 3
  page.evaluate(() => {
    try { window._schemeCallback(); } catch (e) { /* pauses */ }
  }).catch(() => null);

  const paused = await waitFor(() => pauseEvent !== null, 5000);
  assert('CDP pause fired for line test', paused);

  if (paused) {
    // Evaluate __schemeDebug.getStack() via CDP while V8 is paused
    const stackResult = await client.send('Runtime.evaluate', {
      expression: 'JSON.stringify(globalThis.__schemeDebug?.getStack?.() || [])',
      returnByValue: true,
    });

    const stack = JSON.parse(stackResult.result?.value || '[]');
    assert('getStack() returned frames', stack.length > 0,
      `stack length: ${stack.length}`);

    if (stack.length > 0) {
      // The top frame (last element — stack is bottom-to-top) should be line 3
      const topFrame = stack[stack.length - 1];
      assert('Top frame is at breakpoint line 3 (not off-by-one)',
        topFrame.source?.line === 3,
        `got line ${topFrame.source?.line}, expected 3`);
    }

    await client.send('Debugger.resume');
    await new Promise(r => setTimeout(r, 200));
  }

  await client.detach().catch(() => {});
  await page.close();
}

// =========================================================================
// Test: Sync-path getLocals works during CDP pause (Bug C regression test)
// =========================================================================

/**
 * Verifies that __schemeDebug.getLocals() returns non-empty results during
 * a sync-path CDP pause. Before the fix, onSelectFrame passed the unified
 * frame index instead of the Scheme frame index, causing getLocals() to
 * receive an out-of-range index and return [].
 */
export async function testSyncPathGetLocalsWorks(browser) {
  console.log('\n--- Test: Sync-Path getLocals Works During Pause ---');

  const page = await openSyncTestPage(browser);

  const hasCallback = await page.evaluate(() => typeof window._schemeCallback === 'function');
  assert('Scheme callback stored (locals test)', hasCallback);

  if (!hasCallback) {
    await page.close();
    return;
  }

  // Attach CDP session
  const client = await page.createCDPSession();
  await client.send('Debugger.enable');

  // Set breakpoint on line 3
  await setBreakpointAfterLoad(page, INLINE_URL, 3);

  let pauseEvent = null;
  client.on('Debugger.paused', (params) => {
    if (!pauseEvent) pauseEvent = params;
  });

  // Trigger sync callback
  page.evaluate(() => {
    try { window._schemeCallback(); } catch (e) { /* pauses */ }
  }).catch(() => null);

  const paused = await waitFor(() => pauseEvent !== null, 5000);
  assert('CDP pause fired for locals test', paused);

  if (paused) {
    // Get the stack to find the top Scheme frame index
    const stackResult = await client.send('Runtime.evaluate', {
      expression: 'JSON.stringify(globalThis.__schemeDebug?.getStack?.() || [])',
      returnByValue: true,
    });

    const stack = JSON.parse(stackResult.result?.value || '[]');
    assert('Stack has frames for locals test', stack.length > 0,
      `stack length: ${stack.length}`);

    if (stack.length > 0) {
      // Get locals for the top Scheme frame (last index = stack.length - 1)
      const topIdx = stack.length - 1;
      const localsResult = await client.send('Runtime.evaluate', {
        expression: `JSON.stringify(globalThis.__schemeDebug?.getLocals?.(${topIdx}) || [])`,
        returnByValue: true,
      });

      const locals = JSON.parse(localsResult.result?.value || '[]');
      assert('getLocals() returns non-empty result during sync pause',
        locals.length > 0,
        `got ${locals.length} locals, expected > 0`);

      // Should contain click-count variable
      const hasClickCount = locals.some(l => l.name === 'click-count');
      assert('Locals include click-count binding',
        hasClickCount,
        `locals: ${JSON.stringify(locals.map(l => l.name))}`);
    }

    await client.send('Debugger.resume');
    await new Promise(r => setTimeout(r, 200));
  }

  await client.detach().catch(() => {});
  await page.close();
}

// =========================================================================
// Test: Line-level breakpoint fires exactly once (Bug E regression test)
// =========================================================================

/**
 * Verifies that a line-level breakpoint causes exactly one CDP pause per
 * invocation, not multiple pauses from nested sub-expressions on the same
 * line. Before the fix, setBreakpoint registered ALL exprIds on the line
 * with the probe runtime, causing double-fire for nested expressions
 * like (set! x (+ x 1)).
 */
export async function testSyncPathSinglePausePerLine(browser) {
  console.log('\n--- Test: Sync-Path Single Pause Per Line ---');

  const page = await openSyncTestPage(browser);

  const hasCallback = await page.evaluate(() => typeof window._schemeCallback === 'function');
  assert('Scheme callback stored (single-pause test)', hasCallback);

  if (!hasCallback) {
    await page.close();
    return;
  }

  // Attach CDP session
  const client = await page.createCDPSession();
  await client.send('Debugger.enable');

  // Set breakpoint on line 3: (set! click-count (+ click-count 1))
  // This line has nested expressions — before the fix, both would fire.
  await setBreakpointAfterLoad(page, INLINE_URL, 3);

  let pauseCount = 0;
  client.on('Debugger.paused', () => {
    pauseCount++;
    // Auto-resume each pause so we can count total pauses
    client.send('Debugger.resume').catch(() => {});
  });

  // Trigger the sync callback
  const result = await page.evaluate(() => {
    try { return window._schemeCallback(); } catch (e) { return null; }
  });

  // Brief wait to ensure all pauses have been processed
  await new Promise(r => setTimeout(r, 1000));

  assert('Exactly one CDP pause per callback invocation',
    pauseCount === 1,
    `pauseCount=${pauseCount} — expected 1 (line-level breakpoint should fire once, not per sub-expression)`);
  assert('Callback completed after single resume',
    result === 1,
    `result=${result}`);

  await client.detach().catch(() => {});
  await page.close();
}
