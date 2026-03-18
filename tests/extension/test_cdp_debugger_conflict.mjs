/**
 * @fileoverview Tests for the CDP debugger conflict with Scheme probe pauses.
 *
 * When Chrome DevTools is open, V8's built-in debugger is active. The probe
 * function's `debugger;` statement causes V8 to pause synchronously, which
 * blocks the interpreter trampoline BEFORE `handlePause()` can run. This
 * means the cooperative pause channel (`scheme-debug-paused`) never fires,
 * and the Scheme-JS panel never shows the pause.
 *
 * The root cause: when `panelConnected=true`, the probe's `hit()` should
 * return `false` so `debugger;` doesn't fire. The cooperative `handlePause()`
 * in the trampoline handles the pause instead.
 *
 * Test strategy:
 *   - Intercept `__schemeProbeRuntime.hit()` to track how many times it
 *     returns `true` (which would trigger `debugger;`). When panelConnected
 *     is true, hit() should NEVER return true — the cooperative channel
 *     handles breakpoints instead.
 *   - Also attach a CDP debugger to confirm it doesn't catch probe pauses.
 */

import { assert, INLINE_URL } from './test_harness.mjs';
import {
  openTestPageWithBreakpoints,
  waitForPause, getStatus, ackPause,
  doResume, doStepInto, drainPauses,
} from './test_helpers.mjs';

// =========================================================================
// Test: probe hit() does not return true when panelConnected=true
// =========================================================================

/**
 * The critical test that catches the real-world bug.
 *
 * When `panelConnected=true`, the probe's `hit()` must return `false` so
 * that the `debugger;` statement in the probe function does NOT fire. If
 * hit() returns true, Chrome DevTools' built-in debugger catches the
 * `debugger;` and pauses in the Sources tab, blocking the trampoline
 * before `handlePause()` can run the cooperative channel.
 *
 * This test wraps `__schemeProbeRuntime.hit()` to track its return values
 * and verifies that it never returns `true` when `panelConnected=true`.
 */
export async function testProbeHitSkipsWhenPanelConnected(browser) {
  console.log('\n--- Test: Probe hit() Skips When panelConnected=true ---');

  const page = await browser.newPage();

  // Inject a wrapper around __schemeProbeRuntime.hit() BEFORE the page loads.
  // This intercepts the hit() call to track when it would trigger debugger;
  await page.evaluateOnNewDocument(() => {
    // Wait for __schemeProbeRuntime to be defined, then wrap hit()
    const watcher = setInterval(() => {
      if (globalThis.__schemeProbeRuntime) {
        clearInterval(watcher);

        const originalHit = globalThis.__schemeProbeRuntime.hit.bind(
          globalThis.__schemeProbeRuntime
        );
        globalThis.__probeHitLog = [];

        globalThis.__schemeProbeRuntime.hit = function(exprId) {
          const result = originalHit(exprId);
          if (result) {
            globalThis.__probeHitLog.push({ exprId, result: true });
          }
          return result;
        };
      }
    }, 10);
  });

  // Set breakpoints via the standard mechanism
  await page.evaluateOnNewDocument((bps) => {
    globalThis.__SCHEME_JS_BREAKPOINTS = bps;
  }, [{ url: INLINE_URL, line: 8 }]);

  const { TEST_PAGE } = await import('./test_harness.mjs');
  await page.goto(TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(
    () => typeof globalThis.__schemeDebug !== 'undefined',
    { timeout: 10000 }
  );

  // Wait for the cooperative pause to fire
  await waitForPause(page);
  await ackPause(page);

  const status = await getStatus(page);
  assert('Paused via cooperative channel', status.state === 'paused');
  assert('Pause reason is breakpoint', status.reason === 'breakpoint');

  // Check: how many times did probe hit() return true?
  // When panelConnected=true, it should NEVER return true — the cooperative
  // channel (handlePause in the trampoline) handles breakpoints instead.
  const hitLog = await page.evaluate(() => globalThis.__probeHitLog || []);
  assert('Probe hit() did not return true when panelConnected=true',
    hitLog.length === 0,
    `hit() returned true ${hitLog.length} time(s) — ` +
    `debugger; would have fired in a real DevTools session, ` +
    `blocking the cooperative channel. ` +
    `exprIds: [${hitLog.map(h => h.exprId).join(', ')}]`);

  await drainPauses(page);
  await page.close();
}

// =========================================================================
// Test: probe hit() returns true for stepping when panelConnected=true
// =========================================================================

/**
 * Verifies that stepping also skips the probe `debugger;` statement when
 * `panelConnected=true`. The probe runtime's step-related hit() returns
 * should all be false; stepping is handled cooperatively.
 */
export async function testProbeHitSkipsDuringSteppingWhenPanelConnected(browser) {
  console.log('\n--- Test: Probe hit() Skips During Stepping When panelConnected=true ---');

  const page = await browser.newPage();

  await page.evaluateOnNewDocument(() => {
    const watcher = setInterval(() => {
      if (globalThis.__schemeProbeRuntime) {
        clearInterval(watcher);

        const originalHit = globalThis.__schemeProbeRuntime.hit.bind(
          globalThis.__schemeProbeRuntime
        );
        globalThis.__probeHitLog = [];

        globalThis.__schemeProbeRuntime.hit = function(exprId) {
          const result = originalHit(exprId);
          if (result) {
            globalThis.__probeHitLog.push({ exprId, result: true });
          }
          return result;
        };
      }
    }, 10);
  });

  await page.evaluateOnNewDocument((bps) => {
    globalThis.__SCHEME_JS_BREAKPOINTS = bps;
  }, [{ url: INLINE_URL, line: 8 }]);

  const { TEST_PAGE } = await import('./test_harness.mjs');
  await page.goto(TEST_PAGE, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(
    () => typeof globalThis.__schemeDebug !== 'undefined',
    { timeout: 10000 }
  );

  // Wait for initial breakpoint pause
  await waitForPause(page);
  await ackPause(page);

  // Reset hit log before stepping
  await page.evaluate(() => { globalThis.__probeHitLog = []; });

  // Step into
  await doStepInto(page);
  await waitForPause(page);

  const status = await getStatus(page);
  assert('After step: paused via cooperative channel', status.state === 'paused');
  assert('After step: reason is step', status.reason === 'step');

  // Check that stepping also didn't fire debugger;
  const hitLog = await page.evaluate(() => globalThis.__probeHitLog || []);
  assert('Probe hit() did not return true during step',
    hitLog.length === 0,
    `hit() returned true ${hitLog.length} time(s) during stepping — ` +
    `debugger; would block stepping in a real DevTools session. ` +
    `exprIds: [${hitLog.map(h => h.exprId).join(', ')}]`);

  await drainPauses(page);
  await page.close();
}

// =========================================================================
// Test: Cooperative pause fires even with a CDP debugger attached
// =========================================================================

/**
 * Attaches a CDP debugger to the page (simulating Chrome DevTools being open),
 * then triggers a Scheme breakpoint. Verifies the cooperative pause fires.
 */
export async function testCooperativePauseWithCDPDebuggerAttached(browser) {
  console.log('\n--- Test: Cooperative Pause With CDP Debugger Attached ---');

  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);

  // Attach a CDP debugger — simulates Chrome DevTools being open
  const client = await page.createCDPSession();
  await client.send('Debugger.enable');

  let cdpPauseCount = 0;
  client.on('Debugger.paused', () => { cdpPauseCount++; });

  let cooperativePauseFired = false;
  try {
    await waitForPause(page, 5000);
    cooperativePauseFired = true;
  } catch {
    // Timeout
  }

  if (cdpPauseCount > 0) {
    await client.send('Debugger.resume').catch(() => {});
    await new Promise(r => setTimeout(r, 200));
  }

  assert('Cooperative pause fires with CDP debugger attached',
    cooperativePauseFired,
    cdpPauseCount > 0
      ? `debugger; blocked trampoline (CDP saw ${cdpPauseCount} pause(s))`
      : 'cooperative pause did not fire');

  if (cooperativePauseFired) {
    await ackPause(page);
    const status = await getStatus(page);
    assert('Paused state is correct', status.state === 'paused');
  }

  await client.detach().catch(() => {});
  await drainPauses(page).catch(() => {});
  await page.close().catch(() => {});
}
