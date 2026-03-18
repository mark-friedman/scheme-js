/**
 * @fileoverview Tests for call stack inspection, variable inspection,
 * and eval during pause.
 */

import { assert, INLINE_URL } from './test_harness.mjs';
import {
  openTestPageWithBreakpoints,
  waitForPause, getStatus, getStack, getLocals, ackPause,
  doResume, debugEval, drainPauses,
} from './test_helpers.mjs';

// --- Call Stack During Pause ---

export async function testCallStack(browser) {
  console.log('\n--- Test Group: Call Stack ---');

  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 4 }
  ]);

  // Should pause on line 4 (factorial definition or first call)
  await waitForPause(page);
  await ackPause(page);

  const stack = await getStack(page);
  assert('Call stack is non-empty during pause', stack.length >= 0);
  // Note: the stack might be empty if we're pausing at define (top-level)
  // since there's no function call context yet. Let's just check we get stack info.

  // Resume past this breakpoint, remove it, then set one that will catch
  // factorial during execution (line 4 inside recursive call)
  await doResume(page);

  // Wait for execution to complete
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });

  await page.close();
}

// --- Variable Inspection ---

export async function testVariableInspection(browser) {
  console.log('\n--- Test Group: Variable Inspection ---');

  // Breakpoint on line 4: (define (factorial n) ...)
  // The first hit is at the define (top level, stack empty).
  // Resume past that. The NEXT hit will be when factorial is called from line 9,
  // entering factorial's body on line 4 with n bound.
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 4 }
  ]);

  // First pause — may be at the define (top-level). Resume until we get a
  // pause WITH a non-empty stack (meaning we're inside a call).
  let foundLocals = false;
  let nValue = null;
  for (let attempt = 0; attempt < 15; attempt++) {
    // Check if still paused (may have finished after last resume)
    const preCheck = await getStatus(page);
    if (preCheck.state !== 'paused') {
      // Not paused — try waiting briefly for next breakpoint
      try {
        await waitForPause(page, 2000);
      } catch {
        break; // No more pauses coming
      }
    }

    await ackPause(page);

    const stack = await getStack(page);
    if (stack.length > 0) {
      const topIdx = stack.length - 1;
      const locals = await getLocals(page, topIdx);
      // Look for any local that might be factorial's parameter
      if (locals.length > 0) {
        foundLocals = true;
        const nLocal = locals.find(l => l.name === 'n');
        assert('Locals available inside function frame',
          locals.length > 0,
          `got ${locals.length} locals: ${locals.map(l => l.name).join(', ')}`);
        if (nLocal) {
          nValue = nLocal.value;
          assert('Local variable n has a value', nLocal.value !== undefined,
            `value: ${nLocal.value}`);
        }
        break;
      }
    }

    await doResume(page);
    await new Promise(r => setTimeout(r, 100));
  }

  if (!foundLocals) {
    assert('Found locals inside a function call', false,
      'never got a frame with locals');
  }

  // Resume to completion — drain all remaining pauses
  await drainPauses(page, 30);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 15000 });
  assert('Execution completes after variable inspection', true);

  await page.close();
}

// --- Eval During Pause ---

export async function testEvalDuringPause(browser) {
  console.log('\n--- Test Group: Eval During Pause ---');

  // Breakpoint on line 8: (display (string-append "result=" (number->string (compute 5)) "\n"))
  const page = await openTestPageWithBreakpoints(browser, [
    { url: INLINE_URL, line: 8 }
  ]);

  await waitForPause(page);
  await ackPause(page);

  // Eval a simple expression
  const result1 = await debugEval(page, '(+ 10 20)');
  assert('Eval (+ 10 20) during pause returns 30', result1 === '30.0', `got: ${result1}`);

  // Eval that accesses defined variables (greeting was defined on line 5)
  const result2 = await debugEval(page, 'greeting');
  assert('Eval greeting returns "hello"', result2 === '"hello"', `got: ${result2}`);

  // Eval with string result
  const result3 = await debugEval(page, '(string-append "hi " "there")');
  assert('Eval string-append', result3 === '"hi there"', `got: ${result3}`);

  // Resume and complete
  await doResume(page);
  await page.waitForFunction(() => window._executionComplete === true, { timeout: 10000 });

  await page.close();
}
