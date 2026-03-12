/**
 * @fileoverview Tests for the Eval Console UI.
 */

import { assert, INLINE_URL } from './test_harness.mjs';
import { openPanelPage } from './test_helpers.mjs';
import { MOCK_CHROME_SCRIPT } from './test_panel_interactions.mjs';

/**
 * Opens a mocked panel with eval support.
 * Adds __schemeDebug.eval(...) handling to the inspectedWindow.eval mock,
 * fires a pause event, and clicks the top frame so selectedUnifiedFrame is set.
 *
 * @param {import('puppeteer').Browser} browser
 * @param {string} extensionId
 * @returns {Promise<import('puppeteer').Page>}
 */
async function openMockedPanel(browser, extensionId) {
  const panelPage = await browser.newPage();

  // Extend the mock to handle __schemeDebug.eval() calls routed through
  // scheme-bridge → inspectedWindow.eval. We add eval results to __mockState
  // and patch the eval dispatcher to recognise the eval pattern.
  const CONSOLE_MOCK_SCRIPT = MOCK_CHROME_SCRIPT + `
    window.__mockState.consoleEvalResults = {
      '(+ 1 2)': { value: '3', isError: false },
      'invalid-syntax': { value: 'Error: invalid syntax', isError: true },
      'JS: 1 + 2': { value: '3', isError: false },
    };

    // Patch inspectedWindow.eval to also handle __schemeDebug.eval(...)
    const _origEval = window.chrome.devtools.inspectedWindow.eval;
    window.chrome.devtools.inspectedWindow.eval = function(expression, callback) {
      // Match JSON.stringify(__schemeDebug.eval("...", N))
      const evalMatch = expression.match(/__schemeDebug\\.eval\\(("[^"]*"|'[^']*'),/);
      if (evalMatch) {
        // Extract the expression string (remove surrounding quotes)
        const exprStr = evalMatch[1].slice(1, -1);
        const entry = window.__mockState.consoleEvalResults[exprStr];
        if (entry) {
          if (entry.isError) {
            setTimeout(() => callback(undefined, { value: entry.value }), 10);
          } else {
            setTimeout(() => callback(JSON.stringify(entry.value), null), 10);
          }
        } else {
          setTimeout(() => callback(JSON.stringify('mocked-eval-result'), null), 10);
        }
        return;
      }
      // Fallback to original dispatcher
      _origEval.call(this, expression, callback);
    };
  `;

  await panelPage.evaluateOnNewDocument(CONSOLE_MOCK_SCRIPT);
  await panelPage.goto(
    `chrome-extension://${extensionId}/panel/panel.html`,
    { waitUntil: 'domcontentloaded' }
  );
  await new Promise(r => setTimeout(r, 1500));

  // Fire a mock pause event so the console's onEvaluate check passes
  // (it requires selectedUnifiedFrame to be non-null)
  await panelPage.evaluate((url) => {
    window.__fireMessage({
      type: 'scheme-debug-paused',
      detail: {
        reason: 'breakpoint',
        source: { filename: url, line: 3, column: 0 },
        stack: [
          { name: '<top-level>', source: { filename: url, line: 10, column: 0 }, tcoCount: 0 },
          { name: 'test-fn', source: { filename: url, line: 3, column: 0 }, tcoCount: 0 },
        ],
      }
    });
  }, INLINE_URL);
  await new Promise(r => setTimeout(r, 500));

  // Click the top frame in the call stack to set selectedUnifiedFrame
  await panelPage.evaluate(() => {
    const frames = document.querySelectorAll('#call-stack-container .call-stack-frame');
    if (frames.length > 0) frames[frames.length - 1].click();
  });
  await new Promise(r => setTimeout(r, 200));

  return panelPage;
}

export async function testConsoleEvaluation(browser, extensionId) {
  console.log('\n--- Test Group: Eval Console Evaluation ---');

  if (!extensionId) {
    assert('Console: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Type an expression and press Enter
  await panelPage.evaluate(() => {
    const input = document.getElementById('console-input');
    input.value = '(+ 1 2)';
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));
  });
  await new Promise(r => setTimeout(r, 500));

  // Check the output
  const output = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#console-output .console-message');
    return Array.from(items).map(el => ({
      text: el.textContent,
      isError: el.classList.contains('error'),
    }));
  });

  assert('Console shows input and output', output.length === 2, `got ${output.length} messages`);
  if (output.length === 2) {
    assert('Input is shown correctly', output[0].text.includes('(+ 1 2)'), `got: "${output[0].text}"`);
    assert('Output is shown correctly', output[1].text.includes('3'), `got: "${output[1].text}"`);
    assert('Output is not an error', output[1].isError === false);
  }

  // Type an error-producing expression
  await panelPage.evaluate(() => {
    const input = document.getElementById('console-input');
    input.value = 'invalid-syntax';
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));
  });
  await new Promise(r => setTimeout(r, 500));

  const output2 = await panelPage.evaluate(() => {
    const items = document.querySelectorAll('#console-output .console-message');
    return Array.from(items).map(el => ({
      text: el.textContent,
      isError: el.classList.contains('error'),
    }));
  });

  assert('Console shows error output', output2.length === 4, `got ${output2.length} messages`);
  if (output2.length === 4) {
    assert('Error output is marked as error', output2[3].isError === true);
    assert('Error message is shown', output2[3].text.includes('Error: invalid syntax'), `got: "${output2[3].text}"`);
  }

  await panelPage.close();
}

export async function testConsoleHistory(browser, extensionId) {
  console.log('\n--- Test Group: Eval Console History ---');

  if (!extensionId) {
    assert('Console history: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Type two expressions
  await panelPage.evaluate(() => {
    const input = document.getElementById('console-input');
    input.value = 'expr1';
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));

    setTimeout(() => {
      input.value = 'expr2';
      input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));
    }, 100);
  });
  await new Promise(r => setTimeout(r, 1000));

  // Press ArrowUp twice
  await panelPage.evaluate(() => {
    const input = document.getElementById('console-input');
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowUp' }));
  });

  let inputValue = await panelPage.evaluate(() => document.getElementById('console-input').value);
  assert('ArrowUp once shows last expression', inputValue === 'expr2', `got: "${inputValue}"`);

  await panelPage.evaluate(() => {
    const input = document.getElementById('console-input');
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowUp' }));
  });

  inputValue = await panelPage.evaluate(() => document.getElementById('console-input').value);
  assert('ArrowUp twice shows previous expression', inputValue === 'expr1', `got: "${inputValue}"`);

  // Press ArrowDown
  await panelPage.evaluate(() => {
    const input = document.getElementById('console-input');
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown' }));
  });

  inputValue = await panelPage.evaluate(() => document.getElementById('console-input').value);
  assert('ArrowDown goes forward in history', inputValue === 'expr2', `got: "${inputValue}"`);

  await panelPage.close();
}

export async function testConsoleClear(browser, extensionId) {
  console.log('\n--- Test Group: Eval Console Clear ---');

  if (!extensionId) {
    assert('Console clear: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openMockedPanel(browser, extensionId);

  // Type an expression
  await panelPage.evaluate(() => {
    const input = document.getElementById('console-input');
    input.value = '(+ 1 2)';
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));
  });
  await new Promise(r => setTimeout(r, 500));

  // Check output exists
  let numMessages = await panelPage.evaluate(() => document.querySelectorAll('#console-output .console-message').length);
  assert('Messages exist before clear', numMessages > 0, `got ${numMessages}`);

  // Click clear button
  await panelPage.evaluate(() => {
    const clearBtn = document.querySelector('#console-clear');
    if (clearBtn) clearBtn.click();
  });
  await new Promise(r => setTimeout(r, 200));

  // Check output is empty
  numMessages = await panelPage.evaluate(() => document.querySelectorAll('#console-output .console-message').length);
  assert('Messages cleared', numMessages === 0, `got ${numMessages}`);

  await panelPage.close();
}
