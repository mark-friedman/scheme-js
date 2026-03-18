# Fix Breakpoint Pausing in Standard Sources Tab Instead of Scheme-JS Panel

When a Scheme breakpoint is hit, the `debugger;` statement in the generated probe function causes Chrome's built-in Sources panel to intercept the pause because `background.js` has already attached CDP's `Debugger.enable`. The Scheme-JS extension tab doesn't show it as paused because it relies on the cooperative `scheme-debug-paused` message from the content script, which is a separate channel.

The fix has two parts:
1. **Auto-resume Scheme probe pauses in `background.js`** — when a `Debugger.paused` event is identified as a Scheme probe (via `isSchemeProbe()`), automatically `Debugger.resume` via CDP so the Sources tab never stays paused. The cooperative Scheme pause mechanism (via `scheme-debug-paused` from content script) will still work independently to show the pause in the Scheme-JS panel.
2. **Remove dead sidebar code** — the old `panel/sidebar.html`, `panel/sidebar.js`, and `panel/sidebar.css` files are unused orphans that should be cleaned up.

## Proposed Changes

### Extension Core

#### [MODIFY] [background.js](file:///Users/mark/code/scheme-js-4/extension/background.js)

In the `Debugger.paused` handler (line 191-216):
- When `isSchemeProbe(params)` or `isSchemeException(params)` is true, **immediately resume CDP** via `chrome.debugger.sendCommand(tabId, 'Debugger.resume')` so the Sources tab doesn't stay paused
- Still send `debugger-paused` to the panel for any listeners that rely on it
- Update the file-level JSDoc to reflect the current architecture (no more sidebar references)

#### [DELETE] [sidebar.html](file:///Users/mark/code/scheme-js-4/extension/panel/sidebar.html)

#### [DELETE] [sidebar.js](file:///Users/mark/code/scheme-js-4/extension/panel/sidebar.js)

#### [DELETE] [sidebar.css](file:///Users/mark/code/scheme-js-4/extension/panel/sidebar.css)

---

### Tests

#### [NEW] [test_auto_resume.mjs](file:///Users/mark/code/scheme-js-4/tests/extension/test_auto_resume.mjs)

Three tiers of Puppeteer tests:

**1. Mock-based panel tests** (using `buildMockChromeScript`, `openMockedPanel`, `firePauseEvent`):
- `testSchemeProbeAutoResumed` — Simulates a `debugger-paused` message with Scheme probe frames. Verifies the panel correctly handles it via the `scheme-debug-paused` content script relay (cooperative channel) and shows paused state in the UI.
- `testNonSchemeProbeNotAutoResumed` — Simulates a regular JS `cdp-paused` event. Verifies it shows as a JS pause in the panel, not auto-resumed.

**2. Real page-side tests** (no mocks, using actual Scheme code with breakpoints via `__schemeDebug` API):
- `testBreakpointPausesCooperatively` — Sets a breakpoint, triggers it, verifies `getStatus().state === 'paused'` via the cooperative channel.
- `testBreakpointResumeAndContinue` — Sets a breakpoint, pauses, resumes, verifies execution completes. Ensures auto-resume doesn't interfere with cooperative pause/resume.
- `testSteppingWorksAfterAutoResume` — Sets a breakpoint, pauses, steps into/over, verifies stepping works correctly.

**3. Click-based UI simulation tests** (using `openMockedPanel`, Puppeteer mouse clicks, DOM verification):
- `testPanelPauseShowsUIViaCooperativeChannel` — Opens the panel, fires a `scheme-debug-paused` event (simulating the content script relay), then verifies: toolbar shows "Paused at breakpoint", call stack frames render, variables populate, editor highlights the current line. Clicks a call stack frame and verifies variables update. This simulates the real user experience of seeing the debugger panel when paused.
- `testPanelResumeClickAfterCooperativePause` — Fires a cooperative pause, clicks the Resume button in the toolbar via Puppeteer, verifies the mock `resume()` was called, and verifies the UI clears (call stack empty, variables cleared, current line highlight removed).
- `testPanelStepButtonsAfterCooperativePause` — Fires a cooperative pause, clicks Step Into/Over/Out buttons, verifies the mock step functions are called. Re-pauses between each step to re-enable buttons.
- `testPanelBreakpointSetViaGutterClick` — Opens the panel, clicks the breakpoint gutter to set a breakpoint, verifies a breakpoint dot appears and `setBreakpoint` was called on the mock. Then clicks again to remove it and verifies removal.

#### [MODIFY] [run_extension_tests.mjs](file:///Users/mark/code/scheme-js-4/tests/extension/run_extension_tests.mjs)

Register the new `test_auto_resume.mjs` tests in the test runner.

---

### Documentation

#### [MODIFY] [MEMORY.md](file:///Users/mark/code/scheme-js-4/extension/MEMORY.md)

Remove references to sidebar files. Note the auto-resume behavior for Scheme probe pauses.

## Verification Plan

### Automated Tests
- `npm run test:extension` — all existing + new auto-resume tests must pass
- `npm test` — all core tests must pass

### Manual Verification
- User tests with the scheme-blocks app to confirm breakpoints pause in the Scheme-JS panel, not the Sources tab
