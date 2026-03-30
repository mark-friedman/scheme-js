# Standalone Debugger Window + Sync-Path Breakpoints

## Context

When a Scheme closure runs from a JS DOM callback (e.g., button click), it executes on the synchronous `run()` path. The probe fires `debugger;` which pauses V8. Two problems exist:

1. **UX**: Chrome auto-switches to the Sources tab, hiding our DevTools panel. This is unfixable within DevTools — it's intentional Chrome behavior for any V8 pause.
2. **Functional**: The panel currently auto-resumes V8 after a snapshot, so REPL evaluation, stepping, and variable inspection operate on stale state.

**Solution**: Move the debugger UI from a DevTools panel to a **standalone browser window** (`chrome.windows.create()`). This window is independent of DevTools — no Sources tab switch affects it. Combined with CDP-routed sync-path debugging (keep V8 paused, communicate via `evaluateOnCallFrame`), this gives full debugging for both async and sync execution paths.

**Design lineage**: The original probe-based design (`docs/chrome_devtools_debugger_design.md`) was correct for a Sources-panel sidebar. The panel redesign (`docs/chrome-devtools-panel-design.md`) moved to a standalone panel but assumed all execution could become cooperative/async — impossible for DOM callbacks since the browser calls them synchronously. The previous CDP-only plan is preserved at `docs/in-progress/sync-path-cdp-plan.md`.

## Architecture Change Summary

| Aspect | Before (DevTools panel) | After (Standalone window) |
|--------|------------------------|---------------------------|
| Entry point | `chrome.devtools.panels.create()` | `chrome.windows.create()` via toolbar icon |
| Page eval | `inspectedWindow.eval()` | `chrome.scripting.executeScript({world:'MAIN'})` |
| Tab ID | `chrome.devtools.inspectedWindow.tabId` | URL parameter `?tabId=N` |
| Navigation | `chrome.devtools.network.onNavigated` | `chrome.tabs.onUpdated` |
| Theme | `chrome.devtools.panels.themeName` | `prefers-color-scheme` media query |
| Sync breakpoint UX | Sources tab steals focus | Window unaffected |
| "Being debugged" banner | Only for JS debugging | Same (CDP attached lazily) |

## Phase 0: Write Failing Tests First

### New test file: `tests/extension/test_standalone_window.mjs`

Tests that verify the standalone window architecture. These should **fail** before implementation:

1. **testWindowTabIdFromURL** — Panel opened with `?tabId=42` correctly reads the tabId and uses it for `chrome.scripting.executeScript` calls (not `inspectedWindow.eval`).
2. **testScriptingExecuteScriptLoadsSources** — Mock `chrome.scripting.executeScript` returns source data. Verify source list populates.
3. **testNavigationViaTabsOnUpdated** — Fire `chrome.tabs.onUpdated` with `{status:'complete'}`. Verify sources re-queried.
4. **testPanelWorksWithoutDevtoolsAPIs** — Panel with NO `chrome.devtools` property initializes, loads sources, handles pause/resume.
5. **testSyncPausedShowsInStandalonePanel** — Fire `scheme-sync-paused` message to panel. Verify paused state, stack, variables, and source shown correctly.
6. **testSyncPausedEvalViaEvaluateOnCallFrame** — During scheme-sync pause, REPL evaluation routes through `evalWhilePaused` (CDP), not `inspectedWindow.eval`.

### Update mock infrastructure: `tests/extension/test_mock_chrome.mjs`

Add `buildStandaloneWindowMockScript(options)` — generates mock chrome object WITHOUT `chrome.devtools`, WITH:
- `chrome.scripting.executeScript` — dispatches to same eval logic as existing mock
- `chrome.tabs.onUpdated.addListener`
- `chrome.runtime.onMessage` / `sendMessage`

### Register in `tests/extension/run_extension_tests.mjs`

Add imports and entries for the new test module.

## Phase 1: Replace `inspectedWindow.eval()` → `chrome.scripting.executeScript()`

### `extension/panel-src/protocol/scheme-bridge.js`

Add tabId management:
```js
let _tabId = null;
export function setTabId(id) { _tabId = id; }
export function getTabId() { return _tabId; }
```

Replace `evalInPage()`:
```js
export async function evalInPage(expression) {
  if (_tabId === null) throw new Error('tabId not set');
  const results = await chrome.scripting.executeScript({
    target: { tabId: _tabId },
    func: (expr) => eval(expr),
    args: [expression],
    world: 'MAIN'
  });
  if (!results || results.length === 0) throw new Error('No script result');
  if (results[0].error) throw new Error(results[0].error.message || 'eval error');
  return results[0].result;
}
```

All downstream functions (`getSources`, `getStack`, `activate`, etc.) unchanged — they all call `evalInPage`.

### `extension/panel-src/protocol/cdp-bridge.js`

Replace local `getTabId()` (lines 50-55) with import from scheme-bridge:
```js
import { getTabId } from './scheme-bridge.js';
```

Remove local `tabId` variable and `getTabId()` function.

## Phase 2: Replace DevTools-specific APIs in `main.js`

### TabId from URL parameter
```js
import { setTabId } from './protocol/scheme-bridge.js';
const params = new URLSearchParams(window.location.search);
const tabId = parseInt(params.get('tabId'), 10);
if (!isNaN(tabId)) setTabId(tabId);
```

### Navigation detection (replace lines 704-711)
```js
if (typeof chrome !== 'undefined' && chrome.tabs?.onUpdated) {
  chrome.tabs.onUpdated.addListener((updatedTabId, changeInfo) => {
    if (updatedTabId === tabId && changeInfo.status === 'complete') {
      cdpBridge.clearNavigationCache();
      setTimeout(() => activateAndRefresh(), 500);
    }
  });
}
```

### Theme detection (replace lines 750-766)
```js
const darkQuery = window.matchMedia('(prefers-color-scheme: dark)');
const updateTheme = (isDark) => {
  document.documentElement.classList.toggle('theme-dark', isDark);
  document.documentElement.classList.toggle('theme-light', !isDark);
};
updateTheme(darkQuery.matches);
darkQuery.addEventListener('change', (e) => updateTheme(e.matches));
```

### Focus refresh (replace guard at line 716)
Remove `chrome.devtools?.panels` guard — keep the focus refresh unconditionally.

### Message filtering
Add tabId filter to `chrome.runtime.onMessage` handler so multiple debugger windows don't cross-talk:
```js
if (message.tabId && message.tabId !== tabId) return;
```

## Phase 3: Window Lifecycle in `background.js`

### Window management
```js
const debuggerWindows = new Map(); // tabId → windowId

async function openDebuggerWindow(tabId) {
  if (debuggerWindows.has(tabId)) {
    try { await chrome.windows.update(debuggerWindows.get(tabId), { focused: true }); return; }
    catch { debuggerWindows.delete(tabId); }
  }
  const win = await chrome.windows.create({
    url: `panel/panel.html?tabId=${tabId}`, type: 'popup', width: 900, height: 700
  });
  debuggerWindows.set(tabId, win.id);
}
```

### Toolbar icon trigger
```js
chrome.action.onClicked.addListener(async (tab) => {
  await openDebuggerWindow(tab.id);
});
```

### Cleanup
- `chrome.windows.onRemoved`: delete from map, optionally detach CDP
- `chrome.tabs.onRemoved`: close associated debugger window

## Phase 4: Manifest Changes

### `extension/manifest.json`
- **Remove**: `"devtools_page": "devtools.html"`
- **Add**: `"scripting"` and `"tabs"` to permissions
- **Add**: `"action"` block with icon and title
- **Keep**: `"debugger"` permission, `"<all_urls>"` host permissions, content scripts

### Delete: `extension/devtools.html`, `extension/devtools.js`

## Phase 5: Sync-Path CDP-Routed Debugging

### `extension/background.js` — Keep V8 paused
In the sync-path branch of `Debugger.paused` handler (lines ~211-228):
- **Remove** the `Debugger.resume` call (auto-resume)
- Keep: fetch Scheme stack via `evaluateWhilePaused`, send `scheme-sync-paused` to panel
- V8 stays paused until panel sends `resume-debugger` or `scheme-step-*`

### `extension/panel-src/protocol/cdp-bridge.js` — New functions
```js
export async function evalWhilePaused(expression) { ... } // sends eval-paused
export async function schemeStepInto() { ... }            // sends scheme-step-into
export async function schemeStepOver() { ... }            // sends scheme-step-over
export async function schemeStepOut() { ... }             // sends scheme-step-out
```
These use existing message handlers in background.js (lines 347-398).

### `extension/panel-src/protocol/unified-debugger.js` — Route scheme-sync through CDP
- `resume()`: scheme-sync → `cdpBridge.resumeCDP()`
- `stepInto/Over/Out()`: scheme-sync → `cdpBridge.schemeStepInto/Over/Out()`
- `evalInFrame()`: scheme-sync → `cdpBridge.evalWhilePaused(__schemeDebug.eval(...))`
- New `getLocalsForContext(frameIndex)`: scheme-sync → CDP, else → scheme-bridge
- New `getSourceContentForContext(url)`: same pattern
- New `getExpressionsForContext(url)`: same pattern

### `extension/panel-src/main.js` — Context-aware routing
- `onPaused()`: when scheme-sync, load source and locals via unified-debugger context-aware functions
- `onSelectFrame()`: same routing
- Add `loadSourceWhilePaused(url)` helper

## Phase 6: Update Test Infrastructure

### Update `tests/extension/test_mock_chrome.mjs`
All existing mock chrome scripts use `chrome.devtools.inspectedWindow.eval`. Update/add a variant that uses `chrome.scripting.executeScript` with the same eval dispatcher logic.

### Update panel test files
These files open the panel with mocked chrome APIs and must use the new mock:
- `test_panel_interactions.mjs`
- `test_console.mjs`
- `test_js_debugging.mjs`
- `test_auto_resume.mjs`
- `test_error_paths.mjs`
- `test_panel_e2e_breakpoint.mjs`

### Update `test_harness.mjs`
Panel URL: `chrome-extension://${extensionId}/panel/panel.html?tabId=${tabId}`

## Phase 7: Build & Verify

1. `npm run build:panel`
2. `npm test` — unit tests pass
3. `npm run test:extension` — all E2E tests pass (with updated mocks)
4. Manual test with `scheme-blocks/dist`:
   - Click extension icon → debugger window opens
   - Set breakpoint in `handle-click` in app.scm
   - Click "Generate Code" button
   - Debugger window shows "Paused at breakpoint" (no Sources tab switch)
   - Variables pane shows correct locals
   - REPL evaluation returns correct values
   - Step Into advances to next expression
   - Resume continues execution
   - Close debugger window → clean cleanup
   - Close page tab → debugger window closes

## Files Modified

| File | Change |
|------|--------|
| `extension/manifest.json` | Remove devtools_page, add scripting/tabs/action |
| `extension/devtools.html` | Delete |
| `extension/devtools.js` | Delete |
| `extension/background.js` | Add window lifecycle, remove sync-path auto-resume |
| `extension/panel-src/protocol/scheme-bridge.js` | Replace inspectedWindow.eval with scripting.executeScript |
| `extension/panel-src/protocol/cdp-bridge.js` | Import shared getTabId, add evalWhilePaused + step functions |
| `extension/panel-src/protocol/unified-debugger.js` | Route scheme-sync through CDP, add context-aware getters |
| `extension/panel-src/main.js` | TabId from URL, tabs.onUpdated, theme via media query, context-aware routing |
| `extension/panel/panel.js` | Rebuilt bundle |
| `tests/extension/test_standalone_window.mjs` | New test file |
| `tests/extension/test_mock_chrome.mjs` | Add standalone window mock |
| `tests/extension/run_extension_tests.mjs` | Register new tests |
| Multiple test files | Update mock chrome scripts |

## Flow After Fix

```
Async path (initial execution, REPL):
  Scheme runDebug() → cooperative pause → content_script relay
  → panel (standalone window): scripting.executeScript for getLocals/loadSource
  → No debugger; needed, no Sources tab switch, no banner

Sync path (DOM callbacks):
  Button click → Scheme run() [_inSyncPath=true] → breakpoint → debugger;
  → V8 pauses
  → background.js: detect sync probe, fetch stack via evaluateOnCallFrame
  → background.js: send scheme-sync-paused to panel window (V8 stays paused)
  → panel window: show stack, load source via CDP, show variables via CDP
  → User clicks Step/Resume/types in REPL
  → panel → unified-debugger → cdpBridge → background.js
  → background.js: evaluate command via evaluateOnCallFrame, then Debugger.resume
  → V8 resumes → next breakpoint/step → cycle repeats
  → Debugger window stays focused throughout (not affected by Sources tab)
```
