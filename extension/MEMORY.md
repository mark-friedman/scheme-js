# Scheme-JS Extension Memory

## Project Overview
scheme-js-4: A Scheme interpreter in JavaScript targeting R7RS-small standard.
Chrome extension with standalone debugger window for debugging Scheme in the browser.

## Architecture — Standalone Window
- Extension opens a standalone popup window via `chrome.windows.create()` (not a DevTools panel)
- Triggered by clicking the toolbar icon (`chrome.action.onClicked`)
- Panel URL: `panel/panel.html?tabId=N` — tabId passed as URL parameter
- Page eval via `chrome.scripting.executeScript({world:'MAIN'})` (not `inspectedWindow.eval`)
- Navigation detection via `chrome.tabs.onUpdated` (not `chrome.devtools.network.onNavigated`)
- Theme detection via `prefers-color-scheme` media query (not `chrome.devtools.panels.themeName`)
- `devtools.js` and `devtools.html` have been removed
- Manifest permissions: `debugger`, `scripting`, `tabs` + `action` block

## Key Files

### Background
- `background.js` — Service worker: CDP event routing, window lifecycle (`debuggerWindows` map), sync-path pause handling (keeps V8 paused). Handler functions extracted: `handleCDPCommand`, `handleSchemeStepCommand`, `handleEvalInJSFrame`, `handleSetBoundaryBreakpoint`.

### Panel Source (`panel-src/`)
- `main.js` — Entry point: init + wire components. Uses helpers: `formatJSLocals`, `computeSchemeFrameIndex`, `persistBreakpointChange`.
- `breakpoint-state.js` — Breakpoint state management (JSON-keyed Map)
- `splitter.js` — Generic drag-to-resize splitter

### Protocol (`panel-src/protocol/`)
- `constants.js` — Frozen `MSG` and `PAUSE_CONTEXT` enums shared across all protocol files
- `scheme-bridge.js` — Dual-path eval (scripting.executeScript or inspectedWindow.eval fallback). Uses `evalAndParse()` helper internally.
- `cdp-bridge.js` — CDP bridge: attach/detach, step, resume, evalWhilePaused, schemeStepInto/Over/Out, JS breakpoints, boundary breakpoints. 10s timeout on in-progress attachment polling.
- `unified-debugger.js` — Routes commands to correct bridge based on pause context (scheme/js/scheme-sync), context-aware getters

### Components (`panel-src/components/`)
- `editor.js` — CodeMirror 6 viewer (breakpoint gutter, line highlight, expression highlight, diamond markers)
- `toolbar.js` — Debug controls: Resume, Step Into/Over/Out + status
- `call-stack.js` — Call stack panel (Scheme + JS frames, TCO-aware)
- `variables.js` — Variables panel (Local/Closure/Global scope grouping)
- `breakpoints.js` — Breakpoints list panel
- `console.js` — REPL eval console + page console output
- `source-list.js` — Source file browser

### Build
- `build/build-panel.js` — esbuild script; outputs `panel/panel.js`
- `npm run build:panel` — builds the panel bundle

## Three Pause Contexts
1. **scheme** — Cooperative async pause (runDebug path). Commands go through scheme-bridge.
2. **js** — CDP-level JS breakpoint/exception. Commands go through cdp-bridge.
3. **scheme-sync** — Sync DOM callback probe pause. V8 stays paused at `debugger;`. Commands go through CDP (evalWhilePaused, schemeStepInto/Over/Out). Context-aware getters route getLocals/getSourceContent/getExpressions through CDP evaluateOnCallFrame.

## Auto-Resume (background.js)
- Scheme probe pauses (`debugger;` in generated probe functions) are auto-resumed by `background.js` via CDP `Debugger.resume`, so Chrome's Sources tab never stays paused on Scheme code
- `isSchemeProbe(params)` — checks top 5 call frames for `__scheme_E*` function name
- Non-Scheme pauses (real JS breakpoints) are forwarded to the panel as `cdp-paused`
- Sync-path probes are NOT auto-resumed — V8 stays paused so CDP eval works

## `_panelConnected` Flag
- `__schemeProbeRuntime._panelConnected` (default false) — when true, `hit()` returns false immediately
  - Prevents `debugger;` from firing when the panel is connected (would block cooperative channel)
  - Set in `html_adapter.js` from `__SCHEME_JS_PANELCONNECTED` (before scripts run)
  - Set in `devtools_debug.js` `activate()` when panel connects to a live page
  - `activate_debug.js` reads `sessionStorage.schemeJS_panelConnected` at document_start

## Cross-World Relay
- CustomEvent.detail is null when read from ISOLATED world (Chrome limitation)
- `devtools_debug.js` dispatches BOTH CustomEvent + window.postMessage on pause/resume
- `content_script.js` listens on 'message' event (postMessage), not CustomEvent
- Tests that use onPause with stepInto/abort MUST set panelConnected=true

## Testing
- `npm run test:extension` — 394 Puppeteer E2E tests (headed Chrome with `--load-extension`)
- Must use Puppeteer's Chrome for Testing (CfT) — omit `executablePath`
- Extension ID is discovered dynamically from service worker target URL
- Panel UI tested by navigating directly to `chrome-extension://<id>/panel/panel.html?tabId=42`
- Coordinate-based gutter clicks required for CodeMirror 6 breakpoint gutter (`page.mouse.click(x, y)`)
- Page leak cleanup in test runner prevents flaky timeouts from accumulated tabs
