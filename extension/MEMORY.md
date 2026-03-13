# Scheme-JS Project Memory

## Project Overview
scheme-js-4: A Scheme interpreter in JavaScript targeting R7RS-small standard.
Chrome DevTools extension for debugging Scheme in the browser.

## Key Architecture
- `src/core/interpreter/` — Trampoline-based interpreter, reader, analyzer
- `src/core/primitives/` — Native JS procedures (marked with `SCHEME_PRIMITIVE`)
- `src/core/scheme/` — R7RS libraries in .sld/.scm files
- `src/debug/` — Debug runtime (BreakpointManager, StackTracer, PauseController, StateInspector)
- `src/debug/devtools/` — Chrome DevTools integration (probe-based, being replaced with cooperative pausing)
- `src/packaging/` — html_adapter.js, scheme_entry.js, scheme_repl_wc.js
- `extension/` — Chrome extension; `manifest.json`, `devtools.js`, `background.js`
- `tests/` — All tests; run via `npm test`
- `dist/` — Built output (rollup)

## Chrome Extension — Current State
- `extension/devtools.js` — Creates top-level "Scheme-JS" panel via `chrome.devtools.panels.create`
- `extension/panel/panel.html` + `panel.css` + `panel.js` (bundled) — New panel UI
- `extension/panel-src/` — Panel source (main.js, components/, protocol/, language/)
- `extension/panel-src/main.js` — Panel entry point (~575 lines, refactored from 918)
- `extension/panel-src/breakpoint-state.js` — Breakpoint state management (JSON-keyed Map)
- `extension/panel-src/splitter.js` — Generic drag-to-resize splitter
- `extension/panel-src/language/scheme-mode.js` — CodeMirror 6 StreamParser for Scheme
- `extension/panel-src/components/editor.js` — CodeMirror editor (read-only, dark theme)
- `extension/panel-src/components/source-list.js` — Source file list (event delegation)
- `extension/panel-src/components/console.js` — REPL console (history capped at 100)
- `extension/panel-src/protocol/scheme-bridge.js` — inspectedWindow.eval wrapper (with error logging)
- `extension/panel-src/protocol/cdp-bridge.js` — CDP via background.js (with error logging + nav cache clearing)
- `extension/build/build-panel.js` — esbuild script; outputs extension/panel/panel.js
- `npm run build:panel` — builds the panel bundle

## __schemeDebug API (page-side, installed by devtools_debug.js)
- `getStack()`, `getLocals(frameIndex)`, `getSource(frameIndex)`, `eval(code, frameIndex)`
- `getSources()` — returns all registered sources (url, content, lines, origin)
- `getSourceContent(url)` — returns content of a specific source
- `getExpressions(url)` — returns expression spans [{exprId, line, column, endLine, endColumn}]
- `setBreakpoint(url, line, column?)` — column param enables expression-level breakpoints
- `stepInto()`, `stepOver()`, `stepOut()` — delegates to PauseController (cooperative), probe runtime has depth-aware stepping

## Testing
- `npm test` — runs all tests (2415+)
- `npm run test:extension` — Puppeteer E2E tests (258+ tests, extension loaded in headed Chrome)
- `npm run test:extension -- --only <filter>` — Run subset of tests (e.g., `--only console,panel_interactions`)
- Tests must pass in Node.js and browser
- New features need tests in `tests/`
- `tests/extension/run_extension_tests.mjs` — Puppeteer E2E: page-side API + panel UI tests
- `tests/extension/test_mock_chrome.mjs` — Shared mock chrome API infrastructure (composable via `buildMockChromeScript()`)
- `tests/extension/test_error_paths.mjs` — Error path tests (eval timeout, missing API, malformed events)
- `tests/extension/test_harness.mjs` — `waitFor()` and `waitForPage()` for condition-based polling (replaces fixed timeouts)
- Components use `data-testid` attributes for stable test selectors

## Puppeteer Extension Testing — Key Discovery
- System Chrome (145+) does NOT load unpacked extensions via `--load-extension`
- **Must use Puppeteer's Chrome for Testing (CfT)** — omit `executablePath`
- Extension ID is discovered dynamically from service worker target URL
- Panel UI is tested by navigating directly to `chrome-extension://<id>/panel/panel.html`
- Panel target only appears as Puppeteer target AFTER the panel tab is clicked in DevTools
- Extension's `devtools.html` appears as `type=other` target (not `type=page`)

## Key Conventions
- ES Modules throughout (`import`/`export`)
- 2-space indentation
- JSDoc on all JS functions, Scheme-style `;;` on Scheme functions
- All new JS primitives must set `SCHEME_PRIMITIVE` symbol
- Implementations prefer Scheme over JS; isolate minimum JS

## Debugger Roadmap (plan)
- Phase 1 ✓ — Panel shell + CodeMirror (no debugging logic)
- Phase 2 ✓ — Cooperative Scheme breakpoints + pause/resume
- Phase 3 ✓ — Expression-level breakpoints + current expression highlighting
- Phase 4 ✓ — Lazy CDP + JS debugging (cdp-bridge, unified-debugger, JS frames in call stack)
- Phase 5 — Mixed HTML files + boundary stepping
- Phase 6 — Polish (keyboard shortcuts, persist breakpoints, REPL console)

## Phase 4 Key Mechanisms
- `extension/panel-src/protocol/cdp-bridge.js` — Lazy CDP attachment via `chrome.runtime.sendMessage` to background.js
  - Auto-attaches when first JS breakpoint is set via `setJSBreakpoint()`
  - Tracks scriptParsed events for scriptId→URL mapping
  - Provides `stepIntoCDP()`, `stepOverCDP()`, `stepOutCDP()`, `resumeCDP()`
  - `getJSSource(scriptId)` fetches JS source via `Debugger.getScriptSource`
- `extension/panel-src/protocol/unified-debugger.js` — Routes step/resume/breakpoint to correct bridge
  - `isJSFile(url)` — true for .js/.mjs/.ts, false for scheme:// URLs
  - `currentPauseContext` — 'scheme' | 'js' | null
  - `mergeCallStacks()` — interleaves JS frames (from CDP) above Scheme frames
  - `handleSchemePause()` / `handleSchemeResume()` — called by main.js message listener
- `background.js` Phase 4 additions:
  - `cdp-step-into`, `cdp-step-over`, `cdp-step-out` — native V8 step commands
  - `set-js-breakpoint`, `remove-js-breakpoint` — `Debugger.setBreakpointByUrl` / `removeBreakpoint`
  - `get-js-source` — `Debugger.getScriptSource`
  - `cdp-paused` message for non-Scheme pauses, `cdp-script-parsed` forwarding
  - `scriptUrlMaps` — per-tab Map<scriptId, url>
- Call stack: `[JS]` badge (blue) + `[SCM]` badge (green) via `frame.language` field
- Editor: `languageCompartment` switches between Scheme and JavaScript syntax highlighting
- Toolbar: `showCDPNotification()` adds "⚡ JS debugging enabled" when CDP first attaches
- `tests/extension/test_js_debugging.mjs` — 18 Puppeteer E2E tests for Phase 4

## Phase 2 Key Mechanisms
- `panelConnected` flag on SchemeDebugRuntime (default false)
  - handlePause() only blocks when panelConnected=true
  - activate() sets it + saves 'schemeJS_panelConnected'='true' to localStorage
  - activate_debug.js reads localStorage → sets __SCHEME_JS_PANELCONNECTED global
  - html_adapter.js reads __SCHEME_JS_PANELCONNECTED → sets panelConnected before scripts run
- `resumeTimeout = 30000ms` safety: auto-resumes + clears panelConnected if panel goes dark
- panel main.js: calls activateAndRefresh() on load + chrome.devtools.network.onNavigated
- content_script.js uses postMessage (not CustomEvent) for cross-world relay
- Panel polls getStatus() after activation to catch already-paused state
- Panel re-queries sources on resume (catches late-registered scripts)

## Cross-World Relay Fix
- CustomEvent.detail is null when read from ISOLATED world (Chrome limitation)
- devtools_debug.js dispatches BOTH CustomEvent + window.postMessage on pause/resume
- content_script.js listens on 'message' event (postMessage), not CustomEvent
- Tests that use onPause with stepInto/abort MUST set panelConnected=true

## Phase 3 Key Mechanisms
- `SourceRegistry.expressionSpans` — Map<url, span[]> stored during register()
- `getExpressions(url)` on SourceRegistry + __schemeDebug API + scheme-bridge
- Editor: `highlightExpression(line,col,endLine,endCol)` — yellow background on exact sub-expression
- Editor: `setDiamondMarkers(markers)` — Chrome DevTools-style inline ◆/◇ at expression boundaries
- Diamonds appear on lines with line breakpoints + current paused line
- Click diamond → toggles column-level expression breakpoint
- `setExpressionBreakpoints(spans)` — red background highlight on active expression BPs
- Breakpoint key format: `JSON.stringify([url, line, column])` — structured JSON keys (replaces fragile colon-delimited format)
- html_adapter.js passes `column` through __SCHEME_JS_BREAKPOINTS pre-loading

## Phase 2 Test Pages
- tests/debug/diagnostic.html — 8 browser tests, no extension needed
- tests/debug/breakpoint_flow_test.html — end-to-end pause/resume flow, no extension needed
  (sets __SCHEME_JS_PANELCONNECTED + __SCHEME_JS_BREAKPOINTS before scheme-html.js loads)
- tests/debug/postmessage_relay_test.html — verifies dual-channel dispatch (CustomEvent + postMessage)
