# Cross-Cutting Debugging Flows

This document describes three flows that span multiple files across the debug
runtime (`src/debug/`), DevTools integration (`src/debug/devtools/`), and the
extension panel (`extension/panel-src/`). Each section traces data from origin
to destination, listing every file handoff.

---

## 1. Breakpoint Lifecycle

### Setting a breakpoint (gutter click → interpreter)

```
editor.js: mousedown on .cm-breakpoint-gutter
  → dispatches toggleBreakpointEffect(line)
  → breakpointField StateField toggles Set<line>
  → calls onBreakpointToggle(line, isNowSet)      [callback wired in main.js]

main.js: onBreakpointToggle(line, isNowSet)
  → unifiedDebugger.setBreakpoint(url, line, column)
  → bpState.set(url, line, column, id)             [breakpoint-state.js]
  → bpState.saveToPage()                           [persists to localStorage]
  → refreshBreakpointsPanel()

unified-debugger.js: setBreakpoint(url, line, column)
  → isJSFile(url)? → cdpBridge.setJSBreakpoint()
  → else           → schemeBridge.setBreakpoint(url, line, column)

scheme-bridge.js: setBreakpoint(url, line, column)
  → evalInPage('__schemeDebug.setBreakpoint("url", line, column)')

devtools_debug.js: __schemeDebug.setBreakpoint(url, line, col)
  → debugRuntime.setBreakpoint(url, line, col)

scheme_debug_runtime.js: setBreakpoint(filename, line, column)
  → breakpointManager.setBreakpoint(filename, line, column)

breakpoint_manager.js: setBreakpoint(filename, line, column)
  → creates Breakpoint object with unique ID
  → stores in breakpoints Map
  → indexes in locationIndex["filename:line"] → Set<bpId>
  → returns breakpoint ID string
```

### Hitting a breakpoint (execution → pause → panel)

```
interpreter.js: trampoline loop, before evaluating expr
  → debugRuntime.shouldPause(source, env)

scheme_debug_runtime.js: shouldPause(source, env)
  → breakpointManager.hasBreakpoint(source)
  → locationIndex["filename:line"] lookup (O(1))
  → if column-level BP: checks column match
  → returns true → interpreter calls handlePause()

scheme_debug_runtime.js: handlePause(source, env, 'breakpoint')
  → pauseController.pause(reason, bpId)
  → fires onPause callback

devtools_debug.js: onPause handler
  → window.postMessage({type: 'scheme-debug-paused', detail})
  → also dispatches CustomEvent (dual-channel)

content_script.js: listens on 'message' (postMessage, not CustomEvent)
  → chrome.runtime.sendMessage({type: 'scheme-debug-paused', detail})

panel main.js: chrome.runtime.onMessage listener
  → unifiedDebugger.handleSchemePause(detail)
  → toolbar.setStatus('Paused at breakpoint')
  → editor.highlightLine(line)
  → schemeBridge.ackPause()                    [cancels safety timeout]
```

### Breakpoint state storage

The panel maintains a parallel Map in `breakpoint-state.js` using structured
JSON keys (`JSON.stringify([url, line, column])`) to avoid the fragility of
colon-delimited URL strings. This Map is synced to `localStorage` via
`saveToPage()` so that `activate_debug.js` can restore breakpoints before
scripts run on the next page load.

---

## 2. Probe Mechanism

### Registration: Scheme source → probe script

```
html_adapter.js: runScripts()
  → parse(code, {filename: "scheme://inline-scripts/script-0.scm"})
  → expressions = [{car: ..., source: {filename, line, column, endLine, endColumn}}, ...]

sourceRegistry.register(url, code, origin, expressions)
  → _extractSourceSpans(expressions, url)
     assigns unique exprId to each expression location
     stores in expressionSpans Map<url, span[]>
     stores in locationToExprId Map<"url:line:col", exprId>

  → generateProbeScript(url, code, spans)
     emits one JS function per exprId:
       probes[42] = function __scheme_E42(envProxy) {
         with (envProxy) { if (__schemeProbeRuntime.hit(42)) { debugger; } }
       };
     appends inline base64 source map (maps JS positions → Scheme locations)

  → _installProbeScript(url, probeScript)
     injects <script> tag (browser) or eval() (Node.js)
     probe functions register in globalThis.__schemeProbeRegistry
```

### Execution: trampoline → probe → V8 pause

```
interpreter.js: trampoline, before eval(expr)
  → devtoolsDebug.maybeHit(source, env)

devtools_debug.js: maybeHit(source, env)
  → dedup check: key === lastHitKey && env === lastHitEnv? → skip
  → exprId = sourceRegistry.getExprId(url, line, column)
  → probe = sourceRegistry.getProbe(url, exprId)
  → envProxy = createEnvProxy(env)          [env_proxy.js]
  → fireProbe(probe, envProxy)

probe function executes:
  with (envProxy) {
    if (__schemeProbeRuntime.hit(exprId)) {
      debugger;                              // V8 pauses here
    }
  }

probe_runtime.js: hit(exprId)
  → if _breakpoints.has(exprId) → return true
  → if _stepping:
      _shouldStopForStep(exprId):
        'into'  → always true
        'over'  → currentDepth <= _stepStartDepth
        'out'   → currentDepth < _stepStartDepth
  → else → return false
```

### Source maps

Each probe script includes an inline base64 V3 source map that maps the
`debugger;` statement back to the original Scheme expression location. When
V8 pauses, Chrome DevTools resolves the source map and shows the Scheme
source file instead of the generated probe JavaScript.

The source URL uses the `scheme-probe://` protocol prefix to distinguish
probe scripts from original sources in the DevTools Sources panel.

### Deduplication

`maybeHit()` tracks `lastHitKey` (filename:line:column) and `lastHitEnv`
to avoid firing redundant probes for sub-expressions within the same
top-level expression being evaluated in the same environment. This reduces
probe overhead significantly for deeply nested expressions.

---

## 3. panelConnected Protocol

### Problem

The Scheme interpreter runs in the page's MAIN world. The DevTools panel
runs in the extension context. When the interpreter hits a breakpoint, it
needs to pause cooperatively (using async/await, not freezing the browser).
But it should only pause if the panel is actually listening — otherwise the
page hangs for 30 seconds until the safety timeout fires.

### Activation sequence (panel opened on a page)

```
Panel loads → main.js
  → activateAndRefresh()
  → schemeBridge.activate()
  → evalInPage("__schemeDebug.activate()")

devtools_debug.js: __schemeDebug.activate()
  → globalThis.__SCHEME_JS_DEBUG = true
  → debugRuntime.panelConnected = true
  → sessionStorage.setItem('schemeJS_panelConnected', 'true')
  → returns {active: true/false, needsReload: boolean}

Panel receives response:
  → if needsReload: show "Reload page to enable debugging"
  → else: bpState.syncFromInterpreter() + sourceList.refresh()
```

### Persistence across page reload

When the user reloads the page while the panel is open, breakpoints and the
panelConnected flag must be restored *before* any Scheme scripts execute.
This is achieved through a three-stage cascade:

```
Stage 1: activate_debug.js (content script, runs at document_start)
  → reads sessionStorage.schemeJS_panelConnected
    → sets globalThis.__SCHEME_JS_PANELCONNECTED = true
  → reads localStorage.schemeJS_breakpoints
    → sets globalThis.__SCHEME_JS_BREAKPOINTS = [{url, line, column}, ...]

Stage 2: html_adapter.js (runs when <script type="text/scheme"> is parsed)
  → isDevToolsDebugRequested() checks __SCHEME_JS_DEBUG
  → creates SchemeDebugRuntime + DevToolsDebugIntegration
  → reads __SCHEME_JS_PANELCONNECTED → debugRuntime.panelConnected = true
  → reads __SCHEME_JS_BREAKPOINTS → restores breakpoints via setBreakpoint()

Stage 3: Panel (re-initialized on new page)
  → chrome.devtools.network.onNavigated fires
  → activateAndRefresh() runs again
  → syncs with interpreter
```

### Pause blocking behavior

```
scheme_debug_runtime.js: handlePause(source, env, reason)

  panelConnected === false:
    → starts 30s safety timeout
    → auto-resumes when timeout fires
    → execution continues (no hang)

  panelConnected === true:
    → starts 5s safety timeout
    → fires onPause callback → content_script → panel
    → blocks on pauseController.waitForResume()

  Panel receives pause:
    → immediately calls ackPause()
    → ackPause() cancels safety timeout
    → pause now waits indefinitely for user action

  User clicks Resume/Step:
    → unifiedDebugger.resume() / stepInto() / etc.
    → evalInPage("__schemeDebug.resume()")
    → pauseController.resume() resolves the awaited Promise
    → interpreter continues
```

### Cross-world relay

Chrome extensions' content scripts run in an ISOLATED world. `CustomEvent.detail`
is `null` when read from a different world (Chrome limitation). The relay uses
`postMessage` instead:

```
Page (MAIN world):
  devtools_debug.js → window.postMessage({type: 'scheme-debug-paused', detail})

Content script (ISOLATED world):
  window.addEventListener('message', handler)
  → chrome.runtime.sendMessage(msg)

Panel (extension context):
  chrome.runtime.onMessage.addListener(handler)
  → unifiedDebugger.handleSchemePause(detail)
```

Both `CustomEvent` and `postMessage` are dispatched for compatibility, but
the content script relies only on `postMessage`.

---

## Key Flags Summary

| Flag | Storage | Set by | Read by | Purpose |
|------|---------|--------|---------|---------|
| `__SCHEME_JS_DEBUG` | global | activate_debug.js, activate() | html_adapter.js | Enable debug runtime |
| `__SCHEME_JS_PANELCONNECTED` | global | activate_debug.js | html_adapter.js | Pre-set panelConnected |
| `schemeJS_panelConnected` | sessionStorage | activate() API | activate_debug.js | Persist across reload |
| `schemeJS_breakpoints` | localStorage | breakpoint-state.js | activate_debug.js | Persist breakpoints |
| `debugRuntime.panelConnected` | runtime | html_adapter.js, activate() | handlePause() | Control pause blocking |
