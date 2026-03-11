# Custom DevTools Panel for Scheme-JS Debugger

## Context

The previous debugger used a sidebar pane in Chrome's Sources tab (`chrome.devtools.panels.sources.createSidebarPane`), which has several UX problems: the sidebar is buried below other panels, must be manually opened, triggers a scary "started debugging" banner via `chrome.debugger.attach()`, and creates confusion between JS and Scheme debugging controls (two sets of step buttons, scope panels, call stacks).

The goal is to replace this with a **top-level custom DevTools panel** ("Scheme-JS") that owns both Scheme and JavaScript debugging in a unified interface. The panel uses **cooperative pausing** for Scheme (no banner needed) and **lazy CDP attachment** for JS interop (banner only when stepping into JS).

## Architecture

### Communication Model

**Scheme debugging (cooperative, no CDP)**:
```
panel.js --[inspectedWindow.eval]--> __schemeDebug API --> interpreter
```
- Panel opens -> calls `__schemeDebug.activate()` -> interpreter switches to async execution path
- Breakpoints/stepping managed via `PauseController.waitForResume()` (already exists)
- Pause notifications sent via `CustomEvent` on `window`, relayed by a content script to the panel

**JS debugging (CDP, lazy attachment)**:
```
panel.js --[runtime.sendMessage]--> background.js --[chrome.debugger CDP]--> V8
```
- Only attaches when user sets a JS breakpoint or execution steps from Scheme into JS
- Banner appears with explanation in panel UI
- Uses existing CDP infrastructure from `background.js`

### Panel UI Layout
```
+------------------------------------------------------------------+
| Toolbar: [Resume] [Step Into] [Over] [Out] [Pause]  Status: ...  |
+------------------------------------------------------------------+
|                              |                                    |
|   Source Editor (CodeMirror) |  Call Stack                        |
|   - Breakpoint gutter        |    [SCM] factorial                 |
|   - Expression highlighting  |    [JS]  Array.map                 |
|   - Mixed HTML/Scheme/JS     |    [SCM] main                     |
|                              |------------------------------------+
|                              |  Variables                         |
|   [Source Files sidebar]     |    n: 5                            |
|   v factorial.scm            |    result: 120                     |
|   v index.html               |------------------------------------+
|   > app.js                   |  Breakpoints                       |
+------------------------------------------------------------------+
```

## What to Keep, Modify, Replace

### Keep (no changes)
- `src/debug/breakpoint_manager.js` - O(1) breakpoint lookup, expression-level precision
- `src/debug/stack_tracer.js` - Call stack with TCO awareness
- `src/debug/state_inspector.js` - Environment inspection, value serialization
- `src/debug/pause_controller.js` - Cooperative pause, `waitForResume()`, stepping state machine
- `src/debug/scheme_debug_runtime.js` - Coordinator
- `src/debug/devtools/source_registry.js` - Source registration, exprId mapping (probe generation removed)
- `rollup.config.js` - Dist builds unchanged; CodeMirror is extension-only

### Modify
- **`src/debug/devtools/devtools_debug.js`** - Remove all probe-related code (`maybeHit` probe calling, `fireProbe`, async task stack, `onException` probe path). Replace with cooperative pause: `maybeHit()` checks breakpoints/stepping via PauseController and dispatches `CustomEvent`. Expand `__schemeDebug` API: add `activate()`, `getSources()`, `getSourceContent()`, `getExpressions()`, `getCurrentLocation()`, `getStatus()`, `requestPause()`, `setBreakpoint()`, `removeBreakpoint()`. Stepping commands delegate to `PauseController` instead of `__schemeProbeRuntime`.
- **`src/debug/devtools/source_registry.js`** - Remove probe generation/caching. Keep source content storage, exprId mapping, and location-to-expression resolution.
- **`src/packaging/html_adapter.js`** (line 148) - When debug is active, use `await interpreter.runAsync(ast, env)` instead of `interpreter.run(ast, env)`. The function is already `async`. Also support panel-triggered activation (the panel calls `__schemeDebug.activate()` instead of requiring URL params).
- **`extension/manifest.json`** - Change name, add `content_scripts` for event relay, keep `debugger` permission for lazy CDP.
- **`extension/devtools.js`** - Replace `createSidebarPane` with `chrome.devtools.panels.create('Scheme-JS', ...)`.
- **`extension/background.js`** - Make CDP attachment lazy (remove auto-attach, add explicit `attach-cdp` message handler). Keep existing CDP event routing and blackbox logic.

### Remove
- **`src/debug/devtools/probe_generator.js`** - No longer needed; cooperative pausing replaces probe-based debugging
- **`src/debug/devtools/probe_runtime.js`** - The `__schemeProbeRuntime` global is replaced by expanded `__schemeDebug` API
- **`src/debug/devtools/sourcemap_generator.js`** - Source maps for probes no longer needed
- **`src/debug/devtools/env_proxy.js`** - Probe `with(envProxy)` pattern no longer needed; variables are fetched via `__schemeDebug.getLocals()`
- **`src/debug/devtools/custom_formatters.js`** - Custom formatters for DevTools Scope pane no longer needed (panel owns variable display)

### Replace (extension UI)
- `extension/panel/sidebar.html` -> `extension/panel/panel.html`
- `extension/panel/sidebar.js` -> `extension/panel/panel.js` (bundled from `panel-src/`)
- `extension/panel/sidebar.css` -> `extension/panel/panel.css`

### New Files
- **`extension/content_script.js`** - Listens for `scheme-debug-paused`/`scheme-debug-resumed` CustomEvents on `window`, relays to extension via `chrome.runtime.sendMessage`
- **`extension/panel-src/main.js`** - Panel entry point
- **`extension/panel-src/components/editor.js`** - CodeMirror 6 wrapper (read-only, Scheme highlighting, breakpoint gutter, expression decorations)
- **`extension/panel-src/components/call-stack.js`** - Unified call stack (Scheme + JS frames with language indicators)
- **`extension/panel-src/components/variables.js`** - Variables/scopes display
- **`extension/panel-src/components/toolbar.js`** - Step controls, status
- **`extension/panel-src/components/source-list.js`** - File browser
- **`extension/panel-src/components/breakpoints.js`** - Breakpoint list
- **`extension/panel-src/protocol/scheme-bridge.js`** - `inspectedWindow.eval` wrapper for `__schemeDebug` API
- **`extension/panel-src/protocol/cdp-bridge.js`** - CDP communication via background.js
- **`extension/panel-src/protocol/unified-debugger.js`** - Routes to scheme-bridge or cdp-bridge based on file type
- **`extension/panel-src/state/debug-state.js`** - Reactive state management
- **`extension/build/build-panel.js`** - esbuild script to bundle panel-src + CodeMirror 6

## Phased Implementation

### Phase 1: Panel Shell + CodeMirror (no debugging logic)

Get a working "Scheme-JS" tab in DevTools with CodeMirror displaying Scheme source.

1. Set up esbuild build for the extension panel (`extension/build/build-panel.js`)
2. Install CodeMirror 6 packages as devDependencies (view, state, language, lang-javascript, search, commands)
3. Create a simple Scheme language mode (StreamParser for S-expressions)
4. Create `panel.html` with split-pane layout, `panel.css` with dark theme
5. Create `scheme-bridge.js` with `evalInPage(expr)` wrapper around `inspectedWindow.eval()`
6. Create `editor.js` — CodeMirror in read-only mode, Scheme highlighting, line numbers
7. Create `source-list.js` — fetches sources from `__schemeDebug.getSources()`, populates file list
8. Modify `devtools.js` — `chrome.devtools.panels.create('Scheme-JS', ...)`
9. Wire up: clicking a source loads it into the editor

**Deliverable**: A "Scheme-JS" tab shows source files in CodeMirror.

### Phase 2: Cooperative Scheme Breakpoints + Pause/Resume

Full cooperative Scheme debugging without CDP.

1. Expand `__schemeDebug` API in `devtools_debug.js`:
   - `activate()` — enables debug mode, sets flag for async execution
   - `getSources()` / `getSourceContent(url)` — source data from SourceRegistry
   - `setBreakpoint(url, line, col)` / `removeBreakpoint(id)` — delegates to BreakpointManager
   - `getCurrentLocation()` / `getStatus()` — pause state
   - `resume()` / `stepInto()` / `stepOver()` / `stepOut()` — delegates to PauseController
   - Dispatch `CustomEvent('scheme-debug-paused')` and `CustomEvent('scheme-debug-resumed')` on `window`
2. Add cooperative pause path to `maybeHit()`: when in cooperative mode, check breakpoints/stepping via PauseController instead of firing probes
3. Modify `html_adapter.js` line 148: use `await interpreter.runAsync()` when debug is active
4. Create `content_script.js` — relays CustomEvents to panel
5. Create `toolbar.js` — Resume/Step buttons, status display
6. Create `call-stack.js` — renders Scheme stack frames
7. Create `variables.js` — renders locals for selected frame
8. Add breakpoint gutter to CodeMirror editor (click to toggle)
9. Wire pause notifications: content script -> panel -> update all components

**Deliverable**: Set breakpoints, pause, inspect stack/variables, step through Scheme.

### Phase 3: Expression-Level Breakpoints + Current Expression Highlighting

1. Add `__schemeDebug.getExpressions(url)` — returns expression spans from SourceRegistry
2. Highlight current expression on pause using CodeMirror `Decoration.mark()` with exact span
3. Click on intra-line diamond indicators preceding expressions to set expression-level breakpoints
4. Visual indicators: gutter dots for line breakpoints, inline highlights for expression breakpoints

### Phase 4: Lazy CDP + JS Debugging

1. Refactor `background.js` — remove auto-attach, add `attach-cdp` message handler
2. Create `cdp-bridge.js` — manages CDP via background.js messages
3. Create `unified-debugger.js` — routes to scheme-bridge or cdp-bridge based on file type
4. When CDP `Debugger.paused` fires, merge JS frames with Scheme frames in call stack
5. Add language indicators `[SCM]` / `[JS]` to call stack entries
6. Load JS source into CodeMirror with JS language mode when clicking a JS frame
7. Show brief UI notification when CDP attaches ("JavaScript debugging enabled")

### Phase 5: Mixed HTML Files + Boundary Stepping

1. CodeMirror mixed-language support: HTML + nested JS + nested Scheme via `<script>` tags
2. Route breakpoints in HTML files to correct backend based on position within `<script>` blocks
3. Scheme->JS boundary: detect JS function calls in StackTracer, attach CDP if needed, set CDP breakpoint on JS function entry
4. JS->Scheme boundary: detect Scheme frame entry while CDP is active, switch panel to Scheme view
5. Unified stepping across boundaries

### Phase 6: Polish

1. Breakpoint list panel (enable/disable/delete, click to navigate)
2. Eval console (Scheme REPL in current frame context)
3. CodeMirror search (Ctrl+F)
4. Keyboard shortcuts (F8 resume, F10 step over, F11 step into, Shift+F11 step out)
5. Persist breakpoints across reloads (`chrome.storage.local`)
6. Light/dark theme matching DevTools

## Key Technical Details

### Cooperative Pause Flow (Phase 2)

The interpreter's `runAsync()` already yields cooperatively via `PauseController.shouldYield()`. The new flow replaces the old probe-based system (`debugger;` statements + source maps) with direct cooperative pausing:

1. `maybeHit(source, env)` checks breakpoints via `BreakpointManager.hasBreakpoint()`
2. If hit: `PauseController.pause('breakpoint')`, dispatch `CustomEvent('scheme-debug-paused')`
3. At next yield point: `await PauseController.waitForResume()` blocks the interpreter
4. Panel receives event via content script, fetches state via `inspectedWindow.eval`
5. User clicks Resume: `inspectedWindow.eval('__schemeDebug.resume()')` -> `PauseController.resume()` resolves the promise
6. Interpreter continues

### Async Execution Requirement

`html_adapter.js` currently calls `interpreter.run(ast, env)` (sync, line 148). For cooperative pausing, this must become `await interpreter.runAsync(ast, env)`. Since `runScripts()` is already `async`, this is a straightforward change. The key constraint: `runAsync` must be used for all Scheme execution when debug is active (script tags, event handlers, REPL evals).

### Content Script Event Relay

```javascript
// content_script.js
window.addEventListener('scheme-debug-paused', (e) => {
  chrome.runtime.sendMessage({ type: 'scheme-paused', detail: e.detail });
});
window.addEventListener('scheme-debug-resumed', () => {
  chrome.runtime.sendMessage({ type: 'scheme-resumed' });
});
```

Panel listens via `chrome.runtime.onMessage`.

### CodeMirror Bundle (Extension Only)

Bundled into `extension/panel/panel.js` via esbuild. Packages:
- `@codemirror/view`, `@codemirror/state`, `@codemirror/language`
- `@codemirror/lang-javascript` (for JS and HTML)
- `@codemirror/search`, `@codemirror/commands`
- Custom Scheme StreamParser (simple: parens, strings, numbers, comments, keywords)

**No impact on `dist/scheme.js` or `dist/scheme-html.js`**. Users never see CodeMirror.

## Verification

### Puppeteer Tests
- **UI Tests**: Create Puppeteer tests in `tests/extension` directory for testing the UI of the extension.  Every user interaction should be tested.  
- **Running Puppeteer Tests**: Use `npm run test:extension` to run the Puppeteer tests.  

### Per-Phase Testing
- **Phase 1**: Load an HTML page with `<script type="text/scheme">`, open DevTools, verify "Scheme-JS" tab appears with source displayed in CodeMirror
- **Phase 2**: Set a breakpoint in the editor, reload page, verify interpreter pauses and panel shows stack/variables. Step through code. Resume.
- **Phase 3**: Click on an intra-line diamond preceding and expression, verify expression-level breakpoint. Verify current expression is highlighted on pause.
- **Phase 4**: Set breakpoint in a .js file shown in the panel. Verify CDP attaches with notification. Step through JS. Verify unified call stack.
- **Phase 5**: Debug HTML file with inline Scheme + JS. Step from Scheme into JS and back.

### Regression Testing
- Run `npm run test` after each phase to verify interpreter behavior is unchanged
- Verify `dist/scheme.js` and `dist/scheme-html.js` are unaffected (no new dependencies)
- Test with debug disabled to verify zero overhead (`if (devtoolsDebug?.enabled)` guard)
