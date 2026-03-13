Comprehensive Review: Scheme-JS DevTools Debugging System

 Context

 This is a code review evaluating the entire debugging subsystem — the Chrome DevTools extension (rooted at extension/) and the core debug runtime APIs it depends on (in
 src/debug/). The evaluation covers correctness, complexity, modularity, code quality, documentation quality, and maintainability.

 ---
 Executive Summary

 The debugging system is well-architected overall, with clean separation between the debug runtime (interpreter-side), DevTools integration (probe-based), and the extension
 panel (UI). The probe-based approach — generating source-mapped JavaScript that uses debugger; to pause in Chrome DevTools, with with(envProxy) to expose Scheme bindings — is
 genuinely clever and effective.

 Strengths:
 - Clean layered architecture: runtime → devtools integration → bridges → panel
 - Excellent CodeMirror 6 integration with Compartments for hot-swapping
 - Solid cooperative pause mechanism with safety timeouts
 - Good test coverage (246 E2E tests, comprehensive mocking)
 - Well-documented JSDoc throughout

 Areas for improvement:
 - main.js is a 918-line god module with tight coupling
 - Breakpoint key format using ":" delimiter is fragile with URLs
 - Probe stepping is incomplete (all modes = Step Into)
 - Silent error swallowing throughout the bridge layer
 - Some race condition risks in mixed Scheme/JS debugging
 - Resource cleanup gaps (listener accumulation, unbounded growth)

 ---
 1. Architecture & Modularity

 What works well

 Layered bridge pattern:
 Panel UI (main.js + components)
   → Unified Debugger (routes by pause context)
     → Scheme Bridge (inspectedWindow.eval → __schemeDebug)
     → CDP Bridge (chrome.runtime.sendMessage → background.js → chrome.debugger)

 This is clean. Each bridge has a consistent API surface and can evolve independently.

 Component decomposition is good:

 ┌────────────────┬───────┬───────────────────────────┬──────────────────────────────┐
 │   Component    │ Lines │      Responsibility       │           Quality            │
 ├────────────────┼───────┼───────────────────────────┼──────────────────────────────┤
 │ toolbar.js     │ 111   │ Debug controls + status   │ Excellent                    │
 ├────────────────┼───────┼───────────────────────────┼──────────────────────────────┤
 │ call-stack.js  │ 158   │ Frame display + selection │ Good                         │
 ├────────────────┼───────┼───────────────────────────┼──────────────────────────────┤
 │ variables.js   │ 96    │ Locals table              │ Good                         │
 ├────────────────┼───────┼───────────────────────────┼──────────────────────────────┤
 │ source-list.js │ 123   │ File browser              │ Good                         │
 ├────────────────┼───────┼───────────────────────────┼──────────────────────────────┤
 │ breakpoints.js │ 112   │ Breakpoint list           │ Good                         │
 ├────────────────┼───────┼───────────────────────────┼──────────────────────────────┤
 │ console.js     │ 188   │ REPL console              │ Good                         │
 ├────────────────┼───────┼───────────────────────────┼──────────────────────────────┤
 │ editor.js      │ 802   │ CodeMirror integration    │ Good (complex but justified) │
 └────────────────┴───────┴───────────────────────────┴──────────────────────────────┘

 Debug runtime decomposition is excellent:

 ┌───────────────────────┬──────────────────────────────────────────────┐
 │        Module         │                Responsibility                │
 ├───────────────────────┼──────────────────────────────────────────────┤
 │ SchemeDebugRuntime    │ Coordinator                                  │
 ├───────────────────────┼──────────────────────────────────────────────┤
 │ BreakpointManager     │ O(1) breakpoint lookup                       │
 ├───────────────────────┼──────────────────────────────────────────────┤
 │ PauseController       │ Pause/resume state machine with LIFO nesting │
 ├───────────────────────┼──────────────────────────────────────────────┤
 │ StackTracer           │ Call frames with TCO awareness               │
 ├───────────────────────┼──────────────────────────────────────────────┤
 │ StateInspector        │ Scheme→CDP value serialization               │
 ├───────────────────────┼──────────────────────────────────────────────┤
 │ DebugExceptionHandler │ Exception pause decisions                    │
 └───────────────────────┴──────────────────────────────────────────────┘

 What needs work

 main.js (918 lines) is a god module. It handles:
 - All state management (breakpointIds, currentSourceUrl, selectedUnifiedFrame, etc.)
 - DOM splitter drag logic
 - Chrome theme detection
 - Breakpoint persistence (save/sync/refresh)
 - Source loading (Scheme, JS, HTML)
 - Pause/resume orchestration
 - Component wiring
 - Message routing

 Recommendation: Extract into focused modules:
 1. breakpoint-state.js — breakpointIds Map, save/sync/refresh logic
 2. splitter.js — drag resize behavior (generic, reusable)
 3. pause-handler.js — onPaused/onResumed orchestration

 Breakpoint key format ("${url}:${line}:${column}") is fragile:
 - URLs contain colons (http://localhost:8080/file.scm)
 - Code splits on ":" and pops 3 times from the end, then rejoins
 - This works but is brittle and hard to reason about
 - Fix: Use a structured key (JSON, or Map<url, Map<line, Set<column>>>)

 ---
 2. Correctness

 Issues found

 Probe stepping is incomplete (probe_runtime.js:60):
 _shouldStopForStep(exprId) {
     return true;  // Placeholder — all step modes behave as Step Into
 }
 Step Over and Step Out are documented as "to be implemented in Phase 2.5" but the comment dates from early development. This means step-over and step-out are broken for Scheme
  code — they all behave as step-into. The CDP bridge handles JS stepping correctly, but Scheme stepping through probes does not differentiate.

 Race condition in mixed debugging (unified-debugger.js):
 - handleSchemePause() can fire while a CDP pause is pending
 - If timing is wrong, schemeCallFrames is overwritten before the previous pause is fully processed
 - The currentPauseContext flag is a single value, not a queue

 pauseAcknowledged timing (scheme_debug_runtime.js):
 - Reset to false at line 396, but timeout set at line 406
 - If ackPause() arrives in this 10-line window, the flag is set true then the timeout still fires because it reads a stale closure

 HTML file breakpoint routing (unified-debugger.js):
 - isSchemeLine() checks if expression spans exist at that line
 - For HTML files with both <script> and <script type="text/scheme">, this could misroute if the line number falls in a JS block that has no expression spans
 - The fallback "try Scheme first, then CDP" on remove is pragmatic but indicates the routing isn't fully reliable

 What works correctly

 - Cooperative pause mechanism with safety timeout — solid
 - LIFO resolver stack for nested pauses (eval during breakpoint) — correct
 - Breakpoint deduplication via lastHitKey + lastHitEnv in maybeHit() — elegant
 - Panel connection protocol (sessionStorage → document_start → html_adapter) — robust multi-level redundancy
 - Cross-world dual dispatch (CustomEvent + postMessage) — correct workaround for Chrome limitation

 ---
 3. Complexity

 Justified complexity

 - CodeMirror 6 integration (editor.js, 802 lines): The StateField/Effect/Decoration pattern is the right way to use CM6. Multiple visual states (breakpoints, current line,
 diamonds, expression highlights) require this level of sophistication.
 - Probe generation + source maps: Generating JavaScript with debugger; + source maps pointing back to Scheme source is inherently complex but well-executed.
 - Unified debugger: Routing between Scheme and JS debugging contexts is genuinely complex; the abstraction is appropriate.

 Unnecessary complexity

 - Splitter drag logic in main.js (~60 lines × 2 splitters): Generic drag-to-resize behavior that could be a 30-line helper function.
 - Breakpoint key parsing: String concatenation + split-on-colon is more complex than necessary. A structured approach would be simpler.
 - refreshBreakpointsPanel() rebuilds the entire breakpoints list on every change. Fine for small lists but could use a targeted update.

 ---
 4. Code Quality

 Good patterns observed

 - Consistent JSDoc across all files
 - Clean use of ES Modules
 - Proper Promise wrapping of Chrome callback APIs (evalInPage)
 - Well-named functions that describe what they do
 - Good use of CodeMirror 6 Compartments for theme/language hot-swap

 Issues

 Silent error swallowing — the most pervasive issue:
 // scheme-bridge.js pattern (repeated ~15 times)
 try {
     const result = await evalInPage(`JSON.stringify(__schemeDebug.getStack())`);
     return JSON.parse(result);
 } catch (e) {
     return [];  // Silent fallback
 }
 Callers never know why operations fail. No logging, no error state, no user feedback.

 No listener cleanup:
 - source-list.js render() adds click listeners every call without removing old ones
 - unified-debugger.js listener arrays grow if init() is called multiple times
 - cdp-bridge.js has no deregistration API

 Console has no history size limit (console.js):
 history.push(code);  // Grows forever

 Stale cache risk (cdp-bridge.js):
 - tabId is cached on first call to getTabId() — if DevTools are detached/reattached, cache is stale
 - scriptUrlMap grows monotonically; no cleanup on page navigation

 Dead/debug code:
 - Check for any remaining console.log statements that were used for debugging

 ---
 5. Documentation Quality

 Strong

 - All JS functions have JSDoc with param/return types
 - @fileoverview on every file
 - Architecture docs (docs/architecture.md) are accurate and comprehensive
 - extension/MEMORY.md is current with phase-by-phase details
 - Comment blocks separate logical sections in larger files

 Gaps

 - Data flow between main.js and components is implicit — no diagram or written description of the event/callback wiring
 - Breakpoint lifecycle (gutter click → breakpointIds → saveToPage → interpreter → probeRuntime) spans 5 files with no single document describing the full flow
 - Probe mechanism documentation is scattered — probe_generator.js, probe_runtime.js, devtools_debug.js, source_registry.js each document their piece but there's no overview
 - panelConnected protocol spans 4 files (activate_debug.js, html_adapter.js, scheme_debug_runtime.js, devtools_debug.js) — documented in MEMORY.md but not in code

 ---
 6. Testing

 Coverage summary

 ┌────────────────────────┬───────┬──────────────────────────────────────────────────────┐
 │          Area          │ Tests │                       Quality                        │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ Scheme breakpoints     │ 6     │ Comprehensive                                        │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ Stepping               │ 3     │ Good (but tests Scheme stepping which is incomplete) │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ State inspection       │ 3     │ Solid                                                │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ Panel UI (static)      │ 7     │ Good                                                 │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ Panel interactions     │ 30+   │ Extensive                                            │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ Console                │ 3     │ Basic                                                │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ JS debugging (Phase 4) │ 18+   │ Growing                                              │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ JS interop             │ 11+   │ Good                                                 │
 ├────────────────────────┼───────┼──────────────────────────────────────────────────────┤
 │ Cross-world relay      │ 3     │ Verified                                             │
 └────────────────────────┴───────┴──────────────────────────────────────────────────────┘

 Test quality issues

 Brittle selectors: Some tests use querySelectorAll()[N] instead of data attributes:
 const items = document.querySelectorAll('#source-list .source-item');
 return Array.from(items).map(el => ({...}));

 Fixed timeouts instead of condition polling:
 await new Promise(r => setTimeout(r, 1500));  // Hoping 1.5s is enough

 Mock complexity: The MOCK_CHROME_SCRIPT is a ~200-line string injected via evaluateOnNewDocument. It's duplicated (with variations) in test_console.mjs. Should be a shared
 module with composable mock layers.

 Coverage gaps

 - Error paths: Almost no tests for failure cases (eval timeout, source load failure, broken CDP connection)
 - Scheme step-over/step-out: No tests verifying these work differently from step-into (because they don't — see probe_runtime issue)
 - Mixed HTML debugging: Limited testing of breakpoint routing in <script> vs <script type="text/scheme">
 - Resource cleanup: No tests for listener cleanup, memory growth, or stale cache

 ---
 7. Prioritized Recommendations

 High Priority (correctness/reliability)

 1. Implement probe stepping (probe_runtime.js): _shouldStopForStep() needs depth awareness. The debug runtime already tracks frame depth via StackTracer — expose it to the
 probe runtime.
 2. Fix breakpoint key format: Replace string-concatenated keys with structured storage (Map<url, Map<line, Set<column>>> or JSON keys).
 3. Add error visibility: At minimum, log bridge errors to console. Better: show transient error messages in toolbar status. Best: dedicated error/notification component.

 Medium Priority (maintainability)

 4. Extract main.js state management: Pull breakpoint state, splitter logic, and pause handling into separate modules. Target: main.js under 400 lines.
 5. Share mock infrastructure: Extract MOCK_CHROME_SCRIPT to a shared test_mock_chrome.mjs module with composable layers (base, console, breakpoints, etc.).
 6. Add listener cleanup: Implement destroy() or dispose() on components and bridges. Call on page unload or re-init.
 7. Cap unbounded growth: Console history (limit to 100), StateInspector object registry (LRU), cdpBridge.scriptUrlMap (clear on navigation).

 Lower Priority (polish)

 8. Replace fixed timeouts in tests with condition-based polling (waitFor(() => condition, timeout)).
 9. Add data attributes to DOM elements for more stable test selectors.
 10. Document cross-cutting flows: Write a single doc covering the breakpoint lifecycle, probe mechanism, and panelConnected protocol end-to-end.
 11. Add error path tests: Eval timeouts, broken connections, missing sources.

 ---
 Files Referenced

 Extension Panel

 - extension/panel-src/main.js (918 lines) — god module, primary refactoring target
 - extension/panel-src/components/editor.js (802 lines) — well-structured CM6 integration
 - extension/panel-src/components/console.js (188 lines)
 - extension/panel-src/components/breakpoints.js (112 lines)
 - extension/panel-src/protocol/unified-debugger.js (444 lines) — race condition risk
 - extension/panel-src/protocol/scheme-bridge.js (259 lines) — silent error swallowing
 - extension/panel-src/protocol/cdp-bridge.js (414 lines) — stale cache risk

 Debug Runtime

 - src/debug/scheme_debug_runtime.js — coordinator, pauseAcknowledged timing issue
 - src/debug/devtools/probe_runtime.js (112 lines) — incomplete stepping
 - src/debug/devtools/devtools_debug.js — probe mechanism + __schemeDebug API
 - src/debug/devtools/source_registry.js — probe generation + LRU
 - src/debug/pause_controller.js — LIFO resolver stack (solid)
 - src/debug/breakpoint_manager.js — O(1) lookup (solid)

 Tests

 - tests/extension/test_panel_interactions.mjs — largest test file, mock complexity
 - tests/extension/test_console.mjs — duplicated mock setup
 - tests/extension/test_harness.mjs — test infrastructure
