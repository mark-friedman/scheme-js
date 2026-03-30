# True Sync-Path Breakpoint Pauses for DOM Callbacks (CDP Approach)

**Status**: Planned but not implemented. Saved for reference.
**Date**: 2026-03-20

This plan addresses sync-path breakpoints (DOM callbacks calling Scheme closures) by keeping V8 truly paused and routing all panel commands through CDP's `Debugger.evaluateOnCallFrame`.

## Known UX Issue

Chrome auto-switches to the Sources tab whenever V8 pauses (via `debugger;` or CDP breakpoint). There is no Chrome API to suppress this. The user must click back to the Scheme-JS panel tab to use the full debugging UI.

## Key Constraint

When V8 is paused at `debugger;`, `inspectedWindow.eval()` (used by `scheme-bridge.js`) does NOT work. All evaluation must go through CDP's `Debugger.evaluateOnCallFrame` via `background.js`, which already has an `evaluateWhilePaused()` function and `eval-paused` message handler (line 347).

## Changes

### 1. `extension/background.js` — Keep V8 paused

**Remove auto-resume** in the sync-path branch (line ~227). Keep page paused until the panel sends `resume-debugger` or `scheme-step-*`.

### 2. `extension/panel-src/protocol/cdp-bridge.js` — Add CDP-routed functions

Add 4 new exported functions:

- **`evalWhilePaused(expression)`** — sends `{ type: 'eval-paused', tabId, expression }` to background.js. Returns `{ success, result, error }`.
- **`schemeStepInto()`** — sends `{ type: 'scheme-step-into', tabId }` (background.js already handles this: evaluates `probeRuntime.stepInto()` while paused, then resumes)
- **`schemeStepOver()`** — sends `{ type: 'scheme-step-over', tabId }`
- **`schemeStepOut()`** — sends `{ type: 'scheme-step-out', tabId }`

### 3. `extension/panel-src/protocol/unified-debugger.js` — Route scheme-sync through CDP

**`resume()`**: Change `scheme-sync` branch from `schemeBridge.resume()` to `cdpBridge.resumeCDP()`.

**`stepInto/Over/Out()`**: Change `scheme-sync` branches to `cdpBridge.schemeStepInto/Over/Out()`.

**`evalInFrame()`**: Add `scheme-sync` check — when paused via CDP, evaluate `__schemeDebug.eval(expr, frameIndex)` through `cdpBridge.evalWhilePaused()` instead of `schemeBridge.evalInFrame()`.

**New `getLocals(frameIndex)`**: Routes to `cdpBridge.evalWhilePaused('JSON.stringify(__schemeDebug.getLocals(N))')` when `scheme-sync`, otherwise `schemeBridge.getLocals()`.

**New `getSourceContentForContext(url)`**: Routes to `cdpBridge.evalWhilePaused(...)` when `scheme-sync`, otherwise `schemeBridge.getSourceContent()`.

**New `getExpressionsForContext(url)`**: Same pattern.

### 4. `extension/panel-src/main.js` — Route through unified-debugger when paused

**Add `loadSourceWhilePaused(url)`**: Like `loadSource()` but fetches content and expressions via `unifiedDebugger.getSourceContentForContext()` and `unifiedDebugger.getExpressionsForContext()`.

**`onPaused()`**: When `context === 'scheme-sync'`, call `loadSourceWhilePaused()` instead of `loadSource()`.

**`onPaused()`**: Route `getLocals()` through `unifiedDebugger.getLocals()` instead of `schemeBridge.getLocals()` directly.

**`onSelectFrame()`**: Same — route `getLocals()` and `loadSource()` through unified-debugger context-aware functions.

### 5. Rebuild panel

`npm run build:panel` after all panel-src changes.

## Files Modified

| File | Change |
|------|--------|
| `extension/background.js` | Remove auto-resume for sync-path |
| `extension/panel-src/protocol/cdp-bridge.js` | Add `evalWhilePaused()`, `schemeStepInto/Over/Out()` |
| `extension/panel-src/protocol/unified-debugger.js` | Route scheme-sync commands through CDP; add `getLocals()`, `getSourceContentForContext()`, `getExpressionsForContext()` |
| `extension/panel-src/main.js` | Add `loadSourceWhilePaused()`; route getLocals/loadSource through unified-debugger |
| `extension/panel/panel.js` | Rebuilt bundle |

## Flow

```
Button click → Scheme run() [_inSyncPath=true] → breakpoint → debugger;
  → V8 pauses (Chrome Sources tab shows Scheme source)
  → background.js: detect sync probe, fetch stack via evaluateOnCallFrame
  → background.js: send scheme-sync-paused to panel (V8 stays paused)
  → panel: show stack, load source via CDP, show variables via CDP
  → User clicks Step/Resume/types in REPL
  → panel → unified-debugger → cdpBridge → background.js
  → background.js: evaluate command via evaluateOnCallFrame, then Debugger.resume
  → V8 resumes → next breakpoint/step → cycle repeats
```