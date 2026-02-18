# Chrome DevTools Scheme Debugger — Architecture, UI & Implementation Plan

## Executive Summary

This document describes a design for seamless Scheme debugging inside Chrome DevTools for the `scheme-js` interpreter. The core innovation is **probe scripts**: for every loaded Scheme source, we dynamically generate a small JavaScript file whose lines are source-mapped back to the original Scheme code. The interpreter's trampoline calls the appropriate probe whenever the current Scheme source location changes, which makes Chrome DevTools believe real JS execution is happening at the Scheme line.

The call stack experience uses the **E+D design**: **Alternative D** (`console.createTask`) provides lightweight async stack hints in the native Call Stack pane, while **Alternative E** (a Chrome DevTools extension) adds a "Scheme Stack" sidebar in the Sources panel with full clickable frame navigation and per-frame variable inspection.

Combined with `with(envProxy)` scoping, DevTools blackboxing, and boundary detection, this gives us:

- **Scheme source code in the Sources panel** with syntax highlighting and line numbers
- **Native DevTools breakpoints** that actually fire on Scheme lines
- **Step Into / Step Over / Step Out** that tracks Scheme execution
- **Scheme variables in the Scope pane** during pauses (top frame via probe, all frames via extension sidebar)
- **Full Scheme call stack** with clickable navigation and per-frame variable inspection (extension sidebar)
- **Async stack hints** in the native Call Stack showing Scheme function call chain
- **Seamless JS ↔ Scheme boundary crossing** when stepping
- **< 20% overhead** when DevTools is open (zero overhead when disabled)

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Probe Script System](#2-probe-script-system)
3. [Source Map Generation](#3-source-map-generation)
4. [Trampoline Integration](#4-trampoline-integration)
5. [Variable Inspection via Environment Proxy](#5-variable-inspection-via-environment-proxy)
6. [JS ↔ Scheme Boundary Handling](#6-js--scheme-boundary-handling)
7. [Breakpoint Support](#7-breakpoint-support)
8. [Stack Trace Display — The E+D Design](#8-stack-trace-display--the-ed-design)
    - 8.1 [The Problem: V8 Call Frames Cannot Be Fabricated](#81-the-problem-v8-call-frames-cannot-be-fabricated)
    - 8.2 [Solution: Two Complementary Mechanisms (E+D)](#82-solution-two-complementary-mechanisms-ed)
    - 8.3 [Native Call Stack Appearance](#83-native-call-stack-appearance)
    - 8.4 [Alternative D: Async Stack Tagging via `console.createTask`](#84-alternative-d-async-stack-tagging-via-consolecreatetask)
    - 8.5 [Alternative E: Chrome DevTools Extension — Scheme Stack Sidebar](#85-alternative-e-chrome-devtools-extension--scheme-stack-sidebar)
    - 8.6 [Enhanced Console API (`window.__schemeDebug`)](#86-enhanced-console-api-windowschemedebug)
    - 8.7 [Interaction Diagram: E+D Combined](#87-interaction-diagram-ed-combined)
9. [Exception Handling](#9-exception-handling)
10. [Script Tag & File Loading](#10-script-tag--file-loading)
11. [DevTools Blackboxing](#11-devtools-blackboxing)
12. [Performance Design](#12-performance-design)
13. [UI/UX Flow](#13-uiux-flow)
14. [Node.js / Future DAP Support](#14-nodejs--future-dap-support)
15. [File & Module Structure](#15-file--module-structure)
16. [Implementation Plan](#16-implementation-plan)
17. [Testing Strategy](#17-testing-strategy)
18. [Open Questions](#18-open-questions)

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         Chrome DevTools                                  │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │  Sources Panel                                                    │   │
│  │  ┌──────────────────────┐  ┌─────────────────────────────────┐  │   │
│  │  │ Editor: Scheme source │  │ Sidebar: "Scheme Stack" (Alt E) │  │   │
│  │  │ via source maps       │  │ - Full call stack (clickable)   │  │   │
│  │  └──────────────────────┘  │ - Per-frame variables           │  │   │
│  │                             │ - TCO badges                    │  │   │
│  │  ┌──────────────────────┐  └─────────────────────────────────┘  │   │
│  │  │ Scope Pane: bindings │                                        │   │
│  │  │ via with(envProxy)   │  ┌─────────────────────────────────┐  │   │
│  │  │ (top frame only)     │  │ Call Stack (native)              │  │   │
│  │  └──────────────────────┘  │ - Current probe frame           │  │   │
│  │                             │ - Async frames (Alt D)          │  │   │
│  │  Breakpoints: set on       │ - Blackboxed runtime hidden     │  │   │
│  │  Scheme lines via probes   └─────────────────────────────────┘  │   │
│  └─────────────────────────────────┬────────────────────────────────┘   │
│                                    │ V8 debugging hooks                  │
│  ┌─────────────────────────────────│────────────────────────────────┐   │
│  │  Extension (background.js)      │                                │   │
│  │  - Listens for Debugger.paused  │                                │   │
│  │  - Calls inspectedWindow.eval() │                                │   │
│  │  - Drives sidebar updates       │                                │   │
│  └─────────────────────────────────│────────────────────────────────┘   │
└────────────────────────────────────┼────────────────────────────────────┘
                                     │
┌────────────────────────────────────┼────────────────────────────────────┐
│                                    │        Browser JS Engine           │
│                                    │                                    │
│  ┌─────────────────────────────────▼──────────────────────────────────┐ │
│  │            Probe Scripts (generated per Scheme file)               │ │
│  │  One JS function per Scheme line, source-mapped back to .scm      │ │
│  │  Each probe: function(envProxy) { with (envProxy) { void 0 } }   │ │
│  │  Probes fired via task.run() for async stack tagging (Alt D)      │ │
│  └─────────────────────────────────┬──────────────────────────────────┘ │
│                                    │ called by trampoline on line change│
│  ┌─────────────────────────────────▼──────────────────────────────────┐ │
│  │             DevToolsDebugIntegration                               │ │
│  │  - Manages probe registry and source registry                     │ │
│  │  - Tracks current Scheme source location                          │ │
│  │  - Creates envProxy for scope inspection                          │ │
│  │  - Maintains task stack (console.createTask) for Alt D            │ │
│  │  - Exposes __schemeDebug global for Alt E                         │ │
│  │  - Handles boundary detection (JS↔Scheme)                         │ │
│  └─────────────────────────────────┬──────────────────────────────────┘ │
│                                    │                                    │
│  ┌─────────────────────────────────▼──────────────────────────────────┐ │
│  │           Interpreter Trampoline (run / runDebug)                 │ │
│  │  while(true) { step(registers); }                                 │ │
│  │  → on source change: devtools.maybeHit(source, env)               │ │
│  └─────────────────────────────────┬──────────────────────────────────┘ │
│                                    │                                    │
│  ┌─────────────────────────────────▼──────────────────────────────────┐ │
│  │           Existing Debug Runtime (SchemeDebugRuntime)              │ │
│  │  BreakpointManager, StackTracer, PauseController, etc.            │ │
│  │  → StackTracer hooks notify DevToolsDebug of enter/exit/replace   │ │
│  │  (continues to work for REPL/custom UI debugging)                 │ │
│  └────────────────────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────────────────┘
```

### Key Principle: Probe Scripts as the Bridge

The fundamental challenge is that the Scheme interpreter is a trampoline — a single `while(true)` loop in `interpreter.js` that repeatedly calls `step()`. Chrome DevTools cannot distinguish between iterations of this loop; to DevTools, it's the same JS function executing at the same JS line.

**Probe scripts** solve this by creating real, distinct JavaScript locations for each Scheme source line. When the trampoline reaches a new Scheme line, it calls the corresponding probe function. Since that probe function is source-mapped to the Scheme file, DevTools shows the Scheme source.

---

## 2. Probe Script System

### 2.1 What is a Probe Script?

For each Scheme source (inline `<script>` tag or external `.scm` file), we generate a small JavaScript file at runtime. This file contains one function per Scheme source line. Each function is trivial — its only purpose is to create a JS execution point that DevTools can see.

### 2.2 Probe Script Structure

```javascript
// Auto-generated probe script for: scheme://app/factorial.scm
// DO NOT EDIT - generated by scheme-js DevTools integration
(function(__schemeProbeRegistry) {
  const probes = [];

  // Line 1: (define (factorial n)
  probes[1] = function __scheme_L1(envProxy) { with (envProxy) { void 0; } };
  // Line 2:   (if (<= n 1)
  probes[2] = function __scheme_L2(envProxy) { with (envProxy) { void 0; } };
  // Line 3:       1
  probes[3] = function __scheme_L3(envProxy) { with (envProxy) { void 0; } };
  // Line 4:       (* n (factorial (- n 1)))))
  probes[4] = function __scheme_L4(envProxy) { with (envProxy) { void 0; } };
  // Line 5:
  probes[5] = function __scheme_L5(envProxy) { with (envProxy) { void 0; } };
  // Line 6: (factorial 5)
  probes[6] = function __scheme_L6(envProxy) { with (envProxy) { void 0; } };

  __schemeProbeRegistry.set("scheme://app/factorial.scm", probes);
})(globalThis.__schemeProbeRegistry);

//# sourceURL=scheme-probe://app/factorial.scm.probe.js
//# sourceMappingURL=data:application/json;base64,<base64-encoded-source-map>
```

### 2.3 SchemeSourceRegistry

```javascript
/**
 * Manages loaded Scheme sources and their probe scripts.
 */
class SchemeSourceRegistry {
  constructor() {
    /** @type {Map<string, {content: string, lines: number, origin: string}>} */
    this.sources = new Map();

    /** @type {Map<string, Function[]>} */
    this.probes = new Map();  // url → array of probe functions
  }

  /**
   * Registers a Scheme source and generates + injects its probe script.
   * @param {string} url - Canonical URL for this source
   * @param {string} content - The Scheme source code
   * @param {string} origin - 'inline', 'external', or 'repl'
   */
  register(url, content, origin) {
    const lines = content.split('\n').length;
    this.sources.set(url, { content, lines, origin });
    this._injectProbeScript(url, content, lines);
  }

  /**
   * Gets the probe function for a specific source location.
   * @param {string} url - Source URL
   * @param {number} line - 1-indexed line number
   * @returns {Function|null} The probe function, or null
   */
  getProbe(url, line) {
    const probes = this.probes.get(url);
    if (!probes || line < 0 || line >= probes.length) return null;
    return probes[line];
  }
}
```

### 2.4 URL Convention

| Source Type | Canonical URL | Probe Script URL |
|---|---|---|
| External `.scm` file | `scheme://app/path/to/file.scm` | `scheme-probe://app/path/to/file.scm.probe.js` |
| Inline `<script>` (1st) | `scheme://inline/script-0.scm` | `scheme-probe://inline/script-0.scm.probe.js` |
| Inline `<script>` (2nd) | `scheme://inline/script-1.scm` | `scheme-probe://inline/script-1.scm.probe.js` |
| REPL input | `scheme://repl/eval-<n>.scm` | `scheme-probe://repl/eval-<n>.scm.probe.js` |
| Library `.sld` file | `scheme://lib/scheme/base.sld` | `scheme-probe://lib/scheme/base.sld.probe.js` |

---

## 3. Source Map Generation

### 3.1 Source Map Structure

Each probe script includes an inline source map (base64 data URL) with the following structure:

```json
{
  "version": 3,
  "file": "scheme-probe://app/factorial.scm.probe.js",
  "sources": ["scheme://app/factorial.scm"],
  "sourcesContent": ["(define (factorial n)\n  (if (<= n 1)\n      1\n      (* n (factorial (- n 1)))))\n\n(factorial 5)\n"],
  "names": [],
  "mappings": "<VLQ-encoded mappings>"
}
```

### 3.2 Mapping Strategy

Each probe function corresponds to exactly one Scheme source line. The source map maps the `void 0;` statement inside each probe to the corresponding Scheme line:

| Probe JS Line (generated) | Maps To (original) |
|---|---|
| `probes[1] = function __scheme_L1(envProxy) { with (envProxy) { void 0; } };` | `factorial.scm` line 1, column 0 |
| `probes[2] = function __scheme_L2(envProxy) { with (envProxy) { void 0; } };` | `factorial.scm` line 2, column 0 |

The mapping specifically targets the `void 0;` statement (the innermost executable code), not the function definition line, so that when DevTools pauses inside the probe, it shows the correct Scheme line.

### 3.3 Source Map Generator

```javascript
/**
 * Generates a V3 source map for a probe script.
 * @param {string} schemeUrl - Original Scheme source URL
 * @param {string} probeUrl - Generated probe script URL
 * @param {string} schemeContent - Original Scheme source text
 * @param {number} lineCount - Number of lines in the Scheme source
 * @returns {string} Base64-encoded source map JSON
 */
function generateProbeSourceMap(schemeUrl, probeUrl, schemeContent, lineCount) {
  // Uses VLQ encoding to map each probe's `void 0;` to the corresponding
  // Scheme source line. The generated column in the probe JS is the column
  // of `void 0;` inside `with (envProxy) { void 0; }`.
  // ...
}
```

We will use a lightweight VLQ encoder (no external dependencies). The `source-map` npm package is NOT required — VLQ encoding for a simple 1:1 line mapping is trivial to implement in ~30 lines.

---

## 4. Trampoline Integration

### 4.1 The Hit Hook

The core integration point is a single check in the trampoline loop. When DevTools debugging is active, after each step that changes the source location, we call the corresponding probe.

**Changes to `interpreter.js`:**

```javascript
// In the run() trampoline loop (synchronous path):
while (true) {
  // DevTools probe check (only when enabled, only on source change)
  if (this.devtoolsDebug) {
    const src = registers[CTL]?.source;
    if (src) {
      this.devtoolsDebug.maybeHit(src, registers[ENV]);
    }
  }

  if (this.step(registers)) {
    continue;
  }
  // ... frame stack handling ...
}
```

### 4.2 DevToolsDebugIntegration Class

```javascript
/**
 * Bridges the interpreter trampoline to Chrome DevTools via probe scripts.
 * Tracks current Scheme source location and calls probe functions on change.
 */
class DevToolsDebugIntegration {
  constructor(sourceRegistry) {
    this.sourceRegistry = sourceRegistry;
    this.enabled = false;

    /** @type {string|null} - Last hit source key ("url:line") */
    this.lastHitKey = null;

    /** @type {Proxy|null} - Cached environment proxy */
    this.currentEnvProxy = null;

    /** @type {Object|null} - Environment the proxy was built for */
    this.currentEnvRef = null;
  }

  /**
   * Called by the trampoline on every step that has source info.
   * Only calls the probe when the source location actually changes.
   * @param {Object} source - { filename, line, column }
   * @param {Object} env - Current Scheme environment
   */
  maybeHit(source, env) {
    const key = `${source.filename}:${source.line}`;

    // Skip if same location as last hit (common in multi-step expressions)
    if (key === this.lastHitKey) return;
    this.lastHitKey = key;

    // Look up the probe function
    const probe = this.sourceRegistry.getProbe(source.filename, source.line);
    if (!probe) return;

    // Build or reuse environment proxy
    if (env !== this.currentEnvRef) {
      this.currentEnvProxy = createEnvProxy(env);
      this.currentEnvRef = env;
    }

    // Call the probe — this is where DevTools "sees" execution at this Scheme line
    probe(this.currentEnvProxy);
  }

  /**
   * Enables DevTools debugging. Called when DevTools is detected or
   * explicitly enabled by the user.
   */
  enable() {
    this.enabled = true;
  }

  /**
   * Disables DevTools debugging. Removes all overhead from the hot loop.
   */
  disable() {
    this.enabled = false;
    this.lastHitKey = null;
    this.currentEnvProxy = null;
    this.currentEnvRef = null;
  }
}
```

### 4.3 Integration with Both `run()` and `runDebug()`

The DevTools integration works with **both** the synchronous `run()` path and the async `runDebug()` path:

- **`run()` (synchronous):** The probe call is synchronous. If DevTools has a breakpoint, V8 pauses execution at the probe. The trampoline is suspended by V8 itself (not by our async mechanism). This is the natural, zero-overhead-when-no-breakpoint path.

- **`runDebug()` (async):** The existing `SchemeDebugRuntime` continues to work alongside DevTools integration. Both can coexist — the REPL debugger uses `shouldPause()` + promises, while DevTools uses probes + V8 breakpoints.

### 4.4 Diagram: Trampoline → Probe → DevTools

```
Trampoline iteration N:
  CTL = (IfNode for line 2 of factorial.scm)
  ┌─────────────────────────┐
  │ source = {              │
  │   filename: "scheme://app/factorial.scm",
  │   line: 2, column: 2   │
  │ }                       │
  └──────────┬──────────────┘
             │ key = "scheme://app/factorial.scm:2"
             │ key !== lastHitKey? YES
             │
             ▼
  probe = sourceRegistry.getProbe("scheme://app/factorial.scm", 2)
             │
             ▼
  probe(envProxy)
    → function __scheme_L2(envProxy) {
        with (envProxy) {
          void 0;  // ← V8 pauses here if DevTools breakpoint is set on line 2
        }
      }
             │
             ▼
  DevTools shows: factorial.scm, line 2
  Scope pane shows: n = 5 (from envProxy)
```

---

## 5. Variable Inspection via Environment Proxy

### 5.1 The `with(envProxy)` Pattern

When DevTools pauses inside a probe function, the `with` statement creates a scope that DevTools can enumerate. We use a `Proxy` to bridge the Scheme environment to JS property access.

```javascript
/**
 * Creates a Proxy that exposes Scheme environment bindings as JS properties.
 * Used inside `with()` blocks in probe functions for DevTools Scope inspection.
 *
 * @param {Environment} env - The Scheme environment to expose
 * @returns {Proxy} A proxy suitable for `with(proxy) { ... }`
 */
function createEnvProxy(env) {
  const reverseNameMap = buildReverseNameMap(env);

  return new Proxy(Object.create(null), {
    has(target, prop) {
      if (typeof prop !== 'string') return false;
      if (prop === Symbol.unscopables) return false;
      // Check if this binding exists in the environment chain
      return env.findEnv(prop) !== null || reverseNameMap.has(prop);
    },

    get(target, prop) {
      if (prop === Symbol.unscopables) return undefined;
      if (typeof prop !== 'string') return undefined;

      // Try original name first (reverse lookup from nameMap)
      const internalName = getInternalName(env, prop);
      if (internalName) {
        return formatForDevTools(env.lookup(internalName));
      }

      // Try direct lookup
      try {
        return formatForDevTools(env.lookup(prop));
      } catch {
        return undefined;
      }
    },

    ownKeys(target) {
      // Collect all visible bindings using original names
      return collectVisibleBindingNames(env);
    },

    getOwnPropertyDescriptor(target, prop) {
      if (typeof prop !== 'string') return undefined;
      if (this.has(target, prop)) {
        return { enumerable: true, configurable: true, writable: true };
      }
      return undefined;
    }
  });
}
```

### 5.2 Value Formatting for DevTools

Scheme values need to be presented in a way Chrome DevTools can display:

| Scheme Type | DevTools Display |
|---|---|
| Number (fixnum/bigint) | JS number or BigInt |
| String | JS string |
| Boolean | JS boolean |
| Symbol | `Symbol('name')` or custom string `'symbol:name'` |
| Pair/List | Object with `car`/`cdr` properties, or Array for proper lists |
| Vector | JS Array |
| Closure | Function with descriptive name |
| Char | String `#\a` |
| `'()` (empty list) | `null` with description |

### 5.3 Scope Display

When DevTools pauses inside a probe, the Scope pane shows:

```
▶ With Block (scheme://app/factorial.scm)
    n: 5
    factorial: ƒ factorial
▶ Closure Scope
    ... (parent environment bindings)
▶ Global Scope
    ... (global Scheme bindings)
```

---

## 6. JS ↔ Scheme Boundary Handling

### 6.1 JS → Scheme (Stepping Into a Closure Call)

**Scenario:** User is paused in their JS code at a line like `result = mySchemeFunc(42)`. They click "Step Into."

**What happens without our integration:**
1. DevTools steps into the closure wrapper function (in `values.js`)
2. Then into `runWithSentinel()` → `run()` → the trampoline loop
3. User sees interpreter internals — terrible UX

**What happens with our integration:**
1. DevTools steps into the closure wrapper (briefly)
2. The closure calls `runWithSentinel()` → `run()`
3. First trampoline iteration: `maybeHit()` calls the probe for line 1 of the Scheme function
4. **If interpreter runtime is blackboxed:** DevTools skips all runtime frames and lands directly in the probe → user sees Scheme source ✓
5. **If not blackboxed:** User sees a few runtime frames, then the probe. Still works, just noisier.

**Optimization — Immediate probe on entry:**
At the very top of `run()`, before the first trampoline iteration, fire a probe for the initial AST's source location:

```javascript
run(ast, env, ...) {
  // ... setup ...
  // Immediate DevTools probe for entry point
  if (this.devtoolsDebug?.enabled && ast.source) {
    this.devtoolsDebug.maybeHit(ast.source, env);
  }
  // ... trampoline loop ...
}
```

### 6.2 Scheme → JS (Stepping Into a Native Function)

**Scenario:** User is paused at a Scheme line that calls a JS function — e.g. `(my-callback 42)` where `my-callback` holds a JS function. They click "Step Into."

This works regardless of **how** the JS function ended up being called. The boundary detection is structural, not syntactic: `AppFrame.step()` (in `frames.js`) checks `typeof func === 'function'` after ruling out Scheme closures and continuations. This catches all cases:

- Explicit interop: `(js-call "console.log" x)`
- JS function in a Scheme variable: `(my-js-callback 42)`
- JS function from a data structure: `((vector-ref handlers 0) x)`
- JS function returned by another call: `((get-handler) arg)`
- JS method via dot-access: `(console.log "hello")`

**What happens in all cases:**
1. Trampoline evaluates the application in `AppFrame.step()`
2. Section 3 "JS FUNCTION APPLICATION" calls `func(...appliedArgs)` — a direct JS function call
3. DevTools naturally steps into the JS function (it's not blackboxed)
4. When JS returns, the trampoline continues and `maybeHit()` fires the next Scheme line's probe
5. User is back in Scheme source ✓

**No special handling needed** — the existing `pushJsContext()`/`popJsContext()` mechanism plus the natural call into JS gives us boundary crossing for free.

### 6.3 Scheme → Scheme (Step Into a Scheme closure from Scheme)

This works naturally because the trampoline continues running and `maybeHit()` tracks source location changes. When a new closure's body starts executing, its source points to the new file/line.

### 6.4 Boundary Diagram

```
   JS Code                  Scheme Runtime              Probe Scripts
   ────────                  ──────────────              ─────────────
   let x = add(1,2);
        │
        │ Step Into
        ▼
   [closure wrapper]  ─────► run()
   (blackboxed)               │
                              │ first step
                              ▼
                        maybeHit(line 1) ──────────► __scheme_L1(envProxy)
                                                         │
                                                    DevTools shows:
                                                    add.scm line 1
                              │                          │
                              │ more steps...            │
                              ▼                          │
                        maybeHit(line 2) ──────────► __scheme_L2(envProxy)
                                                         │
                              │                     DevTools shows:
                              │                     add.scm line 2
                              │
                        native call to +
                              │
                              ▼
                        [JS + function]  ◄── Step Into enters JS naturally
                              │
                              │ returns
                              ▼
                        maybeHit(line 3) ──────────► __scheme_L3(envProxy)
                              │
                         run() returns
                              │
        ◄─────────────────────┘
   console.log(x);
```

---

## 7. Breakpoint Support

### 7.1 Native DevTools Breakpoints

With probe scripts and source maps, DevTools breakpoints work natively:

1. User opens Sources panel → sees `factorial.scm` (via source map `sourcesContent`)
2. User clicks line 3 gutter → DevTools sets a breakpoint
3. DevTools internally sets the breakpoint on the corresponding probe JS line
4. When the interpreter reaches line 3, `maybeHit()` calls `probes[3](envProxy)`
5. V8 hits the breakpoint inside the probe → execution pauses
6. DevTools shows `factorial.scm` line 3 with Scheme variables in Scope

### 7.2 Coexistence with `BreakpointManager`

The existing `BreakpointManager` continues to work for the REPL debug backend. DevTools breakpoints and `BreakpointManager` breakpoints are independent:

| Feature | BreakpointManager | DevTools Breakpoints |
|---|---|---|
| Storage | In-memory Map | Chrome's breakpoint storage |
| Persistence | Per-session | Persists across page loads (by sourceURL) |
| Conditional | Yes (planned) | Yes (native DevTools feature) |
| UI | REPL commands (`:break`) | Sources panel gutter click |
| Mechanism | `shouldPause()` check in `runDebug()` | V8 breakpoint on probe function |

### 7.3 Logpoints and Conditional Breakpoints

Because the probes are real JS functions, DevTools' native conditional breakpoints and logpoints work automatically. A conditional breakpoint on Scheme line 3 would evaluate its condition in the `with(envProxy)` scope, meaning Scheme variable names resolve correctly.

---

## 8. Stack Trace Display — The E+D Design

### 8.1 The Problem: V8 Call Frames Cannot Be Fabricated

The core challenge is that Chrome DevTools' **Call Stack** pane is populated exclusively from V8's actual JavaScript execution stack via the `Debugger.paused` CDP event's `callFrames[]` array. There is **no mechanism** — not through the CDP, not through extensions, not through in-page code — to inject synthetic or virtual call frames into this array.

Since the interpreter is a trampoline (a single `while(true)` loop), there is only ever **one** real V8 stack frame when paused at a Scheme probe. Previous Scheme call frames exist only as data in our `StackTracer`, not as real V8 frames. This means you can't click on them in the native Call Stack to navigate source and inspect variables.

### 8.2 Solution: Two Complementary Mechanisms (E+D)

We solve this with two mechanisms working together:

| Mechanism | What It Provides | Where It Appears |
|---|---|---|
| **D: Async Stack Tagging** (`console.createTask`) | "How did we get here?" hints in the native Call Stack | Native Call Stack pane, under "Async" separator |
| **E: Chrome DevTools Extension** with Sources sidebar | Full Scheme call stack with clickable navigation and per-frame variable inspection | Custom "Scheme Stack" sidebar in the Sources panel |

**Alternative D** provides lightweight, zero-risk orientation in the standard UI. **Alternative E** provides the authoritative, fully-interactive debugging experience.

### 8.3 Native Call Stack Appearance

When paused at a probe with both mechanisms active, the native DevTools Call Stack shows:

```
  ▼ Call Stack
    __scheme_L3 (factorial.scm:3)        ← Current probe (Scheme line)
    ─── Async ───────────────────────
    scheme: factorial                     ← console.createTask label (D)
    scheme: main                          ← Outer Scheme call (D)
    userCode (app.js:15)                  ← JS caller
```

With blackboxing, the interpreter internals (`maybeHit`, `run`, closure wrapper) are hidden. The async frames from Alternative D provide context but are **read-only**: clicking them jumps to the source location but does NOT provide scope/locals inspection.

### 8.4 Alternative D: Async Stack Tagging via `console.createTask`

#### 8.4.1 API Overview

Chrome 106+ provides `console.createTask(name)` which creates an async stack link:

```javascript
const task = console.createTask("scheme: factorial");
// Later, when executing factorial's body:
task.run(() => {
  // Code here shows "scheme: factorial" as an async ancestor in the Call Stack
  probe(envProxy);
});
```

DevTools displays these links under an "Async" header in the Call Stack pane, labeled with the name provided to `createTask`.

#### 8.4.2 Integration with Interpreter

When the interpreter enters a Scheme function call (in `AppFrame.step()` for closure application), we create a task. When firing the probe inside that function's body, we run it via `task.run()`:

```javascript
class DevToolsDebugIntegration {
  constructor(sourceRegistry) {
    // ... existing fields ...

    /** @type {Array<{task: Object, name: string}>} */
    this.taskStack = [];

    /** @type {boolean} */
    this.hasCreateTask = typeof console !== 'undefined'
                         && typeof console.createTask === 'function';
  }

  /**
   * Called by StackTracer hook when entering a Scheme function.
   * Creates an async task for this call frame.
   * @param {Object} frameInfo - { name, originalName, source }
   */
  onEnterFrame(frameInfo) {
    if (!this.hasCreateTask) return;

    const displayName = frameInfo.originalName || frameInfo.name || 'anonymous';
    const task = console.createTask(`scheme: ${displayName}`);
    this.taskStack.push({ task, name: displayName });
  }

  /**
   * Called by StackTracer hook when exiting a Scheme function.
   */
  onExitFrame() {
    if (!this.hasCreateTask) return;
    this.taskStack.pop();
  }

  /**
   * Called by StackTracer hook on TCO frame replacement.
   * Replaces the current task with a new one for the tail-called function.
   * @param {Object} frameInfo - New frame info
   */
  onReplaceFrame(frameInfo) {
    if (!this.hasCreateTask) return;
    if (this.taskStack.length > 0) {
      const displayName = frameInfo.originalName || frameInfo.name || 'anonymous';
      this.taskStack[this.taskStack.length - 1] = {
        task: console.createTask(`scheme: ${displayName}`),
        name: displayName
      };
    }
  }

  /**
   * Fires a probe, wrapping it in the current task chain if available.
   * @param {Function} probe - The probe function
   * @param {Proxy} envProxy - The environment proxy
   */
  fireProbe(probe, envProxy) {
    if (this.taskStack.length > 0 && this.hasCreateTask) {
      // Run inside the innermost task to create the async stack chain
      this.taskStack[this.taskStack.length - 1].task.run(() => {
        probe(envProxy);
      });
    } else {
      probe(envProxy);
    }
  }

  maybeHit(source, env) {
    const key = `${source.filename}:${source.line}`;
    if (key === this.lastHitKey) return;
    this.lastHitKey = key;

    const probe = this.sourceRegistry.getProbe(source.filename, source.line);
    if (!probe) return;

    if (env !== this.currentEnvRef) {
      this.currentEnvProxy = createEnvProxy(env);
      this.currentEnvRef = env;
    }

    // Use fireProbe instead of direct call to wrap with async task chain
    this.fireProbe(probe, this.currentEnvProxy);
  }
}
```

#### 8.4.3 Task Stack Management

The task stack mirrors the `StackTracer` frame stack. The hooks are wired through the existing `SchemeDebugRuntime`:

```javascript
// In SchemeDebugRuntime, when DevTools integration is active:
enterFrame(frameInfo) {
  this.stackTracer.enterFrame(frameInfo);
  this.devtoolsDebug?.onEnterFrame(frameInfo);
}

exitFrame() {
  this.stackTracer.exitFrame();
  this.devtoolsDebug?.onExitFrame();
}

replaceFrame(frameInfo) {
  this.stackTracer.replaceFrame(frameInfo);
  this.devtoolsDebug?.onReplaceFrame(frameInfo);
}
```

#### 8.4.4 Characteristics and Limitations

| Aspect | Behavior |
|---|---|
| **Visual appearance** | Shown under "Async" separator in native Call Stack |
| **Click behavior** | Jumps to source location (via source map) |
| **Scope/locals** | **NOT available** — clicking does not populate the Scope pane |
| **TCO** | Task is replaced on tail call, so label stays current |
| **Depth** | Chrome may truncate deep async stacks (typically 32 frames) |
| **Fallback** | Gracefully no-ops if `console.createTask` is unavailable |
| **Overhead** | Negligible — one object allocation per function entry |

### 8.5 Alternative E: Chrome DevTools Extension — Scheme Stack Sidebar

#### 8.5.1 Overview

A Chrome DevTools extension adds a **"Scheme Stack"** sidebar pane directly inside the Sources panel, appearing alongside the native Scope, Watch, and Call Stack panes. This sidebar is the primary, authoritative interface for navigating the Scheme call stack and inspecting variables across frames.

#### 8.5.2 Extension Architecture

```
extension/
  ├── manifest.json           # Extension manifest (Manifest V3)
  ├── devtools.html            # DevTools page (entry point)
  ├── devtools.js              # Creates sidebar pane
  ├── panel/
  │   ├── sidebar.html         # Sidebar UI (frame list + variables)
  │   ├── sidebar.js           # Sidebar logic (data fetching, interaction)
  │   └── sidebar.css          # Sidebar styling
  └── background.js            # Service worker (CDP event listener)
```

#### 8.5.3 Extension Manifest

```json
{
  "manifest_version": 3,
  "name": "Scheme-JS Debugger",
  "version": "1.0",
  "description": "Scheme call stack and variable inspection for scheme-js",
  "devtools_page": "devtools.html",
  "permissions": ["debugger"],
  "icons": {
    "48": "icons/icon48.png",
    "128": "icons/icon128.png"
  }
}
```

#### 8.5.4 Sidebar Pane Creation

```javascript
// devtools.js — executed when DevTools opens for a page
chrome.devtools.panels.sources.createSidebarPane(
  "Scheme Stack",
  (sidebar) => {
    // Load the sidebar UI
    sidebar.setPage("panel/sidebar.html");

    // Store reference for communication
    window.schemeSidebar = sidebar;
  }
);
```

#### 8.5.5 Data Flow on Pause

```
  ┌───────────────────────────────────────────────────────────────────────┐
  │                          Chrome DevTools                              │
  │                                                                       │
  │  Sources Panel                                                        │
  │  ┌─────────────────────┐  ┌──────────────────────────────────────┐   │
  │  │   Editor             │  │  Sidebar: "Scheme Stack"             │   │
  │  │   factorial.scm:3 ◄──│──│─── [selected frame highlights here] │   │
  │  │   highlighted         │  │                                      │   │
  │  │                       │  │  ▶ factorial (factorial.scm:3)  ←top │   │
  │  │                       │  │    apply-op (utils.scm:12)          │   │
  │  │                       │  │    main (app.scm:45)                │   │
  │  │                       │  │                                      │   │
  │  │                       │  │  ─── Variables ──────────────────── │   │
  │  │                       │  │    n: 5                              │   │
  │  │                       │  │    result: 120                       │   │
  │  │                       │  │    factorial: #<procedure>           │   │
  │  └─────────────────────┘  └──────────────────────────────────────┘   │
  └────────────────────┬──────────────────────────────┬──────────────────┘
                       │ V8 Debugger.paused            │ inspectedWindow.eval()
                       │                               │
  ┌────────────────────▼───────────────────────────────▼──────────────────┐
  │                        Page Context                                   │
  │                                                                       │
  │  window.__schemeDebug = {                                             │
  │    getStack()       → StackTracer.getStack()                         │
  │    getFrameEnv(i)   → StackTracer.frames[i].env                     │
  │    getLocals(i)     → StateInspector.getLocals(frames[i].env)       │
  │    getSource(i)     → frames[i].source                               │
  │  }                                                                    │
  └───────────────────────────────────────────────────────────────────────┘
```

**Sequence of events when the debugger pauses:**

1. V8 hits a breakpoint inside a probe function
2. The extension's background script detects the `Debugger.paused` event via `chrome.debugger` API
3. It sends a message to the sidebar page
4. The sidebar calls `chrome.devtools.inspectedWindow.eval()` to execute `__schemeDebug.getStack()` in the page context
5. The returned stack data is rendered as a clickable frame list
6. The top frame's variables are displayed automatically

#### 8.5.6 The `__schemeDebug` Page-Side API

This global object is installed by the DevTools integration module and provides the bridge between the extension and the interpreter's internal state:

```javascript
/**
 * Installed on window when DevTools integration is enabled.
 * Provides data access for the Chrome DevTools extension.
 */
window.__schemeDebug = {
  /**
   * Gets the current Scheme call stack.
   * @returns {Array<{name: string, source: {filename: string, line: number, column: number}|null, tcoCount: number}>}
   */
  getStack() {
    const frames = interpreter.debugRuntime?.stackTracer.getStack() || [];
    return frames.map(f => ({
      name: f.name,
      source: f.source,
      tcoCount: f.tcoCount
    }));
  },

  /**
   * Gets the local variable bindings for a specific stack frame.
   * @param {number} frameIndex - Index into the stack (0 = bottom, length-1 = top)
   * @returns {Array<{name: string, value: string, type: string}>}
   */
  getLocals(frameIndex) {
    const frames = interpreter.debugRuntime?.stackTracer.getStack() || [];
    if (frameIndex < 0 || frameIndex >= frames.length) return [];
    const env = frames[frameIndex].env;
    const inspector = interpreter.debugRuntime?.stateInspector;
    if (!env || !inspector) return [];

    const locals = inspector.getLocals(env);
    const result = [];
    for (const [name, value] of locals) {
      const serialized = inspector.serializeValue(value);
      result.push({
        name,
        value: serialized.description || String(value),
        type: serialized.type,
        subtype: serialized.subtype || null
      });
    }
    return result;
  },

  /**
   * Gets source info for a specific stack frame.
   * @param {number} frameIndex
   * @returns {{filename: string, line: number, column: number}|null}
   */
  getSource(frameIndex) {
    const frames = interpreter.debugRuntime?.stackTracer.getStack() || [];
    if (frameIndex < 0 || frameIndex >= frames.length) return null;
    return frames[frameIndex].source || null;
  },

  /**
   * Evaluates a Scheme expression in the context of a specific stack frame.
   * @param {string} code - Scheme expression to evaluate
   * @param {number} [frameIndex] - Frame index (defaults to top frame)
   * @returns {string} Result as a string
   */
  eval(code, frameIndex) {
    const frames = interpreter.debugRuntime?.stackTracer.getStack() || [];
    const idx = frameIndex ?? (frames.length - 1);
    if (idx < 0 || idx >= frames.length) return '#<error: invalid frame>';
    const env = frames[idx].env;
    try {
      const result = interpreter.evaluateInEnv(code, env);
      return String(result);
    } catch (e) {
      return `#<error: ${e.message}>`;
    }
  }
};
```

#### 8.5.7 Sidebar UI Behavior

**Frame List:**

The sidebar renders the Scheme call stack as a list of clickable frames, ordered top-to-bottom (most recent first, matching DevTools convention):

```html
<!-- Rendered frame list (simplified) -->
<div class="scheme-stack">
  <div class="frame selected" data-index="2">
    <span class="frame-name">factorial</span>
    <span class="frame-location">factorial.scm:3</span>
  </div>
  <div class="frame" data-index="1">
    <span class="frame-name">apply-op</span>
    <span class="frame-location">utils.scm:12</span>
    <span class="tco-badge" title="2 tail calls">TCO×2</span>
  </div>
  <div class="frame" data-index="0">
    <span class="frame-name">main</span>
    <span class="frame-location">app.scm:45</span>
  </div>
</div>
```

**Clicking a Frame:**

When the user clicks a non-top frame:

1. The sidebar marks it as "selected" (visual highlight)
2. Calls `chrome.devtools.panels.openResource(source.filename, source.line - 1)` to navigate the Sources editor to that frame's source location
3. Fetches that frame's local variables via `__schemeDebug.getLocals(frameIndex)` and displays them in the Variables section below the frame list

```javascript
// sidebar.js — frame click handler
async function onFrameClick(frameIndex) {
  // Highlight selected frame
  document.querySelectorAll('.frame').forEach(el => el.classList.remove('selected'));
  document.querySelector(`[data-index="${frameIndex}"]`).classList.add('selected');

  // Navigate editor to frame's source
  const source = await evalInPage(`__schemeDebug.getSource(${frameIndex})`);
  if (source) {
    chrome.devtools.panels.openResource(source.filename, source.line - 1);
  }

  // Show frame's variables
  const locals = await evalInPage(`__schemeDebug.getLocals(${frameIndex})`);
  renderVariables(locals);
}

/**
 * Evaluates an expression in the inspected page's context.
 * @param {string} expr - JavaScript expression to evaluate
 * @returns {Promise<*>} The result
 */
function evalInPage(expr) {
  return new Promise((resolve, reject) => {
    chrome.devtools.inspectedWindow.eval(
      expr,
      (result, error) => error ? reject(error) : resolve(result)
    );
  });
}
```

**Variables Display:**

The variables section shows key-value pairs for the selected frame's lexical environment:

```html
<div class="scheme-variables">
  <div class="section-header">Variables</div>
  <div class="variable">
    <span class="var-name">n</span>
    <span class="var-value number">5</span>
  </div>
  <div class="variable">
    <span class="var-name">result</span>
    <span class="var-value number">120</span>
  </div>
  <div class="variable">
    <span class="var-name">factorial</span>
    <span class="var-value function">#&lt;procedure factorial&gt;</span>
  </div>
</div>
```

#### 8.5.8 Scope Inspection: Top Frame vs. Historical Frames

A key distinction in the E+D design:

| Frame | Native Scope Pane | Extension Sidebar Variables |
|---|---|---|
| **Top frame** (current probe) | ✅ Full native inspection via `with(envProxy)` — expandable objects, hover evaluation, Watch expressions all work | ✅ Also shown (redundant but consistent) |
| **Non-top frames** | ❌ Not available — native Scope pane can only show the active V8 frame | ✅ Displayed via `__schemeDebug.getLocals(i)` — text-based rendering of variable names and values |

For the top frame, the user gets the **best of both worlds**: native DevTools Scope pane with full interactivity (property expansion, hover evaluation, Watch expressions), plus the extension sidebar showing the same data in a Scheme-aware format.

For non-top frames, only the extension sidebar provides variable inspection. The values are serialized as text descriptions (e.g., `#<procedure factorial>`, `(1 2 3)`) using the existing `StateInspector.serializeValue()` method. This is less interactive than the native Scope pane but still provides full visibility into the lexical environment at each call frame.

#### 8.5.9 Detecting Pause/Resume in the Extension

The extension uses the `chrome.debugger` API to monitor debugging events:

```javascript
// background.js — monitors for Debugger.paused events

let attachedTabId = null;

// Attach to the inspected tab when DevTools opens
chrome.runtime.onMessage.addListener((msg, sender, sendResponse) => {
  if (msg.type === 'attach') {
    attachedTabId = msg.tabId;
    chrome.debugger.attach({ tabId: attachedTabId }, '1.3', () => {
      chrome.debugger.sendCommand(
        { tabId: attachedTabId },
        'Debugger.enable'
      );
    });
  }
});

// Listen for debugger events
chrome.debugger.onEvent.addListener((source, method, params) => {
  if (source.tabId !== attachedTabId) return;

  if (method === 'Debugger.paused') {
    // Check if this is a Scheme probe pause by inspecting the top frame
    const topFrame = params.callFrames?.[0];
    const isSchemeProbe = topFrame?.functionName?.startsWith('__scheme_L');

    // Notify the sidebar
    chrome.runtime.sendMessage({
      type: 'debugger-paused',
      isSchemeProbe,
      callFrames: params.callFrames,
      reason: params.reason
    });
  }

  if (method === 'Debugger.resumed') {
    chrome.runtime.sendMessage({ type: 'debugger-resumed' });
  }
});
```

The sidebar listens for these messages and refreshes its display:

```javascript
// sidebar.js — listens for pause/resume
chrome.runtime.onMessage.addListener((msg) => {
  if (msg.type === 'debugger-paused' && msg.isSchemeProbe) {
    refreshSchemeStack();
  }
  if (msg.type === 'debugger-resumed') {
    clearDisplay();
  }
});

async function refreshSchemeStack() {
  const stack = await evalInPage('JSON.stringify(__schemeDebug.getStack())');
  const frames = JSON.parse(stack);
  renderFrameList(frames);

  // Auto-select top frame and show its variables
  if (frames.length > 0) {
    onFrameClick(frames.length - 1);
  }
}
```

#### 8.5.10 TCO Display

Frames that have been replaced by tail calls show a TCO badge with the replacement count:

```javascript
function renderFrameList(frames) {
  const container = document.querySelector('.scheme-stack');
  container.innerHTML = '';

  // Render top-to-bottom (reverse of StackTracer's bottom-to-top order)
  for (let i = frames.length - 1; i >= 0; i--) {
    const frame = frames[i];
    const el = document.createElement('div');
    el.className = 'frame' + (i === frames.length - 1 ? ' selected' : '');
    el.dataset.index = i;

    let html = `
      <span class="frame-name">${escapeHtml(frame.name)}</span>
      <span class="frame-location">${formatSource(frame.source)}</span>
    `;

    if (frame.tcoCount > 0) {
      html += `<span class="tco-badge" title="${frame.tcoCount} tail call replacement(s)">TCO×${frame.tcoCount}</span>`;
    }

    el.innerHTML = html;
    el.addEventListener('click', () => onFrameClick(i));
    container.appendChild(el);
  }
}
```

### 8.6 Enhanced Console API (`window.__schemeDebug`)

In addition to serving the extension, the `__schemeDebug` global is available directly in the DevTools Console for manual inspection:

```javascript
// Available in DevTools console when paused at a Scheme probe:

__schemeDebug.getStack()
// → [{name: "main", source: {filename: "app.scm", line: 45}, tcoCount: 0},
//    {name: "apply-op", source: {filename: "utils.scm", line: 12}, tcoCount: 2},
//    {name: "factorial", source: {filename: "factorial.scm", line: 3}, tcoCount: 0}]

__schemeDebug.getLocals(2)
// → [{name: "n", value: "5", type: "number"},
//    {name: "result", value: "120", type: "number"}]

__schemeDebug.eval("(+ n 1)")
// → "6"

__schemeDebug.eval("(list 1 2 3)", 0)  // Evaluate in main's scope
// → "(1 2 3)"
```

### 8.7 Interaction Diagram: E+D Combined

```
  User clicks "Step Into" on mySchemeFunc(42) in JS
        │
        ▼
  V8 steps through closure wrapper (blackboxed) → run() → trampoline
        │
        │  Trampoline iteration: enters factorial's body
        │
        ▼
  StackTracer.enterFrame({name: "factorial", source: ...})
        │
        ├──► DevToolsDebugIntegration.onEnterFrame()
        │    └──► console.createTask("scheme: factorial")  ← Alt D
        │
        ▼
  maybeHit(source={factorial.scm:1}, env)
        │
        ▼
  fireProbe: task.run(() => probe(envProxy))
        │
        ├──► V8 breakpoint hit → Debugger.paused
        │
        ├──► Native Call Stack shows:                        ← Alt D
        │      __scheme_L1 (factorial.scm:1)
        │      ─── Async ───
        │      scheme: factorial
        │      userCode (app.js:15)
        │
        ├──► Native Scope pane shows:                        ← Probes
        │      n: 5, factorial: ƒ
        │
        └──► Extension sidebar refreshes:                    ← Alt E
               [Scheme Stack]
               ▶ factorial (factorial.scm:1)   ← selected
                 main (app.scm:45)
               [Variables]
                 n: 5
                 factorial: #<procedure>
```

---

## 9. Exception Handling

### 9.1 Break on Scheme Exceptions

When a Scheme exception (via `raise`) occurs:

1. The existing `DebugExceptionHandler.shouldBreakOnException()` fires
2. If DevTools integration is active, we fire a probe at the exception source location
3. Additionally, we can `throw` a special `SchemeException` JS error that DevTools catches via its "Pause on Exceptions" feature

```javascript
// In the exception path:
if (devtoolsDebug.enabled && devtoolsDebug.breakOnExceptions) {
  // Fire probe at exception location so DevTools shows the right Scheme line
  const probe = sourceRegistry.getProbe(source.filename, source.line);
  if (probe) {
    probe(envProxy);
    // Throw a JS Error that DevTools can catch
    throw new SchemeRuntimeException(exception, source);
  }
}
```

### 9.2 SchemeRuntimeException Class

```javascript
class SchemeRuntimeException extends Error {
  constructor(schemeError, source) {
    const message = schemeError.message || String(schemeError);
    super(`Scheme Error: ${message} at ${source.filename}:${source.line}`);
    this.name = 'SchemeError';
    this.schemeValue = schemeError;
    this.schemeSource = source;
  }
}
```

DevTools' "Pause on Caught/Uncaught Exceptions" then works naturally.

---

## 10. Script Tag & File Loading

### 10.1 Updated `html_adapter.js`

The HTML adapter is updated to register sources with the `SchemeSourceRegistry`:

```javascript
import { schemeEvalAsync, interpreter } from './scheme_entry.js';
import { SchemeSourceRegistry } from '../debug/devtools/source_registry.js';

const sourceRegistry = new SchemeSourceRegistry();

async function runScripts() {
  const scripts = document.querySelectorAll('script[type="text/scheme"]');
  let inlineIndex = 0;

  for (const script of scripts) {
    try {
      let code, url;

      if (script.src) {
        const response = await fetch(script.src);
        code = await response.text();
        url = `scheme://app/${new URL(script.src, location.href).pathname}`;
      } else {
        code = script.textContent;
        url = `scheme://inline/script-${inlineIndex++}.scm`;
      }

      // Register source and generate probe script
      sourceRegistry.register(url, code, script.src ? 'external' : 'inline');

      // Execute with source tracking
      await schemeEvalAsync(code, { sourceId: url });
    } catch (err) {
      console.error('Error executing Scheme script:', err);
    }
  }
}
```

### 10.2 Source ID Propagation

The `sourceId` (canonical URL) must be propagated through the reader → analyzer → AST chain so that all AST nodes carry the correct `source.filename` matching the probe script's URL:

1. **Reader/Tokenizer:** Accept a `sourceId` parameter, attach to all tokens
2. **Parser:** Propagate `sourceId` into `source` objects on parsed S-expressions
3. **Analyzer:** Propagate `source` from S-expressions to AST nodes
4. **AST Nodes:** Already carry `.source` — just needs correct `filename`

---

## 11. DevTools Blackboxing

### 11.1 Why Blackboxing is Important

Without blackboxing, "Step Into" from JS to Scheme will traverse through interpreter internals (`values.js` closure wrapper → `interpreter.js` `run()` → `frames.js` AppFrame → etc.) before reaching the probe. Blackboxing hides these frames.

### 11.2 Blackbox Patterns

Users should blackbox the following scripts:

| File | Contains |
|---|---|
| `interpreter.js` | Trampoline loop |
| `frames.js` | Frame step methods |
| `ast_nodes.js` | AST node step methods |
| `values.js` | Closure/continuation wrappers |
| `stepables_base.js` | Base Executable class |
| `frame_registry.js` | Frame factories |
| `devtools_debug.js` | DevTools integration (maybeHit) |

### 11.3 Auto-Blackboxing (Optional Extension)

A lightweight Chrome extension can auto-configure blackbox patterns:

```javascript
// background.js for chrome extension
chrome.debugger.onEvent.addListener((source, method, params) => {
  if (method === 'Debugger.scriptParsed') {
    const url = params.url;
    if (isSchemeRuntime(url)) {
      chrome.debugger.sendCommand(source, 'Debugger.setBlackboxPatterns', {
        patterns: [
          '.*interpreter\\.js$',
          '.*frames\\.js$',
          '.*ast_nodes\\.js$',
          '.*values\\.js$',
          '.*stepables_base\\.js$',
          '.*frame_registry\\.js$',
          '.*devtools_debug.*\\.js$'
        ]
      });
    }
  }
});
```

### 11.4 Manual Blackboxing Instructions

For users without the extension, we provide documentation:

> 1. Open Chrome DevTools → Settings (gear icon) → Ignore List
> 2. Enable "Add content scripts to ignore list"
> 3. Add patterns: `/interpreter\.js/`, `/frames\.js/`, `/ast_nodes\.js/`, `/values\.js/`

---

## 12. Performance Design

### 12.1 Zero Overhead When Disabled

```javascript
// In interpreter.js run():
if (this.devtoolsDebug) {  // null check — optimized away by JIT
  // ... probe check ...
}
```

When `devtoolsDebug` is `null` (the default), the JIT compiler eliminates the branch entirely. **Measured overhead: 0%.**

### 12.2 Overhead When Enabled

The probe hit check runs on every trampoline step but short-circuits quickly:

```javascript
maybeHit(source, env) {
  const key = `${source.filename}:${source.line}`;
  if (key === this.lastHitKey) return;  // ← ~90% of calls exit here
  // ... rest only runs on source line changes ...
}
```

**Cost per step (no line change):** One string concatenation + one comparison = ~5ns
**Cost per line change:** Probe lookup (Map.get) + envProxy creation + probe call = ~500ns

**Estimated overhead with DevTools enabled:** 5-15%, well within the 20% budget.

### 12.3 Lazy Probe Generation

Probe scripts are only generated when DevTools integration is enabled. If the user never opens DevTools, no probes are created.

### 12.4 Probe Caching

Probe functions are created once per source file and cached in the `SchemeSourceRegistry`. The `envProxy` is cached and only rebuilt when the environment object reference changes.

---

## 13. UI/UX Flow

### 13.1 User Story: Setting a Breakpoint and Stepping

```
1. Developer loads page with <script type="text/scheme" src="app.scm">
2. Opens Chrome DevTools → Sources panel
3. In the file tree, sees:
   ├── (page resources)
   └── scheme://app/app.scm        ← Scheme source file
4. Clicks on app.scm → sees Scheme source code with line numbers
5. Clicks line 5 gutter → breakpoint set (blue marker appears)
6. Interacts with the page, triggering Scheme execution
7. Execution pauses at line 5:
   - Source panel highlights line 5 in app.scm
   - Scope pane shows: n=5, result=120
   - Call Stack shows: __scheme_L5 (app.scm:5)
8. Clicks "Step Over" → moves to line 6 in app.scm
9. Clicks "Step Into" on (js-call ...) → enters JS source
10. Clicks "Step Out" → returns to next Scheme line
11. Clicks "Resume" → execution continues
```

### 13.2 User Story: JS → Scheme Debugging

```
1. Developer has JS code: let result = schemeAdd(1, 2);
2. Sets breakpoint on that JS line
3. Execution pauses in JS
4. Clicks "Step Into"
5. (With blackboxing) Immediately sees add.scm line 1
6. Steps through Scheme code
7. When Scheme function returns, back in JS at next line
```

### 13.3 Enabling DevTools Integration

```javascript
// Option 1: Explicit API
import { enableDevToolsDebug } from 'scheme-js';
enableDevToolsDebug();  // Call before running Scheme code

// Option 2: Script tag attribute
<script type="text/scheme" debug>
  (define (factorial n) ...)
</script>

// Option 3: URL parameter
// page.html?scheme-debug=true

// Option 4: Global flag
window.__SCHEME_JS_DEBUG = true;
```

---

## 14. Node.js / Future DAP Support

### 14.1 Modular Architecture

The probe-based approach is browser-specific (it relies on injecting `<script>` tags). For Node.js, we use a different backend:

```
src/debug/
  ├── devtools/                    # Chrome DevTools integration (browser)
  │   ├── devtools_debug.js        # DevToolsDebugIntegration class
  │   ├── source_registry.js       # SchemeSourceRegistry
  │   ├── probe_generator.js       # Probe script generation
  │   ├── sourcemap_generator.js   # Source map generation
  │   └── env_proxy.js             # Environment proxy for with()
  │
  ├── node/                        # Node.js Inspector integration (future)
  │   └── inspector_backend.js     # Uses node:inspector API
  │
  └── dap/                         # DAP server (future - VS Code)
      └── dap_server.js            # Debug Adapter Protocol
```

### 14.2 Shared Interface

Both browser and Node.js backends share the `DebugBackend` interface and the core `SchemeDebugRuntime`. The DevTools integration adds a parallel pathway (probes) that doesn't replace the existing debug runtime — it augments it.

---

## 15. File & Module Structure

### 15.1 New Files

```
src/debug/devtools/
  ├── index.js                     # Barrel export
  ├── devtools_debug.js            # DevToolsDebugIntegration (main coordinator)
  │                                  - maybeHit(), fireProbe(), task stack (Alt D)
  │                                  - __schemeDebug global installation (Alt E)
  ├── source_registry.js           # SchemeSourceRegistry
  ├── probe_generator.js           # generateProbeScript()
  ├── sourcemap_generator.js       # generateProbeSourceMap(), VLQ encoder
  └── env_proxy.js                 # createEnvProxy(), formatForDevTools()

extension/                         # Chrome DevTools Extension (Alt E)
  ├── manifest.json                # Manifest V3
  ├── devtools.html                # DevTools page entry point
  ├── devtools.js                  # Creates "Scheme Stack" sidebar pane
  ├── background.js                # Service worker: CDP event listener
  │                                  - Attaches to inspected tab
  │                                  - Listens for Debugger.paused/resumed
  │                                  - Routes events to sidebar
  ├── panel/
  │   ├── sidebar.html             # Sidebar UI
  │   ├── sidebar.js               # Frame list rendering, click handlers,
  │   │                              variable display, evalInPage() helper
  │   └── sidebar.css              # Sidebar styling (dark theme matching DevTools)
  └── icons/
      ├── icon48.png
      └── icon128.png
```

### 15.2 Modified Files

| File | Change |
|---|---|
| `src/core/interpreter/interpreter.js` | Add `devtoolsDebug` field, probe hook in `run()` and `runDebug()` |
| `src/packaging/html_adapter.js` | Register sources, pass `sourceId` |
| `src/packaging/scheme_entry.js` | Export DevTools integration API |
| `src/core/interpreter/reader/tokenizer.js` | Accept `sourceId` parameter |
| `src/core/interpreter/reader/parser.js` | Propagate `sourceId` to source info |
| `src/debug/index.js` | Export DevTools modules |
| `src/debug/scheme_debug_runtime.js` | Wire StackTracer hooks to DevToolsDebugIntegration (enter/exit/replaceFrame → onEnterFrame/onExitFrame/onReplaceFrame) |

### 15.3 Updated Architecture Diagram

Add to `docs/architecture.md`:

```
│   └── debug/                  # Debugger Runtime & Tools
│      ├── ...                  # (existing files)
│      └── devtools/            # Chrome DevTools Integration
│          ├── index.js         # Barrel export
│          ├── devtools_debug.js # Main coordinator (probes + Alt D tasks + Alt E API)
│          ├── source_registry.js # Source & probe management
│          ├── probe_generator.js # Probe script generation
│          ├── sourcemap_generator.js # V3 source map generation
│          └── env_proxy.js     # Environment proxy for Scope pane
│
├── extension/                  # Chrome DevTools Extension (Alt E)
│   ├── manifest.json           # Manifest V3
│   ├── devtools.html/js        # Sidebar pane creation
│   ├── background.js           # CDP event routing
│   └── panel/                  # Sidebar UI (HTML + JS + CSS)
```

---

## 16. Implementation Plan

### Phase 1: Foundation — Source Registration & Probe Generation
**Goal:** Scheme source appears in Chrome DevTools Sources panel.

| Step | Task | Files | Est. |
|---|---|---|---|
| 1.1 | Implement VLQ encoder and `sourcemap_generator.js` | New: `sourcemap_generator.js` | 0.5 days |
| 1.2 | Implement `probe_generator.js` — generates probe JS + inline source map | New: `probe_generator.js` | 1 day |
| 1.3 | Implement `SchemeSourceRegistry` — registers sources, injects probe `<script>` tags | New: `source_registry.js` | 0.5 days |
| 1.4 | Update `html_adapter.js` to register sources with `sourceId` | Mod: `html_adapter.js` | 0.5 days |
| 1.5 | Update reader/tokenizer to propagate `sourceId` through parse → analyze → AST | Mod: `tokenizer.js`, `parser.js` | 0.5 days |
| 1.6 | Write tests: probe generation, source map accuracy, source registration | New: test files | 1 day |
| 1.7 | Manual verification: Scheme files appear in DevTools Sources | — | 0.5 days |

**Milestone:** Open DevTools → Sources → see Scheme file with correct content.

### Phase 2: Trampoline Integration & Stepping
**Goal:** Stepping through Scheme code works in DevTools.

| Step | Task | Files | Est. |
|---|---|---|---|
| 2.1 | Implement `DevToolsDebugIntegration` class with `maybeHit()` | New: `devtools_debug.js` | 1 day |
| 2.2 | Add probe hook to `interpreter.js` `run()` loop | Mod: `interpreter.js` | 0.5 days |
| 2.3 | Add probe hook to `interpreter.js` `runDebug()` loop | Mod: `interpreter.js` | 0.5 days |
| 2.4 | Add immediate probe-on-entry at top of `run()`/`runDebug()` | Mod: `interpreter.js` | 0.25 days |
| 2.5 | Barrel exports and integration API | New: `devtools/index.js`, Mod: `debug/index.js`, `scheme_entry.js` | 0.25 days |
| 2.6 | Write tests: stepping progression, line tracking | New: test files | 1 day |
| 2.7 | Manual verification: Step Into/Over/Out works in DevTools | — | 0.5 days |

**Milestone:** Set breakpoint on Scheme line → execution pauses → Step Over advances to next Scheme line.

### Phase 3: Variable Inspection
**Goal:** Scheme variables visible in DevTools Scope pane.

| Step | Task | Files | Est. |
|---|---|---|---|
| 3.1 | Implement `createEnvProxy()` with Proxy handler | New: `env_proxy.js` | 1 day |
| 3.2 | Implement `formatForDevTools()` value converter | New: `env_proxy.js` | 0.5 days |
| 3.3 | Wire env proxy into probe calls in `maybeHit()` | Mod: `devtools_debug.js` | 0.25 days |
| 3.4 | Handle name mapping (alpha-renamed → original names) | Mod: `env_proxy.js` | 0.5 days |
| 3.5 | Write tests: proxy enumeration, value formatting | New: test files | 0.5 days |
| 3.6 | Manual verification: variables appear in Scope pane | — | 0.5 days |

**Milestone:** Paused at Scheme breakpoint → Scope pane shows `n: 5`, `factorial: ƒ factorial`.

### Phase 4: Async Stack Tagging (Alternative D)
**Goal:** Scheme call context visible in native Call Stack via async frames.

| Step | Task | Files | Est. |
|---|---|---|---|
| 4.1 | Add task stack management to `DevToolsDebugIntegration` (onEnterFrame, onExitFrame, onReplaceFrame) | Mod: `devtools_debug.js` | 0.5 days |
| 4.2 | Implement `fireProbe()` with `task.run()` wrapping | Mod: `devtools_debug.js` | 0.25 days |
| 4.3 | Wire StackTracer hooks in `SchemeDebugRuntime` to DevToolsDebug | Mod: `scheme_debug_runtime.js` | 0.25 days |
| 4.4 | Feature-detect `console.createTask` with graceful fallback | Mod: `devtools_debug.js` | 0.25 days |
| 4.5 | Write tests: task stack mirrors StackTracer, TCO replacement | New: test files | 0.5 days |
| 4.6 | Manual verification: async frames appear in native Call Stack | — | 0.5 days |

**Milestone:** Paused at Scheme breakpoint → native Call Stack shows `scheme: factorial` under "Async" separator.

### Phase 5: DevTools Extension — Scheme Stack Sidebar (Alternative E)
**Goal:** Full Scheme call stack with clickable navigation and per-frame variable inspection.

| Step | Task | Files | Est. |
|---|---|---|---|
| 5.1 | Extension scaffold: `manifest.json`, `devtools.html`, `devtools.js` with `createSidebarPane` | New: `extension/` | 0.5 days |
| 5.2 | Install `__schemeDebug` global API (getStack, getLocals, getSource, eval) | Mod: `devtools_debug.js` | 0.5 days |
| 5.3 | `background.js`: attach CDP, listen for `Debugger.paused`/`resumed`, route to sidebar | New: `extension/background.js` | 0.5 days |
| 5.4 | `sidebar.html`/`sidebar.js`: render frame list, click handler with `openResource()` | New: `extension/panel/` | 1 day |
| 5.5 | Variables display: fetch `getLocals(frameIndex)`, render per-frame bindings | Mod: `extension/panel/sidebar.js` | 0.5 days |
| 5.6 | TCO badge rendering and display name formatting | Mod: `extension/panel/sidebar.js` | 0.25 days |
| 5.7 | Dark theme CSS matching DevTools look and feel | New: `extension/panel/sidebar.css` | 0.25 days |
| 5.8 | Auto-blackboxing integration (set ignore patterns via CDP) | Mod: `extension/background.js` | 0.25 days |
| 5.9 | Write extension tests: mock CDP events, verify sidebar behavior | New: test files | 0.5 days |
| 5.10 | Manual verification: full E2E with breakpoints, frame clicking, variable inspection | — | 0.5 days |

**Milestone:** Paused at Scheme breakpoint → Scheme Stack sidebar shows full call stack → click frame → editor navigates to source → sidebar shows that frame's variables.

### Phase 6: Boundary Handling & Exception Integration
**Goal:** Seamless JS ↔ Scheme stepping transitions + exception support.

| Step | Task | Files | Est. |
|---|---|---|---|
| 6.1 | Test and document JS → Scheme boundary with blackboxing | — | 0.5 days |
| 6.2 | Test and document Scheme → JS boundary | — | 0.5 days |
| 6.3 | Document manual blackboxing instructions | New: docs | 0.25 days |
| 6.4 | Handle edge cases: `call/cc` across boundaries, `dynamic-wind` | Mod: `devtools_debug.js` | 1 day |
| 6.5 | `SchemeRuntimeException` class and exception probing | New/Mod: `devtools_debug.js` | 0.5 days |
| 6.6 | Break-on-exception integration with DevTools | Mod: `interpreter.js`, `devtools_debug.js` | 0.5 days |
| 6.7 | Write integration tests for boundary and exception scenarios | New: test files | 1 day |

**Milestone:** Step from JS into Scheme and back seamlessly. Scheme exceptions pause at correct source line.

### Phase 7: Polish & Documentation
**Goal:** Production-ready debugging experience.

| Step | Task | Files | Est. |
|---|---|---|---|
| 7.1 | Performance benchmarking and optimization | — | 1 day |
| 7.2 | REPL source registration (dynamic eval probes) | Mod: `source_registry.js`, `scheme_entry.js` | 0.5 days |
| 7.3 | Library `.sld`/`.scm` source registration | Mod: `html_adapter.js`, `library_loader.js` | 0.5 days |
| 7.4 | Extension distribution packaging (Chrome Web Store or local .crx) | New: build scripts | 0.5 days |
| 7.5 | Usage guide and documentation | New: docs | 0.5 days |
| 7.6 | End-to-end manual testing scenarios (all 13 scenarios from §17.3) | — | 1 day |

**Milestone:** Complete, polished DevTools debugging experience.

### Phase 8: (Future) Advanced Features
| Task | Est. |
|---|---|
| Node.js `inspector` backend | 2+ days |
| DAP server for VS Code | 3+ days |
| Column-level probe granularity | 1 day |
| Expandable objects in extension sidebar (tree view) | 1+ day |
| Scheme expression evaluation in extension sidebar | 1 day |
| Time-travel debugging via state serialization | 3+ days |

### Total Estimated Timeline

| Phase | Duration |
|---|---|
| Phase 1: Foundation | ~4.5 days |
| Phase 2: Trampoline Integration | ~4 days |
| Phase 3: Variable Inspection | ~3.25 days |
| Phase 4: Async Stack Tagging (Alt D) | ~2.25 days |
| Phase 5: Extension Sidebar (Alt E) | ~4.75 days |
| Phase 6: Boundary & Exceptions | ~4.25 days |
| Phase 7: Polish | ~4 days |
| **Total MVP (Phases 1-4)** | **~14 days** |
| **Full Implementation (Phases 1-7)** | **~27 days** |

---

## 17. Testing Strategy

### 17.1 Unit Tests

| Test File | What It Tests |
|---|---|
| `tests/unit/sourcemap_generator_tests.js` | VLQ encoding, source map structure |
| `tests/unit/probe_generator_tests.js` | Probe script structure, correct line mapping |
| `tests/unit/source_registry_tests.js` | Source registration, probe lookup |
| `tests/unit/env_proxy_tests.js` | Proxy has/get/ownKeys with Scheme envs |
| `tests/unit/devtools_debug_tests.js` | `maybeHit()` line-change tracking, caching, `fireProbe()` |
| `tests/unit/devtools_task_stack_tests.js` | Task stack management: enter/exit/replace mirror StackTracer, `console.createTask` fallback |
| `tests/unit/scheme_debug_api_tests.js` | `__schemeDebug` global: getStack, getLocals, getSource, eval |

### 17.2 Integration Tests

| Test | What It Verifies |
|---|---|
| Stepping progression | Source line advances correctly through multi-line Scheme code |
| Breakpoint accuracy | Probe fires at correct Scheme lines |
| Environment inspection | Proxy returns correct values for active bindings |
| Boundary detection | Source changes correctly across JS/Scheme transitions |
| Exception probing | Exception fires probe at correct location |
| Task stack sync | Task stack stays in sync with StackTracer through enter/exit/TCO |
| `__schemeDebug.getLocals` | Returns correct variable names and values for each frame index |
| Extension sidebar render | Frame list renders correctly from mock stack data (extension unit test) |
| Frame click navigation | `openResource()` called with correct URL and line on frame click |

### 17.3 Manual Testing Scenarios

1. **Basic breakpoint:** Set breakpoint in factorial.scm line 3 → verify pause
2. **Step Over:** Step through 5 lines of a Scheme function → verify line progression
3. **Step Into (Scheme→Scheme):** Step into a nested Scheme function call
4. **Step Into (JS→Scheme):** From JS, step into a Scheme closure call
5. **Step Into (Scheme→JS):** From Scheme, step into `(js-eval "...")`
6. **Step Out:** Step out of a Scheme function → verify return to caller
7. **Variable inspection (Scope pane):** Verify Scheme locals appear in native Scope pane for top frame
8. **Inline script:** Debug `<script type="text/scheme">` inline code
9. **External file:** Debug `<script type="text/scheme" src="app.scm">`
10. **Exception:** Trigger `(error "test")` → verify DevTools catches it
11. **TCO:** Step through tail-recursive function → verify no stack overflow
12. **Multiple files:** Set breakpoints in two different Scheme files
13. **REPL eval:** Debug code evaluated from the web REPL
14. **Async stack (Alt D):** Pause in nested Scheme call → verify native Call Stack shows async frames with `scheme: <name>` labels
15. **Sidebar frame list (Alt E):** Pause in nested call → verify extension sidebar shows full Scheme call stack with correct frame names and source locations
16. **Sidebar frame click (Alt E):** Click a non-top frame in sidebar → verify editor navigates to that frame's source file and line
17. **Sidebar variables (Alt E):** Click a non-top frame → verify sidebar Variables section shows that frame's local bindings (not top frame's)
18. **TCO badge (Alt E):** Pause in tail-recursive function → verify sidebar shows TCO×N badge on the tail-called frame
19. **Console API:** While paused, type `__schemeDebug.getStack()` in Console → verify returns correct stack array
20. **Console eval:** While paused, type `__schemeDebug.eval("(+ n 1)")` → verify evaluates in correct environment

### 17.4 Performance Tests

- Benchmark `run()` with DevTools disabled: <2% overhead
- Benchmark `run()` with DevTools enabled: <20% overhead
- Benchmark probe generation for a 1000-line Scheme file: <100ms
- Benchmark `maybeHit()` calls per second: >1M/sec (no line change)

---

## 18. Open Questions

1. **Probe granularity:** Should probes be per-line or per-expression? Per-line is simpler and sufficient for MVP. Per-expression enables column-level breakpoints but increases probe count significantly.

2. **`with` statement compatibility:** The `with` statement is deprecated in strict mode. Probe scripts must NOT use `"use strict"`. Verify this doesn't cause issues with module bundling.

3. **Source map persistence:** DevTools persists breakpoints by sourceURL. If the probe sourceURL changes between page loads (e.g., for inline scripts), breakpoints may not persist. Need stable URL scheme.

4. **Library debugging:** Should probes be generated for standard library `.sld`/`.scm` files? This could be noisy. Probably gated behind a separate flag.

5. **Rollup bundling:** How do probe scripts interact with the Rollup bundle? The bundle's `scheme_entry.js` creates a shared interpreter — probe scripts need access to the same `__schemeProbeRegistry` global.

6. **REPL dynamic eval:** Each REPL expression creates a new probe script. Need cleanup strategy to avoid accumulating thousands of probe scripts during extended REPL sessions.

7. **Worker threads:** If the interpreter runs in a Web Worker, probe injection via DOM `<script>` tags won't work. Need `importScripts()` or `eval()` alternative.

8. **DevTools closed mid-debug:** If the user closes DevTools while paused at a probe breakpoint, V8 resumes execution. The trampoline should continue normally — verify no dangling state.

9. **Extension installation requirement (Alt E):** The Scheme Stack sidebar requires users to install a Chrome extension. Should we provide a fallback experience (e.g., console-based `__schemeDebug` commands) for users who don't install it? The current design already provides this fallback via §8.6.

10. **`console.createTask` availability (Alt D):** This API is Chrome 106+. Other browsers (Firefox, Safari) don't support it. The design gracefully no-ops, but should we document browser compatibility requirements?

11. **Dual CDP attachment (Alt E):** The extension's `background.js` attaches to the tab via `chrome.debugger` to listen for `Debugger.paused`. If DevTools is already attached (which it is, since the user has DevTools open), can both clients coexist? Chrome supports multiple CDP clients, but we need to verify there are no interference issues with breakpoint handling.

12. **`inspectedWindow.eval` during pause (Alt E):** When V8 is paused at a breakpoint, can `chrome.devtools.inspectedWindow.eval()` still execute? Yes — DevTools eval runs in the console context which operates even during pause. But verify this works reliably for our `__schemeDebug` API calls.

13. **Extension sidebar variable depth (Alt E):** The sidebar shows text-serialized values. For complex nested structures (lists of lists, records), how deep should serialization go? Using existing `StateInspector.serializeValue()` with a depth limit of 2-3 seems reasonable. A future enhancement could add expandable tree views.

---

## Appendix A: VLQ Encoding Reference

Variable-Length Quantity encoding for source maps:

```javascript
const VLQ_BASE = 32; // 2^5
const VLQ_BASE_MASK = VLQ_BASE - 1;
const VLQ_CONTINUATION_BIT = VLQ_BASE;
const BASE64_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function encodeVLQ(value) {
  let vlq = value < 0 ? ((-value) << 1) + 1 : (value << 1);
  let encoded = '';
  do {
    let digit = vlq & VLQ_BASE_MASK;
    vlq >>>= 5;
    if (vlq > 0) digit |= VLQ_CONTINUATION_BIT;
    encoded += BASE64_CHARS[digit];
  } while (vlq > 0);
  return encoded;
}
```

## Appendix B: Probe Script Injection

```javascript
/**
 * Injects a probe script into the DOM.
 * @param {string} jsCode - The probe script JavaScript
 */
function injectProbeScript(jsCode) {
  const script = document.createElement('script');
  script.textContent = jsCode;
  // Must NOT be type="module" (modules are strict mode, `with` is forbidden)
  document.head.appendChild(script);
  // Clean up DOM node (script has already executed)
  document.head.removeChild(script);
}
```

## Appendix C: Full Probe Call Sequence

```
1. Interpreter trampoline: step(registers)
2. CTL = IfNode with source = { filename: "scheme://app/foo.scm", line: 5 }
3. devtoolsDebug.maybeHit(source, env)
4. key = "scheme://app/foo.scm:5" !== lastHitKey → proceed
5. probe = sourceRegistry.getProbe("scheme://app/foo.scm", 5)
6. envProxy = createEnvProxy(env)  [cached if env unchanged]
7. probe(envProxy)
   → function __scheme_L5(envProxy) { with (envProxy) { void 0; } }
   → If DevTools breakpoint on line 5: V8 pauses here
   → DevTools shows foo.scm line 5 via source map
   → Scope pane shows envProxy bindings
8. probe returns → trampoline continues
```
