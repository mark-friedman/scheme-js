# Chrome DevTools Scheme Debugger — Usage Guide

## 1. Overview

The scheme-js Chrome DevTools integration lets you debug Scheme code directly in Chrome DevTools with:

- **Scheme source files** in the Sources panel with syntax highlighting
- **Native breakpoints** on Scheme expressions (including column-level)
- **Step Into / Step Over / Step Out** with correct Scheme semantics
- **Variable inspection** in the Scope pane and extension sidebar
- **Full Scheme call stack** with clickable frame navigation (via extension)
- **Async stack hints** showing Scheme function names in the native Call Stack
- **REPL debugging** — code entered in the web REPL becomes debuggable

---

## 2. Prerequisites

- **Chrome 106+** (required for `console.createTask` async stack tagging)
- **scheme-js distribution files**: `dist/scheme.js` and `dist/scheme-html.js`
- **Chrome DevTools extension** (optional, for Scheme Stack sidebar): `extension/` directory

Build the distribution:
```bash
npm run build
```

---

## 3. Quick Start

### Minimal HTML Setup

```html
<!DOCTYPE html>
<html>
<head>
  <script type="module" src="dist/scheme.js"></script>
  <script type="module" src="dist/scheme-html.js"></script>
</head>
<body>
  <script type="text/scheme" debug>
    (define (factorial n)
      (if (<= n 1) 1
          (* n (factorial (- n 1)))))
    (display (factorial 10))
  </script>
</body>
</html>
```

1. Serve the page over a local server (e.g., `npx http-server`)
2. Open Chrome DevTools → **Sources** panel
3. In the file tree, look for `scheme://inline-scripts/script-0.scm`
4. Click the line number gutter to set a breakpoint
5. Reload the page — execution pauses at your breakpoint

---

## 4. Enabling Debug Mode

Debug mode is required for breakpoints, stepping, and source registration. Enable it using any of these methods:

### Method 1: `debug` Attribute (Recommended)

Add the `debug` attribute to any `<script type="text/scheme">` tag:

```html
<script type="text/scheme" debug>
  (display "Hello, debugger!")
</script>

<script type="text/scheme" src="app.scm" debug></script>
```

### Method 2: URL Parameter

Add `?scheme-debug=true` to the page URL:

```
http://localhost:8080/mypage.html?scheme-debug=true
```

### Method 3: Global Flag

Set the flag before the scheme-js scripts load:

```html
<script>
  globalThis.__SCHEME_JS_DEBUG = true;
</script>
<script type="module" src="dist/scheme.js"></script>
<script type="module" src="dist/scheme-html.js"></script>
```

---

## 5. Setting Breakpoints

### Finding Scheme Files

With debug mode enabled, Scheme sources appear in the DevTools file tree under virtual `scheme://` URLs:

| Source Type | URL Pattern |
|---|---|
| Inline `<script>` | `scheme://inline-scripts/script-0.scm` |
| External `.scm` file | `scheme://scheme-sources/filename.scm` |
| REPL input | `scheme://repl/eval-0.scm` |
| Library `.sld` | `scheme://lib/scheme/base.sld` |

### Setting a Breakpoint

1. Click a Scheme file in the Sources tree
2. Click the line number gutter to set a breakpoint (blue marker appears)
3. For **column-level breakpoints**, right-click the gutter and choose a specific expression

### Expression-Level Precision

Breakpoints fire on **expressions**, not just lines. For example, on this line:

```scheme
(* n (factorial (- n 1)))
```

You can set breakpoints on `(* ...)`, `(factorial ...)`, or `(- n 1)` independently.

---

## 6. Stepping

### Using the Extension Sidebar

When paused, the Scheme Stack sidebar provides stepping buttons:

- **Step Into** — Enters function bodies, follows macro expansions
- **Step Over** — Evaluates the current expression without entering sub-calls
- **Step Out** — Runs until the current function returns

### How It Works

Stepping is **interpreter-managed** rather than V8-native. When you click a step button:

1. The stepping mode is recorded in the probe runtime
2. V8 resumes execution
3. At each subsequent Scheme expression, the runtime checks if the stop condition is met
4. When met, `debugger;` fires and V8 pauses at the correct Scheme source location

### Console-Based Stepping

While paused, you can also step from the Console:

```javascript
__schemeDebug.stepInto()   // then press Resume (F8)
__schemeDebug.stepOver()
__schemeDebug.stepOut()
```

---

## 7. Variable Inspection

### Scope Pane (Top Frame)

When paused at a breakpoint, the native **Scope** pane shows Scheme variables under the "With Block" scope. These are the bindings from the current Scheme environment:

```
▼ With Block
    n: 5
    factorial: ƒ __scheme_closure()
    result: 120
```

### Extension Sidebar (Any Frame)

The Scheme Stack sidebar shows variables for the **selected** frame, not just the top frame. Click any frame in the call stack to see its local bindings.

---

## 8. Call Stack

### Native Call Stack

The native Call Stack pane shows:
- The current probe frame (e.g., `__scheme_E42 (app.scm:5)`)
- **Async frames** with Scheme function names (e.g., `scheme: factorial`)

These async frames are created via `console.createTask` and provide a breadcrumb trail of the Scheme call chain.

### Extension Sidebar Call Stack

The Scheme Stack sidebar shows the **full** Scheme call stack with:
- Function names and source locations
- **TCO badges** (e.g., `TCO×3`) for tail-call optimized frames
- Clickable frames that navigate to the source location

---

## 9. Installing the Chrome Extension

The Scheme Stack extension adds a sidebar to the DevTools Sources panel.

### Load Unpacked (Development)

1. Open `chrome://extensions/`
2. Enable **Developer mode** (toggle in top-right)
3. Click **Load unpacked**
4. Select the `extension/` directory from the project root

### Load from Distribution

After running `node scripts/package_extension.js`:

1. Open `chrome://extensions/`
2. Enable Developer mode
3. Click **Load unpacked**
4. Select the `dist/extension/` directory

The extension adds a **"Scheme Stack"** sidebar in the Sources panel.

---

## 10. Library Debugging

By default, standard library files (e.g., `(scheme base)`) are **not** registered for debugging to reduce noise. To enable:

### URL Parameter

```
http://localhost:8080/mypage.html?scheme-debug=true&scheme-debug-libraries=true
```

### Global Flag

```javascript
globalThis.__SCHEME_JS_DEBUG_LIBRARIES = true;
```

When enabled, any library loaded via `(import ...)` after initialization generates probe scripts and appears in the Sources panel under `scheme://lib/...`.

---

## 11. REPL Debugging

When DevTools debugging is active, code entered in the web REPL is automatically registered as debuggable sources:

1. Enable debug mode on your page
2. Open the REPL (e.g., via `<scheme-repl>` web component)
3. Enter Scheme code — it appears in Sources as `scheme://repl/eval-0.scm`, etc.
4. Set breakpoints on REPL expressions just like file sources

REPL sources use an LRU cache (default: 50 entries) to prevent unbounded memory growth. Older entries are automatically pruned.

---

## 12. Console API (`__schemeDebug`)

While paused at a Scheme breakpoint, the `__schemeDebug` global provides:

### Stack Inspection

```javascript
// Get the full Scheme call stack
__schemeDebug.getStack()
// → [{name: "factorial", source: {filename: "...", line: 2, column: 2}, tcoCount: 0}, ...]

// Get local variables for a specific frame (0 = bottom, length-1 = top)
__schemeDebug.getLocals(frameIndex)
// → [{name: "n", value: "5", type: "number", subtype: null}, ...]

// Get source location for a frame
__schemeDebug.getSource(frameIndex)
// → {filename: "scheme://app/factorial.scm", line: 2, column: 2}
```

### Evaluation

```javascript
// Evaluate Scheme code in the context of a specific frame
__schemeDebug.eval("(+ n 1)")           // top frame
__schemeDebug.eval("(+ n 1)", 0)        // bottom frame
```

### Stepping

```javascript
__schemeDebug.stepInto()
__schemeDebug.stepOver()
__schemeDebug.stepOut()
// After calling a step method, click Resume (F8) to execute the step
```

---

## 13. Troubleshooting

### Scheme files don't appear in Sources

- Verify debug mode is enabled (check for `debug` attribute, URL param, or global flag)
- Ensure `dist/scheme-html.js` is loaded **after** `dist/scheme.js`
- Check the Console for errors during script loading

### Breakpoints don't fire

- Scheme breakpoints fire on **expressions**, not arbitrary lines. Ensure the breakpoint is on a line with a Scheme expression.
- If you edited the Scheme source, reload the page to regenerate probe scripts.

### Stepping lands in JavaScript code

- The interpreter's JavaScript internals should be **blackboxed**. The extension does this automatically via `Debugger.setBlackboxPatterns`.
- If you see interpreter code, manually blackbox `interpreter.js`, `ast.js`, etc. in DevTools Settings → Blackboxing.

### Extension sidebar doesn't update

- Ensure the Scheme Stack extension is loaded and enabled
- The sidebar only updates when paused at a **Scheme** probe breakpoint (not arbitrary JS breakpoints)
- Check the extension's background page console for errors

### Variables show as `undefined`

- Variables are only visible when paused inside a `with(envProxy)` block (which happens automatically at probe breakpoints)
- If paused at a non-probe location, the Scope pane won't show Scheme variables

### Performance is slow with DevTools open

- Probe scripts add overhead when DevTools is attached. This is expected.
- Close DevTools when not debugging for normal performance.
- Library debugging (`?scheme-debug-libraries=true`) increases overhead further — only enable when needed.
