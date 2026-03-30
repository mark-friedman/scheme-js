# Scheme-JS Chrome Debugger Manual

This manual describes how to install, activate, and use the **Scheme-JS** Chrome debugger extension. The extension opens a standalone debugger window that provides a unified debugging experience for Scheme code, including setting breakpoints, inspecting variables, navigating the call stack, evaluating expressions, and stepping through code.

---

## 1. Installation

The Scheme-JS debugger is provided as an unpacked Chrome Extension. To install it:

1. Open Google Chrome.
2. Navigate to `chrome://extensions/` in your address bar.
3. Enable **Developer mode** using the toggle switch in the top right corner.
4. Click the **Load unpacked** button in the top left corner.
5. Select the `extension/` directory located inside your `scheme-js-4` repository.
6. The extension "Scheme-JS Debugger" should now appear in your list of installed extensions.

---

## 2. Opening the Debugger

Once the extension is installed, you can use it to debug any page running the `scheme-js` interpreter.

1. Open your Scheme-JS web application (e.g., `web/index.html` or a custom page) in Chrome.
2. Click the **Scheme-JS Debugger** toolbar icon in Chrome's extension area (puzzle piece icon → pin the extension for quick access).
3. A standalone debugger window will open, automatically detecting the Scheme-JS interpreter on the active tab and activating debug mode.

If you click the toolbar icon again while a debugger window is already open for that tab, the existing window is focused rather than creating a duplicate.

> **Note**: The debugger window is independent of Chrome DevTools. You do not need to open DevTools to use the Scheme-JS debugger. When a breakpoint is hit, the debugger window automatically comes to the foreground.

---

## 3. Debugger UI Overview

The debugger window provides a comprehensive set of debugging views, similar to Chrome's native Sources panel but specifically tailored for Scheme's expression-based execution and Scheme-to-JS interoperability.

```text
+------------------------------------------------------------------+
| Toolbar: [Resume] [Step Into]  [Step Over]  [Step Out]           |
+------------------------------------------------------------------+
|          |                     |                                  |
| Sources  |  Source Editor      |  Call Stack                      |
|          |  - Breakpoint       |    [SCM] factorial (depth 3)     |
| v fact.  |    gutter           |    [JS]  Array.map               |
|   scm    |  - Expression       |    [SCM] main                    |
| v index  |    highlighting     |    [SCM] <top-level>             |
|   .html  |  - Diamond markers  |----------------------------------|
|          |                     |  Variables                       |
|          |                     |   Local                          |
|          |                     |    n: 5                          |
|          |                     |   Closure                        |
|          |                     |    acc: 120                      |
|          |                     |----------------------------------|
|          |---------------------+  Breakpoints                     |
|          |  Console            |    factorial.scm:2               |
|          |  > (+ n 1)          |    index.html:15                 |
|          |  ← 6                |                                  |
+------------------------------------------------------------------+
```

### Components

* **Toolbar**: Execution controls (Resume, Step Into, Step Over, Step Out) and status indicator showing the current pause state.
* **Sources Sidebar**: Lists all parsed Scheme files, inline `<script type="text/scheme">` tags, and REPL evaluations. Click a source to load its code into the Source Editor.
* **Source Editor**: A CodeMirror-based code viewer with Scheme syntax highlighting. Shows the currently executing expression (highlighted) and allows you to set breakpoints by clicking in the gutter. Supports mixed HTML/Scheme highlighting for `.html` sources.
* **Call Stack**: Displays the current execution stack when paused. Scheme frames are prefixed with `[SCM]`, JavaScript boundary frames with `[JS]`. A synthetic `<top-level>` frame at the bottom shows where the outermost function was called from. Click any frame to inspect its variables and navigate to its source location.
* **Variables (Scope)**: Shows the bindings for the currently selected call stack frame, grouped by scope level:
  - **Local**: Variables bound in the current function or let form.
  - **Closure**: Variables captured from enclosing scopes.
  - **Global**: Top-level definitions and primitives.
* **Breakpoints**: Lists all active breakpoints across all sources, with click-to-navigate.
* **Console**: A REPL-like input at the bottom of the editor area for evaluating Scheme expressions in the current execution context. Also displays page console output (`console.log`, `console.warn`, `console.error` from the debugged page).

---

## 4. Setting Breakpoints

Breakpoints let you pause execution at specific locations in your Scheme code.

### Line-Level Breakpoints
Click on the line number in the gutter on the left side of the **Source Editor**. A visual indicator (red dot) will appear to confirm the breakpoint is set. When execution reaches that line, it will pause at the outermost expression on the line.

### Expression-Level Breakpoints
Since Scheme is an expression-based language, a single line might contain multiple distinct operations (e.g., `(* (+ 1 2) (- 5 3))`). The Scheme-JS debugger supports precision debugging:
* When a line has a line-level breakpoint or is the current paused line, diamond markers (◆/◇) appear at individually evaluable sub-expression boundaries.
* Clicking a diamond marker sets an expression-level breakpoint (shown as a filled/active diamond), ensuring the debugger pauses exactly before evaluating that specific sub-expression.

### Breakpoint Persistence
Breakpoints are saved to `chrome.storage.local` and survive page reloads. When the debugger reconnects after a navigation or reload, stored breakpoints are automatically re-sent to the interpreter.

---

## 5. Stepping Through Code

When the debugger is paused, you can control the execution flow using the **Toolbar** buttons or keyboard shortcuts:

| Action | Description | Shortcut |
| :--- | :--- | :--- |
| **Resume** | Continue execution until the next breakpoint or completion. | `F8` |
| **Step Over** | Evaluate the current expression completely (including sub-calls) and pause at the next expression at the same or shallower depth. | `F10` |
| **Step Into** | Proceed to the very next expression evaluated by the interpreter, entering into function or macro bodies. | `F11` |
| **Step Out** | Run until the current function returns, pausing at the next expression in the calling function. | `Shift+F11` |

After each step, the **Variables** pane updates to show the result of the previous expression.

> **Note**: Scheme's evaluation semantics are respected — stepping into special forms like `if`, `define`, or `let` correctly steps into the condition or binding expressions first.

---

## 6. Inspecting State

### Navigating the Call Stack
The **Call Stack** pane shows the sequence of function calls that led to the current point of execution.
* Clicking an `[SCM]` frame highlights the corresponding source location in the Editor and updates the **Variables** pane to show the bindings available in that frame's lexical environment.
* `[JS]` boundary frames indicate where a Scheme procedure invoked a JavaScript function or vice versa.
* The `<top-level>` frame at the bottom represents the call site of the outermost Scheme function (e.g., top-level script code).

### Viewing Variables
The **Variables** pane groups variables by scope:
* **Local**: Variables defined in the current function body or `let`/`letrec` form.
* **Closure**: Variables captured from enclosing lexical scopes.
* **Global**: Top-level definitions and built-in primitives.

Each variable shows its name, value (as a string), and type (with color coding for numbers, booleans, strings, etc.).

### Console Evaluation
The **Console** at the bottom of the editor area lets you evaluate Scheme expressions in the context of the currently selected call stack frame:
* Type an expression and press **Enter** to evaluate.
* Results appear inline with type-appropriate formatting.
* Use **Up/Down arrows** to navigate command history.
* Page console output (`console.log`, `console.warn`, `console.error`) from the debugged page is also displayed here.

### Mixed HTML and Boundary Stepping
If you are debugging Scheme embedded within an HTML file (e.g., `<script type="text/scheme">`), the Source Editor handles mixed HTML and Scheme highlighting. When Scheme code calls into JavaScript, the unified Call Stack shows the transition, and the extension coordinates with Chrome's native JS debugger to provide seamless boundary crossings.

---

## 7. Debugging Synchronous DOM Callbacks

Scheme closures called from synchronous JavaScript contexts — such as DOM event handlers (button clicks, input events) or `setTimeout` callbacks — are fully debuggable. When a breakpoint is hit inside a synchronous callback:

* The debugger window is automatically focused.
* The call stack, variables, and source location are displayed correctly.
* Step commands (Step Into, Step Over, Step Out) and console evaluation work as expected.
* The page remains paused until you resume from the debugger window.

This works by keeping the V8 engine paused at the probe `debugger;` statement and routing all debugger commands through Chrome's CDP (Chrome DevTools Protocol) `evaluateOnCallFrame` API. This is transparent to the user — the debugging experience is identical whether the Scheme code is running on the async trampoline or in a synchronous DOM callback.

---

## 8. Theme Support

The debugger window automatically matches your system theme:
* **Dark mode**: When your OS is set to dark mode (via `prefers-color-scheme: dark`), the editor and all panels use dark backgrounds and light text.
* **Light mode**: Uses light backgrounds and dark text.

The theme updates dynamically if you change your system preference.

---

## 9. Window Lifecycle

* **One window per tab**: Each debugged tab has at most one debugger window. Clicking the toolbar icon for a tab that already has a window focuses the existing window.
* **Auto-cleanup**: When you close a tab, its associated debugger window is also closed. When you close a debugger window, the CDP debugger is detached from the tab.
* **Auto-focus on pause**: When a breakpoint is hit (whether cooperative Scheme pause, sync-path DOM callback pause, or JS CDP pause), the debugger window is automatically brought to the foreground.

---

## 10. Known Limitations

* **Continuations (`call/cc`)**: Aggressive use of first-class continuations that drastically unwind or rewind the stack during a step operation may cancel an active step command to maintain safety.
* **Step timeout**: If a step command does not result in a pause within 3 seconds (e.g., the stepped expression triggers a long-running computation or exits the interpreter), the panel automatically returns to the "running" state.
* **Exact/Inexact numbers**: JavaScript's single numeric type means the Variables pane cannot distinguish between exact integers and inexact floats (e.g., `5` vs `5.0`).
