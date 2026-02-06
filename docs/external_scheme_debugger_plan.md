# Debugging Capabilities Plan

The goal is to provide a comprehensive debugging experience for `scheme-js` in the browser, enabling stepping, breakpoints, and state inspection.

## Proposed Changes

### Core Interpreter & Reader

- Update `tokenize` to track line and column numbers.
- Return tokens with `{ value, hasPrecedingSpace, line, column, sourceId }`.

#### [MODIFY] [parser.js](../src/core/interpreter/reader/parser.js)
- Update `readFromTokens` to attach location info (`sourceId`, `line`, `column`) to `SyntaxObject`'s `context` field.
- Ensure list/vector recursion preserves location.

#### [MODIFY] [ast_nodes.js](../src/core/interpreter/ast_nodes.js)
- Update all AST nodes to store source location (line/column) from their associated syntax objects.

- Add a debugging hook `onStep(registers, interpreter)`.
- Introduce a `BreakpointRegistry` to manage execution pauses.
- Implement an asynchronous trampoline `runAsync` to allow the browser to breathe while debugging.
- **Exception Hook**: Add `onException(error, isCaught)` hook to the interpreter's error handling logic.

## Debugger Architecture Details

### Browser Integration: The Debugger Overlay
The "Built-in Overlay" is a **browser-native UI layer** (using `position: fixed` or `<dialog>`) that renders on top of your application.
- **Context Awareness**: Because the Reader will now track `sourceId` (filename or URI), the debugger can automatically fetch and display the source code for any Scheme file (`.scm`, `.sld`) currently being executed.
- **Multi-file System**: The UI will include a "Sources" file explorer, allowing you to browse and set breakpoints in any file loaded into the `LibraryRegistry`.

### Error & Exception Debugging
The debugger will support **"Pause on Caught/Uncaught Exceptions"**.
- The interpreter's central `try-catch` and `RaiseNode` logic will be updated to check the `DebugController` before proceeding with unwinding.

### Modularity & Node.js Support
To ensure the approach is future-proof:
- **Separation of Concerns**: The `DebugController` (breakpoints, stepping logic, shadow stack) will be strictly decoupled from the `DebuggerUI` (HTML/CSS).
- **Node.js**: In a Node environment, a different "UI" (e.g., a CLI-based REPL or a DAP-server for VS Code) can attach to the same `DebugController` hooks.

### Native Debugger Interoperability
The Scheme debugger and browser DevTools can work in tandem:
- **Blackboxing (Ignore List)**: We will recommend that users "Ignore List" the `scheme.js` bundle in Chrome/Edge DevTools. This hides the internal trampoline loop, making the native Call Stack much cleaner when debugging hybrid JS/Scheme apps.
- **Implicit Boundary Detection**: 
    - The interpreter's application logic (`TailAppNode`) will check if an operator evaluates to a native JavaScript function (regardless of whether it came from `js-invoke`, a variable, or `globalThis`).
    - If "Bridge to Native" is enabled, it will trigger a `debugger;` statement before calling the native function.
- **JS-to-Scheme Boundary Logic**:
    - **Step Over Scheme**: If a JS user clicks "Step Over" on a Scheme closure call, it works out-of-the-box. Since `runWithSentinel` is synchronous, the entire Scheme computation runs to completion, and control returns to the next JS line.
    - **Step Into Scheme**: Since browsers can't natively step into Scheme source, we use a **"Pause on Entry"** toggle. When enabled, *any* entry from JS into Scheme (via `runWithSentinel`) automatically triggers the custom Scheme debugger's pause state.
    - **Stepping through "Glue"**: By "Ignore Listing" (blackboxing) `interpreter.js` in DevTools, the browser will skip over the trampoline's internal loop. If "Pause on Entry" is active, the custom UI will pop up as soon as the first Scheme expression is reached, effectively "rescuing" the user from the JS engine's internals.
- **Scheme-to-JS Boundary Logic**:
    - **Step Over JS**: In the Scheme debugger, clicking "Step Over" on a native call executes that call synchronously and pauses at the next Scheme expression.
    - **Step Into JS**: Clicking "Step Into" on a native call triggers a `debugger;` statement if the "Bridge to Native" option is enabled. This pops open browser DevTools at the start of the JS implementation.

## UI Design

The debugger UI provides a premium, "IDE-like" experience within the browser tab.

### Key Panels
- **Source Editor**: Highlighted current line and breakpoint markers.
- **Call Stack**: Visual historical trace of function calls (Logical Stack).
- **Variable Inspector**: A tree-view for inspecting lexical scopes and global values.
- **Execution Controls**: Standard Resume, Pause, and Stepping maneuvers.

## Verification Plan

### Automated Tests
- `node run_tests_node.js`
- New unit tests in `tests/core/interpreter/debug_tests.js` to verify:
    - Tokenizer line/column tracking.
    - Breakpoint hit detection.
    - Step-by-step register state transitions.

### Manual Verification
- Deploy the updated REPL with integrated debugger UI.
- Verify that setting a breakpoint in a Scheme script pauses execution in the browser.
- Verify that single-stepping correctly moves the highlighted line in the UI.
- Inspect Scheme variables in the debugger panel.


