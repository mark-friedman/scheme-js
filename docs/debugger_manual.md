# Scheme.js Debugger Manual

This manual describes how to use the interactive debugging tools available in Scheme.js, both in the Node.js REPL and the Browser REPL.

## Table of Contents
1. [Getting Started](#getting-started)
2. [Activation and Pausing](#activation-and-pausing)
3. [Debug Commands](#debug-commands)
4. [Inspecting State](#inspecting-state)
5. [Stepping and Resumption](#stepping-and-resumption)
6. [Breakpoints](#breakpoints)

---

## Getting Started

The Scheme.js debugger allows you to pause execution, inspect local variables, navigate the call stack, and evaluate expressions within the context of a paused program.

- **Node.js REPL**: Start with `node repl.js`.
- **Browser REPL**: Open `web/index.html` in your browser.

---

## Activation and Pausing

### Activation
Debugging is enabled by default in the REPLs. You can check the status or toggle it using:
- `:debug` - Show current status.
- `:debug on` - Enable debugging.
- `:debug off` - Disable debugging.

### Manual Pausing
The most direct way to enter the debugger is using the `(pause)` primitive. When the interpreter encounters `(pause)`, it halts execution and enters the **debug prompt** (`debug>`).

```scheme
> (define (calculate x)
    (pause)
    (+ x 10))
> (calculate 5)
;; Paused: manual pause at calculate:2
debug>
```

---

## Debug Commands

When paused, the REPL accepts special commands prefixed with a colon (`:`).

| Command | Shorthand | Description |
| :--- | :--- | :--- |
| `:help` | `:?` | Show available debug commands. |
| `:bt` | `:backtrace` | Show the current call stack. |
| `:locals` | | Show all local variables in the current frame. |
| `:eval <expr>`| | Evaluate a Scheme expression in the selected frame. |
| `:up` | | Move one frame up the stack (older). |
| `:down` | | Move one frame down the stack (newer). |
| `:continue` | `:c` | Resume normal execution. |
| `:step` | `:s` | Step into the next expression. |
| `:next` | `:n` | Step over the current expression. |
| `:finish` | `:fin` | Step out of the current function. |

---

## Inspecting State

### Backtrace
Use `:bt` to see where you are in the execution. The current frame is marked with `=>`.

```scheme
debug> :bt
;; Call Stack:
;;   #1 (calculate) at calculate.scm:2
;; => #0 (anonymous) at <unknown>:1
```

### Local Variables
Use `:locals` to see all variables defined in the current lexical environment.

```scheme
debug> :locals
;; Local variables for frame #1:
;;   x = 5
```

### Evaluating Expressions
You can evaluate any Scheme expression in the context of the current frame. In the REPL, any input that is **not** a colon command is automatically treated as an evaluation in the current scope.

```scheme
debug> (* x 2)
;; result: 10
```
*Note: You can also use the explicit `:eval (* x 2)` command.*

---

## Stepping and Resumption

Once finished inspecting, you can control how execution continues:

- **Continue (`:c`)**: Resume until the next breakpoint or completion.
- **Step Into (`:s`)**: Execute the very next step, potentially entering a function call.
- **Step Over (`:n`)**: Execute the next expression in the current function without entering calls.
- **Step Out (`:fin`)**: Resume until the current function returns.

---

## Breakpoints

Breakpoints allow you to pause execution automatically at specific lines.

- `:break <file> <line>` - Set a breakpoint.
- `:breakpoints` - List all active breakpoints.
- `:unbreak <id>` - Remove a breakpoint by its ID.

Example:
```scheme
> :break main.scm 15
;; Breakpoint 1 set at main.scm:15
```
