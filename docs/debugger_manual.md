# Scheme-JS Debugger Manual

This manual describes how to use the interactive debugging tools available in Scheme-JS, both in the Node.js REPL and the Browser REPL.

## Table of Contents
1. [Getting Started](#getting-started)
2. [Activation and Pausing](#activation-and-pausing)
3. [Debug Commands](#debug-commands)
4. [Inspecting State](#inspecting-state)
5. [Stepping and Resumption](#stepping-and-resumption)
6. [Breakpoints](#breakpoints)
7. [Debug Levels](#debug-levels)
8. [Multi-Line Input](#multi-line-input)

---

## Getting Started

The Scheme-JS debugger allows you to pause execution, inspect local variables, navigate the call stack, evaluate expressions within the context of a paused program, and explore nested debug levels when errors occur.

- **Node.js REPL**: Start with `node repl.js`.
- **Browser REPL**: Open `web/index.html` in your browser.

---

## Activation and Pausing

### Activation
Debugging is enabled by default in the REPLs. You can check the status or toggle it using:
- `:debug` - Show current status.
- `:debug on` - Enable debugging. This is the default for the REPLs.
- `:debug off` - Disable debugging. This can be more performant.

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

### Exception-Based Pausing
When debug mode is on, the debugger automatically pauses on uncaught exceptions. This lets you inspect the call stack and local variables at the point where the error occurred, rather than just seeing an error message.

```scheme
> :debug on
> (define (bad-math x) (/ x 0))
> (bad-math 42)
;; Error: division by zero at bad-math:1
debug [1]>
```

From here you can use `:bt`, `:locals`, and `:eval` to investigate the error, or `:abort` to return to the previous level.

---

## Debug Commands

When paused, the REPL accepts special commands prefixed with a colon (`:`).

### Execution Control

| Command | Shorthand | Description |
| :--- | :--- | :--- |
| `:debug on\|off` | | Enable or disable debugging. |
| `:break <file> <line> [col]` | | Set a breakpoint at the given location. |
| `:unbreak <id>` | | Remove a breakpoint by its ID. |
| `:breakpoints` | | List all active breakpoints. |
| `:step` | `:s` | Step into the next expression. |
| `:next` | `:n` | Step over the current expression. |
| `:finish` | `:fin` | Step out of the current function. |
| `:continue` | `:c` | Resume normal execution. |

### Frame Navigation (within the current debug level)

| Command | Shorthand | Description |
| :--- | :--- | :--- |
| `:bt` | `:backtrace` | Show the call stack for the current debug level. |
| `:locals` | | Show local variables in the selected frame. |
| `:eval <expr>` | | Evaluate a Scheme expression in the selected frame's scope. |
| `:up` | `:u` | Select the next older stack frame. |
| `:down` | `:d` | Select the next newer stack frame. |

### Level Navigation

| Command | Shorthand | Description |
| :--- | :--- | :--- |
| `:levels` | | Show all active debug levels. |
| `:level [N]` | | View or switch to debug level N. |
| `:abort` | `:a` | Pop one debug level. |
| `:toplevel` | `:top` | Pop all debug levels, return to the top-level REPL. |
| `:help` | `:h`, `:?` | Show available debug commands. |

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

### Navigating Frames
Use `:up` (or `:u`) and `:down` (or `:d`) to select different stack frames within the current debug level. The selected frame determines the scope used by `:locals` and `:eval`.

```scheme
debug> :up
;; Frame #1: (calculate) at calculate.scm:2
debug> :locals
;; Local variables for frame #1:
;;   x = 5
debug> :down
;; Frame #0: (anonymous) at <unknown>:1
```

### Evaluating Expressions
You can evaluate any Scheme expression in the context of the selected frame. In the REPL, any input that is **not** a colon command is automatically treated as an evaluation in the current scope.

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

- `:break <file> <line> [col]` - Set a breakpoint. The column is optional.
- `:breakpoints` - List all active breakpoints.
- `:unbreak <id>` - Remove a breakpoint by its ID.

Example:
```scheme
> :break main.scm 15
;; Breakpoint 1 set at main.scm:15
```

---

## Debug Levels

Debug levels and stack frames are distinct concepts:

- **Stack frames** represent the call stack at a single point of paused execution. You navigate them with `:up`, `:down`, `:bt`, and `:locals`.
- **Debug levels** represent nested pauses. A new debug level is created whenever execution pauses again while you are already at a debug prompt — for example, when an exception occurs while evaluating an expression at the debug prompt, or when a breakpoint is hit during a `:step`.

### How Levels Work

Each debug level has its own call stack. The prompt shows the current level number:

```scheme
debug [1]>           ;; Level 1: first pause (e.g., a breakpoint)
debug [2]>           ;; Level 2: an exception occurred while debugging
```

### Viewing Levels

Use `:levels` to see all active debug levels. The currently viewed level is marked with `=>`.

```scheme
debug [2]> :levels
;; Debug levels:
;;   [1] Breakpoint at main.scm:15
;; => [2] Error: unbound variable 'foo' at main.scm:16
```

### Switching the Viewed Level

Use `:level N` to switch which debug level you are viewing. All frame navigation commands (`:bt`, `:locals`, `:up`, `:down`, `:eval`) then operate on that level's call stack.

```scheme
debug [2]> :level 1
;; Viewing debug level 1
debug [1]> :bt
;; (shows the call stack from level 1)
```

### Popping Levels

- **`:abort`** (or `:a`): Pop one debug level — discard the current level and return to the previous one. If you are at level 1, this returns to the top-level REPL.
- **`:toplevel`** (or `:top`): Pop all debug levels at once, returning directly to the top-level REPL.

```scheme
debug [2]> :abort
;; Popped to debug level 1
debug [1]> :toplevel
>
```

---

## Multi-Line Input

Both REPLs support multi-line expressions:

- **Node.js REPL**: When an expression has unbalanced parentheses, the REPL displays a continuation prompt and waits for additional input.
- **Browser REPL**: Press Enter to add a new line when parentheses are unbalanced. The expression is evaluated once all parentheses are balanced.

```scheme
> (define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))
```

This works in both the normal REPL and the debug prompt.
