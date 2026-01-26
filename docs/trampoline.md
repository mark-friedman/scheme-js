# Trampoline Execution Model

This document explains the core execution model of the Scheme interpreter.

## Overview

The interpreter uses a "trampoline" pattern to avoid using the JavaScript call stack for Scheme function calls. This enables:

- **Proper Tail Calls**: Tail-recursive Scheme functions run in constant stack space.
- **First-Class Continuations**: `call/cc` can capture and restore the entire computation state.
- **JavaScript Interoperability**: Scheme closures can be called from JavaScript and vice versa.

## The Register Machine

The interpreter state is modeled as a simple register machine with four registers:

| Register | Constant | Purpose |
|----------|----------|---------|
| `ans` | `ANS` (0) | **Answer** - Result of the last sub-computation |
| `ctl` | `CTL` (1) | **Control** - The next AST node or Frame to execute |
| `env` | `ENV` (2) | **Environment** - Current lexical environment |
| `fstack` | `FSTACK` (3) | **Frame Stack** - The continuation (stack of pending operations) |
| `this` | `THIS` (4) | **This Context** - The current JavaScript `this` binding |

```javascript
// In interpreter.js
import { ANS, CTL, ENV, FSTACK, THIS } from './stepables.js';
const registers = [null, ast, env, [...initialStack], thisContext];
//                 ANS   CTL  ENV  FSTACK             THIS
```

## The Trampoline Loop

The main loop (`Interpreter.run()`) repeatedly calls `step()` on the current `ctl`:

```javascript
while (true) {
    if (this.step(registers)) {
        continue;  // step() returned true: tail call, keep going
    }
    
    // step() returned false: value computed
    const fstack = registers[FSTACK];
    
    if (fstack.length === 0) {
        return registers[ANS];  // Done! Return the answer
    }
    
    // Pop next frame and continue
    const frame = fstack.pop();
    registers[CTL] = frame;
}
```

## Step Return Values

The `step()` method on every `Executable` (AST node or Frame) returns a boolean:

| Return Value | Meaning |
|--------------|---------|
| `true` | **Tail Call** - Update `ctl` and continue immediately |
| `false` | **Value Return** - Result is in `ans`, check the frame stack |

## AST Nodes vs Frames

The interpreter has two types of `Executable` objects:

### AST Nodes
Created by the Analyzer dispatcher and its modular handlers in `analyzers/`. Represent the structure of Scheme code:
- `Literal`, `Variable`, `Lambda` - Atomic expressions
- `If`, `Let`, `Define`, `TailApp` - Compound expressions

When an AST node needs to evaluate sub-expressions, it:
1. Pushes a **Frame** onto `fstack`
2. Sets `ctl` to the sub-expression
3. Returns `true`

### Frames
Represent "the rest of the computation" after a sub-expression completes:
- `IfFrame` - Waits for test result, then evaluates a branch
- `AppFrame` - Accumulates arguments, then applies the function
- `LetFrame` - Waits for binding value, then evaluates body

When a frame's sub-expression completes, the frame:
1. Reads the result from `ans`
2. Does its work (e.g., extends environment)
3. Either returns a value (`false`) or continues (`true`)

## Example: Evaluating `(if #t 1 2)`

```
Step 1: If node pushes IfFrame, sets ctl = #t
        fstack: [IfFrame]
        
Step 2: Literal #t sets ans = true, returns false
        fstack: [IfFrame]
        
Step 3: Trampoline pops IfFrame, sets ctl = IfFrame
        fstack: []
        
Step 4: IfFrame sees ans = true, sets ctl = 1
        fstack: []
        
Step 5: Literal 1 sets ans = 1, returns false
        fstack: []
        
Step 6: Stack empty, return 1
```

## Continuation Capture

When `call/cc` executes, it:

1. Copies the entire `fstack` into a `Continuation` object
2. Passes this object as an argument to its lambda

```javascript
// In CallCC.step()
const continuation = new Continuation(registers[FSTACK]);
registers[CTL] = new TailApp(this.lambdaExpr, [new Literal(continuation)]);
```

## Invoking a Continuation

When a `Continuation` is called as a function:

1. The interpreter compares current stack with target stack
2. Runs `after` thunks for `dynamic-wind` extents being exited
3. Runs `before` thunks for extents being entered
4. Replaces `fstack` with a copy of the captured stack
5. Sets `ans` to the continuation's argument

## JavaScript Interoperability

### Scheme → JavaScript
When Scheme code calls a JavaScript function:
1. The interpreter **pushes the current context** onto `jsContextStack`
2. Calls the JS function directly with raw values
3. **Pops the context** after the call returns
4. Puts the result in `ans`

### JavaScript → Scheme
Scheme closures are now **intrinsically callable JavaScript functions**:
1. Closures are created with attached Scheme metadata (params, body, env)
2. When JS calls a closure, the closure wrapper calls `interpreter.runWithSentinel(ast)`
3. `runWithSentinel` uses the **parent context** from `jsContextStack` for proper `dynamic-wind` handling
4. A `SentinelFrame` ensures the inner run terminates properly

### Callable Continuations
Continuations work the same way - they are callable JS functions:
1. When invoked from JS, they call `interpreter.invokeContinuation(k, value)`
2. The continuation invocation uses `ContinuationUnwind` to properly handle stack replacement
3. `dynamic-wind` thunks are correctly run during the unwind/rewind process

See [Interoperability.md](Interoperability.md) for detailed documentation.

## The Sentinel Frame Pattern

### Problem: Nested Interpreter Runs

When JavaScript calls a Scheme closure, we need to run the interpreter again while we're already inside a `run()` call. The challenge: how does the inner run know when to stop?

```
┌──────────────────────────────────────────────────────┐
│ Scheme code                                          │
│   (js-call "array.map" my-scheme-callback)           │
│        │                                             │
│        ▼                                             │
│ ┌────────────────────────────────────────────┐       │
│ │ JavaScript: array.map()                    │       │
│ │   for each element:                        │       │
│ │     callback(element)  ◄── calls Scheme!   │       │
│ │        │                                   │       │
│ │        ▼                                   │       │
│ │   ┌──────────────────────────────────┐     │       │
│ │   │ Nested Scheme: my-scheme-callback│     │       │
│ │   │   (process element)              │     │       │
│ │   │   Returns value                  │     │       │
│ │   └──────────────────────────────────┘     │       │
│ │     continues with next element...         │       │
│ └────────────────────────────────────────────┘       │
│   Continues after map returns                        │
└──────────────────────────────────────────────────────┘
```

### Solution: SentinelFrame as a Stop Marker

`SentinelFrame` is a special frame pushed onto the stack before nested `run()` calls:

```javascript
// In interpreter.js
runWithSentinel(ast) {
    const parentContext = this.getParentContext();
    const stackWithSentinel = [...parentContext, new SentinelFrame()];
    return this.run(ast, this.globalEnv, stackWithSentinel);
}
```

When the nested computation completes and pops down to `SentinelFrame`:

```javascript
class SentinelFrame {
    step(registers, interpreter) {
        throw new SentinelResult(registers[ANS]);  // Break out!
    }
}
```

The trampoline catches this and returns control to JavaScript:

```javascript
// In the trampoline loop
catch (e) {
    if (e instanceof SentinelResult) {
        return unpackForJs(e.value);  // Return to JS caller
    }
    // ... handle other exceptions
}
```

### Why Use Exceptions?

Using `SentinelResult` (a throw) rather than a return flag has advantages:

1. **Immediate exit**: Skips any remaining loop iterations
2. **Works at any depth**: The nested run could be many call levels deep
3. **Clean pattern**: Similar to coroutine libraries and continuation systems

### Filtering Sentinels from Continuations

When `call/cc` captures the stack, we must remove `SentinelFrame`s - they're boundary markers that don't make sense in a captured continuation:

```javascript
// In frames.js
function filterSentinelFrames(stack) {
    return stack.filter(f => f.constructor.name !== 'SentinelFrame');
}
```

This ensures that invoking a continuation doesn't accidentally terminate early.

### Summary

| Class | Role |
|-------|------|
| `SentinelFrame` | "Stop here" marker on the frame stack |
| `SentinelResult` | Exception to break out of nested `run()` |
| `jsContextStack` | Tracks parent context for `dynamic-wind` |
| `filterSentinelFrames()` | Removes markers when capturing continuations |

## File Organization

The stepable code is organized as:

| File | Contents |
|------|----------|
| `stepables_base.js` | Register constants (`ANS`, `CTL`, `ENV`, `FSTACK`) + `Executable` base class |
| `ast_nodes.js` | All AST node classes (Literal, Variable, Lambda, If, etc.) |
| `frames.js` | Most continuation frame classes (LetFrame, AppFrame, etc.) |
| `stepables.js` | Barrel file re-exporting everything (backwards compatibility) |
| `interpreter.js` | The trampoline loop, state management, and SentinelFrame |
| `frame_registry.js` | Factory functions to avoid circular imports |
| `winders.js` | Dynamic-wind stack walking utilities |

## Related Documentation

- [architecture.md](architecture.md) - High-level architecture
- [directory_structure.md](../directory_structure.md) - File organization
- [hygiene_implementation.md](hygiene_implementation.md) - Macro hygiene algorithm
