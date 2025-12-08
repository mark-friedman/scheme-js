# Trampoline Execution Model

This document explains the core execution model of the Scheme interpreter.

## Overview

The interpreter uses a "trampoline" pattern to avoid using the JavaScript call stack for Scheme function calls. This enables:

- **Proper Tail Calls**: Tail-recursive Scheme functions run in constant stack space.
- **First-Class Continuations**: `call/cc` can capture and restore the entire computation state.
- **JavaScript Interoperability**: Scheme closures can be called from JavaScript and vice versa.

## The Register Machine

The interpreter state is modeled as a simple register machine with four registers:

| Register | Index | Purpose |
|----------|-------|---------|
| `ans` | 0 | **Answer** - Result of the last sub-computation |
| `ctl` | 1 | **Control** - The next AST node or Frame to execute |
| `env` | 2 | **Environment** - Current lexical environment |
| `fstack` | 3 | **Frame Stack** - The continuation (stack of pending operations) |

```javascript
// In interpreter.js
const registers = [null, ast, env, [...initialStack]];
//                 ans   ctl  env  fstack
```

## The Trampoline Loop

The main loop (`Interpreter.run()`) repeatedly calls `step()` on the current `ctl`:

```javascript
while (true) {
    if (this.step(registers)) {
        continue;  // step() returned true: tail call, keep going
    }
    
    // step() returned false: value computed
    const fstack = registers[3];
    
    if (fstack.length === 0) {
        return registers[0];  // Done! Return the answer
    }
    
    // Pop next frame and continue
    const frame = fstack.pop();
    registers[1] = frame;
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
Created by the Analyzer. Represent the structure of Scheme code:
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
const continuation = new Continuation(registers[3]);
registers[1] = new TailApp(this.lambdaExpr, [new Literal(continuation)]);
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
When Scheme code calls a JavaScript function, the interpreter:
1. Wraps any Scheme closures in "bridges" (plain JS functions)
2. Calls the JS function directly
3. Puts the result in `ans`

### JavaScript → Scheme
When JS code calls a bridged Scheme closure:
1. The bridge creates a new `TailApp` AST node
2. Calls `interpreter.run()` recursively
3. A `SentinelFrame` prevents the inner run from consuming parent frames

## File Organization

The AST and frame code is organized as:

| File | Contents |
|------|----------|
| `nodes.js` | AST node classes (Literal, Variable, Lambda, etc.) |
| `frames.js` | Continuation frames (AppFrame, IfFrame, etc.) |
| `frame_registry.js` | Factory functions to avoid circular imports |
| `winders.js` | Dynamic-wind stack walking utilities |
| `ast.js` | Barrel file re-exporting everything |

## Related Documentation

- [layer_plan.md](../layer_plan.md) - Overall architecture plan
- [directory_structure.md](../directory_structure.md) - File organization
