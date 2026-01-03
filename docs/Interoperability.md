# JavaScript Interoperability Design

This document describes how Scheme-JS achieves **Transparent Interoperability** between Scheme and JavaScript.

## Core Philosophy: "Primitives are Primitives"

Runtime values in the registers (`ans`, `env` bindings) are raw JavaScript values whenever possible. There is no "wall" where you have to manually box/unbox numbers or wrap functions.

## Data Mapping Strategy

We use a "shared representation" model:

| Scheme Type | Internal Representation | JS typeof / instanceof | Notes |
| :---------- | :---------------------- | :--------------------- | :---- |
| **Number** | Raw JS Number | `'number'` | 1:1 mapping (also supports Rationals and Complex). |
| **String** | Raw JS String | `'string'` | 1:1 mapping. |
| **Boolean** | Raw JS Boolean | `'boolean'` | `#t` is `true`, `#f` is `false`. |
| **Vector** | Raw JS Array | `Array.isArray()` | `(vector 1 2)` is `[1, 2]`. |
| **Bytevector** | Raw JS `Uint8Array` | `instanceof Uint8Array` | `(bytevector 1 2 3)` is `Uint8Array([1,2,3])`. |
| **Void/Undefined** | Raw JS `undefined` | `'undefined'` | Used for `(if #f #t)`. |
| **Procedure** | **Callable Function** | `typeof === 'function'` | **Directly callable from JS!** |
| **Continuation** | **Callable Function** | `typeof === 'function'` | **Directly callable from JS!** |
| **JS Function** | Raw JS Function | `'function'` | Can be called directly by Scheme. |
| **Pair/List** | `Cons` instance | `instanceof Cons` | Scheme specific. JS sees an object `{car, cdr}`. |
| **Symbol** | `Symbol` instance | `instanceof Symbol` | Distinct from strings. |

## Callable Closures and Continuations

> [!IMPORTANT]
> As of the Callable Closures implementation, Scheme closures and continuations are **intrinsically callable JavaScript functions**. They can be stored in any JavaScript data structure (arrays, objects, Maps, Sets, global variables) and invoked directly.

### How It Works

When a `lambda` expression is evaluated, the interpreter creates a **callable JavaScript function** with attached Scheme metadata:

```javascript
// In values.js - createClosure factory function
function createClosure(params, body, env, restParam, interpreter) {
    // Create a callable JS function
    const closure = function(...jsArgs) {
        const argLiterals = jsArgs.map(val => new LiteralNode(val));
        const ast = new TailAppNode(new LiteralNode(closure), argLiterals);
        return interpreter.runWithSentinel(ast);
    };
    
    // Attach Scheme metadata
    closure[SCHEME_CLOSURE] = true;
    closure.params = params;
    closure.body = body;
    closure.env = env;
    closure.restParam = restParam;
    
    return closure;
}
```

### Usage Examples

```scheme
;; Store a closure in a JS global variable
(js-eval "var myCallback = null")
(set! myCallback (lambda (x) (* x x)))
```

```javascript
// Call it from JavaScript!
myCallback(7);  // Returns 49
```

```scheme
;; Store a closure in a vector and call it
(define callbacks (vector (lambda () "hello") (lambda () "world")))
((vector-ref callbacks 0))  ;; Returns "hello"
```

### Continuations From JS

Continuations are also callable:

```scheme
(define saved-k #f)
(+ 100 (call/cc (lambda (k) 
                  (set! saved-k k)
                  10)))
;; Returns 110

;; Later, from JavaScript:
saved-k(50)  ;; Returns 150
```

## Dynamic-Wind Context Tracking

When Scheme calls a JavaScript function, the interpreter tracks the current "Scheme context" (the frame stack including all `dynamic-wind` frames). If that JavaScript code calls back into Scheme via a callable closure or continuation, the proper context is preserved for correct `dynamic-wind` unwinding/rewinding.

This is implemented via `jsContextStack` in the interpreter:

```javascript
// Before calling a JS function from Scheme:
interpreter.pushJsContext(registers[FSTACK]);

// After JS returns:
interpreter.popJsContext();

// When re-entering Scheme, the parent context is used:
runWithSentinel(ast) {
    const parentContext = this.getParentContext();
    const stackWithSentinel = [...parentContext, new SentinelFrame()];
    return this.run(ast, this.globalEnv, stackWithSentinel);
}
```

## Type Checking

The type checking functions `isSchemeClosure()` and `isSchemeContinuation()` use marker symbols to distinguish Scheme callables from regular JavaScript functions:

```javascript
import { isSchemeClosure, isSchemeContinuation } from './values.js';

const fn = /* some function */;

if (isSchemeClosure(fn)) {
    // It's a Scheme closure - has .params, .body, .env properties
}

if (isSchemeContinuation(fn)) {
    // It's a Scheme continuation - has .fstack property
}
```

## TCO Semantics

The design maintains TCO:

- **Scheme-to-Scheme:** The interpreter loop handles these calls by updating registers. The JavaScript stack does not grow.
- **Scheme-to-JS:** When Scheme calls a JS function (like `Math.max`), it is a "leaf" call. The JS stack grows by one frame, returns a value, and shrinks.
- **JS-to-Scheme:** When JS calls a callable Scheme closure, a new interpreter loop is started via `runWithSentinel()`. Within that loop, Scheme execution is fully tail-recursive.

> [!WARNING]
> Infinite mutual recursion between JS and Scheme (JS calls Scheme which calls JS which calls Scheme...) is bounded by the JS engine's stack size. This is an unavoidable consequence of interoperating with a non-TCO language.

## `call/cc` Semantics

- **Capturing:** `call/cc` captures the `fstack` (continuation frames). This only contains Scheme frames, not native JS stack frames.
- **Restoring (Scheme context):** Invoking a continuation replaces the `fstack`. The interpreter jumps to the captured context.
- **Restoring (JS context):** If a JS callback invokes a continuation, this effectively **aborts** the JS callback. The captured Scheme context is restored, abandoning the rest of the JS callback logic. This is correct Scheme semantics - invoking a continuation *replaces* the future.

## Backwards Compatibility

The legacy `createJsBridge()` method is still available but is now essentially a passthrough for callable closures:

```javascript
createJsBridge(closure, parentStack = []) {
    // If it's already a callable Scheme closure, return it directly
    if (isSchemeClosure(closure)) {
        return closure;
    }
    // Legacy path for old-style Closure instances (during migration)
    return (...jsArgs) => { /* ... */ };
}
```

## JS Property Access Syntax

The reader provides JS-style dot notation for accessing JavaScript object properties:

### Reading Properties

```scheme
;; Define a JS object
(define obj (js-eval "({name: 'alice', age: 30})"))

;; Access properties using dot notation
obj.name    ;; => "alice"
obj.age     ;; => 30

;; Chained property access
(define nested (js-eval "({a: {b: {c: 42}}})"))
nested.a.b.c  ;; => 42
```

### Writing Properties

Use `set!` with dot notation to modify properties:

```scheme
(define obj (js-eval "({x: 0})"))
(set! obj.x 42)
obj.x  ;; => 42

;; Chained property set
(define nested (js-eval "({a: {b: 0}})"))
(set! nested.a.b 99)
nested.a.b  ;; => 99
```

### Under the Hood

The reader transforms dot notation at read time:

| Input | Transformed To |
|:------|:---------------|
| `obj.prop` | `(js-ref obj "prop")` |
| `obj.a.b` | `(js-ref (js-ref obj "a") "b")` |
| `(set! obj.prop val)` | `(js-set! obj "prop" val)` |

> [!NOTE]
> Numbers with dots like `3.14` are parsed as numbers, not property access.

## Summary

1. **Execution Model:** The interpreter registers hold raw JS values and class instances for Scheme-specific types.
2. **Scheme calling JS:** The evaluator invokes JS functions directly with `func(...args)`.
3. **JS calling Scheme:** Closures and continuations are callable functions. No explicit wrapping needed.
4. **Context Tracking:** The `jsContextStack` ensures proper `dynamic-wind` handling across boundaries.
5. **Type Identification:** Marker symbols (`SCHEME_CLOSURE`, `SCHEME_CONTINUATION`) distinguish Scheme callables from regular JS functions.
6. **Property Access:** Use `obj.prop` syntax to access JS object properties, and `(set! obj.prop val)` to modify them.

This design provides "Transparent Interoperability" - closures stored in JS globals, arrays, Maps, or any data structure can be invoked directly without special handling.