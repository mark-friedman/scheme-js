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
| **JS Object** | Raw JS Object | `'object'` | Can be created via `js-obj` or `#{...}` syntax. |
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

When a `lambda` expression is evaluated, the interpreter creates a **callable JavaScript function** with attached Scheme metadata. See [docs/architecture.md](./architecture.md) for technical details on the `Values.js` factory.

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

### Continuations From JS

Continuations are also callable and can be invoked from JS to jump back into a Scheme execution context:

```scheme
(define saved-k #f)
(+ 100 (call/cc (lambda (k) 
                   (set! saved-k k)
                   10)))
;; Returns 110

;; Later, from JavaScript:
saved-k(50)  ;; Returns 150
```

### Multiple Values in JavaScript

If a Scheme function returns multiple values (via `(values ...)`) to a JavaScript caller, JavaScript only receives the **first value**.

```scheme
(define (get-results) (values 1 2 3))
```

```javascript
const result = getResults(); // Returns 1
```

---

## Dynamic-Wind Context Tracking

When Scheme calls a JavaScript function, the interpreter tracks the current "Scheme context" (the frame stack including all `dynamic-wind` frames). If that JavaScript code calls back into Scheme via a callable closure or continuation, the proper context is preserved for correct `dynamic-wind` unwinding/rewinding.

This is implemented via a context stack in the interpreter that preserves the Scheme stack state across the JavaScript boundary.

## Type Identification

The types can be identified from JavaScript using the following markers:

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

## TCO and `call/cc` Semantics

- **TCO:** Scheme-to-Scheme calls are tail-recursive. JS-to-Scheme calls start a new interpreter loop, which is then tail-recursive internally.
- **`call/cc`:** Invoking a continuation from JS effectively **aborts** the JS callback (if it was called from Scheme) or simply jumps into the Scheme context (if it was a standalone call). The captured Scheme context is restored, replacing the current Scheme future.
- **Callable Closures**: Scheme procedures can be stored in JS variables and called like native functions.
- **Global JS Access**: JavaScript global variables (on `window` or `node global`) are automatically accessible in Scheme.

---

## Global JavaScript Access

The Scheme interpreter's global environment automatically falls back to the JavaScript global context (`globalThis`) for any unbound variable. This allows you to access browser APIs, Node.js globals, or variables defined in other `<script>` tags directly by name.

### Reading Global Variables

```scheme
(display console)        ;; Accesses globalThis.console
(display window.location) ;; Accesses window.location via dot notation
(define my-val someGlobal) ;; Accesses a variable defined in another JS file
```

### Writing Global Variables

You can also use `set!` to modify global JavaScript variables:

```scheme
(set! document.title "My Scheme App")
(set! myGlobalVar 123)
```

---

## JS Interop Primitives `(scheme-js interop)`

The following procedures provide low-level access to JavaScript:

| Procedure | Description |
|-----------|-------------|
| `(js-eval str)` | Evaluates a string as JavaScript code. |
| `(js-ref obj prop)` | Accesses a property on a JS object. |
| `(js-set! obj prop val)` | Sets a property on a JS object. |
| `(js-invoke obj method args ...)` | Invokes a method on a JS object. |
| `(js-obj k1 v1 ...)` | Creates a plain JS object from key-value pairs. |
| `(js-obj-merge obj ...)` | Merges multiple JS objects. |
| `(js-typeof val)` | Returns the JavaScript `typeof` as a string. |
| `js-undefined` | The JavaScript `undefined` value. |
| `(js-undefined? val)` | Returns `#t` if val is `undefined` or `null`. |

---

## JS Property Access Syntax

The reader provides concise syntax for common interop tasks:

### Dot Notation

The reader transforms dot notation into property access. When used in the operator position of a list, the analyzer further optimizes these into method calls.

| Input | Transformed To (Reader) | Optimized To (Analyzer) |
|:------|:------------------------|:-------------------------|
| `obj.prop` | `(js-ref obj "prop")` | - |
| `(obj.method arg)` | `((js-ref obj "method") arg)` | `(js-invoke obj "method" arg)` |
| `obj.a.b` | `(js-ref (js-ref obj "a") "b")` | - |
| `(set! obj.prop val)` | `(js-set! obj "prop" val)` | - |

> [!NOTE]
> Standard Scheme number syntax takes precedence. `3.14` is a number, not a property access on `3`.

### The `this` Pseudo-Variable

When a Scheme closure is invoked from JavaScript as a method (or via `js-invoke`), the JavaScript `this` context is automatically bound to a pseudo-variable named `this` within the closure's scope.

```scheme
(define obj #{(name "Alice")})
(js-set! obj "greet" (lambda (msg) 
                       (string-append msg ", " this.name)))

(obj.greet "Hello") ;; => "Hello, Alice"
```


### Object Literal Syntax `#{...}`

Create JavaScript objects using a concise syntax:

```scheme
#{(x 1) (y 2)}              ;; => {x: 1, y: 2}
#{(sum (+ 1 2)) (pi 3.14)}  ;; => {sum: 3, pi: 3.14}

;; Spread syntax
(define base #{(a 1) (b 2)})
#{(... base) (c 3)}         ;; => {a: 1, b: 2, c: 3}
```

This syntax expands to calls to `js-obj` and `js-obj-merge` at read-time.

---

## Class Interoperability: `define-class`

You can define Scheme classes that are compatible with JavaScript's class system and inheritance.

```scheme
(define-class ColoredPoint Point
  (make-colored-point x y color)
  colored-point?
  (fields
    (color point-color set-point-color!))
  (methods
    (get-description ((self))
      (string-append (point-color self) " point"))))
```

- **Inheritance:** `ColoredPoint` can extend a JS class or another Scheme class.
- **Constructors:** `make-colored-point` calls `super()` automatically if a parent exists.
- **Methods:** Methods are added to the JavaScript prototype, making them accessible to JS code.
- **`this` Binding:** Methods are called with `self` explicitly passed, but also have access to the JS `this` if needed via the implementation details.

---

## JS Promise Interoperability `(scheme-js promise)`

The Promise library provides CPS-style hooks for working with JS Promises. While `call/cc` cannot jump back *into* an awaited JavaScript frame (due to JS engine limitations), the `(scheme-js promise)` library provides safe patterns for asynchronous execution in Scheme.

See the README for a full list of `js-promise-` procedures.