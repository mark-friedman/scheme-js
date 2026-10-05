# JavaScript Interoperability Design

This document describes how Scheme-JS achieves **Transparent Interoperability** between Scheme and JavaScript.

## Core Philosophy: "Primitives are Primitives"

Runtime values in the registers (`ans`, `env` bindings) are raw JavaScript values whenever possible. There is no "wall" where you have to manually box/unbox numbers or wrap functions.

## Data Mapping Strategy

We use a "shared representation" model:

| Scheme Type | Internal Representation | JS typeof / instanceof | Notes |
| :---------- | :---------------------- | :--------------------- | :---- |
| **Exact integer** | JS `BigInt` | `'bigint'` | Converted at the boundary; see *Numbers at the boundary*. |
| **Inexact real** | Raw JS Number | `'number'` | Passed as is. |
| **Rational, complex** | Scheme objects | `'object'` | A rational becomes a JS number when passed to JavaScript. |
| **String** | Raw JS String, or a `SchemeString` | `'string'`, or `instanceof SchemeString` | A literal, a symbol's name or a string from JavaScript is a JS string, immutable; a newly made string is a `SchemeString`, which may be changed. JavaScript always receives a JS string; see *Strings at the boundary*. |
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

## Numbers at the boundary

JavaScript has one kind of number, so `1` and `1.0` cannot stay distinct once they cross. What each
direction does, the same in both tiers -- compiled code calls a JavaScript function as the
interpreter does (`callForeign` in `src/core/interpreter/values.js`):

| Case | Result |
|---|---|
| exact `1`, inexact `1.0` or `1/2` passed to a JavaScript function | a JS `number`: `1`, `1`, `0.5` |
| exact integer beyond ±2^53 passed to a JavaScript function | throws: outside the safe integer range |
| a list passed to a JavaScript function | the pairs are passed as they are, so their cars are still `BigInt`s |
| integral JS number read by `js-ref`, dot notation or `js-eval`, or returned by a JavaScript function, called directly, `(f 1)`, or through `js-invoke` | **exact** |
| an array or object a JavaScript function returns | JavaScript's own, its contents as they are: an integral number inside it is still a JS `number`, inexact in Scheme |
| JS `BigInt` returned by JavaScript | exact |
| a flonum Scheme stored in a property with `js-set!`, read back with `js-ref` | still inexact |

A JavaScript function's result is converted one level, with `jsToScheme`, however it is called:
its arguments are converted throughout on the way out, but a result converted throughout on the way
in would be a copy of every array and object JavaScript returned, which would then lose its
identity. A number inside an array or object is converted when it is read with `js-ref`.

A JavaScript function's **arguments** are converted the same way however it is called -- directly,
in tail position or not, as a method through dot notation or `js-invoke`, or as a constructor by
`js-new` -- in either tier, and nothing a program does changes that: each goes through
`schemeToJsDeep` (*The conversions*, below). So a vector reaches JavaScript as a new array of
converted elements, and JavaScript changing that array changes no Scheme vector; and an exact
integer beyond ±2^53, which has no JavaScript number, cannot be passed at all.

## Strings at the boundary

R7RS strings may be changed: every string a procedure newly allocates -- `make-string`'s,
`string-append`'s, `substring`'s, `number->string`'s -- can be altered with `string-set!`,
`string-fill!` and `string-copy!`, and the change is seen through every reference to it. A
JavaScript string cannot be: it is a value, with no identity, so it cannot be changed in place
for everyone holding it, and two strings with the same characters are the same value. So a
newly made Scheme string is an object, a `SchemeString`, which holds a JavaScript string until it
is first changed, and splits into an array only then; a string never changed costs one small
object.

JavaScript has no mutable strings, so **a string crosses the boundary as its value**, as a number
does:

| Case | Result |
|---|---|
| a Scheme string passed to a JavaScript function, stored with `js-set!` or in `js-obj`, or returned to JavaScript | a JS `string` holding its characters at that moment |
| the Scheme string changed afterwards | JavaScript's copy is unchanged |
| a string JavaScript returns, or a property read | a JS string, immutable in Scheme, like a literal: `string-set!` on it is an error, which says to use `string-copy` |
| a Scheme string passed to JavaScript and returned to Scheme | a JS string with the same characters, not the same Scheme string |

The last row is the price. Only a program that changes a string after sending it through
JavaScript and back, or compares strings with `eq?`, can see it: `equal?` and `string=?` compare
characters. It is the same kind of loss as a number's exactness through JavaScript.

`eq?` and `eqv?` compare a newly made string by identity, as R7RS says, so `case`, `memv` and
`assv` do not match one against a string datum: `(case (string-append "cl" "ick") (("click")
...))` takes no clause. A string from JavaScript is a JS string and still compares by value, so
`(case (js-ref event "type") (("click") ...))` does.

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

### Arguments From JS

A Scheme procedure called from Scheme with the wrong number of arguments signals an error, in
either tier. Called from JavaScript, it takes the arguments it has parameters for, as a JavaScript
function does: those beyond them are dropped, and a parameter given none is undefined. JavaScript
calls functions with whatever it has to give -- an event handler with the event, the callback of
`Array.prototype.map` with an index and the array -- so a handler need not declare what it does
not use.

```scheme
(js-invoke button "addEventListener" "click" (lambda () (display "clicked")))
(js-invoke #(1 2 3) "map" (lambda (x) (* x 10)))   ; called with x, index and array
```

A class's constructor passes its arguments on to its parent's, as `super(...args)` does, so a
parent's constructor body takes the same way the ones it has parameters for.

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

## Calling Scheme from JavaScript

A Scheme procedure is a JavaScript function, and called as one it behaves the same whichever tier
runs it -- interpreted, or compiled by the tier or ahead of time:

- **its arguments are converted into Scheme** with `jsToScheme`: an integral JavaScript number
  becomes an exact integer, and anything else arrives as it is;
- **the call runs to its end before JavaScript gets a value**: pending tail calls are run, a
  recursion deeper than the JavaScript stack moves to the interpreter's heap, and a continuation
  captured inside works;
- **its result is converted out of Scheme** with `schemeToJsDeep`: several values become the first,
  an exact integer a JavaScript number, a Scheme string a JavaScript string, a vector an array,
  recursively.

That is exactly `schemeToJsDeep(callSchemeProcedure(f, args.map(jsToScheme)))`, so JavaScript can
make any part of the call itself. All five are exported from the bundle:

```javascript
import { callSchemeProcedure, jsToScheme, jsToSchemeDeep, schemeToJs, schemeToJsDeep } from './scheme.js';
```

A continuation called from JavaScript converts its arguments the same way, and jumps to where it
was captured.

**Primitives are the exception.** A primitive -- `car`, `+`, a record's accessor -- is a JavaScript
function that takes and returns Scheme values, and its plain call converts nothing. To hand one to
JavaScript, wrap it in a procedure: `(lambda (x) (car x))`.

### The call that converts nothing

`callSchemeProcedure(proc, args)` calls a Scheme procedure with Scheme values and returns a Scheme
value. It is for JavaScript that holds Scheme values -- taken from a Scheme data structure, or to be
handed back to Scheme -- and would lose something by converting them: an exact integer's exactness, a
mutable string's identity, a list. In every other way it is the plain call: a closure or a compiled
procedure runs on its interpreter, so tail calls, deep recursion and continuations behave as they do
in Scheme. Anything else -- a primitive, a function of JavaScript's own -- is called directly.

What JavaScript then holds are Scheme's own representations (*Data Mapping Strategy*, above): an
exact integer is a `BigInt`, a list a chain of `Cons` pairs ending in `null`, a newly made string a
`SchemeString`, a character a `Char`, a symbol a `Symbol`, and several values a `Values`, whose
`first()` is the first.

### The conversions

| function | direction | converts |
|---|---|---|
| `jsToScheme` | into Scheme, one level | an integral number to an exact integer |
| `jsToSchemeDeep` | into Scheme, throughout | the same, and an array to a vector and a plain object to a `js-object` record, recursively |
| `schemeToJs` | out of Scheme, one level | several values to the first; an exact integer to a number, throwing beyond ±2^53; a rational to a number; a character or a Scheme string to a JavaScript string |
| `schemeToJsDeep` | out of Scheme, throughout | the same, and a vector to an array and a `js-object` record to a plain object, recursively; a list stays a list of pairs |

The plain call converts its arguments with `jsToScheme`, and its result with `schemeToJsDeep`,
always: nothing a program does changes either, so JavaScript can rely on what a call gives it.
JavaScript that wants other conversions makes them itself, around `callSchemeProcedure`. Going the
other way, Scheme calling a JavaScript function converts the arguments with `schemeToJsDeep` and the
result with `jsToScheme`, as `js-invoke` does: into Scheme one level, out of it throughout (*Numbers
at the boundary*).

The same conversions are Scheme procedures in `(scheme-js js-conversion)`: `js->scheme`,
`js->scheme-deep`, `scheme->js` and `scheme->js-deep`. They are how a program converts a value
itself, where the boundary does not. There is no setting that changes what the boundary does: a
parameter choosing the conversion would make one JavaScript call return different kinds of value as
the Scheme code beneath it changed, and would cost every call to a JavaScript function a look-up.

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

The Scheme interpreter's global environment automatically falls back to the JavaScript global context (`globalThis`) for any unbound variable. This allows you to access browser APIs, Node.js globals, or variables defined in other `<script>` tags directly by name. So does the environment of a library, or of a program that begins with import declarations, which otherwise sees only what it imports -- a name it neither imports nor defines is looked up in `globalThis`, so in Chrome an unimported `when` is `EventTarget.prototype.when`, not `(scheme base)`'s macro.

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
| `js-null` | The JavaScript `null` value. |
| `(js-null? val)` | Returns `#t` if val is `null`. |
| `(js-new constructor args ...)` | Creates a new instance using the `new` operator. |

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

**Where it applies.** R7RS allows a dot in an identifier, and portable code uses it -- SRFI 135's
reference implementation names a procedure `length&i0.length`. So dot notation is on in programs,
pages' scripts and the REPLs, where interop is written, and off in the files of a library loaded by
the library system, which are R7RS. Either can be changed for the rest of a file by a directive, as
`#!fold-case` changes case folding:

```scheme
#!no-dot-notation    ; node.left is an identifier from here on
#!dot-notation       ; document.title is a property access from here on
```

A library defined inline, in a program's file, is read as the program is. Anywhere, `|a.b|` is an
identifier.

**What it needs imported.** Dot notation is written as calls to `js-ref`, `js-invoke` and
`js-set!`, which `(scheme-js interop)` exports. A program or page script with no import
declarations sees everything, these included; one that begins with them, and a library, sees only
what it imports (R7RS 5.6.1), so imports `(scheme-js interop)` to use dot notation:

```scheme
(import (scheme base) (scheme-js interop))
(set! document.title "My Scheme App")
```

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