# Scheme R7RS-Small on JavaScript

A faithful, layered implementation of the **Scheme R7RS-Small** standard in JavaScript, designed for correctness, extensibility, and deep JavaScript interoperability.

## 🎯 Goals

### Implementation Highlights
- **R7RS-Small**: High degree of compatibility with the R7RS-small standard (see [R7RS Libraries](#r7rs-libraries)).
- **Tail Call Optimization (TCO)**: Proper tail recursion by the interpreter. Interleaved JS and Scheme code may still cause stack overflow.
- **First-Class Continuations**: Full support for `call/cc`.
- **JavaScript Interop**: Seamless calling between Scheme and JavaScript, including shared data structures and transparent boundary crossing. Scheme closures and continuations are first-class JavaScript functions.
- **Global Environment**: JavaScript global definitions (on `window` or `globalThis`) are automatically visible in the Scheme global environment.
- **Node.js REPL**: Full-featured interactive REPL with history, multiline support, and paren matching.
- **Browser REPL**: Interactive browser-based REPL via a custom `<scheme-repl>` web component.
- **Browser Scriptng**: Replace JavaScript in web apps with `<script type="text/scheme">` tags.  Direct Scheme evaluation by JavaScript is also supprted.

### Architectural Goals
- **Layered Design**: Build complex features (macros, data structures) on top of a minimal, robust kernel.
- **Maintainability**: Clear separation of AST, Runtime, and Library code.
- **Testability**: Comprehensive test suite running in both Node.js and Browser environments.

---

## 📦 Installation

### From Source
Clone the repository:
```bash
git clone https://github.com/mark-friedman/scheme-js-4.git
cd scheme-js-4
```

### Build Distribution Bundles
```bash
npm install
npm run build
```

This produces the following files in `dist/`:
- `scheme.js` — Core interpreter with `schemeEval` API
- `scheme-repl.js` — `<scheme-repl>` Web Component
- `scheme-html.js` — `<script type="text/scheme">` support

---

## 🚀 Getting Started

### Node.js REPL (from source)

Run the interactive REPL directly from source:
```bash
node repl.js
```

#### Execute an expression
```bash
node repl.js -e "(+ 1 2 3)"
# Output: 6
```

#### Run a Scheme file
```bash
node repl.js myprogram.scm
```

### Browser REPL (from source)

1. Start a local HTTP server in the project root:
    ```bash
    python3 -m http.server 8080
    ```

2. Open your browser to:
    [http://localhost:8080/web/index.html](http://localhost:8080/web/index.html)

3. Start typing Scheme code in the interactive shell!

---

## 📦 Using the Built Packages

After running `npm run build`, you can use the distribution bundles in various ways.

### Using `<scheme-repl>` Web Component

Embed a full REPL in any webpage:

```html
<!DOCTYPE html>
<html>
<head>
    <!-- Import Scheme implementation and Web Component -->
    <script type="module" src="dist/scheme.js"></script>
    <script type="module" src="dist/scheme-repl.js"></script>
</head>
<body>
    <scheme-repl></scheme-repl>
</body>
</html>
```

See `dist/repl-demo.html` for a complete working example.

### Using `<script type="text/scheme">`

Run Scheme code inline in HTML and interact with the DOM:

```html
<!-- Import Scheme implementation and HTML adapter -->
<script type="module" src="dist/scheme.js"></script>
<script type="module" src="dist/scheme-html.js"></script>

<button id="click-me">Click Count: 0</button>

<script type="text/scheme">
  (import (scheme base) (scheme-js interop))
  
  (let ((count 0)
        (btn (document.getElementById "click-me")))
    (btn.addEventListener "click"
      (lambda (event)
        (set! count (+ count 1))
        (set! btn.textContent (string-append "Click Count: " (number->string count))))))
</script>
```

### Using the JavaScript API

Import and use the interpreter programmatically:

```html
<script type="module">
import { schemeEval } from './dist/scheme.js';

// Evaluate Scheme expressions
const result = schemeEval('(+ 1 2 3)');
console.log(result); // 6

// Define and call functions
schemeEval('(define (greet name) (string-append "Hello, " name "!"))');
const greeting = schemeEval('(greet "World")');
console.log(greeting); // "Hello, World!"
</script>
```

---

## 🧪 Testing

The project uses a custom universal test runner that works in both Node.js and the Browser.

### Run All Tests (Node.js)
```bash
node run_tests_node.js
```

### Run Browser Tests
1. Ensure the HTTP server is running (see above).
2. Navigate to `http://localhost:8080/web/tests.html`.
3. The test suite runs automatically in the console on load.

---

## 📊 Benchmarks

The project includes a performance benchmark suite for tracking regressions.

```bash
npm run benchmark          # Run benchmarks and display results
npm run benchmark:save     # Save current results as baseline
npm run benchmark:compare  # Compare against baseline
```

Benchmarks cover arithmetic, list operations, and JS interop.

---

## 🔌 JavaScript Interoperability

This implementation provides deep integration between Scheme and JavaScript. See [docs/Interoperability.md](./docs/Interoperability.md) for complete technical details.

### Library: `(scheme-js interop)`

Import with: `(import (scheme-js interop))`

| Procedure | Description | Example |
|-----------|-------------|---------|
| `(js-eval str)` | Evaluate JavaScript code | `(js-eval "Math.PI")` → `3.14159...` |
| `(js-ref obj prop)` | Access object property | `(js-ref console "log")` |
| `(js-set! obj prop val)` | Set object property | `(js-set! obj "x" 42)` |
| `(js-invoke obj method args...)` | Call object method | `(js-invoke console "log" "Hi")` |
| `(js-obj key val ...)` | Create JS object | `(js-obj 'x 1 'y 2)` → `{x: 1, y: 2}` |
| `(js-obj-merge obj ...)` | Merge objects | `(js-obj-merge obj1 obj2)` → `{...obj1, ...obj2}` |
| `(js-typeof val)` | Get JS type | `(js-typeof 42)` → `"number"` |
| `js-undefined` | JS undefined value | `(eq? x js-undefined)` |
| `(js-undefined? val)` | Undefined predicate | `(js-undefined? x)` → `#t` |
| `js-null` | JS null value | `(eq? x js-null)` |
| `(js-null? val)` | Null predicate | `(js-null? x)` → `#t` |
| `(js-new constructor args...)` | Instantiate JS class | `(js-new Date 2024 0 1)` |

### Instantiating JavaScript Classes

Use `js-new` to create instances of JavaScript classes with the `new` operator:

```scheme
;; JavaScript globals are automatically available
(define now (js-new Date))
(define birthday (js-new Date 1990 0 1))

;; Standard library classes
(define my-map (js-new Map))
(my-map.set "key" "value")
(my-map.get "key")  ;; => "value"

;; Create arrays with specific length
(define arr (js-new Array 10))
arr.length  ;; => 10
```

### Dot Notation Syntax

Access JavaScript object properties using familiar dot notation:

```scheme
;; Property access
(define obj (js-eval "({name: 'alice', age: 30})"))
obj.name          ;; => "alice"
obj.age           ;; => 30

;; Chained access
(define nested (js-eval "({a: {b: {c: 42}}})"))
nested.a.b.c      ;; => 42

;; Property mutation
(set! obj.name "bob")
obj.name          ;; => "bob"

;; Method call
(obj.method args) ;; => (js-invoke obj "method" args)
```

**Under the hood:**
| Input | Transformed To |
|:------|:---------------|
| `obj.prop` | `(js-ref obj "prop")` |
| `(obj.method arg)` | `(js-invoke obj "method" arg)` |
| `(set! obj.prop val)` | `(js-set! obj "prop" val)` |

### Object Literal Syntax `#{...}`

Create JavaScript objects using a concise literal syntax:

```scheme
;; Basic object
#{(x 1) (y 2)}              ;; => {x: 1, y: 2}

;; With expressions
#{(sum (+ 1 2)) (pi 3.14)}  ;; => {sum: 3, pi: 3.14}

;; Spread syntax
(define base #{(a 1) (b 2)})
#{(... base) (c 3)}         ;; => {a: 1, b: 2, c: 3}
```

> [!IMPORTANT]
> **Literal Evaluation Semantics:** The `#{}` object literal syntax evaluates its values at runtime (like JavaScript), while `#()` vector literals do not evaluate their contents (following R7RS standard). When nesting object literals (or any evaluated expressions) inside vectors, use `(vector ...)` instead of `#(...)` to ensure the objects are evaluated:
> 
> ```scheme
> ;; Correct - objects are evaluated:
> (vector #{(x 1)} #{(y 2)})
> ;; => [object, object]
> 
> ;; Incorrect - creates unevaluated expressions:
> #(#{(x 1)} #{(y 2)})
> ;; => [cons-cell, cons-cell]
> ```


### Callable Closures

Scheme closures are callable JavaScript functions:

```scheme
;; Define a Scheme function
(define square (lambda (x) (* x x)))

;; Store it in a JS variable
(js-eval "var myFunc = null")
(set! myFunc square)
```

```javascript
// Call it from JavaScript!
myFunc(7);  // Returns 49
```

---

## ⏳ Promise Library: `(scheme-js promise)`

Import with: `(import (scheme-js promise))`

Provides transparent interoperability with JavaScript Promises.

| Procedure | Description |
|-----------|-------------|
| `(js-promise? obj)` | Returns `#t` if obj is a Promise |
| `(make-js-promise executor)` | Create Promise with `(lambda (resolve reject) ...)` |
| `(js-promise-resolve value)` | Create resolved Promise |
| `(js-promise-reject reason)` | Create rejected Promise |
| `(js-promise-then p handler)` | Attach fulfillment handler |
| `(js-promise-catch p handler)` | Attach rejection handler |
| `(js-promise-finally p thunk)` | Attach cleanup handler |
| `(js-promise-all list)` | Wait for all promises |
| `(js-promise-race list)` | Wait for first to settle |
| `(js-promise-all-settled list)` | Wait for all to settle |
| `(js-promise-map f p)` | Apply function to resolved value |
| `(js-promise-chain p f ...)` | Chain promise-returning functions |
| `(async-lambda formals body ...)` | Macro for CPS transformation |

**Example:**
```scheme
(import (scheme-js promise))

(define p (js-promise-resolve 42))
(js-promise-then p
  (lambda (x)
    (display x)
    (newline)))

;; Chain promises
(js-promise-chain (fetch-url "http://example.com")
  (lambda (response) (parse-json response))
  (lambda (data) (process data)))
```

---

## 📖 R7RS Libraries

The following R7RS-small standard libraries are supported:

| Library | Description |
|---------|-------------|
| `(scheme base)` | Core Scheme procedures and macros |
| `(scheme case-lambda)` | Multi-arity procedure dispatch |
| `(scheme char)` | Character predicates and case conversion |
| `(scheme complex)` | Complex number operations |
| `(scheme cxr)` | Extended car/cdr accessors (caar, cadr, etc.) |
| `(scheme eval)` | `eval` and `environment` |
| `(scheme file)` | File I/O (Node.js only) |
| `(scheme lazy)` | `delay`, `force`, `make-promise`, `promise?` |
| `(scheme process-context)` | `command-line`, `exit`, `get-environment-variable` |
| `(scheme read)` | `read` procedure |
| `(scheme repl)` | `interaction-environment` |
| `(scheme time)` | `current-second`, `current-jiffy`, `jiffies-per-second` |
| `(scheme write)` | `display`, `write`, `newline` |

**Extension Libraries:**

| Library | Description |
|---------|-------------|
| `(scheme-js interop)` | JavaScript interop (`js-eval`, `js-ref`, etc.) |
| `(scheme-js promise)` | JavaScript Promise integration |

---

## 🔧 Extensions Beyond R7RS-Small

### Macros

Standard R7RS macros plus extensions:

| Macro | Description |
|-------|-------------|
| `and`, `or` | Short-circuit boolean operations |
| `let`, `let*`, `letrec`, `letrec*` | Binding forms |
| `cond`, `case` | Conditional dispatch (with `=>` support) |
| `when`, `unless` | One-armed conditionals |
| `do` | Iteration construct |
| `guard` | Exception handling |
| `let-values`, `let*-values`, `define-values` | Multiple value bindings |
| `define-record-type` | R7RS record definitions |
| `case-lambda` | Multi-arity procedure dispatch |
| `define-class` | **Extension:** JS-compatible class definitions |

### `define-class` (Extension)

Define Scheme classes compatible with JavaScript inheritance.

**Syntax:**
```scheme
(define-class ClassName [ParentClass]
  constructor-name
  predicate-name
  (fields
    (field-name accessor [mutator])
    ...)
  [(constructor (params...)
    body...)]
  (methods
    (method-name (params...)
      body...)
    ...))
```

**Basic example:**
```scheme
(define-class Point
  make-point
  point?
  (fields (x point-x) (y point-y))
  (constructor (x y)
    (set! this.x x)
    (set! this.y y))
  (methods
    (magnitude ()
      (sqrt (+ (* this.x this.x) (* this.y this.y))))))

(define p (make-point 3 4))
(p.magnitude)  ;; => 5
```

**Custom constructor with explicit super call:**
```scheme
(define-class ColoredPoint Point
  make-colored-point
  colored-point?
  (fields (color point-color))
  (constructor (x y color)
    (super x y)              ;; Call parent constructor with custom args
    (set! this.color color))
  (methods
    (describe ()
      (string-append this.color " point"))))

(define cp (make-colored-point 3 4 "red"))
(cp.magnitude)  ;; => 5 (inherited)
(cp.describe)   ;; => "red point"
```

**Features:**
- **Inheritance**: Optional parent class
- **Constructor clause**: Custom initialization with `this` binding
- **Explicit super call**: `(super arg...)` to pass specific args to parent constructor
- **Super method calls**: `(super.methodName args...)` to call parent methods
- **Fields**: Define accessors and optional mutators
- **Methods**: Use `this` to access instance properties


### Additional Procedures

| Procedure | Library | Description |
|-----------|---------|-------------|
| `load` | REPL | Load and execute a Scheme file (Node.js only) |

---

## 📚 Documentation

Detailed documentation for project internals:

- [**Architecture & Directory Structure**](./docs/architecture.md): High-level design and detailed file map.
- [**Core Interpreter Implementation**](docs/core-interpreter-implementation.md): How the interpreter handles stack 
  frames, continuations and TCO.
- [**Hygiene Implementation**](./docs/hygiene.md): How macro hygiene works using pure marks/scopes.
- [**Macro Debugging**](./docs/macro_debugging.md): Troubleshooting common macro issues.
- [**JavaScript Interoperability**](./docs/Interoperability.md): Deep JS integration and callable closures.
- [**Changes**](./CHANGES.md): A log of major implementation steps and walkthroughs.
- [**Roadmap**](./ROADMAP.md): Compliance progress and future plans.

---

## 🏗️ Architecture

The project follows a **Two-Tier Architecture**:
1. **JavaScript Core**: The interpreter engine (`src/core/interpreter/`).
2. **Scheme Libraries**: The standard library in Scheme (`src/core/scheme/`).

### Core Components (`src/core/interpreter/`)
- **`interpreter.js`**: Trampoline execution loop.
- **`stepables_base.js`**: Register constants and `Executable` base class.
- **`ast_nodes.js`**: AST node classes (Literal, If, Lambda, etc.).
- **`frames.js`**: Continuation frame classes.
- **`library_loader.js`**: R7RS `define-library` and `import` support.

### Extensions (`src/extras/`)
- **`primitives/interop.js`**: JS interop procedures.
- **`primitives/promise.js`**: Promise library primitives.
- **`scheme/`**: Library definitions for extras.

For a detailed breakdown, see [docs/architecture.md](./docs/architecture.md).

---

## ⚠️ Limitations

### Hygiene

The macro system implements full **referential transparency** for `syntax-rules` macros:

- **Accidental capture prevention**: Macro-introduced bindings don't capture user variables
- **Definition-site bindings**: Free variables in macro templates correctly reference bindings from where the macro was defined, not where it's expanded

```scheme
;; Macro defined inside a let with a local helper
(let ((helper (lambda (x) (* x 2))))
  (define-syntax my-double
    (syntax-rules ()
      ((my-double x) (helper x))))
  ;; Use later, even with shadowed 'helper':
  (let ((helper (lambda (x) (+ x 1))))
    (my-double 5)))
;; Returns 10 (uses definition-site helper, not expansion-site)
```

### Exact/Inexact Numbers

JavaScript has only one numeric type (IEEE 754 double). Our implementation uses `Number.isInteger()` to distinguish exact from inexact:
- Integers (`5`, `-42`) are considered **exact**
- Non-integers (`3.14`, `0.5`) are considered **inexact**
- `Rational` objects are always **exact**

| Operation | R7RS Expected | Our Result |
|-----------|---------------|------------|
| `(inexact 5)` | Inexact `5.0` | `5` (still exact by our predicate) |
| `(inexact? (inexact 5))` | `#t` | `#f` ❌ |
| `(exact 3.0)` | Exact `3` | `3` (correct by coincidence) |

**Practical Impact:** Most programs don't rely on the exact/inexact distinction for integers.

### Promise and `call/cc` Interaction

Using `call/cc` across promise boundaries has limitations:

```scheme
;; WARNING: This will abandon the promise chain
(call/cc
  (lambda (k)
    (js-promise-then (js-promise-resolve 1)
      (lambda (x)
        (k (* x 10))  ; Escapes the promise chain!
        (+ x 1)))))   ; Never executes
```

Each `promise-then` callback runs in a fresh interpreter invocation. Continuations captured inside a callback only escape *that* callback, abandoning the JavaScript Promise chain.

---

## 🛠️ Code Standards

- **Style**: ES Modules (`import`/`export`), JSDoc documentation.
- **Testing**: Dual environment - all features must work in both Node.js and browsers.
- **Code Quality**: Strict separation of step-able logic from runtime state.

---

## 📄 License

MIT License
