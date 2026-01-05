# Scheme R7RS-Small on JavaScript

A faithful, layered implementation of the **Scheme R7RS-Small** standard in JavaScript, designed for correctness, extensibility, and deep JavaScript interoperability.

## 🎯 Goals

### Language Goals
- **R7RS-Small Compliance**: Strictly follow the standard.
- **Tail Call Optimization (TCO)**: Proper handling of tail recursion (even when interleaved with JS) using a trampoline architecture.
- **First-Class Continuations**: Full support for `call/cc`, including `dynamic-wind` and multiple return values.
- **JavaScript Interop**: Seamless calling between Scheme and JavaScript, including shared data structures and transparent boundary crossing.

### Architectural Goals
- **Layered Design**: Build complex features (macros, data structures) on top of a minimal, robust kernel.
- **Maintainability**: Clear separation of AST, Runtime, and Library code.
- **Testability**: Comprehensive test suite running in both Node.js and Browser environments.

## 🏗️ Architecture

The project follows a **Two-Tier Architecture**:
1.  **JavaScript Core**: The interpreter engine that executes Scheme code (`src/core/interpreter/`).
2.  **Scheme Libraries**: The standard library implemented in Scheme itself (`src/core/scheme/`, `src/lib/`).

### The Core (`src/core/`)
The foundational layer is currently implemented and stable:
-   **`interpreter/`**: Trampoline interpreter with TCO via heap-managed frames.
-   **`primitives/`**: Native JavaScript implementations of core procedures (+, cons, etc.).
-   **`scheme/`**: Core Scheme subset (`base.scm`) defining macros like `and`, `let`, `cond`.

Key modules in `interpreter/`:
-   **`interpreter.js`**: Trampoline execution loop.
-   **`stepables_base.js`**: Register constants and `Executable` base class.
-   **`ast_nodes.js`**: AST node classes (Literal, If, Lambda, etc.).
-   **`frames.js`**: Continuation frame classes.
-   **`library_loader.js`**: R7RS `define-library` and `import` support.

For a detailed breakdown of the internal file structure, see [directory_structure.md](./directory_structure.md).

## 🚀 Getting Started

### Prerequisites
-   **Node.js**: v14+ (for running tests and CLI/server).
-   **Modern Browser**: (Optional) For running the Web REPL.

### Installation
Clone the repository:
```bash
git clone https://github.com/mark-friedman/scheme-js-4.git
cd scheme-js-4
```

### Running the Web REPL
1.  Start a local HTTP server in the project root:
    ```bash
    python3 -m http.server 8080
    ```
2.  Open your browser to:
    [http://localhost:8080/web/ui.html](http://localhost:8080/web/ui.html)
3.  Start typing Scheme code!

## 🧪 Testing

The project uses a custom universal test runner that works in both Node.js and the Browser.

### Run All Tests (Node.js)
Execute the complete test suite including unit, functional, and integration tests:
```bash
node run_tests_node.js
```

### Run Browser Tests
1.  Ensure the HTTP server is running (see above).
2.  Navigate to `http://localhost:8080/web/ui.html`.
3.  The test suite runs automatically in the console on load. Check the browser developer console (F12) to see the results.

## 📚 Documentation

We maintain detailed documentation for the project internals:

-   [**Directory Structure**](./directory_structure.md): Detailed map of the codebase and where files belong.
-   [**Architecture**](./docs/architecture.md): High-level architecture overview.
-   [**Trampoline Execution**](./docs/trampoline.md): A deep dive into how the interpreter handles stack frames and TCO.
-   [**Hygiene Implementation**](./docs/hygiene_implementation.md): How macro hygiene works.
-   [**Changes**](./CHANGES.md): A log of major implementation steps, walkthroughs of features, and refactors.

## 🛠️ Code Standards

-   **Style**: We use ES Modules (`import`/`export`) throughout. All functions are documented with JSDoc. Scheme code is written in a style that is similar to JSDoc.
-   **Testing**: We follow a "Dual Environment" rule - every new feature must represent correct behavior in both Node.js V8 and standard browser engines.
-   **Code Quality**: We separate "step-able" logic (instructions for the machine) from runtime state (values and environments).

## Limitations

### Hygiene

The current implementation addresses **accidental capture** (macro bindings don't capture user variables). It does NOT fully address **reference transparency** for free variables in templates that reference non-global bindings at macro definition time. However:
- Special forms (`if`, `let`, etc.) are recognized by the analyzer
- Primitives are globally bound
- These cover 99% of practical `syntax-rules` use cases

What is NOT handled are local definition-site bindings — If a macro is defined inside a let that binds a helper, and that name is shadowed at the expansion site:

```scheme
;; Macro defined inside a let with a local helper
(let ((helper (lambda (x) (* x 2))))      ;; definition-site binding
  (define-syntax my-double
    (syntax-rules ()
      ((my-double x) (helper x)))))
;; Later, at expansion site:
(let ((helper (lambda (x) (+ x 1))))      ;; shadows helper!
  (my-double 5))
;; BUG: Returns 6 (expansion-site helper) instead of 10 (definition-site helper)
```

Why this is rare in practice:
- `define-syntax` is almost always used at top level in R7RS
- Macros typically only reference globally-bound names
- Special forms are immune (they're recognized syntactically)

### Exact/Inexact Numbers

R7RS distinguishes between **exact** numbers (integers, rationals - mathematically precise) and **inexact** numbers (floating-point - approximate). The `exact` and `inexact` procedures convert between these representations, and `exact?`/`inexact?` test which category a number belongs to.

**JavaScript Limitation:**
JavaScript has only one numeric type (`number`), which is an IEEE 754 double-precision float. There is no native way to distinguish between `5` (an exact integer) and `5.0` (an inexact float) - they are identical values:

```javascript
5 === 5.0  // true in JavaScript
```

**Current Implementation:**
Our implementation uses `Number.isInteger()` to distinguish exact from inexact:
- Integers (`5`, `-42`) are considered **exact**
- Non-integers (`3.14`, `0.5`) are considered **inexact**
- `Rational` objects are always **exact**

**Semantic Differences from R7RS:**

| Operation | R7RS Expected | Our Result |
|-----------|---------------|------------|
| `(inexact 5)` | Inexact `5.0` | `5` (still exact by our predicate) |
| `(inexact? (inexact 5))` | `#t` | `#f` ❌ |
| `(exact 3.0)` | Exact `3` | `3` (correct by coincidence) |

**Practical Impact:**
- Most Scheme programs don't rely on the exact/inexact distinction for integers
- Arithmetic works correctly - only the `exact?`/`inexact?` predicates are affected
- Rationals (`1/2`, `3/4`) are fully supported and always exact
- This is documented as a known limitation

**Future Improvement:**
See `r7rs_roadmap.md` for potential approaches to fix this, including:
- Creating an `InexactNumber` wrapper class
- Using a tag map to track exactness separately

### JavaScript Promise Interoperability

The `(scheme-js promise)` library provides transparent interoperability with JavaScript Promises using a CPS (Continuation-Passing Style) approach.

**Basic Usage:**

```scheme
(import (scheme-js promise))

;; Create and work with promises
(define p (js-promise-resolve 42))
(js-promise-then p (lambda (x) (display x)))

;; Chain promises
(js-promise-chain (fetch-url "http://example.com")
  (lambda (response) (parse-json response))
  (lambda (data) (process data)))
```

**Available Procedures:**

| Procedure | Description |
|-----------|-------------|
| `(js-promise? obj)` | Returns `#t` if obj is a JavaScript Promise |
| `(make-js-promise executor)` | Creates a Promise with `(lambda (resolve reject) ...)` |
| `(js-promise-resolve value)` | Creates a resolved Promise |
| `(js-promise-reject reason)` | Creates a rejected Promise |
| `(js-promise-then p handler)` | Attaches fulfillment handler |
| `(js-promise-catch p handler)` | Attaches rejection handler |
| `(js-promise-all list)` | Waits for all promises |
| `(js-promise-race list)` | Waits for first to settle |
| `(js-promise-map f p)` | Apply function to resolved value |
| `(js-promise-chain p f ...)` | Chain promise-returning functions |

**Limitations with `call/cc`:**

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

**Why this happens:**
1. Each `promise-then` callback runs in a fresh interpreter invocation
2. Continuations captured inside a callback only escape *that* callback
3. The JavaScript Promise chain is abandoned, not just the Scheme continuation

**Safe patterns:**
- Use `call/cc` within a single callback (before any `await` points)
- Avoid jumping out of promise chains with continuations
- Consider using explicit error handling with `promise-catch`

### Deep JavaScript Interoperability

The `(scheme-js js-conversion)` library provides a robust system for converting complex data structures between Scheme and JavaScript.

**Features:**
- **Automatic Conversion**: The `js-auto-convert` parameter (default `#t`) automatically handles deep conversion of arguments and return values at the JS boundary.
- **Deep Conversion Procedures**:
  - `(scheme->js-deep val)`: Recursively converts Scheme lists/vectors to JS arrays and records to JS objects.
  - `(js->scheme-deep val)`: Recursively converts JS arrays to Scheme vectors and JS objects to `js-object` records.
- **Bi-Directional**: Works seamlessly for both Scheme calling JS and JS calling Scheme.
- **Safe BigInts**: Errors are thrown when converting BigInts outside the safe JavaScript integer range to prevent silent precision loss.

**Usage:**

```scheme
(import (scheme-js js-conversion))

;; JS Array -> Scheme Vector (deep)
(define vec (js->scheme-deep (js-eval "[1, 2, [3]]")))

;; Scheme List -> JS Array (deep)
(js-call "console.log" (scheme->js-deep '(1 2 (3))))

;; Accessing JS Objects
(define obj (js->scheme-deep (js-eval "({x: 10})")))
(display (js-ref obj "x")) ; -> 10
(js-set! obj "y" 20)
```

**Limitations:**
- **Circular References**: Deep conversion does **not** support circular data structures. Attempting to convert a circular structure will result in a stack overflow error.

