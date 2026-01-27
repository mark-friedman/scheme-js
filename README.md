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

### Exact/Inexact Numbers (Full Numeric Tower)

R7RS distinguishes between **exact** numbers (mathematically precise) and **inexact** numbers (approximate). Our implementation provides a full R7RS-compliant numeric tower by leveraging JavaScript's native types and custom classes.

| Category | Internal Representation | Exactness |
|----------|-------------------------|-----------|
| **Integers** | JavaScript `BigInt` | **Exact** |
| **Reals** | JavaScript `Number` (IEEE 754) | **Inexact** |
| **Rationals** | `Rational` (BigInt Numerator/Denominator) | **Exact** |
| **Complex** | `Complex` (Exact or Inexact components) | **Varies** |

#### Key Features
- **Arbitrary Precision**: Exact integers use `BigInt`, allowing for calculations with hundreds of digits (e.g., `(factorial 100)`) without precision loss.
- **R7RS Semantics**: `exact?` and `inexact?` predicates work correctly. `5/1` is an exact integer, while `5.0` is an inexact real.
- **Rational Support**: Exact fractions like `1/3` are preserved and correctly handled in all arithmetic operations.

#### JavaScript Interop Boundary
To maintain safe interoperability with standard JavaScript code, the following rules apply at the language boundary:

1.  **Scheme → JS**:
    - **Safe Integers**: `BigInt` values within the safe range (`±2^53 - 1`) are automatically converted to standard JS `Number` types.
    - **Large Integers**: `BigInt` values exceeding this range throw an error during conversion to prevent silent precision loss (unless using explicit conversion).
    - **Rationals/Complex**: Converted to their closest `Number` representation or passed as opaque objects.
2.  **JS → Scheme**:
    - JavaScript **integers** are automatically converted to `BigInt` and treated as **exact** (preserving Scheme's preference for exactness).
    - JavaScript **non-integers** (floats) are imported as standard `Number` types and treated as **inexact** reals.
    - JavaScript `BigInt` values are imported as **exact** integers.

> [!NOTE]
> For performance-critical code where exactness is not required, see the `r7rs_roadmap.md` for planned optimizations like the `define-js-native` mode.

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
- **Bi-Directional**: Works seamlessly for both Scheme calling JS and JS calling Scheme.
- **Safe BigInts**: Errors are thrown when converting BigInts outside the safe JavaScript integer range to prevent silent precision loss.

**Exported Procedures:**

| Procedure | Description |
|-----------|-------------|
| `(scheme->js val)` | Shallow conversion: Scheme values to JS (BigInt→Number if safe). |
| `(scheme->js-deep val)` | Deep conversion: Vectors → Arrays, Records → Objects. |
| `(js->scheme val)` | Shallow conversion: JS values to Scheme (Number→BigInt if int). |
| `(js->scheme-deep val)` | Deep conversion: Arrays → Vectors, Objects → `js-object` records. |
| `(js-auto-convert [bool])` | Parameter to control automatic boundary conversion (default `#t`). |
| `(make-js-object)` | Creates a new empty `js-object` wrapper. |
| `(js-object? obj)` | Returns `#t` if `obj` is a `js-object` record. |
| `(js-ref obj key)` | Access a property on a JS object (key is a string). |
| `(js-set! obj key val)` | Set a property on a JS object. |

**Usage:**

```scheme
(import (scheme-js js-conversion))

;; JS Array → Scheme Vector (deep)
(define vec (js->scheme-deep (js-eval "[1, 2, [3]]")))

;; Scheme Vector → JS Array (deep)
(js-call "console.log" (scheme->js-deep #(1 2 3)))

;; Accessing JS Objects
(define obj (js->scheme-deep (js-eval "({x: 10})")))
(display (js-ref obj "x")) ; → 10
(js-set! obj "y" 20)

;; Creating and using js-object records
(define my-obj (make-js-object))
(js-set! my-obj "name" "example")
(display (js-object? my-obj)) ; → #t
```

**Limitations:**
- **Circular References**: Deep conversion does **not** support circular data structures. Attempting to convert a circular structure will result in a stack overflow error.

