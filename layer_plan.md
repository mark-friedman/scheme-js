# Plan: R7RS-small Scheme Implementation

This document outlines a layered plan for implementing a compliant R7RS-small Scheme on top of the existing JavaScript-based "Unexceptional" continuation interpreter. The strategy prioritizes a robust syntactic foundation (macros) to allow the majority of the standard library to be implemented in Scheme itself, rather than as JavaScript primitives.

## Layer 1: The Syntactic Foundation (Macros & Core Data)

**Goal:** Enable the implementation of derived forms and richer data structures without touching the core interpreter loop. This layer focuses on upgrading the Reader, Analyzer, and Runtime to support `syntax-rules` macros and fundamental data types.

For detailed implementation steps, see [plan_layer_1.md](plan_layer_1.md).

### Core Special Forms (JavaScript Implementation)
These forms cannot be implemented as macros and must be supported by the interpreter directly:
*   `quote`: Prevents evaluation of syntax.
*   `lambda`: Creates closures.
*   `if`: Conditional execution.
*   `set!`: Variable assignment.
*   `define`: Variable definition (current scope).
*   `define-syntax`: Registers macro transformers.
*   `let-syntax`, `letrec-syntax`: Local macro definitions.
*   `define-library`, `import`, `export`: Module system.
*   `include`, `include-ci`: File inclusion.
*   `apply`: Invokes a procedure with a list of arguments.
*   `eval`: Evaluates an expression in a given environment.

### Derived Forms (Scheme Macros)
These will be implemented in Scheme using the core forms:
*   `cond`, `case`, `and`, `or`, `when`, `unless`, `do`, `let`, `let*`, `letrec`, `letrec*`, `parameterize`, `guard`, `define-record-type`.

### Summary of Work
*   **Quasiquote & Quote:** Enable template construction.
*   **Macro System:** Implement `define-syntax` and `syntax-rules`.
*   **Core Data Structures:** Implement `Cons` cells, `Symbol`s, `Vector`s and `Record`s.



-----

### Layer 1: The Syntactic Foundation (Macros & Core Data)

**Goal:** Enable the implementation of derived forms and richer data structures without touching the core interpreter loop.

**Key Components:**
*   **Macro System:** `syntax-rules` implementation.
*   **Core Data:** `Cons`, `Symbol`, `Vector`, `Record`.
*   **Boot Code:** `src/layer-1-kernel/scheme/boot.scm` (contains `and`, `let`, `cond`, etc.).

### Layer 2: The Standard Library (Scheme-on-Scheme)

**Goal:** Implement the bulk of R7RS procedures using Scheme code.

Implement standard special forms as macros:

* `cond`
* `case`
* `and`, `or`
* `when`, `unless`
* `do`
* `let*`
* `letrec*`

### Phase 2: List Library

Implement standard list processing procedures in Scheme:

* `map`, `for-each`
* `member`, `assoc`
* `fold`, `reduce`
* `append`, `reverse`

### Phase 3: Higher-Order Functions

* `apply`: Requires support in the `TailApp` node to handle a list of arguments.
* `call-with-values`, `values`: Requires extending the interpreter to support multiple return values (likely by returning a special `Values` object that is unpacked by `call-with-values` continuations).

-----

## Layer 3: Advanced Control & Environment

**Goal:** Fill in the complex runtime gaps that require interpreter support.

### Phase 1: Multiple Return Values

* Update `Interpreter` to handle the `values` primitive returning a special package.
* Update continuation frames to handle receiving multiple values (defaulting to the first one in single-value contexts).

### Phase 2: Dynamic Wind

* This interacts with `call/cc`. We need to track "winders" (before/after thunks) and invoke them correctly when a continuation jumps in or out of a dynamic extent.
* Requires a new `DynamicWindFrame` or similar mechanism.

### Phase 3: Exceptions

* `raise`, `guard`, `with-exception-handler`.
* These can be modeled on top of `call/cc` and `dynamic-wind`, or implemented as primitives for performance.

-----

## Layer 4: The R7RS Polish

**Goal:** Full compliance and robust system features.

### Phase 1: Ports & I/O

* Abstraction over `console.log` (output ports) and string/file input (input ports).
* `read`, `display`, `write`, `newline`.

### Phase 2: Hygienic Macros

*   The initial macro system is **non-hygienic** (similar to `defmacro` but with `syntax-rules` pattern matching).
*   **Goal:** Implement a fully hygienic renaming mechanism (e.g., syntactic closures or explicit renaming) to ensure safety and full R7RS compliance.

### Phase 3: Records

*   `define-record-type`. This is usually a macro that generates vectors or a specific JS object structure with predicate and accessor functions.

### Phase 4: Libraries

*   `define-library`, `import`, `export`.
*   This adds a meta-layer above the standard environment lookup, managing sets of bindings.

-----

## Outcome

At the end of Layer 1, we will have a powerful enough core to define complex control structures like `when`:

```scheme
(define-syntax when
  (syntax-rules ()
    ((when test stmt1 stmt2 ...)
     (if test (begin stmt1 stmt2 ...)))))
