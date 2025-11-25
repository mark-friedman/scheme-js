I'll start by creating a new markdown document `plan.md` that outlines the layered implementation plan for R7RS-small.


# Plan: R7RS-small Scheme Implementation

This document outlines a layered plan for implementing a compliant R7RS-small Scheme on top of the existing JavaScript-based "Unexceptional" continuation interpreter. The strategy prioritizes a robust syntactic foundation (macros) to allow the majority of the standard library to be implemented in Scheme itself, rather than as JavaScript primitives.

## Layer 1: The Syntactic Foundation (Macros & Core Data)

**Goal:** Enable the implementation of derived forms and richer data structures without touching the core interpreter loop. This layer focuses on upgrading the Reader, Analyzer, and Runtime to support `syntax-rules` macros and fundamental data types.

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

### Derived Forms (Scheme Macros)
These will be implemented in Scheme using the core forms:
*   `cond`, `case`, `and`, `or`, `when`, `unless`, `do`, `let`, `let*`, `letrec`, `letrec*`, `parameterize`, `guard`, `define-record-type`.

### Phase 1: Quasiquote & Quote

Before we can write macros comfortably, we need the ability to construct code templates easily.

1.  **Reader Update:** Ensure `` ` ``, `,`, and `,@` are parsed into `(quasiquote x)`, `(unquote x)`, and `(unquote-splicing x)`.
2.  **Analyzer/Macro Expansion:** Implement the expansion logic for `quasiquote`. While often implemented as a macro itself, in a bootstrap interpreter it's often easier to implement as a primitive syntactic form in the `Analyzer` that expands into `cons`, `list`, `append`, etc.

### Phase 2: The Macro System (syntax-rules)

1.  **`define-syntax`:** Add support to the `Analyzer` to recognize `(define-syntax name transformer)`.
    * This requires a separate "compile-time" environment or a flag in the current environment to distinguish macros from variables.
2.  **`syntax-rules` Transformer:**
    * Implement the pattern matching logic in JavaScript.
    * **Pattern Matching:** Match input syntax against literals, variables, and lists (including improper lists and ellipses `...`).
    * **Transcription:** Valid matches must generate new ASTs based on the associated template.
3.  **Hygiene (Basic):**
    * Initially, we can implement a non-hygienic system (essentially `defmacro`) to get things working, or go straight for a renaming mechanism to ensure hygiene (renaming local variables in macros so they don't clash with user variables). *Recommendation: Start non-hygienic for speed, then refactor.*

### Phase 3: Core Data Structures (The "Cons" Cell)

1.  **`Cons` Class:** Create a class `Cons { car, cdr }`.
2.  **Refactor `Reader`:** Change the reader to produce `Cons` chains instead of JS Arrays for lists.
3.  **Refactor `Analyzer`:** Update the analyzer to traverse `Cons` chains.
4.  **Primitives:** Add `car`, `cdr`, `cons`, `set-car!`, `set-cdr!`, `null?`, `pair?` to the global environment.
5.  **Symbols:** Introduce a `Symbol` class and an `intern` mechanism to distinguish symbols from strings.
6.  **Vectors:** Add support for Scheme vectors (`#(1 2 3)`), mapping them to JS arrays.

-----

## Layer 2: The Standard Library (Scheme-on-Scheme)

**Goal:** Implement the bulk of R7RS procedures using Scheme code defined in a "prelude" or "boot" file. This layer leverages the macro system from Layer 1.

### Phase 1: Derived Forms

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

### Phase 2: Records

* `define-record-type`. This is usually a macro that generates vectors or a specific JS object structure with predicate and accessor functions.

### Phase 3: Libraries

* `define-library`, `import`, `export`.
* This adds a meta-layer above the standard environment lookup, managing sets of bindings.

-----

## Outcome

At the end of Layer 1, we will have a powerful enough core to define complex control structures like `when`:

```scheme
(define-syntax when
  (syntax-rules ()
    ((when test stmt1 stmt2 ...)
     (if test (begin stmt1 stmt2 ...)))))
