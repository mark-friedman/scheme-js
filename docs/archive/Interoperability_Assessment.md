# Interoperability Design Assessment

## Executive Summary
The proposed design in `docs/Interoperability.md` is **High Quality** and **Viable**. It correctly addresses the user's requirements for transparent interoperability between Scheme and JavaScript. The "Primitives are Primitives" philosophy effectively removes the friction of wrapper objects, allowing for seamless function calls in both directions.

## Detailed Analysis

### 1. Correctness
*   **Type Mapping**: The 1:1 mapping of shared types (Number, String, Boolean, Vector/Array) is correct and aligns with the goal of transparency.
*   **Function Calls (Scheme -> JS)**: The modification of `AppFrame` to handle raw JS functions is the correct approach. It allows Scheme to call standard libraries (e.g., `Math.max`, `console.log`) without wrappers.
*   **Function Calls (JS -> Scheme)**: The **Hybrid Representation** is the correct choice. It keeps internal `Closure` objects for efficient Scheme-to-Scheme TCO, but applies a "Bridge" wrapper when a closure "leaks" out to JavaScript (e.g. returned from `run()`). This avoids the pitfalls of the "Eager Bridge" approach.
*   **TCO & Continuations**: The analysis of TCO and `call/cc` semantics is accurate. Infinite cross-language recursion being bounded by the JS stack is a standard and acceptable limitation.

### 2. Quality
*   **Simplicity**: The design simplifies the runtime by removing `NativeJsFunction` and `Vector` wrappers.
*   **Performance**: Operating on raw JS primitives (especially arrays and numbers) will likely improve performance by avoiding object allocation and indirection.
*   **Usability**: The user experience is significantly improved. Writing `(console-log "Hello")` is much more natural than `(js-invoke console "log" (new Literal "Hello"))`.

### 3. Viability & Implementation Impact
The design is highly viable but requires careful implementation in the following areas:

*   **`src/data/vector.js`**: The `Vector` class should be removed. The Reader and Analyzer must be updated to produce raw JS arrays for vector literals (e.g., `#(...)`).
*   **`src/syntax/ast.js` (`AppFrame`)**: Needs to be updated to detect `typeof func === 'function'` and invoke it directly.
*   **`src/core/interpreter.js`**: Needs to implement the "Bridge" creation logic when a `Closure` is returned from `run()`.
*   **`src/data/values.js`**: `NativeJsFunction` can be removed.

### 4. Caveats and Recommendations

#### A. String Mutability
*   **Issue**: Scheme strings are typically mutable (`string-set!`). JS strings are immutable.
*   **Impact**: If the user requires `string-set!`, mapping Scheme strings directly to JS strings will make this operation impossible (or require converting to an object wrapper, breaking the "raw string" rule).
*   **Recommendation**: Accept that strings are immutable (like in Racket or modern JS/Scheme hybrids) or implement `string-set!` by returning a *new* string (which changes semantics). Given the "transparent interop" goal, **immutable strings** are the pragmatic choice.

#### B. Boolean Truthiness
*   **Issue**: In Scheme, only `#f` is false. In JS, `false`, `null`, `undefined`, `0`, `""` are falsy.
*   **Impact**: A JS function returning `null` will be treated as **true** by Scheme `if`.
*   **Recommendation**: This is acceptable and expected in Scheme. Users should be aware that they might need to check `(eq? val null)` if they care about JS `null`.

#### C. `undefined` vs `Void`
*   **Issue**: The design maps `Void` to `undefined`.
*   **Impact**: This is a good choice, as `undefined` is the natural "no value" in JS.

## Conclusion
The design is sound. It transforms the Scheme implementation from an isolated island into a fully integrated guest language within the JS ecosystem. I recommend proceeding with this implementation.
