# R7RS-Small Compliance Roadmap

A phased plan to achieve full R7RS-small compliance, building on the existing Layer 1 Kernel.

## Current Status

**Completed:**
- Trampoline interpreter with TCO
- First-class continuations (`call/cc`, `dynamic-wind`)
- Multiple values (`values`, `call-with-values`)
- Core data structures (`Cons`, `Symbol`, `Vector`)
- Basic macros (`define-syntax`, `syntax-rules`) — **Hygienic!** ✅
- `eval`, `apply`, Records
- **Library System Refactor:** Clean core/primitives split, `(scheme base)` is a facade.
- **Exceptions:** `error`, `raise`, `raise-continuable`, `with-exception-handler`, `guard` ✅
- **JS Exception Integration:** Scheme handlers catch JavaScript errors ✅
- **Type Predicates:** `number?`, `boolean?`, `procedure?`, `list?`, `symbol?`, `error-object?` ✅
- **Type/Arity/Range Checking:** All primitives validate inputs ✅
- **Full Numeric Tower:** BigInt exactness, Rationals, and Complex numbers successfully implemented and verified ✅
- **Chibi R7RS Compliance:** **982 passed, 0 failed, 24 skipped** (97.6% pass rate) ✅

**Incomplete:**
- **Library system** — `cond-expand` not implemented; full R7RS library clauses incomplete

---

## Phase -1: Packaging and Distribution ✅

Created build system to package the interpreter for Node.js and Browser use.

| Feature | Status | Notes |
|---------|--------|-------|
| Rollup Build | ✅ | Produces ESM bundles |
| Core Bundle | ✅ | `dist/scheme.js` with `schemeEval` API |
| HTML Adapter | ✅ | `<script type="text/scheme">` support |
| Shared Environment | ✅ | All scripts run in same interpreter instance |

**Deliverable:** `src/packaging/` and `rollup.config.js`.

---

## Phase 0: Library System Completion

Complete the R7RS library system.

| Feature | Status | Notes |
|---------|--------|-------|
| `export` (simple) | ✅ | |
| `export` (`rename`) | ✅ | |
| `import` with filters | ✅ | `only`, `except`, `prefix`, `rename` |
| `begin` clause | ✅ | |
| `include` | ✅ | |
| `include-ci` | ❌ Missing | Case-insensitive include |
| `include-library-declarations` | ❌ Missing | |
| `cond-expand` | ❌ Missing | Feature-based conditional |

**Deliverable:** Implement `cond-expand` in `library_loader.js`, add tests.

---

## Phase 1: Hygienic Macros ✅

Implemented proper hygiene for `syntax-rules` to avoid identifier capture.

| Feature | Status | Notes |
|---------|--------|-------|
| Mark introduced identifiers | ✅ | Alpha-renaming via gensym |
| Respect lexical scope | ✅ | Pattern variables vs. template literals |
| Hygiene tests | ✅ | 7 tests verify no identifier capture |

> [!NOTE]
> Implemented using alpha-renaming (gensyms).

**Deliverable:** `syntax_rules.js` updated, hygiene tests passing.

---

## Phase 1.5: Library Architecture Refactor ✅
**Target Library:** `(scheme primitives)`, `(scheme core)`, `(scheme base)`

Restructured the codebase to align with R7RS Appendix A.

| Feature | Status | Notes |
|---------|--------|-------|
| `(scheme primitives)` | ✅ | Registered JS-native primitives |
| `(scheme core)` | ✅ | Encapsulates core scheme implementations |
| `(scheme base)` | ✅ | Facade library re-exporting primitives & core |

**Deliverable:** Refactored `src/core/scheme/` and `library_loader.js`.

---

## Phase 2: Core Syntax (Standard Macros) ✅
**Target Library:** `(scheme base)`

Defined missing standard macros in `src/core/scheme/control.scm` and `macros.scm`.

| Macro | Status | Notes |
|-------|--------|-------|
| `or` | ✅ | |
| `let*` | ✅ | |
| `case` | ✅ | Uses `memv`, supports `=>` syntax |
| `when` | ✅ | |
| `unless` | ✅ | |
| `do` | ✅ | Normalized expansion |
| `letrec*` | ✅ | Sequential initialization |
| `let-values` | ✅ | Multiple value bindings |
| `let*-values` | ✅ | Sequential multiple value bindings |
| `define-values` | ✅ | Define from multiple values |
| `begin` | ⚠️ Analyzer | Special form in analyzer |

**Deliverable:** `src/core/scheme/control.scm` created and verified.

---

## Phase 3: Numeric Primitives ✅
**Target Library:** `(scheme base)`, `(scheme complex)`, `(scheme inexact)`

Completed in `math.js` and `numbers.scm` per R7RS §6.2.

| Primitive | Status | Notes |
|-----------|--------|-------|
| `<=`, `>=` | ✅ Scheme | In `numbers.scm` |
| `abs` | ✅ JS | In `math.js` |
| `quotient`, `remainder` | ✅ JS | In `math.js` |
| `number?` | ✅ | |
| `integer?` | ✅ JS | In `math.js` |
| `zero?`, `positive?`, `negative?` | ✅ Scheme | In `numbers.scm` |
| `odd?`, `even?` | ✅ Scheme | In `numbers.scm` |
| `max`, `min` | ✅ Scheme | In `numbers.scm` |
| `gcd`, `lcm` | ✅ Scheme | In `numbers.scm` |
| `floor`, `ceiling`, `truncate`, `round` | ✅ | JS + Scheme |
| `expt`, `sqrt` | ✅ JS | In `math.js` |
| `exact-integer-sqrt` | ✅ JS | Returns two values |
| `floor/`, `floor-quotient`, `floor-remainder` | ✅ JS | Floor division |
| `truncate/`, `truncate-quotient`, `truncate-remainder` | ✅ JS | Truncate division |
| Variadic `=`, `<`, `>`, `<=`, `>=` | ✅ Scheme | In `numbers.scm` |

> [!NOTE]
> **Scope decision:** We will **not** implement full exact rationals or complex numbers.

**Deliverable:** ✅ Done in `math.js` and `numbers.scm`.

---

## Phase 4: Boolean & Equivalence ✅
**Target Library:** `(scheme base)`

| Primitive | Status | Notes |
|-----------|--------|-------|
| `not` | ✅ | |
| `boolean?` | ✅ | |
| `boolean=?` | ✅ JS | In `eq.js` |
| `eq?` | ✅ | |
| `eqv?` | ✅ | |
| `equal?` | ✅ Scheme | In `equality.scm` |

**Deliverable:** ✅ Done.

---

## Phase 5: List Procedures ✅
**Target Library:** `(scheme base)`, `(scheme cxr)`

Expanded `list.scm` and `cxr.scm` to cover R7RS §6.4.

| Primitive | Status | Notes |
|-----------|--------|-------|
| `list?` | ✅ | |
| `length` | ✅ Scheme | In `list.scm` |
| `list-ref`, `list-tail` | ✅ Scheme | In `list.scm` |
| `reverse` | ✅ Scheme | In `list.scm` |
| `memq`, `memv`, `member` | ✅ Scheme | In `list.scm` with type checking |
| `assq`, `assv`, `assoc` | ✅ Scheme | In `list.scm` |
| `list-copy` | ✅ Scheme | In `list.scm` |
| `cadr`, `cddr`, `caddr`, `cdddr`, `cadddr` | ✅ | |
| All 28 cxr accessors | ✅ Scheme | In `cxr.scm` |

**Deliverable:** ✅ Done in `list.scm` and `cxr.scm`.

---

## Phase 6: Characters ✅
**Target Library:** `(scheme char)`, `(scheme base)`

Implemented `char.js` for R7RS §6.6.

| Primitive | Status | Notes |
|-----------|--------|-------|
| `char?` | ✅ | |
| `char=?`, `char<?`, etc. | ✅ | Variadic comparison |
| `char-ci=?`, etc. | ✅ | Case-insensitive in `(scheme char)` |
| `char-alphabetic?`, etc. | ✅ | Character class predicates |
| `char-upcase`, `char-downcase`, `char-foldcase` | ✅ | |
| `char->integer`, `integer->char` | ✅ | |
| `digit-value` | ✅ | |

> [!NOTE]
> Characters are represented as single-character JavaScript strings for JS interop.

**Deliverable:** ✅ Done in `char.js`, `char.sld`, reader updated for `#\...` literals.

---

## Phase 7: Strings ✅
**Target Library:** `(scheme base)`

Expanded `string.js` for R7RS §6.7.

| Primitive | Status | Notes |
|-----------|--------|-------|
| `make-string` | ✅ | |
| `string` | ✅ | From character args |
| `string-length`, `string-ref` | ✅ | |
| `string-set!` | ✅ | Raises error (immutable) |
| `string=?`, `string<?`, etc. | ✅ | Variadic comparison |
| `string-ci=?`, etc. | ✅ | Case-insensitive |
| `substring` | ✅ | |
| `string->list`, `list->string` | ✅ | |
| `string-copy` | ✅ | With optional start/end |
| `string-fill!` | ✅ | Raises error (immutable) |
| `string-upcase`, `string-downcase`, `string-foldcase` | ✅ | |
| `string->number` | ✅ | With radix support |

> [!IMPORTANT]
> **Immutability Decision:** `string-set!` and `string-fill!` raise errors for JavaScript interoperability.

**Deliverable:** ✅ Done in `string.js`. Updated `(scheme base)`.

---

## Phase 8: Vectors (Expansion) ✅
**Target Library:** `(scheme base)`

Expanded `vector.js` with additional R7RS operations.

| Primitive | Status | Notes |
|-----------|--------|-------|
| `vector-fill!` | ✅ | With optional start/end |
| `vector-copy` | ✅ | With optional start/end |
| `vector-copy!` | ✅ | Handles overlapping correctly |
| `vector->string`, `string->vector` | ✅ | |
| `vector-append` | ✅ | Variadic |

**Deliverable:** ✅ Done in `vector.js`. Updated `(scheme base)`.

---

## Phase 9: Control Flow ✅
**Target Library:** `(scheme base)`, `(scheme case-lambda)`

| Primitive | Status | Notes |
|-----------|--------|-------|
| `procedure?` | ✅ | In `control.js` |
| `map` | ✅ Scheme | In `list.scm` with type checking |
| `for-each` | ✅ Scheme | In `list.scm` with type checking |
| `case-lambda` | ✅ Scheme | In `case_lambda.scm`, dispatches on arity |

**Deliverable:** Complete.
- `for-each` in `src/core/scheme/list.scm`
- `case-lambda` macro in `src/core/scheme/case_lambda.scm`
- `(scheme case-lambda)` library in `src/core/scheme/case-lambda.sld`

---

## Phase 10: Input/Output (Ports) ✅
**Target Library:** `(scheme write)`, `(scheme read)`, `(scheme file)`, `(scheme base)`

Implemented textual string ports for in-memory I/O. File I/O deferred due to async complexity.

| Primitive | Status | Notes |
|-----------|--------|-------|
| `port?`, `input-port?`, `output-port?` | ✅ | Port predicates |
| `textual-port?`, `binary-port?` | ✅ | Type predicates |
| `input-port-open?`, `output-port-open?` | ✅ | Open state |
| `current-input-port`, `current-output-port`, `current-error-port` | ✅ | Default ports |
| `close-port`, `close-input-port`, `close-output-port` | ✅ | Port control |
| `open-input-string`, `open-output-string`, `get-output-string` | ✅ | String ports |
| `read-char`, `peek-char`, `char-ready?` | ✅ | Character input |
| `read-line`, `read-string` | ✅ | String input |
| `eof-object`, `eof-object?` | ✅ | EOF handling |
| `write-char`, `write-string` | ✅ | Character/string output |
| `display`, `newline`, `write` | ✅ | Formatted output with optional port |
| `flush-output-port` | ✅ | Flush buffered output |
| `open-input-file`, `open-output-file` | ✅ | Node.js only |
| `call-with-input-file`, `call-with-output-file` | ✅ | Node.js only |
| `file-exists?`, `delete-file` | ✅ | Node.js only |
| `read` | ✅ | S-expression parsing from ports |

> [!NOTE]
> File I/O (`(scheme file)`) only works in Node.js. Browser calls raise errors.
> Binary ports and bytevector operations are deferred to Phase 12.

**Deliverable:** ✅ Done in `io.js`, `write.sld`, `read.sld`, `file.sld`. Updated `(scheme base)`.

---

## Phase 11: Exceptions & Errors ✅
**Target Library:** `(scheme base)`

R7RS §6.11 requires: `error`, `raise`, `raise-continuable`, `with-exception-handler`, `guard`.

| Primitive | Status | Notes |
|-----------|--------|-------|
| `error` | ✅ | Creates and raises SchemeError |
| `raise` | ✅ | Non-continuable |
| `raise-continuable` | ✅ | Handler return becomes value |
| `with-exception-handler` | ✅ | Primitive |
| `guard` | ✅ Scheme | Macro in `control.scm` |
| `error-object?` | ✅ | |
| `error-object-message` | ✅ | |
| `error-object-irritants` | ✅ | |
| **JS Exception Integration** | ✅ | Scheme handlers catch JS errors |

**Deliverable:** ~~Exception system integrated with continuations.~~ Done!

---

## Phase 12: Bytevectors ✅
**Target Library:** `(scheme base)` (basics)

R7RS §6.9 defines bytevectors. Implemented in `src/core/primitives/bytevector.js`.

| Primitive | Status | Notes |
|-----------|--------|-------|
| `bytevector?` | ✅ | Type predicate |
| `make-bytevector` | ✅ | Constructor with optional fill |
| `bytevector` | ✅ | Construct from bytes |
| `bytevector-length` | ✅ | Return length |
| `bytevector-u8-ref` | ✅ | Read byte |
| `bytevector-u8-set!` | ✅ | Write byte |
| `bytevector-copy` | ✅ | Copy with optional start/end |
| `bytevector-copy!` | ✅ | Copy into existing |
| `bytevector-append` | ✅ | Concatenate bytevectors |
| `utf8->string` | ✅ | Convert to string |
| `string->utf8` | ✅ | Convert from string |

---

## Phase 13: Final R7RS Libraries ✅
**Target Library:** `(scheme repl)`, `(scheme load)`, `(scheme process-context)`, `(scheme time)`, `(scheme lazy)`, `(scheme eval)`

Implemented the remaining R7RS standard libraries:

| Library | Status | Notes |
|---------|--------|-------|
| `(scheme lazy)` | ✅ | `delay`, `force`, `delay-force`, `make-promise`, `promise?` |
| `(scheme eval)` | ✅ | `eval`, `environment` |
| `(scheme load)` | ⚠️ Deferred | Complex, environment-specific |
| `(scheme process-context)` | ✅ | `command-line`, `exit`, `get-environment-variable` |
| `(scheme repl)` | ✅ | `interaction-environment` |
| `(scheme time)` | ✅ | `current-second`, `current-jiffy`, `jiffies-per-second` |

---


## Phase 14: Advanced Interop
**Target:** Enhance Scheme <-> JS usability

| Feature | Description |
|---------|-------------|
| **Dot-Syntax** | Concise syntax for JS method calls (e.g. `(.log console "Hello")`). |
| **Iterable Lists** | Make `Cons` implement JS Iterable protocol for easier use with `Array.from`, spread syntax, etc. |
| **JS Subclassing** | Mechanism to define Scheme records that subclass native JS classes. |

---

## Phase 15: Full Numeric Tower ✅
**Target:** R7RS full numeric compliance

Implemented arbitrary-precision exact integers using `BigInt`, exact rationals, and complex numbers with proper exactness tracking per R7RS §6.2.

| Feature | Status | Notes |
|---------|--------|-------|
| **Exact Integers** | ✅ | BigInt support (`5n`) |
| **Rationals** | ✅ | Fraction support (`1/3`) |
| **Complex Numbers** | ✅ | Complex number support (`3+4i`) |

**Deliverable:** Updated numeric primitives and reader, all numeric compliance tests passing.

---

## Numeric Performance Optimization
**Target:** Reduce boundary friction and arithmetic overhead

Following the implementation of the full numeric tower, several optimizations have been identified to mitigate the performance impact of `BigInt` operations and JavaScript boundary conversions.

| Priority | Optimization | Effort | Impact | Notes |
|----------|--------------|--------|--------|-------|
| **High** | Precompute `MIN_SAFE_BIG`/`MAX_SAFE_BIG` | 5 min | Minor | Faster safe-range checks in `schemeToJs`. |
| **High** | Skip internal conversions | 1 hour | Significant | Add `external` flag to `run()` to skip `unpackForJs` during library loading and macro expansion. |
| **Medium** | LRU cache for BigInt→Number | 30 min | Moderate | Helps when the same exact integers cross the JS boundary repeatedly. |
| **Medium** | Smart shallow convert | 1 hour | Moderate | Fast-path for primitive arrays/vectors to avoid recursive overhead in `schemeToJsDeep`. |
| **Low** | `SchemeInt` wrapper class | 2-4 h | Variable | Persistent caching of Number representation on the integer object itself. |
| **Low** | `define-js-native` mode | 4-8 h | High | Opt-in pragma to use JS Numbers directly for performance-critical hot loops where exactness is not required. |

> [!TIP]
> **Boundary Friction vs. Arithmetic:** Profiling indicates that for many workloads, the cost of converting `BigInt` to `Number` at the JS boundary is more significant than the `BigInt` arithmetic itself. The "High" priority items address the most frequent conversion points.


---

## Phase 16: Developer Experience
**Target:** Debugging and usability

| Feature | Description |
|---------|-------------|
| **Source Locations** | Track line/column numbers in AST for better error reporting. |
| **Stack Traces** | Readable Scheme stack traces (filtering internal JS frames). |

---

## Phase 18: Modular Analyzer Refactoring (Future)
**Target:** Improve analyzer architecture for extensibility.

> [!NOTE]
> An experimental class-based `SyntacticAnalyzer` with pluggable special form handlers was prototyped but never integrated. It was removed during code cleanup (Dec 2024).

### Potential Benefits
| Benefit | Description |
|---------|-------------|
| **Extensibility** | Register custom special forms at runtime without modifying core code |
| **Testability** | Each handler is a pure function, easily unit-testable in isolation |
| **DSL potential** | Could support dialect variations (e.g., Racket-flavored syntax) |

### Considerations
| Concern | Notes |
|---------|-------|
| **Hygiene complexity** | Main analyzer now handles `SyntaxObject` wrapping, scoped macros, and `let-syntax` which would need careful porting |
| **Performance** | Map lookup per expression vs. direct switch dispatch (likely negligible) |
| **Migration effort** | Would require maintaining two implementations during transition |

### Recommendation
**Pursue after R7RS compliance is complete.** A good approach:
1. Extract handlers from `analyzer.js` into standalone functions (same file initially)
2. Only split into modules if file exceeds ~1000 lines
3. Consider making special form registration opt-in for custom DSLs

---

## Phase 17: Robust Hygiene ✅

| Feature | Description | Status |
|---------|-------------|--------|
| **Referential Transparency** | Static alpha-renaming ensures identifiers are resolved in their original lexical context. | ✅ |
| **Shadowing** | Local bindings correctly shadow global or macro-introduced bindings. | ✅ |


---

## Verification Plan

### Automated Tests
Each phase adds tests to:
- `tests/core/interpreter/` — JavaScript module tests
- `tests/functional/` — Integration tests
- `tests/core/scheme/` — Scheme-based tests

Run all tests:
```bash
node run_tests_node.js
```

### R7RS Conformance Suite
Consider running the [Chibi Scheme R7RS test suite](https://github.com/ashinn/chibi-scheme/tree/master/tests) against this implementation as a final validation step.

---

## Recommended Priority Order

1. **Phase 0–2** — Foundation: library system, hygiene, standard macros.
2. **Phase 3–5** — Primitives: numerics, booleans, lists.
3. **Phase 6–8** — Data types: characters, strings, vectors.
4. **Phase 9** — Control flow (small).
5. **Phase 10** — I/O (largest effort, async challenges).
6. **Phase 11** — Exceptions (robust error handling).
7. **Phase 12–13** — Bytevectors and remaining R7RS libraries.

---

## Future Improvements (Deferred)

### High-Precision Inexact Numbers (Future)

For applications requiring more precision than IEEE 754 doubles (e.g., scientific computing, financial calculations), consider integrating **[decimal.js](https://github.com/MikeMcl/decimal.js)**.

**Use Cases:**
- Arbitrary precision decimal arithmetic (100+ digits)
- Avoiding binary float quirks (`0.1 + 0.2 ≠ 0.3`)
- Financial applications requiring exact decimal representation

**Implementation Notes:**
- Would be an optional "extended precision" mode
- Not required for R7RS compliance
- Could be exposed via a `(scheme decimal)` library

**Decision:** Deferred. Implement if user demand requires high-precision inexact arithmetic.

---

### Delimited Continuations (Future)

For improved async semantics and cleaner control flow, consider implementing **delimited continuations** (`shift`/`reset` or `control`/`prompt`).

**Motivation:**
- The current `(scheme-js promise)` library uses CPS transformation for async/await
- Full `call/cc` has problematic interactions with JavaScript Promise chains
- Delimited continuations (`shift`/`reset`) would provide cleaner semantics

**Benefits:**
- `shift` captures only up to the enclosing `reset` (not the whole program)
- Natural fit for async operations - continuation becomes callback
- TCO preserved within each delimited segment
- More principled than full `call/cc` for async patterns

**Example Usage (proposed):**
```scheme
(define (fetch-data url)
  (reset
    (let ((response (shift k (promise-then (fetch url) k))))
      (let ((json (shift k (promise-then (parse-json response) k))))
        json))))
```

**Implementation Notes:**
- Based on SRFI-226 "Control Features" (partial) or a simpler `shift`/`reset`
- Would require modifications to the trampoline and frame stack
- Could coexist with existing `call/cc`

**Decision:** Deferred. Consider after JavaScript Promise integration is battle-tested.

