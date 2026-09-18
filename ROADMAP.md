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
- **Exceptions:** `error`, `raise`, `raise-continuable`, `with-exception-handler`, `guard`. **Exception Debugging (pause-on-error) fully enabled.** ✅
- **Type Predicates:** `number?`, `boolean?`, `procedure?`, `list?`, `symbol?`, `error-object?` ✅
- **Reader Refactor:** Modularized `reader.js` into focused submodules with expanded unit testing ✅
- **Type/Arity/Range Checking:** All primitives validate inputs ✅
- **Full Numeric Tower:** BigInt exactness, Rationals, and Complex numbers successfully implemented and fully verified across all JS-interop scenarios ✅
- **Printer:** Expanded coverage for exact integers, rationals, and complex numbers ✅
- **Interop Standardization:** `schemeToJsDeep` BigInt conversion standardized; 'raw' mode and Bridge exactness preservation implemented ✅
- **Chibi R7RS Compliance:** **982 passed, 0 failed, 24 skipped** (100% of applicable tests) ✅
- **Chapter R7RS Compliance:** **219 passed, 0 failed, 0 skipped** (100% compliance) ✅
- **Macro Hygiene:** Pure marks hygiene system fully verified with standard compliance tests. ✅
- **Object Printing:** Proper `#{(key val)...}` syntax for JS objects with circular support ✅
- **JS Interop Conversion Safety:** Implemented "Scheme-aware" primitive marking to prevent breaking Scheme exactness while enabling auto-conversion for foreign JS functions. Verified with 1657+ tests. ✅
- **JS Interop Benchmarks:** Added realistic interop benchmarks and verified boundary conversion costs (Deep In / Shallow Out) ✅
- **Async Execution Model:** Implemented `runAsync` and `evaluateStringAsync` with configurable yields. Verified TCO, `call/cc`, and JS interop under async execution with 1977 passing tests. ✅
- **Debugger Infrastructure**: Implemented `BreakpointManager`, `StackTracer`, `PauseController`, and `StateInspector`.
- **REPL Debugging**:- [x] Full integration in Node.js and Browser REPLs with `:bt`, `:locals`, `:eval`, and `:continue`. ✅
  - [x] Dynamic switching between "Fast Mode" (Sync) and "Debug Mode" (Async) in REPLs. ✅
  - [x] **REPL Evaluation Lock:** Implemented state-based locking in browser and Node.js REPLs to prevent concurrent evaluations and hide the prompt during long-running operations. ✅
  - [x] **REPL Pause Button:** Added a "PAUSE" button to the browser REPL to interrupt long-running asynchronous evaluations and enter the debugger. ✅

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
| Web Component | ✅ | `<scheme-repl>` custom element |
| Shared Environment | ✅ | All scripts run in same interpreter instance |
| **Bundled Libraries** | ✅ | Embeds Scheme sources into the JS bundle for file-free loading |

**Deliverable:** `src/packaging/`, `rollup.config.js`, and `scripts/generate_bundled_libraries.js`.

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


## Phase 14: Advanced Interop ✅
**Target:** Enhance Scheme <-> JS usability

| Feature | Description | Status |
|---------|-------------|--------|
| **Dot-Syntax** | Concise syntax for JS property access and method calls (e.g. `obj.method`). | ✅ |
| **JS Subclassing** | Mechanism to define Scheme classes that subclass native JS classes (`define-class`). | ✅ |
| **`this` Binding** | Correct handling and lexical binding of `this` in Scheme methods. | ✅ |
| **Iterable Lists** | Make `Cons` implement JS Iterable protocol. | ❌ |
| **Object Printing** | Proper reader syntax and circular support for JS objects. | ✅ |

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

## Phase 16: Developer Experience ✅
**Target:** Debugging and usability

| Feature | Description | Status |
|---------|-------------|--------|
| **Source Locations** | Track line/column numbers in AST (Lists/Vectors/Quotes). | ✅ |
| **Stack Traces** | Environment-agnostic `StackTracer` with TCO awareness. | ✅ |
| **Breakpoints** | Line and column-level precision via `BreakpointManager`. | ✅ |
| **Async Stepping** | Step-by-step execution with periodic yields. | ✅ |
| **Scope Inspection** | Full scope chain traversal and CDP value serialization. | ✅ |

**Deliverable:** Core debugger runtime implemented and verified across 1977 tests.

---

## Phase 18: Modular Analyzer Refactoring ✅
**Target:** Improve analyzer architecture for extensibility and maintainability.

> [!NOTE]
> The `analyzer.js` has been successfully refactored to use a modular handler registry. This decouples special form logic into themed modules and allows for isolated macro registry state.

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

---

### Extended Syntax: Bracket Access (Future)

Consider implementing `(expr)[key]` notation for computed property access, similar to JavaScript.

**Motivation:**
- Computed property access is common in JS interop.
- Current `.prop` syntax only supports static keys.
- Reader refactor for dotted notation makes implementation feasible (identifying `[` adjacent to expression).

**Considerations:**
- Conflict with Scheme implementations using `[]` as synonyms for `()`.
- Need to decide if `[]` should be reserved for this syntax or strictly brackets-as-parens.

**Decision:** Deferred pending user feedback on syntax preferences.

---

### Debugger: Restartable Conditions (Future)

Consider implementing Common Lisp-style "restartable conditions" for advanced error recovery.

**Motivation:**
- Classic Lisp REPLs allow navigating the stack when exceptions occur
- Users can provide alternate values and "restart" computation from specific points
- Would enable interactive debugging workflows like "use-value", "abort", "retry"

**Current Implementation:**
- Exception debugging pauses in the catch handler after exception propagates
- Sufficient for "inspect and continue" workflows
- Does not support modifying the exception or providing alternate return values

**Complex Approach (Required for Restarts):**
- Modify `RaiseNode.step()` to return `true` when pausing (signaling more work)
- Push a "continuation frame" that captures the exception context
- `resume()` can then continue, suppress, or restart with different values
- Would require creating a new frame type and restructuring exception propagation

**Benefits:**
1. Allow "suppressing" an exception (returning a different value instead of re-throwing)
2. Modify the exception value before continuing  
3. Choose from multiple restart strategies
4. Full stack navigation during exception debugging

**Decision:** Deferred. Current simple approach covers typical "inspect and continue" debugging. Revisit if user demand for advanced restart capabilities.

---

## Compiler Effort

**Target:** Address the structural performance limits of the interpreter by adding a
Scheme-to-JavaScript compiler tier.

Full analysis and staged plan: [docs/compiler_strategy.md](docs/compiler_strategy.md).
Measurements: [docs/performance_baseline.md](docs/performance_baseline.md).

The headline finding is that `fib(30)` takes 6,524 ms here against 10 ms in plain JavaScript and
~240 ms in Gambit's *interpreter*, and that CPU profiling attributes ~84% of runtime to evaluator
overhead against under 3% to the program's actual arithmetic. Being interpreted accounts for
roughly a factor of 25; the rest is representation, not execution model.

> [!IMPORTANT]
> The "Numeric Performance Optimization" section above targets a measured ~3x cost. Interpretive
> overhead is a measured ~200x cost. The numeric items remain worth doing but should not be
> mistaken for the performance work.

### Stage 0: Measurement & Instrumentation ✅

| Deliverable | Description | Status |
|---|---|---|
| **Standard benchmark suite** | 8 portable R7RS programs in `benchmarks/programs/`, four drawn from Thivierge & Feeley (SFP 2012) for comparability with published numbers. Four exercise `call/cc`; two require multi-shot semantics. | ✅ |
| **Cross-implementation harness** | `npm run benchmark:implementations` runs the same sources under Gambit `gsi` and Racket. All three agree on all eight results. | ✅ |
| **CPU profiler** | `npm run benchmark:profile <name>` reports self time by function and the evaluator/primitive split. | ✅ |
| **Step counting** | `npm run benchmark:steps` reports deterministic dispatch counts via `src/debug/instrumentation.js`, which wraps the interpreter rather than adding a hot-path branch. | ✅ |
| **R7RS conformance audit** | `npm run audit:r7rs` probes every identifier the standard requires. | ✅ |
| **Committed baseline** | `benchmarks/baseline_standard.json`. | ✅ |

### Stage 0: Pre-existing bugs fixed ✅

Three debugger behaviours were implemented but non-functional. They are fixed rather than
preserved, so that "does the debugger still work?" is a meaningful question during later stages.

| Bug | Fix |
|---|---|
| Every `source.filename` was the literal `'<unknown>'`, so file-scoped breakpoints could never match. | `parse()` accepts a `filename` option, threaded from the library loader and `load`. |
| `pauseOnException` read `registers.env` from the register *array*, always yielding `undefined`, so locals were unavailable at an exception breakpoint. | Indexes `registers[ENV]`. |
| **Enabling the debugger broke tail-call optimization.** Each procedure entry pushed a `DebugExitFrame`, so a tail loop accumulated one frame per iteration — measured at depth 806 for 800 iterations, against 4 with debugging off. | `recordDebugFrameEntry` detects tail position from the frame stack and reuses the existing exit frame. Tail loops now hold at constant depth; non-tail recursion still grows. |

### Stage 0: Conformance findings

7 required identifiers missing, 2 stubs that throw unconditionally, 2 libraries not importable.
Details in [docs/performance_baseline.md](docs/performance_baseline.md). The substantive cluster is
string mutability — `string-set!` and `string-fill!` throw, and `string-copy!` is absent — which is
a deliberate trade against JS interop and is scheduled for resolution in Stage 2b.

### Stage 1: Interpreter representation (partial) — **2.57x**

Measured **2.57x geometric mean** across the suite, and a **3.4–5.7x reduction in evaluator
dispatches**. The gap to Gambit's *interpreter* closed from ~26x to ~8x.
Results after every stage: [docs/performance_progress.md](docs/performance_progress.md).

| Item | Status | Effect |
|---|---|---|
| Native variadic comparison primitives | ✅ | `=`, `<`, `>`, `<=`, `>=` were Scheme procedures with rest parameters, so one integer comparison became four nested applications. Also **fixed rational comparison**, which had been falling through to JavaScript's `<` and `===` on `Rational` objects and therefore comparing them as strings and by identity. |
| Inlined evaluation of non-capturing subexpressions | ✅ | **The largest win.** A literal or a variable reference cannot capture a continuation, so there is no suspension point to preserve and it can be evaluated in place rather than suspended into a frame and bounced through the trampoline. Applied to the operator, the operands, and `if` tests. A call like `(< n 2)` now completes in a single dispatch where it previously cost three frames and six. |
| Application logic extracted to a module function | ✅ | `continueApplication`, reached from `ast_nodes.js` through a `frame_registry` **live binding** rather than a forwarding wrapper, which showed up at 4.3% of profile time on a path taken by every application. |
| Lazy JS-context capture | ✅ | `pushJsContext` copied the entire frame stack on every primitive application. It now records the stack by reference plus depth and copies only if something asks. |
| Precomputed operand arrays, deferred argument array, lazy `nameMap`, single-probe lookup | ✅ | Removes several allocations per call. |
| **Lexical addressing** | ❌ Deliberately not done | Profiling showed the entire cost of variable lookup — `Environment.lookup`, `extendManyFrom` and `VariableNode` dispatch combined — was only ~15% of runtime, so perfect elimination would be worth ~1.17x. That does not justify reworking `SyntacticEnv` (which has one binding per frame, against one runtime frame per lambda), the environment representation, `StateInspector` and the REPL's `:eval`. |
| **Global value cells** | ❌ Not done | Same reasoning; depends on the same rework. |
| Unify `run` and `runAsync` | ❌ Not done | They remain hand-maintained near-duplicates. |

> [!IMPORTANT]
> **The 10–30x Stage 1 estimate was wrong, and the profile explains why.** After these changes the
> remaining time is dominated by the frame machinery itself — `continueApplication`, the
> trampoline, and the two application node/frame dispatches — which together are ~55% of runtime
> and cannot be removed without changing the execution model. Primitive work rose from 2.8% of
> runtime at baseline to 8.5%, so the ratio of real work to overhead improved about 3x, but an
> AST-walking interpreter with a reified frame stack has a floor well above native code.
>
> This strengthens rather than weakens the case for the compiler: the remaining interpreter
> optimizations are worth small constant factors, while the measured headroom to Racket CS is still
> 300–600x on call-heavy programs. Stage 2 is where the rest is.

### Stage 2a: Calling-convention bake-off ✅ — **convention B**

Two throwaway compilers in [`experiments/stage2a/`](experiments/stage2a/), built from a shared front
end so the numbers compare conventions rather than compilers. Both pass all eight benchmarks,
including the two requiring multi-shot continuations. Run `node experiments/stage2a/summary.js`.

**Decision: convention B — native JavaScript stack, trampoline for tail calls, cooperative unwind
for `call/cc`** (Pettyjohn et al. with Marshall's distinguished-return-value variant).

| | A — explicit frame stack | B — native JS stack |
|---|---|---|
| Normal path | baseline | **up to 3.3x faster** |
| Capture-heavy | **1.2–1.5x faster** on 3 of 4 | 1.5x faster on `threads` |
| vs interpreter (geometric mean) | 13.0x | **17.5x** |
| Scheme frames visible to a debugger | **1** | **13 of 12 live** |
| Generated code size | **23 KB** | 95 KB (4.09x) |

The plan expected to trade performance for debuggability. There was no trade: B won both. The one
real cost is code size, because a procedure needs a second re-enterable copy to be resumed after a
capture — an effect analysis proving a procedure never captures would drop most of them, now a
Stage 2b task.

> [!NOTE]
> What was measured is the stack *shape* — one live Scheme frame is one live JavaScript frame.
> Relabelling those frames via a source map in DevTools is mechanical but was **not** verified end
> to end. That check belongs in Stage 2b, before `extension/` is deleted.

Two findings the plan had not anticipated, both recorded in the strategy document's revision log:
*both* conventions need re-enterable procedures (A at every non-tail call, B only where a capture
is possible), and both need assignment conversion, because a re-entered procedure restores locals
into a fresh binding that closures made earlier do not share.

### Remaining stages

| Stage | Description | Status |
|---|---|---|
| **Stage 1** | Interpreter representation, no compiler. Target was 10–30x; that estimate was wrong (see above). | **2.57x — closed** |
| **Stage 2a** | Calling-convention bake-off. | **Complete — convention B chosen** |
| **Stage 2b** | The compiler tier: IR, codegen, source maps, value representation, macro phase separation. | Not started |
| **Stage 3** | Optimization: direct calls, primitive inlining, arity specialization, unboxing, escape analysis. | Not started |
