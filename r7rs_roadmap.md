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

**Incomplete:**
- **Library system** — `cond-expand` not implemented; full R7RS library clauses incomplete

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

Defined missing standard macros in `src/core/scheme/control.scm`.

| Macro | Status | Notes |
|-------|--------|-------|
| `or` | ✅ | |
| `let*` | ✅ | |
| `case` | ✅ | Uses `memv` |
| `when` | ✅ | |
| `unless` | ✅ | |
| `do` | ✅ | Normalized expansion |
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

## Phase 9: Control Flow (Partial) ✅
**Target Library:** `(scheme base)`, `(scheme case-lambda)`

| Primitive | Status | Notes |
|-----------|--------|-------|
| `procedure?` | ✅ | |
| `map` | ✅ Scheme | In `core.scm` with type checking |
| `for-each` | ❌ Missing | |
| `case-lambda` | ❌ Missing | |

**Deliverable:**
1. Expand `control.js` / `core.scm`.
2. **[NEW]** Define `src/core/scheme/case_lambda.sld` for `(scheme case-lambda)`.

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

## Phase 12: Bytevectors (Optional)
**Target Library:** `(scheme base)` (basics)

R7RS §6.9 defines bytevectors.

---

## Phase 13: Final R7RS Libraries
**Target Library:** `(scheme repl)`, `(scheme load)`, `(scheme process-context)`, `(scheme time)`, `(scheme lazy)`, `(scheme eval)`

Implement the remaining R7RS standard libraries:

| Library | Status |
|---------|--------|
| `(scheme lazy)` | ❌ (`delay`, `force`) |
| `(scheme eval)` | ❌ |
| `(scheme load)` | ❌ |
| `(scheme process-context)` | ❌ |
| `(scheme repl)` | ❌ |
| `(scheme time)` | ❌ |

---


## Phase 14: Advanced Interop
**Target:** Enhance Scheme <-> JS usability

| Feature | Description |
|---------|-------------|
| **Dot-Syntax** | Concise syntax for JS method calls (e.g. `(.log console "Hello")`). |
| **Iterable Lists** | Make `Cons` implement JS Iterable protocol for easier use with `Array.from`, spread syntax, etc. |
| **JS Subclassing** | Mechanism to define Scheme records that subclass native JS classes. |

---

## Phase 15: Full Numeric Tower
**Target:** R7RS full numeric compliance

| Feature | Description |
|---------|-------------|
| **Exact Integers** | Arbitrary-precision integers (BigInt support). |
| **Rationals** | Fraction support (e.g. `1/3`). |
| **Complex Numbers** | Complex number support (e.g. `3+4i`). |

---

## Phase 16: Developer Experience
**Target:** Debugging and usability

| Feature | Description |
|---------|-------------|
| **Source Locations** | Track line/column numbers in AST for better error reporting. |
| **Stack Traces** | Readable Scheme stack traces (filtering internal JS frames). |

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
- `tests/unit/` — JavaScript module tests
- `tests/functional/` — Integration tests
- `tests/scheme/` — Scheme-based tests

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

