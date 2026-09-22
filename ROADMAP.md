# Roadmap

Where the project is going, and the constraints it has to get there under.

**What this is not.** It does not track individual tasks — compiler work is ranked in
[docs/compiler_plan.md](docs/compiler_plan.md), what was built is in [CHANGES.md](CHANGES.md), and
what we believed that turned out to be false is in
[docs/compiler_findings.md](docs/compiler_findings.md). The phase-by-phase R7RS-small
implementation checklist, now complete, is archived at
[docs/archive/r7rs_compliance_phases.md](docs/archive/r7rs_compliance_phases.md).

---

## The constraints

These are the durable part of this document. Everything below is a plan; these are what any plan
has to satisfy, and they were set before the work began.

| | |
|---|---|
| **1. High JavaScript interoperability** | Scheme values and procedures are usable from JavaScript and vice versa, without a marshalling layer in between. |
| **2. Browser and CLI** | The same implementation runs in a web page and at a terminal. |
| **3. A REPL in both** | Interactive evaluation is a first-class mode, not an afterthought. |
| **4. A full-featured debugger in both** | Breakpoints, stepping, stack and scope inspection — in the browser and at the terminal. |
| **5. Full multi-shot `call/cc`** | R7RS continuations, re-invocable any number of times. This rules out several otherwise attractive implementation strategies. |
| **6. Fully compliant R7RS-small** | Deviations are bugs to be closed, not trade-offs to be kept. |

Constraint 4 is the one under pressure. It is stated here because being stated once in a design
document and never again is how a compiler got fourteen increments deep before anyone noticed
that compiled code cannot be debugged.

---

## Planned

### Compile Scheme to JavaScript, for speed

**Goal:** Scheme programs fast enough to be worth writing real software in. The compiler is the
approach, not the goal — worth separating, because the approach may change and the goal will not.

The interpreter is roughly 650x slower than plain JavaScript on `fib(30)`, and profiling put ~95%
of that in interpretive overhead rather than in anything the program asked for. A compiler removes
precisely that 95%. The design emits JavaScript source rather than bytecode or WebAssembly, uses
the native JavaScript stack for calls with a trampoline for tail calls, and implements continuation
capture by cooperative unwinding — the one strategy compatible with constraint 5 that also leaves
Scheme frames visible on the JavaScript stack, which constraint 4 needs.

The interpreter is a **permanent** tier, not a transitional one: it is the mode that works where
generating code is forbidden, the reference semantics for differential testing, the highest-fidelity
debugging tier, and the compiler's own bootstrap.

Part of the compiler is written in Scheme, and the intent is that most of it will be. A Scheme
compiler good enough to compile a Scheme compiler is the standing test of whether this succeeded.

**Where it stands:** the tier works and the standard library runs through it; user code does not yet
reach it, and compiled code cannot yet be debugged. Current state, ranked work and rationale:
[docs/compiler_plan.md](docs/compiler_plan.md) and
[docs/compiler_design.md](docs/compiler_design.md).

### Close the two known R7RS-small deviations

`string-set!` and `string-fill!` throw, because Scheme strings are JavaScript strings and those are
immutable — a deliberate trade of compliance for interop that constraint 6 says should not stand.
And `equal?` does not terminate on circular structure, which R7RS §6.1 requires.

### Numeric performance

**Deferred, deliberately.** The full numeric tower costs about 3x; interpretive overhead cost about
200x. These are real optimizations aimed at the smaller of the two problems, and picking them up
while the larger one is open is how this list came to rank a 3x problem above a 200x one in the
first place.

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

---

## Delivered

Detail in [CHANGES.md](CHANGES.md); the R7RS-small implementation checklist in
[docs/archive/r7rs_compliance_phases.md](docs/archive/r7rs_compliance_phases.md).

| | |
|---|---|
| **R7RS-small, end to end** | Every phase of the implementation checklist. **982 of 982** applicable Chibi conformance tests and **219 of 219** chapter tests pass, with the two deviations above outstanding. |
| **Hygienic macros** | `syntax-rules` via sets-of-scopes, verified against standard hygiene suites. |
| **The library system** | `define-library`, import filters, `include`, `include-ci`, `include-library-declarations`, `cond-expand`. |
| **The full numeric tower** | Exact integers on `BigInt`, rationals, complex numbers, with exactness preserved across the JavaScript boundary. |
| **JavaScript interoperability** | Scheme closures are callable JavaScript functions; conversion is exactness-safe in both directions; classes, promises and property access are reachable from Scheme. |
| **Async execution** | `runAsync` with configurable yields, preserving tail calls, `call/cc` and interop. |
| **A debugger, twice** | Breakpoints, stepping, stack and scope inspection — in the Node and browser REPLs, and as a Chrome extension with a standalone window, expression-level breakpoints and mixed JavaScript/Scheme stepping. |
| **A compiler tier** | Emits JavaScript for most of the standard library, which is compiled at build time. Per workload class against the interpreter: `call` 21.8x, `fixnum` 16.2x, `vector` 15.2x, `flonum` 10.2x, `list` 10.1x, `continuation` 3.2x, `string` and `bignum` 1.2x. |
| **A measurement discipline** | 51 vendored canonical benchmarks classified by workload and never blended into one number; cross-implementation comparison against Gambit and Racket; 2,426 tests, including 41 whole programs run under both tiers. |
