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
Scheme frames visible on the JavaScript stack, which constraint 4 needs. In the browser, compiled
code is to be debugged in the browser's own DevTools through source maps, not through an extension.

The interpreter is a **permanent** tier, not a transitional one: it is the mode that works where
generating code is forbidden, the reference semantics for differential testing, the highest-fidelity
debugging tier, and the compiler's own bootstrap.

A Scheme compiler good enough to compile a Scheme compiler is the standing test of whether this
succeeded; see the next goal.

**Where it stands:** the tier works, the standard library runs through it, and so does a program's
own code, compiled as it runs, by default, in the CLI, the browser and both REPLs. Compiled code is
debugged by running it as the interpreted closures it replaced while a program is being debugged,
so every breakpoint fires in the CLI and the browser alike. In the browser's DevTools it shows as
Scheme in place: frames named for their procedures and placed in the Scheme source by source maps,
the page's own scripts included. Current state, ranked work and rationale:
[docs/compiler_plan.md](docs/compiler_plan.md) and
[docs/compiler_design.md](docs/compiler_design.md).

### An interpreter and compiler written in Scheme

**Goal:** as much of the system as can be is written in Scheme -- the compiler, and the interpreter's
reader, macro expander, library system, printer, numeric tower and primitive libraries, and the
debugger's logic -- over a JavaScript core kept to what needs JavaScript: the evaluator for now, the
value representations, code generation and the compiled-code runtime, and the parts of libraries that
need JavaScript features (host input and output, interop reflection, JavaScript classes, hash-table
storage, Unicode tables, `BigInt`). Scheme and JavaScript call each other freely, so the language of a
caller or callee decides nothing.

Why: it is the system using itself; a compiler is a good benchmark of itself, and wherever the
compiled Scheme is slower than the JavaScript it replaced, that is the next thing for the compiler to
optimize; a Scheme system should be able to host an effective, performant interpreter and compiler
written in Scheme; and it shows Scheme at its best. The system's own Scheme ships compiled, so it
costs a page nothing to load; a debugging mode will let it be stepped into and appear in stack traces
like a program's own code, for working on the interpreter and compiler themselves.

**Where it stands:** the compiler is Scheme -- its passes, the driver that decides what to compile,
and the tier that compiles a program's own code as it runs -- over a small JavaScript host library;
the interpreter is JavaScript. The evaluator's own loop moves last, once compiled Scheme is fast enough
for it. Ranked in [docs/compiler_plan.md](docs/compiler_plan.md).

### Close the known R7RS-small deviations

`equal?` does not terminate on circular structure, which R7RS §6.1 requires. The file procedures
(`call-with-input-file` and the rest) return an exact integer the procedure returned as inexact.
And `current-input-port`, `current-output-port` and `current-error-port` are procedures rather than
parameter objects, so `parameterize` of one has no effect: the program goes on reading and writing
the port it had.

### Numeric performance

**Deferred, deliberately.** The full numeric tower costs about 3x; interpretive overhead cost about
200x. These are real optimizations aimed at the smaller of the two problems, and picking them up
while the larger one is open is how this list came to rank a 3x problem above a 200x one in the
first place.

The list of boundary-conversion optimizations that stood here predates the compiler, and what the
compiler found since points elsewhere: the `bignum` class's cost appears to sit in tower dispatch,
and exact integer loops are bounded by V8's own `BigInt` arithmetic, neither of them conversion. Numeric work is now ranked in
[docs/compiler_plan.md](docs/compiler_plan.md) -- profiling bignums, and fixnums as JavaScript
numbers -- and the old list is in the history of this file.

### Smaller compiled programs

**Goal:** an optimization level that minimizes what a page loads: smaller generated code, library
procedures a program never reaches left out, and, for a program that needs no `eval`, REPL or
debugger, no interpreter at all. Every page carries every shipped library compiled, and one that
compiles its own code also fetches the compiler, about 2.2 MB, so size is what a page pays for speed.

The interpreter stays a permanent tier; this is a build that leaves it out where a program does not
need it. That needs compiled code to finish its own continuation captures and moves of its frames to
the heap, which today it hands to an interpreter beneath it.

**Decision:** planned, ranked low (2026-09-30). The work is in
[docs/compiler_plan.md](docs/compiler_plan.md).

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
- Until it is decided, the reader rejects `[` and `]` with a read error, which leaves either choice open: R7RS 2.3 reserves them. Of the corpus's 351 files only an R6RS test file uses them.

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
| **R7RS-small, end to end** | Every phase of the implementation checklist. **982 of 982** applicable Chibi conformance tests and **219 of 219** chapter tests pass, with the deviations above outstanding, both with the standard library interpreted and with it compiled as the browser installs it -- two of Chibi's only because its runner rescues a failure whose values agree in JavaScript. |
| **Hygienic macros** | `syntax-rules` via sets-of-scopes, verified against standard hygiene suites. |
| **The library system** | `define-library`, import filters, `include`, `include-ci`, `include-library-declarations`, `cond-expand`. |
| **The full numeric tower** | Exact integers on `BigInt`, rationals, complex numbers. JavaScript cannot tell `1` from `1.0`, so exactness does not survive a round trip through it; see [docs/Interoperability.md](docs/Interoperability.md). |
| **JavaScript interoperability** | Scheme procedures are callable JavaScript functions, which convert their arguments and results and finish their tail calls, deep recursion and continuations before returning, whichever tier runs them -- primitives excepted, which take Scheme values; the parts of that call, a call that converts nothing and the conversions both ways, are exported for JavaScript to use itself; numbers convert at the boundary, an integral number from JavaScript arriving exact however it arrives, and strings cross as their characters -- a newly made string may be changed in Scheme, and JavaScript always receives a JavaScript string; classes, promises and property access are reachable from Scheme. |
| **Scheme programs in a pipeline** | A program run from the CLI, `node repl.js prog.scm` or `-e`, has the process's standard input, output and error as its current ports: it reads what is piped in as it arrives, and what it writes is seen a line at a time, a prompt before the program waits for its answer, and all of it by the time the program ends; errors go to standard error, and a closed pipe ends it quietly, as `head` expects. `-e` writes its result as `write` does. The interactive REPL keeps standard input for itself. |
| **Lists and strings** | SRFI 1 and SRFI 152, as `(srfi 1)` and `(srfi 152)`: the list library, and the index-based string library that fits R7RS-small's own. The compiler is written with them too. |
| **Bitwise operations** | SRFI 151, as `(srfi 151)`, on exact integers of any size as two's-complement bit strings; the associative operations, the shift and the counts are `BigInt`'s own operators. The compiler's liveness analysis keeps its sets in it. |
| **Hash tables and comparators** | SRFI 125 and SRFI 128, as `(srfi 125)` and `(srfi 128)`. Tables on `eq?`, `eqv?`, `string=?` and `string-ci=?` sit directly on a JavaScript `Map`; any other equivalence works through its hash function. |
| **Async execution** | `runAsync` with configurable yields, preserving tail calls, `call/cc` and interop. |
| **A debugger, twice** | Breakpoints, stepping, stack and scope inspection — in the Node and browser REPLs. A Chrome extension with a standalone window, expression-level breakpoints and mixed JavaScript/Scheme stepping was built on the `debugger-take-3` branch; it is not on the compiler branch and is no longer a goal. |
| **A compiler tier** | Emits JavaScript for most of the standard library and every library the bundle ships, all compiled at build time, so a page starts in about 60 ms without running the compiler. **A program's own code is compiled as it runs, by default**, in the CLI (`--no-compile` to turn it off), a browser page (which fetches the compiler after it starts; `setUserCodeCompilation(false)`) and both REPLs: a procedure that loops when it is defined, any other on its second call, each still debuggable -- `fib(30)` from the CLI goes from 1.8 s to 64 ms. Against the other Scheme compiled to JavaScript, Gambit, on the same V8: level on calls and fixnums, faster on flonums (4x) and lists (1.7x), behind on bignums and continuations; 2.5x plain JavaScript on calls (details in `docs/r7rs_benchmark_results.md`). Per workload class against the interpreter, as the range over two runs: `flonum` 120–122x, `call` 83–86x, `fixnum` 57–58x, `vector` 37–38x, `list` 23x (and 2.8x more since top-level expressions are compiled), `continuation` 4.2–4.3x, `bignum` 1.2x, `string` 1.0x. Compiled recursion is not bounded by the JavaScript stack, alternating with interpreted code or not: past half of it, compiled frames move to the heap. A continuation may be captured beneath any number of alternations of compiled and interpreted code. |
| **Compiled Scheme in DevTools** | A stack trace or profile of compiled code is a Scheme stack: each frame named for its procedure, the code the tier generates listed as `scheme:///<file>/<procedure>`, and a source map placing each frame at its expression in the Scheme source -- a page's scripts included, an inline one's text carried in its map. `schemeEval(code, { filename })` names code evaluated from JavaScript. |
| **A measurement discipline** | 51 vendored canonical benchmarks classified by workload and never blended into one number; cross-implementation comparison against Gambit and Racket; a differential fuzzer running generated programs interpreted, compiled and tiered; 6,222 tests, including 41 whole programs run under both tiers. |
