# Compiler plan

What is being worked on in the compiler effort, in what order, blocked on what. **This is the only
place that ranks compiler work.** `../ROADMAP.md` holds the high-level arc of user-visible features,
planned and finished; `compiler_findings.md` holds numbered findings; `../CHANGES.md` holds
walkthroughs. None of those rank work.

**The rule that keeps it true:** read this before starting a task, update it when finishing one.
A plan nobody reads is how the last one rotted.

**How to read the table.** The number is the *suggested* order and is renumbered freely as
priorities move; `Depends on` is the *binding* constraint, and names a task rather than a number for
that reason. Several items are genuinely independent, so a lower number does not imply a
prerequisite.

**Why the `Evidence` column exists:** a rank you cannot argue with is a rank nobody checks. Each
entry cites the finding that puts it where it is, so a proposal to reorder can be answered with a
measurement instead of an opinion. Links go one way — this document points at
`compiler_findings.md` and never the reverse, because that log is append-only and a back-link would
have to be edited every time priorities move.

**Completed items** keep their numbers and collect at the bottom, compressed to one line each, so
the live work stays at the top. A finished task's row goes both here and at the end of
`compiler_plan_completed.md`, which keeps every one; this section keeps the fifteen most recent, and
the oldest row is dropped from here when a sixteenth arrives. The bound is on purpose: this file is
read in full at the start of every task, and an unbounded list of ✅ is exactly how the last plan
buried its own next step at line 883 of 1,003.

**New compiler code is written in Scheme.** Not written in JavaScript and ported later — that
ordering never produced a port (R56). Where Scheme lacks a capability the compiler needs, build it as
a Scheme library over the minimum JavaScript. Unported JavaScript stays reachable: Scheme calls it
through interop, and it calls Scheme through `../src/compiler/lowering.js`.

---

## Live

`→` in progress · `⊘` blocked · blank not started

| # | | Task | Depends on | Why here | Evidence |
|---|---|---|---|---|---|
| 19 | | **Code generation, as Scheme passes over the IR** | — | Coverage is exhausted: 839 of 1089 definitions compile, and every gain from R45 to R48 was coverage while R49 got none, so further speed has to come from the generated code — direct calls to known procedures, arity specialization, unboxed fixnum paths, escape analysis. **Written as Scheme analyses** that run on the IR `ir.scm` produces — it is already Scheme data there, before `marshal.js` converts it — and leave annotations for the JavaScript emitter, whose changes to read them should be small. So this does not wait for the emitter to move. A change of regime; design it as one before writing code. **The first measured targets**, from what loops left behind: every inlined primitive calls its global accessor and compares the binding on every use (`G() === P`) -- six times per element of `assq` -- because Scheme permits redefining `car`; and a call to a non-inlined procedure goes through the generic path (accessor, `SCHEME_RAW_CALL` lookup, `TailCall` and `UNWIND` checks) even for `list?`. Native `assq` and `memq` still make lowering 1.38x faster. | R45–R49, R56, R60 |
| 20 | | **Debug an optimized procedure by not optimizing it** | — | The tier declines procedures for a dozen reasons already; "a breakpoint is set inside it" is one more, with recompilation when breakpoints change. This is the `-O0 this translation unit` mechanism every real toolchain has beside its debug info, and it is the only thing that gives **full** fidelity — the interpreter knows every binding, and no optimization can have removed one. More valuable now that the compiler is being written in Scheme: its prebuilt form is compiled code, so we are its first users. Two concrete pieces: the interpreted closure is discarded when a procedure compiles (`env.define` overwrites it and `markProcedure` keeps no handle), so it must be retained; and `BreakpointManager` has no location-to-procedure mapping — task 13 built half of that, since compiled procedures now carry their span and `SchemeDebugRuntime.compiledProcedureAt` answers "is this location compiled?". The Chrome extension debugger lives on the `debugger-take-3` branch and will need the same check when the two merge. | R54 |
| 21 | ⊘ | **Enable the tier for user code** | Debugging by not optimizing | Nothing outside `src/compiler/` calls `compileProgram` or `tryCompileDefinition`. The standard library is compiled; **user code never is** — not in the browser, the CLI or either REPL. Every speedup measured (fib 21.97x, earley 23.85x) reaches benchmarks and nobody else. Held behind debugging-by-not-optimizing because enabling it is what turns the debugger gap from unreachable into everybody's problem. Also needs a call on the two shapes that are *refused* rather than answered: a capture crossing more than one tier boundary, and one beneath a redefined inlined primitive. | R43, R49, R54 |
| 22 | | **Profile bignums** | — | Worst class at 1.22x, and **not** a coverage problem: `pi` compiles 9 of 9 and still measures 1.00x. Both implementations do arbitrary precision, so 53x against Gambit is anomalous and probably sits in tower dispatch. The one part of R29 that survived, twice. | R48 |
| 23 | | **Compliance suites into `npm test`** | — | 219 + 982 conformance tests still outside the default run; `tests/core/scheme/compliance/` has its own runners and nothing references them. Cheap — `tests/test_manifest.js` grew a `programTests` section that is the natural home. Same failure mode just closed for the benchmark programs. | R53 |
| 24 | | **Split `compiled_compiler.js` out of the browser bundle** | — | 383 KB of generated code after task 18, needed only by a page that compiles at run time; the loop analysis alone added ~100 KB, because `and` and `cond` chains compile verbosely. A static import in `index.js` is why rollup inlines it. Weight rather than urgency. | R52, R55 |
| 25 | | **Move `lift.js` to Scheme** | — | An IR analysis, so it belongs beside `ir.scm`. Do it as part of whichever task next has to change it — code generation probably will, since direct calls and lifting interact — rather than as a port for its own sake. | R56 |
| 26 | | **Move `safety.js` to Scheme** | — | Consumes exactly what `ir.scm` produces and would share its scope and traversal helpers. It needs environment lookup and closure introspection that Scheme cannot express today, which used to count as a blocker; under the policy it is the minimal JavaScript to expose, not a reason to wait. Like `lift.js`, move it when it is next changed. | R56 |
| 27 | ⊘ | **Rewrite the emitter in Scheme** — with `resume`, `codegen`, `inline` and `liveness` | Code generation | A rewrite, not a port. The Scheme emitter should produce statements as **data** rather than strings; liveness then reads definitions and uses straight off them, and the text scanning that is its weakest part — a local's name inside a string literal counts as a read — disappears. `inline.js` is only used by the emitter, so it moves with it rather than alone. Waits for code generation so the design being rewritten has stopped moving. Prove it with a differential against the JavaScript emitter on the whole corpus, the same way `ir` was proved. | R52, R55, R56 |
| 28 | ⊘ | **Source maps and debug points** | The emitter rewrite | The answer for constraint 4, and the reason calling convention B was chosen over the faster alternative: one live Scheme frame is one JavaScript frame, so DevTools can show a Scheme stack. Until this exists we have paid for that choice and taken none of it, and the Chrome extension cannot shrink. Debug points are emitter output, so they wait for the emitter that will survive — building them in the JavaScript one means building them twice. Every optimization in code generation also degrades the mapping by design. It does **not** replace debugging by not optimizing: source maps map locations, not bindings an optimizer removed. | R54, R56 |
| 29 | ⊘ | **The analyzer** | tier fast enough on its own shape | 2,014 lines, 579 of sets-of-scopes hygiene. **The real self-hosting seam**: until it is ported the AST crosses the boundary on every compile no matter what else is in Scheme. Its own project. Gate on `npm run benchmark:self-host`. | R52 |
| 30 | | **Finish the benchmark suite** | — | Three steps left from the validity review: replace `threads` with the real `threads10` (which also buys vector coverage), add the missing axes (interop, debugger-on, startup), and report a per-stage coverage metric so overfitting stays visible. The review that found the overfitting also listed the fix, and half of it is still undone. | R23 |
| 31 | ⊘ | **Flonum fast paths** | The emitter rewrite | Inline expansions still guard `bigint` only; flonum is 13.1x behind Gambit. They are emitter code, so under the policy they are written in the Scheme emitter rather than added to `inline.js` first. | R56 |
| 32 | ⊘ | **Fixnums as JS numbers** | Profiling bignums | R29 ranked this first on the premise that BigInt dominates and code generation cannot help. `fixnum` is now the best tier class, so that premise is false. May still pay — needs a profile, not R29's reasoning. | R29, R39 |
| 33 | | **Hygienic procedural macros, and phase separation** | — | `define-macro` is procedural and non-hygienic, and `core_forms.js:625` still constructs a second `Interpreter` per macro definition — compile time and run time conflated. `syntax-rules` is already hygienic via sets-of-scopes, so the hard part is done. Add explicit renaming (`er-macro-transformer`), keep `define-macro` as a marked legacy extension on top of it, and maintain a phase-1 environment instead of a second interpreter. Blocks nothing today; the expander and compiler must agree on phases eventually. | — |
| 34 | | **Mutable strings, immutable-until-mutated** | — | A real R7RS conformance gap. But `string` is the one class where we beat both references, so it can only cost performance. Use the R31 design and measure. | R31 |
| 35 | | **Drop `source` from runtime `Cons`** | — | Unchanged, unmeasured, low priority. | — |

## Completed

The fifteen most recent. Every completed task, these included, is in [compiler_plan_completed.md](compiler_plan_completed.md) under the same number, and anything older is there only. Detail in `../CHANGES.md`; what each one *falsified* in `compiler_findings.md`.

| # | | Task | Outcome | Evidence |
|---|---|---|---|---|
| 4 | ✅ | The symbolic-code weakness | It was an interpreted standard library, not weak code generation. Every figure arguing against self-hosting was measuring that. | R45 |
| 5 | ✅ | AOT stdlib + the `apply` unblock | A one-line change unblocked most of the corpus; coverage 623 → 807 of 1089. | R46 |
| 6 | ✅ | `let` binding chains | The code-size blow-up was `let`, not closures. | R47 |
| 7 | ✅ | `values` / `call-with-values` | Neither was a control operation; found a third tier-boundary conversion bug. | R48 |
| 8 | ✅ | Compiled `call/cc`; the boxing bug | Assigned locals were copied into spilled frames rather than shared. Boxing fixed it at 2–7%. | R49 |
| 9 | ✅ | Build-time AOT for the standard library | CSP-safe: nothing calls `new Function` at run time. The time saving was small; that was never the point. | R50 |
| 10 | ✅ | Letrec-aware lambda lifting | Generated code linear in nesting, not exponential. A size change, not a speed change. | R51 |
| 11 | ✅ | **Promote `ir.scm`; delete `ir.js`** | The lowering pass is Scheme. The bootstrap terminates in the interpreter; `npm run prebuild` runs the chain in 0.62 s, reproducibly. ~18x slower than the JavaScript it replaced. | R52 |
| 12 | ✅ | Whole-program correctness tests | 41 programs × both tiers = 82 assertions, 8.2 s, inside `npm test` (2,426 total). | R53 |
| 13 | ✅ | Make the silent breakpoint failure loud | `:break` and `:breakpoints` say when a breakpoint is inside compiled code and will not fire. Needed a prerequisite fix first: `(define (f x) ...)` produced closures with **no source span at all**, so the debugger could not place most procedures — `:bt` said "unknown location" for every one. Also fixed `:breakpoints` listing every breakpoint as disabled. | R54 |
| 14 | ✅ | Liveness for frame spills | Each suspension point saves only what is live where it resumes. Corpus 12.75 → 5.93 MB generated, frame literals 7.21 → 0.40 MB, `make-relative-nuc` 3.18 → 0.27 MB, `dist/scheme.js` 1.84 → 1.53 MB. Continuation class 1.09x; nothing else moved beyond noise. | R55 |
| 15 | ✅ | SRFI-125 hash tables, with SRFI 128 comparators | Both SRFIs complete. Tables on `eq?`, `eqv?`, `string=?` and `string-ci=?` live in a JavaScript `Map` with keys normalised to match — one primitive call per lookup, no Scheme predicate; every other table buckets by hash in Scheme, so user predicates never run under JavaScript. Three blockers fixed on the way: libraries imported the *interpreted* standard library, because imports copy values and the prebuilt install replaced only global bindings (R57); `define-record-type` rejected field names that are not JavaScript identifiers; `case-lambda` broke on five or more fixed parameters. 2,798 tests in Node, 2,695 in the browser. | R56, R57 |
| 16 | ✅ | Measure the tier on hash tables and records | Libraries the bundle ships are now compiled as they load, through a loader hook. Per operation in a compiled loop: an `eq?` lookup ~1,600 ns with the library interpreted, **~115 ns** compiled, flat from 4 to 256 keys; `equal?` 1,100 ns; `update!/default` 315 ns; a record read 33 ns against `car`'s 15. The finding that mattered was elsewhere: an empty compiled loop costs ~95 ns an iteration. `npm run benchmark:hash-tables`. | R59 |
| 17 | ✅ | Hash tables in `ir.scm` -- **not done, on the evidence** | The premise was false: the lists average 1.85 entries (`assq`) and 6.8 (`memq`). 39% of lowering is the compiled `assq` and `memq` call overhead, not scanning -- native versions take the corpus from 66.5 to 40.4 ms a pass. No site switched; the `ir.scm` header gives the real reason, and the work moved to compiling loops. | R59 |
| 18 | ✅ | Tail calls to known loops as JavaScript loops | A tail call to the procedure itself reassigns its parameters and jumps; a `letrec` loop only ever entered once in tail position is emitted inside the procedure that enters it (contification), so entering allocates nothing. The analysis is Scheme, in `ir.scm`; the emitter reads two IR flags. Compiled tier: `fixnum` 1.41x, `vector` 1.25x, `call` 1.24x, `list` 1.21x, `continuation` 1.10x, `flonum` 1.04x; no class regressed. The compiler's own lowering only ~15% faster -- the rest of `assq`'s cost is guards, not loops. | R60 |

## Decided, so not open

- **Calling convention B** — native JS stack for non-tail calls, trampoline for tail calls,
  cooperative unwind for capture. Settled by the Stage 2a bake-off; see
  [compiler_design.md](compiler_design.md).
- **The interpreter is a permanent tier**, not a transitional one: the CSP-safe execution mode, the
  differential oracle, the maximum-fidelity debug tier, and the compiler's own bootstrap.
- **The compiler moves to Scheme**, above `../src/compiler/runtime.js`. That file stays JavaScript
  because it needs native JavaScript features — a `Map` behind hash tables — that neither generated
  code nor Scheme libraries can express, not because generated JavaScript calls it.
- **New compiler code starts in Scheme.** Decided 2026-09-23, after a third increment in a row added
  JavaScript to the compiler under the "don't port a moving target" argument — liveness among them,
  written after the move to Scheme had been decided. Capabilities Scheme lacks are built as Scheme
  libraries over minimal JavaScript, not worked around by writing the compiler code in JavaScript.
  Regular expressions are deliberately **not** on that list: the compiler only needed text scanning
  because the emitter produces strings, and a Scheme emitter producing data removes the need.
