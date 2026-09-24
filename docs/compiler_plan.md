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
| 22 | | **Code generation, as Scheme passes over the IR** | — | Coverage is exhausted -- 839 of 1089 definitions compile, and every gain from R45 to R48 was coverage while R49 got none -- so further speed has to come from the generated code: direct calls to known procedures, arity specialization, unboxed fixnum paths, escape analysis. The first measured target, the per-use primitive guard, turned out to need a runtime representation rather than an analysis and is done (19, R61). **Measure a ceiling before designing anything for the next ones**, as R61 did: the global accessor on every call, which is a hash lookup, and the generic call path (`SCHEME_RAW_CALL` lookup, `TailCall` and `UNWIND` checks) even for a known procedure. Written in the Scheme emitter (`emit.scm`). The compiler itself is now a measured customer: its code generation spends most of its time in `case` dispatch, which compiles to a `memv` call per clause, and in the global accessor on every call. The lowering tests in `loop_compilation_tests.js` inspect the IR from JavaScript and belong in `tests/compiler/` as Scheme. | R45–R49, R56, R60, R61 |
| 23 | | **Debug an optimized procedure by not optimizing it** | — | The tier declines procedures for a dozen reasons already; "a breakpoint is set inside it" is one more, with recompilation when breakpoints change. This is the `-O0 this translation unit` mechanism every real toolchain has beside its debug info, and it is the only thing that gives **full** fidelity — the interpreter knows every binding, and no optimization can have removed one. More valuable now that the compiler is being written in Scheme: its prebuilt form is compiled code, so we are its first users. Two concrete pieces: the interpreted closure is discarded when a procedure compiles (`env.define` overwrites it and `markProcedure` keeps no handle), so it must be retained; and `BreakpointManager` has no location-to-procedure mapping — task 13 built half of that, since compiled procedures now carry their span and `SchemeDebugRuntime.compiledProcedureAt` answers "is this location compiled?". The Chrome extension debugger lives on the `debugger-take-3` branch and will need the same check when the two merge. | R54 |
| 24 | ⊘ | **Enable the tier for user code** | Debugging by not optimizing | Nothing outside `src/compiler/` calls `compileProgram` or `tryCompileDefinition`. The standard library is compiled; **user code never is** — not in the browser, the CLI or either REPL. Every speedup measured (fib 21.97x, earley 23.85x) reaches benchmarks and nobody else. Held behind debugging-by-not-optimizing because enabling it is what turns the debugger gap from unreachable into everybody's problem. Also needs a call on the two shapes that are *refused* rather than answered: a capture crossing more than one tier boundary, and one beneath a redefined inlined primitive. In the browser the compiler is no longer in the bundle (21): `loadCompiler` fetches it, asynchronously, so a page's code runs interpreted until it arrives and is compiled after. | R43, R49, R54 |
| 25 | | **Profile bignums** | — | Worst class at 1.22x, and **not** a coverage problem: `pi` compiles 9 of 9 and still measures 1.00x. Both implementations do arbitrary precision, so 53x against Gambit is anomalous and probably sits in tower dispatch. The one part of R29 that survived, twice. | R48 |
| 26 | | **Compliance suites into `npm test`** | — | 219 + 982 conformance tests still outside the default run; `tests/core/scheme/compliance/` has its own runners and nothing references them. Cheap — `tests/test_manifest.js` grew a `programTests` section that is the natural home. Same failure mode just closed for the benchmark programs. | R53 |
| 27 | | **Move `safety.js` to Scheme** | — | Consumes exactly what `ir.scm` produces and would share its scope and traversal helpers. It needs environment lookup and closure introspection that Scheme cannot express today, which used to count as a blocker; under the policy it is the minimal JavaScript to expose, not a reason to wait. Move it when it is next changed rather than as a port for its own sake. | R56 |
| 28 | | **Look at whether the compile policy in `index.js` belongs in Scheme** | — | `index.js` mixes two things. The entry points and `new Function` are JavaScript by nature. The policy is not: which definitions to compile, and each reason one is declined -- a control global, a capture, a lowering failure, a source over the size limit, a procedure `safety.js` holds back. That policy is decided from what `ir.scm` returns, and moving it would put it beside the lowering and `safety.js` (27) that feed it. An assessment first, not a port: say what would move, what the boundary would look like, and whether it pays, then decide. Best looked at together with 27, since the policy consumes `safety.js`'s answers. | R56 |
| 29 | | **Source maps and debug points** | — | The answer for constraint 4, and the reason calling convention B was chosen over the faster alternative: one live Scheme frame is one JavaScript frame, so DevTools can show a Scheme stack. Until this exists we have paid for that choice and taken none of it, and the Chrome extension cannot shrink. Debug points are emitter output, written in `emit.scm`; its statements are already data, which is where a source position can ride along. Every optimization in code generation also degrades the mapping by design. It does **not** replace debugging by not optimizing: source maps map locations, not bindings an optimizer removed. | R54, R56 |
| 30 | | **Smaller generated code** | — | Since 21 every page carries every shipped library compiled, so the generated code's size is now what a page pays for: SRFI 1, 125, 128 and 152 are 1.1 MB of `dist/scheme.js`'s 2.67 MB, about 6 KB a procedure. Every procedure is emitted twice, fast and resumable, and a procedure none of whose callees can capture -- one that calls nothing able to call back into Scheme, say -- can never be suspended, so its resumable form is dead weight. **Measure first** how much of each table that is. The alternative, loading a library's table only when the library is imported, needs an asynchronous import, which the interpreter's `import` is not. Written in the Scheme emitter. | R65 |
| 31 | ⊘ | **The analyzer** | tier fast enough on its own shape | 2,014 lines, 579 of sets-of-scopes hygiene. **The real self-hosting seam**: until it is ported the AST crosses the boundary on every compile no matter what else is in Scheme. Its own project. Gate on `npm run benchmark:self-host`. | R52 |
| 32 | | **Finish the benchmark suite** | — | Three steps left from the validity review: replace `threads` with the real `threads10` (which also buys vector coverage), add the missing axes (interop, debugger-on, startup), and report a per-stage coverage metric so overfitting stays visible. The review that found the overfitting also listed the fix, and half of it is still undone. | R23 |
| 33 | | **Flonum fast paths** | — | Inline expansions still guard `bigint` only; flonum is 13.1x behind Gambit. They are emitter code: `inline.scm`. | R56 |
| 34 | ⊘ | **Fixnums as JS numbers** | Profiling bignums | R29 ranked this first on the premise that BigInt dominates and code generation cannot help. `fixnum` is now the best tier class, so that premise is false. May still pay — needs a profile, not R29's reasoning. | R29, R39 |
| 35 | | **Hygienic procedural macros, and phase separation** | — | `define-macro` is procedural and non-hygienic, and `core_forms.js:625` still constructs a second `Interpreter` per macro definition — compile time and run time conflated. `syntax-rules` is already hygienic via sets-of-scopes, so the hard part is done. Add explicit renaming (`er-macro-transformer`), keep `define-macro` as a marked legacy extension on top of it, and maintain a phase-1 environment instead of a second interpreter. Blocks nothing today; the expander and compiler must agree on phases eventually. | — |
| 36 | | **Mutable strings, immutable-until-mutated** | — | A real R7RS conformance gap. But `string` is the one class where we beat both references, so it can only cost performance. Use the R31 design and measure. | R31 |
| 37 | | **Drop `source` from runtime `Cons`** | — | Unchanged, unmeasured, low priority. | — |

## Completed

The fifteen most recent. Every completed task, these included, is in [compiler_plan_completed.md](compiler_plan_completed.md) under the same number, and anything older is there only. Detail in `../CHANGES.md`; what each one *falsified* in `compiler_findings.md`.

| # | | Task | Outcome | Evidence |
|---|---|---|---|---|
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
| 19 | ✅ | Primitive guards that survive calls | Not an analysis, on the evidence: a guard cached inside a procedure lasts only until its next call, and in recursive code that is the next expression. The interpreter now keeps one cell per primitive's name, cleared the first time the name is rebound anywhere, and compiled code reads it -- `W.intact \|\| G() === P`. Compiled tier `call` 2.28x, `fixnum` 2.24x, `list` 1.73x, `vector` 1.32x, `continuation` 1.19x; nothing regressed in either tier; lowering 98.9 → 68.1 ms. Fixed two bugs on the way: the guard compared against the compile-time binding, so a redefinition made before compiling was ignored (R62); and the resumable form lost a value computed by a call inside a value-position `if` branch or an operator (R63). | R61–R63 |
| 20 | ✅ | Rewrite the emitter in Scheme | Done ahead of the rest of code generation, by decision. `emit.scm`, `lift.scm`, `liveness.scm` and `inline.scm` replace `emitter.js`, `resume.js`, `liveness.js`, `lift.js` and `inline.js`: one emitter with a mode for the two forms, statements as data, liveness read off the data. Proved by a differential against the JavaScript emitter on every procedure the test suite, the benchmark programs, the standard library and the compiler compile -- over 10,000 -- byte-identical except three frames that save less (R64). Written with SRFI 1 and SRFI 152, implemented in full as libraries for users too; the compiler's tests of its own Scheme are Scheme (`tests/compiler/`). Code generation is 7.5x slower than the JavaScript was (1.2 ms a procedure); `dist/scheme.js` grew from 1.81 to 2.97 MB. | R63, R64 |
| 21 | ✅ | Make the compiler a Scheme library, and split it out of the browser bundle | The compiler is `(scheme-js compiler)`, loaded by name into a registry of its own, so it shares no library with the program it compiles and its file list lives in its `.sld`. Needed first: **every page was running the compiler at start-up** (R65), so every library the bundle ships now has a prebuilt table, installed as it loads -- start-up 193 → 61 ms, importing SRFI 125 300 → 25 ms, no compiler loaded. The compiler is `dist/scheme_compiler.js`, fetched by `loadCompiler`. `dist/scheme.js` 2.97 → 2.67 MB (424 → 340 KB gzipped): less than the compiler weighed, because SRFI 1, 125, 128 and 152's compiled code moved in. `npm run prebuild` is 1.8 s from a checked-in build, 15 s from nothing. | R65 |

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
