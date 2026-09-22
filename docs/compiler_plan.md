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
the live work stays at the top. When that section passes ~15 rows the oldest move to
`../ROADMAP.md`. The growth is bounded on purpose: an unbounded list of ✅ is exactly how the last
plan buried its own next step at line 883 of 1,003.

---

## Live

`→` in progress · `⊘` blocked · blank not started

| # | | Task | Depends on | Why here | Evidence |
|---|---|---|---|---|---|
| 13 | → | **Decide the debugger/compiler contract** | — | A breakpoint inside a compiled procedure **silently never fires**. The only debug hook is `interpreter.js:455`, inside the step loop; compiled procedures never enter it, and `src/debug/` has no reference to compiled procedures at all. Harmless today because only the standard library is compiled. Three options: **(a)** decline breakpoints out loud, so the limitation is known rather than silent; **(b)** decline to *compile* any procedure with a breakpoint in it, recompiling when breakpoints change — the tier already declines for a dozen reasons and this composes without source maps; **(c)** debug points and source maps, the real answer. (b) looks like the sweet spot and has not been considered before. | R54 |
| 14 | ⊘ | **Enable the tier for user code** | debugger contract | Nothing outside `src/compiler/` calls `compileProgram` or `tryCompileDefinition`. The standard library is compiled; **user code never is** — not in the browser, the CLI or either REPL. Every speedup measured (fib 21.97x, earley 23.85x) reaches benchmarks and nobody else. Also needs a call on the two shapes that are *refused* rather than answered: a capture crossing more than one tier boundary, and one beneath a redefined inlined primitive. | R43, R49 |
| 15 | | **Liveness for frame spills** | — | Largest remaining source of generated code, and not what lambda lifting addressed. `nucleic.scm:make-relative-nuc` is 3.25 MB, **94% of it `reify` frame literals** — 550 call sites spilling ~476 names each, because a suspended frame saves every declared variable. Quadratic in procedure size. Also shrinks `compiled_compiler.js`, so it pays twice. | R51 |
| 16 | | **Code generation, for the first time** | — | Coverage is exhausted: 839 of 1089 definitions compile, and the only real declines left are 7 reaching `with-exception-handler` and ~12 capture-related ones held back deliberately. Every gain from R45 to R48 was coverage; R49 got none. Direct calls to known procedures, arity specialization, unboxed fixnum paths, escape analysis. A change of regime — plan it as one. **Note:** promoting `ir.scm` moved the IR into Scheme ahead of this, and parts of this work touch the IR, so that slice now iterates at ~16x rather than in JavaScript. That was the unstated cost of reordering. | R45–R49 |
| 17 | | **Profile bignums** | — | Worst class at 1.22x, and **not** a coverage problem: `pi` compiles 9 of 9 and still measures 1.00x. Both implementations do arbitrary precision, so 53x against Gambit is anomalous and probably sits in tower dispatch. The one part of R29 that survived, twice. | R48 |
| 18 | | **Compliance suites into `npm test`** | — | 219 + 982 conformance tests still outside the default run; `tests/core/scheme/compliance/` has its own runners and nothing references them. Cheap — `tests/test_manifest.js` grew a `programTests` section that is the natural home. Same failure mode just closed for the benchmark programs. | R53 |
| 19 | | **Split `compiled_compiler.js` out of the browser bundle** | — | 535 KB of `dist/scheme.js`, needed only by a page that compiles at run time. A static import in `index.js` is why rollup inlines it. ~10 KB gzipped, so weight rather than urgency. | R52 |
| 20 | | **Port `inline.js` to Scheme** | — | 73 lines, a table. The cheapest next step of the port. | R52 |
| 21 | ⊘ | **Port `emitter` + `resume` + `codegen`** | code generation | Least-settled code in the compiler. Do not port a moving target — the codegen work rewrites all three. Use a source-text differential, the same trick that proved `ir`. | R52 |
| 22 | ⊘ | **Port `safety.js`** | Scheme primitives for `env.findEnv` and closure introspection | Consumes exactly what `ir.scm` produces and would share its scope and traversal helpers. Blocked on introspection it cannot express today. | — |
| 23 | ⊘ | **The analyzer** | tier fast enough on its own shape | 2,014 lines, 579 of sets-of-scopes hygiene. **The real self-hosting seam**: until it is ported the AST crosses the boundary on every compile no matter what else is in Scheme. Its own project. Gate on `npm run benchmark:self-host`. | R52 |
| 24 | ⊘ | **Source maps, and debug points** | debugger contract | Serves constraint 4 (debuggers in both environments), which optimization has crowded out indefinitely. Option (c) of task 13. | R54 |
| 25 | | **Finish the benchmark suite** | — | Three steps left from the validity review: replace `threads` with the real `threads10` (which also buys vector coverage), add the missing axes (interop, debugger-on, startup), and report a per-stage coverage metric so overfitting stays visible. The review that found the overfitting also listed the fix, and half of it is still undone. | R23 |
| 26 | | **Flonum fast paths in `inline.js`** | — | Expansions still guard `bigint` only; flonum is 13.1x behind Gambit. Incremental, no longer an unblock. | — |
| 27 | ⊘ | **Fixnums as JS numbers** | profile bignums | R29 ranked this first on the premise that BigInt dominates and code generation cannot help. `fixnum` is now the best tier class, so that premise is false. May still pay — needs a profile, not R29's reasoning. | R29, R39 |
| 28 | | **Hygienic procedural macros, and phase separation** | — | `define-macro` is procedural and non-hygienic, and `core_forms.js:625` still constructs a second `Interpreter` per macro definition — compile time and run time conflated. `syntax-rules` is already hygienic via sets-of-scopes, so the hard part is done. Add explicit renaming (`er-macro-transformer`), keep `define-macro` as a marked legacy extension on top of it, and maintain a phase-1 environment instead of a second interpreter. Blocks nothing today; the expander and compiler must agree on phases eventually. | — |
| 29 | | **Mutable strings, immutable-until-mutated** | — | A real R7RS conformance gap. But `string` is the one class where we beat both references, so it can only cost performance. Use the R31 design and measure. | R31 |
| 30 | | **Drop `source` from runtime `Cons`** | — | Unchanged, unmeasured, low priority. | — |

## Completed

Detail in `../CHANGES.md`; what each one *falsified* in `compiler_findings.md`.

| # | | Task | Outcome | Evidence |
|---|---|---|---|---|
| 1 | ✅ | Stages 0, 1, 2a, 2b.1 | Measurement, interpreter representation, the calling-convention bake-off, the first working tier | R1–R38 |
| 2 | ✅ | Stage 2c′ — `letrec` as a core form | Unblocked every named-`let` loop. Overturned R29. | R39 |
| 3 | ✅ | Stage 2b.2a–2c — resumable forms, capture protocol | Compiled frames take part in captured continuations. The guard became a speed heuristic, not a soundness device. | R40–R43 |
| 4 | ✅ | The symbolic-code weakness | It was an interpreted standard library, not weak code generation. Every figure arguing against self-hosting was measuring that. | R45 |
| 5 | ✅ | AOT stdlib + the `apply` unblock | A one-line change unblocked most of the corpus; coverage 623 → 807 of 1089. | R46 |
| 6 | ✅ | `let` binding chains | The code-size blow-up was `let`, not closures. | R47 |
| 7 | ✅ | `values` / `call-with-values` | Neither was a control operation; found a third tier-boundary conversion bug. | R48 |
| 8 | ✅ | Compiled `call/cc`; the boxing bug | Assigned locals were copied into spilled frames rather than shared. Boxing fixed it at 2–7%. | R49 |
| 9 | ✅ | Build-time AOT for the standard library | CSP-safe: nothing calls `new Function` at run time. The time saving was small; that was never the point. | R50 |
| 10 | ✅ | Letrec-aware lambda lifting | Generated code linear in nesting, not exponential. A size change, not a speed change. | R51 |
| 11 | ✅ | **Promote `ir.scm`; delete `ir.js`** | The lowering pass is Scheme. The bootstrap terminates in the interpreter; `npm run prebuild` runs the chain in 0.62 s, reproducibly. ~18x slower than the JavaScript it replaced. | R52 |
| 12 | ✅ | Whole-program correctness tests | 41 programs × both tiers = 82 assertions, 8.2 s, inside `npm test` (2,426 total). | R53 |

## Decided, so not open

- **Calling convention B** — native JS stack for non-tail calls, trampoline for tail calls,
  cooperative unwind for capture. Settled by the Stage 2a bake-off; see
  [compiler_design.md](compiler_design.md).
- **The interpreter is a permanent tier**, not a transitional one: the CSP-safe execution mode, the
  differential oracle, the maximum-fidelity debug tier, and the compiler's own bootstrap.
- **The compiler moves to Scheme**, above `../src/compiler/runtime.js`. That file stays JavaScript
  because it needs native JavaScript features — a `Map` behind hash tables — that neither generated
  code nor Scheme libraries can express, not because generated JavaScript calls it.
