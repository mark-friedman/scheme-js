# Compiler findings

**Formerly `docs/compiler_strategy.md`.** References to "R25", "R20–R22" and the like elsewhere in
the repository point here. It was split because one file was doing three jobs with three different
lifetimes: the current design now lives in [compiler_design.md](compiler_design.md), which can be
rewritten, and the ranked task list in [compiler_plan.md](compiler_plan.md), which changes constantly. What is
left is the part that must never be rewritten.

## What this is

An append-only record of **what we believed that turned out to be false**, and the measurement that
proved it. Not a history of what was built — that is [../CHANGES.md](../CHANGES.md) — and not a
record of what works, which is the design doc.

The distinction matters because the two read very differently. `CHANGES.md` says "lambda lifting
made generated code linear in nesting." R51 says "it is a size change, not a speed change, and the
remaining outlier turned out to be something else entirely." The second is the one that changes
what you do next.

## Adding an entry

Add one whenever a measurement contradicts something written in the design doc, a gate is missed,
or a technique is used that the plan did not anticipate. **If an entry does not correct a belief or
record an unanticipated technique, it belongs in `CHANGES.md` instead** — a rule worth enforcing,
because entries R50 through R53 each carry a real falsification wrapped in increment narrative that
duplicates the walkthrough.

Entries are numbered, append-only, and never edited away — including the ones that correct earlier
entries in this same log. Annotate a superseded passage with a callout pointing at the entry rather
than rewriting it, so the reasoning that led to a wrong call stays legible.

What makes this worth the effort is visible in R4 and R8: both estimates failed the same way, by
reasoning about which structures *looked* expensive instead of measuring, and by comparing numbers
taken at one benchmark size against a target set at another. Neither pattern would have been
apparent from the corrected document alone.

Nothing here should link out to the design doc or `compiler_plan.md`. Those move; this does not. Links run
the other way.

---

## R0 — the founding analysis, and the beliefs everything since was measured against

This is the opening analysis of the compiler effort, kept verbatim as the baseline the numbered
entries below correct. Several of its claims have since been overturned; where that happened, the
entry that did it says so. It is `R0` rather than prose at the top of a design document because
that is what it is — the first set of beliefs, not a description of what exists.

### Context

The Scheme implementation in this repo is an interpreter, and the concern is that its runtime
performance is structurally limited. The question asked: how likely is it that a compiler-based
approach yields significant runtime gains while preserving (1) high JS interoperability,
(2) browser + CLI operation, (3) REPLs in both, and (4) full-featured debuggers in both?

**Answer: very likely — the measured headroom is roughly 100–600x, and almost none of it is
being spent on actual Scheme semantics.** The constraints are satisfiable, and notably the
existing evaluator is already the *right shape* for the recommended target, so this can be an
evolution of the current machine rather than a rewrite — though nothing below depends on that.

Two constraints were subsequently relaxed: debugger hook points may be redesigned for compiled
code, and any part of the implementation may change provided the four high-level constraints hold
and the result is a fully compliant R7RS-small Scheme. The second of those has a consequence
worth stating up front — see *R7RS compliance* below, the implementation is not fully compliant
today and the deviation is deliberate.

This document records the evidence and a staged plan. It is analysis and planning only —
no implementation is proposed for this pass.

#### Measured baseline (this machine, Node v24.11.1, Apple Silicon)

`fib(30)`, the same naive definition in each system:

| System | fib(30) | vs scheme-js-4 |
|---|---|---|
| Racket CS (compiled) | 6.3 ms | 1000x faster |
| Plain JavaScript on V8 | 10 ms | 650x faster |
| JS with every value heap-boxed + allocated | 23 ms | 280x faster |
| JS with BigInt arithmetic throughout | 28 ms | 230x faster |
| Gambit `gsi` — an *interpreter* | ~240 ms | 27x faster |
| **scheme-js-4** | **6,524 ms** | — |

Gambit's interpreter is the important row: being an interpreter is worth maybe 25x of the gap,
but the remaining ~25x is self-inflicted. The two boxing rows matter too — they show the full
numeric tower and heap-allocated values cost only about 3x, so the numeric-tower optimizations
currently queued in `ROADMAP.md:400-417` are aimed at a ~3x problem while a ~200x problem sits
next to them.

> *Annotation, not a rewrite:* that line reference no longer resolves — the numeric-performance
> section has moved to the `Planned` section of `ROADMAP.md`, carrying this finding with it as the
> reason it is deferred. Line-number citations into other files are the one kind of reference an
> append-only log cannot keep true, which is an argument for citing sections by name.

#### Where the time actually goes

CPU profile of `fib(25)` (`node --cpu-prof`), self time:

| Share | Function |
|---|---|
| 23.2% | `AppFrame.step` — [frames.js:270](../src/core/interpreter/frames.js:270) |
| 12.0% | `Interpreter.step` megamorphic dispatch — [interpreter.js:408](../src/core/interpreter/interpreter.js:408) |
| 11.5% | `TailAppNode.step` + other AST node steps — [ast_nodes.js:341](../src/core/interpreter/ast_nodes.js:341) |
| 6.9% | `Interpreter.run` trampoline — [interpreter.js:234](../src/core/interpreter/interpreter.js:234) |
| 5.9% | `Environment.lookup` — [environment.js:58](../src/core/interpreter/environment.js:58) |
| 4.5% | `Environment.extendMany` — [environment.js:38](../src/core/interpreter/environment.js:38) |
| 3.6% | garbage collector |
| 1.8% | `pushJsContext` — [interpreter.js:195](../src/core/interpreter/interpreter.js:195) |
| **~2.3%** | **actual arithmetic** (`+`, `-`, `%num<` in `primitives/math.js`) |

**Roughly 95% of runtime is interpretive overhead and ~2% is the program's real work.** That
ratio is the whole answer to the question: a compiler removes precisely the 95%.

#### Why it is this slow — specific, fixable causes

A two-argument call `(f a b)` currently costs:

- **O(n²) argument accumulation.** `AppFrame.step` rebuilds `[...this.argValues, value]` and
  `this.argExprs.slice(1)` and allocates a *fresh `AppFrame`* per argument
  ([frames.js:270-286](../src/core/interpreter/frames.js:270)). `TailAppNode.step` additionally
  rebuilds `[funcExpr, ...argExprs]` and slices it again on every invocation
  ([ast_nodes.js:342-355](../src/core/interpreter/ast_nodes.js:342)) — re-deriving at runtime
  what the analyzer already knew.
- **No lexical addressing.** The analyzer alpha-renames to `x_$147` but computes no slot or
  depth, so every variable reference is a string hash walking a parent chain of `Map`s
  ([environment.js:58](../src/core/interpreter/environment.js:58)). Globals walk the chain to the root.
- **Three `Map`s + an `Environment` allocated per call.** `extendMany` builds `bindings` and
  `nameMap`, and the constructor allocates a third `Map` that is immediately discarded
  ([environment.js:38-50](../src/core/interpreter/environment.js:38)). `nameMap` exists solely for
  the debugger, and is built on every call whether or not anyone is debugging.
- **Megamorphic dispatch on every step.** ~30 distinct classes reach `ctl.step`, so V8 cannot
  form an inline cache at [interpreter.js:418](../src/core/interpreter/interpreter.js:418). The
  debug-hook check on `ctl.source` also runs on every step even with debugging off.
- **`<`, `>`, `=`, `<=`, `>=` are Scheme-level variadic procedures with rest parameters**
  ([numbers.scm:32](../src/core/scheme/numbers.scm:32)), so one integer comparison becomes four
  nested applications plus rest-list construction.

That last one is measurable on its own. Swapping `(< n 2)` for the existing binary primitive
`(%num< n 2)` in `fib`, changing nothing else:

```
fib(25) with Scheme variadic  <     : 584 ms
fib(25) with binary primitive %num< : 303 ms   -> 1.93x
```

**One operator is worth 1.93x.** This is why the plan starts before the compiler.

#### What is *not* a problem

- **Startup.** 52 ms module import + ~30 ms to parse/analyze/evaluate all 2,295 lines of
  bootstrap Scheme. No AST caching needed yet.
- **The numeric tower.** ~3x, per the BigInt row above. Worth fixing, not the story.

> [!NOTE]
> **Revised after the canonical suite (R29).** That 3x was measured on `fib`, whose values fit in a
> machine word, and it does not generalize. Bignums are now our **worst** workload class at 52.8x
> Gambit's interpreter (`pi` alone at 94x), and small-integer code gets **0.98x** from the compiler
> tier because BigInt arithmetic — not dispatch — is what dominates it. The tower is not the story
> for call-heavy code and *is* the story for everything numeric.
- **The overall machine design.** The register machine
  (`[ANS, CTL, ENV, FSTACK, THIS]`) with an explicit frame stack and a trampoline is exactly
  the architecture the attached papers advocate and exactly what the fastest published
  Scheme→JS compiler uses. The *encoding* is the problem, not the *model*.

#### R7RS compliance: a pre-existing gap that collides with a stated constraint

`string-set!` and `string-fill!` **throw unconditionally**
([string.js:191](../src/core/primitives/string.js:191), [string.js:323](../src/core/primitives/string.js:323)),
with the message *"strings are immutable in this implementation for JavaScript interoperability."*
Scheme strings are JS strings, which are immutable. R7RS-small §6.7 requires mutable strings.

This is a deliberate trade of compliance for interop, not a bug — but it means "fully compliant
R7RS-small" and "high JS interoperability" are in direct tension at exactly this point, and the
implementation currently resolves it against compliance. Since both are now stated requirements,
this needs an explicit decision, and a re-architecture is the natural moment to take it.

**Agreed resolution:** a `SchemeString` wrapper holding a mutable character array with a cached
JS-string projection, converted lazily at the interop boundary — cheap for the read-mostly case,
correct for the mutating case. This is a value-representation change, so it is scheduled alongside
the other representation work in Stage 2b rather than as a separate exercise.

A compliance audit in Stage 0 should establish whether this is the only such deviation.

> [!NOTE]
> **Revised after Stage 0 — see [R3](#revision-log).** It is not. The audit found 7 required
> identifiers missing, 2 stubs and 2 libraries not importable. `string-copy!` is missing for the
> same reason as the two stubs, so this cluster is **three** procedures. Full results in
> [performance_baseline.md](performance_baseline.md#r7rs-small-conformance).
>
> **Added after Stage 1 — see [R9](#revision-log).** Rational comparison was also broken: `<` was
> comparing `Rational` objects as strings and `=` by identity, so `(= 1/2 1/2)` returned `#f`. Fixed
> in Stage 1. A binding-level audit could not have seen this, which argues for extending the audit
> with behavioural checks.

---

#### Known-broken things found along the way *(all three since fixed — see R0a)*

These are pre-existing and should be **fixed, not preserved** — the goal is a debugger that works
as intended, not conformance to a buggy implementation. Worth fixing early, in Stage 0 or 1, so
that "does the debugger still work?" is a meaningful question later:

- `parse()` never threads a filename — `tokenize` is called with no second argument
  ([reader/index.js:36](../src/core/interpreter/reader/index.js:36)), so every `source.filename` is
  the literal string `'<unknown>'` and file-scoped breakpoints can never match.
- `StackTracer.replaceFrame` has no caller anywhere in `src/`, so the advertised TCO-aware stack
  tracking is not actually wired up; tail calls never push a `DebugExitFrame` either.
- `pauseOnException` reads `registers.env` ([scheme_debug_runtime.js:242](../src/debug/scheme_debug_runtime.js:242))
  but `registers` is an array indexed by `ENV = 2`, so that value is always `undefined`.

---

## R0a — the three "known-broken things" R0 listed are all fixed

R0 closed with three defects to be "fixed, not preserved". All three were, and the document went on
listing them as broken for months, which is the clearest single piece of evidence for splitting a
living design out of an append-only log:

| R0 claimed | actually |
|---|---|
| `parse()` threads no filename, so every `source.filename` is `'<unknown>'` | fixed — `reader/index.js:39` passes `options.filename` to `tokenize` |
| `StackTracer.replaceFrame` has no caller in `src/` | fixed — called from `frames.js:238` |
| `pauseOnException` reads `registers.env` on an array indexed by `ENV` | fixed — `scheme_debug_runtime.js:245` uses `registers[ENV]` |

---

## Revision log

Each entry records a claim in this document that measurement later changed, what the measurement
was, and what followed from it. Entries are append-only; the superseded text stays where it was,
with a callout pointing here.

### After Stage 0 — measurement infrastructure

**R1. Continuation cost was assumed to be a liability. It is the opposite.**
The [calling-convention section](#calling-convention-the-pivotal-decision) treated the cost of our
explicit frame stack as a live risk, and cited Thivierge & Feeley's finding that capture-heavy code
punishes the wrong design by 18.5–96x. Cross-implementation measurement showed that against Racket
CS our ratio *falls* from ~1400x on `fib` to 40–140x on the continuation benchmarks. Our
continuations are relatively less bad than our ordinary evaluation.
*Consequence:* the performance argument for calling convention (B) is weaker than stated. The
DevTools-call-stack argument for (B) is untouched, so the bake-off still stands — but the prior
going into it should be neutral, not tilted toward (B) on continuation grounds.

**R2. One of the three "known-broken" debugger items was materially worse than described.**
It was recorded as "TCO-aware stack tracking is not wired up", i.e. a reporting gap. Measurement
showed that **enabling the debugger broke tail-call optimization outright**: each procedure entry
pushed a `DebugExitFrame` that survived until the whole chain returned, so a tail loop reached frame
depth 806 for 800 iterations against 4 with debugging off. A long-running tail-recursive program
would have exhausted memory under the debugger and not otherwise. Fixed in Stage 0.
*(Only for loops whose tail call is not the last expression of a sequence; see R58.)*
*Consequence:* none for the plan's direction, but it is a reminder that "implemented" and "working"
were not the same thing in the debugger, which is worth carrying into Stage 2b's hook redesign.

**R3. The R7RS compliance gap is wider than mutable strings.**
The [compliance section](#r7rs-compliance-a-pre-existing-gap-that-collides-with-a-stated-constraint)
named `string-set!` and `string-fill!`. The audit found 7 required identifiers missing, 2 stubs and
2 libraries not importable. `string-copy!` is missing for the same reason as the two stubs, so the
string-mutability cluster is three procedures, not two. `(scheme inexact)` has no `.sld` although
all twelve of its procedures are bound globally — a packaging gap, not a functionality one.
*Consequence:* the Stage 2b value-representation work covers three procedures, and there is a
separate small cleanup task that does not depend on it.

### After Stage 1 — interpreter representation

**R4. The 10–30x Stage 1 estimate was wrong. Actual: 2.57x.**
This is the largest correction in the document. Two distinct errors produced it.

*First, the item list was wrong about what mattered.* Stage 1 named lexical addressing as
"probably the biggest single win available". After the other changes landed, profiling put the
entire cost of variable lookup — `Environment.lookup`, `extendManyFrom` and `VariableNode` dispatch
combined — at about **15% of runtime**, so perfect elimination would have been worth ~1.17x. It was
deliberately skipped rather than attempted: it would have required reworking `SyntacticEnv` (which
has one binding per frame, against one runtime frame per lambda), the environment representation,
`StateInspector` and the REPL's `:eval`, for a sixth of the benefit the plan attributed to it.

*Second, the item that produced most of the win was not in the plan at all.* See R5.

*Consequence:* the estimate's failure mode is instructive for Stage 2 — it came from reasoning about
which structures *looked* expensive rather than from measuring where time went after each change.
Stage 2's gates should require a profile between changes, not only at the end.

**R5. A technique not in the original plan produced most of Stage 1's gain.**
**A literal or a variable reference cannot capture a continuation.** There is therefore no
suspension point inside one, so it can be evaluated in place instead of being suspended into a frame
and bounced through the trampoline. Applied to operators, operands and `if` tests, this took `fib`
from 268 ms to 141 ms on its own and cut evaluator dispatches by ~5x overall.
*Consequence for Stage 2:* the compiler should make the same distinction, statically. Classifying
subexpressions by whether they can capture a continuation determines which call sites need a
continuation frame at all — which is precisely the information the calling-convention designs in 2a
are organised around. A procedure containing no capturing subexpressions needs no frame machinery
whatever, under either convention.

**R6. Frames cannot be made mutable, and this constrains Stage 2b.**
The obvious form of "one frame per call" — advancing a single frame in place — is unavailable.
`call/cc` captures the frame stack by copying the array, so frames are shared with every
continuation captured during a call, and mutating one would let a captured continuation observe
operands evaluated after its capture. Cloning at capture is not a workaround either: `dynamic-wind`
finds the common ancestor of two stacks by frame **identity**, so cloning would run the wrong
`before`/`after` thunks.
*Consequence:* this is a hard constraint on any continuation representation Stage 2b chooses, not
just on the interpreter. Tests pinning the behaviour were written first, in
`tests/core/scheme/number_tests.scm`.

**R7. Stage 1's gate — "close the gap to Gambit `gsi` to roughly parity" — was not met.**
The gap closed from ~26x to **~8x**, not to parity.
*Consequence:* combined with R4, this says an AST-walking interpreter over a reified frame stack has
a floor meaningfully above a bytecode interpreter like `gsi`. Closing the last ~8x is a change of
execution model, which is Stage 2. Primitive work rose from 2.8% of runtime to 8.5% across Stage 1,
so the work-to-overhead ratio improved about 3x; the remaining ~55% is frame machinery that cannot
be removed without changing the model.

**R8. The Stage 1 gate was missed on its own yardstick too, not just on the suite average.**
An earlier draft of this log claimed `fib` had landed inside the gate's predicted 250–650 ms band
and that only the multiplier was missed. That was wrong: the band was stated for `fib(30)`, and the
benchmark suite runs `fib` at size 25. Measured directly, `fib(30)` is **1,650 ms** — 3.95x better
than the 6,524 ms baseline, and 2.5–6.6x above the predicted band.
*Consequence:* none for the plan, but it is the second time in this document that a number was
quoted at one benchmark size against a target set at another (the first being the estimate in R4).
Sizes are recorded with every result in
[performance_progress.md](performance_progress.md) for exactly this reason, and comparisons there
are refused across differing sizes rather than silently drawn.

**R9. Rational comparison was broken, found while optimizing it.**
`<`, `>`, `<=`, `>=` bottomed out in JavaScript's `<` applied to `Rational` objects, which have no
`valueOf`, so they were compared **as strings**; `=` used `===` and compared by identity.
`(= 1/2 1/2)` returned `#f`. Fixed in Stage 1.
*Consequence:* adds to R3's picture. It also suggests the conformance audit should be extended from
"is this identifier bound and does it throw?" to behavioural checks, since this bug was invisible to
a binding-level audit.

### After Stage 2a — calling-convention bake-off

**R10. Convention B wins, and by more than expected: it is faster on the normal path *and*
faster overall.**
Both prototypes were built from a shared front end and both pass all eight benchmarks, including
the two that require multi-shot continuations. Measured (median of 7, `quick` sizes):

| Benchmark | capture | A explicit stack | B native stack | winner |
|---|---|---|---|---|
| `fib` | no | 16.4 ms | 5.1 ms | **B 3.2x** |
| `tak` | no | 4.7 ms | 1.4 ms | **B 3.3x** |
| `oddeven` | no | 2.7 ms | 2.7 ms | B 1.0x |
| `nqueens` | no | 2.3 ms | 1.3 ms | **B 1.8x** |
| `ctak` | yes | 9.0 ms | 11.0 ms | A 1.2x |
| `contfib` | yes | 2.8 ms | 4.2 ms | A 1.5x |
| `btsearch` | multi-shot | 5.9 ms | 8.8 ms | A 1.5x |
| `threads` | multi-shot | 17.4 ms | 11.4 ms | **B 1.5x** |

Geometric mean speedup over the current interpreter: **A 13.0x, B 17.5x.**

The plan's stated prior was that B would win "if its overhead is under ~2x". B has no overhead to
excuse: it is ahead on the normal path by up to 3.3x and ahead overall, and only gives up 1.2–1.5x
on the capture-heavy programs. [R1](#revision-log) had already weakened the performance case for B;
the measurement reversed it.

**R11. The DevTools question is settled, and it is one-sided.**
A debugger's call-stack panel is rendered from the JavaScript stack, and a source map relabels
those frames but cannot invent frames that are not there. Probing a Scheme recursion 12 frames
deep:

| Convention | Scheme frames visible on the JavaScript stack |
|---|---|
| A — explicit frame stack | **1** |
| B — native JavaScript stack | **13** (12 levels plus the entry call) |

Under B the Scheme call stack *is* the JavaScript call stack. Under A the frames live in an array
the debugger cannot see, so a custom stack view has to survive no matter how good the source maps
are.

*Caveat, stated plainly:* what was measured is the stack **shape** — that one live Scheme frame is
one live JavaScript frame. Relabelling those frames with Scheme names and positions via a source
map is a separate, mechanical step that was **not** verified end to end in DevTools. That
verification belongs in Stage 2b before `extension/` is actually deleted.

**R12. Supporting `call/cc` everywhere costs code size, not speed — 4.09x in the prototype.**
Convention B's fast path is straight-line JavaScript; to be re-enterable after a capture it needs a
second, state-machine copy of each procedure (Marshall's separate `Continue` method). Generated
code across the suite: **A 23,244 bytes, B 95,008 bytes.**

The prototype emits a twin for *every* procedure, which is conservative. An effect analysis that
proves a procedure can never capture would let most twins be dropped, and that is now a concrete
Stage 2b task rather than a speculative optimization. Code size matters for browser delivery in a
way it does not for a server, so this is the one real cost of the recommended convention.

**R13. Both conventions need the same re-entry machinery; they differ in where it is paid.**
Not anticipated by the plan, which framed fragmentation as a cost unique to B. Convention A needs a
procedure to be resumable at *every non-tail call*, because control leaves through the trampoline
each time. Convention B needs it only where a capture is possible. A pays on the normal path; B
pays in code size. That is the whole trade, and it is why A's straight-line speed is worse despite
its much smaller output.

**R14. Assignment conversion is required by both, for a reason the plan did not record.**
A procedure that can be re-entered restores its locals from a frame, creating a *fresh* JavaScript
binding; a closure made before that point still refers to the old one, so assignments through one
are invisible to the other. The `threads` benchmark returned 0 instead of 4000 until local mutable
variables were boxed. Pettyjohn et al. list this as step 1 of their transformation and Marshall
keeps it; it is a requirement of first-class continuations, not a property of either convention.

### After Stage 2b increment 1 — the compiler tier

**R15. Declining individual procedures that mention `call/cc` is unsound. The `btsearch`
benchmark returned a wrong answer, not an error.**
The first compiler tier declined any procedure that referenced a control-transferring global and
compiled the rest. That looked safe and was not. In `btsearch`, `in-range` was correctly declined,
but `btsearch` and `enumerate` were compiled — and both sit in the *dynamic extent* of the capture
and must be re-entered when the search backtracks. A compiled frame cannot be re-entered, so the
program silently produced the wrong pair instead of failing.

The property that actually matters is not "does this procedure mention `call/cc`" but **"can a
capture occur within this frame's dynamic extent"**, which per-procedure inspection cannot answer.

*Consequence:* the rule is now coarser — if *any* definition in a compilation unit references a
control global, the whole unit is left interpreted. That is sound for a self-contained unit and is
what the four non-continuation benchmarks need. It is still not sound in general, because a
compiled procedure can call into another unit that captures within its extent. **Until compiled
frames are re-enterable the tier is opt-in and off by default**, and making them re-enterable via
the unwind protocol chosen in Stage 2a is now the top of increment 2.

The differential suite had passed before this was found, because its cross-tier cases used `apply`
rather than `call/cc`. `btsearch` is now a test, alongside one that bypasses the guard and asserts
the result goes wrong — so the guard cannot be weakened later by someone who sees no consequence.

**R16. Compiling the analyzed AST rather than source was the right call, and it replaced the IR the
plan asked for.**
[Stage 2b](#stage-2b--the-compiler-tier) called for lowering to "ANF or CPS over a control-flow
graph". Increment 1 instead consumes the output of `src/core/interpreter/analyzer.js` and lowers it
to a normalized tree that records only the two things code generation actually needs: **tail
position** (which the analyzer does not track — every application is a `TailAppNode`) and
**local versus global** reference.

Consuming the analyzed AST means macro expansion, hygiene, alpha-renaming and internal-definition
hoisting are inherited rather than reimplemented, so the two tiers agree on what a program *means*
by construction instead of by two front ends being kept in step. That is worth more than an
optimizer-ready IR is at this stage.

*Consequence:* the CFG/CPS form is deferred until an optimization actually needs it. The warning in
Stage 2b — that direct AST-to-JavaScript codegen "supports almost no real optimization" — still
stands and still applies to the increment *after* continuation support.

**R17. Reusing the interpreter's own `TailCall` as the tail-call signal made interoperation free.**
Not anticipated by the plan. A compiled tail call returns the interpreter's `TailCall`, which the
interpreter already knows how to continue, and a compiled trampoline already knows how to continue
one returned by an interpreted procedure. Mixed-tier mutual tail recursion therefore works in both
directions with no boundary code, and a compiled procedure marked `SCHEME_PRIMITIVE` is called by
the interpreter without argument conversion — so exact integers stay exact across the boundary.

**Measured:** **5.35x geometric mean** over the Stage 1 interpreter on the four benchmarks the tier
accepts (`fib` 5.8x, `tak` 8.4x, `nqueens` 8.3x, `oddeven` 2.0x). Cumulatively `fib` has gone from
**593 ms at the Stage 0 baseline to 25 ms** — about **23x**. `npm run benchmark:compiled`.

### After Stage 2b increment 1b — primitive inlining

**R18. Inlining primitives more than doubled the tier, from 5.35x to ~12x.**
Profiling the tier put **25% of its runtime in primitive calls and only 17% in the generated code
itself**, plus 11% in resolving globals. A call such as `(+ a b)` was going through a variadic
primitive that allocates a rest array, type-checks each argument and dispatches across the numeric
tower — to add two integers.

Two changes followed, both contained in the compiler:

- **Global resolution** now caches the *frame* holding a binding and reads it with one hash lookup,
  instead of walking the environment chain on every reference. Caching the frame rather than the
  value keeps a later `define` or `set!` observable, because both mutate that frame's map in place,
  and Scheme has no way to remove a binding. (`fib` 25.1 → 20.4 ms.)
- **Primitives are expanded inline** with an exact-integer fast path and a fallback to the real
  primitive, so the numeric tower is preserved rather than approximated — a rational, a flonum, a
  complex or a wrong type all take the fallback and behave exactly as interpreted. Every expansion
  is guarded on the binding, since Scheme allows `+` to be redefined after compilation. The largest
  single part of this was inlining primitives **in tail position**: `(+ ...)` closing a procedure
  body was allocating a `TailCall` for something that cannot tail-call. (`fib` 20.4 → 8.0 ms.)

Result: **~12x geometric mean** where the tier applies, and primitives now measure **0.0%** of the
compiled profile. `fib` has gone from **593 ms at the Stage 0 baseline to about 8 ms — roughly
70–75x.**

**R19. The next optimization the profile suggested was a 7% regression, and the profile was the
reason.**
With primitives inlined, the profile attributed 9.3% to the global accessor, and `fib`'s recursive
self-call looked like the obvious target: call the compiled function directly, guarded on the
binding. Implemented and A/B measured at a larger size, it was **slower** — `fib(30)` 82.7 ms
against 76.7 ms, `tak(22)` 21.3 against 20.5. The accessor is already a single hash lookup that V8
inlines, and the guard's conditional callee costs more than it saves. **It was removed**, with the
reason recorded at the site so it is not tried again.

*Consequence, and a correction to how this document reads profiles:* that profile was taken at a
**9 ms wall time, where the sampling profiler's own overhead was 66% of samples** and inflated
every remaining share. This is the third time in this log that an estimate failed by reasoning from
what looked expensive rather than from an A/B measurement ([R4](#revision-log),
[R8](#revision-log)). The rule going forward: **profile to find candidates, A/B to decide**, and
distrust any profile whose own overhead is a large fraction of the run.

### After the benchmark-validity review

**R20. The benchmark suite was overfitted to the optimizations, and the reported speedups do not
transfer. Measured.**
Prompted by the question "how do we know these are the right benchmarks", a coverage comparison and
a macro-benchmark on the repository's own Scheme gave the answer: they are not, and we do not.

| | microbenchmarks | the repo's own `.scm` test files |
|---|---|---|
| distinct callables exercised | **16** | **136** |
| share of calls on a primitive the compiler inlines | **98.0%** | **34.2%** |
| compiler tier speedup | **~12x** | **1.39x** per-file geometric |

Every optimization since Stage 0 was chosen by measuring against eight microbenchmarks that *I*
wrote in Stage 0, so the suite and the optimizations were fitted to each other. The fifteen
primitives inlined in increment 1b account for 98% of primitive calls in those benchmarks and 34%
in real code. `fib` is literally `<`, `+`, `-` on small integers.

*Consequence:* **the ~12x figure should be read as an upper bound on hot numeric loops, not as what
a program will see.** The fast paths only fire when both operands are `bigint`, so a flonum- or
rational-heavy program gets close to none of it — and nothing in the suite would have revealed that.
Stage 1's gains are better founded, because the *evaluator node-type* distributions do match real
code closely (`TailAppNode` 44% against 45%, `IfNode` 11% against 18%), and Stage 1 targeted
dispatch mechanics rather than particular primitives.

**R21. The macro-benchmark's first two versions were both wrong, in instructive ways.**
- Version one swept the environment and compiled the *standard library* before running the
  workload. It reported **0.92x — the tier looking 8% slower than the interpreter.** The cause was
  structural: the workload defines its own hot procedures at run time, after the sweep, so they
  stayed interpreted. Compiling definitions as they appear, the way a tiered runtime would, changed
  the same measurement to 3.79x.
- Version two reported that 3.79x as the headline. But **one file, `tco_tests.scm`, is 95% of the
  total** — a space-usage test running a million-iteration tail loop. The "macro-benchmark" was
  reporting a microbenchmark. Per-file speedups with a geometric mean give every file equal weight
  and yield **1.39x**, with dominant files named so the total can still be read.

*Consequence:* a total over a suite of unequal files reports the biggest file. The benchmark now
reports both, plus the per-file table, and flags any file over 20% of the total.

**R22. Two documentation errors, both found by being asked a direct question.**
- This document and `CHANGES.md` said "four" benchmarks came from Thivierge & Feeley. Their set is
  **seven** (`fib35`, `nqueens12`, `oddeven`, `ctak`, `contfib30`, `btsearch2000`, `threads10`), and
  we have all seven, plus `tak` which I added from the Gabriel set. `CHANGES.md` said "four" and
  then listed six.
- **Our `threads` is not their `threads10`.** Theirs (their Figure 15) uses a vector-based
  doubly-linked queue with a `graft`/`boot` continuation pattern and runs 10 threads × 100,000
  yields, about a million context switches. Mine is a list-based scheduler doing 4,000 yields. Its
  numbers are not comparable to their published table, and it misses the vector coverage theirs
  would have given. Comparability to their tables also requires `canonical` sizes, which nothing
  reported so far has used.

*Consequence:* the canonical sources should come from `ecraven/r7rs-benchmarks`, the Larceny/Gabriel
lineage the paper drew from, rather than from transcribing figures out of a PDF — the program count
was already mis-read off those tables once.

### After running real code across implementations

**R23. The microbenchmarks are unrepresentative of *which optimizations help*, but they are
accurate about *where we stand*. These are different questions and the suite is fit for one of
them.**
Running the repository's own Scheme test files under Gambit and Racket -- the same workload as the
macro-benchmark, timed inside the program with R7RS `current-jiffy` so process startup is excluded:

| measure | microbenchmarks | real code |
|---|---|---|
| slower than Gambit `gsi` | 7–14x | **13.8x** (geometric mean over 21 files, range 4.9–42.4x) |
| compiler tier speedup | ~12x | 1.39x |

So the suite's *standing* against an external implementation transfers almost exactly, while its
*sensitivity to our optimizations* does not. That is the sharpest available statement of what went
wrong: the eight programs are a reasonable sample of Scheme's cost structure in aggregate, and a
poor sample of the specific operations increment 1b optimized.

*Consequence for methodology:* cross-implementation measurement is not only a comparison, it is a
**validity check on the benchmark**. If a program is relatively expensive for us *and* for Gambit
and Racket, the benchmark is measuring something intrinsic to the program; if it is expensive only
for us, it is measuring our implementation. A single-implementation number cannot tell those apart,
which is why every future benchmark should be run cross-implementation before its numbers are
trusted.

**R24. Three measurement bugs in building that comparison, each of which would have produced a
confidently wrong number.**
- **Process timing could not resolve the workload.** Subtracting a measured 21 ms startup from files
  doing 1–3 ms of work clamped every result to zero. Fixed by timing *inside* the program with
  `current-jiffy` and repeating the body 100 times.
- **We were charged for work the others do once.** Our side re-ran `analyze` on every repetition
  while Gambit and Racket ran already-compiled code. That inflated our figure several-fold. Fixed by
  analyzing before the timed region.
- **Racket's clock cannot measure this workload.** Its `jiffies-per-second` is 1,000 against
  Gambit's 1,000,000 — 10 µs of effective resolution per iteration against 0.01 µs — so most files
  were being measured in one to three ticks. The comparison now probes each implementation's clock
  and labels the figure LOW CONFIDENCE rather than presenting arithmetic as measurement.

*Consequence:* all three were found by checking whether a number was plausible rather than by the
measurement failing. A benchmark that produces a number is not a benchmark that produced the right
number.

**R25. The canonical suite found four R7RS conformance gaps on its first run, three of them
previously unknown.**

Step 2 of the benchmark-validity plan vendored the Gabriel/Gambit/Larceny programs from
`ecraven/r7rs-benchmarks` into `benchmarks/r7rs/`. Fifty-one programs were imported; six cannot run
here, for four distinct reasons:

| Gap | Programs blocked | Status before this |
|---|---|---|
| Identifiers containing `.` are rejected by the reader's **extended dot notation**, which turns `foo.bar` into a JavaScript property access. `(define x.y 1)` fails, though R7RS §7.1.1 permits the dot. | `gcbench`, `matrix`, `slatex` | the feature was known, documented and tested; its conformance cost was not |
| `read-char` and `peek-char` return JavaScript strings, not Scheme characters, so `(char? (read-char p))` is `#f`. | `parsing`, `read0` | unknown |
| `equal?` does not terminate on circular structure, which R7RS §6.1 requires. Gambit runs the program in 0.08 s; we hang at every size. | `equal` | unknown |
| `string-set!` throws unconditionally. | `compiler` (not vendored) | known, documented as a deliberate interop trade |

*Consequence:* the conformance audit run in Stage 0 was a survey of what we had implemented, not a
test of whether it behaved. Forty-odd programs written by people with no knowledge of this
implementation found three behavioural gaps in an afternoon.

The dotted-identifier one is the most consequential, and it needs stating precisely: extended dot
notation is a deliberate, documented and tested interop feature
([Interoperability.md](Interoperability.md), `tests/extras/scheme/dot_access_tests.scm`). This is
therefore the *second* instance of the `string-set!` pattern — a trade of R7RS conformance for
JavaScript interop, made on purpose, whose conformance cost was never written down. Both constraints
are now stated requirements, so both trades need an explicit decision rather than an inherited
default. A plausible resolution for this one is narrower than for `string-set!`: dot notation could
fire only where the head is bound to a JavaScript object, leaving `foo.bar` as an ordinary
identifier otherwise. That is a reader change, and it should be measured against the dot-access
tests before being adopted.

**R26. The compiler tier silently returns wrong answers when compiled code calls an interpreted
closure. Single root cause, found by the first run of an independent suite.**

Nine of the forty-one programs failed under the compiler tier while passing under the interpreter:
`pi`, `chudnovsky`, `lattice`, `puzzle`, `destruc`, `earley`, `array1`, `bv2string`, `string`, plus
`maze` with an out-of-range vector index. The minimal reproduction, using only project APIs:

```scheme
(define (hide r x)                       ; from the benchmark suite's common.scm
  (call-with-values
    (lambda () (values (vector values (lambda (x) x)) (if (< r 100) 0 1)))
    (lambda (v i) ((vector-ref v i) x))))
(define (probe) (list (exact? (hide 1 33)) (hide 1 33)))
```

| | `(probe)` |
|---|---|
| interpreter | `(#t 33)` |
| compiler tier, `hide` left interpreted | `(#f 33.0)` |

A value returned from an interpreted closure into compiled code has **JavaScript auto-conversion
applied**: exact integers become inexact JS numbers, and a `BigInt` outside the safe integer range
throws outright, which is what `pi` and `chudnovsky` hit. Everything downstream follows —
`(case k ((33) ...))` stops matching in `lattice`, `equal?` comparisons fail in `destruc` and
`earley`, and a computed vector index goes negative in `maze`.

*Consequence, and the reason this is the most important entry in the log so far:* **none of this
was detectable by the existing suite.** All 2,152 tests pass. All eight microbenchmarks pass. They
pass because those eight programs are self-contained — every procedure they call is one they
defined and the compiler accepted, so nothing ever crosses back from the interpreter — and because
their result checks use numeric `=`, under which `75025.0` and `75025` are indistinguishable. The
canonical programs cross the boundary constantly and check with `equal?`.

This is the R15 shape for a third time: the failure mode of this tier is a *wrong answer*, not an
error. It is also the second time a benchmark found what the tests missed. Fixing it is now the
first item of Stage 2b increment 2, ahead of re-enterable frames, because there is no point making
the tier enableable while it is unsound at the boundary.

**R27. Two harness bugs while building the suite, both caught before they reached a result.**
- The `read` shim took no port argument, so `dynamic`, `read0`, `read1` and `sum1` — which open
  their own data files — silently read the wrong stream and returned wrong answers instead of
  failing. Fixed by accepting an optional port.
- The compiler tier was being pointed at the harness's own prelude, which made `sum` and `tak`
  report inexact results. That is a real compiler defect (the same one as R26) but not one this
  benchmark should be provoking: the prelude is scaffolding, and compiling it would have meant
  reporting a measurement of the test rig as the workload. The prelude is now evaluated
  uncompiled.

Also: `takl` was first sized from the "old inputs" documented in its canonical input file, which
turn out to be `tak(32,16,8)` and did not finish in two minutes here. That looked like an
implementation pathology and was reported as one for about ten minutes. It was a sizing mistake.
Re-sized to 18/12/6, `takl` runs in 880 ms and is the compiler tier's best result on the whole
suite at 27.8x.

*Consequence:* three of the last five measurement errors in this project have been in the
measuring apparatus rather than in what was measured. The suite now runs each program in a child
process under a wall-clock budget, so a hang is reported as a hang.

**R28. The compiler tier has no shippable configuration today, and every speedup this project has
quoted for it was measured in the unsound one.**

R15 established that declining *individual* procedures that reference `call/cc` is unsound —
`btsearch` returned a wrong answer because procedures in the dynamic extent of a capture must be
re-enterable and a compiled frame is not. The recorded fix was a **unit-level** guard: if any
definition in the unit touches a control global, the whole unit stays interpreted.

That guard lives only in `compileProgram`. `tryCompileDefinition` — the incremental entry point —
has no continuation guard at all; it declines only when IR lowering fails, and `lowerLambda`
declines a lambda that references a control global, which *is* the per-procedure rule R15 proved
unsound. Both `benchmarks/run_macro.js` and `benchmarks/lib/r7rs_harness.js` use that path.

Measured, on the canonical suite:

| guard | programs compiling anything |
|---|---|
| per-definition (`tryCompileDefinition`) — **unsound** | 41 of 41 |
| unit-level (`compileProgram`) — **sound** | **0 of 41** |

Zero. `CONTROL_GLOBALS` includes `values`, `call-with-values` and `apply`, and the benchmark
suite's shared `common.scm` defines `hide` in terms of `call-with-values`. One such form anywhere in
a unit disables the tier for the whole program, and `values`/`apply` appear in nearly all real
Scheme.

*Consequences, and this is the most important entry in the log:*

1. **The tier is either unsound or vacuous.** There is no setting in between. "Opt-in and off by
   default" understated this: the configuration that is on is the wrong one, and the configuration
   that is correct does nothing.
2. **Every tier number this project has published was taken in the unsound mode** — the ~12x on
   microbenchmarks (R18), the 1.39x transfer figure (R20), and the per-class figures from the
   canonical suite. They are not fabrications: none of the failing programs capture a continuation,
   so per-procedure declining did not actually mis-execute them. But they were measured in a
   configuration that cannot ship, and every one of them must be re-taken once the guard is sound.
3. **Increment 2 is not "next", it is a prerequisite to measuring the tier at all.** Re-enterable
   compiled frames are what let the guard be both sound and useful. Until then, further codegen work
   is optimizing something whose performance cannot be honestly reported.

*How this was missed:* the unit-level guard was written, documented at length in
`src/compiler/index.js`, and tested — and then the benchmarks were pointed at a different entry
point that does not use it. The doc comment describing the soundness argument sits above a function
neither benchmark calls.

**R29. The compiler tier is a control-flow optimizer, and five of seven workload classes are not
control-flow-bound.**

> [!WARNING]
> **Overturned by R39.** The measurements were taken while named `let`, `do` and internal
> definitions could not be compiled at all, so the hot loop of every fixnum, flonum and vector
> program was interpreted. With `letrec` a core form, fixnum goes 1.01x → **11.95x** and flonum
> 1.30x → **8.94x**. Only the bignum finding survives. The resequencing this entry justified is
> withdrawn.

Per-class, from the canonical suite:

| Class | tier speedup | what dominates once calls are cheap |
|---|---|---|
| call | **4.17x** (`takl` 25.6x) | — this is what it removes |
| flonum | 1.35x | generic primitive dispatch; inline fast paths guard `bigint` only |
| list | 1.34x | allocation and standard-library calls |
| continuation | 1.04x | compilation largely declined |
| fixnum | **0.98x** | BigInt arithmetic itself |
| string | 0.97x | already at or ahead of the references |

The cleanest evidence is a matched pair and a matched contrast. `fib` gets 5.60x and `fibfp` 7.06x —
same shape, different numeric type, both win, because both are call-bound. `sumfp` 0.99x, `mbrot`
1.02x and `fft` 1.02x are loops where arithmetic dominates and calls do not. `sum` at 0.98x is the
purest case: its body is already entirely inlined primitives, so what remains is V8's BigInt
addition, which no amount of better code generation touches.

*Consequence:* the plan's premise — that removing ~95% interpretive overhead is broadly valuable —
holds for call-heavy code and not elsewhere. **Value representation moves ahead of further codegen
work**: fixnums as JS numbers with checked promotion, and flonum fast paths in `inline.js`, are what
unblock the non-call classes. Two cheap follow-ons are implied:

- The founding profile (`AppFrame.step` 23%, "~95% overhead, ~2% real work") was taken on `fib`,
  one program from the suite later shown to be overfitted. **Re-profile per workload class.** A
  `sumfp` or `pi` profile almost certainly looks nothing like it.
- `graphs` measured **0.84x** — a real regression under the tier. Under the Pareto rule adopted for
  this project (ship when at least one class improves and none regresses), the list class does not
  currently pass.

**R30. Mixed-tier execution is the steady state, not a transition, so the tier boundary is a
component that has to be designed.**

Compilation coverage is low and uneven — `puzzle` 1/21 definitions, `graphs` 3/18, `mbrot` 1/6,
`nboyer` 3/7 — and high coverage does not rescue the result: `scheme` compiles 86 of 112 for 1.03x,
`dynamic` 145 of 232 for 1.01x. Two independent problems: not enough is compiled, and what is
compiled often does not help (R29).

This reframes R26. The compiled→interpreted edge was built as a *JavaScript* boundary, reusing
`createClosure`'s JS-callable wrapper with its `unpackForJs` conversion, while the tail-call edge got
a proper Scheme boundary via the `TailCall` reuse celebrated in R17. There are two boundaries and
only one was designed. With most programs 5–20% compiled that edge is crossed constantly — it is the
common case, not an edge case. Note also that *every* canonical program routes its result through
the interpreted `hide`, which is exactly why a single conversion defect took out ten of them.

*Consequences:*

- Compiled code needs its own internal calling path to interpreted closures that never reaches
  `unpackForJs`. Correctness first; then measure the boundary's **cost**, which nobody has.
- **AOT-compiling the standard library (increment 5) rises in value.** Symbolic code leans on `map`,
  `assoc` and `append`, all interpreted Scheme, so each is a boundary crossing. This may matter more
  for the list class than any codegen change.
- **Compilation coverage becomes a reported metric**, with a histogram of decline reasons — we have
  printed "N/M compiled" since increment 1 and never acted on it. Better still, report the share of
  *runtime* spent in compiled code rather than the share of definitions.
- Convention B's 4.09x code-size cost buys re-enterable twins for capture, and the continuation
  class currently returns 1.04x. That is not a reason to revisit B, whose rationale was DevTools
  stack fidelity — but it is a reason to generate the twin **lazily** rather than eagerly for every
  procedure.

**R31. The string representation is a measured advantage, which changes what increment 4 should
build.**

We are **faster than both references** on `string`, at 0.3x of Gambit and 0.5x of Racket — the only
class where we lead. The cause is the representation: Scheme strings are JavaScript strings, so V8's
rope representation makes `string-append` close to free.

That is the *same* decision that makes `string-set!` throw. Increment 4 proposes a mutable
`SchemeString` holding a character array, which done naively trades away the one class where we are
ahead.

*Consequence:* the design should be **immutable-until-mutated** — keep the JavaScript string as the
representation and explode to a mutable character array only on first `string-set!`. Read-mostly
code, which is nearly all Scheme string code, keeps today's performance; the cost is paid only by
programs that actually mutate. The earlier framing of this as "a `SchemeString` wrapper with a
mutable char array and a cached JS-string projection" has the default backwards.

**R32. The boundary conversion was fixed, and it had been corrupting the *measurements* as well as
the answers.**

Increment 2a. Compiled code now reaches an interpreted closure through a new raw entry point
(`SCHEME_RAW_CALL` in `values.js`, `R.invoke` in `src/compiler/runtime.js`) that converts nothing,
instead of through the closure's JavaScript-facing wrapper. Nine of the ten failing canonical
programs recovered; `maze` did not, and is a separate defect — see below.

The surprise was the performance. Fixing a correctness bug made the compiler tier **substantially
faster**:

| | before | after |
|---|---|---|
| `fib` | 5.60x | **23.67x** |
| `tak` | 9.35x | **29.78x** |
| `ack` | 8.53x | **23.63x** |
| call class | 4.17x | **6.69x** |

The mechanism is worth stating because it generalises. Every canonical program passes its input
through the interpreted `hide`, so every program's working value was being converted from `BigInt`
to a JavaScript number on the way in. The inline fast paths in `src/compiler/inline.js` are guarded
on `typeof x === 'bigint'`. A converted input fails that guard **on every operation for the whole
run**, so the entire program fell back to the generic tower primitives. The bug was not just
returning wrong answers; it was quietly disabling the single optimization the tier depends on.

*Consequence:* a correctness defect at a type boundary can masquerade as a performance ceiling. The
per-class table in R29 was measured through it and has been re-taken.

**R29 survives the correction, and sharpens.** The revised classes:

| Class | tier speedup | range |
|---|---|---|
| call | **6.69x** | 1.02x – 29.78x |
| flonum | 1.34x | 0.98x – 6.59x |
| list | 1.27x | 0.96x – 3.68x |
| bignum | 1.11x | 0.99x – 1.24x |
| continuation | 1.07x | 0.98x – 1.16x |
| string | 1.05x | 0.99x – 1.13x |
| fixnum | 1.01x | 0.98x – 1.05x |
| vector | 1.01x | 0.98x – 1.04x |

The gap between the call class and everything else *widened*, from 4.17-against-1.35 to
6.69-against-1.34. The conclusion that value representation must precede further codegen work
therefore stands unchanged, and the resequencing in `ROADMAP.md` stands with it. The `graphs`
regression noted in R29 is gone (0.84x → 1.04x); `earley` is now the mildest regression at 0.96x.

*Also worth recording:* the fix is Pareto-positive by the rule this project adopted — one class
improved by 60%, none regressed.

**R33. `maze` fails under the tier for a second, unrelated reason, still open.**

> [!NOTE]
> **Wrong — corrected by R34.** It is not a second reason and it is not unrelated. It is the
> continuation unsoundness of R15/R28, reproduced in the wild. The narrowing below is accurate; the
> conclusion drawn from it was not.

Narrowed but not solved, recorded so the next attempt does not start over:

- Three definitions each cause it alone: `make-maze`, and `pmaze` and `run`, which call it. So the
  defect is in `make-maze`.
- The **generated code is correct** on inspection: `let*` initialisers are emitted in source order
  and the body ends with `return new R.TailCall(G10(), [cells, entrance, exit])`, `G10` being
  `vector`.
- Calling the compiled `make-maze` directly and driving its tail-call chain by hand **produces the
  right answer** — nine hops ending in `vector` with three arguments.
- It fails only when called from interpreted code, which reaches it through
  `continueApplication`'s `TailCall` branch.

So the defect is in how a tail-call chain returned by a compiled procedure is driven by the
interpreter, not in what the compiler generated. Two candidate suspects, neither confirmed: the
chain passes through a two-argument inner `let` (`$fn8`), and it terminates in a *primitive* rather
than a closure.

A hypothesis that was tested and **disproved**, so nobody repeats it: `make-maze` rebinds `walls` in
an inner `let*` that shadows an outer one, which looked like an obvious culprit. Five shadowing and
ordering cases were written against both tiers and all five agree.

**R34. `maze` is not a compiler defect. It is R28's unsoundness producing a wrong answer in the
default benchmark configuration, on a program that never mentions `call/cc`.**

R33 concluded that the defect lay "in how a tail-call chain returned by a compiled procedure is
driven by the interpreter." That was wrong, and it was wrong because the investigation stopped at
the point where the evidence was still consistent with two explanations.

What the evidence actually was: the same compiled `make-maze`, in the same environment, returns the
right maze when its tail-call chain is driven by hand or by `R.settle`, and `#f` when driven by the
interpreter. R33 read that as a defect in the interpreter's driving. Tracing the interpreter's
tail-call branch showed something else — the chain simply *stops* three hops in, and the next thing
to run is `call-with-current-continuation`:

```
[TC] from make-maze  -> anonymous  args 1     ; $fn0(cells)
[TC] from anonymous  -> anonymous  args 1     ; $fn1(walls)
[TC] from anonymous  -> anonymous  args 0     ; $fn2()  -- calls dig-maze, and stops
[TC] from call-with-current-continuation -> anon args 0
```

`dig-maze` wraps its loop in `call/cc` and aborts early with `(quit #f)`
(`benchmarks/r7rs/src/maze.scm:277`). Compiled `make-maze` returns `#f` — the escape value itself.
The escape unwinds past `make-maze`'s compiled JavaScript frame, which has no representation on the
interpreter's frame stack and so cannot be resumed, and `#f` becomes `make-maze`'s result instead of
`dig-maze`'s.

Minimal reproduction, now a test:

```scheme
(define (escaper n)
  (call/cc (lambda (quit) (if (> n 0) (quit 'escaped)) 'normal)))
(define (caller n) (cons (escaper n) '(tail)))
(caller 1)
```

| | result |
|---|---|
| interpreted | `(escaped tail)` |
| `caller` compiled | `escaped` |

When the escape is not taken both tiers agree, which isolates it precisely.

*Why this matters more than a compiler bug would.* `caller` does not mention `call/cc`, so
`lowerLambda` compiles it without hesitation — the per-procedure rule R15 proved unsound. This is
the **second** instance after `btsearch`, and it is more damning than the first for three reasons:

1. It is an **escape**, not a re-entry. R15 was explained in terms of re-entering a frame when a
   search backtracks, which reads like an exotic case. Escaping early from a loop is not exotic; it
   is what `call/cc` is mostly used for in ordinary Scheme.
2. It is happening **right now**, in the configuration both benchmark harnesses use, and it is the
   only remaining wrong answer on the canonical suite.
3. The compiled procedure is two call levels away from the capture. No local inspection of
   `make-maze` would suggest it is unsafe.

*Consequence:* there is no fix for `maze` short of increment 2b. The remaining wrong answer on the
suite is the guard, not the compiler, and R28's "unsound or vacuous" is now a measured statement
about a real program rather than an argument about `CONTROL_GLOBALS` membership.

**One option worth weighing before increment 2b**, because it is small and strictly better than
either current mode: replace the whole-unit veto with **call-graph reachability**. `lowerLambda`
already returns each procedure's global references, so a fixpoint can decline any procedure that
reaches a control global transitively. `make-maze` would be declined (it reaches `dig-maze`); `fib`
would still compile. It is not sound in general — a callee arriving as an argument is invisible to
it — so it would not permit enabling the tier by default. But it would make the tier simultaneously
useful and far harder to trip, which neither current setting manages, and it would make the
benchmark numbers mean something in the meantime.

*Process note:* R33 was published as a finding after the narrowing but before the mechanism was
established. The narrowing was sound and is preserved above; the conclusion was a guess dressed as
a result. The tell was available at the time — "correct by hand, wrong through the interpreter" has
at least two explanations, and only one of them had been checked.

**R35. The continuation guard is now a call-graph closure. The tier compiles 88% of what the unsound
rule compiled, and both known unsound shapes are caught.**

Increment 2b′, the interim before re-enterable frames. `src/compiler/safety.js` decides which
definitions a continuation could be captured inside, by closing the control-global rule over the
call graph rather than over one procedure's text or over the whole unit.

A procedure is declined if it can reach a control global: directly, through another definition in
the same unit, through an interpreted closure already in the environment (which reaches into the
standard library), or — under `strict`, the default — by calling a callee it cannot name, because a
parameter may be anything. Reasons are paths, which makes them checkable by a reader:

```
run -> pmaze -> make-maze -> dig-maze -> references 'call-with-current-continuation'
```

**Measured on the canonical suite**, definitions compiled out of 754:

| rule | compiles | sound against `maze` | sound against `btsearch` |
|---|---|---|---|
| per-definition (`tryCompileDefinition`) | 425 | no | no |
| reachability, `strict: false` | 418 | **yes** | no |
| **reachability, `strict: true`** | **375** | **yes** | **yes** |
| whole-unit veto (`compileProgram`, before) | **0** | yes | yes |

So the cost of catching both shapes is **12% of compiled definitions** against the unsound rule, and
the gain against the sound one is everything, because the sound one compiled nothing. R28's "unsound
or vacuous" no longer describes the tier.

`maze` now returns the right answer — the last wrong answer on the canonical suite — at 1.42x with
46 of its 69 definitions compiled. `fib` is unaffected at 26.5x.

**What this is not.** It is not soundness, and it must not be read as permission to enable the tier
by default. A global rebound *after* compilation to something that captures is invisible to an
analysis that ran before it, and compiled code resolves globals through a live accessor, so it would
call the new binding. That hole is closed only by re-enterable frames — increment 2b — which remains
the real fix and makes this module unnecessary. There is a test asserting this limitation so that
nobody mistakes the analysis for a proof.

*Consequence for the numbers:* both benchmark harnesses now compile through this guard, so figures
taken from here on are measured in a configuration that is at least defensible. The macro transfer
test moved 1.39x → **1.43x**, which is to say the unsound rule was not buying anything on real code
either.

*Consequence for the tests:* `CONTINUATION_CASES` previously asserted that the whole unit was
declined and that **nothing** was compiled. That contract was satisfiable only by a rule that never
compiles anything, which is how it hid R28. It now names, per case, the procedures that must be
declined — and deliberately does *not* list `fail` in the backtracking case, because `fail` captures
nothing, reaches nothing that does, and compiling it is correct.

**R36. The compiler cannot lower a named `let`, a `do` loop, a `letrec` or a `case`. This, not the
continuation guard, is the largest cause of low compilation coverage.**

> [!NOTE]
> **Diagnosis corrected by R38.** The finding — that these forms cannot be lowered, and that this
> dominates the guard as a cause — is right. The framing, "extend the IR over `ScopedVariable`", is
> wrong: it is not a missing syntactic case but deferred hygiene resolution, and the fix belongs in
> the expander, not the IR.

Found while tuning R35's guard, by checking a suspicion instead of acting on it. `sum` compiles none
of its hot code and returns 1.00x under every configuration tried; the guard looked responsible.
It is not. `lowerLambda` reports `unsupported node: ScopedVariable` for all of these:

| form | lowers? |
|---|---|
| plain recursion, `let`, `let*`, `cond`, `when`/`unless`, `and`/`or`, internal defines | yes |
| **named `let`** | **no** |
| **`do`** | **no** |
| **`letrec`** | **no** |
| **`case`** | **no** |

Across the canonical suite, definitions the compiler refuses for reasons *other* than the guard:

| reason | definitions |
|---|---|
| **`unsupported node`** | **150** |
| not a procedure (a `define` of a value) | 129 |
| names a control global | 50 |

So `sum`'s loop was never compiled, `lattice`'s `case` dispatch was never compiled, and the
`fixnum` class's 1.00x has been measuring the interpreter all along. Named `let` and `do` are among
the most common loop forms in Scheme, and `letrec` is what several macros expand into.

*Consequence:* extending the IR over `ScopedVariable` is now the highest-value coverage work, ahead
of anything in code generation, and it should be done before the per-class table is read as evidence
about what compilation is worth. It is queued in `ROADMAP.md` as increment 2c.

*Process note:* this is the second time in two days that a plausible cause was nearly recorded as a
finding without being checked (see R34). The check took one script: lower eleven common forms and
print which ones fail.

**R37. The guard's cost is real reachability, not false positives.**

Two refinements were made to R35 after measuring, both aimed at *not* declining things needlessly:

- A call to a local bound to a lambda in the same lowering — what a named `let` and an internal
  procedure definition produce — is a callee the pass can name, so it no longer counts as unknown.
  Knownness is computed during lowering and carried on the IR node, so it follows through `if`,
  `seq`, `let` and `letrec` rather than being a special case at the call site.
- A local that is both called and `set!` is unknown again, because the binding that was lowered is
  not necessarily what the call reaches.

Neither changed the suite total, which is the useful part of the result: **375 of 425 both before
and after**, so the 12% the guard costs is genuine reachability rather than imprecision. The
breakdown of what it declines:

| reason | definitions |
|---|---|
| calls a callee it cannot name | 95 |
| reaches a control global transitively | 51 |
| names one directly | 50 |

**R38. Increment 2c: hygiene resolution moved to expansion time, where the literature says it
belongs. Lowering is now unblocked completely — and that exposed a blind spot in the safety
analysis that had been flattering its coverage.**

R36 framed `ScopedVariable` as four missing cases in the IR. It is not a syntactic gap at all. The
analyzer creates one at a single site (`analyzer.js:229`) for a **free reference that still carries
its hygiene scope marks**, and `ScopedVariable.step` re-runs the sets-of-scopes resolution *on every
evaluation*.

Every system in `docs/hygiene.md`'s own reference list completes resolution during expansion —
Kohlbecker et al. (1986), Clinger and Rees (1991), Dybvig et al. (1992), Flatt (2016). The universal
pipeline is `expander → fully-expanded core language → compiler`, and the compiler never receives a
syntax object. That document already describes "Resolution" as step 3 of *The Expansion Process*.
Only the code disagreed.

Measured before changing anything:

| | |
|---|---|
| `ScopedVariable` evaluations across the whole 2,188-test suite | **3,966** |
| resolutions that found a scoped binding | **0** |
| identifiers involved | `car`, `cdr`, `list`, `memv` — ordinary stdlib globals from macro templates |

The misses are not luck. Locals are alpha-renamed, so a plain name can only denote a global, and the
runtime fallback was `env.lookup(name)` — exactly `VariableNode(name)`. Referential transparency was
verified to still hold under local shadowing.

So `analyzeVariable` now resolves at analysis time and emits a `VariableNode` on a miss. The path
that *does* resolve is left resolving at run time, deliberately: it has never been observed to fire,
and an unobserved path is not one to move on the strength of an argument.

**Result: the `unsupported node` category is gone.** Definitions the compiler can lower went
**425 → 573** of 754. Named `let`, `do`, `letrec` and `case` all lower.

**And the count of definitions actually compiled fell, 375 → 282.** That is not a regression; it is
the removal of a blind spot. `unsafeDefinitions` **skipped every definition it could not lower**
(`if (facts !== null) local.set(...)`), so 150 procedures were invisible to the reachability
analysis — they could neither be flagged unsafe nor propagate unsafety to their callers. R35's
"375, and both known unsound shapes caught" was true about those two shapes but rested on an
incomplete call graph. **282 is the honest number.**

The canonical suite remains fully correct, and per-class the tier is flat to slightly down: call
5.42x → 5.16x, list 1.13x → 1.09x, flonum 1.36x → 1.30x, fixnum 0.99x → 1.01x.

**What still blocks the coverage win**, and it is not hygiene any more: a named `let` expands to
`((letrec ((tag (lambda ...))) tag) val ...)`, and `letrec` expands by Petrofsky's list-based method

```scheme
(let ((var 'undefined) ...)
  (let ((temp (list init ...)))
    (begin (set! var (car temp)) (set! temp (cdr temp))) ...
    (let () . body)))
```

so the loop variable is bound to `'undefined` and receives its lambda through `(car temp)`. No local
analysis can see that the callee is the lambda two forms up — the laundering through a list defeats
it by construction. `sum`, `nqueens` and `puzzle` therefore still compile nothing.

*Two diagnoses were wrong on the way here and both were caught by measuring rather than reasoning:*
that `ScopedVariable` was a missing syntactic form (R36), and that the assignment-poisoning rule was
what declined named `let` — removing it changed the suite total by exactly zero. Third time in two
days (R33, R36, this). The measurement is cheap; the reasoning is not reliable.

**R39. `letrec` is a core form again, and it overturns R29. The compiler tier is not a
control-flow optimizer — it looked like one because every hot loop in the suite was uncompilable.**

Increment 2c′, taken as option (a): stop implementing `letrec` and `let` as library macros and let
the native analyzer handlers produce them, with `LetRecNode` rebuilt as a multi-binding node that
survives into the IR.

**The headline is a reversal.** R29 concluded, on measurements, that "the compiler tier is a control
flow optimizer, and five of seven workload classes are not control-flow-bound", and that value
representation must therefore precede further code generation. That conclusion was drawn while
named `let`, `do` and internal definitions **could not be compiled at all**, so the hot loop of
every fixnum, flonum and vector program in the suite was running interpreted. The tier was not being
measured; the interpreter was.

| Workload class | R29 / after 2c | after 2c′ |
|---|---|---|
| fixnum | 1.01x | **11.95x** |
| flonum | 1.30x | **8.94x** |
| vector | 1.01x | **4.32x** |
| list | 1.09x | **1.60x** |
| call | 5.16x | 5.55x |
| string | 1.05x | 1.24x |
| bignum | 1.11x | 1.11x |
| continuation | 1.06x | 1.06x |

Programs: `sum` 0.99x → **5.71x**, `nqueens` 0.97x → **27.0x**, `puzzle` 1.01x → **5.83x**,
`browse` 0.99x → **23.5x**, `array1` → **18.8x**. All 41 still correct.

*What survives from R29:* bignums stay flat at 1.11x, so "BigInt arithmetic dominates and code
generation cannot reach it" is right **for actual arbitrary-precision work**. What does not survive
is the generalisation to small-integer code — `sum` was the flagship example of it and now gets
5.71x. **The resequencing R29 justified — value representation ahead of codegen — is no longer
supported by its own evidence** and is withdrawn pending re-measurement. The worst classes are now
continuation (1.06x), bignum (1.11x) and string (1.24x).

*The interpreter got faster too*, which was not the goal: `lattice` 18.7 → 10.7 ms, `graphs` 1.00 s
→ 662 ms, `earley` 2.10 → 1.59 s, `destruc` 521 → 432 ms. That is the removed cost of the old
expansion — a `ScopedVariable` registry lookup per reference, plus allocating and walking a list to
deliver each lambda.

**Why this was a library/core boundary problem, not a language-choice problem.** `letrec` is a core
form in every serious Scheme — Chez, Racket, Guile — and their expanders are written in Scheme.
R7RS §4.2 *specifies* the derived forms by macro definitions, but implementations are free to
implement them natively, and the ones that compile well all do. Petrofsky's list-based `letrec` is a
**portability** technique: it achieves R7RS `letrec` semantics using only `let`, `set!` and list
operations, which is exactly what you want when your Scheme lacks `letrec` and exactly what you do
not want inside the implementation of one. Routing every lambda through `(list ...)` and
`(car temp)` erases the binding structure the compiler needs.

Compilers go further and deliberately *recover* that structure: Waddell, Sarkar and Dybvig, "Fixing
Letrec" (2005), classifies `letrec` bindings so unassigned lambdas become directly callable, and
Guile's `<fix>` node is that analysis. The multi-binding `LetRecNode` here is the same treatment.

**The rule this leaves behind**, and the answer to "what else has this problem": a macro may expand
into core forms, but it must not **encode binding structure in runtime data**. Audited across every
derived form, after this change: `and`, `or`, `let*`, `letrec*`, `cond` (including `=>`), `case`,
`when`, `unless`, `do`, named `let`, `let-values` and `delay`/`force` are all clean.
`parameterize` and `guard` are not, and correctly so — they involve continuations by nature.
`case-lambda` is genuinely higher-order. Only `define-record-type` is worth revisiting.

**Two latent bugs surfaced, both invisible while the macros shadowed the handlers:**

- `analyzeLetRec` read its body with `cdddr` instead of `cddr`, dropping the first body expression.
  Nothing had ever reached it.
- `analyzeLet` desugared a named `let` to `(letrec ((tag ...)) (tag val ...))`, putting the
  initializers **inside** the scope of the loop name. R7RS puts them outside —
  `((letrec ((tag ...)) tag) val ...)` — and the difference is observable: `(let - ((n (- 1))) n)`
  called the loop instead of negating. That is Al Petrofsky's pitfall 8.1, and
  `tests/core/scheme/r7rs-pitfalls.scm` caught it immediately.

Thirteen behavioural tests were written **before** the change, pinning R7RS `letrec` against
`letrec*` — `(letrec ((a 1) (b a)) b)` must not yield 1 — plus init ordering, mutual recursion,
named `let`, `do`, and `call/cc` inside an initializer. They passed against the macro
implementation first, which is what made them a contract rather than a description.

**R40. Increment 2b, first half: a capture that crosses a compiled frame is now refused instead of
answered wrongly. The tier's failure mode is no longer silent.**

Investigating 2b turned up a problem the Stage 2a prototype did not have, because that prototype was
compiled-only. In the real system the two tiers must share **one** continuation representation, and
they do not:

- A continuation *is* the interpreter's frame stack — `createContinuation(registers[FSTACK])`.
- Compiled procedures are not in it. They run in JavaScript stack frames.
- When compiled code calls interpreted code, `runWithSentinel` starts a nested run over
  `[...parent, SentinelFrame]`, and `filterSentinelFrames` drops that marker from any continuation
  copied out of it.

So a capture below the boundary produces a continuation from which **everything the compiled caller
had left to do is simply absent**. That is precisely how `maze` returned `#f` (R34) and `btsearch`
returned the wrong pair (R15). Both produced a plausible value rather than failing.

Full re-entry needs a capture protocol that propagates *outward* through the boundary before the
continuation can be built — the compiled caller must reify its own frame and return an unwind
sentinel, repeatedly, up to the outermost compiled entry, which then hands the collected frames to
the interpreter to splice in. That, plus a resumable twin of every emitted function, is the second
half and is a substantial piece of work.

This entry is the first half, and it is worth having on its own. The sentinel now records whether it
marks a *compiled* boundary, and `CallCCNode.step` scans for one before capturing. If it finds one
it throws, naming the cause. Measured on the reproduction from R34:

| | before | after |
|---|---|---|
| guard on | `(escaped tail)` | `(escaped tail)` |
| guard bypassed | **`escaped`** — silently wrong | refused, with an explanation |

*Why this matters beyond tidiness.* The call-graph guard of R35 is explicitly **not sound**: a
global rebound after compilation is invisible to it, and so is a callee that arrives as an argument
where the strict rule cannot see it. Those holes previously produced wrong answers. They now produce
errors. **The tier's remaining unsoundness has been converted from silent to loud**, which is the
difference between a bug you find and a bug you ship.

It does *not* let the tier be enabled by default. Refusing a valid R7RS program is not an acceptable
end state, so the guard still has to decline. That is what the second half buys.

*Also fixed on the way:* `filterSentinelFrames` matched on `constructor.name === 'SentinelFrame'`,
so any sentinel carrying extra information would have stopped being filtered and would have been
executed while restoring a continuation. It now matches on a property.

**R41. The resumable form of every compiled procedure now exists and is verified against the fast
form. Code size is 2.21x, not the 4.09x the bake-off predicted, and runtime speed is unchanged.**

The second half of increment 2b has two parts: a resumable copy of every procedure, and the capture
protocol that suspends into it. This entry is the first part.

**Why a second copy at all.** A compiled procedure is straight-line JavaScript, which is what makes
it fast and also what makes it impossible to re-enter half-way through — a JavaScript function
cannot be resumed at a statement in the middle of its body. So each procedure is emitted twice: once
in the fast form, and once as a state machine over its own call sites, entered as `($pc, $f)`. The
state machine runs only while a continuation is being reinstated, so it can be slow; the cost is
code size, paid once at compile time, rather than speed, paid on every call.

**Structure.** `src/compiler/resume.js` subclasses the fast-path emitter and overrides *only*
control flow. Everything about expressions — inlined primitives, global accessors, temporaries, rest
arguments — is inherited, so the two forms cannot drift apart in what they compute. Making that work
needed one extraction: `ProcedureEmitter` moved to `src/compiler/emitter.js` so that `codegen.js` and
`resume.js` could both import it without a cycle.

Two things had to differ beyond `if` and calls, and both were found by running the twin rather than
by reading it:

- A **rest parameter** must not be rebuilt from a JavaScript argument array. The twin takes no
  argument list; everything arrives in `$f`, already converted.
- A **nested procedure** must be emitted as an assignment, not a declaration. A function declaration
  inside a `switch` case only takes effect when that case runs, and resuming jumps straight to a
  later block — so the name would have been undefined. As an assignment to a declared variable it is
  spilled and restored like anything else.

That second one matters more than it sounds. A `let` body, a named `let`'s loop and every anonymous
procedure become nested procedures, so **most call sites in a program are inside one**. Without
their own resumable forms, a continuation captured in the commonest place in a program could not be
resumed.

**Measured:**

| | |
|---|---|
| code size, fast form + twin against fast form alone | **2.21x** (the bake-off predicted 4.09x) |
| `fib` / `nqueens` / `sum` compiler-tier speedup | 26.98x / 28.27x / 5.72x — unchanged |
| tests | 2,216 passing |

The size came in better than predicted because the emitted code is already factored into nested
procedures, so a twin duplicates a body rather than a whole monolithic state machine.

**Verification.** Ten differential cases run each procedure's fast form and its twin from block 0
and require the same answer — recursion with two call sites, tail recursion, named `let`, nested
conditionals, allocation, a call in a `let` initializer, `let*`, mutual recursion, and a rest
parameter with and without extra arguments. Entered at block 0 with its arguments in a frame, a twin
is just another way to call the same procedure, which makes this a cheap and direct check that the
two copies agree.

**Still to come**, and it is what actually turns the tier on: the capture protocol. `call/cc`
currently refuses when it finds a compiled frame. It must instead begin an unwind — returning the
sentinel out through the compiled/interpreted boundary, each compiled frame reifying itself on the
way out, up to the outermost compiled entry, which splices the collected frames into the
interpreter's stack as frames that resume through `$resume`. The runtime side of that (`UNWIND`,
`reify`, the frame record) is in place and the generated code already calls it; nothing produces an
`UNWIND` yet.

**R42. The capture protocol is built on the interpreter side and stops one step short on the
compiler side, for a reason worth recording.**

The design, now implemented in `src/core/interpreter/unwind.js`:

1. `call/cc` finds a compiled boundary beneath it, records what it needs to finish later
   (`{lambdaExpr, fstack, env, boundary}`), and throws `CaptureUnwind` to abandon the nested run.
2. `run` catches it and returns the `UNWIND` sentinel to whoever called in — compiled code.
3. Compiled code checks for `UNWIND` after every non-tail call, spills its locals and resume point
   with `reify`, and returns `UNWIND` so its own caller does the same.
4. `continueApplication` sees `UNWIND` from a compiled callee, splices the reified frames in where
   the boundary sat — outermost first, since the interpreter pops from the end — and applies the
   receiver to a continuation built from the completed stack.
5. Reinstating runs those frames through `CompiledFrame`, which calls the procedure's resumable form
   with the slots **copied**, so a second invocation cannot see what the first assigned.

Steps 1, 2, 4 and 5 are implemented and the protocol is layered correctly: the interpreter owns it,
and `src/compiler/runtime.js` only re-exports `UNWIND` and `reify` so generated code can reach them.

**Step 3 is not finished, and the obstacle is naming.** The resumable form restores its locals with
`({ a, b, $t7, ... } = $f)`, so the fast form must spill under *those* names. It does not: temporaries
are allocated from a counter shared across both emissions, so the fast form holds a value in `$t3`
where the twin expects `$t13`. The two forms traverse the same IR in the same order and would agree
on names if each allocated temporaries from its own counter — but `branch()` and the value-position
`if` create sub-emitters that must share their parent's counter, and a nested procedure must not
share it, so this is a small, careful change rather than a one-liner.

Until it lands, a compiled procedure that sees `UNWIND` **throws**, naming the limitation. That
keeps the previous guarantee: the tier's remaining unsoundness is loud, never silent. An
intermediate state where the sentinel leaked into a result was briefly present during this work and
was caught by running the escape case -- `(Symbol(scheme.unwind) tail)` where `(escaped tail)` was
expected -- which is exactly the failure mode all of this exists to prevent.

*What is verifiably true now:* 2,216 tests, 219 chapter-conformance tests and 982 chibi-conformance
tests pass; the compiler tier's measured speedups are unchanged (`fib` 19.4x, `sum` 5.78x, `maze`
2.36x); and every piece of the protocol except the fast form's spill is in place and layered so that
the interpreter does not depend on the compiler.

**R43. The capture protocol is finished, and relaxing the guard it was built to replace turned out
to be a performance decision rather than a correctness one.**

Step 3 of R42 — the fast form spilling its locals under the names the resumable form restores — is
done, and the naming problem was worse than R42 described. Two things had to line up, not one:

*Temporary numbering.* Each **emission** now counts from zero (`procedureScope` in
`src/compiler/emitter.js`), so the fast form and the resumable form reach the same name for the
same value. Sub-emitters for a branch or a value-position `if` share their parent's counter; a
nested procedure gets a fresh one. The first attempt shared one counter object between a
procedure's two emissions, so the second continued the first's numbering — visible immediately as
`$t4` in one form against `$t0` in the other.

*Procedure naming.* Counting from zero per procedure made nested procedure names **collide across
nesting levels**: every first nested lambda was `$fn0`, including one directly inside another. A
procedure names its own resumable form when it suspends, so a nested `$fn0$r` shadowed exactly that
reference and a frame reified into the wrong twin — resuming a different procedure at a block
number that meant nothing there. Names are now paths (`$fn2_0` is the first procedure inside the
third), which is unique by construction without a shared counter.

*The frame layout is the resumable form's to decide.* It is generated first, and records both the
resume block per call site and the final set of slot names; the fast form reads both. Having the
fast form derive the names itself would have been a second implementation of the same rule, which
is the kind of agreement that holds until it quietly does not.

**A third bug was found by testing, not by reasoning.** `CompiledFrame.step` drives the resumed
procedure to completion, and a capture can happen *during* that — a loop that captures on every
turn does it every time. It set `ANS` to the sentinel and returned, on the stated assumption that
whoever called it would finish the capture. Nothing did, and the sentinel flowed into an ordinary
frame as a value. `CompiledFrame` now completes the capture itself, which is correct because the
frame has already been popped and its remaining work went into the reified frames on the way out.
It moved to `frames.js` to do so, and `completeCapture` takes the interpreter's vocabulary —
building a frame, a continuation, an application — as a hooks object, so `unwind.js` still knows
nothing about the compiler.

**Then the measurement overturned the plan.** The roadmap said "align temporary naming, then
enable", meaning relax the safety guard. With the guard fully off, all eight continuation
benchmarks are **correct** — including `btsearch`, the shape that motivated the guard in the first
place. But `btsearch` ran at **0.50x**, half the interpreter's speed. A procedure that a capture
repeatedly unwinds through pays to suspend and resume on every capture, and that costs more than
interpreting it.

So the guard survives, with its justification replaced. What was removed is `strict` — the rule
declining any procedure that calls a callee it cannot name, which existed *solely* to catch the
`btsearch` shape and is now redundant. It is off by default:

| policy | `btsearch` | `oddeven` | `threads` | correct |
|---|---|---|---|---|
| reachability + `strict` (the old default) | 1.00x | 1.61x | 1.05x | yes |
| **reachability only (the new default)** | **1.82x** | **2.56x** | **1.11x** | yes |
| nothing declined | 0.50x | 2.81x | 1.17x | yes |

The reachability closure, written as a soundness device, turns out to be a decent *performance*
heuristic: do not compile what a capture will unwind through. That is not why it was built, and it
is worth being explicit that its original purpose is gone — a reader who assumes the guard is load
bearing for correctness will draw the wrong conclusion about what is safe to change.

**What is still refused rather than answered:** a capture crossing more than one boundary between
compiled and interpreted code, and a capture beneath a *redefined* inlined primitive — an inline
expansion is not a call site the resumable form splits at, so there is no point to resume from.
Both throw with an explanation. The inline fallback also went through `invoke`/`settle` in this
change, closing a path where a redefined primitive that was an interpreted closure had its
arguments converted at the JavaScript boundary and its tail calls left undriven.

*Verified:* 2,234 tests, 219 chapter-conformance and 982 chibi-conformance tests pass; nine capture
shapes match the interpreter, including a continuation invoked twice, a capture inside a
`dynamic-wind`, and a capture on every turn of a compiled loop; `fib` 15.2x, `nqueens` 13.3x,
`btsearch` 1.82x, all eight compiled benchmarks correct.

**R44. Emitting every procedure twice costs 2.06x *per level of nesting*, not 2.21x overall. R42's
code-size measurement was taken on programs too shallow to show it.**

R42 reported the twin at 2.21x code size against a predicted 4.09x, and put the happy result down
to the emitted code already being factored into nested procedures. That reading was wrong, and it
was wrong because every case measured was two or three levels deep.

A procedure's fast form emits both forms of each procedure nested inside it, and so does its
resumable form. A lambda at nesting depth *d* is therefore emitted **2^d times**. Measured on a
chain of `let`s, each of which lowers to a nested procedure:

| depth | generated characters | ratio |
|---|---|---|
| 0 | 409 | |
| 4 | 24,658 | 2.16 |
| 8 | 462,386 | 2.06 |
| 12 | 8,349,234 | 2.07 |

2.21x was the ratio at the depth those cases happened to reach. The real figure is an exponent.

Seven of the 52 canonical benchmarks — `earley`, `equal`, `graphs`, `nucleic`, `parsing`, `peval`,
`simplex` — exceed JavaScript's maximum string length while the source is still being assembled.
That surfaced only when compiling every definition in each program; under the default decline policy
those procedures are held back for other reasons and the limit is never reached. It was found by
sweeping the whole benchmark corpus through the compiler, not by running it.

Worse than the size was the failure mode: the exception escaped `compileProgram`, so one
over-large procedure **aborted compilation of the entire program** rather than being declined.
A compiler that throws part-way through cannot be run across a unit at all. It now declines per
definition, which is the difference between 567 and 818 definitions compiled across the corpus.

The cap is a containment measure, not a fix. Emitting each nested procedure once, with its free
variables passed in rather than closed over, would make this linear -- that is lambda lifting, and
it is a change to how closures are generated rather than a limit to be tuned. Until then, a
procedure nested deeply enough is interpreted.

**R45. The tier's weakness on symbolic code was mostly an interpreted standard library, and every
figure used to argue against self-hosting was measuring that instead.**

`src/compiler/ir.js` was ported to Scheme (`experiments/ir_in_scheme/`) to settle the
self-hosting question with a measurement rather than an inference from two benchmarks. The port is
close rather than idiomatic — same traversal, same scope discipline, same mutable state — so the
comparison is between two implementations of one algorithm.

Correctness first: every `define` of a procedure across all 52 canonical benchmarks and the
standard library, 952 lambdas, lowered by both implementations and compared field by field
including the globals set and the `callsUnknown` flag. All three Scheme configurations produce
**identical IR on all 952**.

| | per pass | vs JavaScript |
|---|---|---|
| JavaScript | 3.8 ms | 1.0x |
| Scheme, interpreted | 1162 ms | 305x |
| Scheme, compiled by the tier | 808 ms | 212x |
| **Scheme, + standard library compiled** | **75 ms** | **19.6x** |

**The third row and the fourth differ by 10.8x, and only the standard library changed.** `ir.scm`
calls `memq` and `assq` on every scope lookup and every global it records, and those are themselves
Scheme, in `src/core/scheme/list.scm`. With them interpreted, a compiled module crosses into the
interpreter on its hottest path and the tier is worth 1.44x. With them compiled, the tier is worth
**15.5x on the same code**.

That invalidates the reasoning recorded in ROADMAP's *Revisiting the Scheme-port decision*, and the
argument built on it in this session: that `peval` at 1.21x and `scheme` at 1.30x showed the tier
to be weak on the symbolic work a compiler does, and that self-hosting was therefore off the table
until code generation improved. Those programs are not slow because code generation is weak on
symbolic code. They are slow because they spend their time inside an interpreted standard library.
The `list` class at 1.92x against `vector` at 17x is the same artefact, not a property of the
workload.

The error has a shape worth naming, because it is the third time in this log: a number was
attributed to the thing being studied without checking what else differed between the two sides
being compared. R29 did it with hot loops that could not be compiled, R28 with the unsound guard,
and this with the library underneath. In each case the measurement was real and the attribution was
wrong.

**Consequences.** AOT-compiling the standard library was roadmap item 5a, ranked eighth, on the
grounds that mixed-tier crossing was "less acute than R30 judged it". On symbolic code it is worth
about 10x, and it should be re-measured across the whole canonical suite before anything else is
prioritised — every class figure in this document is suspect for the same reason.

On self-hosting itself the experiment gives a number rather than a verdict: a Scheme-hosted
lowering is **19.6x** slower than the JavaScript one with everything compiled, and 305x with the
tier off. Whether that is affordable depends on what the compiler is asked to do — 75 ms to lower
952 procedures is not a REPL problem, while the interpreted fallback at 1.16 s is. Two costs the
port surfaced that a rewrite in the abstract would not have: the tier declines any procedure using
`apply` or `values`, so self-hosting means writing in the accepted subset; and R7RS-small has no
hash tables, so sets are association lists. Neither was decisive on this corpus.

**R46. Compiling the standard library, and the one-line change that unblocked most of the corpus.**

R45 predicted about 10x on symbolic code from compiling the library. Measured across the whole
canonical suite, by workload class, tier against interpreter:

| class | before | after | |
|---|---|---|---|
| `list` | 1.92x | **4.61x** | 2.40x better |
| `call` | 5.70x | **12.47x** | 2.19x better |
| `continuation` | 1.00x | **2.86x** | 2.86x better |
| `fixnum` | 10.52x | 10.01x | unchanged |
| `flonum` | 2.95x | 2.75x | unchanged |
| `vector` | 17.12x | 16.46x | unchanged |
| `bignum` | 1.07x | 1.07x | unchanged |
| `string` | 1.19x | 1.05x | unchanged |

Exactly the three classes that spend their time in the library moved; the numeric classes, which
do not, did not. Individual programs: `divrec` 1.40x → 17.74x, `destruc` 1.73x → 16.91x, `dynamic`
1.00x → 8.10x, `lattice` 1.54x → 11.18x, `mazefun` 3.40x → 19.30x, `peval` 1.21x → 6.96x,
`scheme` 1.30x → 6.54x.

**`peval` and `scheme` are the two figures R45 said were measuring the wrong thing, and they moved
by 5.8x and 5.0x.** They were used, in this session, to argue that the tier was weak on the
symbolic work a compiler does and that self-hosting was therefore not viable. Neither claim
survives.

**The larger part of the win was not the library itself.** Compiling it reached only 49 of 61
procedures, and the twelve it missed were `map`, `for-each`, `vector-map`, `string-map`, `max`,
`min`, `gcd` and `lcm` -- the ones that matter most. All twelve were declined for the same reason:
they reference `apply`.

`apply` was on the control-global list because it returns a `TailCall` rather than a value. That
was the wrong reason. It transfers control to an ordinary procedure with ordinary arguments, which
a compiled trampoline can continue perfectly well. What actually blocked it was the *shape* of the
`TailCall` it returned: `TailCall(expression, environment)`, for the interpreter to evaluate, where
compiled code has no evaluator and expects `TailCall(procedure, arguments)`. Both shapes were
already accepted by `continueApplication`, so returning the second was a one-line change to
`applyPrimitive`.

A decline-reason histogram over the corpus is what found it, and it should have been run long ago
-- it has been a roadmap item since increment 1 and the ratio has been printed since then without
anyone acting on it. Of 466 declines, 229 were top-level definitions that are not procedures at
all, and of the ~237 real ones the overwhelming majority traced to `apply`, mostly indirectly
through `map` and `for-each`. Removing it took corpus coverage from **623 of 1089 to 807 of 1089**,
and library coverage from 49 of 61 to **61 of 61**.

The remaining declines are now: 20 too-large generated source (R44), 20 reaching `call/cc`, 13
reaching `values`/`call-with-values`.

**Four programs got slower**, and this is worth recording rather than averaging away: `earley`
1.00x → 0.87x, `takl` 25.66x → 20.93x, `sum` 5.62x → 4.05x, `sumfp` 3.35x → 2.17x. `earley`
compiles only 4 of 8 definitions, so its interpreted procedures now call *compiled* library code
and pay the boundary in the other direction. The tier-crossing cost is symmetric, and compiling
more of the library makes partially-compiled programs worse. That is an argument for raising
coverage further, not for reverting.

**Where it runs.** `compileEnvironment` compiles procedures where they already sit, which is the
only way to reach a library that exists as values by the time anything considers compiling it. It
is on by default in `src/packaging/scheme_entry.js`, costing about 22 ms of a 76 ms bootstrap. It
probes `new Function` once and, where a Content-Security-Policy forbids code generation, reports
that and leaves everything interpreted -- correct and merely slower, which is what the interpreter
being a permanent tier is for.

*Verified:* 2,272 tests pass; the full suite passes again with `SCHEME_AOT_STDLIB=1`, as do both
conformance suites -- 219 and 982 -- against a compiled library, which is the strongest correctness
evidence available for the tier, since the library is the most heavily exercised code in the system.

**R47. The code-size blow-up was not a closure-conversion problem. It was `let`.**

R44 measured generated code doubling with every level of lambda nesting and concluded that the fix
was lambda lifting -- emitting each nested procedure once with its free variables passed in. The
roadmap carried that as the second priority. It was the wrong fix, because it was aimed at the
wrong cause.

**The analyzer expands every `let` into an immediately-applied lambda.** A chain of bindings is
therefore a chain of nested procedures, and `let*` produces one level per clause. The deepest
procedure in the canonical corpus was **29 levels**, of which almost none were closures in any
meaningful sense -- they were bindings wearing a lambda.

Reducing `((lambda (a b) body) x y)` back to bindings during lowering (`lower-direct-application`
in `src/compiler/ir.scm`, then `lowerDirectApplication` in `src/compiler/ir.js`) is sound because the operator is a literal lambda applied exactly there: it
cannot be called from elsewhere and nothing can capture it. The only part needing care is that the
body inherits the *call's* tail position rather than being a procedure body, so that a call in the
body produces a value when the caller wants one.

| | before | after |
|---|---|---|
| deepest nesting in the corpus | 29 | **7** |
| nested-`let` growth per level | 2.06x | **quadratic overall**: depth 40 is 127 KB |
| depth-12 binding chain | 8.3 MB | 25 KB |
| declined for over-large source | 20 | **0** |
| corpus coverage | 807/1089 | **827/1089** |

It is also a straightforward speed win, since each reduced binding removes a closure allocation and
a call. Every workload class improved and none regressed:

| class | AOT only | with the reduction | |
|---|---|---|---|
| `list` | 4.61x | **9.85x** | 2.14x |
| `flonum` | 2.75x | **8.26x** | 3.01x |
| `call` | 12.47x | **14.28x** | 1.15x |
| `fixnum` | 10.01x | **11.41x** | 1.14x |
| `vector` | 16.46x | 16.66x | 1.01x |
| `continuation` | 2.86x | 2.95x | 1.03x |
| `bignum` | 1.07x | 1.22x | 1.14x |
| `string` | 1.05x | 1.11x | 1.06x |

`earley` went **0.87x → 21.41x**, which is the R46 regression removed and then some -- its coverage
rose from 4 of 8 definitions to 6 of 8. `paraffins` 1.38x → 23.33x, `fft` 1.00x → 12.21x, `mbrotZ`
1.05x → 13.10x, `simplex` 1.14x → 13.59x, `graphs` 5.50x → 22.80x, `peval` 6.96x → 16.11x. Four
programs moved down by 4-11% (`fibfp`, `tak`, `array1`, `ack`); re-run at a five-times-longer
measurement target they are unchanged, so that is run-to-run variance on short benchmarks.

**Lambda lifting was then measured rather than built, and the measurement says not yet.** Of 799
genuinely nested lambdas in the corpus, 533 could be lifted to a top-level factory taking their free
variables, 238 are `letrec` initializers that refer to themselves or their siblings, and 28 close
over a variable that is assigned -- the last two being unliftable without boxing or a group factory.

But the liftable ones only reach **depth 5**, and everything at that depth compiles comfortably. The
worst remaining case is `earley.scm:make-parser` at **3.6 MB of generated source** -- 11% short of
the 4 MB bound -- and its depth-7 nesting is `letrec`-bound, so the simple lift would not touch it.
Lifting the 533 easy cases would reduce total generated source (24.4 MB across the corpus) without
addressing the one procedure that is actually near the limit.

So the residual is real but narrow, and the fix that would address it is specifically
**letrec-aware** lifting: a single-binding `letrec` -- which is what a named `let` is -- can be
lifted by declaring its name inside the factory and assigning the procedure to it before returning,
so self-reference resolves lexically. Mutual recursion needs a group factory. That is worth doing
when a program is actually declined for size, and it is worth knowing that the cheap version of it
would not have helped.

**R48. `values` was never a control operation; `call-with-values` needed rewriting rather than
implementing; and the boundary between the tiers was dropping values on the way out.**

Three things, one of which was a bug.

**`values` did not belong on the control-global list at all.** The primitive builds a `Values`
object and returns it. It transfers control nowhere and needs no interpreter. It was declined
because it sits beside `call-with-values` in the same file.

**`call-with-values` genuinely could not be called from compiled code**, and neither of the obvious
fixes works. The primitive returns a `TailCall` carrying an expression for the interpreter to
evaluate, which compiled code has no evaluator for -- the same shape problem `apply` had in R46.
But unlike `apply`, it cannot simply return a procedure-shaped `TailCall`, because it has to call
the producer *first* and then do something with the result. Making it a plain Scheme procedure has
the mirror-image problem: the pending consumer application would sit in a JavaScript frame that a
captured continuation could not restore.

So it is rewritten during lowering into `(apply consumer (%values->list (producer)))`. Every part is
something the compiler already emits, and the producer call becomes an ordinary call site -- which
is what gives a capture inside the producer somewhere to resume, for free, rather than by new
machinery. `%values->list` is a new primitive whose only subtlety is that a non-`Values` result
counts as exactly one value, including the unspecified value, because that is what
`CallWithValuesFrame` does and the two tiers have to agree.

The rewrite fires only on a direct two-argument call, and deliberately does not record the name as
referenced -- so `call-with-values` stays on the control-global list and a reference to it by any
other route still declines. That is asserted as a test, because the primitive would break a
compiled trampoline if it were ever reached.

**The bug.** `unpackForJs` collapsed a `Values` to its first value *before* checking the conversion
mode, so it did so even in `raw` mode -- which is the mode the compiled/interpreted boundary uses.
An interpreted producer returning two values handed compiled code the first one, silently:
`(call-with-values p +)` returned 4 where the interpreter returned 9. Collapsing several values to
one is a JavaScript-interop behaviour, because a JavaScript caller can only receive one, and `raw`
means the caller is not one. This is the third defect found at that boundary, after R26's numeric
conversion and R46's `apply` shape, and all three had the same character: a conversion applied
where no conversion was wanted.

It was found by a test asserting the two tiers agree, not by a benchmark -- the benchmarks were
passing.

**Coverage and effect.** 827 → **839 of 1089**. The entire remaining decline list is 229 top-level
definitions that are not procedures and 21 reaching `call/cc`.

| class | before | after | |
|---|---|---|---|
| `call` | 14.28x | **22.01x** | 1.54x |
| `fixnum` | 11.41x | **15.22x** | 1.33x |
| `flonum` | 8.26x | **10.93x** | 1.32x |
| `list` | 9.85x | **10.49x** | 1.06x |
| `string` | 1.11x | 1.28x | 1.15x |
| `continuation` | 2.95x | 3.11x | 1.05x |
| `vector` | 16.66x | 16.05x | 0.96x |
| `bignum` | 1.22x | 1.21x | 1.00x |

The `call` class moving by 1.54x from *multiple-values* support wants explaining. The canonical
suite's shared prelude defines `hide`, the idiom that stops a compiler folding a benchmark's input
away, and it is written with `call-with-values`. All 51 programs reference it. One declined
procedure in `common.scm` was holding down every program that called it. `earley` now compiles
**8 of 8** definitions at 23.65x.

**A negative result worth as much as the rest.** R29's surviving claim was that bignum performance
is BigInt-bound and code generation cannot reach it, and the roadmap has carried "profile bignums"
on that basis. It was tempting to think otherwise once `pi` turned out to compile 0 of 9
definitions, all of them blocked by `values` -- a coverage explanation for what had been read as an
arithmetic one, which is exactly the mistake R45 and R46 were. But `pi` now compiles **9 of 9** and
measures **1.00x**. The coverage explanation is wrong and R29's claim stands: for this class the
arithmetic really is the work.

**R49. `call/cc` compiles, and building it exposed a soundness bug that had been in the tier since
the capture protocol landed: a spilled frame copied assigned locals instead of sharing them.**

Three findings, in the order they arrived.

**First, `read1` was a real defect and not a harness problem.** It was the only canonical benchmark
whose compiled run produced no answer, failing with "read: port is closed", and it had been deferred
for several increments. `call-with-input-file` read `try { return proc(port); } finally {
port.close(); }`. A *compiled* procedure signals a tail call by returning a `TailCall` rather than a
value, so when `proc` ended in a tail call the port closed before the call ran. Four io primitives
had the shape; `settleTailCalls` in `values.js` fixes them. `read1` now compiles 4 of 4 and answers,
so every canonical benchmark is correct under the tier.

**Second, compiled `call/cc` works.** A capture is emitted as a call site that suspends: the
procedure records what the capture needs, spills its locals, and reports the unwind outward. That is
the protocol a capture made by an interpreted callee already used, entered from this end instead of
beneath, so it needed no new runtime machinery -- `beginCompiledCapture` records no frame stack and
no boundary, and `completeCapture` reads the interpreter frames from the registers and puts the
compiled frames directly inside them. The captured value arrives at the resume point rather than
from the call, so everything after the capture is emitted once and serves every invocation.

Building it turned up a **latent bug in the resumable form**: `statement` intercepted every `if`
node and emitted it as a *tail* `if`, including ones whose value is discarded. The fast form
allocates a temporary for such an `if`'s result and the resumable form did not, so the two
disagreed about which temporary held what -- a frame spilled by one was restored wrongly by the
other. It was invisible while every capturing procedure was declined.

**Third, and the important one: a spilled frame copied assigned locals.** `CompiledFrame` copies
its slots, and the note added with it said that copying is "what makes a continuation multi-shot
rather than one-shot". That reasoning was wrong. In Scheme a continuation *shares* the environment,
so an assignment made after a capture is visible when the continuation is invoked again, and to any
closure over the same variable. Copying is right for temporaries, which are always written before
they are read, and wrong for a variable the program can name. Reduced:

```scheme
(define (f) (let ((n 0)) (capturer) (set! n (+ n 1)) n))
```

driven three times through the captured continuation, the interpreter answers `(3 2 1)` and compiled
code answered `(1 1 1)`. The `threads` benchmark has the same shape in a counter shared between a
scheduler and the threads it runs, and returned a wrong total -- which is how this was found, by a
benchmark's own correctness check rather than by any test.

This was **reachable in the default configuration** and not only with `call/cc` compiled: it needs a
capture in the dynamic extent of a compiled procedure that assigns a local, and `strict` being off
means a capture arriving through an unknown callee is not declined.

Declining such procedures was measured first and is far too expensive -- only 13 of 891 lowerable
definitions assign a local, but they are in hot loops:

| class | copying (unsound) | declining | **boxing** |
|---|---|---|---|
| `vector` | 16.05x | 4.40x | **15.19x** |
| `flonum` | 10.93x | 6.96x | **10.18x** |
| `list` | 10.49x | 8.54x | **10.14x** |
| `fixnum` | 15.22x | 14.89x | **16.18x** |
| `call` | 22.01x | 22.09x | 21.79x |
| `string` | 1.28x | 1.11x | 1.22x |

So an assigned local is held in a one-element array and the frame copies the array's *reference*,
which is what an environment does in the interpreter. That costs 2-7% against the unsound baseline
rather than the 20-73% declining cost, and it is correct. Only assigned locals are boxed; an
unassigned one cannot tell a copy from the original.

**`call/cc` is nonetheless declined by default, on measurement.** Compiling a capture means every
capture unwinds and reifies the frames between it and the interpreter, and a program that captures
in a loop pays it each time: `btsearch` goes from 2.00x faster to **2x slower**, `ctak` from 0.99x
to 0.69x. Two programs improve -- `contfib` 1.03x to 1.92x, `threads` 1.12x to 1.66x -- so it is a
default rather than a rule, lifted by `allowCaptures`. By the project's own Pareto ship rule
(improve one class, regress none) it does not qualify, so the capability ships tested and off.

That means this increment **raised coverage by nothing**: 839 of 1089, unchanged. What it delivered
was a correctness fix, a soundness fix, and a capability waiting on a reason to enable it. Worth
stating plainly, because the increment was chosen on the expectation of the 21 declines going away.

**R50. The standard library is compiled at build time. The time saving is small; the point is that
nothing generates code at run time.**

`scripts/generate_compiled_stdlib.js` writes `src/packaging/compiled_stdlib.js`: one factory per
library procedure, holding the JavaScript the compiler used to produce at startup. Which procedures
are compiled is decided by `generateEnvironment`, the same function the runtime path uses, so the
bundle cannot disagree with the runtime about what was compiled.

**What it is worth, measured rather than predicted.** I expected about 20 ms of a 71 ms bootstrap.
The real figures: compiling at run time is 12 ms (median of six), installing prebuilt code is
**0.1 ms**, the fingerprint check is 0.5 ms, and importing the 736 KB generated module costs 6.9 ms.
So the saving is roughly **5 ms**, not 20. Production bootstrap went from ~72 ms to ~67 ms.

The two things that do matter are not about time:

- **Nothing calls `new Function` at run time.** With `Function` made to throw, 61 procedures still
  install and runtime compilation correctly reports itself unavailable. A page with a strict
  Content-Security-Policy now gets the *compiled* library, where before it got an interpreted one.
- **Compile speed is decoupled from deployment**, which is the precondition for writing the compiler
  in something slower than JavaScript. A Scheme-hosted compiler at 17x would have added ~200 ms to
  every startup; now it adds nothing, because nothing compiles at startup.

**The cost is bundle size, and it is substantial.** `dist/scheme.js` goes from 773 KB to **1513 KB**,
or 207 KB gzipped against about 172 KB -- so +740 KB raw and +35 KB over the wire. Generated code
compresses about 20:1, which is why the gzipped figure is tolerable and the raw one is not pretty.
Four procedures account for 35% of it: `map` 67 KB, `vector-map` 59 KB, `for-each` 56 KB,
`string-map` 55 KB. They are large for the reason R47 identified and deferred -- nested closures are
emitted in both forms of each parent, so a `letrec` nested three deep multiplies its bodies by about
sixty. Letrec-aware lambda lifting would cut this directly, and bundle size is now a second reason
to do it besides the 4 MB generation cap.

**A staleness guard, and a mistake in the first version of it.** Prebuilt code that no longer matches
its source would be the worst kind of wrong, so the table records a fingerprint of the library
sources and installs nothing if it does not match what was loaded.

The first version also compared each procedure's *renamed parameter names* against the live closure's,
on the theory that a mismatch meant the code and the source disagreed. That was wrong twice over.
Useless, because generated code names locals only inside itself -- its only external references are
`globalAccessor(E, "name")`, `currentBinding` and `E.set`, and all three use the source name, never a
renamed one; I checked, and there are zero renamed global references in the generated module. And
harmful, because renaming comes from a counter that advances as the analyzer works, so a *second*
interpreter in the same process sees different names for identical source. It rejected all 61
procedures the moment a measurement script bootstrapped twice. The check is now on arity, which is
renaming-independent, and a test asserts that a later bootstrap still installs.

**What is not prebuilt.** The library's source still loads and is still interpreted first -- that is
what creates the macros the analyzer needs and the closures this replaces -- which is the remaining
27 ms. Skipping it too would mean separating each file's macro definitions from its procedure
definitions. Worth doing, but larger than this, and it is interpreter time rather than compiler
time, so it does not affect the self-hosting question.

*Verified:* 2,345 tests pass in both modes; both conformance suites pass both ways; the compiled
benchmarks are unchanged and all correct.

**R51. Lambda lifting makes generated code linear in nesting instead of exponential. It is a size
change, not a speed change -- and the remaining outlier turned out to be something else entirely.**

Each nested procedure is now emitted once, at the top level, as a factory over its free variables
(`src/compiler/lift.js`). Nothing about variable *references* changes, which is what makes it cheap:
the inner function closes over the factory's parameters, and those already have the names its body
used.

Two things that were prerequisites, both arrived by accident:

- **Boxing solves mutation.** An assigned free variable is already a one-element array (R49), so
  passing it by value passes the array and sharing is preserved. Without R49 this would have needed
  its own answer.
- **`letrec` needed the "aware" part.** A `letrec`-bound lambda refers to names the group is still
  defining. Self-reference needs nothing -- the factory declares the name and assigns the procedure
  before returning, so a named `let` loop stays a direct call. Only a name a *sibling* refers to is
  boxed. That distinction is what makes `map` liftable: its `loop` reads `any-null?`, `all-cars` and
  `all-cdrs`, so those three are boxed while `loop` is not.

| | before | after |
|---|---|---|
| nested closures, per level | 4.2x | **linear** |
| sixteen levels deep | 138,801,809 chars | **11,478** |
| generated source, whole corpus | 24.36 MB | **12.80 MB** |
| compiled standard library module | 736 KB | **437 KB** |
| `dist/scheme.js` | 1513 KB | **1235 KB** |

**Performance is flat**, and that is the expected result rather than a disappointment: `vector`
+10%, `fixnum` -8%, everything else within 3%. Lifting removes nothing from the hot path and adds
one call per closure creation. It was done for size and it delivered size.

**The wire size barely moved** -- about 206 KB gzipped either way. Generated code compresses roughly
20:1 and gzip was already deduplicating what lifting removed. Bundle size was one of the two
motivations for doing this, and on that measure it was worth 280 KB of parse-and-memory rather than
anything a user downloads. The other motivation, no longer being able to exceed what can be
generated, is met completely.

**A separate 21% came free.** A fifth of the generated library was one repeated string: the "captured
beneath a redefined primitive" message, inlined at 676 sites. Moving it into a runtime function took
the module from 535 KB to 437 KB. Worth noting how it was found -- by attributing the remaining bytes
rather than assuming they were all structural.

**And the remaining outlier is not nesting at all.** `nucleic.scm:make-relative-nuc` is still 3.25 MB,
and **94% of that is `reify` frame literals**: 550 call sites each spilling about 476 names, because
a suspended frame conservatively saves every declared variable. That is quadratic in procedure size
and completely independent of lifting. A liveness analysis -- spill only what is live across each
suspension point -- is the fix, and it is now the largest single source of generated code in the
corpus.

**Three bugs of my own, all found by measurement rather than by the test suite.**

The first was the free-variable scan binding internal `define` names *as* the sequence was scanned
rather than before it. Internal definitions are in scope throughout a body -- that is what lets two
of them refer to each other -- so a self-recursive one was reported as *free* of the procedure
containing it, which put its name in the enclosing factory's parameter list and left the caller
passing a variable it had never declared. Four benchmarks failed with `s_check is not defined` and
friends.

The second was creating boxes for internal definitions by walking only the node kinds I had thought
of -- `seq`, `let`, `letrec` -- which missed `if`. A definition inside a conditional branch is
ordinary Scheme, and `peval` has one. It is now a general walk that stops at nested procedures,
because enumerating kinds is exactly the mistake that produced the bug.

The third was process, not code: I edited compiler sources while a benchmark was running in child
processes, so half its workers picked up a partly-applied refactor. Five programs "failed" for that
reason and five for real ones, and I could not tell which until I re-ran cleanly. Benchmarks that
spawn processes read the source at spawn time; edits during a run make the results meaningless.

*Verified:* 2,344 tests in both modes, both conformance suites both ways, 952 of 952 on the port
differential, every canonical benchmark producing an answer.

**R52. The lowering pass is Scheme, the JavaScript one is deleted, and the compiler now compiles
itself. The bootstrap terminates in the interpreter.**

R45 measured the cost of a Scheme lowering and left the decision open; the duplicate then sat in
`experiments/` being kept in step with a module under active development, which cost two
divergences and bought nothing after it had answered its question. It is promoted:
`src/compiler/ir.scm` is the pass, `src/compiler/lowering.js` is the door into it, and
`src/compiler/ir.js` no longer exists.

Before deleting it, the differential was run once more and the two agreed on all 952 lambdas — but
only after the harness was taught to compare `captures`, which it never had. The Scheme side had
never reported that field, and `control-globals` still listed `values` and `apply` months after
R48 removed them from the JavaScript. Both are the same lesson: *a differential test compares the
fields it renders, and a field it leaves out is a field two implementations may disagree about
silently.*

**The chain.** A compiler written in the language it compiles has to start somewhere, and here it
starts in the interpreter, which runs `ir.scm` from source with no compiler at all:

| step | produces | why it is in this order |
|---|---|---|
| the interpreter runs `ir.scm` | a working, slow compiler | needs nothing |
| it compiles the standard library | `compiled_stdlib.js` | — |
| that compiles `ir.scm` | `compiled_compiler.js` | lowering spends its time in `memq` and `assq`, which are themselves Scheme |

The last column is the whole reason the order matters and not a detail. Compiling `ir.scm` against
an interpreted library is worth **1.45x**; against a compiled one, **13.68x**. `npm run prebuild`
does all three in **0.62 s from nothing**, and both generated tables come out byte-identical to the
ones they replace, so the bootstrap is reproducible rather than merely repeatable.

**What had to be built that the library did not need.** `generate_compiled_stdlib.js` had been
leaving out any procedure with a pooled constant, on the stated grounds that no library procedure
had one. `ir.scm` has 144, in 11 procedures including `lower-node` and `lower-lambda` — the hot
ones — so the exception swallowed the point of the exercise. All 144 turned out to be **symbols**,
which are the one interned value that survives being written down: `intern("lambda")` read back
gives the same object, so the identity the pool exists to preserve is preserved by reconstructing
it. A pair would not be, and `serializeConstants` still refuses one, leaving that procedure
interpreted — the same answer it gave before, for a reason that is now stated rather than assumed.

**What it cost.** About **18x** against the JavaScript it replaced: 70 ms a pass over the corpus
against 3.8 ms, plus 7 ms of marshalling. Almost none of that is on a path anyone waits on — the
library is lowered at build time and a program's definitions are lowered once each — and it is not
visible in either suite's wall time (`npm test` 28 s unchanged; the program pass 8.2 s → 8.8 s).
What it did cost is **535 KB** on `dist/scheme.js`, for a compiled compiler that only a page
compiling at run time needs. That is a code-splitting problem and is on the roadmap as one.

**What it bought.** The tier's own performance is now the project's performance, and there is a
number for it: `npm run benchmark:self-host` lowers 993 lambdas under all three configurations,
checks that they agree about every one of them, and reports the ratio. That agreement check is the
differential the port used to provide, pointed at something that still exists: the interpreted run
is the reference semantics, so a disagreement means the tier changed the meaning of the compiler.

*Verified:* 2,426 tests in both modes, 82 of 82 on the new whole-program pass, 952 of 952 on the
final JavaScript-to-Scheme differential, 993 of 993 across tiers afterwards, and a build from an
empty `src/packaging/` reproducing both tables byte for byte.

**R53. The benchmark programs were a correctness corpus sitting outside `npm test`, and three
compiler bugs had already escaped through the gap.**

Every canonical benchmark carries an expected result and prints `INCORRECT` when it does not match,
so the suite has been checking 41 real programs all along — and none of those checks ran in
`npm test`. R51's three defects were all found there and missed by 2,344 unit tests, because each
needed a procedure shaped in a way no unit test happened to build.

The obstacle was never principle, only time: the timed suite calibrates each program to a second of
work and repeats it. Correctness needs one iteration, and being correctness-only buys two things
timing forbids. Runs go **in parallel** — an answer is the same answer whether or not seven other
processes are busy — and sizes can **shrink**: `nboyer` and `sboyer` at their benchmark size are 82
of the suite's 141 seconds and exercise the same code at size 0, whose expected value came from
Gambit for the same reason every other expected value did.

Result: 41 programs, both tiers, 82 assertions, **8.2 s**, in `npm test`. `tests/programs/` also
runs alone with `npm run test:programs`, and `--slow` adds the four programs that take no size
parameter for 26 s total. Two findings fell out of writing it: `ray` had been failing on a missing
`outputs/` directory rather than anything in the implementation, and the manifest's note claiming
`maze` returns a wrong answer under the compiler tier was stale — the whole-program continuation
analysis declines the escape route, 60 of 69 definitions compile, and both tiers answer correctly.



**R54. The compiler and the debugger have no relationship at all, and a breakpoint inside a compiled
procedure silently never fires.**

The plan said debugger hook points would be "redesigned for compiled code", and treated that as
scheduled work. It was never done, and the shape of what is missing is larger than a redesign:

- generated code carries **no source locations, no line information, no debug points** — nothing;
- `src/debug/` contains **zero** references to `$compiled`, `markProcedure`, or compiled procedures
  in any form;
- the single debug hook is `interpreter.js:455`, inside the interpreter's step loop and gated on
  `ctl.source`. A compiled procedure never enters that loop.

So `setBreakpoint` succeeds, and the breakpoint never fires. Not an error, not a warning — a no-op.
Stepping steps over the whole procedure and the stack tracer shows nothing.

Nobody had noticed because the tier compiles only the standard library, and people do not set
breakpoints inside `assq`. That also means this is presently harmless, and stops being harmless the
moment user code is compiled.

**What makes it a finding rather than a task** is what it says about how the work was ranked.
"Full-featured debuggers in both environments" is one of the project's four stated constraints;
performance is not among them and was added later. Fourteen increments of compiler work went by with
every ranked list measuring speed, and the constraint quietly moved further from satisfied at each
one — while the obvious next step, enabling the tier for user code, is precisely the step that would
break it. Ranking by the thing that is easiest to measure is not the same as ranking by what matters,
and nothing in the process caught the difference.

This is also the entry that argued for splitting these documents. It was found by grep in the middle
of answering an unrelated question, not by reading a plan, because no plan recorded it.

**R55. Saving every local at every suspension point was "never on a path that matters". It was 57%
of all generated code.**

The resumable form's own comment justified spilling every declared name: suspension happens only
while a continuation is captured or reinstated, "so the waste is never on a path that matters".
True of *time*, false of *size*. Every call site carried a literal naming every local, so a
procedure with *n* locals and *n* call sites wrote *n²* names. Measured over the corpus, frame
literals were **7.21 MB of 12.75 MB** generated, and 94% of the largest procedure,
`nucleic:make-relative-nuc` at 3.18 MB.

Backward liveness over the resumable form's blocks fixed it: each suspension point saves only what
is live where it resumes. Corpus **12.75 MB → 5.93 MB**, frame literals **7.21 → 0.40 MB**,
`make-relative-nuc` **3.18 → 0.27 MB**, `compiled_compiler.js` **548 → 271 KB**, `dist/scheme.js`
**1.84 → 1.53 MB**.

Two further beliefs fell on the way, both mine, both worth keeping.

**"The spill placeholder is not a read."** Written into the first draft of the analysis's tests,
and wrong. A capture has no ordinary edge to the code after it — it spills and returns — so the
frame is the *only* path by which its variables reach that code, and the spill must be treated as
reading exactly what is live there. Without that, resuming at an earlier call site and running
forward into a capture spills `undefined` for anything read only after the capture. Breaking the
rule deliberately makes the ctak shape return a wrong answer; nothing else in the suite noticed.

**"The assigned-local test guards the box-write rule."** An assigned local is a one-element array,
and `x[0] = v` reads `x`. The test written for that rule assigned `(set! acc (cons v acc))` — which
reads `acc` on the right-hand side, keeping it live however the write was classified. Breaking the
rule on purpose left all 258 compiler tests passing. The test that actually guards it needs a
local whose next mention after the capture is a pure write. Each of the analysis's four rules was
then broken in turn and a test confirmed to fail, which is the only way to know a soundness test
tests anything.

On speed, nothing moved that could be attributed to the change. The continuation class, the only
one where spills execute, improved 1.09x (`dynamic` 1.19x). A single-run comparison showed four
classes slightly below 1.0; interleaved reruns put every one within run-to-run spread, measured at
up to 13% on identical code. `array1` kept a 2.5% shift across six paired runs, and its executed
code was then verified **byte-identical** before and after — only literals inside never-taken
`UNWIND` branches differ — so the residual is either noise or V8 treating differently sized
functions differently.

**R56. "Write it in JavaScript now, port it once the design settles" never ported anything.**

After the decision to move the compiler to Scheme, three consecutive increments added JavaScript to
it — roughly 900 lines — and each was justified the same way: the code was still changing, so porting
it would mean porting a moving target. The argument was sound every time it was made and the port
receded every time, because a compiler under active development always has code that is still
changing. Liveness for frame spills (R55) was written in JavaScript *after* the direction had been
settled, next to its JavaScript caller, without the choice being weighed at all.

The one module that did move, `ir.scm`, moved because it had been written as Scheme first — as an
experiment, then promoted (R52). Nothing written in JavaScript was ever ported.

The belief that failed is not about any one module; it is that deferring the language choice is
free. It costs a port that never happens. So the policy is inverted: **new compiler code starts in
Scheme**, and where Scheme lacks something the compiler needs — hash tables, first — that capability
is built as a Scheme library over the minimum JavaScript. Interop is what makes this workable
mid-migration: Scheme code can call the unported emitter directly, and the emitter reaches Scheme
through `src/compiler/lowering.js`, so a new module never has to wait for its neighbours.

**R57. "The standard library is compiled" was true for the global environment only. Every library
kept the interpreted closures.**

R46 and R50 measured the compiled standard library through code evaluated in the global
environment, and the start-up comment in `scheme_entry.js` describes the library as compiled without
qualification. But importing copies values. `(scheme base)`'s export map, and the environment of
every library that imported it, captured `map`, `equal?`, `assoc` and the rest while they were still
interpreted closures; installing the prebuilt table replaced the *global* bindings and nothing else.
So a library loaded after start-up imported the interpreted versions — `(eq? map probe-map)` was
`#f` between a library and the code that loaded it — and so did the procedures of every library
loaded during start-up. Library code paid the interpreter boundary R45 measured at 10x on exactly the
calls the prebuilt table exists to speed up.

It surfaced as a correctness bug, not a speed one: SRFI 125 recognises `equal?` by identity to choose
a hash function, and the library's `equal?` was not the user's. Both install paths now pass what they
replaced to `substituteLibraryValues`, which updates export maps and library environments.

How much library code the old behaviour slowed was never measured and now cannot be without undoing
the fix. The broader lesson is the one R45 already taught, one level up: a speedup measured from one
environment says nothing about code that reaches the same procedures by another path.

**R58. The interpreter did not run tail recursion in constant space when the tail call ended a
sequence.**

`compiler_design.md` says tail calls go through a trampoline, "so tail recursion runs in constant
space", and R2 recorded the debugger's version of this bug as fixed. Both held only when the tail
call was not the last expression of a sequence. `BeginFrame.step` pushed a frame for the remaining
expressions even when none remained, so the last expression of every `begin`, every procedure body of
two or more expressions, and everything that expands to them -- `when`, `unless`, `cond` clauses,
`do` -- ran with an exhausted frame beneath it. A loop through any of them gained a frame per
iteration: `array1` peaked at 100,007 frames, `quicksort` at 10,008, `fft` at 8,198. Under the
debugger it was worse, because the exhausted frame hid the procedure's `DebugExitFrame` from R2's
tail-call detection, so the shadow call stack grew too. And since re-entering Scheme from JavaScript
copies the frame stack, a loop calling a `define-class` method was quadratic: 6 us a call at 2,000
iterations, 40 us at 20,000.

Nothing caught it because the space test, `tests/core/scheme/tco_tests.scm`, loops through `if`
branches with a single-expression body -- the one shape that was right -- and so did R2's test.

*Consequence:* the interpreter baseline every compiled-tier ratio was measured against carried this
cost. Fixed, it takes 0-9% fewer dispatches per benchmark and roughly 5-12% less time on the list,
vector and string classes, so those classes' compiled-tier ratios measured before the fix read
correspondingly high. No ranking in `compiler_plan.md` depended on a margin that size.

---

## Appendix — the original staged plan

Kept because it is the plan the entries above were measured against, not because it is the plan.
The current one is [compiler_plan.md](compiler_plan.md). Stages 0, 1, 2a and 2b are done; Stage 3 was folded
into "code generation, for the first time".

### Plan of attack (historical)

Four stages, each independently valuable, each gated on measurement. Stage 1 is worth doing
whether or not Stage 2 ever happens, and it de-risks Stage 2 because the compiler emits code
against the same runtime.

### Stage 0 — Measurement and instrumentation

Make progress legible before changing anything.

- Extend `benchmarks/` beyond its current numeric-tower framing. Add the standard programs from
  the Thivierge/Feeley evaluation so results are comparable to published numbers: `fib35`,
  `nqueens12`, `oddeven`, plus the continuation-heavy set `ctak`, `contfib30`, `btsearch2000`,
  `threads10`. Consider pulling a subset of `ecraven/r7rs-benchmarks`.
- Add a cross-implementation harness. Gambit (`gsi`) and Racket are already installed on this
  machine; record their numbers alongside ours in `benchmarks/baseline.json`.
- Add a `--cpu-prof` profiling script and a per-node-type step counter behind a flag, so the
  cost of a change is attributable rather than guessed.
- Run an **R7RS-small conformance audit** to size the compliance gap before committing to a value
  representation — mutable strings are known; find the rest. A published conformance suite plus
  Appendix A of `docs/r7rs-small.pdf` is the reference.
- Key files: `benchmarks/run_benchmarks.js`, `benchmarks/save_baseline.js`,
  `benchmarks/compare_baseline.js`, `benchmarks/baseline.json`.

**Gate:** a reproducible table covering compute, allocation, `call/cc`, and interop, plus a
written list of compliance deviations.

> [!TIP]
> **Stage 0: done.** Delivered as planned, plus three things not in the plan: the three broken
> debugger behaviours were fixed (see [R2](#revision-log)); the step counter became
> `src/debug/instrumentation.js`, which wraps the interpreter rather than adding a hot-path branch;
> and `npm run benchmark:record` was added later, which regenerates
> [performance_progress.md](performance_progress.md) from an append-only history so results after
> each stage stay visible without hand-maintenance.
>
> Commands: `benchmark:standard`, `benchmark:steps`, `benchmark:profile`,
> `benchmark:implementations`, `benchmark:record`, `audit:r7rs`.

### Stage 1 — Fix the representation, no compiler

> [!IMPORTANT]
> **Stage 1: done, at 2.57x against a 10–30x estimate. See [R4](#revision-log)–[R7](#revision-log).**
> The item list below is preserved as written. Two things about it were wrong: it named lexical
> addressing (item 3) as the biggest available win when measurement put the whole cost of variable
> lookup at ~15% of runtime, and it omitted the technique that actually produced most of the gain —
> inlining subexpressions that cannot capture a continuation ([R5](#revision-log)). Items 3, 4 and 6
> were deliberately not done; what was done and why is in `ROADMAP.md` and `CHANGES.md`.
>
> Outcome: `fib` 593 → 136 ms, evaluator dispatches down 3.4–5.7x, gap to Gambit `gsi` from ~26x to
> ~8x, primitive work up from 2.8% to 8.5% of runtime.

All of this is interpreter work with no architectural change, and all of it is reused by the
compiler later. Ordered by measured impact per unit effort.

1. **Binary fast paths for arithmetic and comparison.** Make `<`, `>`, `=`, `<=`, `>=` native
   primitives with a 2-argument fast path and a variadic fallback; give `+`, `-`, `*` a
   `typeof a === 'bigint' && typeof b === 'bigint'` path ahead of the tower dispatch. Drop the
   `forEach`/`reduce` closure allocation in `math.js:305`. Files: `src/core/primitives/math.js`,
   `src/core/scheme/numbers.scm`, `src/core/interpreter/type_check.js`. *Measured: 1.93x from
   the comparison operators alone.*
2. **One `AppFrame` per call, not one per argument.** Evaluate arguments into a pre-sized array
   with an index; precompute the operator/operand layout at analysis time instead of rebuilding
   it per invocation. Kills the O(n²) copying and most of the 23% in `frames.js`. Files:
   `src/core/interpreter/frames.js`, `src/core/interpreter/ast_nodes.js`.
3. **Lexical addressing.** The analyzer already alpha-renames and already knows each lambda's
   parameter list — extend `SyntacticEnv` to compute `(depth, index)` for every `VariableNode`,
   and make environments flat arrays (`Rib { vals, parent }`) instead of `Map` chains. Keep a
   *static* `names[]` on the rib template for the debugger, replacing the per-call `nameMap`.
   Files: `src/core/interpreter/analyzer.js`, `src/core/interpreter/environment.js`,
   `src/debug/state_inspector.js`.
4. **Global value cells.** Resolve top-level and library references to a mutable cell at analysis
   time, so a global reference is `cell.v` rather than a walk to the root. Directly targets
   `symbol-lookup-100K` (1,506 ms today).
5. **Cheap allocation removals.** Drop the discarded `Map` in the `Environment` constructor;
   replace `Object.defineProperty(closure, 'name', ...)` in `createClosure`
   ([values.js:96](../src/core/interpreter/values.js:96)) with a plain property, which currently
   forces every closure into dictionary mode; make `pushJsContext`'s `[...fstack]` copy lazy so
   it is paid only when a closure or continuation actually escapes into JS.
6. **Unify `run` and `runAsync`.** They are hand-maintained near-duplicates
   ([interpreter.js:235](../src/core/interpreter/interpreter.js:235) and
   [interpreter.js:440](../src/core/interpreter/interpreter.js:440)), and only `runAsync` can
   actually pause. Every subsequent change otherwise has to be made twice.

**Gate:** re-run Stage 0. Expectation is **10–30x** (fib(30) from 6.5 s into the 250–650 ms
range, i.e. Gambit-interpreter territory). If it lands far below that, re-profile before
committing to Stage 2 — the compiler's win depends on the same costs being real.

> [!IMPORTANT]
> **Gate outcome: missed on both measures, and the gate's own instruction was followed.**
> The suite came in at **2.57x** geometric mean against a 10–30x target. On the gate's own
> yardstick, `fib(30)` went from 6,524 ms to **1,650 ms** — a 3.95x improvement, but well outside
> the predicted 250–650 ms band.
>
> The gate said: *"If it lands far below that, re-profile before committing to Stage 2."* That is
> what happened, and re-profiling produced [R4](#revision-log) and [R7](#revision-log): the
> remaining ~55% of runtime is frame machinery that cannot be removed without changing the
> execution model.
>
> **This does not weaken the case for Stage 2; it sharpens it.** The gate's worry was that "the
> compiler's win depends on the same costs being real". Those costs are real — they are simply not
> reachable from inside an AST-walking interpreter. Primitive work is now 8.5% of runtime, so
> ~90% of it remains interpretive overhead, and the measured headroom to Racket CS is still
> 300–600x on call-heavy programs.

### Stage 2a — Calling-convention bake-off

Before committing, settle the decision above with data. Build a throwaway compiler for a Scheme
subset (lambda, `if`, `let`, arithmetic, `call/cc`) under **both** conventions and measure:
normal-path speed on `fib35`/`nqueens12`/`oddeven`, capture-heavy speed on `ctak`/`contfib30`/
`threads10`, deep tail recursion, and — critically — **what the Chrome DevTools call stack and
scope panel actually show** for each, with a source map attached. That last item is a
30-minute experiment that decides whether the extension can be retired, and it should be run
early rather than assumed.

> [!NOTE]
> **Revised after Stages 0 and 1.** Three additions to this bake-off:
> - Go in **neutral** on continuation cost, not tilted toward (B) — [R1](#revision-log).
> - Have both prototypes classify subexpressions by whether they can capture a continuation, and
>   emit frames only where one can. This was worth ~5x in dispatch count in the interpreter and is
>   the same information both conventions are organised around — [R5](#revision-log).
> - Whatever continuation representation each prototype uses must tolerate capture-during-argument-
>   evaluation and multi-shot re-invocation; frame mutation and clone-at-capture are both ruled out
>   for the reasons in [R6](#revision-log). The tests in `tests/core/scheme/number_tests.scm` pin
>   the required behaviour and should be run against both prototypes.
>
> Budget the DevTools experiment first. It is cheap and it is the only input that decides whether
> `extension/` can be retired.

**Gate:** a written convention decision with numbers and DevTools screenshots behind it.

> [!IMPORTANT]
> **Decision: convention B — native JavaScript stack, trampoline for tail calls, cooperative
> unwind for capture.** See [R10](#revision-log)–[R14](#revision-log) for the measurements.
>
> Both prototypes were built from a shared front end and both pass all eight benchmarks, including
> the two requiring multi-shot continuations. Prototypes are in
> [`experiments/stage2a/`](../experiments/stage2a/); run `node experiments/stage2a/summary.js`.
>
> **Why B:**
> - Faster on the normal path by up to 3.3x, and faster overall — 17.5x over the interpreter
>   against A's 13.0x. The plan expected to be trading performance away for debuggability; there
>   is no trade.
> - One live Scheme frame is one live JavaScript frame, so the Scheme call stack is visible to any
>   JavaScript debugger. Under A it is one frame. This is what decides whether `extension/` can be
>   retired, and it is not close.
> - It gives up 1.2–1.5x on three of the four capture-heavy benchmarks, which is a small price for
>   the normal path that dominates real programs.
>
> **The cost, which is real:** B's generated code is **4.09x larger**, because a procedure needs a
> second, re-enterable copy to be resumed after a capture. Code size matters for browser delivery.
> The prototype emits that twin for every procedure; an effect analysis proving a procedure can
> never capture would drop most of them. That is now a named Stage 2b task.
>
> **Still to verify before deleting `extension/`:** the stack *shape* was measured, not the
> source-map relabelling of those frames in DevTools. That end-to-end check belongs in Stage 2b.

### Stage 2b — The compiler tier

Add a backend that emits JS source, reusing the Stage 1 runtime.

- **Introduce a real IR.** Since the implementation is open to change, do not compile the analyzed
  AST directly to JS. Lower it first to an explicit intermediate representation — ANF or CPS over a
  control-flow graph, in the manner of Gambit's GVM or Guile's CPS-SSA. Direct AST→JS codegen
  removes interpretive overhead but supports almost no real optimization; an IR is what later makes
  contification, inlining, unboxing, and closure elimination expressible at all. This is the
  difference between landing at ~10x native JS and approaching ~2x. The AST→IR lowering is also the
  natural place to do the tail-position analysis the current analyzer does not do (every call is a
  `TailAppNode` today; the name is vestigial).
> [!NOTE]
> **Revised after the canonical suite (R29, R30).** The ordering below is wrong. Value
> representation is listed as something to do "at the same time" as codegen; the measurements say it
> should come **first**, because the compiler tier is worth 4.17x on call-heavy code and 0.97–1.35x
> on every other workload class. Codegen improvements multiply a term that is not dominant in five
> of seven classes. The IR argument in the bullet above, which R16 walked back, is also back on the
> table for the same reason.

- **Revisit the value representation at the same time.** This is the only cheap moment to do it.
  Candidates: fixnums as JS numbers with a checked promotion to BigInt on overflow (the ~3x tower
  cost measured above), unboxed flonums, mutable `SchemeString` per the compliance note, and
  dropping the `source` field from every runtime-allocated `Cons`.
- **Codegen model:** one JS function per Scheme lambda, per the convention chosen in 2a. Tail calls
  become trampoline returns either way. Follow Thivierge/Feeley's optimizations — branch-destination
  inlining, branch-destination call, intermittent polling — and Marshall's rule of signalling
  control transfer with a distinguished return value rather than an exception.
> [!IMPORTANT]
> **Constraint discovered in Stage 1 — see [R6](#revision-log).** Whatever continuation
> representation is chosen must tolerate capture during argument evaluation and multi-shot
> re-invocation. Two tempting designs are ruled out: frames cannot be advanced in place, because
> `call/cc` shares them with every continuation captured during the call; and they cannot be cloned
> at capture, because `dynamic-wind` finds the common ancestor of two stacks by frame **identity**.
> `tests/core/scheme/number_tests.scm` pins the behaviour.

- **Continuations:** under (A), capture heapifies the frame array (their `heapify`/`underflow`),
  which maps closely onto today's `createContinuation`
  ([values.js:139](../src/core/interpreter/values.js:139)). Under (B), capture runs the unwind protocol
  and rewind rebuilds a fresh chain. Multi-shot works in both because frames are copied, not consumed.
- **Source maps** are emitted from the start, not retrofitted — including `x_google_ignoreList` for
  runtime files and column-accurate mappings for expression-level breakpoints.
- **Macro phase separation and explicit renaming** land here (see *Two design questions* below),
  since the expander and compiler must agree on phases.
- **Debug modes:** compile-time flag emitting `full` / `statement` / `off` debug points, with a
  side table mapping each point to `{filename, line, column}` and each rib to its variable names.
  `SchemeDebugRuntime`'s interface is redesigned around these emitted points rather than
  `ctl.source` — confirmed as acceptable. The debugger's current coupling to interpreter internals
  is the part to plan deliberately: `StateInspector` walks `Environment.parent`/`.bindings` as raw
  `Map`s, and the REPL's `:eval` reconstructs the analyzer's alpha-renaming from `nameMap`
  ([repl_debug_commands.js:206](../src/debug/repl_debug_commands.js:206)). Both need a defined
  replacement API rather than incidental access to whatever the runtime happens to expose.
- **Tiering and fallback:** the interpreter stays **permanently**, as the CSP-safe execution mode,
  the reference semantics for differential testing, and the maximum-fidelity debug tier.
- **Retire or shrink the DevTools extension** once source-map debugging is verified — a deliverable
  in its own right, not a side effect.
- **Loading:** AOT-compile `src/core/scheme/*.scm` into the bundle (extending
  `scripts/generate_bundled_libraries.js`, which today only inlines source text), so stdlib
  procedures like `map`/`assoc`/`append` stop being interpreted.

**Gate:** whole test suite green under both tiers, plus a differential harness asserting
interpreter and compiler agree on every test.

### Stage 3 — Optimization

Only once Stage 2 is correct and measured: direct calls to statically known procedures, primitive
inlining, arity specialization, unboxed fixnum paths guarded by tower fallback, and escape
analysis to stack-allocate environments for procedures whose closures never escape.

---
