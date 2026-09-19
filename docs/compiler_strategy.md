# Compiler strategy for scheme-js-4

> **Status:** Stage 0 complete. Stage 1 complete (2.57x). Stage 2a complete — convention B
> chosen. Stage 2b **increment 1** complete — a working compiler tier with primitive inlining.
>
> **Read the speedup figures with [R20](#revision-log) in hand.** The ~12x measured on the
> microbenchmarks does **not** transfer: on the repository's own Scheme the tier is **1.39x** per
> file. The microbenchmark suite was overfitted to the optimizations chosen against it. The tier is
> also **opt-in and off by default** until compiled frames are re-enterable; see
> [R15](#revision-log).
>
> **This is a living document.** The original analysis is kept intact, including the parts that
> later turned out to be wrong, because how an estimate failed is worth more than the estimate.
> Where measurement has since revised a claim, the passage carries a *Revised after Stage N*
> callout and the [Revision log](#revision-log) records what changed and why. Nothing is deleted.
>
> Measurements were taken on Apple Silicon / Node v24.11.1 and are reproducible via the commands in
> [performance_baseline.md](performance_baseline.md). Results after each stage:
> [performance_progress.md](performance_progress.md). Re-measure before relying on any of this on
> other hardware.

## Context

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

### Measured baseline (this machine, Node v24.11.1, Apple Silicon)

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

### Where the time actually goes

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

### Why it is this slow — specific, fixable causes

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

### What is *not* a problem

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

### R7RS compliance: a pre-existing gap that collides with a stated constraint

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

## Recommendation: transpile to JavaScript

Given **full multi-shot R7RS `call/cc`**, the target-language decision is easy and the
*calling-convention* decision is the hard one. They should be taken separately.

### Target language: JavaScript source

Rejected alternatives:

- **Bytecode VM + JIT.** Self-defeating on this platform. You would write an interpreter loop in
  JS that V8 cannot optimize, in order to avoid emitting JS that V8 *can* optimize. Published
  results on tail-calling vs switch-based interpreters under Wasm point the same way.
- **WebAssembly.** The stack-switching proposal is still Phase 3 and not shipped, so Wasm gives
  you no continuation primitive — you would implement the same machinery anyway, *plus* pay a
  boundary crossing on every JS interop call (violating constraint 1) and give up source-mapped
  debugging in DevTools. Worth revisiting once stack switching ships; not now.

### Calling convention: the pivotal decision

This determines performance, `call/cc` cost, **and whether the Chrome DevTools call stack shows
Scheme frames** — which is what decides whether the extension can be retired (see below). Two
serious candidates:

**(A) Explicit frame stack — the Gambit-JS model.** Thivierge & Feeley, *Efficient Compilation of
Tail Calls and Continuations to JavaScript* (Scheme Workshop 2012). Continuation frames live in a
JS array; everything runs under a trampoline; `call/cc` heapifies the array.

- Best-measured performance: `fib35` in 0.80 s on 2012 V8, ≈1.2–1.8x plain JS normalized to today.
  1.1–96x faster than Scheme2JS, 2.9–6.8x faster than the CPS-based Spock.
- Zero overhead when nothing captures; capture cost proportional to depth, paid only at capture.
- Closest to the current architecture, so lowest risk.
- **But the JS call stack is one frame deep.** All Scheme frames are in an array DevTools cannot
  see, so the DevTools call-stack panel is useless and the extension cannot be fully retired.

**(B) Native JS stack for non-tail calls, trampoline for tail calls, unwind/rewind for capture.**
This is precisely the design in the two attached papers — Pettyjohn et al.'s generalized stack
inspection, with Marshall's modification replacing the exception with a distinguished return value
and a per-call-site trampoline.

- One JS stack frame per live Scheme frame, and tail calls correctly *don't* add one. **The
  DevTools call stack becomes the Scheme call stack.** This is the property that makes source-map
  debugging genuinely work.
- Marshall's measurements support it: stack-allocated continuations beat heap-allocated ones by up
  to 3x, his C# interpreter matched the C one, and returning a distinguished value beats throwing
  by ~3,800x — so the technique's historical weakness (exception cost) is designed out.
- Multi-shot works: rewind rebuilds a fresh frame chain per invocation.
- **Costs:** procedure fragmentation (each procedure splits at call sites so it can be re-entered,
  typically a `switch` on a resume index), a check after every call site, and somewhat noisier
  generated code — which slightly degrades source-map fidelity, the very thing it buys.
- **Uncertainty:** there is no published measurement of Marshall's variant on V8. Thivierge &
  Feeley measured the *exception-based* version (Scheme2JS) at 18.5–96x slower on capture-heavy
  code, and Marshall's change targets exactly that weakness — but that inference is not data.

> [!NOTE]
> **Revised after Stage 0 — see [R1](#revision-log).** The performance case for (B) above is weaker
> than written. Cross-implementation measurement showed our continuation benchmarks run 40–140x
> slower than Racket against ~1400x for `fib`, i.e. our explicit frame stack is *relatively* one of
> the healthier parts of the implementation. The DevTools-call-stack argument for (B) is unaffected,
> so the bake-off still stands — but go into it neutral rather than tilted toward (B).
>
> **Added after Stage 1 — see [R5](#revision-log).** Stage 1 found that subexpressions which cannot
> capture a continuation need no frame at all. The compiler should make that classification
> statically; it determines which call sites need continuation frames under *either* convention, and
> so should be part of the 2a prototypes rather than a later optimization.

> [!IMPORTANT]
> **Resolved in Stage 2a: convention B.** It won on the normal path *and* overall, and it is the
> only one of the two under which a debugger can see the Scheme call stack. The costs and the
> remaining caveat are recorded at the Stage 2a gate below and in
> [R10](#revision-log)–[R14](#revision-log). The analysis below is kept as written.

**Recommendation: prototype both in Stage 2a and measure before committing.** The decision rests
on an unmeasured quantity (B's normal-path overhead on V8) and on how much the DevTools stack
panel is worth. A reasonable prior is that (B) wins overall if its overhead is under ~2x, because
retiring the extension is worth a great deal of ongoing maintenance. Both designs share the same
IR, runtime, value representation, and roughly 80% of the compiler, so the prototype is
comparatively cheap and is not wasted either way.

### How this satisfies the four constraints

| Constraint | How |
|---|---|
| **JS interop** | Unchanged. Scheme closures stay callable JS functions ([values.js:96](../src/core/interpreter/values.js:96)); compiled procedures keep the same wrapper, so `addEventListener('click', scheme-proc)` still works. Value representation is untouched, so `js_interop.js` conversion is untouched. Synchronous JS→Scheme re-entry keeps working via the existing `SentinelFrame` mechanism. |
| **Browser + CLI** | Generated code is ordinary JS. AOT-compile the stdlib into the bundle for the browser; compile on demand elsewhere. |
| **REPL** | Compilation is a *backend after `analyze`*, not a replacement for it. `analyze` stays runtime-callable, so `eval`, `load`, `define-macro`'s expansion interpreter, and `:eval` all keep working. Per-form compilation via `new Function`; the interpreter remains as the fallback tier. |
| **Debugger** | Hook points get redesigned for compiled code (confirmed acceptable). Compiled code emits explicit debug points under a compile flag: `full` (expression-level, the default) / `statement` / `off`. Today's per-step `ctl.source` check is paid even when debugging is off — the compiler can make `off` genuinely free, which is a debugger *win*. In-page debugging additionally gains source maps; see below. |

### Browser-page debugging: can source maps replace the extension?

**Largely yes, and this is a strong argument for the compiler — but it is contingent on the
calling convention, and it is not free.**

What a standard source map buys immediately, in any design:

- DevTools displays the original `.scm` file and sets breakpoints in it. Source Map v3 carries
  column information, so if the compiler emits distinct columns per Scheme subexpression you get
  expression-level breakpoints — the feature the extension implements by hand today.
- Stepping highlights Scheme source rather than generated JS.
- `x_google_ignoreList` hides runtime/trampoline files from stack traces and stepping, which is
  well supported in Chrome.

What does **not** come for free, and how to close each gap:

| Gap | Resolution |
|---|---|
| **Call stack shows JS, not Scheme** | Only solved by calling convention **(B)**. Under **(A)** the stack lives in an array DevTools cannot see, and you must keep a custom stack view. *This is the single biggest factor in whether the extension can go away.* |
| **Scheme values render as JS objects** — a pair shows as `{car, cdr}` | Chrome **Custom Object Formatters** (`window.devtoolsFormatters`). Renders pairs as `(1 2 3)`, symbols, chars, records properly. Requires the user to tick one DevTools setting. Well-trodden (ClojureScript and Dart both ship these). |
| **Scope panel shows JS locals under compiled names** | Partly closed by the source map `names` field; Chrome's variable-name mapping is real but incomplete. The robust fallback is to keep the compile-time rib name tables and expose them, rendered via a custom formatter. |
| **DevTools console evaluates JavaScript, not Scheme** | Expose a global helper — `$scm("(+ 1 2)")` — that compiles and evaluates in the paused frame's environment. Loses the seamless feel, keeps the capability, costs almost nothing. |

So the realistic outcome is: under **(B)**, the Chrome DevTools extension can plausibly be
**retired entirely**, replaced by a source map, a formatter script, and a console helper. Under
**(A)**, it shrinks a lot but a custom stack view must survive. Either way this is a large
reduction in the surface area now carried by `extension/` and `src/debug/devtools/`, and it should
be treated as a first-class goal of the project rather than a side effect — it materially changes
the cost/benefit of the whole effort.

The REPL debugger (`src/debug/repl_debug_*`) is unaffected and stays as-is.

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

### Keeping this log

Add an entry whenever a stage produces a measurement that contradicts something written here, or a
gate is missed, or a technique is used that the plan did not anticipate. Entries are numbered,
append-only, and never edited away — including the one above that corrects an earlier entry in this
same log. Annotate the superseded passage with a callout pointing at the entry rather than rewriting
it, so the reasoning that led to the wrong call stays legible.

What makes this worth the effort is visible in R4 and R8: both estimates failed the same way, by
reasoning about which structures *looked* expensive instead of measuring, and by comparing numbers
taken at one benchmark size against a target set at another. Neither pattern would have been
apparent from the corrected document alone.

---

## Plan of attack

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

## Verification

- **Correctness is non-negotiable and already well covered**: 2,021 tests pass today
  (`node --expose-gc run_tests_node.js`, ~63 s). Every stage must keep them green in **both**
  Node and browser, per `AGENTS.md`.
- **Expect to rewrite some tests, and say so explicitly.** A meaningful share of `tests/unit/` is
  written against interpreter *internals* — `Environment.bindings` as a `Map`, `AppFrame` shape,
  `registers` layout, `debugRuntime` hook presence. Stage 1 changes those representations by
  design, so those tests must be updated rather than preserved. The rule to apply: **behavioural
  tests (`.scm` files, functional, integration) are the contract and must not change; unit tests
  on internals are implementation detail and follow the implementation.** Where a unit test
  encodes a real invariant, re-express it against the new representation rather than deleting it.
- **New compiler unit tests are their own workstream**, written alongside the compiler per
  `AGENTS.md`'s test-first rule: IR lowering, tail-position analysis, closure conversion, codegen
  per special form, source-map offset correctness, and debug-point emission at each of the three
  fidelity levels.
- **Differential testing** between interpreter and compiler on every `.scm` test file — this is
  the main safety net for Stage 2 and should be built before the compiler, not after.
- **Continuation semantics specifically**: `ctak`, `contfib30`, `btsearch2000`, `threads10`
  exercise deep capture, repeated capture, backtracking, and coroutining. Multi-shot re-invocation
  must be asserted explicitly, since that is the property that rules out the faster designs.
- **Interop and debugger**: the existing `tests/extension/` Puppeteer suite (394 tests) and
  `tests/debug/` must pass unchanged in Stage 1. In Stage 2 they are rewritten against the new hook
  contract — and if the extension is retired, much of `tests/extension/` is replaced by source-map
  fidelity tests (breakpoint in `.scm` line N stops at the right place) rather than deleted.
- **R7RS conformance** must not regress, and the string-mutability gap should close rather than
  persist, given full compliance is now a stated goal.
- **Performance regression gating** via `npm run benchmark:compare` against a committed baseline.

---

## Known-broken things found along the way

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

## Two design questions, answered

### CSP: alternatives to `new Function`

Yes, there are feasible alternatives, and they compose — you don't have to pick one.

1. **AOT + interpreter fallback (the main answer).** Compile everything reachable at build time:
   the stdlib, and `<script type="text/scheme">` sources. Nothing dynamic is needed to *run* a
   compiled page. For the genuinely dynamic paths — `eval`, `load`, the REPL — fall back to the
   **interpreter tier**, which needs no code generation at all. This is the cleanest resolution and
   is a strong reason to keep the interpreter permanently rather than transitionally: it is the
   CSP-safe execution mode, the differential-testing oracle, and the debug-fidelity reference, all
   for the price of something you already have.
2. **Blob-URL dynamic `import()`.** `import(URL.createObjectURL(new Blob([js], {type:'text/javascript'})))`
   is governed by `script-src blob:` rather than `unsafe-eval`, and many strict policies allow
   `blob:`. Gives full compiled speed for dynamically compiled code under a policy that forbids
   `eval`. Asynchronous, so it suits `load` and the REPL better than a synchronous `eval` primitive.
3. **Wasm codegen.** `WebAssembly.compile` is permitted under `wasm-unsafe-eval`, a *weaker*
   directive than `unsafe-eval`. Technically a route to dynamic compilation under strict CSP, but
   only worth it if a Wasm backend is built for other reasons. Not now.

Note `js-eval` ([interop.js:22](../src/extras/primitives/interop.js:22)) is a deliberate interop
escape hatch, not core machinery; under strict CSP it simply throws, which is acceptable. It does
not force the rest of the system to depend on `eval`.

**Recommendation:** treat CSP-strict as a supported configuration = AOT + interpreter, with
blob-import as an optional upgrade. Decide this before Stage 2, because it determines whether the
interpreter is a permanent tier (it should be).

### Hygienic macros without a second interpreter

The second-`Interpreter` construction
([core_forms.js:585](../src/core/interpreter/analyzers/core_forms.js:585)) is about `define-macro`,
which is *procedural and non-hygienic* — so the question is really "can procedural macros be both
hygienic and cheap?" Yes, and the pieces are largely in place already.

The actual defect is **conflating compile time with run time**, not hygiene. `syntax-rules` here is
already hygienic via `SyntaxObject` and scope sets
([syntax_object.js](../src/core/interpreter/syntax_object.js), 579 lines) — that is Flatt's *Binding
as Sets of Scopes*, the modern correct answer, and the hardest part to get right. It's done.

The fix is **phase separation**: maintain a phase-1 (compile-time) environment distinct from the
phase-0 (runtime) one, and evaluate transformers in phase 1 of the *same* runtime, rather than
constructing a fresh `Interpreter` and global environment per macro definition. This is Racket's
expander tower, and it is what makes AOT coherent — compile-time code needs a runtime present *at
compile time*, which is unremarkable, rather than at deploy time, which was the worry.

For the procedural-macro surface itself, two options, both fully hygienic:

- **Explicit renaming (`er-macro-transformer`)** — the transformer receives `(expr rename compare)`.
  Simple, fully hygienic, procedural, and used by Chicken and Chibi. Given `src/core/scheme/chibi/`
  already exists in the tree, this is the natural lineage and by far the smaller job.
- **`syntax-case`** (R6RS) — more expressive, needs `datum->syntax` / `syntax->datum` and a full
  syntax-object protocol. The `SyntaxObject` layer already present covers most of the cost.

**Recommendation:** add explicit renaming as the hygienic procedural macro system, keep
`define-macro` as a clearly-marked non-hygienic legacy extension implemented on top of it, and
introduce phase separation so no second interpreter is ever constructed. Fold this into Stage 2's
IR work, since the expander and the compiler have to agree on phases anyway.
