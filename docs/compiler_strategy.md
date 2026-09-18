# Compiler strategy for scheme-js-4

> **Status:** Stage 0 complete. Stage 1 complete (2.57x). Stage 2a complete — convention B
> chosen, measured at 17.5x over the interpreter. Stage 2b is next.
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
