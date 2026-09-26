# Compiler effort: a step-back assessment

*2026-09-26, on branch `compiler-investigation` at commit `a0bb098` plus the uncommitted task-27
tree. A point-in-time review, not a plan: it ranks nothing on its own authority. Where it
recommends a reordering, the place to record the decision is `compiler_plan.md`.*

**Questions asked:** Is the overall design right? Is the general approach the best way to build
what we want? Is the implementation plan the right plan, with the right priorities and ordering?
What has been overlooked?

**Constraints the assessment holds everything against** (the four the user restated, plus the two
`ROADMAP.md` records): JavaScript interoperability; browser and CLI; a REPL in both; a
full-featured debugger in both; full multi-shot `call/cc`; fully compliant R7RS-small.

**Six facts the user supplied**, which shape the ordering more than anything in the code:

1. The first user of the tier is Scheme-JS itself — its interpreter, REPLs and compiler — followed
   by decent public benchmark numbers for an announcement to the Scheme and Lisp community.
2. There are no external users yet; the representative programs are the implementation's own
   Scheme and the public benchmark suites.
3. The Chrome extension is not a goal. Source maps are meant to obviate it entirely.
4. An exact Scheme `1` need not be distinguishable from a JavaScript `1.0` on the JavaScript side.
   (What the implementation does with this is verified in §4.2 F.)
5. Strict Content-Security-Policy is not a stated target; §4.2 I says what it is and recommends
   keeping the property since it is already paid for.
6. Bundle size has no target; §4.2 E puts today's size beside comparable runtimes.

---

## 1. Verdict in brief

**The design is right. The approach is unusually well run. The plan is mostly right but is ordered
for the benchmark suite's per-class numbers rather than for the tier's first user, and it is
missing three items that decide whether the tier reaches that user at all.**

The compiler tier is sound, fast where it applies, and built on choices that were measured rather
than argued: JavaScript source as the target, the analyzed AST as the input, the native stack with
cooperative unwind for continuations, the interpreter as a permanent tier. The findings log is a
genuine asset; the project's own confident errors were caught by its own instruments every time.

Three things need to change, in this order of importance:

1. **The tier has still reached no user, and its first user is this implementation.** Twenty-seven
   tasks in, every speedup is a benchmark number: no REPL, script tag or CLI run compiles a line of
   user code. Task 30 is gated behind a per-procedure deoptimization mechanism that is itself
   undesigned. A simpler v1 policy — *debugger enabled ⇒ user code runs interpreted* — satisfies
   the "full fidelity by default" requirement exactly, costs almost nothing to build, and unblocks
   30 now. Per-procedure deoptimization (29) and source maps (37) then become refinements that
   recover speed under the debugger, not gates on having speed without it.

2. **There is no written design for debugging compiled code**, and the two mechanisms the plan
   names (decline-to-optimize, source maps) are necessary but not sufficient: neither covers stack
   traces that include compiled frames, stepping *into* a compiled procedure, inspecting lifted,
   boxed and renamed locals, or the CLI debugger, which has no DevTools to hand a source map to.
   With the extension out of scope the design simplifies to two contexts and is worth a day to
   write down. Separately, and reachable today: a breakpoint inside an interpreted callback
   invoked from a *compiled* library procedure (`map`, `for-each`, `assoc` with a predicate) cannot
   pause where it is hit, because a nested run is synchronous and only the outer `runAsync` loop
   honours a pause. Compiling the standard library moved these procedures from interpreted to
   compiled, so this is a regression the browser REPL's debugger already has.

3. **The decline policy will hold back far more of a real program than of a benchmark**, and the
   announcement audience will find this with the first program they type. `guard`, `raise`,
   `with-exception-handler`, `parameterize` and `dynamic-wind` are all "control globals", and the
   reachability rule declines every procedure in a unit that can *transitively* reach one. A
   `guard` in one utility procedure therefore interprets every caller of that utility. Benchmarks
   avoid these forms; applications use them everywhere; the compiler's own Scheme was written to
   avoid every one of them, which is the cost made visible. This is the overfitting shape the
   project already found once (R23). It is not urgent for the first user, whose library compiles
   61 of 61, but it must be measured and largely fixed before the announcement.

Everything else in this document is detail behind those three points, plus a list of smaller
things overlooked.

---

## 2. What this assessment rests on

Read in full: `ROADMAP.md`, `docs/compiler_design.md`, `docs/compiler_plan.md`, the founding
analysis and the last sixteen findings (R60–R75) in `docs/compiler_findings.md`, the original
staged plan in its appendix, `docs/architecture.md`, `docs/Interoperability.md`, the debugger
requirements and design documents, the module headers of every file in `src/compiler/` and of
`unwind.js`, the last two walkthroughs in `CHANGES.md`, and the generated JavaScript for `assq`.

Run or checked directly:

- `npm test` on the current tree: **3,473 passed, 0 failed, 7 skipped.**
- A probe compiling procedures that call plain JavaScript functions with exact integers, flonums,
  bignums and lists, in both tiers (§4.2 F). Both tiers agree on every case.
- `interpreter.js`: the synchronous `run` loop has no `isPaused` check; only `runAsync` has one. A
  nested run started by `runWithSentinel` — which is how compiled code calls an interpreted closure —
  is always the synchronous one. This is the basis of point 2 above; it was established by reading,
  not by running a debugger session.
- Branch divergence: `debugger-take-3` and `compiler-investigation` share a merge base from
  2026-02-10 and are 31 and 33 commits apart; 49 files under `src/` differ, including
  `interpreter.js`, `frames.js`, `values.js`, `analyzer.js` and every file in `src/debug/`.
- Bundle sizes as built today: `dist/scheme.js` 3.04 MB, `dist/scheme_compiler.js` 2.03 MB.
- Published sizes of comparable in-browser runtimes, from the sources listed in §4.2 E.

---

## 3. The constraints, scored

| Constraint | Status | Notes |
|---|---|---|
| **1. JS interop** | **Met** | Same value representation in both tiers; compiled procedures are the same callable functions; the three boundary-conversion bugs found (R26, R46, R48) are fixed and tested. Verified today for calls out to plain JavaScript in both tiers. No interop *benchmark* exists yet (task 40). |
| **2. Browser and CLI** | **Met** | Prebuilt tables make strict-CSP pages work with compiled libraries and no `new Function`. The compiler is split out of the main bundle. Cost: 3 MB of JavaScript on every page (§4.2 E). |
| **3. REPL in both** | **Met in principle, not in practice** | Compilation is a backend after `analyze`, so nothing about the REPL breaks. But no REPL compiles anything; a user typing `(define (fib n) ...)` gets the interpreter. |
| **4. Debugger in both** | **Not met for compiled code; partially regressed for interpreted code** | Compiled procedures have no debug points, no source map, and no presence in the interpreter's frame stack. Breakpoints inside them are now reported as inert (task 13), which is the right interim behaviour. The nested-run pause problem (§4.2 C) affects interpreted callbacks under compiled library procedures today. |
| **5. Multi-shot `call/cc`** | **Met, with two refused shapes** | The capture protocol, resumable twins, boxing of assigned locals and `MovedFrames` are correct on every canonical program. Refused loudly: a capture across more than one compiled/interpreted boundary, and one beneath a redefined inlined primitive. Both are unreachable only because user code is never compiled; enabling the tier makes the first one reachable by any program. |
| **6. R7RS-small compliance** | **Nearly met; the compiler adds nothing to the gap yet** | Known: `string-set!`/`string-fill!`, `equal?` on cycles, dotted identifiers, `read-char` returning strings, `call-with-port` missing, referential transparency of macro-introduced free identifiers (R69). The refused capture shape above would become a seventh once user code compiles. |

---

## 4. Is the design right?

### 4.1 The calls that are right, and why they should not be reopened

**JavaScript source, not bytecode and not WebAssembly.** Correct for the platform and correctly
argued in the design document. Wasm without stack switching would force the same continuation
machinery plus a boundary crossing per interop call. Revisit only if stack switching ships and
constraint 1 can be preserved across it.

**Lowering from the analyzed AST.** The single cheapest decision in the project: macro expansion,
hygiene, renaming and internal-define hoisting are inherited, so the two tiers agree on meaning by
construction. Every later coverage win (R45–R48) was possible because the front end was shared.

**Calling convention B.** Native JavaScript stack for non-tail calls, trampoline (now with direct
calls inside a stack budget) for tail calls, cooperative unwind for capture. Chosen by a bake-off
with numbers and a stack-shape probe. It is faster than the alternative *and* it is the only
convention under which a JavaScript debugger can show a Scheme stack — which, now that the
extension is out of scope, is the only browser debugging story for compiled code. The cost, every
procedure emitted twice, is real and is being managed (lifting, liveness, and task 38).

**The interpreter as a permanent tier.** This is what makes partial coverage safe, CSP-strict
deployments possible, differential testing meaningful, and full-fidelity debugging always
available. It should stay permanent even after the tier is on by default.

**Rebinding noticed by the interpreter rather than checked by compiled code.** The per-name
primitive cell (`W.intact`) and the per-global value cell are the two representation changes that
recovered most of the guard cost. Both are sound by construction and elegant.

**Measure the ceiling, then design.** R61, R66, R68, R72, R73, R75: each optimization was preceded
by an unsound upper-bound measurement, and two proposed optimizations were dropped because the
ceiling was zero. This is the single best habit in the project and should be protected.

### 4.2 Design risks and open questions

#### A. The tier boundary is the design's cost centre

Almost every hard bug since increment 2 lived at the seam between a compiled JavaScript frame and
an interpreter frame: three value-conversion bugs, the `maze`/`btsearch` unsoundness, the boxing
bug, the port-closing bug, the frame-copying blow-up, and now the nested-interpreter recursion
limit (task 31). Each fix added a special case to shared machinery (`run`, `continueApplication`,
`unwind.js`, `suspendFlush`/`restoreFlush` at eleven call-back sites).

None of this says the design is wrong. It says the boundary must become *general* before the tier
is switched on for users, because user code multiplies the number of boundaries: REPL definitions,
`eval`, declined procedures, higher-order library calls with user callbacks, and JavaScript
callbacks all create them.

The specific gap: a capture across more than one boundary is refused. That is a valid R7RS program
being rejected. The design document's own words are that refusing valid R7RS is not acceptable.
Task 31 (unwinding through nested interpreters) is the same problem and should be treated as a
**dependency of task 30**, not a follow-up to it. The general form is that every nested `run`
participates in the unwind: when it receives `UNWIND` from compiled code it called, it appends its
own frame-stack segment to the capture in progress and returns `UNWIND` to *its* compiled caller.
That is one change to `run`, and it also removes the alternating-recursion depth limit.

#### B. Debugging compiled code has no design

The plan names two mechanisms, and `compiler_design.md` explains why both are needed. What neither
covers:

- **Stack traces.** The interpreter's `FSTACK` has no entries for live compiled frames; `:bt` at a
  breakpoint inside a callback called from compiled `map` shows a hole where `map` and its callers
  should be. Convention B was chosen so that the *JavaScript* stack has one frame per Scheme frame,
  but nothing today reads the JavaScript stack back into `StackTracer`. Options: parse
  `Error().stack` (the functions are named via `markProcedure`), or emit enter/exit debug points
  when a debug runtime is attached. Either needs designing and measuring.
- **Step-into across the tier boundary.** Stepping into a compiled callee from interpreted code
  must either behave as step-over or deoptimize the callee on demand. The latter is feasible — the
  debugger knows the callee closure at the step — but it requires the interpreted closure to still
  exist, which task 29 already notes it does not.
- **Scope inspection.** Compiled locals are renamed (`s_x_$1792`), boxed (`s_x[0]`), lifted into
  factory parameters, or live only as `$t` temporaries. `StateInspector` walks `Environment` maps.
  A mapping from generated names back to source names is required even for the source-map path.
- **The CLI.** Source maps help only where there is a JavaScript debugger to consume them. The CLI
  REPL's debugger works through the `step()` hook, which compiled code never enters. For the CLI,
  decline-to-optimize is the *only* mechanism, so it has to be complete: breakpoint set → procedure
  re-interpreted; step-into → callee re-interpreted; and both need the retained interpreted closure.

With the extension out of scope, the end state is two contexts, and it is short enough to write in
an afternoon as a section of `compiler_design.md`:

| Context | Interpreted code | Compiled code |
|---|---|---|
| CLI REPL | the existing `:break`/`:step`/`:bt` debugger | decline-to-optimize, per procedure, on breakpoint or step-into |
| Browser | the existing REPL debugger (cooperative, under `runAsync`) | DevTools, through source maps, plus decline-to-optimize for bindings a source map cannot resurrect |

The v1 policy, *debugger enabled ⇒ user code interpreted*, is the top-left and bottom-left cells
alone, with the library still compiled, and it is what point 1 of the verdict proposes shipping
first. A one-day spike that attaches a `//# sourceURL` to each generated unit and opens DevTools
on a compiled recursion would also confirm, cheaply, that the convention-B promise holds end to
end. The stack-shape probe in `experiments/stage2a/` measured the shape, not the relabelling; the
original plan said to verify the relabelling before deleting the extension, and it has not been.

#### C. A pause inside a synchronous nested run is not honoured

`SchemeDebugRuntime.pause` sets state and fires `onPause`; it does not throw. Only `runAsync`
checks `isPaused` after a step. Every call from compiled code into an interpreted closure runs in a
synchronous nested `run`. So in the browser REPL's debug mode, a breakpoint inside
`(for-each (lambda (x) ...) xs)` is *reached* on every element, `pause` is called on every element,
and execution does not stop until `for-each` — now compiled — returns. Before the standard library
was compiled, `for-each` was interpreted and the breakpoint stopped where it should.

This was inferred from the code, not exercised. It deserves a test (there is a `map` case in
`async_mode_functional_tests.js` but it asserts results, not pausing) and then a decision: make the
synchronous `run` able to pause, or accept and document it. The extension branch's synchronous
path used a `debugger;` statement for exactly this, which is worth cherry-picking as an idea even
though the branch itself is not being merged.

#### D. Exception handling and dynamic binding are treated as continuation capture

`control-globals` in `ir.scm` lists `dynamic-wind`, `with-exception-handler`, `raise`,
`raise-continuable`, `guard`, `make-parameter` and `parameterize` beside `call/cc`. `guard` expands
through `call/cc` in `control.scm`; `parameterize` through `dynamic-wind`. Under `safety.js`, a
procedure that can reach any of them — directly, through another procedure in the unit, or through
an interpreted closure in the environment — is declined.

On the canonical suite this costs 21 `call/cc` declines and 7 `with-exception-handler` declines,
because benchmarks are written to avoid such things. On an application it will cost most of the
program: one `guard` in a parsing helper declines the parser and everything that calls the parser.
The compiler's own Scheme is written to avoid every one of these forms, which `ir.scm`'s header
records as "what the subset costs to write in".

Two separate fixes, both standard:

- **Split the list.** `guard`, `raise`, `with-exception-handler`, `parameterize`, `dynamic-wind`
  and `exit` need only *escaping*, one-shot, upward control transfer. That can be a JavaScript
  `throw` caught at the establishing frame, with `dynamic-wind` after-thunks run on the way out —
  the way most compiled Schemes and every JavaScript Scheme implement them. Only `call/cc` needs the
  unwind protocol. None of the escaping forms should poison reachability.
- **Re-measure the reachability rule on real code.** It was kept, after the capture protocol made
  it unnecessary for soundness, because `btsearch` ran at 0.5x with everything compiled. That is one
  backtracking program. The rule's cost on programs with error handling in their utilities has not
  been measured because no such program is in the corpus. Given fact 2, the available real code is
  the repository's own test `.scm` files, the SRFI libraries, and the compiler; a decline-reason
  histogram over them is an afternoon, and it should be run before the announcement.

The same applies to compiled `call/cc`, which works but is off by default because `btsearch` and
`ctak` got slower while `contfib` and `threads` got faster. A program whose main loop reaches a
coroutine or generator built on `call/cc` — a common teaching idiom — runs interpreted in its
entirety through the reachability closure. The same measurement answers both.

#### E. Code size, against comparable runtimes

`dist/scheme.js` is 3.04 MB today (the roadmap's 2.67 MB predates tasks 26 and 27), about 340 KB
gzipped; the compiler is another 2.03 MB when a page needs it. The generated `assq` shows where it
goes: every inlined `car` is a guarded conditional of about 120 characters, every call site carries
a raw-call check, a stack-room store, a trampoline loop and an unwind check, and the whole
procedure exists twice.

Where that sits among runtimes people load in a browser, from published figures:

| Runtime | Size |
|---|---|
| **Scheme-JS today** | 3.04 MB always loaded (~340 KB gzipped), plus 2.03 MB compiler on demand |
| BiwaScheme 0.7.1 | ~250 KB |
| LIPS | ~150 KB JS plus ~100 KB Scheme |
| Gambit's JavaScript backend, browser REPL | 11–22 MB |
| Chibi via Emscripten | "a big file", slow load; no figure given |
| Brython | 869 KB minified plus 4.55 MB stdlib |
| Pyodide | ~6.4 MB for a minimal REPL (2021); 24.6 MB full build |
| Guile Hoot | designed to avoid multi-megabyte hello-worlds; no figure found |

Scheme-JS sits between the small interpreters and the heavyweight runtimes. That is fine for
embedding in an application and for the announcement, where the comparison is with Gambit's
22 MB; it is a poor fit against BiwaScheme's 250 KB for "drop a script tag on any page". The
always-loaded SRFI tables are 1.1 MB of it, so loading a library's table on import (task 38) is
the lever if the always-loaded bundle should ever be under 1 MB raw. Given facts 1 and 2, size
should not be spent on before the tier reaches its first user.

Task 38 says measure first, which is right. Three cheap things to measure early: how many
procedures can never be suspended (no callee can call back into Scheme) and so need no twin;
whether the twin can be emitted as a string and materialised on first capture where `new Function`
is allowed, keeping the eager form under CSP; and the on-import table loading above, which needs an
asynchronous import path the interpreter does not have.

Sources: [try.scheme.org file sizes issue](https://github.com/schemeorg-community/try.scheme.org/issues/3),
[gambscript README](https://github.com/ultraschemer/gambscript/blob/master/README.md),
[BiwaScheme](https://github.com/biwascheme/biwascheme),
[Pyodide stdlib discussion](https://discuss.python.org/t/minifying-the-stdlib-in-pyodide/8414),
[Pyodide minimal build sizes](https://lightrun.com/answers/pyodide-pyodide-optimize-the-size-of-minimal-pyodide-build),
[Brython sizes](https://groups.google.com/g/brython/c/clL21M_Tb98),
[Hoot design](https://spritely.institute/news/scheme-wireworld-in-browser.html).

#### F. Value representation, exactness at the boundary, and where the absolute gap is

Exact integers are `BigInt` everywhere in Scheme. That is the reason `bignum` is 1.2x and `sum` is
bounded by "V8 BigInt add is the floor". What happens at the JavaScript boundary was probed today,
in both tiers, which agree on every case:

| Case | Result |
|---|---|
| Scheme `1` passed to a JS function | JS `number` 1, indistinguishable from `1.0` — as the user recalled deciding |
| JS number 1 returned into Scheme | **inexact**: `(exact? (id 1))` is `#f`, prints `1.0`, `(eqv? (id 1) 1)` is `#f` |
| exact integer beyond ±2^53 passed to JS | throws "outside safe integer range for JS API call" |
| JS `BigInt` returned into Scheme | stays exact |
| list `(3 6)` handed to JS | JS sees `3n` and `6n` inside the pairs; only a bare return value is converted |

So a round trip through JavaScript loses exactness, by design, and the boundary conversion is
shallow with respect to pairs. Neither is a bug; both should be stated in `Interoperability.md`,
whose table still says numbers are "raw JS Number, 1:1 mapping", which has not been true since the
numeric tower.

The consequence for fixnums-as-JS-numbers (task 41): the JavaScript-side decision does not settle
the Scheme-side one. Inside Scheme, `1` and `1.0` must still be distinguishable, and today that
distinction is `typeof`. If small exact integers became JS numbers, inexact numbers would need a
tag instead: box every flonum, which costs the class that is currently the best; or box only
integral-valued flonums so a non-integral number stays unambiguous, which then requires
classifying every inbound JS number and makes integral-valued float arithmetic pay. Either is a
representation design with its own measurement, so the plan's "profile first" gate stands.

What is missing is the number that would decide it, and that fact 1 asks for anyway: **the
compiled tier has never been reported against Gambit, Racket, or plain JavaScript.** Every figure
in `ROADMAP.md` is "against the interpreter". `compare_r7rs.js` exists. From the two published
tables one can infer that the compiled tier is roughly five to nine times faster than Gambit's
*interpreter* on call, fixnum and flonum code — but that is an inference across two dates and two
configurations, not a measurement, and it says nothing about Gambit's compiler or Racket CS, which
is what an announcement audience will ask. The founding question was a 650x gap to plain
JavaScript; nobody has written down how much of it is closed. That table, at the canonical sizes
the validity review (R23) noted nothing yet uses, and in the configuration users will actually
run, is the announcement artifact and should be produced as soon as the tier is on for user code.

#### G. Self-hosting: benefits, costs, and the analyzer

Writing the compiler in Scheme was the right dogfooding decision — it found the interpreted-library
cost (R45), the `apply` and `values` blocks (R46, R48), the `let`-chain blow-up (R47), and it makes
the compiler the tier's most demanding customer, which under fact 1 is exactly the customer that
matters. Its measured costs: lowering ~18x slower than the JavaScript it replaced, code generation
7.5x slower (1.2 ms per procedure), a 2 MB compiler module, and every compiler edit needing a
regeneration step before it takes effect.

Two consequences to make explicit:

- **Compile latency now matters for the REPL and for pages.** At ~1.5 ms per definition, a page
  with 500 definitions spends nearly a second compiling. The plan has no item for a compilation
  *policy* (compile on definition, on second call, in idle time). Task 30 needs one. Because the
  compiler's own code generation is the slowest part and is itself compiled code, this is also the
  first place where the tier's speed is the implementation's own speed.
- **The analyzer port (39) is a different project from the compiler.** It is 2,014 lines including
  the sets-of-scopes expander, it is what the debugger, the library system and `define-macro` all
  hang off, and porting it properly requires the phase separation of task 42 first. It serves the
  self-hosting ideal, not any of the six constraints. It should be marked as aspirational and
  coupled to 42, so that it is never started by accident as "the next port".

#### H. The extension branch

Given fact 3, `debugger-take-3` should not be merged. It shares a merge base of 2026-02-10 with
this branch, differs in 49 files under `src/`, and its two pausing channels — cooperative under
`runAsync` and a `debugger;` statement in a probe runtime — both assume every procedure runs
through `step()`. Recommended: mark the branch abandoned in `ROADMAP.md`, correct the delivered
table's "A debugger, twice" row (the extension is not on this branch), and cherry-pick two things
before closing it: the Puppeteer harness, which is the shortest path to browser tests in CI (§5.2),
and the synchronous-pause idea (§4.2 C).

#### I. Strict Content-Security-Policy

A page's CSP is an HTTP header or meta tag the browser enforces. A strict one omits
`'unsafe-eval'` from `script-src`, after which `eval`, `new Function` and string timers throw. Who
sets it: security-conscious and enterprise sites, Chrome extensions (Manifest V3 forbids it
outright), some hosting platforms. GitHub Pages does not, so the demo is unaffected.

The ramification is only that the compiler, which materialises generated source with
`new Function`, cannot run on such a page. The project already handles this: prebuilt tables
install compiled libraries without generating code, `loadCompiler` reports itself unavailable, and
user code stays interpreted. Given fact 5 the recommendation is to keep that as a
"degrades gracefully" guarantee — it is already paid for, and one test enforces it by making
`Function` throw — but not to let it veto design options. A lazily materialised resumable twin
(§4.2 E) can be a non-CSP optimisation with the eager form as the CSP fallback.

---

## 5. Is the approach right?

### 5.1 What is working and should be protected

- **The measurement discipline.** Per-class reporting, ceiling-before-design, targeted construct
  benchmarks in both tiers, whole-program correctness under both tiers inside `npm test`, and the
  self-host differential. Few compiler projects of any size have this.
- **The findings log.** Sixteen of the last sixteen entries record a belief that was wrong and how
  it was found. That is the mechanism by which the project corrects itself, and it works.
- **The document discipline.** Lifetimes, one-way links, comments that stand alone. The rewrite of
  the planning documents (task 20 era) fixed a real problem.

### 5.2 Where the approach has a gap

**Bugs are found by benchmark correctness checks, not by tests.** R26 (ten programs silently wrong),
R49's boxing bug (a wrong total from `threads`), all three of R51's bugs, and the `read1` port bug
were each caught by a whole-program check and missed by two to three thousand unit tests. The
pattern is structural: unit tests are written for the shape the author has in mind, and the
unwind/flush/twin machinery fails on shapes nobody had in mind. The generalisation of what worked
is a **differential fuzzer**: generate random programs over the compiled subset (loops, closures,
captures at random sites, assignments, multi-shot re-entry, deep recursion), run under both tiers,
compare. This is a few hundred lines and would have found every one of the bugs above. It belongs
in the plan ahead of enabling the tier for users.

**CI does not cover this branch, the browser, or the compliance suites.** `ci.yml` runs on pushes
to `main` only, runs the old numeric-tower benchmark rather than the canonical suite, and never runs
the browser test page — the "3,368 in the browser" count is hand-run. The 219 + 982 conformance
tests (task 33) are outside `npm test` and, as far as the documents say, are run with the
interpreted standard library, not the compiled one the browser ships. Task 33 is an afternoon and
should be done now, in both library configurations. The extension branch's Puppeteer harness is
the shortest path to browser tests in CI.

**Velocity is high and the surface is wide.** Twenty-seven tasks between 2026-09-18 and
2026-09-25. Each was measured and tested, and the tree is green, so this is not a complaint; it is
a note that the unwind/flush machinery has been changed in nearly every one of the last six tasks
and the fuzzer above is the cheapest insurance against that rate.

---

## 6. Is the plan the right plan, in the right order?

### 6.1 What the current order optimises for

Reading tasks 28–45 as a sequence, the plan is ordered to keep the canonical suite's per-class
numbers moving while paying down debts found on the way. That is how it was built and it has
worked: every class but two is 20x or more over the interpreter.

What it does not optimise for is **reaching the first user**, which fact 1 says is this
implementation's own REPLs and build, followed by an announcement with public numbers. The tier
has never compiled a line of user code, the reason is a gate (29) that is itself undesigned, the
announcement numbers do not exist, and the task that would expose the tier's real-program
coverage (§4.2 D) is not on the list at all.

### 6.2 Proposed order, with reasons

Not a rewrite of the plan: a proposal for `compiler_plan.md` to accept, amend or reject, entry by
entry. Numbers in brackets are the plan's. The first seven are "reach the first user"; the next
three are "be ready to announce"; the rest are as today with reasons adjusted.

| # | Task | Why here |
|---|---|---|
| 1 | Errors raised inside compiled code [28] | Already first; cheap, user-visible, and a precondition for anyone debugging compiled code by reading messages. |
| 2 | **Compliance suites into `npm test`, both library configurations** [33] | An afternoon. Constraint 6 is untested in the configuration the browser ships. Do before anything that touches semantics. |
| 3 | **Differential fuzzer across the two tiers** (new) | The generalisation of the only technique that has found the tier's serious bugs. Before the tier reaches users. |
| 4 | **Unwind through nested interpreters** [31] | Promote to a dependency of 30: it is the refused R7RS shape, and user code makes it reachable. |
| 5 | **Debug-mode design, and the v1 policy "debugger on ⇒ user code interpreted"** (new; §4.2 B, C) | The two-context table written into `compiler_design.md`; the v1 policy needs almost no code; retain the interpreted closure when a procedure compiles (the half of 29 everything else needs); test, then fix or explicitly defer the nested-run pause. |
| 6 | **Enable the tier for user code** [30] | CLI script, browser script tag, both REPLs, with a compilation policy (when to compile, and lazily). The first task with a user on the other end, and that user is the REPL you use every day. |
| 7 | **Top-level expressions compiled as thunks** (new; §7) | A script's top-level loop is the commonest thing a REPL user times, and it is never compiled today. Small, and it belongs with 30. |
| 8 | **Compiled tier against Gambit, Racket and plain JavaScript, per class, at canonical sizes** (new; §4.2 F) | The announcement artifact, and the number that decides between further code generation and a representation change. |
| 9 | **Decline policy on real code** (new; §4.2 D) | Decline-reason histogram over non-benchmark Scheme; split escaping control forms from `call/cc`; compile `guard`/`raise`/`parameterize`/`dynamic-wind` as escapes; re-justify or narrow the reachability rule with numbers. Before the announcement, because it is the first thing a new user's program will hit. |
| 10 | Source maps and debug points [37] | The browser debugging story for compiled code now that the extension is out of scope. Start with the cheap spike (`sourceURL`, named functions, DevTools on a compiled recursion) to confirm the convention-B promise; then column mappings. |
| 11 | Debug by not optimizing, per procedure [29] | Now a refinement of 5 that recovers speed under the debugger, and the CLI's stepping mechanism. |
| 12 | Smaller generated code [38] | Page weight; §4.2 E gives the comparison. Measure the never-suspended fraction first, as the entry says. Not before the announcement unless the try-it page is judged too slow to load. |
| 13 | Profile bignums; fixnums as JavaScript numbers [32, 41] | After 8 says where the absolute gap is. §4.2 F states the representation consequence that must be designed before 41 starts. |
| 14 | Finish the benchmark suite [40] | The interop axis matters most: it is the product's distinguishing constraint and has no benchmark. Canonical sizes are part of 8. |
| 15 | Hygiene and phase separation [42] | A compliance bug in both tiers, and the true prerequisite of any expander in Scheme. Middle of the list, not the bottom. |
| 16 | `call-with-port`; mutable strings [34, 43] | Compliance items with no dependency; do when a compliance pass is convenient. |
| 17 | `safety.js` and the compile policy to Scheme [35, 36] | When next touched, as the entries already say. |
| 18 | The analyzer [39] | Mark aspirational; couple to 42; gate as today. |
| 19 | `source` on `Cons`; speculative code generation [44, 45] | Bottom, as today. |

And three decisions outside the task list, to record in `compiler_plan.md`'s "Decided" section
and, where user-visible, in `ROADMAP.md`:

- `debugger-take-3` is abandoned; source maps are the browser debugger for compiled code (fact 3,
  §4.2 H).
- The refused capture shapes are not acceptable in a shipped tier; task 31 is a hard gate on 30.
- Strict CSP is a degrades-gracefully guarantee, not a design constraint (fact 5, §4.2 I).

---

## 7. Overlooked, or not yet on any list

Smaller than the above; each is a candidate plan entry.

- **Top-level expressions are never compiled.** 229 of the corpus's 250 declines are "not a
  procedure". A script's top-level `(do ...)` or `(let loop ...)` runs interpreted however good the
  tier is. Wrapping a top-level expression as a thunk and compiling it is straightforward; it is
  item 7 above.
- **Compile latency policy** (§4.2 G). No item exists.
- **`Error().stack` readability.** Generated functions are anonymous factories; `markProcedure`
  names the fast form but nothing names the unit. A `//# sourceURL=scheme:<library>/<procedure>` per
  unit is one line in the emitter and makes every DevTools stack trace and profile readable
  immediately, ahead of source maps. It also makes the compiler's own profiles readable, which
  fact 1 makes worth having.
- **The nested-run pause** (§4.2 C) needs a test regardless of the decision.
- **Interpreted closures discarded on compilation.** `env.define` overwrites them and nothing keeps
  a handle. Every debugging mechanism needs them back; this is the one piece of task 29 that should
  be done first and separately.
- **`Interoperability.md` is stale on numbers.** Its table says Scheme numbers are raw JS numbers;
  §4.2 F records what is actually true, including the two asymmetries.
- **Allocation profile.** Boxing, `MovedFrames`, `TailCall` objects and argument arrays are all
  allocations; GC share has not been reported since R0's 3.6%. A note in the profile discipline.
- **`ROADMAP.md`'s numeric-performance table** is the pre-compiler list (precompute `MIN_SAFE_BIG`,
  LRU cache) and no longer reflects what R29/R48 found about where numeric time goes. It is marked
  deferred, but a reader will take it as the plan.
- **`ROADMAP.md` claims a delivered Chrome extension** that is absent from this branch and, per
  fact 3, is no longer a goal.
- **Delimited continuations** are listed as a future item for async. If `guard`/`parameterize` move
  to an escape implementation (§4.2 D), the cheapest path to `shift`/`reset` later is through the
  same one-shot mechanism; worth a sentence in the design when that work happens.
- **Multiple values returned to JavaScript** collapse to the first value. Documented behaviour, and
  fine; noted because compiled code's `raw` mode had exactly this bug once (R48).
