# Compiler design

How the Scheme-to-JavaScript compiler tier works, and why it is built this way.

**This document can be rewritten.** That is the point of it existing separately: the design changes,
and it used to live inside an append-only log that structurally could not hold it — which is how a
list of three "known-broken things" stayed in the documentation for months after all three were
fixed.

Three companions, and the division is by lifetime:

| | lifetime | answers |
|---|---|---|
| this document | rewritable | how does it work, and why |
| [compiler_plan.md](compiler_plan.md) | living | what next, blocked on what |
| [compiler_findings.md](compiler_findings.md) | append-only | what did we believe that was false |
| [../CHANGES.md](../CHANGES.md) | append-only | what happened, increment by increment |

## What belongs here

Only the reasoning **no single module can own**. The calling convention spans the emitter, the
resumable form, the interpreter's frames and the runtime — no header owns it, so it is here. Why
`letrec` self-reference needs no box is one decision inside `lift.js`, so it is in that file's
header and not here.

The rule is checkable, and it matters because module headers are the *freshest* rationale in the
project — the "comments must stand alone" convention forces them to be edited with the code.
Duplicating a header here would rot it. Where this document needs that detail it links.

---

## Why a compiler at all

`fib(30)` took 6.5 seconds against 10 ms for the same program in plain JavaScript, and a CPU profile
put **~95% of runtime in interpretive overhead and ~2.3% in the program's actual arithmetic**.
Gambit's *interpreter* was 27x faster than us, so being an interpreter accounted for maybe a quarter
of the gap and the rest was how ours was written. The full numeric tower — the thing the roadmap had
queued optimizations for — cost about 3x.

Full detail, including the measurements and the parts of that analysis later overturned, is R0 in
the findings log.

## Target: JavaScript source

Not a bytecode VM: writing an interpreter loop in JavaScript that V8 cannot optimize, in order to
avoid emitting JavaScript that V8 *can* optimize, is self-defeating on this platform.

Not WebAssembly: stack switching has not shipped, so Wasm offers no continuation primitive — the
same machinery would have to be built anyway, *plus* a boundary crossing on every JavaScript interop
call, which is constraint 1. Worth revisiting if stack switching ships.

## Calling convention B

The pivotal decision, settled by a bake-off (`experiments/stage2a/`) rather than by argument.

Non-tail calls use **the native JavaScript stack**. Tail calls return a `TailCall` to a
**trampoline**, so tail recursion runs in constant space. Continuation capture runs a **cooperative
unwind** — Pettyjohn et al.'s generalized stack inspection, with Marshall's modification replacing
the thrown exception with a distinguished return value, which is what removes the technique's
historical weakness.

The alternative (A) kept every continuation frame in a JavaScript array under a trampoline. It is
the faster-measured design in the published literature and it was rejected for a reason that is not
about speed: **the JavaScript call stack would be one frame deep**, so DevTools could never show
Scheme frames, and the debugger extension could never be retired. Under B, one live Scheme frame is
one JavaScript frame and tail calls correctly add none.

The cost of B is procedure fragmentation, which is the next section.

## Every procedure is emitted twice

Straight-line JavaScript is fast and impossible to re-enter in the middle. So each compiled
procedure is emitted as:

- **the fast form** — ordinary JavaScript, what actually runs;
- **the resumable twin** — a state machine over that procedure's own call sites, entered as
  `($pc, $f)`.

The twin runs *only* during continuation reinstatement, so it is allowed to be slow. The cost is
code size at compile time rather than speed at run time: measured at 2.21x, against 4.09x predicted.

`src/compiler/resume.js` subclasses the fast-path emitter and overrides **only control flow**. All
expression emission — inlining, global accessors, temporaries — is inherited, so the two forms
cannot drift apart in what they mean. The one thing that must be kept aligned by hand is temporary
*naming*, because the fast form spills into a frame the twin restores; getting that wrong produced
a bug where a nested `$fn0` shadowed its parent's twin and reified into the wrong one.

## The capture protocol

Owned by `src/core/interpreter/unwind.js`, deliberately on the interpreter side: the interpreter
must not depend on the compiler.

`call/cc` begins an unwind by returning a distinguished sentinel. `run` propagates it. Each compiled
frame on the way out **reifies itself** — saving its locals and which call site it had reached — and
the interpreter splices the resulting frames in where the tier boundary sat. Invoking the
continuation re-enters each compiled procedure through its twin at the saved `$pc`.

Multi-shot works because frames are copied rather than consumed.

**A frame saves only what is live where it resumes** (`src/compiler/liveness.js`). Saving every
local at every suspension point was quadratic — frame literals were 57% of all generated code in
the benchmark corpus — and a frame only needs what can still be read after it resumes. Three
properties make that safe, and they belong to three different modules, which is why they are
recorded here:

- **The analysis runs over the resumable form's emitted statements, not the IR**, because what
  must survive a suspension includes JavaScript temporaries the IR has no name for: in
  `(list (one) (capturer))`, the result of `(one)` is live across `(capturer)`.
- **A spill reads what is live at its resume block.** A capture has no ordinary control-flow edge
  to the code after it — it spills and returns, and the frame is the only path. Treat the spill as
  anything less and a value read only after a capture is judged dead before it.
- **No nested function closes over a frame's locals by reference.** Lambda lifting hands every
  nested procedure its free variables as factory arguments, so creating one is a visible read. An
  inline closure could instead read a local whenever it was *called*, invisibly; if one ever
  appears, the resumable form falls back to saving everything.

The restore side is unchanged and names every local. One that was not saved destructures to
`undefined`, which is safe precisely because it is dead there.

One shape is genuinely unsupported and is **refused rather than answered**: a capture crossing more
than one boundary between compiled and interpreted code. A second is refused beneath a redefined
inlined primitive. Both throw with an explanation. They are currently unreachable because user code
is never compiled; see `compiler_plan.md`.

## Boxing, and why copying was wrong

A spilled frame copies each local's value, but Scheme *shares* the binding: an assignment made after
a continuation is captured must be visible when that continuation is invoked again, and to every
closure over the same variable. Copying is right for temporaries, which are always written before
they are read, and wrong for anything the program can name.

So an assigned local is held in a one-element array, and a spilled frame copies the reference.
Measured cost: 2–7%. The alternative — declining every procedure with an assigned local — cost
16.05x on the declining-cost vector against 4.40x for boxing.

Which locals must be boxed is decided in one place, `src/compiler/lift.js`, from a single walk over
the IR. That includes cases lowering has no view of: `letrec` names a sibling refers to, and
internal definitions a nested procedure refers to.

## Lambda lifting

A procedure is emitted twice, and so is every procedure nested inside it, once within each form of
its parent — so a lambda at depth *d* appeared about 4^d times. Measured at 4.2x per level.

Each nested procedure is now emitted **once, at the top level**, as a factory over its free
variables, created with `$t5 = $mk$fn0(s_a, s_b)`. Variable *references* do not change, which is what
makes it cheap: the inner function closes over the factory's parameters, which already have the names
its body used. Code size becomes linear in the program rather than exponential in its nesting.

Free variables can be passed by value because an unassigned one cannot be observed to differ from a
copy, and an assigned one is already a box — so what is passed is the box. `letrec` is the whole
difficulty and `lift.js`'s header explains it.

## The IR, and lowering

Lowering consumes the **analyzed** AST, not source — so macro expansion, hygiene, alpha-renaming and
internal-definition hoisting are already done, and compiled and interpreted code agree on the meaning
of a program by construction rather than by two front ends being kept in step.

It computes the two things code generation needs and the analyzed AST does not record: **tail
position** (the analyzer makes every application a `TailAppNode` regardless) and **local versus global
reference**.

Lowering is **partial on purpose**. Anything outside the compiler's subset is declined and left to the
interpreter, because a compiler that must handle everything before it handles anything cannot be
shipped incrementally or trusted early.

The pass is Scheme: `src/compiler/ir.scm`, reached through `src/compiler/lowering.js`.

## Self-hosting, and the bootstrap

The compiler is meant to end up in Scheme, because a Scheme compiler good enough to compile a Scheme
compiler is the goal and it cannot be argued from priors. The lowering pass is the first module
across.

A compiler written in the language it compiles has to start somewhere. It starts in the
**interpreter**, which runs `ir.scm` from source with no compiler at all:

| step | produces |
|---|---|
| the interpreter runs `ir.scm` | a working, slow compiler |
| it compiles the standard library | `src/packaging/compiled_stdlib.js` |
| that compiles `ir.scm` | `src/packaging/compiled_compiler.js` |

`npm run prebuild` runs the chain in 0.62 s from nothing, reproducibly — both tables come out
byte-identical.

**The order is the design, not an optimization.** Lowering calls `memq` and `assq` on every scope
lookup and every global it records, and those are themselves Scheme. Compiling `ir.scm` against an
interpreted library is worth 1.45x; against a compiled one, 13.68x. Almost all of a compiled module's
cost can be the interpreted library underneath it.

Both prebuilt tables are guarded by a fingerprint of the sources they were generated from
(`src/compiler/prebuilt.js`), so a stale build costs speed and never correctness.

`runtime.js` stays JavaScript permanently — not because generated JavaScript calls it, but because it
needs native JavaScript features that neither generated code nor Scheme libraries can express, a
`Map` behind hash tables being the clearest case. Chez keeps a C kernel for the same reason.

## What is declined, and why

Two different questions, deliberately kept apart.

**Cannot be expressed.** Lowering fails and reports a reason. The procedure stays interpreted.

**Can be expressed but should not be compiled.** A judgement needing more than one lambda to make,
because it depends on what the callees do. `src/compiler/safety.js` closes it over the call graph:
`maze`'s `make-maze` names no control global and is still held back, because `dig-maze` escapes
through it.

Since compiled frames can take part in a captured continuation, this is **no longer a soundness
device**. What it holds back, it holds back for speed: a procedure a capture repeatedly unwinds
through pays to suspend and resume every time, and on capture-heavy code that costs more than
interpreting it.

## Tiering

**The interpreter is permanent**, not transitional. It is four things at once, all of which are still
needed after the compiler is finished:

- the **CSP-safe** execution mode, where generating code is forbidden;
- the **reference semantics** for differential testing;
- the **maximum-fidelity debug tier**;
- the compiler's own **bootstrap**.

Under a strict Content-Security-Policy the configuration is AOT plus interpreter, and it needs no
`new Function` anywhere: the standard library and the compiler's own Scheme are compiled at build
time into ordinary module text. Dynamic paths — `eval`, `load`, the REPL — fall back to the
interpreter. (`js-eval` in `src/extras/primitives/interop.js` is a deliberate interop escape hatch;
it throws under strict CSP, which is acceptable and forces nothing else to depend on `eval`.)

## The four constraints, honestly

| Constraint | Status |
|---|---|
| **1. JS interop** | Met. Scheme closures stay callable JavaScript functions; compiled procedures keep the same wrapper. Value representation is untouched. |
| **2. Browser + CLI** | Met. Generated code is ordinary JavaScript; the library and the compiler are AOT-compiled into the bundle. |
| **3. REPLs in both** | Met in principle — compilation is a backend *after* `analyze`, so `analyze` stays runtime-callable and `eval`, `load` and macro expansion keep working. Not met in practice: **nothing outside `src/compiler/` compiles user code**, so a REPL never reaches the tier. |
| **4. Debuggers in both** | **Not met for compiled code.** Generated code carries no source locations and no debug points, and `src/debug/` has no notion of a compiled procedure. The only hook is `interpreter.js:455`, inside the step loop that compiled procedures never enter, so a breakpoint inside one silently never fires. Harmless only because the tier compiles nothing but the standard library today. |

Constraint 4 is the open design question of the project. The intended answer is **two mechanisms,
not one**, which is what every real toolchain ships:

- **Debug info** — source maps and emitted debug points, so compiled code can be stepped and
  inspected in place. This is what calling convention B was chosen for: one live Scheme frame is one
  JavaScript frame, so DevTools can show a Scheme stack. Until it exists that choice has been paid
  for and not collected.
- **Declining to optimize what is being debugged** — a procedure with a breakpoint in it is left to
  the interpreter, and recompiled when the breakpoint moves. The equivalent of compiling one
  translation unit at `-O0`.

The second is not a lesser substitute for the first. Lowering already beta-reduces immediately
applied lambdas into bindings, lifts nested procedures into factories, inlines primitives and boxes
assigned locals — and the optimization work still to come adds direct calls, arity specialization
and unboxing. A source map maps *locations*; it cannot resurrect a binding that no longer exists.
So debug info yields "optimized out" exactly where a user is most confused, and the interpreter
yields the real value. Sequencing is in `compiler_plan.md`.

## How this is verified

- **2,426 tests**, Node and browser, via `npm test`.
- **Whole-program correctness**: 41 canonical programs run end to end under *both* tiers and checked
  against expected results that came from Gambit — `npm run test:programs`, 8.2 s, inside `npm test`.
  This is the check that catches what unit tests structurally cannot: three compiler defects in one
  increment were found here and missed by 2,344 unit tests.
- **Cross-tier differential on the compiler's own source**: `npm run benchmark:self-host` lowers 993
  lambdas interpreted, compiled, and compiled-with-compiled-library, and refuses to report timings
  unless all three agree. The interpreted run is the reference semantics, so a disagreement means the
  tier changed the meaning of the compiler.
- **Workload classes are never blended.** Eight classes — call, fixnum, bignum, flonum, list, vector,
  string, continuation — reported separately, because there is no average Scheme program to weight
  them against and a single number is what hid an overfitting problem before. Ship rule: an
  optimization is worth shipping when it improves at least one class and regresses none.

## Where the rest of the reasoning lives

Per-module rationale is in the module headers, which are edited with the code:

| | |
|---|---|
| `src/compiler/ir.scm` | the IR's shape; what the Scheme subset costs to write in |
| `src/compiler/lowering.js` | hosting the Scheme pass; the bootstrap in detail |
| `src/compiler/marshal.js` | the JavaScript/Scheme boundary, and how it shrinks |
| `src/compiler/lift.js` | lifting, and `letrec` |
| `src/compiler/emitter.js` | the fast form |
| `src/compiler/resume.js` | the twin |
| `src/compiler/safety.js` | the call-graph closure, and its measured trade-off |
| `src/compiler/prebuilt.js` | staleness, and why arity rather than names |
| `src/compiler/inline.js` | primitive expansions, tower-faithful |
| `src/compiler/runtime.js` | the trampoline, global accessors, procedure marking |
