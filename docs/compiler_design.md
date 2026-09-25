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
`letrec` self-reference needs no box is one decision inside `lift.scm`, so it is in that file's
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
**trampoline**, so tail recursion runs in bounded space -- except that a tail call to another
compiled procedure is made directly while the stack such calls hold is inside a budget (see *Tail
calls between procedures*). Continuation capture runs a **cooperative
unwind** — Pettyjohn et al.'s generalized stack inspection, with Marshall's modification replacing
the thrown exception with a distinguished return value, which is what removes the technique's
historical weakness.

The alternative (A) kept every continuation frame in a JavaScript array under a trampoline. It is
the faster-measured design in the published literature and it was rejected for a reason that is not
about speed: **the JavaScript call stack would be one frame deep**, so DevTools could never show
Scheme frames, and the debugger extension could never be retired. Under B, one live Scheme frame is
one JavaScript frame, and tail calls add none beyond the budget below.

The cost of B is procedure fragmentation, which is the next section.

## Every procedure is emitted twice

Straight-line JavaScript is fast and impossible to re-enter in the middle. So each compiled
procedure is emitted as:

- **the fast form** — ordinary JavaScript, what actually runs;
- **the resumable twin** — a state machine over that procedure's own call sites, entered as
  `($pc, $f)`.

The twin runs *only* during continuation reinstatement, so it is allowed to be slow. The cost is
code size at compile time rather than speed at run time: measured at 2.21x, against 4.09x predicted.

One emitter produces both, in `src/compiler/emit.scm`, with a mode. Only control flow — `if`,
calls, captures, loop heads — depends on the mode; all expression emission — inlining, global
reads, temporaries — is the same code, so the two forms cannot drift apart in what they mean.
What must still agree exactly is temporary *naming*, because the fast form spills into a frame the
twin restores by name. They agree because each counts from zero and both walk the same IR in the
same order; getting it wrong once produced a nested `$fn0` that shadowed its parent's twin.

**Statements are data.** The emitter builds each statement as a tagged list, and each expression as
a list of text and local variables, and renders them last. So which locals a statement reads is
in the data rather than recovered by scanning the text — which is what liveness, below, needs. The
emitter is Scheme, written with SRFI 1 and SRFI 152 like any other Scheme program, and it replaced
a JavaScript one after producing byte-identical output across the test suite, the benchmark
programs, the standard library and the compiler itself — except for frames that save less.

## The capture protocol

Owned by `src/core/interpreter/unwind.js`, deliberately on the interpreter side: the interpreter
must not depend on the compiler.

`call/cc` begins an unwind by returning a distinguished sentinel. `run` propagates it. Each compiled
frame on the way out **reifies itself** — saving its locals and which call site it had reached — and
the interpreter splices the resulting frames in where the tier boundary sat. Invoking the
continuation re-enters each compiled procedure through its twin at the saved `$pc`.

Multi-shot works because frames are copied rather than consumed.

**A frame saves only what is live where it resumes** (`src/compiler/liveness.scm`). Saving every
local at every suspension point was quadratic — frame literals were 57% of all generated code in
the benchmark corpus — and a frame only needs what can still be read after it resumes. Three
properties make that safe, and they belong to three different modules, which is why they are
recorded here:

- **The analysis runs over the resumable form's statements, not the IR**, because what must
  survive a suspension includes JavaScript temporaries the IR has no name for: in
  `(list (one) (capturer))`, the result of `(one)` is live across `(capturer)`. The statements are
  data with every local marked, so a local's name inside a string literal is not a read.
- **A spill reads what is live at its resume block.** A capture has no ordinary control-flow edge
  to the code after it — it spills and returns, and the frame is the only path. Treat the spill as
  anything less and a value read only after a capture is judged dead before it.
- **No nested function closes over a frame's locals by reference.** Lambda lifting hands every
  nested procedure its free variables as factory arguments, so creating one is a visible read. An
  inline closure could instead read a local whenever it was *called*, invisibly; the emitter has
  no way to write one, since every nested lambda is lifted.

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

Which locals must be boxed is decided in one place, `src/compiler/lift.scm`, from walks over
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
difficulty and `lift.scm`'s header explains it.

## Loops

The trampoline costs an allocation and a return per tail call, and a loop is a tail call per
iteration, so two shapes are compiled as JavaScript loops instead. Both are decided by lowering, in
Scheme, and handed to the emitter as flags on the IR:

- **A tail call to the procedure itself** reassigns the parameters and jumps back to the top:
  `continue` in the fast form, `$pc = 0` in the twin. For a procedure bound by `letrec` or an internal
  definition this needs the name never to be assigned. For a top-level procedure calling its own
  global, the jump is guarded on the binding, since the global can be redefined after compilation.
- **A `letrec` loop entered once, in tail position** — a named `let`, a `do`, the loop inside `assq`
  — is emitted inside the procedure that enters it, when its name is only ever called by that entry
  and by its own looping calls. Its parameters become the enclosing procedure's locals and entering it
  allocates nothing. Without this, each call of `assq` still made a closure and a `TailCall` to enter
  a loop that usually runs twice.

Reassigning parameters in place is sound only because every nested procedure is lifted and receives
its free variables by value, or by box: a closure made in one iteration holds that iteration's values.
Each iteration re-runs what a fresh call would — boxing the boxed parameters and making the boxes for
internal definitions — so no two iterations share a binding.

## Tail calls between procedures

Any other tail call used to return a `TailCall`, which allocated the pending call and an argument
array, returned through the caller, and was run by the nearest trampoline through `invoke`'s spread:
a fifth of `earley`'s time, with an eighth more in the collector. Made as plain JavaScript calls,
unsoundly and without any bound, those calls were worth 1.2-1.4x on six canonical programs.

The unsound version is unsound twice over. A chain of tail calls that never returns -- `cpstak`, the
loops in `fft` -- grows the JavaScript stack until it overflows, where Scheme requires constant space.
And a tail call to a continuation, made directly, throws to get where it is going, where a returned
`TailCall` let the interpreter reinstate it without one: `fibc` ran 2.6 times slower.

So a tail call is made directly only to a compiled procedure or a primitive -- a function marked
`SCHEME_PRIMITIVE` -- and only while the stack held by direct tail calls is under a budget,
`R.tailStack`. Anything else, and anything past the budget, returns a `TailCall` as before; a chain
that reaches the budget unwinds to the nearest trampoline and carries on from there, so it runs in
bounded space. Each call site adds the size of its own frame, which stays on the stack under the
callee, and takes it off in a `finally`.

Three things about the budget were decided by measurement rather than by argument:

- **It counts stack, not calls.** Frames differ by two orders of magnitude: a chain through a
  procedure with 60 locals filled V8's whole stack in about 730 calls, and one with 200 overflowed in
  400. The emitter knows each frame's size at compile time -- its locals and parameters plus the fixed
  part of an interpreter frame -- and the limit is an eighth of V8's default stack.
- **It is one count for the whole stack, not one per chain.** Resetting the count at every non-tail
  call would bound each chain rather than the total, and cost 3-6% on call-heavy programs for the
  store around every call; worse, a program recursing through long tail chains then keeps every
  chain on the stack at once: with a limit of 100 calls a chain, one survived 132 levels of recursion
  where it had survived 10,385. The shared count
  keeps the guarantee simple -- direct tail calls never add more than the limit to what the stack
  held -- at the price that a program recursing deeply *through* tail calls spends the budget and
  gets fewer of them. `earley` is that program.
- **It is given back in a `finally`.** An error or a continuation thrown through a direct tail call
  skips the code after the call, and a count left high sends every later tail call to the
  trampoline for the rest of the program. `try`/`finally` measured as free.

A capture beneath a direct tail call needs nothing from the caller's frame, whose continuation is its
callee's: the unwind sentinel is returned as the callee's value, and the frame is absent from the
recorded continuation, as it would have been had the call been trampolined.

Only the fast form makes direct calls. The resumable form runs only when a continuation is resumed,
and always returns the `TailCall`, which keeps down what the roughly 1,200 tail call sites in the
shipped libraries and the compiler add to the generated code: 4.5-6%.

Direct tail calls do show in a JavaScript stack trace, as they would in a language without proper
tail calls; the budget bounds how many.

## Inlined primitives, and knowing they are still primitives

`(car x)` compiles to `x.car` behind a type test, and `(+ a b)` to a BigInt addition, with the real
primitive as the fallback for every other operand shape (`src/compiler/inline.scm`). Scheme lets a
program redefine `car`, so the expansion is correct only while the name still denotes the
primitive — and that has to hold every time it runs, not just when it was compiled.

The obvious guard asks the environment on every use: read the global, compare it with the
primitive. That is a hash lookup per `car`, and it was most of what compiled code did. Removing
every guard outright, unsoundly, made the `call` and `fixnum` classes about 1.95x faster.

Caching the answer inside a procedure does not recover that. The binding can change whenever code
runs, which in compiled code means at any call, so a cached answer lasts until the next call — and
in recursive code like `fib` there is a primitive between every pair of calls. What does recover it
is turning the question round: **the interpreter notices rebinding instead of compiled code asking
about it.** Every binding write reports its name and value to
`src/core/interpreter/primitive_bindings.js`, which keeps one cell per primitive's name, and marks
the cell *not intact* the first time the name is bound to anything but its primitive, anywhere.
The guard is then `W.intact || (C.v ?? G()) === P`: one property load while nothing has rebound
`car`, and a read of the global once something has.

Two properties make that sound:

- **Every write is seen.** Environments are written through `define` and `set`, which report; the
  interpreter's `letrec` frames and installing compiled code write through `rebind`, which does not,
  because they only ever bind renamed locals or replace a closure with its own compiled form.
  Library imports go through `define`.
- **The flag is per name, not per environment.** A library that defines its own `car` turns the
  shortcut off for `car` everywhere. That costs speed in an unusual program and is never wrong, and
  it is what lets the check ignore which environment a piece of compiled code resolves its globals
  in.

**Two numeric fast paths, not one.** An arithmetic or comparison expansion is taken when both
operands are exact integers or both are flonums -- JavaScript `bigint`s or JavaScript `number`s --
since for either pair the JavaScript operator computes what the tower does; `=` as `===` agrees with
the tower on `-0.0` and on NaN, and every ordering with a NaN is false. Exact integers are tested
first. Until the flonum path existed every flonum operation went through the variadic primitive,
which was most of what flonum programs did: adding it made the class 7.5x faster compiled. Mixed
exactness stays with the tower: JavaScript can compare a `number` with a `bigint`, but exactly,
where the tower converts to a double, and the two differ on large integers.

**An expansion can be a call.** `vector-ref` and `vector-set!` expand to a runtime helper,
`R.vectorRef`/`R.vectorSet`, rather than to inline checks. The checks written inline were slower
than the primitive -- a `bigint` index compared with a length costs more than converting it once --
while the helper converts once, reads or writes the array, and passes anything else, and so every
error, to the primitive. It removes what calling the primitive through the generic call path cost,
and caught nearly all of a 1.4-2.1x ceiling on vector-heavy programs. Like the other runtime values
call sites use, the helpers are read from `R` once per procedure.

**Some expansions apply only to some operands.** `eqv?` is JavaScript `===` exactly when one
operand is a constant whose identity is its value -- a symbol, a boolean, the empty list -- and is
not for numbers, which compare by value and exactness, or characters, which compare by code point.
So its table entry has a compile-time predicate over the operands' IR as well as the run-time test,
and a call against anything else stays a call. That is the shape `case` produces: it expands to one
`eqv?` test per datum, nested `if`s rather than `or`, whose expansion binds a variable per datum
that the interpreter pays for as an environment. Compiled symbol dispatch went from a `memv` call
per clause, 15-207 ns, to 1.3-10 ns a call.

An expansion is also only *emitted* when the name is bound to its primitive at compile time. It
used to be emitted when the name was bound to any function, and the guard compared against
whatever that was — so a program that redefined `car` and then compiled a procedure had its own
`car` replaced by the primitive's.

## Reading a global

A global cannot be taken at compile time: a definition may be a forward reference, and any binding
may be redefined or assigned afterwards, by a later `define`, a `set!` or a REPL. Compiled code
used to find the frame holding the name once and then look the name up in that frame's map on every
read. Rewriting the generated code to cache each value after its first read -- unsound, a ceiling
only -- was worth `call` 1.50x and 1.12–1.24x on four other classes.

So the question is turned round, as it was for primitives: **the frame keeps a cell current rather
than compiled code asking it.** `Environment.cellFor(name)` hands out one cell per name, created the
first time compiled code asks; `define`, `set`, and `rebind` update it. Generated code declares, per
global, a cell and its resolver, `let C0 = R.UNRESOLVED; const G0 = () => (C0 =
R.globalCell(E, "fib")).v;`, and reads `(C0.v ?? G0())`. The first read resolves the cell; every
later read is one property load. A value that is `null` -- the empty list -- or `undefined` looks
unresolved and takes the resolver every time, which is slower and still right. A name that is only
a JavaScript global has no frame to keep a cell, so its cell reads the name afresh each time.

The frame is found once, as the lookup it replaces found it once, so a later definition of the same
name in a frame nearer the reader would go unseen. That does not arise for the top-level and library
frames compiled procedures close over.

The other cost expected on every call, the `SCHEME_RAW_CALL` lookup that decides whether a callee is
an interpreted closure, measured as nothing, and is left alone. Reading the runtime values every
call site uses -- `TailCall`, `step`, `UNWIND`, `SCHEME_RAW_CALL` -- once per procedure rather than
from `R` at every site was worth a few percent more.

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
compiler is the goal and it cannot be argued from priors. Lowering and code generation are Scheme;
the analyzer in front of them, and the safety analysis beside them, are not yet.

A compiler written in the language it compiles has to start somewhere. It starts in the
**interpreter**, which loads the compiler's Scheme from source with no compiler at all:

| step | produces |
|---|---|
| the interpreter loads the compiler's library | a working, slow compiler |
| it compiles every library the bundle ships | `src/packaging/compiled_libraries.js` |
| that compiles the compiler's library | `src/packaging/compiled_compiler.js` |

The compiler's Scheme is a library, `(scheme-js compiler)`: `src/compiler/compiler.sld` imports
`(scheme base)`, `(scheme char)`, `(scheme cxr)`, SRFI 1 and SRFI 152, includes `ir.scm` and the
emitter's files in dependency order, and exports the entry points `lowering.js` calls. So the list of
files that make up the compiler, and their order, is said once, in Scheme, and SRFI 1's private
helpers stay private to SRFI 1. Its table keeps only what its exports can reach.

Every table has one shape, a map from library name to the procedures that library defines, and is
installed into the library's own environment as the library loads (`installLibraryTable`). A
library's environment also holds everything it imported; `generateEnvironment`'s `ownOnly` option
leaves those out, so no procedure is compiled into two tables.

The compiler loads its library, and the ones that imports, into **a registry of its own**
(`withPrivateLibraries` in `src/core/interpreter/library_registry.js`). The library registry is
otherwise one per process, and sharing it would be wrong both ways: a program that redefined a
procedure of `(scheme base)` would change the compiler, and the build step compiling
`(scheme base)` would find it already loaded -- by the compiler -- and never see it load.

`npm run prebuild` runs the chain in about 1.8 s from a checked-in build, and about 15 s from
nothing, where the first link compiles every shipped library with the compiler still interpreted.
Both tables come out byte-identical either way. Code generation costs more in Scheme than it did in
JavaScript: about 1.2 ms a procedure against 0.16 ms, measured over 1,014 procedures, most of it
`case` dispatch through `memv`, a code-generation target, so the compiler speeds up as the tier
does. Reading globals through cells (below) made its lowering 1.31x faster and left code generation
where it was.

**The order is the design, not an optimization.** Lowering calls `memq` and `assq` on every scope
lookup and every global it records, and those are themselves Scheme. Compiling the lowering against
an interpreted library is worth 1.33x; against a compiled one, 25x (`npm run benchmark:self-host`).
Almost all of a compiled module's cost can be the interpreted library underneath it.

Every prebuilt table is guarded by a fingerprint of the library's sources -- its `.sld` and each file
it includes -- (`src/compiler/prebuilt.js`), so a stale build costs speed and never correctness.

Because every library the bundle ships arrives compiled from its table, **a page needs no compiler
to get compiled libraries.** The compiler is therefore not in `dist/scheme.js`: it is
`dist/scheme_compiler.js`, fetched by `loadCompiler` only for a page that compiles code of its own.

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
`new Function` anywhere: every shipped library and the compiler's own Scheme are compiled at build
time into ordinary module text. Dynamic paths — `eval`, `load`, the REPL — fall back to the
interpreter. (`js-eval` in `src/extras/primitives/interop.js` is a deliberate interop escape hatch;
it throws under strict CSP, which is acceptable and forces nothing else to depend on `eval`.)

## The four constraints, honestly

| Constraint | Status |
|---|---|
| **1. JS interop** | Met. Scheme closures stay callable JavaScript functions; compiled procedures keep the same wrapper. Value representation is untouched. |
| **2. Browser + CLI** | Met. Generated code is ordinary JavaScript; the libraries and the compiler are AOT-compiled, and a browser page fetches the compiler only if it compiles code of its own. |
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

- **3,426 tests**, Node and browser, via `npm test`.
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
- **Each code-generation decision also has a targeted benchmark**, `npm run benchmark:codegen`
  (`benchmarks/run_codegen.js`), timing the construct it changes in the shapes that decide its cost,
  in both tiers, with the tiers' answers compared. The suite is blind to a construct its programs do
  not use hot: `case` dispatch got up to 30x faster compiled without moving a class.
- **The self-host benchmark compares compilers, not macros.** It lowers lambdas from real programs,
  so a macro change changes its workload; a per-pass figure across one is not a speed change.

## Where the rest of the reasoning lives

Per-module rationale is in the module headers, which are edited with the code:

| | |
|---|---|
| `src/compiler/ir.scm` | the IR's shape; what the Scheme subset costs to write in |
| `src/compiler/emit.scm` | both forms of a procedure; statements as data |
| `src/compiler/lift.scm` | lifting, and `letrec` |
| `src/compiler/liveness.scm` | what a suspended frame saves |
| `src/compiler/inline.scm` | primitive expansions, tower-faithful |
| `src/compiler/lowering.js` | hosting the compiler's Scheme; the bootstrap in detail |
| `src/compiler/marshal.js` | the JavaScript/Scheme boundary, and how it shrinks |
| `src/compiler/safety.js` | the call-graph closure, and its measured trade-off |
| `src/compiler/prebuilt.js` | staleness, and why arity rather than names |
| `src/compiler/runtime.js` | the trampoline and the tail-call budget, global cells, procedure marking |
