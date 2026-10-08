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

Non-tail calls use **the native JavaScript stack**, and when it gets deep the compiled frames on it
move to the interpreter's heap stack (see *Deep recursion*). Tail calls return a `TailCall` to a
**trampoline**, so tail recursion runs in bounded space -- except that a tail call to another
compiled procedure is made directly while there is room on the stack (see *Tail calls between
procedures*). Continuation capture runs a **cooperative
unwind** — Pettyjohn et al.'s generalized stack inspection, with Marshall's modification replacing
the thrown exception with a distinguished return value, which is what removes the technique's
historical weakness.

The alternative (A) kept every continuation frame in a JavaScript array under a trampoline. It is
the faster-measured design in the published literature and it was rejected for a reason that is not
about speed: **the JavaScript call stack would be one frame deep**, so DevTools could never show
Scheme frames, and the browser would have no debugger for compiled code but a custom one. Under B, one live Scheme frame is
one JavaScript frame, until the stack is deep enough that some move to the heap, and tail calls add
none beyond the room below. The bake-off's probe measured that shape. Each frame shows as its Scheme
procedure, and a source map puts it at its place in the Scheme source (*Compiled code in a stack
trace*, below).

The cost of B is procedure fragmentation, which is the next section.

### Compiled code in a stack trace

An engine names a frame, in an error's stack trace, a debugger's call stack or a profile, by its
function's `name`, and places it by its script's URL. Generated code would show every procedure as
`$proc`, at `eval at instantiate (host.js)`. So each fast and resumable form is made as the value of a
property keyed by the Scheme procedure's name, `const $proc = { "count-down": function (n) {...}
}["count-down"]`, which names the function as it is made (`named-function` in `emit.scm`); a nested
procedure shows as its name if a named `let` or an internal definition gave it one, and as
`anonymous`, the expander's name for it, otherwise. Setting `name` afterwards would read the same in a trace, but reconfiguring a function's
own property leaves all its properties slow to read (R110), and compiled code reads its callee's on
every call. V8 removes the object literal, so making a closure costs what it did.

Code generated as a program runs is given a `//# sourceURL=scheme:///<file>/<procedure>`, the file
being the one the procedure was read from, or else its library, or else `program`
(`source-url` in `driver.scm`), so a debugger lists each procedure's code as a source of its own.
The prebuilt tables are module code, placed by their module's URL, and named the same way.

And it is given a source map, which places each frame in the Scheme source. Positions ride from the
reader to the lines of generated code: the expander gives the core form of each application the
span it was read from (its `source`), the lowering keeps it on the `call` node it makes, and while the
emitter emits a call, each statement it makes is noted as coming from that span -- beside the
statement, in a weak table, since every reader of a statement would otherwise have to step over
it. A procedure renders as items, each a line, a line with its span, or an indented group of them,
so the function, the factory and the unit around it wrap what is inside rather than copying it, and
the unit's text is written once, listing each line's span (`render-items` in `emit.scm`). Each line
with a span maps, from its start, to the start of the span (`sourcemap.scm`): a frame shows at the
Scheme expression whose code holds its call. The map goes into the script as a `data:` URL holding
the JSON as it is: a URL's parser percent-encodes what it must and the URL's body is
percent-decoded, so only `%`, `#`, `?` and spaces in a file's name are escaped. Only code read under
a name is mapped. A page's scripts are read under names (`html_adapter.js`): one with a `src` under
its URL, which a debugger fetches, and an inline one as `<page>#scheme-<n>`, whose text the page
keeps (`source_texts.js`), for nothing could fetch it, and the map carries as its `sourcesContent`.
A file named by a URL is placed in the `scheme:///` URL by its path. The prebuilt tables, which are
modules, have no map of their own yet.

## A compiled procedure faces JavaScript; its code faces Scheme

Every Scheme procedure is called the same way from JavaScript, whatever its tier: called as a plain
function, it converts its arguments into Scheme and its result out of it, and finishes its tail
calls, deep recursion and continuations before it returns, as an interpreted closure always has
(`Interoperability.md`, *Calling Scheme from JavaScript*). Compiled code wants none of that between
compiled procedures, so a compiled procedure is two functions:

- **the procedure**, which Scheme holds and JavaScript is given, made by `markProcedure` in
  `runtime.js` (`createCompiledProcedure` in `values.js`). Its plain call converts its arguments,
  calls its code directly and converts the value out; what the code leaves unfinished -- a pending
  tail call, frames to move to the heap, a continuation being captured, an exception -- it hands to a
  run of the interpreter the procedure's environment belongs to (the program's, or for the compiler's
  own procedures the compiler's), whose first step takes it up with the stack the call would have had
  in a run (`Interpreter.callCompiledEntry`, `CompiledEntryRemainder` in `frames.js`). Every call used
  to start that run, which cost JavaScript about half a microsecond a call; now only the calls that
  need it do.
- **its code**, the fast form, which is the procedure's raw entry, `SCHEME_RAW_CALL`, and takes and
  returns Scheme values, a pending `TailCall` or the unwind sentinel among them. Compiled code calls
  it: a call whose value is wanted always read the raw entry, and a direct tail call now does too,
  `(callee?.[$RAW] ?? callee)`, a primitive being its own. So does the interpreter, which holds Scheme
  values, and `callSchemeProcedure`, the public call that converts nothing.

The procedure is what `eq?` sees, what a global holds, what a self-call through the global is
compared with, what a move of frames records to call again, and what carries `$compiled`, `$resume`
and `source`; the code is never a Scheme value. A primitive is a single function, which converts
nothing.

What it cost, compiled, measured in `run_codegen.js` (best of five, alternated with the commit before): a direct tail call 1.0 to 2.4 ns, ten mutually recursive ones 56 to 77 ns, a tail call to a primitive 5.8 to 7.1 ns, from the second property load; making a closure 13.5 to 14.5-14.9 ns, from the second function, once every compiled procedure shared one `toString` rather than each being given its own, which had made it 17.5; calls whose value is wanted unchanged, since they always read the raw entry; recursion deep enough to move frames 5-7% slower; JavaScript calling a compiled procedure about 600 ns, as it costs an interpreted closure, where calling the code itself was 30 ns and wrong; the generated code 1.1% larger, 1.3-1.8% gzipped. On the canonical suite, compiled, best of two passes alternated with the commit before, every workload class within 1-2% (0.99-1.02), and `earley`, which makes many tail calls, 7-8% slower, measured alone three times.

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
emitter is Scheme, written with SRFI 1, SRFI 151 and SRFI 152 like any other Scheme program, and it replaced
a JavaScript one after producing byte-identical output across the test suite, the benchmark
programs, the standard library and the compiler itself — except for frames that save less.

## The capture protocol

Owned by `src/core/interpreter/unwind.js`, deliberately on the interpreter side: the interpreter
must not depend on the compiler.

`call/cc` begins an unwind by returning a distinguished sentinel. `run` propagates it. Each compiled
frame on the way out **reifies itself** — saving its locals and which call site it had reached — and
the interpreter splices the resulting frames in where the tier boundary sat. Invoking the
continuation re-enters each compiled procedure through its twin at the saved `$pc`.

Multi-shot works because frames are copied rather than consumed. A resumed frame runs as compiled
code the interpreter called: it records the interpreter's stack for whatever the procedure calls
back into Scheme, as `continueApplication` does -- without it, a continuation invoked from a
resumed frame started from whatever stack was recorded last, and rewound into winds it was already
in, which the differential fuzzer found.

**A capture made by compiled code is finished by a driver of the runtime's own** (`drive` in
`unwind.js`, task 77), when nothing but compiled frames lies between it and the outermost run and
no debugger is on. The frames go on a list of the driver's, innermost first, which a continuation
shares rather than copies; each is resumed by calling its twin; and a continuation invoked from
compiled code running in the driver -- by a call or a tail call -- is taken by a jump, a plain
object thrown to the driver. The continuation is the interpreter's too: the run's stack beneath
the driver, copied once at the driver's first capture, with the frames on top, made only when
something else invokes it -- interpreted code, code inside a nested run, anyone after the driver
has returned -- and those take the interpreter's way, which runs the `dynamic-wind` thunks between.
A run clears the driver it starts beneath (`enterRun`), so no jump crosses one. Anything else an
unwind collects -- a nested run's frames, a capture made by interpreted code, a step taken again
for the debugger, and a move to the heap -- goes to `completeCapture` as before, the driver's
frames beneath it. Compiled `ctak` runs 1.65 times faster and `fibc` 2.5 times; `puzzle`, whose
capture holds frames in the driver while its whole search runs, 7% slower.

Moves to the heap stay with the interpreter, which hands each moved frame back to its run to resume
(`MovedFrames`): run inside a driver, what a move's frames go on to do -- in `earley` the rest of
the program -- took a fifth longer, all of it in garbage collection, the same cost that once made
resuming a moved frame from inside the frame holding it slower (R140).

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

The analysis is ordinary backward dataflow over the twin's blocks, with each set of locals an exact
integer holding a bit per local (SRFI 151): a large procedure has hundreds of locals live across
its call sites, and kept as lists, where each union tests every member of one set against the
whole of the other, the sets were a sixth of what compiling cost the canonical programs under the
tier.

The restore side is unchanged and names every local. One that was not saved destructures to
`undefined`, which is safe precisely because it is dead there.

**Compiled and interpreted code may alternate any number of times beneath a capture.** An
interpreted procedure that compiled code calls runs in a nested run of the interpreter on the
JavaScript stack, starting on a copy of its parent's frame stack and a sentinel. A run compiled code
called passes an unwind on (`Interpreter.unwindsOut`): it adds its own frames -- those above its
sentinel -- and returns the unwind sentinel, and the compiled code that called it saves itself as any
compiled frame does. The first run that cannot pass it on stacks everything in order: compiled
frames, the frames of the run they called, the compiled frames that run called, and so on inwards.
This used to be refused past one boundary, and that shape was reachable with no user code compiled:
an interpreted procedure passed to the compiled `for-each`, calling the compiled `map` with an
interpreted procedure that captures.

**A run passes the unwind on only if its compiled caller can pass it on in turn** -- only if
`flushable` was true when the caller called, so that no JavaScript caller sits beneath it. JavaScript
that is not compiled code -- the file procedures calling a procedure back, `js-invoke`, a class
constructor, a promise's executor -- cannot save itself, so a run such a caller started finishes the
unwind, and the continuation leaves out the JavaScript caller and anything beneath it that the
interpreter did not run, exactly as it does with no compiled code anywhere. It works as an escape,
which is how `guard` uses it; resumed after those frames have returned, it resumes without them. Before
this, the unwind was handed to such a caller as a return value, which produced a wrong answer rather
than a refusal: 11 for `(+ 1 (+ 100 ...))` with the capture beneath `with-input-from-file`.

One shape is **refused rather than answered**: a capture beneath a redefined inlined primitive, whose
expansion is not a call site the resumable form splits at. `R.callBinding` marks the state
(`refusesCapture`), and the sentinel of the run it starts carries it to `call/cc`.

## Operands in the interpreter's order

A call's value is a statement and a temporary in generated code, but a global read, a boxed local's
read, or a sequence ending in either is an expression, written into the call that uses it -- and
so evaluated after every operand to its right. R7RS leaves the order of a call's operands
unspecified, so that was Scheme; but the interpreter evaluates the procedure first and then the
operands left to right, and it is the reference semantics. A program that depended on the order --
`(list g (f))` with `f` assigning `g` -- gave a different answer compiled. So `emit-operands!` puts
such an operand into a temporary before any later operand that could have an effect -- anything but
a literal, a variable or a lambda. A literal and an unassigned local are left as they are, since
nothing can change them. The differential fuzzer found this in its first long run: six programs in
5,000, one cause.

## Raising from compiled code

`raise`, `raise-continuable` and `error` do not raise: they return a pending raise, a `TailCall`
whose function is a `RaiseNode`, for their caller to perform. The interpreter performs it by
running the node, which finds the handler on its frame stack, runs the `dynamic-wind` after-thunks
on the way, and pauses first if the debugger breaks on exceptions. Compiled code has no evaluator,
and continues any pending call by calling its function; so a pending raise that reached compiled
code where it wanted a value -- the argument checks in the compiled library's `length`, `assv` and
`member` -- used to fail with JavaScript's "args is not iterable".

It is now performed by **throwing it to the nearest interpreter run**, which performs it from
where it called compiled code. `RaiseNode` has a raw entry, as an interpreted closure does, and a
pending raise carries the exception as its arguments, because a raw entry is called without a
receiver (`src/core/interpreter/ast_nodes.js`). This is exactly the raise the interpreter would have
performed, for one reason: **compiled frames never hold a handler or a wind.** A procedure that names
`with-exception-handler`, `guard`, `parameterize` or `dynamic-wind` is not compiled, so everything
in force where compiled code raises is on the frame stack of the run beneath it. The compiled
frames the throw leaves are abandoned, which a raise that cannot return does to them anyway.

What is thrown is what a raise nobody handles throws, so JavaScript calling a compiled procedure
directly, with no run beneath it, receives what it would have from an interpreted one.

A **continuable** raise is refused, with an explanation: a handler that returns would deliver its
value to the frame that raised, which is compiled and which the throw has left. Compiled code only
meets one when handed `raise-continuable` as a value.

**Calling a non-procedure** is reported as the interpreter reports it, not as JavaScript does --
naming the temporary that held it, "$t0 is not a function", or, for the empty list, which is `null`,
failing to read its raw entry. Where a call's value is wanted, the callee is tested for being a
function first, as a statement of its own. It runs on every such call, so the form was measured:
folded into the call expression it made `divrec` 9% slower; as a statement it costs 2-4.5% on the
programs made of calls between compiled procedures, and a few nanoseconds a call. Catching the
call's failure in a `try` instead cost those nothing and a call into an *interpreted* procedure 15% --
the call the compiled library makes to a program's callbacks, which is the browser's common case --
so it lost. A tail call already reported a non-procedure, in `R.tailCall`.

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

**Loops on raw doubles.** An inexact integer is a box, so a loop whose variables are inexact reals
allocated whenever one landed on an integer, and V8, finding a variable sometimes a double and
sometimes a box, stopped holding it in a register. So the fast form runs a second copy of such a loop
first, on raw doubles (`emit-double-loop!` in `emit.scm`): one whose body calls nothing but inlined
primitives and itself -- so no continuation can be captured in it, and no frame of it is ever
reified -- holds an inexact constant, and whose variables provably stay inexact, found as a fixed
point over its looping calls, with the `let`s in its body and the locals bound outside it that those
need. It is entered when those variables arrive inexact and its operators are intact, and unboxes
them; the ordinary loop follows it. Nothing inside it boxes or calls: a way out breaks out and its
return, boxing what it returns, is made after the loop (`deferred-exits`); an operator rebound
mid-loop breaks out of a block whose end boxes the variables, and the ordinary loop goes on from
there; and the test that the operators are intact is JavaScript's boolean, not a Scheme value. A box
made in a branch inside the loop, even one that never ran, made `sumfp`'s loop seven times slower,
since V8 compiles such a loop as it runs and then stopped holding its variables raw (R120). The cost
is the compiler's size: the analysis and the emitter are about 600 KB of its prebuilt image.

## Tail calls between procedures

Any other tail call used to return a `TailCall`, which allocated the pending call and an argument
array, returned through the caller, and was run by the nearest trampoline through `invoke`'s spread:
a fifth of `earley`'s time, with an eighth more in the collector. Made as plain JavaScript calls,
unsoundly and without any bound, those calls were worth 1.2-1.4x on six canonical programs.

The unsound version is unsound twice over. A chain of tail calls that never returns -- `cpstak`, the
loops in `fft` -- grows the JavaScript stack until it overflows, where Scheme requires constant space.
And a tail call to a continuation, made directly, throws to get where it is going, where a returned
`TailCall` let the interpreter reinstate it without one: `fibc` ran 2.6 times slower.

So a tail call is made directly only to a compiled procedure, through its raw entry (*A compiled
procedure faces JavaScript; its code faces Scheme*), or to a primitive -- the callee's raw entry, or
the callee with none, being marked `SCHEME_PRIMITIVE` -- and only while compiled frames have room left on the stack, `R.stack.room`
(see *Deep recursion*, which the same count serves). Anything else, and anything without room,
returns a `TailCall` as before; a chain that runs out of room unwinds to the nearest trampoline and
carries on from there, so it runs in bounded space.

Two things about the bound were decided by measurement rather than by argument:

- **It counts stack, not calls.** Frames differ by two orders of magnitude: a chain through a
  procedure with 60 locals filled V8's whole stack in about 730 calls, and one with 200 overflowed in
  400. The emitter knows each frame's size at compile time -- its locals and parameters plus the fixed
  part of an interpreter frame.
- **It is one count for the whole segment, not one per chain.** Resetting the count at every
  non-tail call would bound each chain rather than the total: a program recursing through long tail
  chains then keeps every chain on the stack at once, and with a limit of 100 calls a chain, one
  survived 132 levels of recursion where it had survived 10,385.

When this was first built the count held only direct tail calls, each adding its frame before the
call and taking it off in a `finally`. Counting every frame for deep recursion replaced that: room is
now stored before each call rather than taken and given back, which needs no `finally` and cannot be
left wrong by an exception.

A capture beneath a direct tail call needs nothing from the caller's frame, whose continuation is its
callee's: the unwind sentinel is returned as the callee's value, and the frame is absent from the
recorded continuation, as it would have been had the call been trampolined.

Only the fast form makes direct calls. The resumable form runs only when a continuation is resumed,
and always returns the `TailCall`, which keeps down what the roughly 1,200 tail call sites in the
shipped libraries and the compiler add to the generated code: 4.5-6%.

Direct tail calls do show in a JavaScript stack trace, as they would in a language without proper
tail calls; the room bounds how many.

## Deep recursion

The interpreter keeps its frames on the heap and recurses as deep as memory allows. Compiled
frames are on the JavaScript stack, which holds about 5,900 of the smallest under Node's default
stack, and the standard library the browser installs is compiled -- so there `map`, `make-list`,
`list-copy` and `equal?` overflowed on a list of 10,000 elements, which the interpreter handles at
100,000.

The capture protocol already knew how to take compiled frames off the stack, so deep recursion uses
it. Each procedure that calls anything takes its frame's size from `R.stack.room` on entry and stores
what is left before each call. A fast form entered with no room does not run: it records itself and
its arguments as a pending call (`R.flush`) and returns the unwind sentinel. Every compiled frame
beneath it saves itself on the way out, exactly as for a capture, and the interpreter puts them on
its heap stack and makes the pending call, with the JavaScript stack empty again. Each frame moved
finishes, later, in its procedure's resumable form.

The moved frames go on the heap stack as **one** frame, `MovedFrames`, and a later move that finds
the last one still on top links to it. Pushed one at a time, they made the interpreter's frame stack
as deep as the recursion, and every call from compiled code into an interpreted procedure starts a
nested run with a copy of that stack: compiled `map` with an interpreted procedure took a second on
20,000 elements and ran out of memory on 100,000. Held as one, it takes 110 ms on 100,000 and 0.8 s
on a million, where the interpreter takes 4.5 s. `MovedFrames` is never changed, only replaced, since
a continuation shares it and may be resumed more than once.

- **Room is a segment's, and only some segments may move.** The interpreter resets the room whenever
  it calls compiled code (`openCompiledSegment` in `unwind.js`), because the unwind ends there and
  it can finish it. Anything else that calls a Scheme procedure from JavaScript -- a port primitive,
  `js-invoke`, a class constructor, a promise's executor, the evaluator and the compiler's
  JavaScript calling its Scheme (`callSchemeProcedure` in `values.js`) -- turns moving off for its duration
  (`suspendFlush`), since it would take the sentinel for a value; compiled code beneath it can still
  overflow as before. One way past that remains: compiled code calling a plain JavaScript function
  directly, which calls compiled code back. The interpreter gives the setting back after a normal
  return, and `run` gives back what it found however it ends; a `finally` at each call would sit in
  every nested run on the JavaScript stack.
- **It counts room left, stored, not depth added and taken away.** Adding a frame before each call
  and taking it off after, testing at each call site, cost 5-12% on programs made of calls; storing
  what is left before each
  call and taking the frame once on entry, 1-4% -- and comparing room with zero, rather than depth
  with a limit read from a second field, took `tak` from 7% to under 4%. A procedure that calls
  nothing cannot deepen the stack and pays nothing.
- **The limit is half the stack**, in the emitter's estimate of frame sizes, which overstates them.
  Moving is not free, because a moved frame finishes in its resumable form: at a quarter of the stack
  `earley`, which never overflowed, moved its outer loop and ran a fifth slower.
- **A rest parameter's arguments count**: they arrive on the stack, and `apply` spreading a long list
  put 20,000 of them in one frame.

**Recursion that alternates between compiled and interpreted code** goes as deep as either alone. An
interpreted procedure called from compiled code runs in a nested run on the JavaScript stack, so a
run that passes unwinds on continues the room of the compiled code that called it, less a fixed
`NESTED_RUN_ROOM` for its own JavaScript frames, rather than starting fresh; a move started deep in
the alternation passes through the nested runs, each adding its frames, to the run that finishes it,
and the JavaScript stack is empty again. It used to overflow at about 575 levels -- an interpreted
tree walk through compiled `map` -- and 100,000 levels now take 169 ms against the interpreter's
135 ms. Moving frames out of nested runs also made shallower alternation faster: each nested run
starts on a copy of its parent's frame stack, and between moves that stack grows by a sentinel a
level, so a tree walk 400 deep through compiled `map` went from 1.9 to 1.25 ms.

## Inlined primitives, and knowing they are still primitives

`(car x)` compiles to `x.car` behind a type test, and `(+ a b)` to a JavaScript addition, with the
real primitive as the fallback for every other operand shape (`src/compiler/inline.scm`). Scheme lets a
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

**Numbers, as they are held, and arithmetic on them.** An exact integer is a JavaScript number in
the safe range and a `BigInt` beyond it; an inexact real is a number unless its value is an integer
-- 3.0, -0.0 -- when it is a `Flonum`, a box holding the double
(`src/core/interpreter/number_representation.js`). So an integral number is exact, which is what
JavaScript hands Scheme too. Chosen (43) over `BigInt` for every exact integer, which held fixnum
loops at `BigInt`'s floor, 12x a loop on numbers, and over boxing every inexact, which would make
every float operation allocate; the price is that an inexact integer allocates, unless it is one of
those from -1024 to 1024, whose boxes are shared.

An arithmetic expansion is the JavaScript operator on two numbers whose result needs no deciding:
one that is not an integer is inexact, and an integer in the safe range of two integers is exact.
Anything else -- an inexact result that lands on an integer, an exact one past 2^53, a box, a
`BigInt` -- calls the runtime's operation (`add` and the rest in `src/compiler/runtime.js`), small
enough for V8 to inline, which decides exactness, boxes an inexact integer, and hands rationals,
complex numbers and errors to the primitive. A comparison is the operator on two numbers; `=` as
`===` agrees with the tower on NaN, and every ordering with a NaN is false. Against an inexact
constant the result is inexact whatever the other operand is, so a box is taken inline on its
double too -- `fibfp`'s `(- n 1.)` on every call -- and an inexact integral constant is its double
there. Taking a box inline in every generic operation was tried, for `fft`, whose data are all
`0.0`, and made it slower: the larger expansion, everywhere, cost more than the calls it saved
(R121). Before 43 the two fast paths were two `bigint`s and two `number`s, and adding the second
had made the flonum class 7.5x faster compiled.

The primitives keep the tower's own representation, `BigInt` exact and number inexact: `math.js`
wraps each to convert its arguments in and its result out, and gives the commonest a direct path on
numbers, boxes and `BigInt`s, each in a function of its own, since the wrapper, shared by every
primitive, is a call V8 cannot inline: it cost `(inexact x)` 33 ns a call against 4.6.

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

A library's macro used outside the library refers to the library's own bindings, as R7RS 4.3
requires (`library-var` in the core forms; `docs/hygiene.md`). Lowered, such a reference is a global
of its own, keyed by the name and the library -- `eqv?@scheme.control` -- so that it never merges
with the program's `eqv?`; its cell is read from the library's environment, which the constant pool
holds, `R.globalCell(K[n], "eqv?")`. Inlining checks that the library's binding is still the
primitive, and the control-global check and the safety analysis read the binding by its name in the
library's environment. A prebuilt table writes that environment as the library's name, which whoever
installs the table finds: the registry it is installed in, or the library system's seed among its
own libraries, which are in no registry.

The other cost expected on every call, the `SCHEME_RAW_CALL` lookup that decides whether a callee is
an interpreted closure, measured as nothing, and is left alone. Reading the runtime values every
call site uses -- `TailCall`, `step`, `UNWIND`, `SCHEME_RAW_CALL` -- once per procedure rather than
from `R` at every site was worth a few percent more.

## The IR, and lowering

Lowering consumes the **analyzed** AST, not source — so macro expansion, hygiene, alpha-renaming and
internal-definition hoisting are already done, and compiled and interpreted code agree on the meaning
of a program by construction rather than by two front ends being kept in step.

It computes the two things code generation needs and the analyzed AST does not record: **tail
position** (the expander makes every application a `TailAppNode` regardless) and **local versus global
reference**.

Lowering is **partial on purpose**. Anything outside the compiler's subset is declined and left to the
interpreter, because a compiler that must handle everything before it handles anything cannot be
shipped incrementally or trusted early.

The pass is Scheme: `src/compiler/ir.scm`, reached through `src/compiler/lowering.js`.

## Self-hosting, and the bootstrap

The compiler is meant to end up in Scheme, because a Scheme compiler good enough to compile a Scheme
compiler is the goal and it cannot be argued from priors. Lowering, code generation, the driver that
decides what to compile and why not, and the tier's decisions about a program's own code are Scheme;
and so is the expander in front of them, `(scheme-js expander)`, whose core forms the compiler lowers
as they are: each node of the evaluator's keeps the core form it was made of (`assembler.js`), and
the host hands the compiler that. What the compiler's Scheme needs from the interpreter --
`new Function`, reading and rebinding environments, the lambda behind a closure, weak tables -- it
imports from `(scheme-js compiler host)`, `src/compiler/host.js`, and the JavaScript entry points in
`index.js` and `tiering.js` only hand arguments across and read back the records it returns.

A compiler written in the language it compiles has to start somewhere. It starts in the
**interpreter**, which loads the compiler's Scheme from source with no compiler at all:

| step | produces |
|---|---|
| the interpreter loads the compiler's library | a working, slow compiler |
| it compiles every library the bundle ships | `src/packaging/compiled_libraries.js` |
| that compiles the compiler's library | `src/packaging/compiled_compiler.js` |

The compiler's Scheme is a library, `(scheme-js compiler)`: `src/compiler/compiler.sld` imports
`(scheme base)`, `(scheme char)`, `(scheme cxr)`, SRFI 1, SRFI 151, SRFI 152, `(scheme-js interop)` and the
host library, includes `ir.scm`, the emitter's files, `driver.scm`, `safety.scm` and `tier.scm` in
dependency order, and exports the entry points JavaScript calls, which `lowering.js` hands out. So the list of
files that make up the compiler, and their order, is said once, in Scheme, and SRFI 1's private
helpers stay private to SRFI 1. Its table keeps only what its exports can reach.

Every table has one shape, a map from library name to the procedures that library defines, and is
installed into the library's own environment as the library loads (`installLibraryTable`). A
library's environment also holds everything it imported; `generateEnvironment`'s `ownOnly` option
leaves those out, so no procedure is compiled into two tables.

A shipped library whose table matches its sources is restored from it, its procedures bound from
compiled code and its other forms run, and no closure is made (`libraryRestorer`). One loaded from
source has its table installed over the closures its source made: **each closure runs compiled in
place** (`runCompiled` in `src/core/interpreter/values.js`), staying the object every holder of it
has -- a library that imported it, a value made with it, a name bound to it -- so nothing is
searched or replaced, and installing is only that and recording the closures for a debugger. The
compiler tier's compiles, `compileEnvironment` and a compiled program's procedures are the same: a
closure compiled keeps its identity, `(eq? f (car kept))` after `(define kept (list f))` and `f`
compiled. A closure run compiled stops being marked a closure, the interpreter's mark for entering
its body, and answers as a compiled procedure, with the compiled procedure's raw entry, so neither
tier asks anything more on a call.

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
`dist/scheme_compiler.js`, which the bundle fetches after it has started, to compile the page's own
code (*Compiling the program's own code*, below); the page does not wait for it.

`runtime.js` stays JavaScript permanently — not because generated JavaScript calls it, but because it
needs native JavaScript features that neither generated code nor Scheme libraries can express, a
`Map` behind hash tables being the clearest case. Chez keeps a C kernel for the same reason.

## What is compiled: procedures, and top-level expressions

A top-level procedure definition is compiled as a procedure. A top-level expression, or the value of
a definition that is not a procedure, is compiled as a thunk and called once, from the interpreter
so that a capture or a move of frames in it finishes where it should (`tryCompileExpression`,
`runCompiledThunk`) -- where it makes a procedure or loops; straight-line code runs once, and
compiling it costs more than running it. A form that defines at top level through `begin` stays
interpreted, since wrapping it would make its definitions internal. This is not a nicety:
`benchmarks/r7rs/src/nboyer.scm` defines stubs and assigns every real procedure from inside one
top-level `(let () ...)`, so with definitions alone compiled it never ran compiled code at all
(R84).

## What is declined, and why

Two different questions, deliberately kept apart.

**Cannot be expressed.** Lowering fails and reports a reason. The procedure stays interpreted.

**The compiler fails.** An error raised while compiling a procedure -- in the lowering, the emitter,
or making the code a function -- is a bug of the compiler's own, and must not change what the
program does, since compiling is only an optimization. The procedure is declined, with "the compiler
failed" and the error's message as its reason, and runs interpreted; a warning naming it goes to the
error port -- the console on a page, standard error under the CLI -- and the failure is kept until
taken (`unless-failing` and `take-compiler-failures!` in `src/compiler/driver.scm`). The test suite,
the canonical harness and `run_tier.js` take them and fail on any, so that the bug is fixed rather
than hidden. One guard per procedure compiled, about 4 microseconds.

**Can be expressed, and might be slower compiled.** A procedure that captures a continuation, or
that a capture unwinds through, is compiled; whether that pays is decided as the program runs.
Compiled frames can take part in a captured continuation, so this is not a soundness question. It
is a speed one: each capture unwinds and saves the compiled frames beneath it, and each re-entry
resumes them, which costs more than the interpreter's copy of its frame stack.

Measured with every such procedure declined and with all compiled (`--decline-captures` on
`benchmarks/run_compiled.js` and `benchmarks/run_r7rs.js`), declining wins on three programs --
`btsearch` 4.5x, `fibc` 1.8x, `ctak` 1.1-1.2x -- and loses on five, by more -- `quicksort` 21x,
`puzzle` 4x, `maze` 3.8x, `contfib` 2.9x, `threads` 1.35x. The winners of declining re-enter their
continuations or capture at every call; the losers escape now and then, which is nearly every
capture in real libraries (`benchmarks/decline_reasons.js --corpus`, results in
`corpus_decline_results.md`: 98% of the procedures declined for a control form ended at `call/cc`).
No static test tells the shapes apart -- both are `call/cc` -- so a count made as the program runs
does:

- **Every compiled frame saved is counted, and every saved frame resumed**, by the procedure's
  resumable form (`reify` and `noteResume` in `src/core/interpreter/unwind.js`). An escape saves a
  frame and resumes it once; so does a frame moved to the heap to make room on the JavaScript
  stack. Backtracking resumes the same saved frame again and again: `btsearch` resumes its frames
  200 times for each save. A procedure nested in another has a resumable form per closure, so its
  counts are per closure (R91); only a top-level procedure can be switched back, and it has one.
- **A procedure whose frames are resumed at least four times as often as they are saved, after a
  thousand resumes, is switched back to its interpreted closure, for good**
  (`note-resume` in `src/compiler/tier.scm`, through `switch-back-to-closure!` in
  `src/core/scheme/library_system.scm`): the closure runs as itself again, for whatever holds it.
  It is then no longer recorded, so the debugger's switching leaves it alone. The runtime keeps the counts, since that is where frames are saved and resumed, and asks
  the Scheme only at the resumes it names: first at the minimum, then wherever the ratio could next
  hold, since saves only grow. Asked at every resume instead, `ctak` was 9% slower. Until the
  compiler has started nothing is switched back, which only a program running prebuilt library
  code with its own code interpreted can see. A procedure restored from a library's table has no
  closure, and is never switched back.

That needs the closure, so every procedure is compiled over the one the interpreter made of its
definition, and the pair recorded: the tier, `compileProgram`, `compileEnvironment`, the prebuilt
tables and the canonical benchmark harness all do. A procedure nested in a compiled one has no
closure of its own and stays compiled; so does anything compiled from its analyzed definition
(`tryCompileDefinition`). `safety.scm` keeps the old rule, closed over the call graph, for
`declineCaptures`.

What the count does not catch is a program that captures at every call and resumes each frame
once -- `fibc`, `ctak` -- which is the shape the escape fast path below is for. They ran compiled
slower than interpreted mostly for another reason: each invocation of a continuation across
compiled code threw a `ContinuationUnwind` that was a JavaScript `Error`, and so took a stack trace,
half of `ctak`'s time. A plain object since task 96, they run compiled 1.8 and 4.3 times faster than
interpreted (R139), and the fast path's case rests on escapes in general rather than on them.

An escape also needs less than the protocol gives it. A continuation called while the capture that
made it is still on the stack reifies nothing it will use: a JavaScript `throw` caught at the
capture does, running `dynamic-wind` after-thunks on the way out. What makes that harder than it
looks is that the same continuation may be called again after the capture has returned, and must
then re-enter, so a fast path has to fall back to the full protocol rather than refuse.

The other forms need different things. `guard`'s escape into its clauses, and `exit`, are one-shot
and upward, the escape just described. The others are not escapes at all: a `with-exception-handler`
handler runs in `raise`'s dynamic context before anything unwinds, `raise-continuable` returns to its
raiser, a `guard` with no matching clause re-raises in the original `raise`'s context, and
`dynamic-wind` must rerun its before-thunks when a full continuation re-enters. Those need the
handler stack and the wind list to be runtime state compiled code can call through. Several of
these names are on the list for how they are implemented, not for what they do: `raise`'s primitive
returns a node for the interpreter to run, and `guard` expands through `call/cc`.

## A program compiled ahead of time

A program can be built so that it runs with no interpreter, expander, reader or library system:
`node repl.js --build PROGRAM -o OUTPUT` writes one ES module, which `node OUTPUT` runs and a page
loads with `<script type="module">` (tasks 98 and 99). The build, `(scheme-js ahead)` in
`scripts/lib/ahead.scm`, writes a table of the program's code, which imports nothing; rollup bundles
it with the runtime, `runMain` in `src/compiler/ahead.js`, which runs it as the module loads and
reports a raise nobody handles as the CLI reports one (`src/packaging/ahead_bundle.js`). Three things
make that possible, and each is a choice the rest of the system has to keep true.

**Every form that runs is compiled, and nothing else is needed.** The build loads the program's
libraries from their source in a registry of its own, noting each top-level form with the core form
it expanded into, as the prebuilt tables' build does, and expands the program's own forms without
running them. Each core form is items, in order: a procedure's definition, bound to compiled code; a
value's, computed by a compiled thunk; a form run for its effect, the same. A macro's definition
expands into nothing, and an expanded program uses no macro, so neither the expander nor the macros'
transformers are carried. A library's few values -- the current ports' cells, `equal-tree-budget`
-- are items like the program's.

**A name is bound where the program would find it, and only what is reached is kept.** Each unit's
imports are replayed as `import-into!` binds them, and each name a piece of code reads is followed
to the item that defined it, through every library that re-exported it, or to a primitive
(`binding-of`). The build follows the code from every item that runs as its unit loads, and keeps
the procedures it reaches: of `(scheme core)`'s 140-odd procedures, a program that prints keeps
about 40, most of them the printer. An import is written from the unit that defined the binding,
by value, as the library system binds one; a primitive imported under its own name is not written
at all, since every unit's environment is inside the runtime's environment of primitives.

**What cannot run is refused by name, at build time, rather than failing as it runs.** Code the
compiler declined, reached from anything that runs -- a procedure naming `dynamic-wind`,
`with-exception-handler`, `guard` or `parameterize`, for which there is no compiled code yet --
refuses the program, saying which procedure and why; so does a constant that cannot be written
down, and a primitive the runtime does not carry (`AHEAD_PRIMITIVES`: those that need nothing of
the interpreter or the library system -- not `read`, whose reader is a library of its own, nor
`eval`). Unreached, a procedure that could not be compiled costs nothing.

The program runs on the driver that needs no interpreter beneath it (`runAhead`), which finishes
its captures and its moves to the heap itself. Where the compiled tier hands a move to the
interpreter, finishing it in the driver made `earley` a fifth slower, all of it garbage collection
(R140); with no interpreter there is nothing to hand it to, and `earley` compiled ahead of time is
1.2 times slower than under the tier, and the difference is garbage collection: at 200 iterations,
1.05 s of its 3.6 s, against 0.55 s under the tier. Nearly everything else runs as fast or faster
ahead of time: of the 45 programs of the canonical suite's default profile, 43 run, at 0.52 to 1.02
times the tier's time per iteration, median 0.93 -- `takl` 0.52, `sum` 0.64, `ctak` and `mazefun`
0.67 (`benchmarks/run_ahead.js`) -- and the two that read data files with `read` have no reader.
Why those are faster was not looked into.

A table is large: 650 KB for `fib`, most of it `(scheme core)`'s printer, which every program that
writes reaches, and 15 MB for `benchmarks/r7rs/src/compiler.scm`, nearly all of it generated code
(task 41).

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

Strict CSP is not a target in its own right, so this is a guarantee to keep, not a constraint on
design: a page under one degrades to interpreted user code, and a test enforces that by making
`Function` throw. An optimization that needs `new Function` is allowed provided the code it
replaces remains as the CSP fallback.

### Compiling the program's own code

A program's own procedures are compiled while it runs, by a tier attached to its interpreter
(`src/compiler/tier.scm`, attached by `src/compiler/tiering.js`) -- on by default in the CLI (`--no-compile` turns it off), in the browser
bundle (`setUserCodeCompilation(false)`), and in both REPLs. The interpreter never depends on the
compiler, which a browser page loads after it has started; it reports to the tier, if one is
attached, and asks it to run top-level forms (`Interpreter.runTopLevel`):

- **A closure bound at top level**, by `define` or by `set!` -- `nboyer` assigns every procedure it
  has from inside a `let` -- in the program's global environment or in the body of a library of the
  program's own. A shipped library's procedures are its prebuilt table's.
- **A waiting closure's calls running out.** Each closure carries a count, zero unless the tier set
  it, checked where the interpreter applies closures; the check costs 1-2% of interpreted call time.

**How the interpreter calls the tier.** It holds the tier's record (`interpreter.tier`), which
carries a Scheme procedure for each of the three -- `bound`, `due` and `form` -- and calls them
directly, from the step that noticed, with compiled frames kept from moving to the heap
(`callSchemeProcedure` in `values.js`); the runtime asks the re-entry policy (`note-resume`, below)
the same way. So the tier's Scheme runs compiled, or in the compiler's own interpreter, and never
where the program's debugger could pause it. Applied instead through the program's interpreter, as
it applies any procedure, each compile's capture -- the compiler's interpreted `emit-guarded` holds a
`guard` -- would unwind into the program's interpreter, which would then run the rest of the
compiler's code (R95). A mode in which the debugger may pause in the system's own code needs exactly
that, and is planned for later (62). Calling a hook costs about 0.1 µs; what the hooks do costs about
2 µs a binding and 0.7 µs a top-level form, and compiling about 3 ms a procedure.

**When.** Generating a procedure's code costs about a millisecond, so compiling every definition as it
is made would cost a page with five hundred of them half a second, much of it for code run once. A
procedure whose body loops or makes procedures is compiled when it is bound, since a loop inside a
procedure called once is where time goes and no call count would ever see it; any other is compiled
on its second call. A top-level expression is compiled, as a thunk called once, only if it loops.

**No on-stack replacement.** Both tiers look a top-level name up at every call. Once the compiled
procedure is bound in the closure's place, the next call through the name -- a recursive call below
frames already made, or the next iteration of a loop written as a self tail call -- runs compiled, and
the frames already on the stack finish interpreted. Every other name holding the closure is rebound
too, as are the copies other libraries imported, since an import copies the value.

**Over the closure, for the debugger.** Every procedure is compiled from the closure the program
made, and the pair recorded, so it runs as that closure while the program is debugged (below), and
its breakpoints fire. That is why a top-level expression that only makes procedures is not compiled:
compiled as a thunk, the procedures it made and kept would have no closure to go back to. The
procedures it binds are compiled when bound instead. A loop's thunk keeps that limitation for any
procedure the loop makes and stores.

**When not.** Nothing is compiled while the program is being debugged, since it would be switched
straight back; a procedure due meanwhile is compiled on its first call after. Nor while a library is
loading: the compiler is Scheme, and running it defines things, which inside a library's body would
be registered with the scopes that library's macros resolve their free identifiers through. A
library's procedures wait for calls once it has loaded, however they loop: ten, since most of what a
library defines is not hot in any one program -- compiled at their first call, as they were, the
test programs of 21 of 22 libraries that are not shipped ran slower with the tier than without
(`library-calls-before-compiling` in `tier.scm` says why ten).

**Starting the compiler.** The tier's decisions are the compiler's Scheme, so the compiler starts when
the tier is attached: about 130 ms, which a script that compiles nothing now pays as well -- the CLI
running `(display 1)` takes 0.28 s against 0.14 s with `--no-compile`. A program that compiles
anything paid it before too, at its first compile. Most of it is analyzing and running the source of
the compiler and of `(scheme base)`, SRFI 1, SRFI 151 and SRFI 152 in the compiler's own registry, which their
prebuilt tables then replace; making that fast is ranked in `compiler_plan.md`.

**Only top-level procedures.** A procedure nested in one is compiled with it. So a procedure the tier
declines keeps its inner loops interpreted; compiling those separately is possible, since the
interpreter looks a local loop's name up in its frame at every iteration too, but not done.

## The constraints, honestly

| Constraint | Status |
|---|---|
| **1. JS interop** | Met. Scheme closures stay callable JavaScript functions; compiled procedures keep the same wrapper. Value representation is untouched, and compiled code converts at the boundary exactly as the interpreter does -- including where the interpreter is inconsistent: a JavaScript function's integral result reads as exact through `js-invoke` and inexact through a direct call (`Interoperability.md`, *Numbers at the boundary*). No benchmark measures interop yet. |
| **2. Browser + CLI** | Met. Generated code is ordinary JavaScript; the libraries and the compiler are AOT-compiled, and a browser page fetches the compiler after it has started, to compile the page's own code. |
| **3. REPLs in both** | Met. Compilation is a backend *after* `analyze`, so `analyze` stays runtime-callable and `eval`, `load` and macro expansion keep working, and both REPLs compile what is typed into them as it runs, by the policy above. |
| **4. Debuggers in both** | **Met by running compiled code as its closures where it is debugged**, in the CLI and the browser: a procedure holding a breakpoint runs as its closure, and every one while stepping or paused, so every breakpoint fires, the library's and the program's own included, stepping and `:bt` see every frame, and a breakpoint in a callback of the compiled `map` stops the program where it is hit, `map` still compiled. Not reached: code compiled with no closure kept -- `tryCompileDefinition`, and a procedure a compiled top-level loop made and kept -- and debugging compiled code in place, which needs source maps. See below. |
| **5. Multi-shot `call/cc`** | Met, across any number of alternations of compiled and interpreted code. Refused rather than answered: a capture beneath a redefined inlined primitive. A continuation captured above a JavaScript caller that is not compiled code leaves that caller out, as the interpreter's always have. |
| **6. R7RS-small** | The compiler adds two refusals: the capture above, and `raise-continuable` handed to a compiled procedure as a value. The rest are the interpreter's: `equal?` on circular structure, `read-char` returning strings, and the file procedures returning a procedure's exact integer as inexact. Both conformance suites pass with the standard library interpreted and compiled, inside `npm test` -- three of Chibi's only because its runner rescues a failure whose values agree once converted to JavaScript (R85); and passing them is not evidence of completeness, since neither tested `call-with-port`, which was missing. |

Constraint 4 has **two mechanisms, not one**, which is what every real toolchain ships:

- **Declining to optimize what is being debugged** -- shipped per procedure. While a program is
  being debugged, the closures run compiled that hold a breakpoint run as themselves again, and every
  one while a step is in progress or the program is paused (`debugger-interpretation` in
  `src/core/scheme/debugger.scm`, `Interpreter.interpretForDebugger`, `interpret-compiled-over!` in
  `src/core/scheme/library_system.scm`): its own and its libraries', which the registry's other
  programs share and get back once none of them is being debugged.
  The closures are recorded when they are made to run compiled, by the library registry current
  then, each with the environment it was compiled in. Whatever holds a closure holds the object
  switched, a program's data too. The equivalent of compiling at `-O0` while debugging. A
  shipped library restored from its table makes no closures (`libraryRestorer` in
  `src/compiler/prebuilt.js`), so its procedures stay compiled while a program is debugged, and a
  breakpoint inside one is reported as being in compiled code: the user's choice, since
  debugging compiled code in place, below, is what is to reach them. A library loaded from its
  source -- one without a table, or whose table is stale -- switches as before.
- **Debug info** -- source maps and emitted debug points, so compiled code can be stepped and
  inspected in place, without switching. This is what calling convention B was chosen for: one live
  Scheme frame is one JavaScript frame, so DevTools can show a Scheme stack. Its frames are named for
  their procedures and, where the code was read from a file, mapped to their places in it
  (*Compiled code in a stack trace*); a page's scripts and the prebuilt tables are still to be.

The first is not a lesser substitute for the second. Lowering beta-reduces immediately applied
lambdas into bindings, lifts nested procedures into factories, inlines primitives and boxes assigned
locals; a source map maps *locations*, and cannot resurrect a binding that no longer exists. So debug
info yields "optimized out" exactly where a user is most confused, and the interpreter yields the
real value.

### Running compiled code as its closures where it is debugged

The debugger pauses only between the interpreter's steps. Compiled code takes none, so a breakpoint
inside it could not fire. Worse, a breakpoint in an *interpreted* procedure that compiled code
called was reached in a synchronous nested run of the interpreter, which cannot wait: in the browser
REPL, a breakpoint in a procedure given to the compiled `map` was reached on every element and the
program stopped only when `map` returned. The first fix switched the whole program while it was
debugged, since it is the compiled library that makes the nested runs, and the whole program then ran
at the interpreter's speed.

Now such a run moves the compiled frames beneath it to the heap, as a continuation captured there
would, and the step is taken again, and paused at, by the run that finishes the move -- the
asynchronous loop's, which can wait (`beginStepAgain` in `unwind.js`, `Interpreter.step`). No
continuation is taken: the frames go on the stack as a move for a call too deep to make puts them,
with the nested run's own on top. So only what a breakpoint is in need run as its closure, and every
procedure while a step is in progress or the program is paused, since a step may go anywhere. A
program using the compiled library, debugged with a breakpoint in a procedure it does not reach,
runs in 0.15 s with only that procedure switched, where switching the whole program took 1.6 s.

- **What switches.** The pairs are switched through the frames that hold them -- in the program's
  global environment and in every library loaded in the current registry -- so the cells compiled
  code reads globals through follow, and compiled code still running calls the closures from its
  next call on. A registry's libraries are switched back once none of its programs is being
  debugged.
- **When.** `SchemeDebugRuntime.updateInterpretation`, on setting or removing a breakpoint, stepping,
  pausing, resuming, enabling or disabling, and at the start of each asynchronous run, which catches
  a library loaded during the session. An enabled runtime with nothing set costs nothing: the CLI
  REPL enables one at start-up.
- **What it gives.** Every breakpoint fires, the library's included, beneath compiled code too; a
  step goes into any procedure; `:bt` has every frame; every local is an interpreted binding, by its
  own name. In the CLI and the browser alike.
- **What it does not reach.** A procedure compiled with no closure to go back to --
  `tryCompileDefinition` compiles from the analyzed definition, and the REPL's `:break` still warns
  that a breakpoint there will not fire. A compiled procedure a program holds in a data structure,
  or has captured in a closure. A pause the program asks for itself, `(pause)`, inside a nested run:
  it is not a step the debugger stops at, so it is not taken again. A nested run that JavaScript
  other than compiled code started -- a callback the host calls -- cannot move what is beneath it,
  and a breakpoint there stops the program once that JavaScript returns, as without compiled code.

| Context | Interpreted code | Compiled code |
|---|---|---|
| CLI REPL | the `:break` / `:step` / `:bt` debugger | runs as its closures where a breakpoint is, or everywhere while stepping |
| Browser | the REPL debugger, cooperative under `runAsync` | runs as its closures where a breakpoint is, or everywhere while stepping; in place through DevTools and source maps, still to come |

## How this is verified

The gap first, since it is what to distrust: CI runs only on `main` and never loads the browser
tests. `compiler_plan.md` ranks closing it.

- **6,124 tests**, Node and browser, via `npm test`.
- **A differential fuzzer** (`tests/fuzz/`): a generator, written in Scheme, builds programs from a
  seed -- loops, closures, assignments, escapes, a continuation captured at a random site and
  re-entered twice, errors raised and caught or not, `dynamic-wind`, multiple values, higher-order
  calls through the library, recursion deep enough to move frames, alternating between the tiers --
  and says which procedures to compile. Each is run with everything interpreted and with the
  library and those procedures compiled, and the answers compared. 120 fixed seeds run in
  `npm test`; `node tests/fuzz/run_fuzz.js` runs as many more as wanted. Five bugs reintroduced into
  the capture, moving, boxing and liveness machinery were each found within the first 27 programs.
- **R7RS conformance, in both library configurations**: the chapter tests and Chibi's, 1,201 in
  all, run with the standard library interpreted and again with it installed from the prebuilt tables
  as a browser installs it, which is also checked to have happened (`compliance_tests.js`).
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
| `src/compiler/driver.scm` | what is compiled, what is declined and why, and the records the entry points return |
| `src/compiler/safety.scm` | the call-graph closure, and its measured trade-off |
| `src/compiler/tier.scm` | when a program's procedures are compiled, installing them, and the re-entry policy |
| `src/compiler/host.js` | what the compiler's Scheme takes from the interpreter's JavaScript, and why each is there |
| `src/compiler/prebuilt.js` | staleness, and why arity rather than names |
| `src/compiler/runtime.js` | the trampoline, stack room and moving frames, global cells, reporting a non-procedure, procedure marking |
