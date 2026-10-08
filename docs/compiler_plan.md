# Compiler plan

What is being worked on in the compiler effort, in what order, blocked on what. **This is the only
place that ranks compiler work.** `../ROADMAP.md` holds the high-level arc of user-visible features,
planned and finished; `compiler_findings.md` holds numbered findings; `../CHANGES.md` holds
walkthroughs. None of those rank work.

**The rule that keeps it true:** read this before starting a task, update it when finishing one.
A plan nobody reads is how the last one rotted.

**How to read the table.** The *row order* is the suggested order, and rows move as priorities do.
The number is the task's name and does not change once the task has been cited, because
`../CHANGES.md` and `compiler_findings.md` are append-only and cite tasks by number; since
2026-09-27 a moved task keeps its number, so the numbers are no longer in order. `Depends on` is the
*binding* constraint, and names a task rather than a number: a task is blocked while a task it
depends on is unfinished. Several items are genuinely independent, so a row above another is not
a prerequisite of it.

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

**Code-generation decisions are measured twice.** The canonical suite (`npm run benchmark:r7rs`)
decides whether a change ships -- it must improve a workload class and regress none -- but it is blind
to a construct its programs do not exercise hot: `case` dispatch got 20-30x faster compiled, and
worse interpreted, without moving a class. So each decision also gets a group in
`benchmarks/run_codegen.js` (`npm run benchmark:codegen`), timing that construct in the shapes that
decide its cost, in both tiers. Before designing, a ceiling (R61) and a profile (R71).

**Decisions about compiling, and ports, are measured on more than kernels.** The canonical programs
are kernels -- a handful of procedures and one hot loop each, sized for native implementations --
and on the tier's policy they pointed the wrong way (80). So `benchmarks/run_tier.js --set all`,
which adds the repository's Scheme test files run as scripts, the corpus's own test programs and
synthetic programs in the shapes of a page's code, with `--policies` interleaving variants, decides
anything about what compiling costs or when the tier compiles (41, 58, 69, 77). A code-generation
change keeps the canonical suite and `run_codegen.js` as its gate and must not regress the corpus
and page sets either (37, 43, 54). A port of JavaScript to Scheme, whose cost falls on the
interpreted and start-up paths the kernels barely touch, is measured on the test-file and corpus
sets as well as `npm run benchmark:self-host` (63, 64, 66, 68). One process holds a long
comparison: since a library registry made for a while takes its scopes and interned syntax with it
(R106-R108), `--set all` with three policies runs in 1.2 GB, its live heap under 190 MB. The sets are
small -- 22 corpus programs, three synthetic -- and grow as more of the corpus's test suites run.

**New code, the interpreter's as well as the compiler's, is written in Scheme.** Not written in JavaScript and ported later — that
ordering never produced a port (R56). Where Scheme lacks a capability the compiler needs, build it as
a Scheme library over the minimum JavaScript. Unported JavaScript stays reachable: Scheme calls it
through interop, and it calls Scheme through the public interop too, `callSchemeProcedure` or a
procedure's plain call -- the compiler's Scheme included, whose exports `../src/compiler/lowering.js`
hands out once it has started it, and which never gains logic. A task's outcome names the JavaScript it could not avoid, and why, checked against
`npm run audit:languages -- <base>`. The rule in full, with what may be JavaScript, is *Scheme first*
in `../.agent/rules/rules.md`.

---

## Live

**Reordered 2026-09-26**, after an outside assessment of the whole effort
([compiler_assessment_2026-09-26.md](compiler_assessment_2026-09-26.md)), for the reason in
*Decided*: the tier's first user is this implementation, and twenty-seven tasks in, no user code
reaches it. 28-35 get it there -- correctness and test coverage first, then the refused shape, the
debugging policy, and the switch itself; 36-39 are what showing it to anyone needs; the rest are as
before, re-reasoned where the assessment changed the reason. Not everything in the assessment was
taken: its escape-based design for exception handling was wrong for most of the forms it covered,
and 37 carries the corrected version.

**Moved 2026-09-27:** 32 to the end, blocked on the merge into `main`, since this branch is to have no
CI of its own; 49 to just after 34, since mutable strings are an R7RS requirement agreed at the start
and the commonest reason real libraries do not load (R86).

**Reordered 2026-09-29:** as much of the interpreter and compiler as can be moves to Scheme (*Decided*),
and the ports are ranked against the other tasks by these criteria, heaviest first. (1) Never extend
JavaScript that is to become Scheme: a task that would change such code goes after its port, or is
written in Scheme as the port, so the work is done once. (2) Correctness and user-visible gaps keep
their places; ports ride along with them through (1). (3) Evidence before guesses: ports that give the
compiler real work rank above tasks with no evidence yet (54), and each records where compiled Scheme
loses to the JavaScript it replaced. (4) Small ports first, to settle how the evaluator's hooks call
Scheme and what it costs. (5) The reader and expander after that, since they need a bootstrap seed and a
start-up measurement. (6) The evaluator last, gated on speed. 51 is merged into 50, and 52 into 45.

**Reordered 2026-09-30:** JavaScript calling a compiled procedure as a plain function gets compiled
code's own calling convention -- its arguments and its result unconverted, a pending `TailCall`
object, a stack overflow on deep recursion -- so `../ROADMAP.md`'s constraint 1 fails in the
browser's default configuration, where the tier compiles a page's callbacks, and no test combines the
tier with JavaScript calling Scheme (70). Found by asking why `lowering.js` calls the compiler's
exports through `call` rather than directly: `call` works around exactly this, and the JavaScript
around the compiler -- `lowering.js`, `index.js`, `tiering.js` -- drives the compiler's Scheme from
outside, where ordinary calls would do. So 70, 71, 72 and 47 go first, as a user-visible correctness
gap, above 69's start-up time; 73 next, being small and independent; 74 ahead of the ports, since it
is criterion (4); 75 once 72-74 are done; 76 among the late ports; and 77, a goal decided the same
day, beside 41.

**Moved 2026-09-30, later,** after checking what the first tasks would write in JavaScript: most of
it need not be. 70's tests are Scheme, with a runner for both tiers. 74 moves up to follow 70, since
once the interpreter applies the tier's hooks, `call` is off the path 71 is about; 71 is kept only if
70's test still fails after 74. 47 begins by moving two file procedures to Scheme, and 78, which moves
the other two, comes with it. 69 tries its candidate that adds no JavaScript first. And 72 says where
its design lives, in `emit.scm`.

**Moved 2026-09-30, after 70:** 71 ahead of 74. 70 found that the tier abandons every compile it
starts beneath compiled code, so a procedure whose second call comes from compiled code is never
compiled (R93). 71's fix is one line; 74, which would also fix it, is a port.

**Added 2026-09-30, after 71:** 80, beside 69. Once 71 let the tier compile what it had been
abandoning, measuring the canonical programs under the tier found compiling to be most of a short
program's run (R94), a cost every CLI run and every page with the tier pays, as each pays 69's.

**Moved 2026-10-03:** 37 and 38 down beside 54. 37(b), next, was ranked on `run_escapes.js`, where an
escape costs several times a jump; counted in programs (R111), captures are rare and the scope recommended
covers few of them, so it is code generation without the evidence to rank it higher. 75 is next.

**Moved 2026-10-04, decided with the user:** the correctness gaps 55, 56, 57 and 48 first, then 67, 40, 63
and 45 as before. Debugging the system's own Scheme -- 62, and 84, which maps the shipped libraries for
DevTools -- waits until near the end: useful, but to a subset of users, and not until there are users. 58 moves
beside 37, since it pays only if many procedures stay declined, and since 37(a) few do.

**Added 2026-10-04, after 85:** 87, first, since it is a correctness gap and small: three keywords R7RS
requires that 85's audit found unimplemented (R116). The analyzer's special forms being found in every
environment, imported or not, which 85 also found, goes to 45, whose expander binds keywords in scopes.

**45 done 2026-10-05**, in the four increments decided with the user; its outcome is under *Completed*.
What it left: the seed and the library system know which names are special forms from a JavaScript
list (`SYNTAX_KEYWORDS` in `library_registry.js`), beside the expander's own (`special-form`), since the
seed must know them before any Scheme runs; the two are kept by hand.

**43 done 2026-10-06**, prototyped and kept as decided with the user; its outcome is under *Completed*.
What it left is 88, placed after 44: the flonum class is faster than before 43 overall, so `fibfp`, `fft`
and the rest rank below finishing the benchmark suite, which would measure interop and start-up, both
among 43's costs. Its start-up cost, the compiler's image, is 41's.

**Added 2026-10-05, after complex arithmetic was made exact:** 89, first. A correctness gap, so it ranks
with the others; it was to wait for 43, since it adds to the numeric primitives 43 wraps, and 43 is done.
Written as 88 on its own branch, and renumbered when merged, since 43's follow-up had taken 88.

**89 done 2026-10-06**; its outcome is under *Completed*. What it found is 90, first: the rest of Chibi's
tests are run as our copy rewrote them, which hid real failures in the number section (R122).

**90 done 2026-10-06**; its outcome is under *Completed*. Every Chibi test now runs as Chibi wrote it.

**66 done 2026-10-06**; its outcome is under *Completed*. What it found is 91, first: `run_tier.js`, on
which the tier's policy and every port are judged, loads each shipped library from source where a page
restores it from its table (R124), so its figures overstate everything a run does but compile. It is
small, and it corrects the instrument the tasks after it are ranked with. It found too that the
self-host benchmark's last row was mostly the printer (R125), now timed without it, and gave 54 and 41
evidence of their own.

**91 done 2026-10-06**; its outcome is under *Completed*. With libraries loaded as a page loads them,
the comparison behind the tier's policy (80) comes out as it did, so the policy stands; the figures are
under *Decided*. The tests that said they ran as a page did not either (R126), and now do.

**76 done 2026-10-06**, decided with the user at the size it turned out to be (R128): the build steps
and the self-host benchmark are Scheme programs; `decline_reasons.js` and `run_macro.js`, analysis
tools run by hand, stay JavaScript, as `index.js` keeps the entry points they and other tests call.
Its outcome is under *Completed*.

**44 half done 2026-10-06**: the interop, start-up and debugger-on axes and the coverage report are
in; what is left is two downloads, the user's to approve. The interop axis found 92, placed next:
JavaScript calling a compiled procedure costs what calling an interpreted one does, and the
constraint that sets this implementation apart is crossing that boundary.

**92 done 2026-10-06**; its outcome is under *Completed*.

**Moved 2026-10-06:** 88 down beside 54. Measured before building, its raw-double entry would serve
the canonical kernels: the programs that are not benchmarks make almost no inexact integers (the row
has the counts). 41 is next.

**41 measured 2026-10-06** (R132): the twins that can never run are under 2% of the code, so what is
left of 41 -- not shipping the twins at all -- waits on the user's judgement of page load.

**2026-10-07:** 44 is done. Vendoring `compiler` found a defect that stopped any program with a
procedure whose one parameter is named `set` (R134), and that a compiler error ends the program rather
than leaving the procedure interpreted: 93, first. The user judged page load and size acceptable for
now, so 41 moves down, its direction several runtime files a page chooses among.

**2026-10-07, later:** 93 and 94 are done. 94 found 95: a compiled program's library procedures share
V8's type feedback with the compiler's copies (R137). It is ranked first for the user to confirm; a
remedy that costs page size is theirs to decide.

**2026-10-07, last:** 95 measured and not shipped, decided with the user (R138): separating a
program's library code from the system's gains 2% on the compiled list class for 23 ms on every page's
start. 77 is next.

**2026-10-07, after 96:** the user chose to go ahead with 77 as scoped. Its increment (1) is done; (2)
to (5) are 97 to 99, ranked first. 99's command and output are the user's to spell.

**2026-10-07, after 98:** 98 is done; 100, programs that read, is added after 99, which it needs.

**2026-10-08, during 99:** 101 found and done (R141); 102, measuring pages through the bundle, added after 100.

**2026-10-08, after 99:** 99 is done; 103, `this` in compiled code, which classes need ahead of time, added after 102.

**2026-10-08, after 100:** 100 is done; 102 is next.

| # | Task | Depends on | Why here | Evidence |
|---|---|---|---|---|
| 102 | **The canonical suite through the page bundle** | — | From 101 (R141): every figure for compiled code was taken in Node from the source modules, and a page, which runs the bundle, ran some programs up to 1.9 times slower for a reason only the bundle had. A harness that runs the canonical suite, the tier attached, through `dist/scheme.js` -- in Node, and in headless Chrome -- beside the source modules, so that any other difference the bundle makes is seen. | R141 |
| 103 | **`this` in compiled code** | — | From 99. The interpreter binds `this`, a method's receiver, as JavaScript calls a procedure as a method (`frames.js`); compiled code has no receiver, and read `this` as a global, so a procedure that made a method failed under the tier. Such procedures are declined now (`ir.scm`): the tier leaves them interpreted, and a program compiled ahead of time that reaches one is refused -- every class whose constructor or methods read its fields through `this`. Compiling them means the receiver reaching compiled code -- a method entered through its raw entry with JavaScript's `this` -- and staying with the closures made inside it, across lifting and suspension. | — |
| 58 | **Compile the inner loops of a procedure the tier declines** | — | From 34. The tier compiles top-level procedures, each with everything nested in it, so a procedure it declines -- one that reaches `call/cc`, say (37) -- keeps its inner loops interpreted too, however hot. They need not: `tryCompileClosure` compiles a closure against its own environment, a named `let`'s procedure included, and the interpreter looks a local loop's name up in its frame on every iteration, so a compiled one bound there would be picked up at the next iteration, with no on-stack replacement. Untested on a call's frame, and needs the count on local closures too, which is a cost on every interpreted call that 34 kept to top-level ones. Worth it only if 37 leaves many procedures declined. Judged on `run_tier.js --set all`, not on the canonical suite alone (*Decisions about compiling* at the head of this file). | — |
| 37 | **The decline policy on real code: escapes, not exception handling** | — | `ir.scm`'s control globals put `guard`, `raise`, `with-exception-handler`, `parameterize`, `dynamic-wind` and `exit` beside `call/cc`, and `safety.js` declines every procedure that can reach one or that captures -- directly, through its unit, or through an interpreted closure. The plan assumed applications use the exception forms everywhere. **Measured on real code, 2026-09-27** (`decline_reasons.js --corpus`, over 7 SRFI reference implementations and 17 Snow-Fort packages recorded in `benchmarks/corpus/manifest.json`; [corpus_decline_results.md](corpus_decline_results.md)): of 1,855 procedures 78% compile, and **of the 406 declined for a control form, 399 end at `call/cc`** -- 305 of them only by reaching a capture through another procedure. `guard`, `with-exception-handler`, `parameterize` and `dynamic-wind` decline five. And nearly every capture is an **escape** -- `(call/cc (lambda (return) ... (return x) ...))`, often from a callback given to `for-each` or a search, or SRFI 146's pattern-matching macro -- with re-entry only in coroutine generators and Schelog (31 declines). The capture default was justified on `btsearch`, which re-enters; for escapes it is the slower choice: `benchmarks/run_escapes.js` has compiling the captures 1.5-3.9x faster than the default at every depth measured, the gap narrowing with the compiled frames a capture unwinds (R86). **So the order is now:** (a) change the default for captures and for the reachability rule by shape, or drop them. **Measured 2026-09-28** (`--captures` on `run_compiled.js` and `run_r7rs.js`, R90): compiling them is 21x faster on `quicksort`, 4x on `puzzle`, 3.8x on `maze`, 2.9x on `contfib`, 1.35x on `threads`, and slower on `btsearch` (4.5x), `fibc` (1.8x) and `ctak` (1.1-1.2x) -- escapes taken now and then against captures at every call or re-entered, which no static test tells apart. **Decided 2026-09-28: per procedure, as the program runs, and done.** Every procedure is compiled, those that capture included, over the closure the interpreter made of it (the tier, `compileProgram`, `compileEnvironment`, the prebuilt tables and the canonical harness all do); saves and resumes of each procedure's compiled frames are counted, and one whose frames are resumed at least four times as often as saved, after a thousand resumes, is switched back to its closure for good (`noteResume` in `unwind.js`, `switchBackToClosure` in `library_registry.js`). Counting captures or their rate could not have worked: `contfib`, a 2.9x win, captures faster than any loser; re-entry separates exactly -- `btsearch` resumes 200 times per save, every winner once. Result: `btsearch` back to the old rule's 69-72 ms while `quicksort`, `puzzle`, `maze`, `contfib` and `threads` keep their wins; `fibc` (1.8x) and `ctak` (1.2x) stay slower, since they capture at every call and resume each frame once. `declineCaptures` keeps the old rule; (b) next, in Scheme like the rest of the driver (50) -- its proof is an analysis of what `ir.scm` produces -- an escape fast path, **only where the compiler proves the continuation is not kept**. A continuation called while its capture is still on the stack could reach the capture with a JavaScript `throw` (a plain object, so no stack trace is built) caught there, saving no frames. But that alone is unsound, and so is a fall-back taken "when the receiver returns": the frames a continuation needs are those *below* the `call/cc`, still on the stack after an escape and discarded as they return, so `(call/cc (lambda (k) (set! saved k) (k 1)))` followed later by `(saved 2)` would find them gone. Whether `k` was kept cannot be told cheaply at run time -- it would mean watching every store into a variable, pair, vector, closure or JavaScript object -- so it is proved when compiling, and every capture not proved safe takes today's protocol unchanged; the unsafe case then cannot arise, rather than depending on each exit being caught. **Two scopes:** *local* -- the receiver's parameter appears only as the operator of calls, never stored, returned, captured by a closure or passed on -- covers the library escape pattern and is simple; *across procedures* -- `k` passed only to known procedures whose matching parameter is proved the same way, a fixed point over the program's globals -- is what `fibc` and `ctak` need, since both pass `k` on (to `addc`/`fibc`, to `ctak-aux`) without storing it, and a proof resting on a global has to be dropped when that global is redefined, switching its dependants back as the capture policy already does. **Recommended: the local scope now, the cross-procedure scope as a later step if `fibc` and `ctak` still matter** -- they are benchmarks built to stress `call/cc`, and the corpus shows real code escaping locally. What the throw must still do on the way out, as the full protocol does now: run the `dynamic-wind` after-thunks between call and capture in their own dynamic environment (handlers, `parameterize`), restore the state of any nested interpreter run it crosses, and leave the debugger's stack tracking consistent. **Testing**, each case checked against the interpreter's answer: every way `k` can be kept (variable, pair, vector, closure, returned, passed to a procedure the analysis cannot see, handed to JavaScript) must take the full protocol and re-enter correctly after its frames have returned; every way it is proved not kept must take the fast path, asserted by which path ran and not only by the answer; redefining a global a proof rests on must switch its dependants back and keep answers right; every way control leaves (the receiver returns, escapes through `k`, through another continuation, raises) crossed with what lies between (`dynamic-wind`, `parameterize`, handlers, a nested interpreter run, a JavaScript frame, the debugger on); and the fuzzer extended to generate programs that store continuations, escape, and re-enter them later, which it now rarely produces. Performance: `fibc`, `ctak`, `run_escapes.js` and the programs the capture policy wins on must hold or improve, and how fast V8 unwinds a throw through a deep compiled stack is measured before the design relies on it; (c) the exception forms last. Re-measured after 49 let the Chibi libraries load, which hold most of the corpus's `guard` and `parameterize`: 2,235 definitions, and still 399 of 407 control declines at `call/cc`, 3 at `guard` or `with-exception-handler`. The design for (c) stands: `guard`'s escape into its clauses and `exit` are one-shot and upward and can be (b)'s mechanism; a `with-exception-handler` handler runs in `raise`'s dynamic context before anything unwinds, `raise-continuable` returns to its raiser, and a `guard` with no matching clause re-raises in the original `raise`'s context -- so the handler stack and the wind list become runtime state compiled code can call through. Several are on the list for how they are implemented, not for what they do: `raise`'s primitive returns a node for the interpreter to run, and `guard` expands through `call/cc` in `control.scm`. Found in 28: a control global handed to a compiled procedure as a value -- `(map call/cc ...)`, `eval` -- still fails with JavaScript's "args is not iterable", since its pending call carries an expression for the interpreter. Must not regress `run_tier.js`'s corpus and page sets, beside the canonical suite and `run_codegen.js` (*Decisions about compiling* at the head of this file). **Found in 45:** a `parameterize` entered from compiled code costs about 100 microseconds, through `dynamic-wind`; one around each top-level expansion made expansion 1.8 times slower. **Moved down 2026-10-03, before (b) was built (R111):** the local scope covers 9 of the corpus's 77 captures, the library pattern being an escape from a callback (56), and captures are rare under the tier: outside `ctak` and `fibc`, which pass `k` on, at most 2,572 in a program, two compiled frames each; `puzzle`, the canonical program the local scope covers, would gain at most 10%. Should it come back, the scope is the callback's, and the cost to beat a few milliseconds a program. **2026-10-07 (96, R139):** compiled `ctak` and `fibc` were slow mostly because invoking a continuation across compiled code threw an `Error`, taking a stack trace; a plain object since, they run compiled 1.8 and 4.3 times faster than interpreted, so (b)'s case for the cross-procedure scope, which they were, is gone. **Since 98** a program compiled ahead of time that reaches any of these is refused, by name, since it has no interpreter to run them: (c) is what lets such a program run ahead of time, `parameterize`, and `with-output-to-file`, which uses it, among them. | R23, R49, R86, R111, R139 |
| 38 | **Lowering failure as an escape, once compiled code can escape** | The decline policy on real code: escapes, not exception handling | The compiler's Scheme was written to avoid every declined form (`ir.scm`'s header says so), and the visible cost is lowering failure: `fail!` returns `#f`, and each of about twenty sites in `lower-node` checks for it before going on -- `(if (not test) #f (let ((then ...)) (if (not then) #f ...)))`. Once 37 makes `guard`'s escape compile, `fail!` escapes instead, from a handler established once per definition, and those sites become plain `let*`s. Nothing needs undoing on the way out, since a failed lowering discards its whole state. **Clearer, and a test, rather than faster:** removing a comparison per site saves little, but the self-host differential then exercises compiled exception handling on every run, and the compiler is the tier's most demanding user. Set up the handler per definition, never per node: the compiler also runs interpreted, where `guard` expands through `call/cc` and copies the frame stack. Measure with `npm run benchmark:self-host` in both tiers. At the same time correct that header, which still says the compiler avoids `apply` and `values` -- both compile now -- and look for anything that builds a list only to return two results. The rest of the compiler's Scheme stays as it is: it holds no resources for `dynamic-wind`, never backtracks, and passing its lowering state explicitly is faster than `parameterize` and good Scheme anyway. | — |
| 88 | **What the new number representation still costs** | — | From 43. Integral inexacts are boxes, and where they cross calls or sit in data no expansion removes them (R121): `fibfp` is 1.81x the code before 43, its values all integral and every call taking and returning a box; `fft` 1.18x, its data a vector of `0.0`. The candidate for `fibfp` is a raw-double entry for a procedure whose parameters provably stay inexact, beside its ordinary one, as the loops on raw doubles have (`emit-double-loop!` in `emit.scm`), with the stack-room and capture protocol a recursive procedure needs and a loop does not, and with nothing inside it that boxes or calls (R120); for `fft`, nothing yet but measuring how common integral inexacts in data are outside the benchmarks. Also: `chudnovsky` 1.41x, unprofiled -- conversions at the edge of the safe range are the guess; ten mutual tail calls in `run_codegen.js` 105 ns against 92 with identical generated code, unexplained; and a math primitive without a direct path still pays the wrapper, which cost `(inexact x)` 33 ns a call against 4.6 before it had one. **Measured 2026-10-06, and moved down beside 54:** what it would remove falls on kernels. Profiled, `fibfp` (2.9 ms against 1.5 before 43) spends its time in its own code on the boxed representation -- 365,000 inexact results a run, only 89 of them new boxes, the rest the shared small ones -- and `chudnovsky` (200 us against 148) spreads its time over bignum work, conversions at the edge of the safe range about 13% of it. Counted over `run_tier.js`'s sets, one run each, the boxes a program makes and the inexact results it computes: the 74 test files 2,028 and 150,427, the 23 corpus programs 7,513 and 272,319, the page programs none and 4,060, the 44 canonical programs 249,105 and 1,591,435, most of it `quicksort`, `fft`, `fibfp` and `mbrotZ`. A raw-double entry would be an optimization fitted to the kernels, by those counts; it waits, as 54's do, for code outside the benchmarks that does heavy integral inexact arithmetic. Judged on the canonical suite, `run_codegen.js` and `run_tier.js --set all` (*Decisions about compiling* at the head of this file). | R120, R121 |
| 54 | **Code generation without evidence yet: arity specialization, unboxed fixnum paths, escape analysis** | — | Listed as the next code-generation targets until a profile of the compiled tier found none of them among its costs (R71); kept here, last, until one does. Profile first, then ceiling, then design. Since 2026-09-29 the ports to Scheme (50, 63-66, 45) are where that evidence should come from: each records where compiled Scheme loses to the JavaScript it replaced, with the profile. **First evidence, from 78:** the procedures that take an optional port, Scheme over JavaScript cores, lose 20-30 ns a call to the JavaScript they replaced -- `write-char` to the current port 18 to 40 ns, to a port passed 19 to 49 -- from the call itself and from the list a rest parameter is made into on every call, and `dynamic`, which calls `read` constantly, ran 5-13% slower: an optional argument taken without a list is arity specialization's case. Arity specialization has a measured cost to recover since 2026-10-02: every fast form tests `arguments.length` on entry, so that a procedure called with the wrong number of arguments signals an error in both tiers, which costs call-heavy compiled code 2-5% (fib 10 in `run_codegen.js --only recursion`, 1,970-2,000 against 2,070-2,115); a call whose callee and count the compiler knows, a self-call above all, could enter past the test. **Since 43** an exact integer in the safe range is a JavaScript number, so the unboxed fixnum path is the representation itself; its analogue left is raw doubles across calls (88). Must not regress `run_tier.js`'s corpus and page sets, beside the canonical suite and `run_codegen.js` (*Decisions about compiling* at the head of this file). **Evidence from 67:** the debugger's `should-pause?`, asked at every step of a program being debugged, spends its 0.12 µs on out-of-line calls -- a record accessor, `string?`, `real?` -- that inlined would be a field read and two `typeof` tests: record accessors and type predicates are not among the inline expansions. A program under the debugger with a breakpoint set runs 1.9x slower than with the JavaScript debugger it replaced (19.3 ms against 10.4 for `(fib 18)`). **And from 63:** the reader spends a fifth of its time in record accessors, every field read a call into `record.js`. **And from 66:** the printer asks of every value it writes what it is, and `symbol?`, `string?`, `number?` and `vector?` are calls, as are `char->integer`, `string-ref` and `char=?`, the last two taking a rest list as `char-ci=?` does; written so, the printer wrote symbols 15 times as slowly as the JavaScript it replaced. Comparing characters by code, which compiled code does inline, and handing a scan of a whole name or string to one primitive (`%string-find-any`), took that to twice; an internal `define` in a procedure called for every symbol made a closure on each call. | R71 |
| 53 | **Drop `source` from runtime `Cons`** | — | Unchanged, unmeasured, low priority. | — |
| 59 | **A string that keeps its identity through JavaScript** | — | From 49. A Scheme string crosses into JavaScript as its characters, so one sent through JavaScript and back comes back as another string with the same characters -- the rule that keeps every JavaScript API working, decided 2026-09-27 on no user experience, to be revisited with some. For code that needs the same string back -- one parked in a JavaScript structure, or handed through JavaScript to a Scheme callback -- an explicit form that passes the `SchemeString` object itself, which JavaScript sees as an object, and which returns as the same string. Nothing automatic can do it: a JavaScript string has no identity, and recovering the Scheme string from its characters would alias strings that only happen to be equal (`Interoperability.md`, *Strings at the boundary*). | R89 |
| 62 | **Debugging the system's own Scheme** | Source maps and debug points | Asked for 2026-09-29, for debugging the interpreter and compiler themselves: a mode in which the system's own compiled Scheme appears in stack traces and can be stepped into, as a program's does. Chosen at load time, so it costs nothing when off: the shipped libraries load compiled over the closures their bundled source makes, as a program's code has been since 34, and 33's machinery runs them as closures while debugging. To design: the evaluator calls the tier's Scheme directly (74), which is a nested run that cannot pause (R82), so in the mode those calls are applied through the program's interpreter instead, as it applies any procedure, which runs the compiler's code there (R95) -- and the rule that the debugger skips the system's code while the mode is off comes with it; the debugger's own Scheme (67) stays out of its own stepping even in this mode; the reader and expander (63, 45) start compiled, since something has to read their source, and switch to closures once loaded. Worth it only if debugging a program is no worse for it and start-up in the mode stays tolerable; measure both. Next to 39, where the debugging design is being settled anyway. | — |
| 84 | **Source maps for the shipped libraries, and DevTools formatters** | — | From 39, which maps the code the tier compiles as a program runs. The prebuilt tables are module code, bundled by rollup, so a frame in `map` or `vector-map` is named but placed in `dist/scheme.js`: the table writer would write a map for `compiled_libraries.js` and `compiled_compiler.js`, the bundled sources as its `sourcesContent`, and rollup chain it into the bundle's (`output.sourcemap`). And DevTools' custom formatters (`window.devtoolsFormatters`), so a pair shows as a list, a symbol as its name and a record by its fields, which DevTools uses only when its user turns them on. Neither is needed to debug a program's own code. | — |
| 68 | **The evaluator, in Scheme** | Compiled Scheme fast enough for the evaluator's own loop | The step loop, the frames, the syntax tree's `step` methods and the environments (`interpreter.js`, `frames.js`, `ast_nodes.js`, `environment.js`, `context.js`, about 3,300 lines): the hottest code in the system, and the most tied to the debugger. It could be Scheme compiled ahead of time from the seed, like the reader and expander, but only once the ports before it show compiled Scheme close enough to hand-written JavaScript on hot code -- until then they are the measurement. The interpreter stays a permanent tier either way: this changes what it is written in, not whether it exists. JavaScript regardless: the value representations, `runtime.js`, code generation, the save-and-resume protocol and the host interfaces. Measured on `run_tier.js`'s test-file and corpus sets too, not only the kernels (*Decisions about compiling* at the head of this file). | — |
| 41 | **Smaller generated code** | — | **Decided with the user 2026-10-07: not urgent.** Page load and size are acceptable for now. Eventually, several runtime files for a page to choose among -- with and without the compiler, with and without the resumable twins -- where the runtime's size or a program's matters more. **Measured 2026-10-06 (R132):** the resumable twins are 47% of the libraries' generated code and 49% of the compiler's, but those that can never run are 1.9% and 1.3%; twins kept as text, made functions when first needed, save 9 ms of the libraries' 63 ms import and nothing of the download, 0.94 MB gzipped (the compiler 0.49 MB). The large lever left is not shipping twins, making one on a procedure's first suspension with the compiler a page fetches anyway -- about half the generated code -- which waits on the user's judgement of page load: a page runs its first script 196 ms after its navigation starts (`run_startup.js`). Since 34 every page also fetches the compiler after it starts, `dist/scheme_compiler.js`, whose own prebuilt table makes it about 2.2 MB -- most of it the compiler's own Scheme compiled twice over, so the same measurement and the same remedy serve it. Since 21 every page carries every shipped library compiled, so the generated code's size is now what a page pays for: SRFI 1, 125, 128 and 152 were 1.1 MB of `dist/scheme.js`'s 2.67 MB, now 3.04 MB, about 6 KB a procedure. Every procedure is emitted twice, fast and resumable, and a procedure none of whose callees can capture -- one that calls nothing able to call back into Scheme, say -- can never be suspended, so its resumable form is dead weight. **Measure first** how much of each table that is. The alternative, loading a library's table only when the library is imported, needs an asynchronous import, which the interpreter's `import` is not. 22's cell reads then made the generated code about 8% larger, and 26's direct tail calls 4.5-6% more, most of it the direct call and its fallback written out at each of about 1,200 tail call sites. 27's room on the stack added 6-9.5% more (4-6.5% gzipped): a line at the entry of each procedure that calls, a store before each call. 28's test of each callee for being a procedure 5.5% more for the libraries, 8% for the compiler; 31's operands in order 0.9% and 3.8%. Written in the Scheme emitter. Also worth measuring: materialising the resumable form from source text on its first capture where `new Function` is allowed, keeping the eager form for strict CSP. For scale, BiwaScheme is about 250 KB and Gambit's browser REPL 11-22 MB (`compiler_assessment_2026-09-26.md`, §4.2 E); not before the tier reaches users unless page load is judged too slow. Since 80, with a library's procedures waiting ten calls, the corpus's test programs still run slower with the tier than without, 20 or 21 of 23 measured as a page loads its libraries (91), compiling about 29% of the time, so less emitted per procedure is the lever there. Judged on `run_tier.js --set all`, not on the canonical suite alone (*Decisions about compiling* at the head of this file). **Since 67 and 63** the debugger's and the reader's tables, about 0.5 MB and 0.6 MB, have taken `dist/scheme.js` from 5.1 MB to 6.7. **Since 45** the expander's adds 1.2 MB more to `compiled_libraries.js`, from 45 KB of Scheme; holding the forms a table restores as JSON rather than as code that builds them took 0.2 MB back, and a start what parsing them cost: `compiled_libraries.js` is 6.9 MB and `dist/scheme.js` 8.1 MB. **Since 43** the compiler's own prebuilt image is 4.07 MB, from 3.41, all of it the emitter of loops on raw doubles -- about 600 KB of generated code for some 25 procedures, `emit-double-loop!` alone 98 KB -- and a CLI start, which parses it, is 10-13 ms slower. **Since 66** `(scheme core)`'s table holds the printer's 21 procedures, 753 KB to 1,030 KB, `print-compound` alone 70 KB, and a CLI start is 3 ms slower. | R65, R66 |
| 32 | **CI for `main`, when this branch merges, browser included** | The merge into `main` | `ci.yml` runs only on pushes and pull requests to `main`, runs the old numeric-tower benchmark, which measures nothing the compiler changes, and never loads the browser test page -- every browser test count in these documents was run by hand. **Decided 2026-09-27: no CI for this branch**, whose tests are run by hand as it is developed. When it merges, extend `main`'s CI to what the compiler changes: `npm test` -- the conformance suites in both configurations and the fuzzer's fixed seeds are inside it -- and the browser tests, headless; the Puppeteer harness on `debugger-take-3` is the shortest path to the second. Replace the numeric-tower benchmark step. | — |

## Completed

The fifteen most recent. Every completed task, these included, is in [compiler_plan_completed.md](compiler_plan_completed.md) under the same number, and anything older is there only. Detail in `../CHANGES.md`; what each one *falsified* in `compiler_findings.md`.

| # | | Task | Outcome | Evidence |
|---|---|---|---|---|
| 66 | ✅ | The printer, in Scheme | `write`, `display`, `write-shared`, `write-simple` and the text the REPLs show are `printer.scm` in `(scheme core)`; JavaScript keeps doors that call it (`writeString` and the rest, `prettyPrint`) and what only it can say of a value: a host object's fields, a procedure's name, a continuation, several values. Written now: a procedure by its name, a control character by R7RS's name or escape, an error object as its message, several values one to a line. Otherwise the same text as before for 12,601 data read from the repository, 21 holding control characters excepted. Against the JavaScript printer, 1.2-2.8x on large data and 1.5x on the self-host's lowered lambdas, once names and strings were scanned in one call and a datum was walked as a tree within a budget before its cycles were looked for, as `equal?` does; finding cycles still costs `write` 1.2-3.4x past the budget, as it did in JavaScript. Test-file set 2-3.5% slower, corpus set level, CLI start 3 ms slower (41). Found: `run_tier.js` loads every shipped library from source (R124, 91); the self-host's last row was mostly the printer (R125). JavaScript under `src/`: 134 lines added, 480 removed. | R124, R125 |
| 91 | ✅ | `run_tier.js` restores the shipped libraries as a page does | It, the tiered Scheme tests and the conformance suites' compiled run load libraries through `tests/harness/page_libraries.js`, which restores a shipped library from its table as `scheme_entry.js` does and fails a run that reads one from source; all three had read every shipped library's source and installed its table over it (R124, R126). The corpus set, today's policy, 3.64 s to 2.89 s, compiling 29% of a run; the comparison behind the tier's policy (80) run again on all four sets, with the same outcome, so the policy stands. Every tiered and conformance test passes on the path pages run. JavaScript under `src/`: none. | R124, R126 |
| 76 | ✅ | The build steps and the compiler's harnesses, as Scheme programs | `npm run prebuild` and `npm run pin:seed` run Scheme programs from the CLI (`scripts/generate_compiled_libraries.scm`, `generate_compiled_compiler.scm`, `pin_seed.scm`, over `scripts/lib/prebuild.scm`), writing what the JavaScript wrote but for the expander's renaming numbers, deterministically, the whole build in about 5.5 s; `benchmarks/run_self_host.scm` loads the compiler's library three ways and times the lowering alone, its corpus read as R7RS (R127). The CLI gained `-I`, the compiler's library importable as any library, and `(scheme-js compiler build)`, the doors a build needs. Decided with the user at the size it turned out (R128): `decline_reasons.js` and `run_macro.js` stay JavaScript, and `index.js` keeps their entry points. JavaScript under `src/`: `build_host.js`, 194 lines, the doors; the build scripts' JavaScript, outside it, gone. | R127, R128 |
| 92 | ✅ | JavaScript calling compiled Scheme through its compiled entry | A compiled procedure's plain call calls its code directly and hands what the code leaves -- a tail call, an unwind, an exception -- to a run whose first step takes it up with the stack the call would have had (`Interpreter.callCompiledEntry`): JavaScript calling a compiled procedure 460 ns to 48 (`run_interop.scm`), 497 to 92 in `run_codegen.js`, `Array.prototype.map` over ten 4,826 to 529. Its tests found that a continuation captured and re-entered inside a compiled procedure JavaScript called escaped to the JavaScript; fixed (R131). JavaScript under `src/`: the evaluator's, 127 lines added and 27 removed. | R131 |
| 44 | ✅ | Finish the benchmark suite | Four axes beside the canonical suite's timings -- interop (`run_interop.scm`), start-up (`run_startup.js`), the debugger not stopping (`run_debugger.js`) and coverage (`run_coverage.js`, which found the canonical suite nearly as concentrated on inline expansions as the Stage 0 programs, R130) -- and two programs. Thivierge and Feeley's `threads10`, transcribed by the user from their Figure 15, joins `threads`, which turned out to spend most of its time in `append` (R133). The canonical `compiler`, vendored at the pinned commit, found on arrival that a procedure whose one parameter is named `set` stopped the program under the tier (R134, fixed); it runs 13 ms an iteration compiled against 296 interpreted, Gambit's interpreter 21, Gambit compiled to JavaScript 33 and Racket 0.86. JavaScript under `src/`: none. | R130, R133, R134 |
| 93 | ✅ | A compiler error leaves its procedure interpreted | Decided with the user: warn. An error raised while compiling a procedure -- lowering, emitting, or making the code a function -- declines it with "the compiler failed" and the message as its reason, writes a warning naming it to the error port (the console on a page, standard error under the CLI), and keeps it until taken (`unless-failing`, `take-compiler-failures!` in `driver.scm`), replacing `emit-guarded`, which covered code generation alone and declined silently. The test suite ends by failing on any kept; the canonical harness reports one as the run's error, so the correctness pass and `run_r7rs.js` fail on it; `run_tier.js` throws. Checked end to end with R134's bug put back: a CLI program and a page both finished, warning once. One guard per procedure, about 4 microseconds. JavaScript under `src/`: none. | R134 |
| 94 | ✅ | The benchmark harness and the compiler's test runner expand as a program does | Decided with the user. The harness read the standard library's files into the program's environment, so their macros' expansions named globals a program could take over -- `parameterize` became whatever `param-dynamic-bind` the program defined -- and its compiled tier compiled that copy, not the shipped tables (R136). Now `withBenchmarkInterpreter` imports the libraries at the top level, as the REPL does, each run in a registry of its own, from source for the interpreted tier and from the shipped tables for the compiled; every benchmark goes through it, and `run_tier.js` shares its pieces. The compiler's tests run inside a registry of their own while it is current (`withCompilerLibrary`), so they can use `parameterize`. Measured against the code before: the interpreted tier and the Stage 0 programs unchanged, step counts identical, the compiled list class 10% slower, which is R137 and 95. The coverage report now counts only a program's calls into the language, which restated R135's figures (annotated) without changing its conclusion. Fixed on the way: `run_hash_tables.js`, broken since 43. JavaScript under `src/`: `withCompilerLibrary`, the start-up of `lowering.js` given a callback. | R136, R137 |
| 95 | ✅ | A program's library code apart from the compiler's | Measured, and not shipped, decided with the user (R138). Giving the compiler's copies of the libraries code of their own, made from the tables' text, changed nothing: the library system's seed shares the same code and is what feeds `length` and `zero?`. Giving a program's libraries code of their own instead brought `destruc`, `peval` and `scheme` back to their speed before 94, 10-18% faster, but the compiled list class only 2%, and cost every page 23 ms before its first script ran and the CLI 28 ms: the copies are parsed and compiled cold. Left as a known cost; a remedy whose cost follows use -- code of its own only for the procedures a program calls -- is the one to try if it matters. JavaScript under `src/`: none kept. | R137, R138 |
| 96 | ✅ | A continuation's unwind is not an `Error` | Found scoping 77: half of compiled `ctak`'s time was the constructor of `ContinuationUnwind`, thrown at every invocation of a continuation across compiled code, which extended `Error` and so took a stack trace. Made a plain object, as `CaptureUnwind` was: compiled `ctak` 152 to 68 ms, `fibc` 104 to 36, interpreted unchanged, so both now run compiled faster than interpreted, 1.8x and 4.3x (R139). Tested in `interop_tests.js`. JavaScript under `src/`: the evaluator's signal, fixed in place. | R139 |
| 77 | ✅ | Compiled code without an interpreter beneath it | Increment (1) of its scoping; (2) to (5) are 97 to 99. A capture made by compiled code, with only compiled frames between it and the outermost run and no debugger on, is finished by a driver of the runtime's own (`drive` in `unwind.js`): its frames kept as a shared list, each resumed through its twin, a continuation invoked from compiled code in the driver -- by a call or a tail call -- taken by a jump the driver catches, and the continuation the interpreter's too, its frame stack made only when something else invokes it; anything else goes to `completeCapture` as before. Compiled `ctak` 1.65x and `fibc` 2.5x faster, the continuation class 1.6x, `puzzle` 7% slower, every other class and `run_tier.js`'s test-file, corpus and page sets level. Moves to the heap stay with the interpreter: in the driver they made `earley` 23% slower in garbage collection (R140). Tested in `tiers/continuation_tests.scm` (both tiers) and `native_unwind_tests.js` (which way ran). JavaScript under `src/`: the save-and-resume protocol and the continuation it makes. | R140 |
| 97 | ✅ | The runtime's modules apart from the interpreter and the library system | What compiled code needs as it runs -- the runtime, the values, the environment, the primitive groups that stand alone, the console ports -- now bundles with none of the interpreter, the expander, the reader or the library system, which `runtime_separation_tests.js` checks by bundling it with rollup: about 430 KB unminified with every comment, where one import had brought in the library tables' 3.4 MB. Untied: `string.js` imports the number parser from its own module, not through the reader, whose module starts the library system's seed; `procedure?` and `apply` are registered from `apply.js`, not `control.js`; the error-object predicates and accessors are `error_object.js`, apart from raising in `exception.js`; the console and file ports get `node:fs` with `process.getBuiltinModule`, not a top-level `await`. Raising and handlers stay the interpreter's, for 98. JavaScript under `src/`: primitives moved, a module of them made, and imports fixed. | — |
| 98 | ✅ | A whole program compiled ahead of time | `scripts/build_ahead.scm`, over `(scheme-js ahead)` (`scripts/lib/ahead.scm`), compiles every form a program and the libraries it imports run as they load -- procedures, values, forms run for effect; no macro, since an expanded program uses none -- and writes a table that `runProgram` in `src/compiler/ahead.js` runs with no interpreter, expander, reader or library system, on 77's driver, which finishes captures and moves to the heap itself. Each name is followed to where it is bound at its point in the load, so a program may read an import before defining its own, as the interpreter lets it, and only what is reached is kept; what could not run is refused by name at build time -- a reached procedure the compiler declined (`guard`, `parameterize`, `dynamic-wind`), a constant that cannot be written down, a primitive the runtime does not carry (`read`, `eval`). Fixed on the way: `define-values` was declined everywhere, the lowering rewriting `call-with-values` only as a variable and not as the library's binding its macro writes; `make-parameter` was a control global; exact ratios and complex numbers could not be written as constants. `raise`, `raise-continuable` and `error` are `raise.js`, `values` is in `apply.js`. Canonical suite's default profile at the tier's calibrated counts (`benchmarks/run_ahead.js`): 43 of 45 run, at 0.52 to 1.02 times the tier's time, median 0.93; `earley` 1.2 times slower, its difference garbage collection, as R140 found when moves were finished in the driver; `read1` and `dynamic` read files, and there is no reader (100). JavaScript under `src/`: the loader, as the runtime compiled code starts on; two build-host doors; primitives moved. | R140 |
| 101 | ✅ | Generated code reads the runtime fast when bundled | Found building 99 (R141): generated code reads the runtime as `R.name`, and `R` was `runtime.js`'s namespace, which rollup, bundling the page's `dist/scheme.js` or a program compiled ahead of time, writes as `Object.freeze({__proto__: null, ...})` -- an object V8 keeps in dictionary mode. Given a copy made by spreading instead (`runtime_object.js`), through the page bundle `nboyer` runs 1.92 times faster, `browse` 1.77, `deriv` 1.37, `earley` 1.23, `ctak` 1.09; `tak`, `fib`, `puzzle`, `fft` level. `runtime_separation_tests.js` checks the bundled copy with V8's `%HasFastProperties`. JavaScript under `src/`: the copy, one line in a module of its own, for `runtime.js`'s item. | R141 |
| 99 | ✅ | A program and its runtime as one file | Decided with the user: `node repl.js --build PROGRAM -o OUTPUT` writes one ES module -- the program's table bundled by rollup with the runtime and the primitives (`src/packaging/ahead_bundle.js`) -- which `node OUTPUT` runs and a page loads with `<script type="module">`; `runMain` reports a raise nobody handles as the CLI does. The runtime also carries JavaScript interop, classes, promises and the command line, a procedure JavaScript calls back running on the driver. Found on the way: rollup's namespace had made generated code slow on every page (101, R141); the tier compiled a procedure reading `this`, a method's receiver, as reading a global, and it failed -- such procedures are declined now, and refused ahead of time (103). Measured over six small programs that print, raise, make records, use a parameter, use JavaScript and compute (`benchmarks/run_ahead_startup.js`): about 1 MB, 160 KB gzipped, against the 8.4 MB, 920 KB gzipped, today's page loads before its Scheme runs; under Node 45-50 ms from spawn to exit, against `node repl.js`'s 300-345; on a page, finished 30-34 ms after its navigation began, against 204-334. JavaScript under `src/`: the bundling, rollup's; `runMain`, the runtime's start; the CLI's door; a build-host door that reads a program as the CLI does. | R141 |
| 100 | ✅ | A program compiled ahead of time that reads | A primitive that is a door into a library the library system's seed loads for itself is that library's procedure, compiled with the program: `%read`, which `read` calls, is the reader's `read-from-port` (`library-primitives` in `scripts/lib/ahead.scm`), whose port check moved into it from the JavaScript primitive. A program that reaches one has the library loaded and compiled as its imports are, and the runtime binds the name among the primitives once it has loaded; `(scheme read)` is unchanged, and nothing loads a second reader. Under Node the current ports are the standard ones, as the CLI makes them, so a program compiled ahead of time reads what is piped to it. `read1` and `dynamic`, which read data files, run at 0.86 and 0.97 of the tier's time, so every program of the canonical suite's default profile runs ahead of time; a program that reads carries the reader, about a megabyte more. | — |

## Decided, so not open

- **Small exact integers are prototyped as JavaScript numbers, integral inexacts boxed.** Decided
  2026-10-05 by the user, for 43, after its profile: an integral JavaScript number in the safe range
  is an exact integer, a non-integral one an inexact real, and an inexact real with an integral value
  -- 3.0, -0.0 -- a box; exact integers beyond the safe range stay `BigInt`. It keeps the earlier
  decision that an integral number from JavaScript arrives exact (47). Chosen over boxing every
  inexact, which would make every float operation allocate, and over keeping `BigInt`, which leaves
  fixnum loops at `BigInt`'s floor, 12x a loop on numbers. A prototype: built, then measured --
  fixnum gains against what float code with integral values (`fibfp`, `sumfp`), the corpus and the
  pages pay -- and kept or reverted. **Kept**, decided 2026-10-06 by the user once the flonum class
  was faster than before overall (0.79x), with `fibfp`, `fft`, start-up and the tier sets' few
  percent left as costs (88, 41).

- **A procedural macro's procedure runs where the macro is defined.** Decided 2026-10-05 by the user,
  for increment 4 of 45: an `er-macro-transformer`'s or `define-macro`'s procedure is evaluated in
  the environment of the library or program that defines the macro -- its imports, and what it
  defined before -- as Chibi, Gauche and Guile do: one instance, no phase separation, so a library
  can share helper procedures with its macros and nothing is loaded twice; the price is that
  expansion can see the library's state as it runs. Chosen over a fixed fresh `(scheme base)` for
  every transformer, which keeps phases apart but leaves a library no way to use its own helpers, and
  over R6RS's `(import (for lib expand))`, a second instance of each library, beyond R7RS.
- **The tier's first user is this implementation, and the order reaches that user first.** Decided
  2026-09-26. Its REPLs, CLI and build come first, then public benchmark numbers for an announcement;
  there are no outside users, so the representative programs are the implementation's own Scheme and
  the public benchmark suites. Until then the order kept the canonical suite's per-class numbers
  moving, which worked, but left the tier compiling no user code at all.
- **A refused capture is not acceptable in a shipped tier.** Refusing valid R7RS is a bug, so
  unwinding through nested interpreters (30) is a dependency of enabling the tier (34), not a
  follow-up to it -- and since the compiled standard library already makes the refusal reachable
  (R76), it is a regression to fix regardless. Done in 30; the one refusal left, beneath a
  redefined inlined primitive, is of a program redefining a primitive the compiled code inlined.
- **The Chrome extension is not a goal.** Stated by the user for the 2026-09-26 assessment. In the
  browser, compiled code is debugged through DevTools with source maps (39); interpreted code keeps
  the REPL debugger. Nothing here depends on the `debugger-take-3` branch, where the extension lives.
  Decided 2026-10-04 by the user: that branch is ignored. The ports -- the debugger's logic (67), the
  reader (63), the expander (45) -- start from this branch's code, though `debugger-take-3` rewrote
  much of the same, and are not shaped to keep its API.
- **Strict Content-Security-Policy degrades gracefully; it is not a design constraint.** Decided
  2026-09-26: a strict CSP is not a stated target. Such a page already works -- prebuilt tables
  install the compiled libraries without generating code, `loadCompiler` reports itself unavailable,
  user code runs interpreted, and a test enforces it by making `Function` throw. Keep that. But an
  option that needs `new Function`, such as materialising a resumable form on first capture, is
  allowed as long as the eager form remains the CSP fallback.
- **Calling convention B** — native JS stack for non-tail calls, trampoline for tail calls,
  cooperative unwind for capture. Settled by the Stage 2a bake-off; see
  [compiler_design.md](compiler_design.md).
- **The evaluator calls the system's Scheme directly, with compiled frames kept from moving.** Decided
  2026-10-01 by the user, between two designs (74): called that way, through `callSchemeProcedure`,
  the tier's Scheme runs compiled or in the compiler's own interpreter, out of the program's debugger.
  Applied through the program's interpreter instead, it would run there (R95), which a mode letting
  the debugger pause in the system's code needs; that mode is wanted, and comes later (62).
- **Calling Scheme from JavaScript is public interop, the compiler's included.** Decided 2026-10-01 by
  the user: the compiler's JavaScript calls Scheme through no internal procedure, and whatever it uses
  an ordinary developer can use too -- the plain call, a call that converts nothing for JavaScript
  holding Scheme values, and the conversions in both directions, all exported from the bundle and
  documented, the plain call being exactly the conversions around the call that converts nothing (72,
  75). The evaluator may keep internal procedures of its own, and needs none once that call is public.
- **What crosses the JavaScript boundary is converted as the call says, never by a setting.** Decided
  2026-10-01 in 81, which was given the choice between connecting `js-auto-convert` and removing it.
  JavaScript chooses by the entry it calls -- the plain call, or `callSchemeProcedure` with the public
  conversions around it (72) -- a JavaScript function's arguments are converted throughout however
  Scheme calls it, and Scheme converts a value itself with `(scheme-js js-conversion)`. A parameter
  would make one JavaScript call return different kinds of value as the Scheme beneath it changed,
  and cost every call to a JavaScript function a look-up. If a program needs to hand JavaScript a
  value unconverted -- a vector JavaScript is to change, an exact integer beyond 2^53 -- the way is a
  form written at the call, not dynamic state.
- **Every Scheme procedure is called the same way from JavaScript, whatever its tier.** Decided
  2026-09-30 by the user, between two designs (72): a compiled procedure's plain call faces
  JavaScript, as a closure's does, and compiled code calls it through its raw entry, which costs a
  direct tail call a property load, to be measured. Wrapping a compiled procedure only where it
  leaves Scheme is the fallback, since each exit it missed would be the bug again.
- **The interpreter is a permanent tier**, not a transitional one: the CSP-safe execution mode, the
  differential oracle, the maximum-fidelity debug tier, and the compiler's own bootstrap.
- **Compiled code with no interpreter beneath it is a goal, ranked low.** Decided 2026-09-30 by the
  user, as part of a possible optimization level that minimizes compiled code size, perhaps with tree
  shaking (77, 41). It leaves the interpreter a permanent tier: it is a build that leaves the
  interpreter out of a program that does not need it.
- **As much of the interpreter and compiler as can be is Scheme.** Decided 2026-09-29 by the user,
  widening the compiler rule below to the whole system: for dogfooding; because a compiler is a good
  benchmark of itself, and the slow parts of the compiler's Scheme should suggest what to optimize next;
  because a Scheme system should be able to host an effective, performant interpreter and compiler
  written in Scheme; and because the system should show Scheme at its best. A caller's or callee's
  language decides nothing, since Scheme calls JavaScript and JavaScript calls Scheme. What stays
  JavaScript is the core runtime -- for now the evaluator, and for good the value representations,
  `../src/compiler/runtime.js`, code generation and the save-and-resume protocol -- and the cores of
  libraries that need JavaScript features: host input and output, reflection for interop, JavaScript
  classes, the `Map` under hash tables, Unicode tables, `BigInt`. An audit of this branch that day
  found about a third of the JavaScript it added belonged in Scheme -- the rule below had been kept
  for the compiler's passes and broken for everything around them, in 34, 49 and 37 among others --
  about 8% goes when the expander is Scheme, and the rest is core runtime. The system's own Scheme is
  to be debuggable on request (62).
- **The primitives stay JavaScript; what is above them is Scheme.** Decided 2026-10-03 by the user,
  in place of 61, which was to move `string.js`'s procedures -- and those of `list.js`, `vector.js`,
  `char.js`, `bytevector.js`, `exception.js` and `control.js` above their cores -- into `(scheme core)`
  (R113). Over only `string-length`, `string-ref`, `string-set!` and `make-string`, compiled Scheme made
  `string-append` 66x slower, `substring` 22x and `string=?` 40x: a string keeps its JavaScript text
  until it is changed, which is what makes those operations cheap, and a character loop gives that up,
  as no code generation can win back. Over cores that do the work on the whole string, the Scheme
  would only check the arguments, at 1.0-2.4x today's cost. The procedures that take a procedure --
  `map`, `for-each`, `vector-map`, `string-map` -- are Scheme already, and the control primitives hand
  their calls to the interpreter as tail calls, so no primitive calls Scheme back. So a primitive stays
  JavaScript; a new procedure above them is Scheme (56); and one that would call a Scheme procedure back
  from JavaScript is Scheme. `hash_table.js` and the class and record interop get a review for logic
  that is not their store, not a port. **The numeric primitives too**, decided 2026-10-05 by the user
  in place of 65, which was to move the numeric tower's dispatch -- which representation each
  argument has, how each pair combines -- out of `math.js`, `rational.js` and `complex.js` into
  Scheme: they check their arguments and do their work with JavaScript's own number and `BigInt`
  operations, and 42's profile found the dispatch at most about 15% of the bignum programs, whose
  time had been one primitive's algorithm (R119), so the port would buy no speed and risk the fixnum
  and flonum classes.
- **The compiler moves to Scheme**, above `../src/compiler/runtime.js`. That file stays JavaScript
  because it needs native JavaScript features — a `Map` behind hash tables — that neither generated
  code nor Scheme libraries can express, not because generated JavaScript calls it.
- **The library system's Scheme starts from a small JavaScript seed loader.** Decided 2026-10-03 by
  the user (64), between three bootstraps: a seed loader of about 60-100 lines for exactly
  `(scheme core)` and the library system's own library, at the cost of reading and running the
  library system's source at every start until tables install without running source (69, 63); a
  compiled seed image installed without running source, which would build that mechanism first;
  or the library system written in the subset of Scheme that exists before `(scheme core)` loads,
  without `cond`, `case` or records, rejected as not idiomatic. The speed of the ported code itself
  matters little -- it runs once per library, and loading is mostly reading and running a
  library's source -- unlike porting primitives that run in inner loops (61).
- **A library's procedures wait ten calls; a program's keep their rule.** Decided 2026-10-02 by the
  user (80), on `run_tier.js --set all`, today's policy run twice as a control within 2%. For a
  program's own procedures no simple change won: waiting longer moved nothing beyond the noise but
  the test files, and compiling at definition only what loops cost the page programs 35-41% and
  `cpstak`, `quicksort` and `graphs` 2-4x -- a procedure called once making closures called often.
  A library's procedures, compiled at their first call after loading whatever the policy, made 21
  of the corpus's 22 test programs slower with the tier than without; waiting ten calls, however
  they loop, made them 28% faster, none more than 15% slower, where a hundred cost one program 31%.
  **Measured again 2026-10-06 (91)**, the shipped libraries restored as a page restores them, which
  `run_tier.js` had read from source at every run (R124), today's policy twice as a control within 1%
  on every set but the page programs, 5%: the same. A program's: waiting ten or a hundred calls within 1-5% of today on the
  canonical, test-file and page sets; only loops at definition 9-13% faster on the test files, 22-27%
  slower on the page programs, and `cpstak`, `quicksort` and `graphs` 2-3x slower. A library's, on the
  corpus, 2,881-2,905 ms today and 2,589 without the tier: at the first call +57%, the second +20%,
  the hundredth -11% but `edn` +19%.
- **Decisions about compiling, and ports, are measured on more than kernels.** Decided 2026-10-02
  by the user; the rule, *Decisions about compiling, and ports, are measured on more than kernels*, is at the head of this file.
- **The compiler imports SRFI 151 for liveness, whatever it costs its start.** Decided 2026-10-02 by
  the user (80): bit sets cut compiling by a tenth, and by two fifths for a large procedure, while
  loading the library costs every start of the compiler 6.5-11 ms. The alternatives were liveness
  calling the library's `%` primitives without importing it, which would have the compiler use what
  is internal to a library, and lists until starting a library is cheap. The start is 69's to bring
  down, with the rest of the compiler's.
- **New compiler code starts in Scheme.** Decided 2026-09-23, after a third increment in a row added
  JavaScript to the compiler under the "don't port a moving target" argument — liveness among them,
  written after the move to Scheme had been decided. Capabilities Scheme lacks are built as Scheme
  libraries over minimal JavaScript, not worked around by writing the compiler code in JavaScript.
  Regular expressions are deliberately **not** on that list: the compiler only needed text scanning
  because the emitter produces strings, and a Scheme emitter producing data removes the need.
