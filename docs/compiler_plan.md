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

| # | Task | Depends on | Why here | Evidence |
|---|---|---|---|---|
| 90 | **The rest of Chibi's sections, as Chibi wrote them** | — | From 89 (R122). Our copy of Chibi's R7RS tests leaves out, or rewords, 156 of Chibi's 1,198 tests; its number section, restored, had hidden four failures. Run as written, with Chibi's own comparison (`chibi_revised/test-equal.scm`), characters and strings fail seven: `char-alphabetic?` of `#\Λ`, `char-numeric?` of `#\๐`, and `digit-value` of Arabic-Indic and Gujarati digits, which R7RS 6.6 defines by Unicode; and `string-foldcase` of "Maß", "ſ" and "ΜΈΛΟΣ", which full case folding takes to "mass", "s" and "μέλοσ". Environments, input and output, and the two syntax sections need Chibi's `test-assert` and `test-error` forms before they run as written. So: each section replaced by Chibi's, as 6.2 was, a test Chibi's harness cannot express here kept as a skip with its reason, and each failure fixed or listed. The Unicode tables are JavaScript's (R7RS-small's character procedures, *Scheme first*). | R122 |
| 66 | **The printer, in Scheme** | — | `printer.js` and `io/printer.js`, about 550 lines: `write`, `display` and `write-shared`, cycle and sharing detection, number and character syntax. Nothing in it needs JavaScript but the port it writes to. Since 2026-10-02 one labelled writer serves `write`, `display`, `write-shared` and `write-simple`, differing in which objects get datum labels (R7RS 6.13.3), and `write_tests.scm` says what each writes for circular and shared structure; the port keeps both. Finding the cycles costs `write` 1.5-2.7x on large structures, the price of the Map a depth-first walk keeps: worth measuring again in Scheme. Measured on `run_tier.js`'s test-file and corpus sets too, not only the kernels (*Decisions about compiling* at the head of this file). | — |
| 76 | **The build steps and the compiler's harnesses, as Scheme programs** | One thin door into the compiler | `scripts/generate_compiled_libraries.js` and `scripts/generate_compiled_compiler.js` (whose table writer, `scripts/lib/table_writer.scm`, is Scheme since 69), and the benchmarks that drive the compiler -- `run_self_host.js`, `decline_reasons.js`, `run_macro.js` -- become Scheme programs that import `(scheme-js compiler)` and run from the CLI, and `index.js` keeps only what pages and the packaged compiler (`scheme_compiler.js`, `scheme_entry.js`) call. The lowest of the tasks from 2026-09-30: nothing is wrong, and what is left of the JavaScript is small. | — |
| 44 | **Finish the benchmark suite** | — | Three steps left from the validity review: replace `threads` with the real `threads10` (which also buys vector coverage), add the missing axes (interop, debugger-on, startup), and report a per-stage coverage metric so overfitting stays visible. The review that found the overfitting also listed the fix, and half of it is still undone. The interop axis first: it is the constraint that sets this implementation apart, and nothing measures it. And since 49 the canonical `compiler` program, left out only because `string-set!` threw, can be vendored: the largest program in the upstream suite, and a string-heavy one. | R23 |
| 88 | **What the new number representation still costs** | — | From 43. Integral inexacts are boxes, and where they cross calls or sit in data no expansion removes them (R121): `fibfp` is 1.81x the code before 43, its values all integral and every call taking and returning a box; `fft` 1.18x, its data a vector of `0.0`. The candidate for `fibfp` is a raw-double entry for a procedure whose parameters provably stay inexact, beside its ordinary one, as the loops on raw doubles have (`emit-double-loop!` in `emit.scm`), with the stack-room and capture protocol a recursive procedure needs and a loop does not, and with nothing inside it that boxes or calls (R120); for `fft`, nothing yet but measuring how common integral inexacts in data are outside the benchmarks. Also: `chudnovsky` 1.41x, unprofiled -- conversions at the edge of the safe range are the guess; ten mutual tail calls in `run_codegen.js` 105 ns against 92 with identical generated code, unexplained; and a math primitive without a direct path still pays the wrapper, which cost `(inexact x)` 33 ns a call against 4.6 before it had one. Profile first. Judged on the canonical suite, `run_codegen.js` and `run_tier.js --set all` (*Decisions about compiling* at the head of this file). | R120, R121 |
| 41 | **Smaller generated code** | — | Since 34 every page also fetches the compiler after it starts, `dist/scheme_compiler.js`, whose own prebuilt table makes it about 2.2 MB -- most of it the compiler's own Scheme compiled twice over, so the same measurement and the same remedy serve it. Since 21 every page carries every shipped library compiled, so the generated code's size is now what a page pays for: SRFI 1, 125, 128 and 152 were 1.1 MB of `dist/scheme.js`'s 2.67 MB, now 3.04 MB, about 6 KB a procedure. Every procedure is emitted twice, fast and resumable, and a procedure none of whose callees can capture -- one that calls nothing able to call back into Scheme, say -- can never be suspended, so its resumable form is dead weight. **Measure first** how much of each table that is. The alternative, loading a library's table only when the library is imported, needs an asynchronous import, which the interpreter's `import` is not. 22's cell reads then made the generated code about 8% larger, and 26's direct tail calls 4.5-6% more, most of it the direct call and its fallback written out at each of about 1,200 tail call sites. 27's room on the stack added 6-9.5% more (4-6.5% gzipped): a line at the entry of each procedure that calls, a store before each call. 28's test of each callee for being a procedure 5.5% more for the libraries, 8% for the compiler; 31's operands in order 0.9% and 3.8%. Written in the Scheme emitter. Also worth measuring: materialising the resumable form from source text on its first capture where `new Function` is allowed, keeping the eager form for strict CSP. For scale, BiwaScheme is about 250 KB and Gambit's browser REPL 11-22 MB (`compiler_assessment_2026-09-26.md`, §4.2 E); not before the tier reaches users unless page load is judged too slow. Since 80, with a library's procedures waiting ten calls, the corpus's test programs still run slower with the tier than without, 20 of 22, compiling about 29% of the time, so less emitted per procedure is the lever there. Judged on `run_tier.js --set all`, not on the canonical suite alone (*Decisions about compiling* at the head of this file). **Since 67 and 63** the debugger's and the reader's tables, about 0.5 MB and 0.6 MB, have taken `dist/scheme.js` from 5.1 MB to 6.7. **Since 45** the expander's adds 1.2 MB more to `compiled_libraries.js`, from 45 KB of Scheme; holding the forms a table restores as JSON rather than as code that builds them took 0.2 MB back, and a start what parsing them cost: `compiled_libraries.js` is 6.9 MB and `dist/scheme.js` 8.1 MB. **Since 43** the compiler's own prebuilt image is 4.07 MB, from 3.41, all of it the emitter of loops on raw doubles -- about 600 KB of generated code for some 25 procedures, `emit-double-loop!` alone 98 KB -- and a CLI start, which parses it, is 10-13 ms slower. | R65, R66 |
| 77 | **Compiled code without an interpreter beneath it** | Start the compiler without re-running its sources | Compiled code needs an interpreter beneath it to finish a continuation capture or a move of its frames to the heap (`UNWIND`), which is why a compiled thunk runs from the interpreter (`runCompiledThunk`) and 72's JavaScript entry lands deep recursion there; and the compiler starts by interpreting its own source (69). With the trampoline and the heap stack the runtime's own, a program that needs no `eval`, REPL or debugger could run with no interpreter at all. **Decided 2026-09-30** (*Decided*): a goal, ranked low, as part of a possible optimization level that minimizes what a page loads, beside 41's smaller generated code and tree shaking of library procedures a program never reaches (`../ROADMAP.md`). Judged on `run_tier.js --set all`, not on the canonical suite alone (*Decisions about compiling* at the head of this file). | — |
| 58 | **Compile the inner loops of a procedure the tier declines** | — | From 34. The tier compiles top-level procedures, each with everything nested in it, so a procedure it declines -- one that reaches `call/cc`, say (37) -- keeps its inner loops interpreted too, however hot. They need not: `tryCompileClosure` compiles a closure against its own environment, a named `let`'s procedure included, and the interpreter looks a local loop's name up in its frame on every iteration, so a compiled one bound there would be picked up at the next iteration, with no on-stack replacement. Untested on a call's frame, and needs the count on local closures too, which is a cost on every interpreted call that 34 kept to top-level ones. Worth it only if 37 leaves many procedures declined. Judged on `run_tier.js --set all`, not on the canonical suite alone (*Decisions about compiling* at the head of this file). | — |
| 37 | **The decline policy on real code: escapes, not exception handling** | — | `ir.scm`'s control globals put `guard`, `raise`, `with-exception-handler`, `parameterize`, `dynamic-wind` and `exit` beside `call/cc`, and `safety.js` declines every procedure that can reach one or that captures -- directly, through its unit, or through an interpreted closure. The plan assumed applications use the exception forms everywhere. **Measured on real code, 2026-09-27** (`decline_reasons.js --corpus`, over 7 SRFI reference implementations and 17 Snow-Fort packages recorded in `benchmarks/corpus/manifest.json`; [corpus_decline_results.md](corpus_decline_results.md)): of 1,855 procedures 78% compile, and **of the 406 declined for a control form, 399 end at `call/cc`** -- 305 of them only by reaching a capture through another procedure. `guard`, `with-exception-handler`, `parameterize` and `dynamic-wind` decline five. And nearly every capture is an **escape** -- `(call/cc (lambda (return) ... (return x) ...))`, often from a callback given to `for-each` or a search, or SRFI 146's pattern-matching macro -- with re-entry only in coroutine generators and Schelog (31 declines). The capture default was justified on `btsearch`, which re-enters; for escapes it is the slower choice: `benchmarks/run_escapes.js` has compiling the captures 1.5-3.9x faster than the default at every depth measured, the gap narrowing with the compiled frames a capture unwinds (R86). **So the order is now:** (a) change the default for captures and for the reachability rule by shape, or drop them. **Measured 2026-09-28** (`--captures` on `run_compiled.js` and `run_r7rs.js`, R90): compiling them is 21x faster on `quicksort`, 4x on `puzzle`, 3.8x on `maze`, 2.9x on `contfib`, 1.35x on `threads`, and slower on `btsearch` (4.5x), `fibc` (1.8x) and `ctak` (1.1-1.2x) -- escapes taken now and then against captures at every call or re-entered, which no static test tells apart. **Decided 2026-09-28: per procedure, as the program runs, and done.** Every procedure is compiled, those that capture included, over the closure the interpreter made of it (the tier, `compileProgram`, `compileEnvironment`, the prebuilt tables and the canonical harness all do); saves and resumes of each procedure's compiled frames are counted, and one whose frames are resumed at least four times as often as saved, after a thousand resumes, is switched back to its closure for good (`noteResume` in `unwind.js`, `switchBackToClosure` in `library_registry.js`). Counting captures or their rate could not have worked: `contfib`, a 2.9x win, captures faster than any loser; re-entry separates exactly -- `btsearch` resumes 200 times per save, every winner once. Result: `btsearch` back to the old rule's 69-72 ms while `quicksort`, `puzzle`, `maze`, `contfib` and `threads` keep their wins; `fibc` (1.8x) and `ctak` (1.2x) stay slower, since they capture at every call and resume each frame once. `declineCaptures` keeps the old rule; (b) next, in Scheme like the rest of the driver (50) -- its proof is an analysis of what `ir.scm` produces -- an escape fast path, **only where the compiler proves the continuation is not kept**. A continuation called while its capture is still on the stack could reach the capture with a JavaScript `throw` (a plain object, so no stack trace is built) caught there, saving no frames. But that alone is unsound, and so is a fall-back taken "when the receiver returns": the frames a continuation needs are those *below* the `call/cc`, still on the stack after an escape and discarded as they return, so `(call/cc (lambda (k) (set! saved k) (k 1)))` followed later by `(saved 2)` would find them gone. Whether `k` was kept cannot be told cheaply at run time -- it would mean watching every store into a variable, pair, vector, closure or JavaScript object -- so it is proved when compiling, and every capture not proved safe takes today's protocol unchanged; the unsafe case then cannot arise, rather than depending on each exit being caught. **Two scopes:** *local* -- the receiver's parameter appears only as the operator of calls, never stored, returned, captured by a closure or passed on -- covers the library escape pattern and is simple; *across procedures* -- `k` passed only to known procedures whose matching parameter is proved the same way, a fixed point over the program's globals -- is what `fibc` and `ctak` need, since both pass `k` on (to `addc`/`fibc`, to `ctak-aux`) without storing it, and a proof resting on a global has to be dropped when that global is redefined, switching its dependants back as the capture policy already does. **Recommended: the local scope now, the cross-procedure scope as a later step if `fibc` and `ctak` still matter** -- they are benchmarks built to stress `call/cc`, and the corpus shows real code escaping locally. What the throw must still do on the way out, as the full protocol does now: run the `dynamic-wind` after-thunks between call and capture in their own dynamic environment (handlers, `parameterize`), restore the state of any nested interpreter run it crosses, and leave the debugger's stack tracking consistent. **Testing**, each case checked against the interpreter's answer: every way `k` can be kept (variable, pair, vector, closure, returned, passed to a procedure the analysis cannot see, handed to JavaScript) must take the full protocol and re-enter correctly after its frames have returned; every way it is proved not kept must take the fast path, asserted by which path ran and not only by the answer; redefining a global a proof rests on must switch its dependants back and keep answers right; every way control leaves (the receiver returns, escapes through `k`, through another continuation, raises) crossed with what lies between (`dynamic-wind`, `parameterize`, handlers, a nested interpreter run, a JavaScript frame, the debugger on); and the fuzzer extended to generate programs that store continuations, escape, and re-enter them later, which it now rarely produces. Performance: `fibc`, `ctak`, `run_escapes.js` and the programs the capture policy wins on must hold or improve, and how fast V8 unwinds a throw through a deep compiled stack is measured before the design relies on it; (c) the exception forms last. Re-measured after 49 let the Chibi libraries load, which hold most of the corpus's `guard` and `parameterize`: 2,235 definitions, and still 399 of 407 control declines at `call/cc`, 3 at `guard` or `with-exception-handler`. The design for (c) stands: `guard`'s escape into its clauses and `exit` are one-shot and upward and can be (b)'s mechanism; a `with-exception-handler` handler runs in `raise`'s dynamic context before anything unwinds, `raise-continuable` returns to its raiser, and a `guard` with no matching clause re-raises in the original `raise`'s context -- so the handler stack and the wind list become runtime state compiled code can call through. Several are on the list for how they are implemented, not for what they do: `raise`'s primitive returns a node for the interpreter to run, and `guard` expands through `call/cc` in `control.scm`. Found in 28: a control global handed to a compiled procedure as a value -- `(map call/cc ...)`, `eval` -- still fails with JavaScript's "args is not iterable", since its pending call carries an expression for the interpreter. Must not regress `run_tier.js`'s corpus and page sets, beside the canonical suite and `run_codegen.js` (*Decisions about compiling* at the head of this file). **Found in 45:** a `parameterize` entered from compiled code costs about 100 microseconds, through `dynamic-wind`; one around each top-level expansion made expansion 1.8 times slower. **Moved down 2026-10-03, before (b) was built (R111):** the local scope covers 9 of the corpus's 77 captures, the library pattern being an escape from a callback (56), and captures are rare under the tier: outside `ctak` and `fibc`, which pass `k` on, at most 2,572 in a program, two compiled frames each; `puzzle`, the canonical program the local scope covers, would gain at most 10%. Should it come back, the scope is the callback's, and the cost to beat a few milliseconds a program. | R23, R49, R86, R111 |
| 38 | **Lowering failure as an escape, once compiled code can escape** | The decline policy on real code: escapes, not exception handling | The compiler's Scheme was written to avoid every declined form (`ir.scm`'s header says so), and the visible cost is lowering failure: `fail!` returns `#f`, and each of about twenty sites in `lower-node` checks for it before going on -- `(if (not test) #f (let ((then ...)) (if (not then) #f ...)))`. Once 37 makes `guard`'s escape compile, `fail!` escapes instead, from a handler established once per definition, and those sites become plain `let*`s. Nothing needs undoing on the way out, since a failed lowering discards its whole state. **Clearer, and a test, rather than faster:** removing a comparison per site saves little, but the self-host differential then exercises compiled exception handling on every run, and the compiler is the tier's most demanding user. Set up the handler per definition, never per node: the compiler also runs interpreted, where `guard` expands through `call/cc` and copies the frame stack. Measure with `npm run benchmark:self-host` in both tiers. At the same time correct that header, which still says the compiler avoids `apply` and `values` -- both compile now -- and look for anything that builds a list only to return two results. The rest of the compiler's Scheme stays as it is: it holds no resources for `dynamic-wind`, never backtracks, and passing its lowering state explicitly is faster than `parameterize` and good Scheme anyway. | — |
| 54 | **Code generation without evidence yet: arity specialization, unboxed fixnum paths, escape analysis** | — | Listed as the next code-generation targets until a profile of the compiled tier found none of them among its costs (R71); kept here, last, until one does. Profile first, then ceiling, then design. Since 2026-09-29 the ports to Scheme (50, 63-66, 45) are where that evidence should come from: each records where compiled Scheme loses to the JavaScript it replaced, with the profile. **First evidence, from 78:** the procedures that take an optional port, Scheme over JavaScript cores, lose 20-30 ns a call to the JavaScript they replaced -- `write-char` to the current port 18 to 40 ns, to a port passed 19 to 49 -- from the call itself and from the list a rest parameter is made into on every call, and `dynamic`, which calls `read` constantly, ran 5-13% slower: an optional argument taken without a list is arity specialization's case. Arity specialization has a measured cost to recover since 2026-10-02: every fast form tests `arguments.length` on entry, so that a procedure called with the wrong number of arguments signals an error in both tiers, which costs call-heavy compiled code 2-5% (fib 10 in `run_codegen.js --only recursion`, 1,970-2,000 against 2,070-2,115); a call whose callee and count the compiler knows, a self-call above all, could enter past the test. **Since 43** an exact integer in the safe range is a JavaScript number, so the unboxed fixnum path is the representation itself; its analogue left is raw doubles across calls (88). Must not regress `run_tier.js`'s corpus and page sets, beside the canonical suite and `run_codegen.js` (*Decisions about compiling* at the head of this file). **Evidence from 67:** the debugger's `should-pause?`, asked at every step of a program being debugged, spends its 0.12 µs on out-of-line calls -- a record accessor, `string?`, `real?` -- that inlined would be a field read and two `typeof` tests: record accessors and type predicates are not among the inline expansions. A program under the debugger with a breakpoint set runs 1.9x slower than with the JavaScript debugger it replaced (19.3 ms against 10.4 for `(fib 18)`). **And from 63:** the reader spends a fifth of its time in record accessors, every field read a call into `record.js`. | R71 |
| 53 | **Drop `source` from runtime `Cons`** | — | Unchanged, unmeasured, low priority. | — |
| 59 | **A string that keeps its identity through JavaScript** | — | From 49. A Scheme string crosses into JavaScript as its characters, so one sent through JavaScript and back comes back as another string with the same characters -- the rule that keeps every JavaScript API working, decided 2026-09-27 on no user experience, to be revisited with some. For code that needs the same string back -- one parked in a JavaScript structure, or handed through JavaScript to a Scheme callback -- an explicit form that passes the `SchemeString` object itself, which JavaScript sees as an object, and which returns as the same string. Nothing automatic can do it: a JavaScript string has no identity, and recovering the Scheme string from its characters would alias strings that only happen to be equal (`Interoperability.md`, *Strings at the boundary*). | R89 |
| 62 | **Debugging the system's own Scheme** | Source maps and debug points | Asked for 2026-09-29, for debugging the interpreter and compiler themselves: a mode in which the system's own compiled Scheme appears in stack traces and can be stepped into, as a program's does. Chosen at load time, so it costs nothing when off: the shipped libraries load compiled over the closures their bundled source makes, as a program's code has been since 34, and 33's machinery runs them as closures while debugging. To design: the evaluator calls the tier's Scheme directly (74), which is a nested run that cannot pause (R82), so in the mode those calls are applied through the program's interpreter instead, as it applies any procedure, which runs the compiler's code there (R95) -- and the rule that the debugger skips the system's code while the mode is off comes with it; the debugger's own Scheme (67) stays out of its own stepping even in this mode; the reader and expander (63, 45) start compiled, since something has to read their source, and switch to closures once loaded. Worth it only if debugging a program is no worse for it and start-up in the mode stays tolerable; measure both. Next to 39, where the debugging design is being settled anyway. | — |
| 84 | **Source maps for the shipped libraries, and DevTools formatters** | — | From 39, which maps the code the tier compiles as a program runs. The prebuilt tables are module code, bundled by rollup, so a frame in `map` or `vector-map` is named but placed in `dist/scheme.js`: the table writer would write a map for `compiled_libraries.js` and `compiled_compiler.js`, the bundled sources as its `sourcesContent`, and rollup chain it into the bundle's (`output.sourcemap`). And DevTools' custom formatters (`window.devtoolsFormatters`), so a pair shows as a list, a symbol as its name and a record by its fields, which DevTools uses only when its user turns them on. Neither is needed to debug a program's own code. | — |
| 68 | **The evaluator, in Scheme** | Compiled Scheme fast enough for the evaluator's own loop | The step loop, the frames, the syntax tree's `step` methods and the environments (`interpreter.js`, `frames.js`, `ast_nodes.js`, `environment.js`, `context.js`, about 3,300 lines): the hottest code in the system, and the most tied to the debugger. It could be Scheme compiled ahead of time from the seed, like the reader and expander, but only once the ports before it show compiled Scheme close enough to hand-written JavaScript on hot code -- until then they are the measurement. The interpreter stays a permanent tier either way: this changes what it is written in, not whether it exists. JavaScript regardless: the value representations, `runtime.js`, code generation, the save-and-resume protocol and the host interfaces. Measured on `run_tier.js`'s test-file and corpus sets too, not only the kernels (*Decisions about compiling* at the head of this file). | — |
| 32 | **CI for `main`, when this branch merges, browser included** | The merge into `main` | `ci.yml` runs only on pushes and pull requests to `main`, runs the old numeric-tower benchmark, which measures nothing the compiler changes, and never loads the browser test page -- every browser test count in these documents was run by hand. **Decided 2026-09-27: no CI for this branch**, whose tests are run by hand as it is developed. When it merges, extend `main`'s CI to what the compiler changes: `npm test` -- the conformance suites in both configurations and the fuzzer's fixed seeds are inside it -- and the browser tests, headless; the Puppeteer harness on `debugger-take-3` is the shortest path to the second. Replace the numeric-tower benchmark step. | — |

## Completed

The fifteen most recent. Every completed task, these included, is in [compiler_plan_completed.md](compiler_plan_completed.md) under the same number, and anything older is there only. Detail in `../CHANGES.md`; what each one *falsified* in `compiler_findings.md`.

| # | | Task | Outcome | Evidence |
|---|---|---|---|---|
| 39 | ✅ | Source maps and debug points | Compiled frames are named for their Scheme procedures, in a stack trace and a profile, each generated function made as a property keyed by its procedure's name, and code compiled as a program runs is placed by a `scheme:///<file>/<procedure>` URL (step 1). That code carries a source map placing each line at the Scheme expression whose call it holds: the analyzer's spans reach the compiler through `marshal.js`, an exception decided with the user, ride the `call` IR node, and are noted against the statements the emitter makes, and a procedure renders as items whose lines know their spans (step 2). A page's scripts are read under names, an inline one's text kept and written into its maps (step 3). Checked on a page through the DevTools protocol. Compiling 0-4% dearer for step 1 and 2.7-6.5% more for step 2 over `run_tier.js`'s sets, running unchanged. The shipped libraries' maps and DevTools formatters are 84. JavaScript: six lines in `marshal.js`, the exception, and 105 for host input, the page's scripts and their text; Scheme about 550 lines added. | R110 |
| 55 | ✅ | Import filters for macros and syntax keywords | Half gone when probed (R114): `prefix` and `rename` apply to macros since 64, and `(rapid match)` and `(rapid syntax)` load. Fixed: a top-level definition -- in a program, a library's body or a top-level `begin` -- or an import of a name that was a macro's now makes the name that variable (`shadowMacro` in `syntax_object.js`), as an internal definition already did, so a program or library defining its own `when` or `assert` calls its own; a form analyzed with no environment is marked as the top level, and a lambda's body gets an environment of its own. That `only` and `except` hide nothing, procedures included, is 85. Tested in `definition_shadowing_tests.scm`. JavaScript: the analyzer's resolution fixed in place, 76 lines added, mostly comments. | R114 |
| 56 | ✅ | The rest of R7RS-small's libraries and identifiers | `rationalize` and `read-bytevector!` in Scheme; the binary file ports, host I/O; `(scheme load)` and `(scheme r5rs)` as libraries, the latter's environments over `environment`, which now imports its sets into a new environment through the library system, `eval` analyzing under its scope. The audit probes `(scheme r5rs)` under a prefix and checks `environment`'s import sets: nothing missing. Found on the way and fixed in `math.js`: `exact` of a non-integral flonum raised, now the dyadic rational it is, and negation made an exact rational inexact and lost a flonum zero's sign. JavaScript 157 lines added, 18 removed; Scheme 187 added. | R3, R87 |
| 57 | ✅ | Dot notation against R7RS identifiers | Decided with the user: on for programs, pages and the REPLs, off in the files of a library the library system loads, and a file says which with `#!dot-notation` or `#!no-dot-notation` (the reader's `dotAccess`, as `#!fold-case`). The benchmark harness reads the canonical programs with it off: `matrix` and `slatex` run, `gcbench` runs `'slow'`, and so does `equal`, since `equal?` terminates on circular structure; only `read0` is blocked. SRFI 135 gets past the reader and fails further on (86). JavaScript: the reader fixed in place, 39 lines added and 12 removed. | R87 |
| 48 | ✅ | The two conformance tests only passing by being rescued | One left rescued, and the test's fault: the revised copy had changed Chibi's `(test 1.0 (inexact 1))` to expect an exact 1. Restored, and the rescue removed from `compliance_suite.js`: 994 of 994 applicable Chibi tests and 220 of 220 chapter tests pass as Scheme counts them, in both library configurations. | R85 |
| 86 | ✅ | SRFI 135 does not load | Not the compiler (R115): invoking a continuation inside a nested run of the interpreter threw to the outermost run, sentinels dropped, so an escape captured in a run JavaScript started -- the library system's `guard`, answering a `cond-expand` that SRFI 135's body asks while it loads -- abandoned the JavaScript and ended the outer `load-library` with its answer. A jump to a continuation captured in the current run now stays in it (`frames.js`). SRFI 135 loads, all 82 exports. Tested in `scheme_call_tests.js`. JavaScript: 25 lines, the evaluator fixed in place. | R115 |
| 85 | ✅ | Imports define what a program or library sees | Decided with the user: a library, an `environment`, and a program -- CLI file, `-e` code or page script -- that begins with `import` declarations see what they import and nothing else; a program with none and the REPLs see everything, as before. Environments of import sets have no parent, a macro is found by name only where imported, `(scheme primitives)` exports every primitive, and a program's own environment is the tier's to compile. Measured before switching on: every corpus test and page program begins with `import`, and all ran right, compiled as much as before. Found eleven names the standard libraries never exported and three keywords never implemented (R116, 87); compiled `call-with-values` reads `apply` from the runtime. A name bound nowhere still falls back to JavaScript's globals. Compared with Gambit 4.9.5 (strict libraries, lenient programs) and Racket's `#lang r7rs` (strict both). JavaScript: the door from the CLI and pages into the library system (`programEnvironment`, `runProgramForm`), the compiler host's `environment-strict?`, and environments made parentless. | R114, R116 |
| 87 | ✅ | `syntax-error`, and `include` and `include-ci` as forms | All three are procedural macros in Scheme (`macros.scm`), since a transformer runs as the form is expanded and the analyzer, which is to become Scheme, is not extended: `syntax-error` raises a syntax error with its message and irritants, which now reaches whoever analyzed the use as raised, rather than once more per enclosing macro; `include` and `include-ci` read their files through the file resolver -- R7RS only encourages looking beside the including file, which a transformer cannot know. A core form with the wrong number of operands, or a `let` binding with no expression, is a syntax error naming its keyword, where it reached a JavaScript `TypeError` or had operands ignored. The R7RS audit finds nothing missing. JavaScript: two primitives (`%raise-syntax-error`, `%include-source`, host input) and the analyzer's checks, fixed in place. | R116 |
| 67 | ✅ | The debugger's logic, in Scheme | `(scheme-js debugger)` (`debugger.scm`) holds breakpoints and which one a location hits, the calls a program is in, the run's mode and whether a step stops, whether an exception breaks, where a breakpoint cannot fire, `:locals`' bindings, the REPL's commands and the pause message; it runs from its prebuilt table on the library system's own interpreter, so it is never debugged itself, loaded the first time a runtime is used. `BreakpointManager`, `StackTracer`, `PauseController`, `StateInspector` and `DebugExceptionHandler` are gone, and `SchemeDebugRuntime` and `ReplDebugCommands` are doors: hooks calling the Scheme (the per-step and per-call ones through their raw entries, thirty times cheaper than running on the interpreter), the paused run's promise, the backends, `:eval`. Dropped the DevTools-protocol formatting, which nothing on this branch reads. A frame is recorded only while debugging is on. Found `:eval` in a paused frame never answering an expression of more than one step (R117); it now runs to its end at no breakpoint. Debugging costs 1.1x the JavaScript with nothing to stop at, 1.9x with a breakpoint set (evidence on 54). | R117 |
| 40 | ✅ | Debug an optimized procedure by not optimizing it, per procedure | While a program is debugged, only the closures run compiled that hold a breakpoint run as themselves, and every one while a step is in progress or the program is paused (`debugger-interpretation` in `debugger.scm`, a choice `interpret-compiled-over!` takes). The callers the task named are not switched: a breakpoint reached in a run compiled code called moves the compiled frames beneath it to the heap, as a capture there would, and the step is taken again and paused at by the run that can wait (`beginStepAgain` in `unwind.js`), so higher-order callers such as `map`, which no call graph could name, stay compiled too. A program using the compiled library, debugged with a breakpoint it does not reach, runs in 154 ms where switching the whole program took 1,556. JavaScript: the move and the step taken again, the save-and-resume protocol. | R54 |
| 63 | ✅ | The reader, in Scheme | `(scheme-js reader)` reads every text and port in the system: `parse` and `read` are doors into it, and the REPLs' completeness, delimiting parentheses and matching ask it; the JavaScript tokenizer, parser and port scanner are gone, about 1,600 lines, with `number_parser.js` kept as `string->number`'s core. Every prebuilt table holds its library's `define-library` form, so a library with a current table is loaded without its files being read, and the seed reads its own stale libraries with the pinned reader (`npm run pin:reader`), which a bundle leaves out. Whole-text scans and line starts made it 1.8x the JavaScript reader, the rest record accessors (evidence on 54); the test suite takes 70 s against 64, a CLI start 0.25-0.27 s against 0.23. A form feed, which the JavaScript reader read as 0, is whitespace; `read` is R7RS's, dot notation off. | — |
| 45 | ✅ | Hygienic procedural macros and phase separation, in a new expander written in Scheme | In four increments decided with the user. The expander is Scheme, `(scheme-js expander)`, and the only one: the JavaScript analyzer, `syntax_rules.js` and `marshal.js` deleted once the two agreed on 2,021,312 forms; core forms assembled into nodes and kept for the compiler; tables hold them as JSON, so the seed restores the expander with no form expanded, or from the pinned seed. A library's macro means the library's bindings wherever it is used (R69), compiled as globals of their own; `er-macro-transformer`, with `define-macro` on it as a legacy form; a procedural macro's procedure runs where the macro is defined (decided with the user); the special forms are keywords a strict scope has only if it imports them (`(scheme-js special-forms)`). Chibi's three disabled 4.3 tests pass. Start-up, self-host and `run_tier.js` level. JavaScript under `src/`: 908 lines added, 3,060 removed. | R69, R52 |
| 42 | ✅ | Profile bignums | The bignum class's 53x behind Gambit's interpreter was neither BigInt arithmetic nor the tower's dispatch, as believed (R119): `pi` and `chudnovsky` spent 98% of their time in `exact-integer-sqrt`, whose Newton iteration started from the integer itself. Rewritten to double its precision each step (Dickinson's, Python's `math.isqrt`): `pi` 560 ms to 2.0 ms compiled, `chudnovsky` 10 ms to 0.15 ms, ahead of Racket CS and Gambit compiled to C; the class 1.2x to 7x over the interpreter. What remains is BigInt division and multiplication; dispatch is at most 15%. JavaScript: the primitive, fixed in place. | R119 |
| 43 | ✅ | Fixnums as JS numbers | Decided with the user, prototyped and kept: an exact integer is a JavaScript number in the safe range and a `BigInt` beyond it, an inexact real a number unless its value is an integer, when it is a `Flonum` box (`number_representation.js`); compiled arithmetic is inline on two numbers and the runtime's otherwise, and a loop whose variables stay inexact runs on raw doubles first. Against the code before it, compiled: fixnum 0.59x, vector 0.61x, flonum 0.79x, call 0.81x, list 0.82x, bignum 1.22x, all 44 programs 0.81x; but `fibfp` 1.81x and `fft` 1.18x, whose integral inexacts cross calls and sit in vectors as boxes (88), `run_tier.js --set all` 1-5% slower, and a CLI start 10-13 ms slower, the compiler's image 19% larger (41). A box made in a branch of a hot loop that never ran made the loop seven times slower (R120). JavaScript: the value representation, the primitives on it and the compiler's runtime, 1,089 lines added and 130 removed. | R120, R121 |
| 89 | ✅ | Complex arguments to the elementary functions | `exp`, `log`, `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan` and `expt` take complex arguments, by R7RS 6.2.6's formulas on the parts as doubles, the branch cuts as R7RS fixes them; an exact-integer power of a complex base is a product, exact for an exact base, and zero's powers follow R7RS. A complex number is real only with an exact zero imaginary part, so `(real? -2.5+0.0i)` is #f and orderings refuse it; `finite?` and `infinite?` of exact parts, and `numerator` and `denominator` of inexact numbers, fixed; an exact zero real part is not written, `+i`. Chibi's whole number section restored, its inexact values compared as Chibi compares them: 211 tests where the copy had 99, the suite 1,108 of 1,108 in both configurations. JavaScript: the numeric primitives, 238 lines added and 26 removed. | R122 |

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
