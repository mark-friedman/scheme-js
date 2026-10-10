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
decide its cost, in both tiers. Before designing, a ceiling (R61) and a profile (R71). Before and
after are two copies of the repository side by side, alike but for the change, never a copy against
the repository itself, which alone moved `fibc` by a fifth (R142).

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

**2026-10-08, after 102 and 103:** both done; 104, `bv2string` on a page, which 102 found, takes 102's place.

**2026-10-08, after 104:** 104 done; 105, exact division in generated code, takes its place.

**2026-10-08, after 105:** 105 done; 58 measured and closed. **Decided with the user:** 37(c), compiled handlers, winds and `parameterize`, next -- its payoff now mostly programs compiled ahead of time, which refuse them.

**2026-10-08, after 106:** 106, 37(c1), done; 37(c2) is next. 107, found building 106, is added after 37: it breaks a user's build only when their program sits beside a file named like a system library's include, so it does not jump the queue.

**2026-10-08, after 108:** 108, 37(c2) and (c3) together, done, and 37 closed with it: its (b) was measured not worth doing. 107 is next.

**2026-10-08, after 38:** 107 was taken up in a session of its own; 38 is done. 88 is next.

**2026-10-08, after 53:** 88 and 54 wait, by their own rows, for code outside the benchmarks that needs them; 53 measured and closed. 59 waits for experience of strings crossing into JavaScript; 62 and 84 are next, and are the user's to choose between.

**2026-10-08, after 107:** 107 done; its outcome is under *Completed*. 62 and 84 are still next, the user's to choose between.

**2026-10-08, decided with the user:** 84 widened to debugging Scheme and JavaScript together in DevTools -- stepping between them past the system's code, values and names shown as each language's, and tested through the DevTools protocol; the libraries' maps and the formatters are now parts of it.

**2026-10-09, decided with the user:** 84's names are both readable generated names and source map scopes; stage (f) says why.

**2026-10-09, decided with the user:** a user's macro is placed at its use, as the system's are; stepping into templates moves to the scopes work, (f). (c) is done.

**2026-10-09, later:** 84, being worked on, is first in the table, and its stages, decisions and progress are under *84, in progress*, below the table: they had grown into a row of 9,700 characters that could not be read.

**2026-10-09, after 84:** 84 done, its outcome under *Completed* and its stages in `CHANGES.md`. Its source map scopes are made only while the tier compiles for DevTools, since DevTools' Scope pane does not read them yet (R146), and stepping into a macro's template did not come with them (R147). 88, 54 and 59 still wait, by their own rows, for evidence; 62 is next.

**2026-10-10, decided with the user:** 62 moves down, after 41: since 84, DevTools steps into the system's Scheme once its user takes it off the ignore list, the prebuilt tables being mapped, which leaves 62 only the REPL's debugger, for a subset of users. 88, 54 and 59 are to be considered next.

**2026-10-10, considered, and decided with the user:** of 88, 54 and 59, only 54 has its evidence. 88 still waits for code outside the benchmarks doing heavy integral inexact arithmetic, and 59 for experience of strings crossing into JavaScript. A profile of the compiled reader found its time in the calls the row names -- `memv` on characters 9.7%, record accessors 6.2%, an optional port's rest list and its sorting out 5.5%, `char=?` 4% -- so 54 goes first, in four items: characters and type predicates inline; record accessors and predicates; optional arguments without a rest list; a known callee entered past the arity test. Each item is measured on the canonical suite, `run_codegen.js` and `run_tier.js --set all`, two copies side by side.

| # | Task | Depends on | Why here | Evidence |
|---|---|---|---|---|
| 54 | **Code generation with evidence: inline type tests and characters, record accessors, optional arguments, known callees** -- in progress | — | Being worked on since 2026-10-10, decided with the user, in the order and stages under *54, in progress*, below the table. Listed as the next code-generation targets until a profile of the compiled tier found none of them among its costs (R71); kept here, last, until one does. Profile first, then ceiling, then design. Since 2026-09-29 the ports to Scheme (50, 63-66, 45) are where that evidence should come from: each records where compiled Scheme loses to the JavaScript it replaced, with the profile. **First evidence, from 78:** the procedures that take an optional port, Scheme over JavaScript cores, lose 20-30 ns a call to the JavaScript they replaced -- `write-char` to the current port 18 to 40 ns, to a port passed 19 to 49 -- from the call itself and from the list a rest parameter is made into on every call, and `dynamic`, which calls `read` constantly, ran 5-13% slower: an optional argument taken without a list is arity specialization's case. Arity specialization has a measured cost to recover since 2026-10-02: every fast form tests `arguments.length` on entry, so that a procedure called with the wrong number of arguments signals an error in both tiers, which costs call-heavy compiled code 2-5% (fib 10 in `run_codegen.js --only recursion`, 1,970-2,000 against 2,070-2,115); a call whose callee and count the compiler knows, a self-call above all, could enter past the test. **Since 43** an exact integer in the safe range is a JavaScript number, so the unboxed fixnum path is the representation itself; its analogue left is raw doubles across calls (88). Must not regress `run_tier.js`'s corpus and page sets, beside the canonical suite and `run_codegen.js` (*Decisions about compiling* at the head of this file). **Evidence from 67:** the debugger's `should-pause?`, asked at every step of a program being debugged, spends its 0.12 µs on out-of-line calls -- a record accessor, `string?`, `real?` -- that inlined would be a field read and two `typeof` tests: record accessors and type predicates are not among the inline expansions. A program under the debugger with a breakpoint set runs 1.9x slower than with the JavaScript debugger it replaced (19.3 ms against 10.4 for `(fib 18)`). **And from 63:** the reader spends a fifth of its time in record accessors, every field read a call into `record.js`. **And from 66:** the printer asks of every value it writes what it is, and `symbol?`, `string?`, `number?` and `vector?` are calls, as are `char->integer`, `string-ref` and `char=?`, the last two taking a rest list as `char-ci=?` does; written so, the printer wrote symbols 15 times as slowly as the JavaScript it replaced. Comparing characters by code, which compiled code does inline, and handing a scan of a whole name or string to one primitive (`%string-find-any`), took that to twice; an internal `define` in a procedure called for every symbol made a closure on each call. | R71 |
| 88 | **What the new number representation still costs** | — | From 43. Integral inexacts are boxes, and where they cross calls or sit in data no expansion removes them (R121): `fibfp` is 1.81x the code before 43, its values all integral and every call taking and returning a box; `fft` 1.18x, its data a vector of `0.0`. The candidate for `fibfp` is a raw-double entry for a procedure whose parameters provably stay inexact, beside its ordinary one, as the loops on raw doubles have (`emit-double-loop!` in `emit.scm`), with the stack-room and capture protocol a recursive procedure needs and a loop does not, and with nothing inside it that boxes or calls (R120); for `fft`, nothing yet but measuring how common integral inexacts in data are outside the benchmarks. Also: `chudnovsky` 1.41x, unprofiled -- conversions at the edge of the safe range are the guess; ten mutual tail calls in `run_codegen.js` 105 ns against 92 with identical generated code, unexplained; and a math primitive without a direct path still pays the wrapper, which cost `(inexact x)` 33 ns a call against 4.6 before it had one. **Measured 2026-10-06, and moved down beside 54:** what it would remove falls on kernels. Profiled, `fibfp` (2.9 ms against 1.5 before 43) spends its time in its own code on the boxed representation -- 365,000 inexact results a run, only 89 of them new boxes, the rest the shared small ones -- and `chudnovsky` (200 us against 148) spreads its time over bignum work, conversions at the edge of the safe range about 13% of it. Counted over `run_tier.js`'s sets, one run each, the boxes a program makes and the inexact results it computes: the 74 test files 2,028 and 150,427, the 23 corpus programs 7,513 and 272,319, the page programs none and 4,060, the 44 canonical programs 249,105 and 1,591,435, most of it `quicksort`, `fft`, `fibfp` and `mbrotZ`. A raw-double entry would be an optimization fitted to the kernels, by those counts; it waits, as 54's do, for code outside the benchmarks that does heavy integral inexact arithmetic. Judged on the canonical suite, `run_codegen.js` and `run_tier.js --set all` (*Decisions about compiling* at the head of this file). | R120, R121 |
| 59 | **A string that keeps its identity through JavaScript** | — | From 49. A Scheme string crosses into JavaScript as its characters, so one sent through JavaScript and back comes back as another string with the same characters -- the rule that keeps every JavaScript API working, decided 2026-09-27 on no user experience, to be revisited with some. For code that needs the same string back -- one parked in a JavaScript structure, or handed through JavaScript to a Scheme callback -- an explicit form that passes the `SchemeString` object itself, which JavaScript sees as an object, and which returns as the same string. Nothing automatic can do it: a JavaScript string has no identity, and recovering the Scheme string from its characters would alias strings that only happen to be equal (`Interoperability.md`, *Strings at the boundary*). | R89 |
| 68 | **The evaluator, in Scheme** | Compiled Scheme fast enough for the evaluator's own loop | The step loop, the frames, the syntax tree's `step` methods and the environments (`interpreter.js`, `frames.js`, `ast_nodes.js`, `environment.js`, `context.js`, about 3,300 lines): the hottest code in the system, and the most tied to the debugger. It could be Scheme compiled ahead of time from the seed, like the reader and expander, but only once the ports before it show compiled Scheme close enough to hand-written JavaScript on hot code -- until then they are the measurement. The interpreter stays a permanent tier either way: this changes what it is written in, not whether it exists. JavaScript regardless: the value representations, `runtime.js`, code generation, the save-and-resume protocol and the host interfaces. Measured on `run_tier.js`'s test-file and corpus sets too, not only the kernels (*Decisions about compiling* at the head of this file). | — |
| 41 | **Smaller generated code** | — | **Decided with the user 2026-10-07: not urgent.** Page load and size are acceptable for now. Eventually, several runtime files for a page to choose among -- with and without the compiler, with and without the resumable twins -- where the runtime's size or a program's matters more. **Measured 2026-10-06 (R132):** the resumable twins are 47% of the libraries' generated code and 49% of the compiler's, but those that can never run are 1.9% and 1.3%; twins kept as text, made functions when first needed, save 9 ms of the libraries' 63 ms import and nothing of the download, 0.94 MB gzipped (the compiler 0.49 MB). The large lever left is not shipping twins, making one on a procedure's first suspension with the compiler a page fetches anyway -- about half the generated code -- which waits on the user's judgement of page load: a page runs its first script 196 ms after its navigation starts (`run_startup.js`). Since 34 every page also fetches the compiler after it starts, `dist/scheme_compiler.js`, whose own prebuilt table makes it about 2.2 MB -- most of it the compiler's own Scheme compiled twice over, so the same measurement and the same remedy serve it. Since 21 every page carries every shipped library compiled, so the generated code's size is now what a page pays for: SRFI 1, 125, 128 and 152 were 1.1 MB of `dist/scheme.js`'s 2.67 MB, now 3.04 MB, about 6 KB a procedure. Every procedure is emitted twice, fast and resumable, and a procedure none of whose callees can capture -- one that calls nothing able to call back into Scheme, say -- can never be suspended, so its resumable form is dead weight. **Measure first** how much of each table that is. The alternative, loading a library's table only when the library is imported, needs an asynchronous import, which the interpreter's `import` is not. 22's cell reads then made the generated code about 8% larger, and 26's direct tail calls 4.5-6% more, most of it the direct call and its fallback written out at each of about 1,200 tail call sites. 27's room on the stack added 6-9.5% more (4-6.5% gzipped): a line at the entry of each procedure that calls, a store before each call. 28's test of each callee for being a procedure 5.5% more for the libraries, 8% for the compiler; 31's operands in order 0.9% and 3.8%. Written in the Scheme emitter. Also worth measuring: materialising the resumable form from source text on its first capture where `new Function` is allowed, keeping the eager form for strict CSP. For scale, BiwaScheme is about 250 KB and Gambit's browser REPL 11-22 MB (`compiler_assessment_2026-09-26.md`, §4.2 E); not before the tier reaches users unless page load is judged too slow. Since 80, with a library's procedures waiting ten calls, the corpus's test programs still run slower with the tier than without, 20 or 21 of 23 measured as a page loads its libraries (91), compiling about 29% of the time, so less emitted per procedure is the lever there. Judged on `run_tier.js --set all`, not on the canonical suite alone (*Decisions about compiling* at the head of this file). **Since 67 and 63** the debugger's and the reader's tables, about 0.5 MB and 0.6 MB, have taken `dist/scheme.js` from 5.1 MB to 6.7. **Since 45** the expander's adds 1.2 MB more to `compiled_libraries.js`, from 45 KB of Scheme; holding the forms a table restores as JSON rather than as code that builds them took 0.2 MB back, and a start what parsing them cost: `compiled_libraries.js` is 6.9 MB and `dist/scheme.js` 8.1 MB. **Since 43** the compiler's own prebuilt image is 4.07 MB, from 3.41, all of it the emitter of loops on raw doubles -- about 600 KB of generated code for some 25 procedures, `emit-double-loop!` alone 98 KB -- and a CLI start, which parses it, is 10-13 ms slower. **Since 66** `(scheme core)`'s table holds the printer's 21 procedures, 753 KB to 1,030 KB, `print-compound` alone 70 KB, and a CLI start is 3 ms slower. | R65, R66 |
| 62 | **Debugging the system's own Scheme** | Source maps and debug points | Asked for 2026-09-29, for debugging the interpreter and compiler themselves: a mode in which the system's own compiled Scheme appears in stack traces and can be stepped into, as a program's does. Chosen at load time, so it costs nothing when off: the shipped libraries load compiled over the closures their bundled source makes, as a program's code has been since 34, and 33's machinery runs them as closures while debugging. To design: the evaluator calls the tier's Scheme directly (74), which is a nested run that cannot pause (R82), so in the mode those calls are applied through the program's interpreter instead, as it applies any procedure, which runs the compiler's code there (R95) -- and the rule that the debugger skips the system's code while the mode is off comes with it; the debugger's own Scheme (67) stays out of its own stepping even in this mode; the reader and expander (63, 45) start compiled, since something has to read their source, and switch to closures once loaded. Worth it only if debugging a program is no worse for it and start-up in the mode stays tolerable; measure both. Next to 39, where the debugging design is being settled anyway. | — |
| 32 | **CI for `main`, when this branch merges, browser included** | The merge into `main` | `ci.yml` runs only on pushes and pull requests to `main`, runs the old numeric-tower benchmark, which measures nothing the compiler changes, and never loads the browser test page -- every browser test count in these documents was run by hand. **Decided 2026-09-27: no CI for this branch**, whose tests are run by hand as it is developed. When it merges, extend `main`'s CI to what the compiler changes: `npm test` -- the conformance suites in both configurations and the fuzzer's fixed seeds are inside it -- and the browser tests, headless. Puppeteer is a dev dependency since 2026-10-10, and the DevTools tests (`tests/devtools/`) already launch Chrome with it, so loading the browser test page so is the shortest path to the second. Replace the numeric-tower benchmark step. | — |

### 54, in progress

**Items**, in order, each done, being done, or to do:

- **(a) Characters, and what a value is, inline -- done 2026-10-10.** A character is one object a code
  point, so `eqv?` against a character constant -- `case` on characters -- is `===`, and `char=?`,
  `char<?`, `char>?`, `char<=?` and `char>=?` of two or three characters compare identity or code
  points; `char->integer` reads the code point; `char?`, `symbol?`, `string?`, `vector?`, `boolean?`
  and `procedure?` are the type tests their primitives make (`character-expansions` in `inline.scm`;
  a name may have an entry for each of several arities). The reader's character tests are `case` now.
  Found on the way: the primitives ordered characters in UTF-16, so a character beyond U+FFFF came
  before one from U+E000 to U+FFFF; they compare code points now. `procedure?` checked for two classes
  nothing makes. Measured: the reader reads the compiler's sources in 0.69 of the time; canonical
  `read1` 0.62, `parsing` 0.72, `dynamic` 0.75, nothing else beyond noise; `run_codegen.js`'s new
  `characters` group 65.6 to 13.4 ns for a letter test, a `case` on eight characters 107 to 6;
  `run_tier.js` corpus 3,133 to 3,019 ms, page 184 to 177.
- **(b) Record accessors and predicates inline -- to do.** Compiled code calls an accessor through the
  generic path, and an integer field's read looks the record up in the table of inexacts stored as
  numbers (`storedToScheme` in `js_interop.js`), as each store of one does. The compiler can know from
  the environment that a global holds an accessor, and generated code check the callee's field and
  type as it runs, which a prebuilt table can hold.
- **(c) Optional arguments without a rest list -- to do.** `read-char`, `peek-char`, `write-char`.
- **(d) A known callee entered past the arity test -- to do.** The test costs call-heavy code 2-5%.

## Completed

The fifteen most recent. Every completed task, these included, is in [compiler_plan_completed.md](compiler_plan_completed.md) under the same number, and anything older is there only. Detail in `../CHANGES.md`; what each one *falsified* in `compiler_findings.md`.

| # | | Task | Outcome | Evidence |
|---|---|---|---|---|
| 101 | ✅ | Generated code reads the runtime fast when bundled | Found building 99 (R141): generated code reads the runtime as `R.name`, and `R` was `runtime.js`'s namespace, which rollup, bundling the page's `dist/scheme.js` or a program compiled ahead of time, writes as `Object.freeze({__proto__: null, ...})` -- an object V8 keeps in dictionary mode. Given a copy made by spreading instead (`runtime_object.js`), through the page bundle `nboyer` runs 1.92 times faster, `browse` 1.77, `deriv` 1.37, `earley` 1.23, `ctak` 1.09; `tak`, `fib`, `puzzle`, `fft` level. `runtime_separation_tests.js` checks the bundled copy with V8's `%HasFastProperties`. JavaScript under `src/`: the copy, one line in a module of its own, for `runtime.js`'s item. | R141 |
| 99 | ✅ | A program and its runtime as one file | Decided with the user: `node repl.js --build PROGRAM -o OUTPUT` writes one ES module -- the program's table bundled by rollup with the runtime and the primitives (`src/packaging/ahead_bundle.js`) -- which `node OUTPUT` runs and a page loads with `<script type="module">`; `runMain` reports a raise nobody handles as the CLI does. The runtime also carries JavaScript interop, classes, promises and the command line, a procedure JavaScript calls back running on the driver. Found on the way: rollup's namespace had made generated code slow on every page (101, R141); the tier compiled a procedure reading `this`, a method's receiver, as reading a global, and it failed -- such procedures are declined now, and refused ahead of time (103). Measured over six small programs that print, raise, make records, use a parameter, use JavaScript and compute (`benchmarks/run_ahead_startup.js`): about 1 MB, 160 KB gzipped, against the 8.4 MB, 920 KB gzipped, today's page loads before its Scheme runs; under Node 45-50 ms from spawn to exit, against `node repl.js`'s 300-345; on a page, finished 30-34 ms after its navigation began, against 204-334. JavaScript under `src/`: the bundling, rollup's; `runMain`, the runtime's start; the CLI's door; a build-host door that reads a program as the CLI does. | R141 |
| 100 | ✅ | A program compiled ahead of time that reads | A primitive that is a door into a library the library system's seed loads for itself is that library's procedure, compiled with the program: `%read`, which `read` calls, is the reader's `read-from-port` (`library-primitives` in `scripts/lib/ahead.scm`), whose port check moved into it from the JavaScript primitive. A program that reaches one has the library loaded and compiled as its imports are, and the runtime binds the name among the primitives once it has loaded; `(scheme read)` is unchanged, and nothing loads a second reader. Under Node the current ports are the standard ones, as the CLI makes them, so a program compiled ahead of time reads what is piped to it. `read1` and `dynamic`, which read data files, run at 0.86 and 0.97 of the tier's time, so every program of the canonical suite's default profile runs ahead of time; a program that reads carries the reader, about a megabyte more. | — |
| 102 | ✅ | The canonical suite through the page bundle | `benchmarks/run_bundle.js` runs each canonical benchmark through the page's entry -- the compiler loaded, the program's own code compiled, the program run as a page runs a script that begins with its imports -- as the source modules and as `dist/scheme.js`, each in Node (`lib/bundle_worker.js`) and in headless Chrome, at one count calibrated on the source in Node. With 101 in, the bundle costs nothing: of the default profile's 45 programs, bundle against source runs 0.89 to 1.06 in Node and 0.91 to 1.04 in Chrome, single runs; the three that read data files have no file system in a browser. Chrome against Node, which it was not built to measure, differs more: `bv2string` 2.7 times slower (104), `string` 3.2 times faster, `ctak` and `fibc` 1.4. Building it found that a page's server blocked by a synchronous measurement in its own process leaves a page's module fetches hanging, so the harness serves from a process of its own. | R141 |
| 103 | ✅ | `this` in compiled code | The runtime keeps the receiver of the method call running in a cell (`methodReceiver` in `values.js`), set by each run of the interpreter and each way JavaScript calls compiled code with a receiver, and given to the nested run compiled code starts by calling an interpreted closure; a lambda that reads `this`, itself or in a lambda inside it, takes the receiver running as it is entered, or else the one the lambda around it took (`receiver-binding` in `ir.scm`), so compiled code agrees with the interpreter's binding of `this` at each application of a method's call. The decline 99 added is gone: the tier compiles such procedures, and a class whose constructor and methods use `this` compiles ahead of time. Procedures that read no `this` are unchanged. | — |
| 104 | ✅ | `bv2string` on a page | Profiled in Chrome and in Node through the page bundle: in Chrome nearly half the time is in the division primitives' wrappers, called about 27 million times by the benchmark's random-number generator, which Node's V8 inlines into the compiled callers and Chrome's does not, and whose integer `%` is slower (6 ns against 1.2 in isolation); the rest is the UTF-8 codecs, made per call. Of ours, the division wrappers now take two parameters rather than rest arguments, and `utf8->string` and `string->utf8` reuse one decoder and one encoder: `bv2string` 3.33 to 2.86 ms an iteration in Node, 9.15 to 8.39 in Chrome; the rest is V8's (105). A remainder by division rather than `%` beyond V8's small integers -- 2^30 in Chrome, which builds V8 with pointer compression, 2^31 in Node -- is 2.5 times faster there, but made the small case 15% slower in Node, and was not kept. | — |
| 105 | ✅ | Exact division in generated code | `quotient`, `remainder` and `modulo` expand inline on two integers held as numbers and a divisor that is not zero, the primitive otherwise (`inline.scm`). `run_codegen.js`'s new `division` group: a remainder 5.2 to 3.4 ns, a quotient 5.6 to 4.1, a digit and the rest of a number 16.0 to 1.8. Canonical suite, before and after from two copies of equal standing: `compiler` 0.75 of its time, `bv2string` 0.80, `maze` 0.86, `primes` 0.87, nothing else beyond noise; `run_tier.js`'s corpus and page sets level (3,304 to 3,315 ms, 208.5 to 202.6). In Chrome `bv2string` gains only 4%: the time moved into the compiled procedure, where Chrome's `%` is slower than Node's. Measuring found that a copy of the repository and the repository itself ran the same code differently (R142). | R142 |
| 58 | ✅ | Compile the inner loops of a procedure the tier declines | Measured, and not done: worth it only if many procedures were left declined, and since 37(a) compiles the procedures that capture, few are. Of the corpus's 2,422 top-level definitions (`decline_reasons.js --corpus`, 2026-10-08) 92.4% compile and 9, 0.4%, are declined for a control form -- 3 handing `call/cc` on as a value, 2 `eval`, 2 `dynamic-wind`, 1 `exit`, 1 `raise`; the 124 made inside a procedure are compiled with it by the tier. A count on local closures, a cost on every interpreted call, would buy inner loops in nine procedures. | — |
| 106 | ✅ | Winds and `parameterize` ahead of time | 37(c1). A program compiled ahead of time keeps its winds in a list the runtime holds (`windList` in `unwind.js`), and `dynamic-wind` is Scheme over it, `(scheme-js winds)`, which the build supplies in the primitive's place (`library-primitives`) and compiles as an ordinary procedure (`lowering-decline`'s `ordinary`); a continuation records the list as it is captured, and invoking one runs the after-thunks it leaves and the before-thunks it enters (`travelTo`). `parameterize`, which is `dynamic-wind`, comes with it, and a program using either now builds and runs. Ahead of time a `parameterize` costs about 1 µs and a `dynamic-wind` 0.6, against 20 and 4.4 under the tier. Found: the build looks for a library's included file by name in the program's directory first, so a program beside a file named like one a system library includes fails to build (107); and the library release test failed once one more library was prebuilt, any library, which it now survives by allocating before it collects (R143). | `tests/functional/ahead_program_tests.js`, `tests/core/scheme/winds_tests.scm`, `tests/compiler/driver_tests.scm`; R143 |
| 108 | ✅ | Handlers and raises ahead of time | 37(c2) and (c3), together, since (c2) alone would have let a program build whose `guard` missed a primitive's error, which it had been refused for. `with-exception-handler`, `raise` and `raise-continuable` are Scheme over a list of handlers the runtime keeps, `(scheme-js handlers)`, supplied as `dynamic-wind` is, each handler in force for an extent `dynamic-wind` makes; they do what the interpreter does. The driver with no interpreter hands what JavaScript throws to that `raise` under a handler its own code installed, so `error` needs no Scheme version, and takes a continuation of a driver still running beneath it by a jump through the drivers and JavaScript between, where it used to re-run the rest of the outer computation inside a callback. `guard`'s `call/cc`, through `(scheme control)`'s binding, is a capture. Found and fixed: the interpreter left the extents around a continuable raise and lost the handler once it returned (R7RS 6.11), and `(car 1)` generated `1.car`, which JavaScript cannot parse. Ahead of time a `guard` costs 0.54 µs when nothing is raised, against 2.6 under the tier, but 5.7 against 3.5 for a raise caught and 22 against 14 for a primitive's error. | `tests/functional/ahead_program_tests.js`, `tests/core/scheme/handlers_tests.scm`, `tests/core/scheme/exception_tests.scm`, `tests/compiler/emit_tests.scm`, `tests/compiler/driver_tests.scm` |
| 37 | ✅ | The decline policy on real code: escapes, not exception handling | Closed. (a) done 2026-09-28: every procedure is compiled, those that capture included, and one whose frames are re-entered far more than saved is switched back to its closure (R86, R90). (b), an escape fast path, not done: it would cover 9 of the corpus's 77 captures (R111), and since 96 compiled `ctak` and `fibc`, its case, run 1.8 and 4.3 times faster than interpreted (R139). (c) done ahead of time, as 106 and 108; the tier still declines a procedure naming an exception form or `dynamic-wind`, which 9 of the corpus's 2,422 definitions are declined for, `eval` and `exit` among them (58's count). | R23, R49, R86, R111, R139 |
| 38 | ✅ | Lowering failure as an escape | A lowering that fails escapes, out of the whole lambda, through a continuation `lower-top-lambda` captures once per definition (`fail!` in `ir.scm`), and the twenty-odd steps that checked what the step before them answered are plain `let`s. `call/cc` rather than `guard`, which 37 made compile only ahead of time: the tier compiles a capture. `npm run benchmark:self-host`, two copies side by side: compiled with its library, 53.2 and 51.1 ms a pass before, 52.9, 53.9 and 50.8 after; interpreted about 4% slower, a capture there copying the frame stack; agreement on every lambda. Also: `ir.scm`'s header said the compiler avoids `apply` and `values`, which compile; and `deferred-exits` in `emit.scm`, which built a pair only to return two results, returns two values, the libraries' generated code unchanged. `generate-environment`'s pair stays, for its JavaScript callers. | `tests/compiler/`, `benchmarks/run_self_host.scm` |
| 53 | ✅ | Drop `source` from runtime `Cons` | Measured, and not done. Every pair carries the reader's span, null for any a program makes; without the field, the ceiling, from two copies side by side over the canonical list programs, compiled: `earley`, which spends a fifth of its time collecting garbage (R140), 15.2 and 15.5 ms against 16.3 and 16.2; `sboyer` 212.5 and 213.3 against 205.0 and 206.4; `nboyer`, `browse`, `destruc`, `deriv`, `graphs`, `lattice`, `mazefun`, `paraffins` and `peval` level. Not worth keeping the spans of what the reader reads elsewhere -- a `WeakMap` every span's reader pays for, or a subclass that makes every `car` site see two shapes. | `benchmarks/run_r7rs.js --only` |
| 107 | ✅ | The build finds a library's included file beside the library | Found building 106: the build, and the CLI for the directory it runs in, looked for every file a library includes by its bare name, the program's directory first, so a program beside a `list.scm` or `numbers.scm` failed to build. The loader now gives the resolver, with an included file's path, the path of the library's own file (`resolve-included` in `library_system.scm`), and the build's resolver (`library-resolver` in `scripts/lib/prebuild.scm`) and the CLI's (`besideLibrary` in `repl.js`) look beside that file first; a resolver of one argument behaves as before. A user's libraries in the program's directory, their includes beside them, still build. JavaScript under `src/`: three doors fixed in place to pass the argument. | — |
| 84 | ✅ | Debugging Scheme and JavaScript together in DevTools | Chosen by the user 2026-10-08. A step from a page's Scheme into its JavaScript, or back, stops at the user's code on the other side, never in the system between: every output of the build has a map listing its sources as ignore-listed (`rollup.config.js`), the prebuilt tables and a program built ahead of time are mapped to their Scheme and ignore-listed too (`write-tables!` in `scripts/lib/prebuild.scm`, `span-placer` in `scripts/lib/ahead.scm`), and every line a pause can land on is placed in the Scheme, a macro's expansion at its use (`with-node-placed` in `emit.scm`, `with-use-span` in the expander). While DevTools debugs -- `scheme-devtools` in a page's URL, `schemeJS.devtools()` in its console, Node's inspector, or `--devtools` -- every procedure, top-level form and definition's value is compiled before it runs, so a breakpoint in it binds (`tier-compile-eagerly!`), and each procedure compiled code makes keeps a way back to an interpreted closure sharing its state, for the REPL's debugger (`way-back` in `emit.scm`). Values are drawn as Scheme by a custom formatter written in Scheme, `(scheme-js devtools)`, switchable to JavaScript, one value or all. Locals keep their written names as near as JavaScript allows (`local-name`), and while DevTools debugs a map gives source map scopes for the exact names (`scopes.scm`), which DevTools 146 names frames by but its Scope pane does not yet read (R146); stepping into a macro's template did not come with them (R147). Tested by driving DevTools' own front end in a headless Chrome, 83 tests (`tests/devtools/`). JavaScript under `src/`: the formatter's shim, reflection and JsonML, for interop; the way back's cell and entry, for the runtime; an environment over compiled code's boxes and a closure over a lambda, for the evaluator; the switch at a page's start-up and the CLI's, doors; the bundle's map chaining, the build's. | R144, R145, R146, R147 |

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
