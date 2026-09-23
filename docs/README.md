# Documentation Overview

This directory contains design documents, research notes, implementation details, and references for the `scheme-js` project.

## File Summaries

### Architecture & Design
- **[../ROADMAP.md](../ROADMAP.md)**: Where the project is going, and the six constraints any plan has to satisfy — JS interop, browser and CLI, a REPL in both, a debugger in both, full multi-shot `call/cc`, and full R7RS-small compliance. Forward-looking: planned work, and a short summary of what has been delivered. It does not track individual tasks.
- **[compiler_plan.md](compiler_plan.md)**: *Living.* The only place that ranks compiler work — a single numbered list with status, dependencies, and the finding that justifies each item's position. The fifteen most recently completed items collect at the bottom.
- **[compiler_plan_completed.md](compiler_plan_completed.md)**: *Append-only.* Every completed compiler task, one line each, under the number it had in the plan, so a reference like "task 13" resolves after the row has left the plan.
- **[archive/r7rs_compliance_phases.md](archive/r7rs_compliance_phases.md)**: *Archived, complete.* The phase-by-phase R7RS-small implementation checklist, feature by feature — what "compliant" was taken to mean here, and the order it was built in.
- **[performance_progress.md](performance_progress.md)**: *Generated.* Benchmark results after each stage of the compiler effort — wall-clock timings, deterministic evaluator step counts, and distance to Gambit and Racket — so progress over time is visible at a glance. Regenerate with `npm run benchmark:record`.
- **[performance_baseline.md](performance_baseline.md)**: The Stage 0 measurement gate for the compiler effort. Records the benchmark suite, cross-implementation timings against Gambit and Racket, the CPU profile, evaluator step counts, and the R7RS-small conformance audit, with instructions for reproducing all of them. **Read the benchmark-validity warning it carries before quoting any speedup from it.**
- **[compiler_design.md](compiler_design.md)**: *Rewritable.* How the compiler tier works and why — calling convention B and the bake-off that chose it, the fast form and its resumable twin, the capture protocol, boxing, lambda lifting, the IR, the self-hosting bootstrap chain, what the tier declines and why, and an honest table of the four project constraints against what is actually met. Carries only the reasoning no single module can own; per-module rationale lives in the module headers.
- **[compiler_findings.md](compiler_findings.md)**: *Append-only.* Formerly `compiler_strategy.md`, and where every `R`-number citation in this repository points. A record of **what we believed that turned out to be false**, and the measurement that proved it — not a history of what was built, which is `CHANGES.md`. Superseded text is annotated rather than deleted, so the reasoning that led to a wrong call stays legible. R0 is the founding analysis (~650x slower than plain JS on `fib(30)`, ~95% of runtime in interpretive overhead); the appendix holds the original staged plan.
- **[architecture.md](architecture.md)**: High-level architectural overview of Scheme V4, detailing the core components (Interpreter, AM, Compiler), the compilation pipeline, and the runtime environment.
- **[new_debugger_design.md](new_debugger_design.md)**: A proposed high-performance debugger architecture — the aspirational design, not a description of what is built. The shipped debugger is described in [debugger_manual.md](debugger_manual.md) and `src/debug/`.
- **[chrome_scheme_debugger_design.md](archive/chrome_scheme_debugger_design.md)**: Comprehensive design document for a Chrome-integrated Scheme debugger. It covers the architecture (Debug Runtime, Source Maps, DevTools), implementation phases, and UI considerations.
- **[external_scheme_debugger_plan.md](archive/external_scheme_debugger_plan.md)**: A plan for implementing an external debugger, likely focusing on the protocol and integration points for remote debugging.
- **[archive/Challenges.md](archive/Challenges.md)**: *Archived.* An early assessment of which parts of R7RS-small would be hardest on JavaScript — the numeric tower, continuations, tail calls. Kept because it is the prediction the implementation can be checked against; the numeric tower it called challenging was built, and the ~3x it costs is measured in `compiler_findings.md`.
- **[archive/Interoperability_Assessment.md](archive/Interoperability_Assessment.md)**: *Archived.* A review of the "primitives are primitives" interop design before it was implemented, judging it viable. The design shipped; the current description is [Interoperability.md](Interoperability.md).
- **[node_devtools_integration.md](node_devtools_integration.md)**: Details the integration with Node.js DevTools, exploring how to bridge the Scheme runtime with Node's inspector protocol.
- **[debugger_requirements.md](debugger_requirements.md)**: Lists the functional and non-functional requirements for the Scheme debugger, including breakpoints, stepping, and state inspection.
- **[debugger_research.md](debugger_research.md)**: Research notes on existing debugging techniques and tools, serving as a background for the debugger design.

#### Benchmark suites
Three suites, deliberately kept separate rather than merged into one number:
- `benchmarks/programs/` — eight microbenchmarks written in Stage 0. Known to be **overfitted** to the optimizations chosen against them; good for tracking our standing against other implementations, unreliable for deciding whether an optimization helps.
- `benchmarks/r7rs/` — the canonical Gabriel/Gambit/Larceny suite, vendored from `ecraven/r7rs-benchmarks`, classified by **workload class** and reported per class. Methodology in [benchmarks/r7rs/README.md](../benchmarks/r7rs/README.md); results in **[r7rs_benchmark_results.md](r7rs_benchmark_results.md)**. Run with `npm run benchmark:r7rs`.
- The project's own Scheme test files, as a transfer test. Run with `npm run benchmark:macro`.

### User Guides
- **[debugger_manual.md](debugger_manual.md)**: A user manual for the Scheme debugger, explaining how to activate it, set breakpoints, step through code, and inspect variables in both Node.js and Browser REPLs.
- **[macro_debugging.md](macro_debugging.md)**: A guide for troubleshooting `syntax-rules` macros, covering common issues like unbound variables, accidental capture, and infinite expansion.

### Implementation Details
- **[core-interpreter-implementation.md](core-interpreter-implementation.md)**: A detailed explanation of the core interpreter's implementation. It covers the trampoline loop, stack frame management, and the execution model.
- **[core-interpreter-implementation-citations.md](core-interpreter-implementation-citations.md)**: Contains citations and references for the algorithms and techniques used in the core interpreter.
- **[continuation_implementation_techniques.md](continuation_implementation_techniques.md)**: Discusses specific techniques for implementing first-class continuations, such as `SentinelFrame` and stack management strategies.
- **[hygiene.md](hygiene.md)**: Deep dive into the hygienic macro expansion algorithm (Sets of Scopes) used in the project, explaining how scopes, marks, and bindings work.
- **[numeric_tower_discussion.md](numeric_tower_discussion.md)**: analysis and discussion regarding the implementation of the numeric tower, specifically comparing BigInt-based approaches vs. Number-based approaches and their performance implications. Note that [compiler_findings.md](compiler_findings.md) measures the total cost of the numeric tower at roughly 3x, against ~200x for interpretive overhead, so the optimizations proposed here are lower-priority than they appear.
- **[Interoperability.md](Interoperability.md)**: Documents the interoperability between Scheme and JavaScript, including type conversion rules (deep vs. shallow) and calling conventions.

### Research & References
- **[REFERENCES.md](REFERENCES.md)**: A list of external references and standards relevant to the project (e.g., R7RS).
- **[r7rs-small errata-corrected.pdf](r7rs-small%20errata-corrected.pdf)**: The PDF specification of the R7RS-small Scheme standard.
- **[An Unexceptional Implementation of Continuations.txt](An%20Unexceptional%20Implementation%20of%20Continuations.txt)**: Text version of a research paper describing a technique for implementing continuations using exceptions.
- **[An Unexceptional Implementation of Continuations.pdf](An%20Unexceptional%20Implementation%20of%20Continuations.pdf)**: PDF version of the research paper mentioned above.
- **[A Method for Implementing First-Class Continuations on the JVM and CLR (AI assisted).pdf](A%20Method%20for%20Implementing%20First-Class%20Continuations%20on%20the%20JVM%20and%20CLR%20\(AI%20assisted\).pdf)**: Research paper on implementing continuations on virtual machines like the JVM and CLR.
- **[Continuations from Generalized Stack Inspection.pdf](Continuations%20from%20Generalized%20Stack%20Inspection.pdf)**: Research paper discussing continuations in the context of stack inspection.
