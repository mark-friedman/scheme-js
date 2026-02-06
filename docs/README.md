# Documentation Overview

This directory contains design documents, research notes, implementation details, and references for the `scheme-js` project.

## File Summaries

### Architecture & Design
- **[architecture.md](architecture.md)**: High-level architectural overview of Scheme V4, detailing the core components (Interpreter, AM, Compiler), the compilation pipeline, and the runtime environment.
- **[chrome_scheme_debugger_design.md](chrome_scheme_debugger_design.md)**: Comprehensive design document for a Chrome-integrated Scheme debugger. It covers the architecture (Debug Runtime, Source Maps, DevTools), implementation phases, and UI considerations.
- **[external_scheme_debugger_plan.md](external_scheme_debugger_plan.md)**: A plan for implementing an external debugger, likely focusing on the protocol and integration points for remote debugging.
- **[node_devtools_integration.md](node_devtools_integration.md)**: Details the integration with Node.js DevTools, exploring how to bridge the Scheme runtime with Node's inspector protocol.
- **[debugger_requirements.md](debugger_requirements.md)**: Lists the functional and non-functional requirements for the Scheme debugger, including breakpoints, stepping, and state inspection.
- **[debugger_research.md](debugger_research.md)**: Research notes on existing debugging techniques and tools, serving as a background for the debugger design.

### User Guides
- **[debugger_manual.md](debugger_manual.md)**: A user manual for the Scheme debugger, explaining how to activate it, set breakpoints, step through code, and inspect variables in both Node.js and Browser REPLs.
- **[macro_debugging.md](macro_debugging.md)**: A guide for troubleshooting `syntax-rules` macros, covering common issues like unbound variables, accidental capture, and infinite expansion.

### Implementation Details
- **[core-interpreter-implementation.md](core-interpreter-implementation.md)**: A detailed explanation of the core interpreter's implementation. It covers the trampoline loop, stack frame management, and the execution model.
- **[core-interpreter-implementation-citations.md](core-interpreter-implementation-citations.md)**: Contains citations and references for the algorithms and techniques used in the core interpreter.
- **[continuation_implementation_techniques.md](continuation_implementation_techniques.md)**: Discusses specific techniques for implementing first-class continuations, such as `SentinelFrame` and stack management strategies.
- **[hygiene.md](hygiene.md)**: Deep dive into the hygienic macro expansion algorithm (Sets of Scopes) used in the project, explaining how scopes, marks, and bindings work.
- **[numeric_tower_discussion.md](numeric_tower_discussion.md)**: analysis and discussion regarding the implementation of the numeric tower, specifically comparing BigInt-based approaches vs. Number-based approaches and their performance implications.
- **[Interoperability.md](Interoperability.md)**: Documents the interoperability between Scheme and JavaScript, including type conversion rules (deep vs. shallow) and calling conventions.

### Research & References
- **[REFERENCES.md](REFERENCES.md)**: A list of external references and standards relevant to the project (e.g., R7RS).
- **[r7rs-small errata-corrected.pdf](r7rs-small%20errata-corrected.pdf)**: The PDF specification of the R7RS-small Scheme standard.
- **[An Unexceptional Implementation of Continuations.txt](An%20Unexceptional%20Implementation%20of%20Continuations.txt)**: Text version of a research paper describing a technique for implementing continuations using exceptions.
- **[An Unexceptional Implementation of Continuations.pdf](An%20Unexceptional%20Implementation%20of%20Continuations.pdf)**: PDF version of the research paper mentioned above.
- **[A Method for Implementing First-Class Continuations on the JVM and CLR (AI assisted).pdf](A%20Method%20for%20Implementing%20First-Class%20Continuations%20on%20the%20JVM%20and%20CLR%20\(AI%20assisted\).pdf)**: Research paper on implementing continuations on virtual machines like the JVM and CLR.
- **[Continuations from Generalized Stack Inspection.pdf](Continuations%20from%20Generalized%20Stack%20Inspection.pdf)**: Research paper discussing continuations in the context of stack inspection.
