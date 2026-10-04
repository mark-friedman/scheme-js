---
trigger: always_on
---

# Global Project Rules

## Agentic Rules
- **Following Orders**: DO NOT make any changes or carry our implementation tasks if the user just asks a question.  Just answer the question!

## Scheme first
The system is to be as much Scheme as it can be, the interpreter as much as the compiler: for dogfooding, so that the compiler is its own benchmark and the slow parts of its Scheme say what to optimize next, and so that the code reads as Scheme at its best.
- **What may be JavaScript**: the evaluator, for now; the value representations, and the primitives that operate on them -- a primitive that checks its arguments and then does its work with JavaScript's own operations on a whole string, vector or number stays JavaScript, since in Scheme it would be a slower copy of the same thing; `src/compiler/runtime.js`; code generation (`new Function`) and the save-and-resume protocol; and the cores of libraries that need a JavaScript feature -- host input and output, reflection for interop, JavaScript classes, the `Map` under hash tables, Unicode tables, `BigInt`. Everything else is Scheme: a procedure above the primitives, and anything that would call a Scheme procedure back from JavaScript.
- **Before adding a function or new logic to a `.js` file under `src/`**, name the item above that requires it. If none does, write it in Scheme. A JavaScript caller, JavaScript beside it, or an existing JavaScript module is not a reason: Scheme and JavaScript call each other. Fixing JavaScript in place is allowed; extending JavaScript that is to become Scheme is not -- the change goes after its port, or is written in Scheme as the port. This holds however the file is written: an editor tool, a shell command or a script. For Claude Code, a hook in `.claude/settings.json` shows this list whenever an edit adds a JavaScript function under `src/`.
- **Where Scheme lacks a capability** (hash tables, for example), build it as a Scheme library over the minimum JavaScript, rather than writing the code that needs it in JavaScript. New code is Scheme from the start: code written in JavaScript to be ported later has never once been ported.
- **JavaScript that has to start Scheme** -- the evaluator's hooks, a page's or the CLI's start-up -- calls Scheme procedures and does nothing else. `src/compiler/lowering.js` is that door into the compiler: it starts the compiler and hands out its exports, which JavaScript calls as it calls any Scheme procedure, through the public interop (`callSchemeProcedure`, or a procedure's plain call); new logic never goes in it.
- **Idiomatic Scheme, written from the intent**: records, small procedures, folds, `cond` and `case` -- not a transliteration of JavaScript's loops, mutable accumulators and vector-slot records. Before porting JavaScript, say what each piece is for, and design the Scheme around that.
- **General helpers are SRFIs**, implemented in full as libraries users can import, as SRFI 1, 125, 128 and 152 are, not private utilities. A specialised version of one (an `eq?`-only set union, say) is fine if its comment says why.
- **Tests of Scheme code are Scheme** (see *Testing*). JavaScript tests are for what only JavaScript can observe: JavaScript calling Scheme, generated code, comparisons between the tiers.
- **At the end of a task**, its outcome lists the JavaScript added or grown, each with the item above that required it. `npm run audit:languages -- <base>` counts the lines of each language added and removed under `src/` since the commit `<base>`, uncommitted and untracked files included.

## Testing Requirements
- All new features must have accompanying unit and/or functional tests in `tests/`.
- Any complex logic should have unit tests.  Refactor to make it testable if needed.
- Ensure tests pass in both Node.js and the browser.
- Most tests should be written BEFORE the code that is being tested is written. Sometimes you'll realize after writing the code that you need to write additional tests for it and that's ok.  It's also ok to occasionally rewrite tests to make them more correct or more effective or cover more cases.

## Testing
- **Dual Environment**: All tests must run in both Node.js and the browser.
  - Use `if (typeof process !== 'undefined')` to guard Node.js-specific code (like `process.exitCode` or `import.meta.url` checks).
- **Structure**: Place new tests in `tests/` and register them in `tests/test_manifest.js`.
  - **Scheme tests** are `.scm` files using the harness in `tests/core/scheme/test.scm`, registered by path: tests of the Scheme libraries in `tests/core/scheme/`, listed in `schemeTestFiles`; tests of the compiler's Scheme in `tests/compiler/`, listed in `compilerSchemeTestFiles`, which run in the compiler's own library environment so that its internal procedures are in scope.
  - **JavaScript tests** are modules registered by path and function, marked `nodeOnly` when they cannot run in the browser.
- **Execution**:
  - Run `node run_tests_node.js` to verify changes locally.
  - Verify browser compatibility via `http://localhost:8080/ui.html`.

## JavaScript Code Style
- **Modules**: Use ES Modules (`import`/`export`).
- **Formatting**: Use 2 spaces for indentation.
- **Exports**: Export functions and classes clearly.
- **Primitives**: All new JavaScript primitives exposed to Scheme (e.g. in `src/core/primitives/`) MUST be marked as "Scheme-aware" by setting the `SCHEME_PRIMITIVE` symbol (exported from `src/core/interpreter/values.js`) on the function. This is typically handled by the `addPrimitives` helper in `src/core/primitives/index.js`.

## Scheme Code Rules
- **Type Checking**: All standard Scheme procedures (i.e. the ones in the r7rs-small standard) must be implemented with all neccessary type, range, and arity checking.
- **Which language**: see *Scheme first*, above.

## Code Organization
- **Directory and File Structure**: Follow the directory and file structure outlined in `architecture.md`.  If you need to deviate from this structure or add to it, update `architecture.md` to reflect the change.
- **Scheme Dependencies**: Scheme procedures and special forms should be described in Scheme `.sld` files in libraries using `define-library` to express their exports, dependencies, etc. Generally the `.sld` files only describe the libraries.  The actual implementations should be in separate `.scm` files. For standard Scheme procedures (i.e. the ones in the r7rs-small standard) use appendix A of the r7rs-small standard to determine the libraries that those procedures should be defined in.


## Documentation
- **JSDoc**: Document all JavaScript functions with JSDoc.
- **Scheme Doc**: Document all Scheme functions with JSDoc-style comments, using the same format as JSDoc, but with Scheme procedure-level comment syntax (i.e. `;;`).
- **Internal Documentation**: Document logic inside JavaScript and Scheme functions and procedures using comment syntax appropriate for the language.
- **Comments must stand alone**: A comment must be understandable by someone reading only the source file. Do not cite planning artifacts as the explanation — no revision-log entry numbers, no "see increment N", no "the plan says". If a decision needs justifying, state the reason in the comment. Referring to a concrete artifact in the repository is fine and often useful: a benchmark by name (`benchmarks/r7rs/src/maze.scm`), a sibling module, a published paper. Comments carry their own reasoning; `docs/compiler_design.md` carries the cross-cutting design a comment cannot, and duplicating a module header into it would only rot the copy.
- **Code Sections**: Document the start of associated collections of functions and procedures using comment syntax appropriate for the language.
- **Directory Structure**: Generally, follow the directory structure outlined in `docs/architecture.md`.  If you need to deviate from this structure or add to it, update `docs/architecture.md` to reflect the change.
- **CHANGES.md**: Document the changes you make by appending your walkthrough.md files to `CHANGES.md` when any major talks are completed.
- **docs/compiler_plan.md**: The only place that ranks compiler work. **Read it before starting a task and update it when finishing one** — tick the entry, move it to `Completed` with a one-line outcome, append the same row to `docs/compiler_plan_completed.md`, and add whatever the work revealed. `Completed` keeps the fifteen most recent; the completed file keeps every one, so task numbers always resolve. A plan nobody reads is how the last one rotted. When a decision would change the order or the dependencies, say so there rather than in conversation, so the reasoning survives a context compaction.
- **Roadmap**: `ROADMAP.md` is forward-looking and high-level — the project's constraints, the features planned, and a short table of what has been delivered. Add to it when a **user-visible** goal is planned or met, and simplify the entry into the delivered table when it lands. It does not track individual tasks, does not rank work, and is not a history: `docs/compiler_plan.md` ranks compiler work, `CHANGES.md` is the history, and the completed R7RS-small phase checklist is archived under `docs/archive/`.
- **Which document takes a given change**, by lifetime:

  | document | lifetime | holds |
  |---|---|---|
  | `ROADMAP.md` | rewritable | the constraints, planned user-visible goals, what shipped |
  | `docs/architecture.md` | rewritable | the file map and how the system fits together |
  | `docs/compiler_design.md` | rewritable | how the compiler works and why |
  | `docs/compiler_plan.md` | living | ranked compiler tasks, status, dependencies |
  | `docs/compiler_plan_completed.md` | **append-only** | every completed compiler task, one line each |
  | `docs/compiler_findings.md` | **append-only** | only what we believed that turned out to be false |
  | `CHANGES.md` | **append-only** | what happened, increment by increment |
  | `docs/archive/` | frozen | finished work kept for the record, not maintained |

  An entry that records no falsification belongs in `CHANGES.md`, not the findings log. Never rewrite an append-only document; annotate the superseded passage instead.
- **Links run one way**: living documents point at append-only ones, never the reverse. A back-link out of `CHANGES.md` or the findings log would have to be edited every time priorities move, which is the same as letting it go stale.
- **Cleanup**: Remove any comments that were created just for yourself and/or that don't explain any functionality or algorithmic details.
## Tools
### Scheme Conformance
- **Execution Based Conformance**: You can use the website at https://try.scheme.org/ to execute any Scheme code on a standard implementation.  This is not a substitute for testing, but it can be helpful for debugging or if you're unsure about the behavior of a Scheme language feature.
- **Standard Documentation Based Conformance**: The R7RS-small standard is available in `docs/r7rs-small.pdf`.  It is the primary reference for Scheme language features and behavior.
