# Scheme Interpreter Architecture

R7RS-Small Scheme in JavaScript: minimal JS runtime, maximal Scheme libraries.

## Two-Tier Model

```
┌─────────────────────────────────────────────────────┐
│                   User Code                          │
├─────────────────────────────────────────────────────┤
│              R7RS Libraries (Scheme)                 │
│   (scheme base) (scheme write) (scheme read) ...    │
├─────────────────────────────────────────────────────┤
│              JavaScript Runtime                      │
│   Interpreter • Primitives • Library Loader          │
└─────────────────────────────────────────────────────┘
```

## The compiler's bootstrap

The compiler tier is itself Scheme -- the library `(scheme-js compiler)`,
`src/compiler/compiler.sld` and the files it includes -- so the system compiles part of
itself. The chain has three links and terminates in the interpreter, which needs no
compiler at all:

```
interpreter loads (scheme-js compiler) from source   (slow, but needs nothing)
   -> compiles every library the bundle ships        -> src/packaging/compiled_libraries.js
   -> compiles the compiler's own library            -> src/packaging/compiled_compiler.js
```

Both steps run at build time (`npm run prebuild`), so nothing calls `new Function` at run
time and a page under a strict Content-Security-Policy gets compiled libraries and a
compiled compiler. Each table is installed into its library's environment as the library
loads (`installLibraryTable`), after checking a fingerprint of the library's `.sld` and
the files it includes; a table whose sources have moved on installs nothing -- a stale
build costs speed, never correctness.

The middle link is not an optimization of the last one. Lowering calls `memq` and `assq`
on every scope lookup, and those are themselves Scheme: compiling the compiler against an
interpreted library is worth 1.5x, against a compiled one 25x. `npm run
benchmark:self-host` measures all three configurations and checks that they agree about
every answer.

The compiler loads its library, and the libraries that imports, into a registry of its
own (`withPrivateLibraries`), so it never shares `(scheme base)` or SRFI 1 with the
program it compiles.

### Two bundle files

Because every shipped library arrives compiled from its table, a page runs no compiler to
get compiled libraries. So the compiler is not in `dist/scheme.js`: it is
`dist/scheme_compiler.js`, split out by rollup from the dynamic import in `loadCompiler`
(`src/packaging/scheme_compiler.js`), which the bundle fetches after it has started, to
compile the page's own code as it runs (`src/compiler/tier.scm`, attached by `src/compiler/tiering.js`).

## JavaScript Runtime Components

| Component | Purpose |
|-----------|---------|
| `interpreter.js` | Trampoline execution loop |
| `stepables_base.js` | Register constants + `Executable` base class |
| `ast_nodes.js` | AST node classes (Literal, If, Lambda...), and the pending raise compiled code throws for the interpreter to perform |
| `frames.js` | Continuation frame classes |
| `reader.js` | S-expression parser |
| `expand.js` | The door into the expander, `(scheme-js expander)`: `analyze`, a form into the evaluator's node |
| `assembler.js` | The evaluator's door: a core form, as the expander makes it, into nodes |
| `library_registry.js` | The library system's door from JavaScript: the current registry, and the API calling the Scheme |
| `library_seed.js` | Loads the library system (Scheme) at first use, apart from programs, and installs its prebuilt tables |
| `source_texts.js` | The text of code read under a name nothing could fetch it by -- a page's inline script -- for source maps |
| `library_loader.js` | Loading, defining and importing libraries from JavaScript, through the Scheme; fetching an asynchronous resolver's files first |
| `number_representation.js` | How a number is held: an exact integer a JavaScript number in the safe range and a `BigInt` beyond it, an inexact real a number unless its value is an integer, when it is a `Flonum` box; the arithmetic the primitives and compiled code share on numbers held so, and the conversions to the numeric tower's own representation, `BigInt` exact and number inexact, which `math.js` computes in |
| `primitives/` | Native procedures |
| `primitives/io/` | Port system, Reader execution, Printer |

## Features Implemented in JS Core

| Feature | Description |
|---------|-------------|
| Trampoline | TCO via register machine |
| `call/cc` | First-class continuations |
| `dynamic-wind` | Before/after thunk protocol |
| Multiple Values | `values` / `call-with-values` |
| Hygienic Macros | Mark/rename algorithm |
| Exceptions | Handler stack, `raise`, `guard` |
| Parameters | `make-parameter`, `parameterize` |
| Library Loader | R7RS module system |
| Debugger | Breakpoints, Stepping, Inspection |


## Directory Structure

```text
/
├── repl.js                         # Node.js REPL entry point; a program it runs has the process's standard ports, imports the compiler as any library, and -I names library directories
├── rollup.config.js                # Rollup bundling configuration
├── .agent/rules/rules.md           # The project's rules; AGENTS.md and CLAUDE.md link here
├── .claude/                        # Claude Code project settings
│   ├── settings.json               # Committed hooks (settings.local.json stays personal)
│   └── hooks/scheme_first.sh       # Reminds the agent what may be JavaScript when an edit adds a JS function under src/
├── .github/                        # CI/CD Workflows
│   └── workflows/
│       └── ci.yml                  # GitHub Actions CI (Tests + Benchmarks)
├── benchmarks/                     # Performance Benchmarks
│   ├── run_benchmarks.js           # Numeric-tower benchmark runner
│   ├── save_baseline.js            # Create/update baseline
│   ├── compare_baseline.js         # Compare current vs baseline
│   ├── baseline.json               # Recorded baseline metrics
│   ├── *.scm                       # Numeric-tower benchmark definitions
│   ├── run_standard.js             # Standard suite runner (call/tail/alloc/call-cc)
│   ├── count_steps.js              # Deterministic evaluator step counts
│   ├── profile.js                  # CPU profiler (inspector API)
│   ├── compare_implementations.js  # Same programs under Gambit and Racket
│   ├── baseline_standard.json      # Stage 0 baseline for the compiler effort
│   ├── run_compiled.js             # Compiler tier vs interpreter, standard suite
│   ├── profile_compiled.js         # CPU profiler for the compiler tier
│   ├── run_macro.js                # Transfer test: the project's own .scm test files
│   ├── compare_macro.js            # That workload under Gambit and Racket
│   ├── run_r7rs.js                 # Canonical suite, both tiers, by workload class
│   ├── compare_r7rs.js             # Canonical suite: both tiers vs Gambit (gsi, C, JS), Racket, plain JS
│   ├── decline_reasons.js          # Why the tier declines procedures: this repository's Scheme, or --corpus
│   ├── run_escapes.js              # The capture policy on escapes: interpreted, default, captures compiled
│   ├── corpus/                     # Real R7RS code, for decline_reasons.js --corpus and run_tier.js --set corpus
│   │   ├── manifest.json           # SRFI repositories at a commit, Snow-Fort packages at a version and SHA-256; each one's test programs
│   │   ├── fetch.js                # Downloads the manifest into downloads/, checking each archive
│   │   ├── wrappers/               # Libraries of ours over a source's code that is not one (srfi-48.sld over SRFI 48's reference file)
│   │   └── downloads/              # Not committed: other people's code, under their licenses
│   ├── run_self_host.scm           # The compiler lowering its own corpus, three ways: a Scheme program
│   ├── run_hash_tables.js          # SRFI 125 tables and record reads under the tier
│   ├── run_codegen.js              # Targeted: one construct per code-generation decision, both tiers
│   ├── run_tier.js                 # Programs as a page runs them, the tier's compiling counted: canonical, test files, corpus, page; the policy settable
│   ├── run_interop.scm             # The interop axis: each crossing between Scheme and JavaScript, both tiers and plain JavaScript; a Scheme program
│   ├── run_startup.js              # The start-up axis: the CLI, a fresh process phase by phase, a page in headless Chrome
│   ├── startup/                    # probe.js, one start phase by phase; page.html, a page as pages are written
│   ├── run_debugger.js             # The debugger-on axis: kernels under four states of the debugger, both tiers
│   ├── run_coverage.js             # How fitted each suite is to the compiler's inline expansions (R20's measures)
│   ├── tier_programs/              # Synthetic programs in the shapes of a page's code, for run_tier.js --set page
│   │   ├── events.scm              # Handlers made once by a setup procedure, then called by a stream of events
│   │   ├── messages.scm            # A model updated by messages through a dispatch table; selectors made once
│   │   └── render.scm              # A page rendered from its data a few times: many small templates
│   ├── record_progress.js          # Regenerates docs/performance_progress.md
│   ├── lib/
│   │   ├── harness.js              # A run as the REPL runs a program, the standard libraries imported, each in a registry of its own; timing
│   │   ├── r7rs_harness.js         # Canonical-suite protocol, sizing, calibration
│   │   ├── r7rs_worker.js          # One measurement per child process, under a budget
│   │   ├── r7rs_compare.js         # Cross-implementation arithmetic: per-class ratios, reading a saved run
│   │   ├── corpus_libraries.js     # The corpus's libraries and test programs, and a resolver over them and the bundle
│   │   ├── coverage.js             # Calls counted by name, the share inlined, and the test runner's environment
│   │   ├── step_counts.js          # Deterministic dispatch counting
│   │   └── progress_report.js      # Progress-document rendering
│   ├── programs/                   # Portable R7RS benchmark programs (Stage 0)
│   │   ├── manifest.js             # Sizes, expected results, categories
│   │   └── *.scm                   # fib, tak, oddeven, nqueens, ctak,
│   │                               #   contfib, btsearch, threads, threads10
│   │                               # NOTE: overfitted -- see benchmarks/r7rs/README.md
│   └── r7rs/                       # Canonical Gabriel/Gambit/Larceny suite (vendored)
│       ├── plain_js_kernels.js     # Plain JavaScript versions of seven of its programs
│       ├── README.md               # Provenance, protocol, sizing, blocked programs
│       ├── UPSTREAM_COMMIT         # Pinned ecraven/r7rs-benchmarks revision
│       ├── manifest.js             # Workload class, sizes, status per program
│       ├── src/*.scm               # 52 programs, verbatim, plus common.scm and
│       │                           #   the Gambit and Racket preludes
│       └── inputs/*                # Canonical inputs and data files, verbatim
├── experiments/                    # Throwaway prototypes, not production code
│   └── stage2a/                    # Calling-convention bake-off (see compiler_design.md)
│       ├── frontend.js             # Shared front end: Scheme subset -> normalized tree
│       ├── runtime.js              # Shared values and primitives for both backends
│       ├── backend_a.js            # Convention A: explicit frame stack + trampoline
│       ├── backend_b.js            # Convention B: native JS stack + unwind capture
│       ├── backend_b_resume.js     # Convention B's resumable twins
│       ├── stack_machine.js        # Convention A's explicit stack
│       ├── run.js / summary.js     # Correctness and timing
│       └── stack_shape.js          # What a debugger's call stack would show
├── scripts/                        # Build and audit tooling
│   ├── generate_bundled_libraries.js # Inlines .sld/.scm sources for the browser
│   ├── generate_compiled_libraries.scm # Compiles every shipped library at build time: a Scheme program, run from the CLI
│   ├── generate_compiled_compiler.scm # Compiles the compiler's own library at build time: a Scheme program
│   ├── pin_seed.scm                # Writes the pinned seed: the reader's and the expander's libraries as core forms (npm run pin:seed)
│   ├── lib/prebuild.sld            # (scheme-js prebuild): what those build steps share, found with -I scripts/lib
│   ├── lib/prebuild.scm            # Its procedures: the libraries' files, the forms loading runs, a library's table, reports
│   ├── lib/table-writer.sld        # (scheme-js table-writer): writes a module of prebuilt tables, one per library
│   ├── lib/table_writer.scm        # Its procedures: constants as JavaScript, entries, tables, the module
│   ├── audit_r7rs.js               # R7RS-small conformance audit
│   ├── language_balance.scm        # Lines of Scheme and JavaScript a change adds under src/ (npm run audit:languages)
│   └── r7rs_identifiers.js         # Required-identifier reference list
├── src/
│   ├── packaging/                  # Bundling and distribution logic
│   │   ├── scheme_entry.js         # Core bundle entry point; installs library tables
│   │   ├── scheme_compiler.js      # The compiler, as loadCompiler() fetches it after start-up
│   │   ├── scheme_repl_wc.js       # Web Component entry point
│   │   ├── html_adapter.js         # HTML script tag adapter
│   │   ├── bundled_libraries.js    # GENERATED: library sources, for the browser
│   │   ├── compiler_sources.js     # GENERATED: the compiler library's sources
│   │   ├── compiled_libraries.js   # GENERATED: each shipped library, compiled; its other forms as core forms and its define-library form, as JSON
│   │   ├── compiled_compiler.js    # GENERATED: the compiler's library, compiled
│   │   └── pinned_seed.js          # GENERATED on purpose, not by the build: what the seed reads and expands with when its tables are stale
│   │
│   └── core/                       # The Core (JS Interpreter + Scheme subset)
│       ├── interpreter/            # JavaScript Interpreter
│       │   ├── index.js            # EXPORT: createInterpreter()
│       │   ├── interpreter.js      # Trampoline execution loop
│       │   ├── stepables.js        # Barrel file (re-exports all stepables)
│       │   ├── stepables_base.js   # Base class + register constants
│       │   ├── ast_nodes.js        # AST node classes (Literal, If, Lambda...)
│       │   ├── frames.js           # Continuation frame classes, incl. CompiledFrame
│       │   ├── unwind.js           # Capturing a continuation across compiled code, and moving deep compiled frames to the heap, through nested runs; the driver that finishes compiled code's captures itself
│       │   ├── ast.js              # Legacy barrel file
│       │   ├── frame_registry.js   # Frame factory functions
│       │   ├── winders.js          # Dynamic-wind utilities
│       │   ├── environment.js      # Environment class
│       │   ├── primitive_bindings.js # Whether a primitive's name was ever rebound
│       │   ├── errors.js           # SchemeError class
│       │   ├── values.js           # Closure, Continuation, TailCall, Values; calling a procedure with Scheme values
│       │   ├── cons.js             # Cons cells + list utilities
│       │   ├── symbol.js           # Symbol interning
│       │   ├── reader.js           # Re-exports reader/: parse, the number parser
│       │   ├── expression_utils.js # The REPLs' doors into the reader: complete or not, the delimiting parentheses, their match
│       │   ├── printer.js          # prettyPrint: the REPLs' door into the printer (printer.scm), a value as they show it
│       │   ├── reader/             # The reader's door, and the number parser; the reader is (scheme-js reader)
│       │   │   ├── index.js        # parse(): the door into (scheme-js reader), on the library system's interpreter
│       │   │   └── number_parser.js # Number syntax with R7RS prefixes: string->number's core
│       │   ├── expand.js           # The door into (scheme-js expander): analyze(), a form into the evaluator's node
│       │   ├── assembler.js        # A core form into the evaluator's nodes
│       │   ├── syntax_object.js    # SyntaxObject, the identifier; scopes; datum walks marking scopes; ScopeBindingRegistry
│       │   ├── macro_registry.js   # The macros defined by name for the process
│       │   ├── type_check.js       # Type checking utilities for primitives
│       │   ├── number_representation.js # How a number is held -- exact integers as numbers or BigInts, inexact integers boxed (Flonum) -- and the arithmetic on numbers held so
│       │   ├── library_loader.js   # Loading, defining and importing libraries, through the Scheme + barrel (re-exports)
│       │   ├── library_registry.js # The library system's door from JavaScript: the current registry, the API
│       │   ├── source_texts.js     # The text of an inline script, kept for the source maps of what is compiled from it
│       │   └── library_seed.js     # Loads the library system at first use, on an interpreter of its own, compiled
│       ├── primitives/             # Native procedures (+, cons, etc.)
│       │   ├── index.js            # Creates global environment
│       │   ├── math.js             # Arithmetic and numeric operations
│       │   ├── list.js             # List operations (cons, car, cdr, etc.)
│       │   ├── string.js           # String operations
│       │   ├── string_class.js     # SchemeString: a string that may be changed, holding a JS string until it is
│       │   ├── vector.js           # Vector operations
│       │   ├── control.js          # map, call/cc, eval, dynamic-wind
│       │   ├── apply.js            # apply and %values->list, which compiled call-with-values calls through the runtime
│       │   ├── char.js             # Character predicates and operations
│       │   ├── complex.js          # Complex number support
│       │   ├── rational.js         # Rational number support
│       │   ├── process_context.js  # exit, command-line, etc.
│       │   ├── time.js             # current-second, current-jiffy
│       │   ├── bytevector.js       # Bytevector operations (R7RS §6.9)
│       │   ├── class.js            # define-class support
│       │   ├── io/                 # Port system and I/O primitives
│       │   │   ├── index.js        # Barrel export
│       │   │   ├── ports.js        # Port base classes
│       │   │   ├── primitives.js   # Scheme binding definitions
│       │   │   ├── file_port.js    # File ports
│       │   │   ├── string_port.js  # String ports
│       │   │   ├── stdin_port.js   # The port over standard input (Node.js), read synchronously
│       │   │   ├── stdout_port.js  # The ports over standard output and error (Node.js), written synchronously
│       │   │   ├── console_port.js # Console ports
│       │   │   ├── bytevector_port.js # Bytevector ports
│       │   │   └── printer.js      # The printer's door: its text for JavaScript (writeString, the REPLs'), and what only JavaScript can say of a value
│       │   ├── eq.js               # Equality predicates (eq?, eqv?, boolean=?)
│       │   ├── record.js           # define-record-type support, and a record's type and fields for Scheme that looks inside any record
│       │   ├── exception.js        # Exception handling primitives
│       │   ├── interop.js          # JavaScript interop utilities
│       │   ├── async.js            # Async primitives (delay-resolve, etc.)
│       │   ├── library.js          # What the library system needs of the host: resolver, reader, environments, keyword tables
│       │   ├── reader_support.js   # What (scheme-js reader) needs: read errors, literal strings, the datum-label note, whole-text scans
│       │   ├── expander_support.js # What (scheme-js expander) needs: identifiers, scopes, the keyword tables, a procedural macro's evaluation
│       │   └── gc.js               # GC-related utilities
│       │
│       └── scheme/                 # Core Scheme subset (base library)
│           ├── base.sld            # (scheme base) library declaration
│           ├── core.sld            # (scheme core) library declaration
│           ├── control.sld         # (scheme control) library declaration
│           ├── cxr.sld             # (scheme cxr) library declaration
│           ├── char.sld            # (scheme char) library declaration
│           ├── write.sld           # (scheme write) library declaration
│           ├── read.sld            # (scheme read) library declaration
│           ├── file.sld            # (scheme file) library declaration
│           ├── repl.sld            # (scheme repl) library declaration
│           ├── complex.sld         # (scheme complex) library declaration
│           ├── inexact.sld         # (scheme inexact) library declaration
│           ├── eval.sld            # (scheme eval) library declaration
│           ├── lazy.sld            # (scheme lazy) library declaration
│           ├── process-context.sld # (scheme process-context)
│           ├── time.sld            # (scheme time) library declaration
│           ├── macros.scm          # Core macros: and, let, letrec, cond; syntax-error, include, include-ci
│           ├── equality.scm        # Deep equality: equal?
│           ├── cxr.scm             # All 28 cxr accessors
│           ├── numbers.scm         # Variadic comparisons, predicates, min/max
│           ├── list.scm            # map, for-each, memq, assq, length, etc.
│           ├── library-system.sld  # (scheme-js library-system): the library system
│           ├── library_system.scm  # define-library, import sets, cond-expand, registries, loading, importing, closures run compiled for a debugger
│           ├── debugger.sld        # (scheme-js debugger): the debugger's logic, loaded beside the library system
│           ├── debugger.scm        # breakpoints, the calls a program is in, stepping, exceptions, the REPL's commands
│           ├── reader.sld          # (scheme-js reader): text into data, with spans
│           ├── reader.scm          # a character-level recursive descent; dot notation, object literals, directives, datum labels
│           ├── expander.sld        # (scheme-js expander): forms into core forms, which it lists
│           ├── expander.scm        # environments, keywords, the special forms, bodies, quasiquote, define-syntax and define-macro
│           ├── syntax_rules.scm    # syntax-rules: matching and transcribing, hygiene by marks
│           ├── explicit_renaming.scm # er-macro-transformer: rename and compare; define-macro is one
│           ├── special-forms.sld   # (scheme-js special-forms): the special forms, as keywords a library imports
│           ├── control.scm         # when, unless, or, let*, do, case, guard
│           ├── parameter.scm       # make-parameter, parameterize
│           ├── ports.scm           # The current ports, reading and writing them, call-with-port, the file procedures
│           ├── printer.scm         # write, display, write-shared, write-simple: a datum's text, datum labels; the REPLs' text
│           └── repl.scm            # REPL utilities
│
│   └── compiler/              # Scheme -> JavaScript compiler tier (Stage 2b)
│      ├── index.js           # EXPORT: tryCompileDefinition(), tryCompileExpression(), compileProgram(); hands each to driver.scm
│      ├── compiler.sld       # (scheme-js compiler): its imports, files and entry points
│      ├── ir.scm             # Analyzed AST -> IR, in Scheme
│      ├── emit.scm           # IR -> JavaScript, in Scheme: both forms of a procedure
│      ├── lift.scm           # Which nested procedures are emitted once, at top level
│      ├── liveness.scm       # Which locals a suspended frame saves
│      ├── sourcemap.scm      # Source maps: each line of generated code to the Scheme it came from
│      ├── inline.scm         # Inline expansions for primitives, tower-faithful
│      ├── driver.scm         # What to compile, and each reason not: definitions, expressions, closures, environments, programs
│      ├── safety.scm         # The opt-in rule declining what a capture could unwind through
│      ├── tier.scm           # A program's own code compiled as it runs: when, installing it, switching re-entered ones back
│      ├── host.js            # (scheme-js compiler host): new Function, the interpreter's structures, weak tables
│      ├── build_host.js      # (scheme-js compiler build), the CLI's only: private registries, loading, expanding, installing generated code
│      ├── lowering.js        # Door into the compiler's Scheme: starts its library, hands out its entry points
│      ├── prebuilt.js        # Installing each library's code compiled at build time, fingerprinted; restoring it, its data decoded from JSON
│      ├── tiering.js         # Attaching the tier: makes its record, whose Scheme procedures the interpreter calls
│      └── runtime.js         # Tail-call step, stack room and flush, global cells, vector helpers, non-procedure report, procedure marking
│
│   └── debug/                  # The debugger's doors and backends; its logic is (scheme-js debugger)
│      ├── index.js            # Barrel export
│      ├── scheme_debug_runtime.js # The evaluator's hooks, each a call into (scheme-js debugger); the paused run's promise
│      ├── debug_backend.js    # Abstract backend interface
│      ├── repl_debug_backend.js # REPL-specific backend adapter
│      ├── repl_debug_commands.js # Hands :commands to the Scheme; evaluates :eval in a frame
│      └── instrumentation.js  # Deterministic evaluator step counting
│
│   └── extras/                     # Extension libraries (non-R7RS)
│       ├── primitives/             # JavaScript primitives for extensions
│       │   ├── interop.js          # JS interop: js-eval, js-ref, js-set!
│       │   ├── promise.js          # Promise interop primitives
│       │   ├── hash_table.js       # Map-backed store under SRFI 125; native hash functions
│       │   └── bitwise.js          # BigInt operators under SRFI 151
│       └── scheme/                 # Scheme library files
│           ├── procedural-macros.sld # (scheme-js procedural-macros): er-macro-transformer, define-macro
│           ├── promise.sld         # (scheme-js promise) library declaration
│           ├── promise.scm         # Promise utilities and macros
│           ├── 125.sld             # (srfi 125) hash tables
│           ├── hash_table.scm      # SRFI 125 implementation
│           ├── 128.sld             # (srfi 128) comparators
│           ├── comparator.scm      # SRFI 128 implementation
│           ├── 1.sld               # (srfi 1) lists
│           ├── list_lib.scm        # SRFI 1 implementation; the compiler imports (srfi 1)
│           ├── 151.sld             # (srfi 151) bitwise operations
│           ├── bitwise.scm         # SRFI 151 implementation; the compiler's liveness sets are its bits
│           ├── 152.sld             # (srfi 152) strings
│           └── string_lib.scm      # SRFI 152 implementation; the compiler imports (srfi 152)
│
│   ├── harness/                    # Test infrastructure
│   │   ├── helpers.js              # Test utilities (run, assert, createTestLogger)
│   │   ├── runner.js               # Test runner logic
│   │   ├── standard_library.js     # The standard library interpreted at top level
│   │   ├── cli_process.js          # Runs `repl.js` in a child process, for the CLI's tests
│   │   ├── page_libraries.js       # Libraries loaded as a page loads them, shipped ones restored from their tables: for run_tier.js, the tiered tests, the compiled conformance run
│   │   ├── compiler_failures.js    # The compiler's own failures, taken for the suite, the canonical harness and run_tier.js to fail on
│   │   └── scheme_test.scm         # Scheme test harness
│   │
│   ├── test_manifest.js            # Central registry of all test files
│   ├── run_all.js                  # Node.js test runner entry (Unit + Functional)
│   ├── run_scheme_tests.js         # Node.js Scheme test runner CLI
│   ├── run_scheme_tests_lib.js     # Shared Scheme test runner logic
│   ├── run_compiler_scheme_tests_lib.js # Runs compiler/ tests in the compiler library's environment
│   ├── run_tiered_scheme_tests_lib.js # Runs tiers/ tests twice, set up as a page is, libraries restored: program interpreted, then compiled by the tier
│   ├── compiler/                   # Scheme tests of the compiler's own Scheme
│   ├── tiers/                      # Scheme tests whose code runs in both tiers: JavaScript calling Scheme, when the tier compiles, arity errors, continuations
│   ├── test_bundle.js              # Integration tests for bundled artifact
│   ├── test_script.scm             # Scheme script test for HTML adapter
│   │
│   ├── core/                       # Tests for src/core/
│   │   ├── interpreter/            # Tests for interpreter modules
│   │   │   ├── unit_tests.js
│   │   │   ├── reader/             # Reader submodule tests
│   │   │   ├── reader_tests.js
│   │   │   ├── expression_utils_tests.js # The browser REPL's input: complete or not, matching parentheses
│   │   │   ├── nodes_tests.js      # AST node behavior tests
│   │   │   ├── frames_tests.js     # Continuation frame tests
│   │   │   ├── primitives_tests.js
│   │   │   ├── winders_tests.js
│   │   │   ├── syntax_object_tests.js # Hygiene and scope tests
│   │   │   ├── data_tests.js
│   │   │   ├── error_tests.js
│   │   │   ├── interpreter_tests.js # Top-level interpreter logic
│   │   │   └── state_isolation_tests.js # Multi-context isolation tests
│   │   │
│   │   ├── primitives/             # Tests for primitives
│   │   │   └── io/                 # I/O unit tests
│   │   │       ├── string_port_tests.js
│   │   │       ├── file_port_tests.js
│   │   │       ├── stdin_port_tests.js # Reads split across characters and line endings; waiting for input
│   │   │       ├── stdout_port_tests.js # When a buffered write reaches its descriptor; waiting for room in a pipe
│   │   │       ├── console_port_tests.js
│   │   │       ├── bytevector_port_tests.js
│   │   │       └── printer_tests.js
│   │   │
│   │   └── scheme/                 # Scheme-based tests
│   │       ├── test.scm            # Scheme test harness
│   │       ├── primitive_tests.scm # Core primitives
│   │       ├── boot_tests.scm      # Environment bootstrap
│   │       ├── tco_tests.scm       # Tail call optimization
│   │       ├── dynamic_wind_tests.scm
│   │       ├── exception_tests.scm
│   │       ├── hygiene_tests.scm    # Basic hygiene
│   │       ├── macro_hygiene_tests.scm # Advanced hygiene suite
│   │       ├── library_macro_tests.scm # A library's macros reach its own bindings; libraries' macros of one name kept apart
│   │       ├── keyword_rename_tests.scm # Syntactic keywords renamed and prefixed on import and export
│   │       ├── syntax_rules_vector_tests.scm # Vector patterns and templates
│   │       ├── datum_label_literal_tests.scm # Shared and circular literals through macros; equal? on cycles
│   │       ├── parameter_tests.scm
│   │       ├── number_tests.scm     # Numeric tower (r7rs)
│   │       ├── list_tests.scm       # List library (r7rs)
│   │       ├── record_tests.scm     # Record types
│   │       ├── eval_tests.scm       # eval and environment
│   │       ├── repl_tests.scm
│   │       ├── cond_expand_tests.scm # cond-expand expression tests
│   │       └── compliance/         # R7RS conformance tests
│   │           ├── compliance_suite.js     # Runs a suite, library interpreted or compiled
│   │           ├── compliance_tests.js     # Both suites, both configurations, in npm test
│   │           ├── compliance_cli.js       # Command-line runs (--compiled, file filters)
│   │           ├── run_chibi_tests.js      # Node.js runner for Chibi
│   │           ├── run_chapter_tests.js    # Node.js runner for chapters
│   │           ├── chibi_ui.html           # Browser UI for Chibi suite (?compiled)
│   │           ├── chapter_ui.html         # Browser UI for chapter tests (?compiled)
│   │           ├── chapter_3.scm           # Basic concepts tests
│   │           ├── chapter_4.scm           # Expressions tests
│   │           ├── chapter_5.scm           # Program structure tests
│   │           ├── chapter_6.scm           # Standard procedures tests
│   │           └── chibi_revised/          # Chibi's tests by section, as Chibi wrote them; test-equal.scm gives Chibi's test forms
│   │               └── sections/           # Individual section files
│   │
│   ├── fuzz/                       # Differential fuzzer: generated programs, both tiers
│   │   ├── program_generator.scm       # Builds a program, and what to compile, from a seed
│   │   ├── fuzz_harness.js             # Runs a program interpreted and compiled
│   │   ├── differential_fuzz_tests.js  # 120 fixed seeds, in npm test
│   │   └── run_fuzz.js                 # Longer runs from the command line
│   │
│   ├── functional/                 # Cross-cutting integration tests
│   │   ├── core_tests.js
│   │   ├── interop_tests.js
│   │   ├── macro_tests.js
│   │   ├── hygiene_tests.js
│   │   ├── io_tests.js
│   │   ├── cli_stdin_tests.js      # `node repl.js` programs reading piped input; the REPL unaffected
│   │   ├── cli_stdout_tests.js     # What they write: when, in what order, and to which stream
│   │   ├── cli_repl_input_tests.js # The interactive REPL continuing an expression over lines
│   │   ├── cli_build_tests.js      # What a build step run from the CLI is given: -I, the compiler's library, the build's doors
│   │   ├── string_tests.js
│   │   ├── string_interop_tests.js # Mutable strings at the JavaScript boundary, both tiers
│   │   ├── vector_tests.js
│   │   ├── char_tests.js
│   │   ├── tiering_tests.js        # When the tier compiles a program's procedures, and what it leaves
│   │   ├── tiered_interop_tests.js # The interop suites the tier compiles code from, again with it attached
│   │   ├── capture_policy_tests.js # Captures compiled; re-entered procedures switched back to closures
│   │   ├── native_unwind_tests.js  # Which unwinds the driver finishes, which it takes by a jump, which go to the interpreter
│   │   └── ...
│   │
│   ├── integration/                # Library system tests
│   │   ├── library_loader_tests.js
│   │   └── cond_expand_library_tests.js # cond-expand in libraries
│   │
│   └── unit/                       # Unit tests of tools around the interpreter
│       ├── repl_debug_commands_tests.js # The REPLs' debug commands
│       ├── repl_parens_tests.js    # The browser REPL colouring and indenting by delimiter parentheses
│       ├── r7rs_compare_tests.js   # The benchmark harness's arithmetic
│       └── page_libraries_tests.js # Loading libraries as a page does: restored, stale, not shipped
│
├── docs/
│   ├── core-interpreter-implementation.md               # Execution model details
│   ├── Interoperability.md         # JS/Scheme interop design
│   ├── hygeine.md                  # Macro hygiene notes
│   ├── macro_debugging.md          # Macro troubleshooting guide
│   ├── architecture.md             # High-level architecture
│   ├── corpus_decline_results.md   # Why the compiler tier declines procedures in real R7RS code
│   └── REFERENCES.md               # Academic references
│
└── web/
    ├── ui.html                     # Browser REPL + test runner
    ├── main.js                     # Browser entry point
    └── repl.js                     # REPL UI logic; colours and indents by the reader's delimiter parentheses
```

### Key Principles

1. **Two-Tier Model**: JavaScript provides the core; Scheme provides libraries.
2. **`src/core/`**: Everything needed to run basic Scheme (JS interpreter + core Scheme subset).
3. **`src/lib/`**: (Future) Additional R7RS libraries built on-top of the core.
4. **Tests mirror source**: `tests/core/` tests `src/core/`.
5. **Split Stepables**: AST nodes in `ast_nodes.js`, frames in `frames.js`, shared base in `stepables_base.js`.
6. **The library system is Scheme**: `(scheme-js library-system)` parses libraries, keeps the registries, loads and imports libraries, and keeps the closures run compiled for a debugger to run as themselves. Its seed (`library_seed.js`) loads it, with `(scheme core)` and `(scheme control)`, from the bundled sources onto an interpreter of its own, apart from every program, installing their prebuilt tables so that it runs compiled; `library_registry.js` and `library_loader.js` are the JavaScript API, which calls it; `primitives/library.js` is what it needs of the host. A library's environment, and one `environment` makes, holds its imports and nothing else (R7RS 5.6.1): it has no parent (`makeScopedEnvironment` in `primitives/library.js`), the primitives reach it through `(scheme primitives)`, which exports every one, and a macro is found by name only where something imported it. A name bound nowhere still falls back to JavaScript's globals, as it does in a program. A program -- a CLI file, `-e` code, a page's script -- that begins with `import` declarations runs in such an environment too: the CLI and the page start-up ask `programEnvironment` (`library_loader.js`), which takes the program apart with the library system's `program-parts`, for the environment and the forms to run there, and run each with `runProgramForm`, under the environment's scope. A program with none runs in the interaction environment, which sees everything. The library system reads every library's files with the reader, `(scheme-js reader)`, and expands every form with the expander, `(scheme-js expander)`, both of which the seed loads before it; a library whose prebuilt table is current is loaded with no file read and no form expanded, the table holding its `define-library` form and its top-level forms as compiled procedures and core forms -- a macro's definition as one that binds it pending, its transformer made the first time it is used -- and the seed reads and expands its own libraries, when their tables are stale, with the pinned seed (`src/packaging/pinned_seed.js`), the reader's and the expander's libraries as core forms.
7. **The expander is Scheme**: `(scheme-js expander)` turns a form into a core form, a tagged list `expander.sld` lists, which `assembler.js` turns into the evaluator's nodes; `expand.js` is the door, `analyze`. It is one of the library system's seed libraries, loaded on its interpreter, and reaches the tables a form's meaning depends on -- scopes, the keywords bound in each library and at the top level, the macros defined for the process -- through `primitives/expander_support.js`, as the library system does. Its `syntax-rules` is `syntax_rules.scm`; identifiers are `SyntaxObject`s (`syntax_object.js`), a value representation.
8. **Minimal Bootstrap**: Scheme libraries define what's needed to load `(scheme base)`.
9. **Self-hosting where it pays**: the compiler's lowering pass is Scheme, and the
   interpreter is what bootstraps it — so the tier's own performance is the
   project's performance, and no second language is needed to build the first.

## Related Documentation

- [core-interpreter-implementation.md](core-interpreter-implementation.md) — Execution model details
- [hygiene.md](hygiene.md) — Macro hygiene algorithm (pure marks)
- [macro_debugging.md](macro_debugging.md) — Troubleshooting common macro issues
- [ROADMAP.md](../ROADMAP.md) — Implementation progress
