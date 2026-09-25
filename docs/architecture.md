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
(`src/packaging/scheme_compiler.js`), and fetched only by a page that asks to compile code
of its own.

## JavaScript Runtime Components

| Component | Purpose |
|-----------|---------|
| `interpreter.js` | Trampoline execution loop |
| `stepables_base.js` | Register constants + `Executable` base class |
| `ast_nodes.js` | AST node classes (Literal, If, Lambda...) |
| `frames.js` | Continuation frame classes |
| `reader.js` | S-expression parser |
| `analyzer.js` | Dispatcher for S-exp → AST conversion |
| `analyzers/` | Modular handlers for special forms |
| `library_registry.js` | Feature + library registries |
| `library_parser.js` | define-library parser |
| `library_loader.js` | Library loading orchestration |
| `syntax_rules.js` | Macro transformer + hygiene primitives |
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
├── repl.js                         # Node.js REPL entry point
├── rollup.config.js                # Rollup bundling configuration
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
│   ├── compare_r7rs.js             # Canonical suite under Gambit and Racket
│   ├── run_self_host.js            # The compiler lowering its own corpus, three ways
│   ├── run_hash_tables.js          # SRFI 125 tables and record reads under the tier
│   ├── run_codegen.js              # Targeted: one construct per code-generation decision, both tiers
│   ├── record_progress.js          # Regenerates docs/performance_progress.md
│   ├── lib/
│   │   ├── harness.js              # Shared bootstrap and timing
│   │   ├── r7rs_harness.js         # Canonical-suite protocol, sizing, calibration
│   │   ├── r7rs_worker.js          # One measurement per child process, under a budget
│   │   ├── step_counts.js          # Deterministic dispatch counting
│   │   └── progress_report.js      # Progress-document rendering
│   ├── programs/                   # Portable R7RS benchmark programs (Stage 0)
│   │   ├── manifest.js             # Sizes, expected results, categories
│   │   └── *.scm                   # fib, tak, oddeven, nqueens, ctak,
│   │                               #   contfib, btsearch, threads
│   │                               # NOTE: overfitted -- see benchmarks/r7rs/README.md
│   └── r7rs/                       # Canonical Gabriel/Gambit/Larceny suite (vendored)
│       ├── README.md               # Provenance, protocol, sizing, blocked programs
│       ├── UPSTREAM_COMMIT         # Pinned ecraven/r7rs-benchmarks revision
│       ├── manifest.js             # Workload class, sizes, status per program
│       ├── src/*.scm               # 51 programs, verbatim, plus common.scm and
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
│   ├── generate_compiled_libraries.js # Compiles every shipped library at build time
│   ├── generate_compiled_compiler.js # Compiles the compiler's own library at build time
│   ├── lib/render_prebuilt.js      # Writes a module of prebuilt tables, one per library
│   ├── audit_r7rs.js               # R7RS-small conformance audit
│   └── r7rs_identifiers.js         # Required-identifier reference list
├── src/
│   ├── packaging/                  # Bundling and distribution logic
│   │   ├── scheme_entry.js         # Core bundle entry point; installs library tables
│   │   ├── scheme_compiler.js      # The compiler, as loadCompiler() fetches it on demand
│   │   ├── scheme_repl_wc.js       # Web Component entry point
│   │   ├── html_adapter.js         # HTML script tag adapter
│   │   ├── bundled_libraries.js    # GENERATED: library sources, for the browser
│   │   ├── compiler_sources.js     # GENERATED: the compiler library's sources
│   │   ├── compiled_libraries.js   # GENERATED: each shipped library, compiled
│   │   └── compiled_compiler.js    # GENERATED: the compiler's library, compiled
│   │
│   └── core/                       # The Core (JS Interpreter + Scheme subset)
│       ├── interpreter/            # JavaScript Interpreter
│       │   ├── index.js            # EXPORT: createInterpreter()
│       │   ├── interpreter.js      # Trampoline execution loop
│       │   ├── stepables.js        # Barrel file (re-exports all stepables)
│       │   ├── stepables_base.js   # Base class + register constants
│       │   ├── ast_nodes.js        # AST node classes (Literal, If, Lambda...)
│       │   ├── frames.js           # Continuation frame classes, incl. CompiledFrame
│       │   ├── unwind.js           # Capturing a continuation across compiled code
│       │   ├── ast.js              # Legacy barrel file
│       │   ├── frame_registry.js   # Frame factory functions
│       │   ├── winders.js          # Dynamic-wind utilities
│       │   ├── environment.js      # Environment class
│       │   ├── primitive_bindings.js # Whether a primitive's name was ever rebound
│       │   ├── errors.js           # SchemeError class
│       │   ├── values.js           # Closure, Continuation, TailCall, Values
│       │   ├── cons.js             # Cons cells + list utilities
│       │   ├── symbol.js           # Symbol interning
│       │   ├── reader.js           # S-expression parser (re-exports from reader/)
│       │   ├── reader/             # Reader submodules
│       │   │   ├── index.js        # Barrel export + parse() entry
│       │   │   ├── tokenizer.js    # Tokenization
│       │   │   ├── parser.js       # Core parsing logic
│       │   │   ├── number_parser.js # Number parsing with R7RS prefixes
│       │   │   ├── dot_access.js   # JS property access syntax
│       │   │   ├── string_utils.js # String/symbol escape processing
│       │   │   ├── character.js    # Character literal parsing
│       │   │   └── datum_labels.js # Circular reference handling
│       │   ├── analyzer.js         # S-exp → AST dispatcher
│       │   ├── analyzers/          # Modular special form handlers
│       │   │   ├── index.js        # Registry initialization
│       │   │   ├── registry.js     # Central handler registry
│       │   │   ├── core_forms.js   # quote, lambda, if, define
│       │   │   ├── control_forms.js # with-exception-handler, raise
│       │   │   └── module_forms.js  # import, define-library, cond-expand
│       │   ├── syntax_rules.js     # syntax-rules transformer
│       │   ├── syntax_object.js    # SyntaxObject and ScopeBindingRegistry
│       │   ├── macro_registry.js   # Macro registry
│       │   ├── identifier_utils.js # Shared identifier helpers
│       │   ├── type_check.js       # Type checking utilities for primitives
│       │   ├── library_loader.js   # Library loading + barrel (re-exports)
│       │   ├── library_registry.js # Feature + library registries
│       │   └── library_parser.js   # define-library parser
│       ├── primitives/             # Native procedures (+, cons, etc.)
│       │   ├── index.js            # Creates global environment
│       │   ├── math.js             # Arithmetic and numeric operations
│       │   ├── list.js             # List operations (cons, car, cdr, etc.)
│       │   ├── string.js           # String operations
│       │   ├── vector.js           # Vector operations
│       │   ├── control.js          # apply, map, call/cc
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
│       │   │   ├── console_port.js # Console ports
│       │   │   ├── bytevector_port.js # Bytevector ports
│       │   │   ├── printer.js      # write/display logic
│       │   │   └── reader_bridge.js # read logic
│       │   ├── eq.js               # Equality predicates (eq?, eqv?, boolean=?)
│       │   ├── record.js           # define-record-type support
│       │   ├── exception.js        # Exception handling primitives
│       │   ├── interop.js          # JavaScript interop utilities
│       │   ├── async.js            # Async primitives (delay-resolve, etc.)
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
│           ├── eval.sld            # (scheme eval) library declaration
│           ├── lazy.sld            # (scheme lazy) library declaration
│           ├── process-context.sld # (scheme process-context)
│           ├── time.sld            # (scheme time) library declaration
│           ├── macros.scm          # Core macros: and, let, letrec, cond
│           ├── equality.scm        # Deep equality: equal?
│           ├── cxr.scm             # All 28 cxr accessors
│           ├── numbers.scm         # Variadic comparisons, predicates, min/max
│           ├── list.scm            # map, for-each, memq, assq, length, etc.
│           ├── control.scm         # when, unless, or, let*, do, case, guard
│           ├── parameter.scm       # make-parameter, parameterize
│           ├── parameter.scm       # make-parameter, parameterize
│           └── repl.scm            # REPL utilities
│
│   └── compiler/              # Scheme -> JavaScript compiler tier (Stage 2b)
│      ├── index.js           # EXPORT: tryCompileDefinition(), compileProgram()
│      ├── compiler.sld       # (scheme-js compiler): its imports, files and entry points
│      ├── ir.scm             # Analyzed AST -> IR, in Scheme
│      ├── emit.scm           # IR -> JavaScript, in Scheme: both forms of a procedure
│      ├── lift.scm           # Which nested procedures are emitted once, at top level
│      ├── liveness.scm       # Which locals a suspended frame saves
│      ├── inline.scm         # Inline expansions for primitives, tower-faithful
│      ├── lowering.js        # Door into the compiler's Scheme: loads its library, calls its entry points
│      ├── codegen.js         # Door into emit.scm, with what only the environment knows
│      ├── marshal.js         # The analyzed AST into Scheme data
│      ├── safety.js          # Which procedures a capture would unwind through
│      ├── prebuilt.js        # Installing each library's code compiled at build time, fingerprinted
│      └── runtime.js         # Tail-call step and budget, global cells, vector helpers, procedure marking
│
│   └── debug/                  # Debugger Runtime & Tools
│      ├── index.js            # Barrel export
│      ├── scheme_debug_runtime.js # Central debugger coordinator
│      ├── debug_backend.js    # Abstract backend interface
│      ├── breakpoint_manager.js # Breakpoint registry
│      ├── stack_tracer.js     # Logical stack tracking
│      ├── pause_controller.js # Stepping state machine
│      ├── state_inspector.js  # Scope & value inspection
│      ├── exception_handler.js # Error interception
│      ├── repl_debug_backend.js # REPL-specific backend adapter
│      ├── repl_debug_commands.js # REPL command parser (:break, :step, etc.)
│      └── instrumentation.js  # Deterministic evaluator step counting
│
│   └── extras/                     # Extension libraries (non-R7RS)
│       ├── primitives/             # JavaScript primitives for extensions
│       │   ├── interop.js          # JS interop: js-eval, js-ref, js-set!
│       │   ├── promise.js          # Promise interop primitives
│       │   └── hash_table.js       # Map-backed store under SRFI 125; native hash functions
│       └── scheme/                 # Scheme library files
│           ├── promise.sld         # (scheme-js promise) library declaration
│           ├── promise.scm         # Promise utilities and macros
│           ├── 125.sld             # (srfi 125) hash tables
│           ├── hash_table.scm      # SRFI 125 implementation
│           ├── 128.sld             # (srfi 128) comparators
│           ├── comparator.scm      # SRFI 128 implementation
│           ├── 1.sld               # (srfi 1) lists
│           ├── list_lib.scm        # SRFI 1 implementation; the compiler imports (srfi 1)
│           ├── 152.sld             # (srfi 152) strings
│           └── string_lib.scm      # SRFI 152 implementation; the compiler imports (srfi 152)
│
│   ├── harness/                    # Test infrastructure
│   │   ├── helpers.js              # Test utilities (run, assert, createTestLogger)
│   │   ├── runner.js               # Test runner logic
│   │   ├── standard_library.js     # The standard library interpreted at top level
│   │   └── scheme_test.scm         # Scheme test harness
│   │
│   ├── test_manifest.js            # Central registry of all test files
│   ├── run_all.js                  # Node.js test runner entry (Unit + Functional)
│   ├── run_scheme_tests.js         # Node.js Scheme test runner CLI
│   ├── run_scheme_tests_lib.js     # Shared Scheme test runner logic
│   ├── run_compiler_scheme_tests_lib.js # Runs compiler/ tests in the compiler library's environment
│   ├── compiler/                   # Scheme tests of the compiler's own Scheme
│   ├── test_bundle.js              # Integration tests for bundled artifact
│   ├── test_script.scm             # Scheme script test for HTML adapter
│   │
│   ├── core/                       # Tests for src/core/
│   │   ├── interpreter/            # Tests for interpreter modules
│   │   │   ├── unit_tests.js
│   │   │   ├── reader/             # Reader submodule tests
│   │   │   ├── reader_tests.js
│   │   │   ├── nodes_tests.js      # AST node behavior tests
│   │   │   ├── frames_tests.js     # Continuation frame tests
│   │   │   ├── primitives_tests.js
│   │   │   ├── winders_tests.js
│   │   │   ├── syntax_rules_tests.js
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
│   │       ├── parameter_tests.scm
│   │       ├── number_tests.scm     # Numeric tower (r7rs)
│   │       ├── list_tests.scm       # List library (r7rs)
│   │       ├── record_tests.scm     # Record types
│   │       ├── eval_tests.scm       # eval and environment
│   │       ├── repl_tests.scm
│   │       ├── cond_expand_tests.scm # cond-expand expression tests
│   │       └── compliance/         # R7RS conformance tests
│   │           ├── chibi_ui.html           # Browser UI for Chibi suite
│   │           ├── chibi_runner_lib.js     # Chibi test runner library
│   │           ├── run_chibi_tests.js      # Node.js runner for Chibi
│   │           ├── chapter_ui.html         # Browser UI for chapter tests
│   │           ├── chapter_runner_lib.js   # Chapter test runner library
│   │           ├── run_chapter_tests.js    # Node.js runner for chapters
│   │           ├── chapter_3.scm           # Basic concepts tests
│   │           ├── chapter_4.scm           # Expressions tests
│   │           ├── chapter_5.scm           # Program structure tests
│   │           ├── chapter_6.scm           # Standard procedures tests
│   │           └── chibi_revised/          # Chibi-based section tests
│   │               └── sections/           # Individual section files
│   │
│   ├── functional/                 # Cross-cutting integration tests
│   │   ├── core_tests.js
│   │   ├── interop_tests.js
│   │   ├── macro_tests.js
│   │   ├── hygiene_tests.js
│   │   ├── io_tests.js
│   │   ├── string_tests.js
│   │   ├── vector_tests.js
│   │   ├── char_tests.js
│   │   └── ...
│   │
│   └── integration/                # Library system tests
│       ├── library_loader_tests.js
│       └── cond_expand_library_tests.js # cond-expand in libraries
│
├── docs/
│   ├── core-interpreter-implementation.md               # Execution model details
│   ├── Interoperability.md         # JS/Scheme interop design
│   ├── hygeine.md                  # Macro hygiene notes
│   ├── macro_debugging.md          # Macro troubleshooting guide
│   ├── architecture.md             # High-level architecture
│   └── REFERENCES.md               # Academic references
│
└── web/
    ├── ui.html                     # Browser REPL + test runner
    ├── main.js                     # Browser entry point
    └── repl.js                     # REPL UI logic
```

### Key Principles

1. **Two-Tier Model**: JavaScript provides the core; Scheme provides libraries.
2. **`src/core/`**: Everything needed to run basic Scheme (JS interpreter + core Scheme subset).
3. **`src/lib/`**: (Future) Additional R7RS libraries built on-top of the core.
4. **Tests mirror source**: `tests/core/` tests `src/core/`.
5. **Split Stepables**: AST nodes in `ast_nodes.js`, frames in `frames.js`, shared base in `stepables_base.js`.
6. **Split Library Loader**: Registry in `library_registry.js`, parser in `library_parser.js`, loader logic in `library_loader.js`.
7. **Modular Analyzer**: `analyzer.js` acts as a dispatcher to themed handlers in `analyzers/`, ensuring the analysis phase is extensible and isolated.
8. **Minimal Bootstrap**: Scheme libraries define what's needed to load `(scheme base)`.
9. **Self-hosting where it pays**: the compiler's lowering pass is Scheme, and the
   interpreter is what bootstraps it — so the tier's own performance is the
   project's performance, and no second language is needed to build the first.

## Related Documentation

- [core-interpreter-implementation.md](core-interpreter-implementation.md) — Execution model details
- [hygiene.md](hygiene.md) — Macro hygiene algorithm (pure marks)
- [macro_debugging.md](macro_debugging.md) — Troubleshooting common macro issues
- [ROADMAP.md](../ROADMAP.md) — Implementation progress
