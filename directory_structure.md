# Directory Structure

The codebase follows a two-tier architecture: JavaScript core and Scheme libraries.

## Current Structure

```text
/
├── repl.js                         # Node.js REPL entry point
├── src/
│   └── core/                       # The Core (JS Interpreter + Scheme subset)
│       ├── interpreter/            # JavaScript Interpreter
│       │   ├── index.js            # EXPORT: createInterpreter()
│       │   ├── interpreter.js      # Trampoline execution loop
│       │   ├── stepables.js        # Barrel file (re-exports all stepables)
│       │   ├── stepables_base.js   # Base class + register constants
│       │   ├── ast_nodes.js        # AST node classes (Literal, If, Lambda...)
│       │   ├── frames.js           # Continuation frame classes
│       │   ├── ast.js              # Legacy barrel file
│       │   ├── frame_registry.js   # Frame factory functions
│       │   ├── winders.js          # Dynamic-wind utilities
│       │   ├── environment.js      # Environment class
│       │   ├── errors.js           # SchemeError class
│       │   ├── values.js           # Closure, Continuation, TailCall, Values
│       │   ├── cons.js             # Cons cells + list utilities
│       │   ├── symbol.js           # Symbol interning
│       │   ├── reader.js           # S-expression parser
│       │   ├── analyzer.js         # S-exp → AST conversion
│       │   ├── syntax_rules.js     # syntax-rules transformer
│       │   ├── syntax_object.js    # SyntaxObject and ScopeBindingRegistry
│       │   ├── macro_registry.js   # Macro registry
│       │   ├── identifier_utils.js # Shared identifier helpers
│       │   ├── type_check.js       # Type checking utilities for primitives
│       │   ├── library_loader.js   # Library loading + barrel (re-exports)
│       │   ├── library_registry.js # Feature + library registries
│       │   ├── library_parser.js   # define-library parser
│       │   └── analysis/           # Syntactic analysis modules
│       │
│       ├── primitives/             # Native procedures (+, cons, etc.)
│       │   ├── index.js            # Creates global environment
│       │   ├── math.js             # Arithmetic and numeric operations
│       │   ├── list.js             # List operations (cons, car, cdr, etc.)
│       │   ├── string.js           # String operations
│       │   ├── vector.js           # Vector operations
│       │   ├── control.js          # apply, map, call/cc
│       │   ├── char.js             # Character predicates and operations
│       │   ├── io.js               # Port system and I/O operations
│       │   ├── eq.js               # Equality predicates (eq?, eqv?, boolean=?)
│       │   ├── record.js           # define-record-type support
│       │   ├── exception.js        # Exception handling primitives
│       │   ├── interop.js          # JavaScript interop utilities
│       │   ├── async.js            # Async primitives (delay-resolve, etc.)
│       │   ├── bytevector.js       # Bytevector operations (R7RS §6.9)
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
│           ├── macros.scm          # Core macros: and, let, letrec, cond
│           ├── equality.scm        # Deep equality: equal?
│           ├── cxr.scm             # All 28 cxr accessors
│           ├── numbers.scm         # Variadic comparisons, predicates, min/max
│           ├── list.scm            # map, for-each, memq, assq, length, etc.
│           ├── control.scm         # when, unless, or, let*, do, case, guard
│           ├── parameter.scm       # make-parameter, parameterize
│           └── repl.scm            # REPL utilities
│
│   └── extras/                     # Extension libraries (non-R7RS)
│       ├── primitives/             # JavaScript primitives for extensions
│       │   └── promise.js          # Promise interop primitives
│       └── scheme/                 # Scheme library files
│           ├── promise.sld         # (scheme-js promise) library declaration
│           └── promise.scm         # Promise utilities and macros
│
│   ├── harness/                    # Test infrastructure
│   │   ├── helpers.js              # Test utilities (run, assert, createTestLogger)
│   │   ├── runner.js               # Test runner logic
│   │   └── scheme_test.scm         # Scheme test harness
│   │
│   ├── test_manifest.js            # Central registry of all test files
│   ├── run_all.js                  # Node.js test runner entry
│   │
│   ├── core/                       # Tests for src/core/
│   │   ├── interpreter/            # Tests for interpreter modules
│   │   │   ├── unit_tests.js
│   │   │   ├── reader_tests.js
│   │   │   ├── analyzer_tests.js
│   │   │   ├── primitives_tests.js
│   │   │   ├── winders_tests.js
│   │   │   ├── syntax_rules_tests.js
│   │   │   ├── data_tests.js
│   │   │   └── error_tests.js
│   │   │
│   │   └── scheme/                 # Scheme-based tests
│   │       ├── test.scm            # Scheme test harness
│   │       ├── primitive_tests.scm
│   │       ├── boot_tests.scm
│   │       ├── tco_tests.scm
│   │       ├── dynamic_wind_tests.scm
│   │       ├── exception_tests.scm
│   │       ├── hygiene_tests.scm
│   │       ├── parameter_tests.scm
│   │       ├── repl_tests.scm
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
│       └── library_loader_tests.js
│
├── docs/
│   ├── trampoline.md               # Execution model details
│   ├── Interoperability.md         # JS/Scheme interop design
│   ├── hygeine.md                  # Macro hygiene notes
│   ├── architecture.md             # High-level architecture
│   └── REFERENCES.md               # Academic references
│
└── web/
    ├── ui.html                     # Browser REPL + test runner
    ├── main.js                     # Browser entry point
    └── repl.js                     # REPL UI logic
```

## Key Principles

1. **Two-Tier Model**: JavaScript provides the core; Scheme provides libraries.
2. **`src/core/`**: Everything needed to run basic Scheme (JS interpreter + core Scheme subset).
3. **`src/lib/`**: (Future) Additional R7RS libraries built on-top of the core.
4. **Tests mirror source**: `tests/core/` tests `src/core/`.
5. **Split Stepables**: AST nodes in `ast_nodes.js`, frames in `frames.js`, shared base in `stepables_base.js`.
6. **Split Library Loader**: Registry in `library_registry.js`, parser in `library_parser.js`, loader logic in `library_loader.js`.
7. **Minimal Bootstrap**: Scheme libraries define what's needed to load `(scheme base)`.
