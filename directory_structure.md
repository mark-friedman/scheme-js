# Directory Structure

The codebase follows a two-tier architecture: JavaScript core and Scheme libraries.

## Current Structure

```text
/
├── src/
│   └── core/                       # The Core (JS Interpreter + Scheme subset)
│       ├── interpreter/            # JavaScript Interpreter
│       │   ├── index.js            # EXPORT: createInterpreter()
│       │   ├── interpreter.js      # Trampoline execution loop
│       │   ├── stepables.js        # AST nodes + continuation frames
│       │   ├── ast.js              # Barrel file (re-exports stepables)
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
│       │   ├── type_check.js       # Type checking utilities for primitives
│       │   ├── library.js          # Micro-library system
│       │   ├── library_loader.js   # R7RS define-library / import
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
├── tests/
│   ├── helpers.js                  # Test utilities (run, assert, createTestLogger)
│   ├── test_manifest.js            # Central registry of all test files
│   ├── run_all.js                  # Node.js test runner entry
│   ├── runner.js                   # Test runner logic
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
│   │       └── compliance/         # R7RS conformance tests (Chibi-based)
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
5. **Unified Stepables**: All executable objects (AST nodes + frames) in `stepables.js`.
6. **Minimal Bootstrap**: `base.scm` defines only what's needed to load `(scheme base)`.
