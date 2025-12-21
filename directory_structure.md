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
│       │   ├── values.js           # Closure, Continuation, TailCall, Values
│       │   ├── cons.js             # Cons cells + list utilities
│       │   ├── symbol.js           # Symbol interning
│       │   ├── reader.js           # S-expression parser
│       │   ├── analyzer.js         # S-exp → AST conversion
│       │   ├── syntax_rules.js     # syntax-rules transformer
│       │   ├── macro_registry.js   # Macro registry
│       │   ├── library.js          # Micro-library system
│       │   ├── library_loader.js   # R7RS define-library / import
│       │   └── analysis/           # Syntactic analysis modules
│       │
│       ├── primitives/             # Native procedures (+, cons, etc.)
│       │   ├── index.js
│       │   ├── math.js
│       │   ├── list.js
│       │   ├── string.js
│       │   ├── vector.js
│       │   ├── control.js
│       │   ├── char.js              # Character predicates and operations
│       │   └── io.js                # Port system and I/O operations
│
│       └── scheme/                 # Core Scheme subset (base library)
│           ├── base.sld            # (scheme base) library declaration
│           ├── core.sld            # (scheme core) library declaration
│           ├── control.sld         # (scheme control) library declaration
│           ├── cxr.sld             # (scheme cxr) library declaration
│           ├── char.sld            # (scheme char) library declaration
│           ├── write.sld           # (scheme write) library declaration
│           ├── read.sld            # (scheme read) library declaration
│           ├── macros.scm          # Core macros: and, let, letrec, cond
│           ├── equality.scm        # Deep equality: equal?
│           ├── cxr.scm             # All 28 cxr accessors
│           ├── numbers.scm         # Variadic comparisons, predicates, min/max
│           ├── list.scm            # map, for-each, memq, assq, length, etc.
│           └── control.scm         # when, unless, or, let*, do, case, guard
│
├── tests/
│   ├── core/                       # Tests for src/core/
│   │   ├── interpreter/            # Tests for interpreter modules
│   │   │   ├── unit_tests.js
│   │   │   ├── reader_tests.js
│   │   │   ├── analyzer_tests.js
│   │   │   └── ...
│   │   ├── primitives/             # Tests for primitives (empty)
│   │   └── scheme/                 # Scheme tests
│   │       ├── test.scm            # Test harness
│   │       ├── primitive_tests.scm
│   │       ├── boot_tests.scm
│   │       └── ...
│   │
│   ├── functional/                 # Cross-cutting integration tests
│   │   ├── core_tests.js
│   │   ├── interop_tests.js
│   │   └── ...
│   │
│   ├── integration/                # Library system tests
│   └── lib/                        # Tests for src/lib/ (future)
│
├── docs/
│   ├── trampoline.md               # Execution model details
│   └── ...
│
└── web/
    ├── ui.html                     # Browser REPL + test runner
    └── main.js                     # Browser entry point
```

## Key Principles

1. **Two-Tier Model**: JavaScript provides the core; Scheme provides libraries.
2. **`src/core/`**: Everything needed to run basic Scheme (JS interpreter + core Scheme subset).
3. **`src/lib/`**: (Future) Additional R7RS libraries built on-top of the core.
4. **Tests mirror source**: `tests/core/` tests `src/core/`.
5. **Unified Stepables**: All executable objects (AST nodes + frames) in `stepables.js`.
6. **Minimal Bootstrap**: `base.scm` defines only what's needed to load `(scheme base)`.
