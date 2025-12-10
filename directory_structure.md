# Directory Structure

The codebase follows a two-tier architecture: JavaScript runtime and Scheme libraries.

## Current Structure

```text
/
├── src/
│   ├── runtime/                # JavaScript Kernel (Layer 1)
│   │   ├── index.js            # EXPORT: createLayer1()
│   │   ├── interpreter.js      # Trampoline execution loop
│   │   ├── stepables.js        # AST nodes + continuation frames
│   │   ├── ast.js              # Barrel file (re-exports stepables)
│   │   ├── frame_registry.js   # Frame factory functions
│   │   ├── winders.js          # Dynamic-wind utilities
│   │   ├── environment.js      # Environment class
│   │   ├── values.js           # Closure, Continuation, TailCall, Values
│   │   ├── cons.js             # Cons cells + list utilities
│   │   ├── symbol.js           # Symbol interning
│   │   ├── reader.js           # S-expression parser
│   │   ├── analyzer.js         # S-exp → AST conversion
│   │   ├── syntax_rules.js     # syntax-rules transformer
│   │   ├── macro_registry.js   # Macro registry
│   │   ├── library.js          # Micro-library system
│   │   ├── library_loader.js   # R7RS define-library / import
│   │   ├── analysis/           # Syntactic analysis modules
│   │   ├── primitives/         # Native procedures (+, cons, etc.)
│   │   └── scheme/
│   │       └── boot.scm        # Pre-library bootstrap
│   │
│   └── lib/                    # R7RS Libraries (Scheme)
│       └── scheme/
│           ├── base.sld        # (scheme base)
│           └── ...
│
├── tests/
│   ├── run_all.js              # Main test runner
│   ├── helpers.js              # Test utilities
│   ├── test_manifest.js        # Test module registry
│   ├── unit/                   # Unit tests for JS modules
│   ├── functional/             # Functional tests (TCO, call/cc, etc.)
│   ├── integration/            # Integration tests (library loader)
│   ├── runtime/                # Runtime-specific tests
│   └── scheme/                 # Scheme test files (.scm)
│
├── docs/
│   ├── trampoline.md           # Execution model details
│   └── ...
│
└── web/
    ├── ui.html                 # Browser REPL + test runner
    └── main.js                 # Browser entry point
```

## Key Principles

1. **Two-Tier Model**: JavaScript provides the runtime; Scheme provides the libraries.
2. **Standard Modules**: Dependencies use R7RS `define-library` / `import`.
3. **Unified Stepables**: All executable objects (AST nodes + frames) in single `stepables.js`.
4. **Minimal Bootstrap**: `boot.scm` defines only what's needed to load `(scheme base)`.
