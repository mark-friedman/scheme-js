# Directory Structure

The codebase follows a two-tier architecture: JavaScript runtime and Scheme libraries.

## Target Structure

```text
/
├── src/
│   ├── runtime/                # JavaScript Kernel
│   │   ├── index.js            # EXPORT: createInterpreter()
│   │   ├── interpreter.js      # Trampoline execution loop
│   │   ├── nodes.js            # AST node classes
│   │   ├── frames.js           # Continuation frame classes
│   │   ├── winders.js          # Dynamic-wind utilities
│   │   ├── environment.js      # Environment class
│   │   ├── values.js           # Closure, Continuation, TailCall
│   │   ├── reader.js           # S-expression parser
│   │   ├── analyzer.js         # S-exp → AST conversion
│   │   ├── syntax_rules.js     # syntax-rules transformer
│   │   ├── library_loader.js   # define-library / import / export
│   │   ├── primitives/         # Native procedures (+, cons, etc.)
│   │   ├── analysis/           # Syntactic analysis modules
│   │   └── boot.scm            # Pre-library bootstrap
│   │
│   └── lib/                    # R7RS Libraries (Scheme)
│       └── scheme/
│           ├── base.sld        # (scheme base)
│           ├── write.sld       # (scheme write)
│           ├── read.sld        # (scheme read)
│           ├── char.sld        # (scheme char)
│           └── ...
│
├── tests/
│   ├── runtime/                # Unit tests for JS runtime
│   ├── integration/            # Full interpreter tests
│   └── lib/                    # Per-library Scheme tests
│       └── scheme/
│           ├── base_tests.scm
│           └── ...
│
├── docs/
│   ├── architecture.md         # This architecture overview
│   ├── trampoline.md           # Execution model details
│   └── archive/                # Deprecated documentation
│
└── web/
    ├── index.html
    └── main.js                 # Browser entry point
```

## Key Principles

1. **Two-Tier Model**: JavaScript provides the runtime; Scheme provides the libraries.
2. **Standard Modules**: Dependencies use R7RS `define-library` / `import`.
3. **Test Mirroring**: `tests/lib/` mirrors `src/lib/` structure.
4. **Minimal Bootstrap**: `boot.scm` defines only what's needed to load `(scheme base)`.
