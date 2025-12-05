# Directory Structure

To ensure strict separation of concerns and enable incremental testing, the source code is organized into numbered layers.

### Layered Directory Structure

```text
/
├── src/
│   ├── layer-1-kernel/         # THE KERNEL
│   │   ├── index.js            # EXPORT: createLayer1()
│   │   ├── interpreter.js      # The core trampoline loop
│   │   ├── ast.js              # Base AST nodes
│   │   ├── library.js          # The "Micro-Library" registry
│   │   └── primitives/         # Base JS natives (+, display, etc.)
│   ├── layer-1-kernel/       # Layer 1: Syntactic Foundation & Core Data
│   │   ├── scheme/           # Scheme boot code
│   │   │   └── boot.scm      # Core macros (and, let, cond)
│   │   ├── primitives/       # JS implementations of primitives
│   │   ├── syntax/           # Reader, Analyzer, Macro Expander
│   │   ├── data/             # Cons, Symbol, Vector, Record classes
│   │   ├── ast.js            # AST Node definitions
│   │   ├── environment.js    # Environment class
│   │   ├── interpreter.js    # Core Interpreter Loop
│   │   └── index.js          # Layer 1 Factory
│   │
│   ├── layer-2-syntax/         # SYNTACTIC ABSTRACTION
│   │   ├── index.js            # EXPORT: createLayer2()
│   │   ├── expander.js         # JS logic for syntax-rules
│   │   └── syntax.scm          # (define-library (scheme syntax) ...)
│   │
│   ├── layer-3-data/           # DATA STRUCTURES
│   │   ├── index.js            # EXPORT: createLayer3()
│   │   ├── cons.js             # JS class for Pairs
│   │   ├── symbol.js           # JS class for Interned Symbols
│   │   └── data.scm            # (define-library (scheme data) ...)
│   │
│   └── layer-4-stdlib/         # STANDARD LIBRARY
│       ├── index.js            # EXPORT: createLayer4()
│       └── base.scm            # (define-library (scheme base) ...)
│
├── tests/
│   ├── runner.js               # Universal Test Runner
│   ├── layer-1/                # Tests for Kernel only
│   ├── layer-2/                # Tests for Macros
│   └── layer-3/                # Tests for Cons/Lists
│
└── web/
    ├── index.html
    └── main.js                 # UI Entry point (targets latest layer)
```

### Key Principles

1.  **Strict Dependency Flow**: Higher numbered layers depend on lower numbered layers. Layer 1 knows nothing about Layer 2.
2.  **Factory Pattern**: Each layer exports a `createLayerN()` function that builds upon the previous layer.
3.  **Isolated Testing**: The `tests/runner.js` can target a specific layer (e.g., `node tests/runner.js 1`) to verify it in isolation.
4.  **Library Separation**:
    *   **JS Primitives**: Go in `src/layer-N/primitives/` or `src/layer-N/filename.js`.
    *   **Scheme Libraries**: Go in `src/layer-N/filename.scm` and use `(define-library ...)`.
