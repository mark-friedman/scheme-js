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

## JavaScript Runtime Components

| Component | Purpose |
|-----------|---------|
| `interpreter.js` | Trampoline execution loop |
| `stepables_base.js` | Register constants + `Executable` base class |
| `ast_nodes.js` | AST node classes (Literal, If, Lambda...) |
| `frames.js` | Continuation frame classes |
| `reader.js` | S-expression parser |
| `analyzer.js` | S-exp → AST conversion |
| `library_registry.js` | Feature + library registries |
| `library_parser.js` | define-library parser |
| `library_loader.js` | Library loading orchestration |
| `syntax_rules.js` | Macro transformer + hygiene primitives |
| `primitives/` | Native procedures |

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

## Directory Structure

```
src/core/
├── interpreter/        # JavaScript interpreter
│   ├── stepables_base.js
│   ├── ast_nodes.js
│   ├── frames.js
│   ├── stepables.js    # Barrel file
│   ├── library_registry.js
│   ├── library_parser.js
│   ├── library_loader.js
│   └── primitives/
└── scheme/             # Core Scheme libraries
    ├── base.sld
    └── ...

tests/
├── harness/            # Test infrastructure
├── core/               # Tests for interpreter/scheme
├── functional/         # Cross-cutting tests
└── integration/        # Library system tests
```

## Related Documentation

- [directory_structure.md](../directory_structure.md) — Detailed file listing
- [trampoline.md](trampoline.md) — Execution model details
- [hygiene_implementation.md](hygiene_implementation.md) — Macro hygiene algorithm
- [r7rs_roadmap.md](../r7rs_roadmap.md) — Implementation progress
