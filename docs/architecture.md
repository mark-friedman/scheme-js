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
| `nodes.js` / `frames.js` | AST nodes and continuation frames |
| `reader.js` | S-expression parser |
| `analyzer.js` | S-exp → AST conversion |
| `library_loader.js` | `define-library` / `import` / `export` |
| `syntax_rules.js` | Macro transformer + hygiene primitives |
| `primitives/` | Native procedures |

## Features Requiring JS Core

| Feature | Description |
|---------|-------------|
| Trampoline | TCO via register machine |
| `call/cc` | First-class continuations |
| `dynamic-wind` | Before/after thunk protocol |
| Multiple Values | `values` / `call-with-values` |
| Hygienic Macros | `rename` / `compare` primitives |
| Exceptions | Handler stack, `raise` |
| Ports | I/O abstraction |
| Library Loader | Module system |

## Directory Structure

```
src/
├── runtime/            # JavaScript kernel
│   ├── primitives/
│   └── boot.scm        # Pre-library bootstrap
└── lib/                # R7RS libraries
    └── scheme/
        ├── base.sld
        └── ...

tests/
├── runtime/            # JS runtime tests
├── integration/        # Full interpreter tests
└── lib/                # Per-library tests
```

## Implementation Phases

1. ✅ Documentation
2. ✅ Consolidate JS Core
3. ⏳ Library Loader
4. 🔲 Multiple Values
5. 🔲 Hygienic Macros
6. 🔲 Exceptions
7. 🔲 Ports/IO
8. 🔲 Standard Libraries
9. 🔲 Complete R7RS
