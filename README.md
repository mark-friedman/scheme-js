# Scheme R7RS-Small on JavaScript

A layered implementation of Scheme R7RS-Small in JavaScript.

## Goals

### Language goals
- Support the R7RS-Small standard.
- Support tail call optimization.
- Support first-class continuations.
- Support maximal interoperability with JavaScript.

### Architectural goals
- Use a layered architecture to make it easy to add new features based on exisiting features from lower layers.

### Development goals
- Use a test-driven development approach.

### Documentation goals
- Document the architecture and design decisions.
- Document the code organization.
- Document the architecture and algorithm for the core interpreter.
- Document the API for each layer.
- Provide useful comments in the code. 
- Document the test suite.

## Architecture

The project is organized into strict layers to ensure separation of concerns and testability.

- **Layer 1: Kernel** (`src/runtime/`)
  - The minimal Scheme interpreter (AST, Environment, Primitives).
  - Uses native JS arrays for lists (initially) or simple Cons.
  - Factory: `createLayer1()`

- **Layer 2: Syntax** (`src/layer-2-syntax/`)
  - Adds macro expansion (`syntax-rules`).
  - Factory: `createLayer2()`

- **Layer 3: Data** (`src/layer-3-data/`)
  - Adds full Scheme data tower (complex numbers, etc.).
  - Factory: `createLayer3()`

- **Layer 4: Stdlib** (`src/layer-4-stdlib/`)
  - The standard library implemented in Scheme.
  - Factory: `createLayer4()`

## Running Tests

Use the universal test runner to test a specific layer:

```bash
# Test Layer 1 (Kernel)
node tests/runner.js 1
```

## Web Interface

Open `web/index.html` to run the REPL. It currently targets Layer 1.
