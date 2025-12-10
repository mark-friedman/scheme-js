# Scheme R7RS-Small on JavaScript

A faithful, layered implementation of the **Scheme R7RS-Small** standard in JavaScript, designed for correctness, extensibility, and deep JavaScript interoperability.

## 🎯 Goals

### Language Goals
- **R7RS-Small Compliance**: Strictly follow the standard.
- **Tail Call Optimization (TCO)**: Proper handling of tail recursion (even when interleaved with JS) using a trampoline architecture.
- **First-Class Continuations**: Full support for `call/cc`, including `dynamic-wind` and multiple return values.
- **JavaScript Interop**: Seamless calling between Scheme and JavaScript, including shared data structures and transparent boundary crossing.

### Architectural Goals
- **Layered Design**: Build complex features (macros, data structures) on top of a minimal, robust kernel.
- **Maintainability**: Clear separation of AST, Runtime, and Library code.
- **Testability**: Comprehensive test suite running in both Node.js and Browser environments.

## 🏗️ Architecture

The project follows a **Two-Tier Architecture**:
1.  **JavaScript Runtime (Kernel)**: The core engine that executes Scheme code (`src/runtime/`).
2.  **Scheme Libraries**: The standard library implemented in Scheme itself, loaded by the runtime (`src/lib/`).

### Layer 1: The Kernel (`src/runtime/`)
The foundational layer is currently implemented and stable:
-   **Trampoline Interpreter**: Enables infinite recursion (TCO) by managing stack frames on the heap (`interpreter.js`).
-   **Stepables**: Unified execution model for AST nodes and Continuation Frames (`stepables.js`).
-   **Environment**: Lexical scoping mechanism (`environment.js`).
-   **Primitives**: Native JavaScript implementations of core procedures (`primitives/`).
-   **Library Loader**: R7RS `define-library` and `import` support (`library_loader.js`).

### Scheme Libraries (`src/lib/`)
-   **boot.scm**: Minimal bootstrap code loaded before libraries.
-   **scheme/base.sld**: The R7RS `(scheme base)` library definition.

For a detailed breakdown of the internal file structure, see [directory_structure.md](./directory_structure.md).

## 🚀 Getting Started

### Prerequisites
-   **Node.js**: v14+ (for running tests and CLI/server).
-   **Modern Browser**: (Optional) For running the Web REPL.

### Installation
Clone the repository:
```bash
git clone https://github.com/mark-friedman/scheme-js-4.git
cd scheme-js-4
```

### Running the Web REPL
1.  Start a local HTTP server in the project root:
    ```bash
    python3 -m http.server 8080
    ```
2.  Open your browser to:
    [http://localhost:8080/web/ui.html](http://localhost:8080/web/ui.html)
3.  Start typing Scheme code!

## 🧪 Testing

The project uses a custom universal test runner that works in both Node.js and the Browser.

### Run All Tests (Node.js)
Execute the complete test suite including unit, functional, and integration tests:
```bash
node run_tests_node.js
```

### Run Browser Tests
1.  Ensure the HTTP server is running (see above).
2.  Navigate to `http://localhost:8080/web/ui.html`.
3.  The test suite runs automatically in the console on load. Check the browser developer console (F12) to see the results.

## 📚 Documentation

We maintain detailed documentation for the project internals:

-   [**Directory Structure**](./directory_structure.md): Detailed map of the codebase and where files belong.
-   [**Layer Plan**](./layer_plan.md): The implementation roadmap for future layers.
-   [**Trampoline Execution**](./docs/trampoline.md): A deep dive into how the interpreter handles stack frames and TCO.
-   [**Changes**](./CHANGES.md): A log of major implementation steps, walkthroughs of features, and refactors.

## 🛠️ Code Standards

-   **Style**: We use ES Modules (`import`/`export`) throughout. All functions are documented with JSDoc. Scheme code is written in a style that is similar to JSDoc.
-   **Testing**: We follow a "Dual Environment" rule - every new feature must represent correct behavior in both Node.js V8 and standard browser engines.
-   **Code Quality**: We separate "step-able" logic (instructions for the machine) from runtime state (values and environments).
