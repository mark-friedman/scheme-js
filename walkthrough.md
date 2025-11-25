# Restructuring Walkthrough

I have reorganized the codebase into a modular structure as requested.

## Directory Structure

- **src/core/**: Core interpreter logic.
  - `interpreter.js`: The main interpreter loop.
  - `environment.js`: Lexical environment implementation.
- **src/syntax/**: Parsing and AST.
  - `reader.js`: Parser (S-expressions).
  - `analyzer.js`: AST transformation.
  - `ast.js`: AST node definitions (split from original `ast.js`).
- **src/data/**: Runtime values.
  - `values.js`: Runtime value classes (`Closure`, `NativeJsFunction`, `Continuation`) (split from original `ast.js`).
- **src/primitives/**: Native implementations.
  - `index.js`: Global environment loader.
  - `math.js`: Math primitives.
  - `io.js`: I/O primitives.
  - `list.js`: List primitives.
  - `async.js`: Async/Interop primitives.
- **web/**: Web interface.
  - `index.html`: Main HTML file (formerly `ui.html`).
  - `main.js`: Entry point.
  - `repl.js`: REPL logic.
- **lib/**: Scheme libraries (placeholders).
  - `boot.scm`
  - `stdlib.scm`
- **tests/**: Test suite.
  - `unit/`: Unit tests.
  - `functional/`: Functional tests.
  - `helpers.js`, `tests.js`, `run_all.js`: Test runners and helpers.

## Changes

1.  **Split `ast.js`**: Separated AST nodes (`src/syntax/ast.js`) from runtime values (`src/data/values.js`) to break circular dependencies and clarify concerns.
2.  **Moved Files**: Moved all files to their respective directories.
3.  **Updated Imports**: Updated all `import` statements to reflect the new paths.
4.  **Created Primitives**: Extracted primitives from `environment.js` into `src/primitives/`.
5.  **Restructured Tests**: Organized tests into `unit` and `functional` folders.

## Verification

Ran `node tests/run_all.js` and all tests passed.
