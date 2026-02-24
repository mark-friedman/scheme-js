# Walkthrough: Implementing define-macro

I have implemented the `define-macro` special form, bringing Common Lisp-style unhygienic macros to Scheme-JS.

## Changes

### 1. Special Form Handler
- **`src/core/interpreter/analyzers/core_forms.js`**: Added `analyzeDefineMacro`.
    - It creates a temporary `Interpreter` with a fresh standard environment.
    - It evaluates the transformer body (which is regular Scheme code) into a closure.
    - It registers a JS wrapper function in the macro registry that invokes this closure during expansion.
- **`src/core/interpreter/library_registry.js`**: Added `define-macro` to `SYNTAX_KEYWORDS` and `SPECIAL_FORMS`.

### 2. Library Support
- **`src/core/scheme/define-macro.sld`**: Created a library exporting `define-macro`.
- **`repl.js` & `web/main.js`**: Updated bootstrap logic to import `(scheme define-macro)` by default.
- **`tests/run_scheme_tests_lib.js`**: Updated test runner to include `(scheme define-macro)`.

### 3. Verification
- Created `tests/functional/test_defmacro.scm` demonstrating:
    - Standard list destructuring: `(define-macro (name . args) ...)`
    - Explicit transformer: `(define-macro name transformer-proc)`
    - Usage in expressions.
- Added to `tests/test_manifest.js`.

## Verification Results

### Automated Tests
Ran the full test suite.

```
TEST SUMMARY: 2021 passed, 0 failed, 7 skipped
```

### Manual Verification
Ran `node repl.js tests/functional/test_defmacro.scm`.

```
OK
Math OK
Unless OK
```
