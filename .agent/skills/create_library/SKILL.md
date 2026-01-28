---
name: Create Library
description: A guide for creating new Scheme libraries (R7RS) in the project.
---

# Create Library

This skill guides you through adding a new Scheme environment/library to the project. This is the standard way to organize Scheme code.

## Steps

### 1. Identify Location

*   **Core Libraries**: `src/core/scheme/` (for fundamental language features)
*   **Extra Libraries**: `src/extras/scheme/` (for extended functionality, interop, tools)

### 2. Create Library Definition (`.sld`)

Create a `.sld` file (e.g., `feature.sld`) in the appropriate directory.

```scheme
;; src/extras/scheme/feature.sld
(define-library (scheme-js feature)
  (export 
    my-procedure
    my-macro)
  (import (scheme base))
  
  ;; Option A: Include implementation file (Preferred for larger logic)
  (include "feature.scm")
  
  ;; Option B: Inline implementation (OK for very small libraries or simple re-exports)
  ;; (begin
  ;;   (define (my-procedure x) ...))
)
```

### 3. Create Implementation File (`.scm`)

If using `(include "feature.scm")`, create the corresponding `.scm` file in the same directory.

```scheme
;; src/extras/scheme/feature.scm

;; Note: No (library ...) wrapper here, this code is included directly into the library body.

(define (my-procedure x)
  (+ x 1))
  
(define-syntax my-macro
  ...)
```

### 4. Tests

*   **Create Test File**: Create a test file in `tests/` mirroring the source structure (e.g., `tests/extras/scheme/feature_tests.scm`).
*   **Format**:
    ```scheme
    (import (scheme base)
            (scheme-js feature) ;; Import your new library
            (scheme-js test))   ;; Import test framework

    (test-group "Feature Tests"
      (test "Description" expected-value expression)
      ;; ...
    )
    ```
*   **Register Test**: Add the test file to `tests/tests.js` (or `test_manifest.js` if applicable) so it runs with the suite.

### 5. Verification

*   Run `node run_tests_node.js`
