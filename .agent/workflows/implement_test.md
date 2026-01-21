---
description: create a new test file
---

1.  **Determine the Test Location.**
    *   Mirror the source directory structure.
    *   Example: `src/core/scheme/list.scm` -> `tests/core/scheme/list_test.scm`.

2.  **Create the Test File.**
    *   Path: `tests/[path]/[name]_test.scm`.
    *   **Import**: `(scheme base)` and `(scheme-js test)`.
    *   **Structure**: Wrap tests in `(test-group ...)`.

    ```scheme
    (import (scheme base)
            (scheme-js test)
            (scheme-js [library-under-test]))

    (test-group "Library Name Tests"
      (test "test description" expected-value expression)
      (test-assert "predicate test" (predicate? value)))
    ```

3.  **Register the Test.**
    *   Open `tests/tests.js` (or `test_manifest.js` if split).
    *   Add: `import './[path]/[name]_test.scm';` (or equivalent registration).

4.  **Run Tests.**
    *   Command: `node run_tests_node.js`
    *   Browser: Open `http://localhost:8080/ui.html` and check console/UI.
