---
description: create a new R7RS library
---

1.  **Ask the user for the library name and location.**
    *   Example: "Where should the library `(scheme-js my-lib)` be created? (Core or Extras)"
    *   Determine the base path: `src/core/scheme/` or `src/extras/scheme/`.

2.  **Create the `.sld` file.**
    *   Path: `[base_path]/[lib_name].sld`
    *   Content: A standard R7RS library definition.
    *   **Import**: `(scheme base)` at a minimum.
    *   **Include**: `[lib_name].scm`.

    ```scheme
    (define-library (scheme-js [lib_name])
      (export [exported_procedures])
      (import (scheme base))
      (include "[lib_name].scm"))
    ```

3.  **Create the `.scm` file.**
    *   Path: `[base_path]/[lib_name].scm`
    *   Content: The implementation of the library.

    ```scheme
    ;; Implementation for (scheme-js [lib_name])

    (define (my-procedure)
      'hello)
    ```

4.  **Create the test file.**
    *   Path: `tests/[base_path_relative]/[lib_name]_tests.scm`
    *   Content: A standard test suite using `(scheme-js test)`.

    ```scheme
    (import (scheme base)
            (scheme-js [lib_name])
            (scheme-js test))

    (test-group "[lib_name] tests"
      (test "example test" 'hello (my-procedure)))
    ```

5.  **Register the test.**
    *   Add the test file path to `tests/tests.js` (or `test_manifest.js`).

6.  **Verify.**
    *   Run `node run_tests_node.js` to ensure the library loads and tests pass.
