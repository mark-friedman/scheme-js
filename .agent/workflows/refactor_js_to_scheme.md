---
description: refactor JS logic to Scheme
---

1.  **Analyze the JavaScript Code.**
    *   Identify the inputs, outputs, and side effects.
    *   Identify dependencies (other JS functions, global state).

2.  **Determine the Strategy.**
    *   **Pure Logic**: Implementation in Scheme is trivial.
    *   **Direct Interop**: Use `js-ref`, `js-set!`, `js-invoke`.
    *   **Complex Interop**: May need a new primitive (see `/add_primitive`).

3.  **Draft the Scheme Implementation.**
    *   Location: An existing or new `.scm` file (see `/scaffold_library`).
    *   **Pattern**: Wrap JS calls if needed.

    ```scheme
    ;; JS: return obj.prop + 1;
    (define (my-func obj)
      (+ (js-ref obj "prop") 1))
    ```

4.  **Replace the JS Call Site.**
    *   If the JS function was called from Scheme, just call the new Scheme procedure.
    *   If called from JS, expose the Scheme procedure:
        *   `globalThis.myFunc = interpreter.call("my-func", ...)` (simplistic view)
        *   Better: Use the `init.scm` or defined exports to make it available.

5.  **Add Tests.**
    *   Port existing JS unit tests to Scheme (see `/implement_test`).

6.  **Verify.**
    *   Run tests: `node run_tests_node.js`
    *   Check for regressions.
