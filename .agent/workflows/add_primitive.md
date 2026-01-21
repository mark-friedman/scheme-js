---
description: add a new JS primitive
---

1.  **Draft the Primitive in JS.**
    *   Locate: `src/extras/primitives/` (or create a new file here).
    *   **Rule**: Use `type_check.js` assertions.
    *   **Rule**: Throw `SchemeError`.

    ```javascript
    import { assertString } from '../../core/interpreter/type_check.js';

    export const myPrimitives = {
      'my-primitive': (arg) => {
         assertString(arg);
         return "result";
      }
    };
    ```

2.  **Register in `src/core/primitives/index.js`**
    *   Import your primitives object.
    *   Call `addPrimitives(myPrimitives)` inside `createGlobalEnvironment`.

3.  **Expose via Scheme Library.**
    *   Create or update an `.sld` file in `src/extras/scheme/`.
    *   Export the primitive name.

    ```scheme
    (define-library (scheme-js my-lib)
      (export my-primitive ...)
      ...)
    ```

4.  **Add Tests.**
    *   Create a test file in `tests/extras/primitives/`.
    *   Test both valid inputs and error cases (type checks).

5.  **Verify.**
    *   Run `node run_tests_node.js`.
