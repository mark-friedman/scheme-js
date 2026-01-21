---
name: Implement Primitive
description: A guide for implementing new JavaScript primitives and exposing them to Scheme.
---

# Implement Primitive

This skill guides you through adding a new JavaScript primitive to the Scheme interpreter. Primitives are JavaScript functions exposed to Scheme, typically used for:
- Interacting with the JavaScript environment (DOM, async, etc.).
- Performance-critical operations.
- Wrapping JavaScript libraries.

> [!IMPORTANT]
> **Scheme over JS**: Only implement primitives when necessary. If logic can be implemented in Scheme (even if it's slower initially), prefer Scheme. Use primitives for things Scheme *cannot* do (IO, direct JS interop).

## Checks

1.  **Is this necessary?** Can this be done in Scheme? if yes, do that instead.
2.  **Location**: 
    - Core primitives go in `src/core/primitives/`.
    - Extra/Extension primitives go in `src/extras/primitives/`.

## Steps

### 1. Create/Update JavaScript Implementation

Create a new file or update an existing one in `src/extras/primitives/` (e.g., `feature.js`).

**Pattern**:
```javascript
import { ... } from '../../core/interpreter/type_check.js'; // conversions/assertions
import { schemePrimitives } from '../../core/interpreter/scheme_primitives.js'; // if wrapping scheme

export const myPrimitives = {
    'scheme-name': (arg1, arg2) => {
        // 1. assertions
        // 2. logic
        return result;
    }
}
// OR if you need the interpreter instance (e.g. for creating closures/promises)
export function getMyPrimitives(interpreter) {
    return {
        'scheme-name': (arg1) => { ... }
    }
}
```

*   **Type Checking**: YOU MUST use assertions (e.g., `assertString`, `assertProcedure`) from `type_check.js`.
*   **Errors**: Throw `SchemeError` or specific subclasses.

### 2. Register Primitive

Update `src/core/primitives/index.js` to import and register your primitives.

```javascript
import { myPrimitives } from '../../extras/primitives/feature.js';

// ... inside createGlobalEnvironment
addPrimitives(myPrimitives);
```

### 3. Create Scheme Library Definition

Create a `.sld` file in `src/extras/scheme/` (e.g., `feature.sld`) to export the new primitives.

```scheme
(define-library (scheme-js feature)
  (export scheme-name ...)
  (import (scheme base)) ;; or others
  ;; The primitives are already in the global environment via index.js, 
  ;; so we usually just export them. 
  ;; Sometimes we might re-export or wrap them.
)
```
*Note: In this project's architecture, primitives added to `createGlobalEnvironment` are available globally, but for R7RS compliance and cleanliness, we wrap them in a library.*

### 4. Tests

*   **Unit Tests**: Add tests in `tests/` (usually `tests/extras/primitives/` or similar).
*   **Run Node**: `node run_tests_node.js`
*   **Run Browser**: Open `http://localhost:8080/ui.html`
