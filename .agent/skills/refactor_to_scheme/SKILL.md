---
name: Refactor to Scheme
description: Guidelines for moving logic from JavaScript to Scheme (Schemification).
---

# Refactor to Scheme

This skill guides you through refactoring existing JavaScript logic into Scheme. The project philosophy is **Scheme over JS**: implement as much as possible in Scheme, using JS only for the bare minimum (primitives, IO, DOM).

## Criteria

*   **Move to Scheme**: Core logic, algorithms, transformation steps, initialization sequences.
*   **Keep in JS**: Low-level DOM manipulation, browser APIs, Node.js APIs, performance-critical hot loops (only if proven slow in Scheme).

## Steps

### 1. Analyze and Plan

1.  Identify the JavaScript file/functionality to convert.
2.  Determine dependencies: what JS primitives does it need?
3.  Design the Scheme interface.

### 2. Implement Scheme Replacement

1.  Create a new Scheme library (see **Create Library** skill) or add to an existing one.
2.  Implement the logic in Scheme.
3.  If you need to call this from JavaScript (e.g., during startup), ensure it's exported or registered.
    *   *Note: Code in `(define-library ...)` is executed when loaded.*

### 3. Bridge Primitives (If needed)

If the new Scheme code needs access to specific JS features it didn't have before, create new primitives (see **Implement Primitive** skill).

### 4. Remove/Minimize JavaScript

1.  Delete the old JavaScript logic.
2.  Refactor the JS entry point to just load/invoke the Scheme code.

### 5. Update Tests

1.  Port existing JS unit tests to Scheme (preferred).
2.  If strictly JS interoperability tests, update them to call the new Scheme implementation via `interop` or the interpreter instance.

## Example: Refactoring Initialization

**Before (JS):**
```javascript
function init() {
  setupWorkspace();
  loadBlocks();
}
```

**After (Scheme):**
```scheme
;; src/scheme/init.scm
(define (init)
  (setup-workspace)
  (load-blocks))
```
**After (JS):**
```javascript
interpreter.eval('(init)');
```
