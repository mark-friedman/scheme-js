`core-interpreter-implementation.md` serves as a practical implementation guide for an interpreter that synthesizes several advanced computer science concepts found in the three specified papers. Below are the citations mapping these references.

### 1. References to "A Method for Implementing First-Class Continuations on the JVM and CLR"

`core-interpreter-implementation.md` draws its core architectural framework—the **Register Machine** and the **Master Loop**—directly from this document.

*   **The Register Machine**: The document defines four registers (ans, ctl, env, fstack). This mirrors the **"Three Sacred Registers"** (ans, ctl, env) described by Marshall, where `ans` is the payload, `ctl` is the next step, and `env` is the lexical context.
*   **The Trampoline Loop**: The main execution loop in `Interpreter.run()` repeatedly calls `step()` on the control register. This is identical to the **"beating heart"** trampoline loop provided in Marshall's code examples: `while (control.Step(out answer, ref control, ref environment)) {}`.
*   **Tail Call Logic**: `core-interpreter-implementation.md` uses a boolean return from `step()` where `true` indicates a **Tail Call**. This corresponds to Marshall’s "Law 3: The Hop," where a `true` return signals the caller's loop to immediately re-execute with new register values without growing the native stack.

### 2. References to "An Unexceptional Implementation of Continuations"

This paper provides the performance optimizations and specific return protocols adopted by the interpreter described in `core-interpreter-implementation.md`.

*   **Boolean Return Protocol**: The use of a boolean `true`/`false` to signal whether to continue stepping or return a value is a central optimization from this paper. This mechanism replaces the slower exception-based unwinding originally proposed in earlier research.
*   **Answer Register Management**: The interpreter stores the result of a sub-computation in the `ans` register. This follows the paper's method of using **`out` and `ref` parameters** to allow state variables to survive returns through the trampoline without constant heap allocation.
*   **Sentinel Values**: While `core-interpreter-implementation.md` uses `SentinelResult` exceptions for JavaScript boundaries, the concept of a **distinguished singleton object** (like `UNWIND_STACK`) to signal control flow changes is a key concept from this paper.

### 3. References to "Continuations from Generalized Stack Inspection"

`core-interpreter-implementation.md` implements the theoretical reification of the call stack into data structures as defined by Pettyjohn et al.

*   **Explicit Frames**: The interpreter distinguishes between **AST Nodes** (atomic work) and **Frames** (pending computation). These Frames represent the **"functional representations of continuations"** stored as continuation marks or records in the Pettyjohn model.
*   **Continuation Capture**: When `call/cc` executes, it copies the entire `fstack` into a Continuation object. This is a direct implementation of the paper’s **"snapshot of the stack"** technique, where the current context is captured and reified into a stored data structure.
*   **Stack Reconstitution**: The process of invoking a continuation—where the `fstack` is replaced and `ans` is set—mirrors the **`resume` function** algorithm that builds a new evaluation context from a list of stored marks.
*   **Procedure Fragmentation**: The architectural split where AST nodes push frames to evaluate sub-expressions matches the paper's requirement for **Procedure Fragmentation**, splitting functions at call boundaries to allow mid-execution re-entry.
