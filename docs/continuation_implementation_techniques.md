# Continuation Implementation Techniques: A Summary of Three Papers

## Overview

These three papers address a fundamental challenge: **how to implement first-class continuations on platforms that don't provide direct stack manipulation**. Each builds on the previous, forming a progression from theoretical foundations to practical, high-performance implementations.

---

## Paper 1: "Continuations from Generalized Stack Inspection" 
*Pettyjohn, Clements, Marshall, Krishnamurthi, Felleisen (ICFP 2005)*

**The Problem**: Managed runtimes like the JVM and CLR don't expose primitives for capturing or restoring the call stack, yet Scheme and other languages require first-class continuations.

**The Solution**: A program transformation that enables stack capture without host language support:

1. **A-Normal Form (ANF) Conversion**: Transform code so every intermediate result is bound to a variable and every call is either an assignment RHS or a return.

2. **Procedure Fragmentation**: Split each function at call boundaries, creating "fragments" that can be re-entered mid-execution.

3. **Continuation Marks**: Introduce `w-c-m` (write-continuation-mark) and `c-c-m` (capture-continuation-mark) primitives that annotate the stack with closures representing suspended computations.

4. **Exception-Based Stack Walking**: When `call/cc` is invoked, throw a special exception. Each stack frame catches it, packages its local state into a closure, appends that to a growing chain, then re-throws.

5. **Defunctionalization**: Convert the closure chain into a data structure (list of frame records) that can be stored and later restored.

**Key Insight**: Keep the **normal call stack for routine execution** — only materialize continuations to the heap when `call/cc` is actually invoked. This avoids the overhead of full CPS transformation.

---

## Paper 2: "A Method for Implementing First-Class Continuations on the JVM and CLR"
*Joe Marshall*

**The Context**: This paper (discussed on lisp.org) describes the practical mechanics of implementing the Pettyjohn technique within an interpreter.

**The Technique: Master Loop with Unwind Events**

1. **Trampoline-Based Interpreter**: The main entry point runs a loop that repeatedly calls `step()` on the current control object.

2. **Unwind Signal**: When a continuation must be captured or restored, the interpreter detects an "unwind event" — a signal that control flow needs to jump non-locally.

3. **Cooperative Protocol**: Each procedure participates in continuation management. When unwinding, each stack frame saves its state before returning.

4. **State Restoration**: When invoking a captured continuation, the interpreter restores the saved frame chain and resumes execution.

**Key Insight**: The interpreter must mediate between the host language's stack (which can't be manipulated directly) and Scheme's continuation semantics (which require arbitrary control flow).

---

## Paper 3: "An Unexceptional Implementation of Continuations"
*Joe Marshall (2009)*

**The Problem**: The Pettyjohn technique works but uses exception throwing to unwind the stack. On the CLR, exception throwing is **~3,800× slower than a normal return**.

**The Solution: Replace Exceptions with Sentinel Returns**

1. **Per-Call-Site Trampolines**: Instead of one global trampoline, each call site has its own mini-loop.

2. **Boolean Return Protocol**: Step methods return `true` (continue stepping) or `false` (value ready), using `out`/`ref` parameters for the actual results.

3. **Sentinel Value Detection**: After each call returns, check if the result is a distinguished `UNWIND_STACK` value. If so, capture the current frame and propagate.

4. **Parameter Repurposing**: Cleverly reuse the environment parameter to carry unwinding state, avoiding extra allocations.

**Performance Results**: The modified C# interpreter matched or exceeded the C interpreter's performance, while heap-based continuation implementations were 3-4× slower.

**Key Insight**: The overhead of checking a flag after every return is negligible compared to exception handling overhead. Stack-based continuations dramatically outperform heap-based ones because modern CPUs have specialized hardware for stack operations.

---

## Comprehensive Comparison Table

| Technique/Idea | Paper 1 (Pettyjohn) | Paper 2 (Marshall - JVM/CLR) | Paper 3 (Marshall - Unexceptional) | This Implementation |
|---------------|---------------------|------------------------------|-----------------------------------|---------------------|
| **Core Approach** | Program transformation + stack inspection | Interpreter with unwind signals | Stack-based + sentinel returns | Explicit frame stack with exception unwind |
| **Stack Representation** | Implicit (host stack) until capture | Implicit until capture | Implicit (host stack) | **Explicit** `FSTACK` array always |
| **Continuation Capture** | Throw exception, catch at each frame | Detect unwind event in loop | Check sentinel after each return | Copy `FSTACK` array |
| **Unwind Mechanism** | Exception throwing | Unwind event propagation | Sentinel value return | `ContinuationUnwind` exception |
| **Performance on Unwind** | Slow (~3800× slower than return) | Slow (same mechanism) | Fast (normal return path) | Medium (exceptions only when crossing JS boundary) |
| **Normal Path Overhead** | Handler setup/teardown at calls | Minimal | Flag check after each return | None |
| **Tail Call Support** | Via trampoline or transformation | Via trampoline | Via trampoline + boolean return | Via `step()` returning `true` |
| **A-Normal Form** | Required for transformation | N/A (interpreter-based) | N/A (interpreter-based) | N/A (already using explicit AST nodes) |
| **Procedure Fragmentation** | Required | N/A | N/A | N/A (frames are already data) |
| **Continuation Marks** | `w-c-m` and `c-c-m` primitives | Implicit in frame capture | Implicit in frame capture | Explicit frame objects capture state |
| **Defunctionalization** | Convert closures to data | Already data in interpreter | Already data in interpreter | Frames are already classes/data |
| **Dynamic-Wind Support** | Not addressed | Implied | Not addressed | Full support via `WindFrame` and before/after thunks |
| **Multiple Values** | Not addressed | Not addressed | Not addressed | `Values` class with `call-with-values` |
| **JS Interop Boundary** | N/A | N/A | N/A | `SentinelFrame`/`SentinelResult` + `ContinuationUnwind` |

---

## How This Implementation Synthesizes These Ideas

This `scheme-js` implementation draws from all three papers but is tailored for JavaScript's constraints:

| Paper Concept | This Implementation | Location |
|--------------|---------------------|----------|
| **Explicit continuation frames** (Paper 1's defunctionalized closures) | `AppFrame`, `IfFrame`, `BeginFrame`, etc. | `src/core/interpreter/frames.js` |
| **Lazy heap allocation** (Paper 1's key insight) | Only copy `FSTACK` when `call/cc` invoked | `CallCCNode.step()` in `src/core/interpreter/ast_nodes.js` |
| **Trampoline loop** (Papers 2 & 3) | `while (true) step()` | `src/core/interpreter/interpreter.js` |
| **Boolean return protocol** (Paper 3) | `step()` returns `true`/`false` | All frame and node `step()` methods |
| **Exception-based unwind** (Paper 1) | `ContinuationUnwind` exception | `src/core/interpreter/values.js` |
| **Master loop catches unwind** (Paper 2) | `catch (e) { if ContinuationUnwind... }` | `src/core/interpreter/interpreter.js` |
| **Sentinel for interop boundaries** (Paper 3 inspired) | `SentinelFrame` and `SentinelResult` | `src/core/interpreter/interpreter.js` |
| **Common ancestor + unwind/rewind** (not in papers, but standard) | `invokeContinuation()` with `WindFrame` handling | `src/core/interpreter/frames.js` |

---

## Summary

**Paper 1** provides the theoretical framework: you can implement continuations without host stack access by transforming programs and using stack inspection. **Paper 2** applies this to interpreters with a master loop. **Paper 3** optimizes by avoiding exceptions entirely.

This implementation is closest to **Paper 2's architecture** (interpreter with explicit loop and unwind handling) combined with **Paper 3's spirit** (avoid exceptions when possible, use them only when necessary). It improves on Paper 1 by never needing the ANF/fragmentation transformations — the explicit frame stack achieves the same goal more directly.

The key architectural decision in this implementation is maintaining an **explicit `FSTACK`** rather than relying on JavaScript's implicit call stack. This gives:

- **Simple `call/cc`**: just copy the array
- **Clean dynamic-wind**: frames can be WindFrames with before/after thunks
- **JS interop**: sentinels and exceptions handle boundary crossing
- **No program transformation needed**: the interpreter naturally produces frame data

---

## References

1. Pettyjohn, G., Clements, J., Marshall, J., Krishnamurthi, S., and Felleisen, M. 2005. *Continuations from generalized stack inspection.* ICFP '05. ACM, New York, NY, 216-227.

2. Marshall, J. *A Method for Implementing First-Class Continuations on the JVM and CLR.* (Available via lisp.org)

3. Marshall, J. 2009. *An Unexceptional Implementation of Continuations.* (Presented at ILC 2009)
