This is a critical architectural decision. What you are describing is **Transparent Interoperability**.

In many language implementations, there is a "wall" where you have to manually box/unbox numbers or wrap functions. You want to tear down that wall.

Here is a design for how we can modify our implementation to achieve this. This largely affects **Layer 1 (Core Data)** and **Layer 3 (Interpreter Logic)** of our plan.

### **The Core Philosophy: "Primitives are Primitives"**

Currently, we wrap everything: new Literal(5), new Literal("hello").

To achieve your goal, runtime values in the registers (ans, env bindings) must be raw JavaScript values whenever possible.

#### **1\. The Data Mapping Strategy**

We will adopt a "shared representation" model.

| Scheme Type | Internal Representation | JS typeof / instanceof | Notes |
| :---- | :---- | :---- | :---- |
| **Number** | Raw JS Number | 'number' | 1:1 mapping (excluding bignums for now). |
| **String** | Raw JS String | 'string' | 1:1 mapping. Immutable in JS, so string-set\! will require a wrapper object later or copy-on-write. |
| **Boolean** | Raw JS Boolean | 'boolean' | \#t is true, \#f is false. |
| **Vector** | Raw JS Array | Array.isArray() | (vector 1 2\) is \[1, 2\]. vector-ref is arr\[i\]. |
| **Void/Undefined** | Raw JS undefined | 'undefined' | Used for (if \#f \#t). |
| **Procedure** | Closure instance | instanceof Closure | Needs a bridge to be callable from JS. |
| **JS Function** | Raw JS Function | 'function' | Can be called directly by Scheme. |
| **Pair/List** | Cons instance | instanceof Cons | Scheme specific. JS sees an object {car, cdr}. |
| **Symbol** | Symbol instance | instanceof Sym | Distinct from strings. |

### **2\. Removing Wrappers (Scheme \-\> JS)**

When Scheme calls a JavaScript function, we want it to look like this:

Scheme
```scheme
;; Scheme

(define console-log (js-global "console" "log")) ;; somehow get the reference

(console-log "Hello" (vector 1 2 3))
```
**Implementation Change in AppFrame.step:**

Currently, AppFrame throws an error if the function isn't a Closure or NativeJsFunction. We will modify it to accept **any** JavaScript function.

JavaScript
```javascript
// In AppFrame.step

// ... arguments are evaluated into 'newArgValues' ...

const func \= newArgValues\[0\];

const args \= newArgValues.slice(1);

if (typeof func \=== 'function') {

    // DIRECT JS INTEROP

    // We assume 'func' is a raw JS function (like Math.max or console.log)

    // We pass the raw values (numbers, strings, arrays) directly.

    // If 'args' contains a Closure, we might need to wrap it (see section 3).

    

    const result \= func(...args);

    

    // The result is placed in 'ans' and we halt this frame

    registers\[0\] \= result; 

    return false; 

}
```
// ... else handle Closure ...

### **3\. Removing Wrappers (JS \-\> Scheme)**

When JavaScript calls a Scheme procedure, we want it to look like this:

JavaScript

// JavaScript
```javascript
const schemeAdd \= interpreter.evaluate("(lambda (a b) (+ a b))");

const result \= schemeAdd(10, 20); // Returns 30, no wrappers\!
```
**The Problem:** schemeAdd internally is a Closure class instance. It doesn't have a () call signature in JavaScript.

**The Solution: The "Bridge" Proxy or Wrapper.**

When the Interpreter evaluates a Lambda and creates a Closure, or when a Closure is returned to JavaScript land, we must wrap it in a native JavaScript function that knows how to spin up the interpreter.

We can do this lazily or eagerly. An Eager approach (wrapping at creation time) is safer for interop.

We modify the Lambda AST node:

JavaScript
```javascript
class Lambda extends Executable {

    // ... constructor ...

    

    step(registers, interpreter) {

        const closure \= new Closure(this.params, this.body, registers\[2\]);

        

        // THE BRIDGE: Create a JS function that wraps the Scheme Closure

        const bridge \= (...jsArgs) \=\> {

            // 1\. Convert JS args to Scheme args if necessary 

            // (Since we use shared types, mostly pass-through)

            

            // 2\. Create the invocation AST

            // We need to inject the \*raw\* values.

            // We might need a specialized "LiteralValue" node that takes

            // an already-computed value without trying to parse/analyze it.

            const argLiterals \= jsArgs.map(val \=\> new Quote(val)); 

            

            const ast \= new TailApp(new Literal(closure), argLiterals);

            

            // 3\. Spin up a NEW interpreter instance or re-enter the existing one

            return interpreter.run(ast, interpreter.globalEnv);

        };

        

        // Attach the underlying Scheme closure to the bridge for introspection if needed

        bridge.\_schemeClosure \= closure;

        

        registers\[0\] \= bridge; // Return the FUNCTION, not the Closure object

        return false; 

    }

}
```
**Crucial change:** The internal representation of a Scheme procedure in registers\[0\] is now **actually a JavaScript function**.

This means AppFrame needs to be updated. Since *both* Native JS functions and Scheme Bridges are now typeof \=== 'function', how do we distinguish them for TCO?

* **Native JS Function:** We call it, get result, return false (halt frame).  
* **Scheme Bridge Function:** We want to **Tail Call** it if possible, or call it and await result.

Actually, if we change the internal representation, we lose the ability to inspect the body for TCO.

**Better Approach: Hybrid Representation**

We keep Closure as the internal representation.

1. **Scheme \-\> Scheme:** AppFrame sees a Closure. It extracts .body and .env and performs a **Tail Call** (modifies ctl and env, returns true).  
2. **Scheme \-\> JS:** AppFrame sees a pure JS function. It calls it func(...args).  
3. **JS \-\> Scheme:** When a Closure "leaks" out to JavaScript (e.g. as a return value from interpreter.run), we apply a **boundary wrapper**.

We modify Interpreter.run:

JavaScript
```javascript
run(ast, env) {

    // ... loop ...

    // loop finishes

    let result \= registers\[0\];

    

    // BOUNDARY CHECK

    if (result instanceof Closure) {

        return this.createJsBridge(result);

    }

    return result;

}

createJsBridge(closure) {

    return (...args) \=\> {

       // Start a new run() for this closure

       // This handles the "JS calling Scheme" requirement

    }

}
```
### **4\. Handling "Shared" Data Types (Vectors/Arrays)**

We simply stop wrapping arrays in Literal nodes during parsing/analysis in a way that hides them.

* **Parser:** Produces standard JS Arrays.  
* **Analyzer:** When it sees a JS Array (that isn't a special form like \['if', ...\]), it treats it as a VectorLiteral (if quoted) or a function application.  
* **Primitives:**  
  * vector-ref is implemented as: (vec, i) \=\> vec\[i\]  
  * vector-set\! is implemented as: (vec, i, val) \=\> { vec\[i\] \= val; }  
  * vector-length is: (vec) \=\> vec.length

Because the underlying object **is** a JS Array, any JS code receiving it can map/reduce/filter it natively.

### **5\. Summary of the Proposed Design**

1. **Execution Model:** The interpreter registers (ans, env) hold **raw** JS values (Numbers, Strings, Booleans, Arrays, Functions) and **class instances** for Scheme-specific types (Symbols, Cons, Closures).  
2. **Scheme calling JS:**  
   * Evaluator sees a TailApp.  
   * Operator evaluates to a plain JS Function (e.g. Math.max).  
   * Evaluator invokes Math.max(...argValues) and stores result in ans.  
   * No explicit wrapper needed in user code.  
3. **JS calling Scheme:**  
   * Evaluator returns a result.  
   * If result is a Closure, the Interpreter wraps it in a BridgeFunction before returning it to JS.  
   * The BridgeFunction captures the interpreter instance and starts a new trampoline session when invoked by JS.  
4. **Numbers:**  
   * Use JS Numbers.  
   * Interoperability is 100%.  
   * (Future Layer: If we add Bignums, they will be objects. JS will see objects. We can add valueOf() to Bignums to allow bignum \+ 5 to work in JS, though with precision loss).

This design provides the "Transparent Interoperability" you requested. It removes the need for js-invoke-k-from-js or js-run-callback—they become implicit.

### TCO and call/cc Semantics

Yes, the proposed design will indeed maintain `call/cc` and TCO semantics, provided we are careful about *how* we implement the "bridge" functions.

The core concern is that if we introduce a native JavaScript stack frame (by calling a JS function directly), we interrupt the trampoline. If that JS function then calls *back* into Scheme, we have a new trampoline loop nested inside a JS stack frame.

Here is why the design maintains semantics:

## 1. TCO Semantics

* **Scheme-to-Scheme:** The interpreter loop handles these calls by updating registers `ans`, `ctl`, and `env` and looping. The JavaScript stack does not grow. TCO is preserved.
* **Scheme-to-JS:** When Scheme calls `Math.max`, it is a "leaf" call. The JS stack grows by one frame, returns a value, and shrinks. TCO is preserved for the Scheme part.
* **JS-to-Scheme (The Critical Part):** If JS calls a Scheme closure, we wrap it in a bridge that starts a *new* interpreter loop.
    * If this is a "tail call" from JS's perspective (e.g., `return schemeCallback()`), the JS engine *might not* optimize it (JS doesn't guarantee TCO). The JS stack will have one frame for the caller and one for the bridge.
    * However, **within** that new interpreter loop, Scheme execution is fully tail-recursive.
    * Does this break Scheme TCO? Strictly speaking, R7RS requires that "an unbounded number of active tail calls" be supported. If you ping-pong between JS and Scheme 10,000 times (JS calls Scheme which calls JS which calls Scheme...), you *will* blow the stack because each transition adds a JS stack frame (the bridge).
    * **Mitigation:** This is an unavoidable consequence of interoperating with a non-TCO language (JavaScript) without full stack introspection. The design is "as TCO as possible": unlimited Scheme recursion is safe, but unlimited *cross-language* recursion is limited by the JS engine's stack size. This is standard behavior for Scheme implementations on the JVM/CLR/JS.

## 2. `call/cc` Semantics

* **Capturing:** `call/cc` captures the `fstack` (continuation frames). Crucially, **this stack only contains Scheme frames**. It does *not* capture the native JS stack frames of any JS functions that might be above us in the call chain.
* **Restoring (Scheme context):** If we invoke a continuation from within Scheme, we replace the `fstack`. The interpreter jumps to the new context. This works perfectly.
* **Restoring (JS context):** If a JS function (e.g., `fetchData` callback) calls a Scheme closure that invokes a continuation:
    1.  The bridge starts a new interpreter.
    2.  That interpreter hits the `Continuation` invocation.
    3.  It replaces its (empty) `fstack` with the captured one.
    4.  The interpreter loop continues, effectively "returning" into the captured context.
    * **The Limitation:** This effectively **aborts** the JS callback. The `interpreter.run()` call inside the bridge *never returns*. The JS frames above it (the `fetchData` callback logic) are abandoned.
    * **Is this correct?** Yes! In Scheme, invoking a continuation *replaces* the future. If you jump out of a callback, you *should* abandon the rest of that callback. The design correctly implements this "escape" behavior.

**Conclusion:**
The design supports full TCO for Scheme-only code. It supports `call/cc` correctly, including escaping from JS callbacks. The only limitation is that infinite mutual recursion between JS and Scheme is bounded by the JS stack size, which is an acceptable trade-off for transparent interoperability. 