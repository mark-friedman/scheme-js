# **Layer 1 Implementation Plan: Syntactic Foundation**

## **Phase 1: Quasiquote & Quote**

Before we can write macros comfortably, we need the ability to construct code templates easily.

1. **Reader Update:** Ensure \`, ,, and ,@ are parsed into (quasiquote x), (unquote x), and (unquote-splicing x).  
2. **Analyzer/Macro Expansion:** Implement the expansion logic for quasiquote. While often implemented as a macro itself, in a bootstrap interpreter it's often easier to implement as a primitive syntactic form in the Analyzer that expands into cons, list, append, etc.

**Proposed Plan for Next Session:**
1.  **Implement `quote`:** The fundamental special form to prevent evaluation. Required for macros to manipulate syntax as data.
2.  **Implement `quasiquote`:** Update the `Reader` and `Analyzer` to handle `` ` ``, `,`, and `,@`. This effectively moves template construction from JS into Scheme.
3.  **Implement `define-syntax` (Basic):** Create the hook in the `Analyzer` to register a transformer.

## **Phase 2: The Macro System (syntax-rules)**

1. **define-syntax:** Add support to the Analyzer to recognize (define-syntax name transformer).  
   * This requires a separate "compile-time" environment or a flag in the current environment to distinguish macros from variables.  
2. **syntax-rules Transformer:**  
   * Implement the pattern matching logic in JavaScript.  
   * **Pattern Matching:** Match input syntax against literals, variables, and lists (including improper lists and ellipses ...).  
   * **Transcription:** valid matches must generate new ASTs based on the associated template.  
3. **Hygiene (Basic):**  
   * Initially, we can implement a non-hygienic system (essentially defmacro) to get things working, or go straight for a renaming mechanism to ensure hygiene (renaming local variables in macros so they don't clash with user variables). *Recommendation: Start non-hygienic for speed, then refactor.*

## **Phase 3: Core Data Structures (The "Cons" Cell)**

1. **Cons Class:** Create a class Cons { car, cdr }.  
2. **Refactor Reader:** Change the reader to produce Cons chains instead of JS Arrays for lists.  
3. **Refactor Analyzer:** Update the analyzer to traverse Cons chains.  
4. **Primitives:** Add car, cdr, cons, set-car\!, set-cdr\!, null?, pair? to the global environment.
5. **Symbols:** Introduce a `Symbol` class and an `intern` mechanism to distinguish symbols from strings.
6. **Vectors:** Add support for Scheme vectors (`#(1 2 3)`), mapping them to JS arrays.

## **Outcome**

At the end of Layer 1, we will be able to define:

```scheme
(define-syntax when  
  (syntax-rules ()  
    ((when test stmt1 stmt2 ...)  
     (if test (begin stmt1 stmt2 ...)))))
```

This unlocks the ability to write the rest of the standard library in Scheme itself.
