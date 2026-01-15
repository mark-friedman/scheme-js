# Hygienic Macro Implementation

This document explains how macro hygiene is implemented in the Scheme interpreter.

## Overview

The implementation uses **pure Dybvig-style scopes (marks)** to achieve hygiene. Unlike traditional rename-based systems that use gensyms, this approach keeps all original identifier names but attaches scope marks that distinguish bindings.

This prevents:
1. **Accidental capture** — Macro-introduced bindings don't capture user variables
2. **Referential transparency** — Free variables in templates resolve in their definition context

## Key Concepts

### Scope Marks

Each identifier can carry a set of **scope marks** (integers). When macros expand:
- Every macro expansion gets a **unique expansion scope**
- All template identifiers (both introduced bindings and free variables) get marked with this scope
- Pattern variables are substituted and scope-flipped

```javascript
// In syntax_object.js
class SyntaxObject {
  constructor(name, scopes = new Set()) {
    this.name = name;    // Original identifier name
    this.scopes = scopes; // Set<number> of scope marks
  }
}
```

### Why Pure Marks?

Two identifiers with the same name but different scope sets are **distinct bindings**:

```scheme
;; Macro introduces 'tmp' with scope mark #1
;; User's 'tmp' has no scope marks
;; They are different identifiers!
```

This eliminates the theoretical edge case where gensym-generated names could collide with user code.

## The Expansion Process

### 1. Pattern Matching

`matchPattern()` matches input against the macro pattern, collecting pattern variable bindings. Input identifiers get "anti-marked" by flipping the expansion scope.

### 2. Transcription

`transcribe()` builds the output with the `expansionScope`:

- **Pattern variables** → substituted with matched input (scope-flipped)
- **All other identifiers** → marked with `expansionScope` via `flipScope()`

```javascript
// Simplified from transcribe()
if (bindings.has(template)) {
    // Pattern variable: substitute with scope flip
    return flipScopeInExpression(bindings.get(template), expansionScope);
}
// All other identifiers: mark with expansion scope
return template.flipScope(expansionScope);
```

### 3. Resolution

When looking up a binding, the `ScopeBindingRegistry` finds the binding whose scopes are a **maximal subset** of the identifier's scopes:

```javascript
// Simplified resolution
resolve(syntaxObj) {
  const candidates = this.bindings.get(syntaxObj.name);
  // Find binding with largest scope set that is subset of syntaxObj.scopes
  return bestMatch;
}
```

## Example: swap! Macro

```scheme
(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((tmp a))
       (set! a b)
       (set! b tmp)))))

(let ((tmp 100))
  (let ((x 1) (y 2))
    (swap! x y)
    tmp)) ; → 100 (not captured!)
```

**Expansion with pure marks:**
1. Expansion scope #42 is created
2. Template `tmp` gets marked: `tmp{#42}`
3. User's `tmp` has no marks: `tmp{}`
4. These are different identifiers!
5. Output: `(let ((tmp{#42} x)) (set! x y) (set! y tmp{#42}))`
6. User's `tmp{}` is unaffected

## Lexical Capture

Macros can capture bindings from their lexical definition site via `capturedEnv`:

```scheme
;; Macro captures 'n' from definition site
(let ((n 100))
  (let-syntax ((add-n (syntax-rules ()
                        ((add-n x) (+ x n)))))
    (add-n 5)))  ; → 105

;; Works even with shadowing at use site
(let ((x 100))
  (let-syntax ((get-x (syntax-rules ()
                        ((get-x) x))))
    (let ((x 999))  ; Shadowing doesn't affect macro
      (get-x))))    ; → 100
```

## Comparison Semantics

### bound-identifier=?

Two identifiers are `bound-identifier=?` if they have:
- The same name
- The same set of scope marks

Used for literal matching in patterns.

### free-identifier=?

Two identifiers are `free-identifier=?` if they resolve to the same binding. Used for comparing literals at the use site vs definition site.

## File Structure

| File | Purpose |
|------|---------|
| `syntax_object.js` | `SyntaxObject`, `ScopeBindingRegistry`, scope utilities |
| `syntax_rules.js` | `compileSyntaxRules`, pattern matching, transcription |
| `identifier_utils.js` | Shared identifier helpers (`getIdentifierName`, `isEllipsisIdentifier`) |
| `macro_registry.js` | Global macro name registry |
| `analyzer.js` | S-exp → AST dispatcher |
| `analyzers/core_forms.js` | `analyzeDefineSyntax` and other special forms |
| `analyzers/registry.js` | Central handler registry |

## Related Documentation

- [macro_debugging.md](./macro_debugging.md) — Troubleshooting common macro issues

## References

This implementation draws on ideas from:

- **Matthew Flatt**, "Binding as Sets of Scopes" (POPL 2016). The core "sets of scopes" model where identifiers carry scope sets and binding resolution uses maximal subset matching.

- **R. Kent Dybvig, Robert Hieb, and Carl Bruggeman**, "Syntactic Abstraction in Scheme" (Lisp and Symbolic Computation, 1993). The foundational work on `syntax-case` and hygienic macro expansion with marks.

- **Eugene E. Kohlbecker, Daniel P. Friedman, Matthias Felleisen, and Bruce Duba**, "Hygienic Macro Expansion" (LFP 1986). The original paper introducing hygiene and the concept of preventing accidental capture.

- **William D. Clinger**, "Hygienic Macros Through Explicit Renaming" (Lisp Pointers, 1991). An alternative approach using explicit renaming that influenced early implementations.
