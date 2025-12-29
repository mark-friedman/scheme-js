# Hygiene Implementation

This document explains how macro hygiene is implemented in the Scheme interpreter.

## Overview

The implementation uses **Dybvig-style scopes** combined with **alpha-renaming (gensyms)** to achieve hygiene. This prevents:

1. **Accidental capture** — Macro-introduced bindings don't capture user variables
2. **Referential transparency** — Free variables in templates resolve in their definition context

## Key Concepts

### Scope Marks

Each identifier can carry a set of **scope marks** (integers). When macros expand:
- Introduced bindings get fresh gensym names
- Free variables get marked with the macro's **defining scope**

```javascript
// In syntax_object.js
class SyntaxObject {
  constructor(name, scopes = new Set()) {
    this.name = name;    // Original identifier name
    this.scopes = scopes; // Set<number> of scope marks
  }
}
```

### ScopeBindingRegistry

The registry maps `(name, scopes) → binding`. Resolution finds the binding whose scopes are a **maximal subset** of the identifier's scopes:

```javascript
// Simplified from syntax_object.js
resolve(syntaxObj) {
  const candidates = this.bindings.get(syntaxObj.name);
  // Find binding with largest scope set that is subset of syntaxObj.scopes
  return bestMatch;
}
```

## Expansion Process

### 1. Pattern Matching

`matchPattern()` matches input against the macro pattern, collecting pattern variable bindings.

### 2. Finding Introduced Bindings

`findIntroducedBindings()` traverses the template to find symbols in **binding positions** (let, lambda, define) that are:
- NOT pattern variables (would be substituted from input)
- NOT special forms (recognized by analyzer)

### 3. Alpha-Renaming

For each introduced binding, generate a unique gensym:

```javascript
// In syntax_rules.js
const renameMap = new Map();
for (const name of introducedBindings) {
  renameMap.set(name, gensym(name)); // e.g., "tmp" → "tmp#42"
}
```

### 4. Transcription

`transcribe()` builds the output. Each invocation receives an `expansionScope` (unique per expansion):

- **Pattern variables** → substituted with matched input (with scope flip applied)
- **Introduced bindings** → renamed to gensyms  
- **Free variables** → wrapped in `SyntaxObject` with the expansion scope

```javascript
// In transcribe() in syntax_rules.js
if (bindings.has(name)) return flipScopeInExpression(bindings.get(name), expansionScope);
if (renameMap.has(name)) return renameMap.get(name);
if (expansionScope !== null) {
  return new SyntaxObject(name, new Set([expansionScope]));
}
```

### Scope Types

- **`definingScope`** — Where the macro was defined. Used to look up the definition environment for literal comparison and lexical capture.
- **`expansionScope`** — Fresh scope created for each macro expansion. Used to mark free variables for hygiene.

## Resolution at Analysis Time

When the analyzer encounters a `ScopedVariable` (created from a `SyntaxObject`), it:
1. Checks the `ScopeBindingRegistry` for a scoped binding
2. Falls back to the runtime `Environment` if not found

```javascript
// In stepables.js - ScopedVariable.step()
const scopedBinding = this.scopeRegistry.resolve(syntaxObj);
if (scopedBinding !== null) {
  registers[ANS] = scopedBinding;
  return false;
}
// Fall back to environment lookup
registers[ANS] = registers[ENV].lookup(this.name);
```

## Example

```scheme
(define-syntax swap
  (syntax-rules ()
    ((swap a b)
     (let ((tmp a))   ; 'tmp' is introduced
       (set! a b)
       (set! b tmp)))))

(let ((tmp 100))
  (let ((x 1) (y 2))
    (swap x y)
    tmp)) ; → 100 (not captured!)
```

**Expansion:**
1. `findIntroducedBindings` finds `tmp` in binding position
2. `renameMap` gets `{ "tmp" → "tmp#1" }`
3. Output becomes `(let ((tmp#1 a)) (set! a b) (set! b tmp#1))`
4. User's `tmp` is unaffected

## Lexical Capture

Macros can capture bindings from their lexical definition site. This is enabled by passing the `capturedEnv` (syntactic environment) to `compileSyntaxRules`:

```javascript
// In analyzer.js - analyzeDefineSyntax
const transformer = compileSyntaxRules(literals, clauses, definingScope, ellipsisName, syntacticEnv);
```

During transcription, identifiers that resolve in the captured environment are properly scoped:

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

## File Structure

| File | Purpose |
|------|---------|
| `syntax_object.js` | `SyntaxObject`, `ScopeBindingRegistry`, scope utilities |
| `syntax_rules.js` | `compileSyntaxRules`, pattern matching, transcription |
| `identifier_utils.js` | Shared identifier helpers (`getIdentifierName`, `isEllipsisIdentifier`) |
| `macro_registry.js` | Global macro name registry |
| `analyzer.js` | `analyzeDefineSyntax`, `ScopedVariable` creation |

## Related Documentation

- [docs/hygiene.md](./hygiene.md) — Additional hygiene notes
