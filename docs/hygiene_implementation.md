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

`transcribe()` builds the output:
- **Pattern variables** → substituted with matched input
- **Introduced bindings** → renamed to gensyms
- **Free variables** → wrapped in `SyntaxObject` with defining scope

```javascript
if (bindings.has(name)) return bindings.get(name);
if (renameMap.has(name)) return renameMap.get(name);
if (definingScope !== null) {
  return new SyntaxObject(name, new Set([definingScope]));
}
```

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

## Known Limitations

Local definition-site bindings are NOT handled:

```scheme
(let ((helper (lambda (x) (* x 2))))
  (define-syntax my-double
    (syntax-rules ()
      ((my-double x) (helper x)))))

(let ((helper (lambda (x) (+ x 1))))
  (my-double 5))  ; Returns 6, not 10
```

**Why this is rare:**
- `define-syntax` is almost always at top level
- Macros typically reference only global names
- Special forms are immune (recognized syntactically)

## File Structure

| File | Purpose |
|------|---------|
| `syntax_object.js` | `SyntaxObject`, `ScopeBindingRegistry`, scope utilities |
| `syntax_rules.js` | `compileSyntaxRules`, pattern matching, transcription |
| `macro_registry.js` | Global macro name registry |
| `analyzer.js` | `analyzeDefineSyntax`, `ScopedVariable` creation |

## Related Documentation

- [README.md](../README.md#limitations) — Documents the hygiene limitation
- [docs/hygeine.md](./hygeine.md) — Additional hygiene notes
