# Hygienic Macro Implementation

## The Two Components of the Hygiene System

### 1. Marks (Scopes) — For Referential Transparency

* **Purpose**: Ensure free variables in macro templates resolve to their meaning at the macro definition site
* **Implemented in**: [syntax_object.js](../src/core/interpreter/syntax_object.js)
* **Mechanism**: Each identifier carries a set of scope IDs that determine which binding it refers to
* **Example**: In `transcribe()` in [syntax_rules.js](../src/core/interpreter/syntax_rules.js), free variables get wrapped with `SyntaxObject` containing the `definingScope`:

```javascript
// Free variable: mark with defining scope for referential transparency
if (definingScope !== null &&
   !SPECIAL_FORMS.has(name) &&
   !globalMacroRegistry.isMacro(name)) {
   return new SyntaxObject(name, new Set([definingScope]));
}
```

### 2. Renaming (Gensym) — For Hygiene Proper

* **Purpose**: Prevent macro-introduced bindings from capturing user variables
* **Implemented in**: `compileSyntaxRules()` and `transcribe()` in [syntax_rules.js](../src/core/interpreter/syntax_rules.js)
* **Mechanism**: Identifiers introduced by the template in binding positions get renamed to fresh symbols
* **Example**: If a macro introduces a `let` binding `tmp`, it gets renamed to `tmp#1`, `tmp#2`, etc.

```javascript
// Generate fresh names for introduced bindings
const renameMap = new Map();
for (const name of introducedBindings) {
   renameMap.set(name, gensym(name));  // tmp → tmp#1
}
```

### 3. Captured Environment — For Lexical Scoping

* **Purpose**: Allow macros to reference bindings from their definition site (not just globals)
* **Implemented in**: `compileSyntaxRules()` via the `capturedEnv` parameter
* **Mechanism**: The syntactic environment at macro definition is captured and used during transcription to resolve free variables

```javascript
// In analyzer.js - analyzeDefineSyntax
const transformer = compileSyntaxRules(literals, clauses, definingScope, ellipsisName, syntacticEnv);
```

This enables patterns like:

```scheme
(let ((n 100))
  (let-syntax ((add-n (syntax-rules ()
                        ((add-n x) (+ x n)))))
    (add-n 5)))  ; → 105
```

## Why You Need All Three

These solve different hygiene problems:

1. **Marks alone** can't prevent capture in all cases:
   ```scheme
   (let-syntax ((bad (syntax-rules () ((bad x) (let ((x 1)) x)))))
     (bad 5))  ; Without renaming: binds x twice!
   ```

2. **Renaming alone** doesn't provide referential transparency:
   ```scheme
   (let ((+ *))
     (let-syntax ((double (syntax-rules () ((double x) (+ x x)))))
       (double 3)))  ; Should use global +, not shadowed *
   ```

3. **Without captured environment**, local bindings can't be referenced:
   ```scheme
   (let ((helper (lambda (x) (* x 2))))
     (define-syntax my-double
       (syntax-rules ()
         ((my-double x) (helper x)))))
   (my-double 5)  ; Should find local helper
   ```

## File Structure

| File | Purpose |
|------|---------|
| [syntax_object.js](../src/core/interpreter/syntax_object.js) | `SyntaxObject`, `ScopeBindingRegistry`, scope utilities |
| [syntax_rules.js](../src/core/interpreter/syntax_rules.js) | `compileSyntaxRules`, pattern matching, transcription |
| [identifier_utils.js](../src/core/interpreter/identifier_utils.js) | Shared identifier helpers (`getIdentifierName`, `isEllipsisIdentifier`) |
| [macro_registry.js](../src/core/interpreter/macro_registry.js) | Global macro name registry |
| [analyzer.js](../src/core/interpreter/analyzer.js) | `analyzeDefineSyntax`, `ScopedVariable` creation |

## The Full Picture

The implementation uses a hybrid approach:

* **Dybvig-style marks** for resolving free references to their definition-site bindings
* **Traditional renaming (gensym)** for introduced bindings to prevent capture
* **Captured environments** for lexical scoping of macro-local bindings

This is a pragmatic and correct design. Pure marks systems (like the full Dybvig algorithm) can eliminate renaming, but they require more complex tracking of mark propagation through all identifiers.

## Advantages of Pure Marks Systems

### 1. Compositional Macro Expansion

* **Problem with gensym**: Renaming happens at expansion time, which can break when macros expand to other macros
* **Marks solution**: All identifiers carry their history through multiple expansion phases

### 2. Correct syntax-case Support

Advanced macros need to manipulate syntax objects directly, compare identifiers, and construct new bindings. Pure marks provide operations like:
* `bound-identifier=?` (do these bind the same?)
* `free-identifier=?` (do these refer to the same thing?)
* `datum->syntax` (create identifiers with specific scopes)

### 3. No Name Pollution

* **Gensym approach**: Creates symbols like `tmp#1`, `tmp#2`, `tmp#3`...
  - These accumulate in the symbol table
  - Can be visible in error messages
* **Pure marks**: Identifiers keep their original names, just with different scope sets
  - Error messages show the original name
  - More debugger-friendly

## Why the Hybrid Approach Makes Sense

Despite these advantages, the hybrid approach is pragmatic because:

1. **Simplicity**: Much easier to implement correctly for `syntax-rules`
2. **No false captures**: Gensym guarantees freshness; marks require careful scope tracking
3. **Good enough**: For `syntax-rules` (vs. full `syntax-case`), the hybrid works perfectly
4. **Incremental path**: Full marks support can be added later if/when `syntax-case` is implemented

## Related Documentation

- [hygiene_implementation.md](./hygiene_implementation.md) — Detailed implementation walkthrough with examples
