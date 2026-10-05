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

## Library Macros

A macro a library defines means the library's bindings, even used in a program
that cannot name them. Three mechanisms carry that across the library boundary.

### The library's scope

Each library has a scope of its own, registered with its environment before
its imports are applied (`library_loader.js`). A macro defined while the
library loads marks every identifier its templates introduce with that scope as
well as the expansion's (`mark-introduced` in `syntax_rules.scm`), unless the
identifier already carries a library's scope -- it was written in another
library, whose macro wrote this one. `libraryScopeOf` reads it back.

### References and assignments

A free identifier carrying a library's scope is the library's binding of its
name (`library-binding-env` in `expander.scm`), wherever the macro is used:
outside the library it becomes a `library-var` core form -- a
`LibraryVariableNode`, or for `set!` a `LibrarySetNode` -- which reaches into
the library's environment. So a program that redefines or assigns `eqv?`
does not change what `case` does, as R7RS 4.3 requires: the macro's `eqv?` is
its library's. Within the library itself, and for a name the library does not
bind, it is a plain global reference. The compiler compiles a library
reference as a global of its own, keyed by the name and the library
(`eqv?@scheme.control`), read through the library's environment; a prebuilt
table writes the environment by the library's name, which whoever restores
the table finds -- a registry, or the library system's seed among its own
libraries.

```scheme
(define-library (counter)
  (export count!)
  (import (scheme base))
  (begin
    (define n 0)                        ; not exported
    (define-syntax count!
      (syntax-rules () ((_) (begin (set! n (+ n 1)) n))))))

(import (scheme base) (counter))
(count!)   ; => 1, though the program cannot name n
```

### Keyword bindings

Macros are defined by name, for the whole process (`macro_registry.js`), so
each library -- and a program's top level -- also binds the keywords it has:
the macros it defines, and every macro or keyword it imports, under the name it
imports it as (`InterpreterContext.defineKeyword`). A keyword's binding keeps
the macro's transformer as it was, so `(import (rename (scheme base)
(quasiquote std-quasiquote)))` still names the standard `quasiquote` after the
library defines its own, and two libraries' internal macros of the same name
each expand into their own. The expander looks an operator up in local macros
(`let-syntax`, a body's `define-syntax`) first, then in the bindings where the
identifier is used (`operator-keyword`), then by name; a pattern literal is
compared by the keyword each side names (`keyword-name`), so a literal written
`ellipsis` in a library that imported `...` as `ellipsis` matches the user's
`...`.

A name that nothing binds is still found by name, so a library that defines a
macro for a keyword other code uses without importing -- `(scheme core)` uses
`quasiquote` -- reaches that code too.

## Procedural Macros

### er-macro-transformer

A macro whose transformer is a procedure, by explicit renaming (Clinger,
1991): `(er-macro-transformer (lambda (form rename compare) ...))`, in
`define-syntax`, `let-syntax` and `letrec-syntax`. The procedure is given the
use as written and returns what it expands into, which is used as it is: a
symbol it makes up is the user's, found where the macro is used, and one it
passed through `rename` is the macro's. `rename` does to an identifier what a
`syntax-rules` template does to one it introduces -- marks it with the
expansion's scope, and the library's if a library defined the macro, or makes
it the local it names where the macro was defined -- so a binding it makes
captures nothing of the user's and a reference it makes means what it did
where the macro was defined (`explicit_renaming.scm`). `compare` is
`free-identifier=?` where the macro is used.

```scheme
(define-syntax swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (car (cdr form))) (b (car (cdr (cdr form)))))
        (list (rename 'let) (list (list (rename 'tmp) a))
              (list (rename 'set!) a b)
              (list (rename 'set!) b (rename 'tmp)))))))
```

The procedure is evaluated as the macro is defined, in the environment of the
library or program defining it: it sees what that imports and what it defined
before, and a library can share its own procedures with its macros. There is
no phase of its own, as in Chibi, Gauche and Guile, so nothing is loaded
twice; the price is that expansion can see the library's state as it runs.

### define-macro

`(define-macro (name . formals) body ...)`, kept for code written for other
Lisps, is an explicit-renaming macro that renames nothing: its procedure is
applied to the use's operands, and what it returns is used as it is, so a
binding it makes captures the user's and a name it refers to means whatever
the use site binds it to. New code should use `syntax-rules`, or
`er-macro-transformer` and `rename`.

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
| `src/core/scheme/expander.scm` | The expander: environments, keywords, the special forms, `define-syntax` and `define-macro` |
| `src/core/scheme/syntax_rules.scm` | `syntax-rules`: pattern matching, transcription, marks |
| `src/core/scheme/explicit_renaming.scm` | `er-macro-transformer`: `rename` and `compare` |
| `src/core/interpreter/syntax_object.js` | `SyntaxObject`, the identifier; `ScopeBindingRegistry`; walks of a datum that mark scopes |
| `src/core/primitives/expander_support.js` | What the expander needs of the host: identifiers, scopes, the keyword tables |
| `src/core/interpreter/macro_registry.js` | The macros defined by name for the process |

## Related Documentation

- [macro_debugging.md](./macro_debugging.md) — Troubleshooting common macro issues

## References

This implementation draws on ideas from:

- **Matthew Flatt**, "Binding as Sets of Scopes" (POPL 2016). The core "sets of scopes" model where identifiers carry scope sets and binding resolution uses maximal subset matching.

- **R. Kent Dybvig, Robert Hieb, and Carl Bruggeman**, "Syntactic Abstraction in Scheme" (Lisp and Symbolic Computation, 1993). The foundational work on `syntax-case` and hygienic macro expansion with marks.

- **Eugene E. Kohlbecker, Daniel P. Friedman, Matthias Felleisen, and Bruce Duba**, "Hygienic Macro Expansion" (LFP 1986). The original paper introducing hygiene and the concept of preventing accidental capture.

- **William D. Clinger**, "Hygienic Macros Through Explicit Renaming" (Lisp Pointers, 1991). Explicit renaming, which `er-macro-transformer` implements.
