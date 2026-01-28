# Macro Debugging Guide

A practical troubleshooting guide for common `syntax-rules` macro issues.

## Quick Reference

| Symptom | Likely Cause | Solution |
|---------|--------------|----------|
| "Unbound variable" | Missing global or shadowed binding | Check if identifier is a pattern variable or global |
| Wrong value captured | Accidental variable capture | Use explicit names or avoid binding-position conflicts |
| Literal not matching | Wrong literal in `syntax-rules` | Ensure literal is in the literals list |
| Infinite expansion | Recursive macro without base case | Add a non-recursive pattern before the recursive one |

---

## Symptom: "Unbound variable" in Macro Expansion

### Cause 1: Identifier is not a pattern variable

```scheme
;; WRONG: 'body' is never bound
(define-syntax my-do
  (syntax-rules ()
    ((my-do e) body)))  ; 'body' appears in template but not in pattern!

;; CORRECT: 'body' must appear in pattern
(define-syntax my-do
  (syntax-rules ()
    ((my-do body) body)))  ; Now 'body' is a pattern variable
```

### Cause 2: Free variable shadowed in user code

```scheme
;; Macro uses '+' from definition site
(define-syntax double
  (syntax-rules ()
    ((double x) (+ x x))))

;; Works fine:
(double 5)  ; → 10

;; Still works! Macro captures global '+', not shadowed one:
(let ((+ *))
  (double 5))  ; → 10, NOT 25
```

> **Tip**: Free variables (like `+`) in macro templates resolve at the *macro definition site*, not the use site. This is called **referential transparency**.

---

## Symptom: Wrong Value Captured (Accidental Capture)

This happens when a macro-introduced binding accidentally shadows a user variable. **Without hygiene**, this would be a problem:

```scheme
;; Hypothetical BAD behavior (in a non-hygienic system):
(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((tmp a))        ; If user passes 'tmp' as 'a'...
       (set! a b)
       (set! b tmp)))))

;; In a non-hygienic system, this would FAIL:
(let ((x 1) (y 2) (tmp 3))
  (swap! tmp y)
  tmp)  ; Expected 2, but might get wrong value
```

**How our implementation handles this (Pure Marks):**

The macro introduces `tmp` in a binding position. Our system uses **scope marks** instead of renaming:
1. Every macro expansion gets a unique scope ID (e.g., `#42`)
2. Template identifiers get marked with this scope: `tmp{#42}`
3. User's `tmp` has different marks: `tmp{}`
4. They are treated as **different identifiers** because their scope sets differ

The user's `tmp` is not captured because `tmp{#42} ≠ tmp{}`.

> **Note**: This automatic scope marking means you generally don't need to worry about capture in `syntax-rules` macros. The hygiene system handles it without any renaming.

---

## Symptom: Literal Not Matching

Literals in `syntax-rules` must be explicitly listed:

```scheme
;; WRONG: 'else' is matched as a pattern variable, not literal
(define-syntax my-cond
  (syntax-rules ()  ; Missing 'else' in literals list!
    ((my-cond (else result)) result)
    ((my-cond (test result)) (if test result))))

(my-cond (else 42))  ; 'else' binds to variable, not literal

;; CORRECT: 'else' in literals list
(define-syntax my-cond
  (syntax-rules (else)  ; NOW 'else' is a literal
    ((my-cond (else result)) result)
    ((my-cond (test result)) (if test result))))
```

---

## Symptom: Infinite Macro Expansion

Recursive macros need a base case that doesn't recurse:

```scheme
;; WRONG: Always expands to another 'my-and' call
(define-syntax my-and
  (syntax-rules ()
    ((my-and e1 e2 ...) (if e1 (my-and e2 ...) #f))))

;; This never terminates:
(my-and #t #t)

;; CORRECT: Base cases first
(define-syntax my-and
  (syntax-rules ()
    ((my-and) #t)                        ; Base: no args
    ((my-and e) e)                       ; Base: one arg
    ((my-and e1 e2 ...) (if e1 (my-and e2 ...) #f))))
```

> **Rule**: Pattern matching tries clauses in order. Put non-recursive patterns *before* recursive ones.

---

## Debugging Tips

### 1. Simplify the macro

Start with the simplest version that should work:

```scheme
;; Start here
(define-syntax test-macro
  (syntax-rules ()
    ((test-macro x) x)))

;; Then add complexity incrementally
```

### 2. Check pattern variable bindings

Each identifier in the template must be either:
- A pattern variable (appears in the pattern)
- A literal (in the literals list)
- A free reference (global procedure/special form)

### 3. Test pattern matching

Ensure your patterns match as expected:

```scheme
;; Does this pattern match?
((my-macro a b c) ...)

;; Try calling:
(my-macro 1 2 3)  ; Should match 3 args
```

### 4. Use the REPL incrementally

Test each expansion step:

```scheme
;; Instead of complex nested macros, test components:
(define-syntax inner ...)
(inner test-input)  ; Check this works first

(define-syntax outer ...)  ; Then build on top
```

---

## Related Documentation

- [hygiene.md](./hygiene.md) — Implementation details: pure marks/scopes algorithm
