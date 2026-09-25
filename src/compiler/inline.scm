;;; inline.scm -- inline expansions for primitives, used by the emitter.
;;;
;;; Profiling the first compiler tier put 25% of its runtime in primitive calls
;;; and only 17% in the generated code itself. A call such as `(+ a b)` was
;;; going through a variadic primitive that allocates a rest array, type-checks
;;; each argument and dispatches across the numeric tower -- all to add two
;;; integers.
;;;
;;; Each entry here gives a fast path for the overwhelmingly common operand
;;; shape and falls back to the real primitive for everything else, so the
;;; numeric tower's semantics are preserved rather than approximated: a
;;; rational, a flonum, a complex or a wrong type all take the fallback and
;;; behave exactly as they do in the interpreter.
;;;
;;; Every expansion is also guarded on the binding, by the emitter. Scheme
;;; allows `+` to be redefined, so an expansion is used only while `+` still
;;; denotes the primitive, and otherwise calls through whatever `+` is bound to
;;; now. The guard is normally one property load, on a cell the interpreter
;;; clears the first time anything rebinds the name
;;; (`src/core/interpreter/primitive_bindings.js`).
;;;
;;; An entry is (name arity test value), or (name arity test value applies?).
;;; `test` and `value` take the operands, as expressions in the emitter's
;;; representation (see `emit.scm`), and return an expression: `test` the
;;; run-time condition under which the fast path is valid, or #f when it always
;;; is; `value` the fast path itself. `applies?`, where there is one, takes the
;;; operands' IR nodes and decides at compile time whether to expand the call
;;; at all, for an expansion that is only worth having for some operands.

;; /**
;;  * The condition that both operands are exact integers or both are inexact
;;  * reals -- JavaScript `bigint`s or JavaScript `number`s -- for either of which
;;  * a JavaScript operator computes what the numeric tower does. Every other
;;  * pair -- mixed exactness, a rational, a complex, a wrong type -- takes the
;;  * primitive. Exact integers are tested first, so the exact path costs what it
;;  * did before flonums had one; without theirs, flonum arithmetic went through
;;  * the variadic primitive and was most of `fibfp`'s run time.
;;  * @param {list} a - An operand expression.
;;  * @param {list} b - An operand expression.
;;  * @returns {list} An expression.
;;  */
(define (both-exact-or-both-inexact a b)
  (js "(typeof " a " === 'bigint' && typeof " b " === 'bigint') || "
      "(typeof " a " === 'number' && typeof " b " === 'number')"))

;; /**
;;  * A binary arithmetic operator: the JavaScript operator applied when both
;;  * operands are exact integers or both are flonums, the tower otherwise.
;;  * For two flonums the JavaScript result is the tower's too: `=` as `===`
;;  * agrees on `-0.0` and on NaN, and every ordering with a NaN is false.
;;  * @param {string} name - The Scheme name.
;;  * @param {string} op - The JavaScript operator.
;;  * @returns {list} A table entry.
;;  */
(define (numeric-binary name op)
  (list name 2
        (lambda (ops) (both-exact-or-both-inexact (car ops) (cadr ops)))
        (lambda (ops) (js (car ops) " " op " " (cadr ops)))))

;; /**
;;  * An expansion whose fast path is exactly what the primitive computes for
;;  * every input, so only the binding guard applies.
;;  * @param {string} name - The Scheme name.
;;  * @param {integer} arity - Its argument count.
;;  * @param {procedure} value - Operands to the fast-path expression.
;;  * @returns {list} A table entry.
;;  */
(define (total name arity value)
  (list name arity (lambda (ops) #f) value))

;; /**
;;  * Whether an operand is a constant whose identity is its value: a symbol, a
;;  * boolean or the empty list. `eqv?` against one is JavaScript `===`, since
;;  * symbols are interned and the other two are immediates; against a number
;;  * or a character it is not, since those compare by value.
;;  * @param {list} node - An operand's IR node.
;;  * @returns {boolean}
;;  */
(define (identity-constant? node)
  (and (eq? (car node) 'const)
       (let ((value (cadr node)))
         (or (symbol? value) (boolean? value) (null? value)))))

;; /**
;;  * Every primitive with an inline expansion.
;;  */
(define inline-expansions
  (list
    ;; Arithmetic: exact-integer and flonum fast paths, tower fallback.
    (numeric-binary '+ "+")
    (numeric-binary '- "-")
    (numeric-binary '* "*")
    (numeric-binary '< "<")
    (numeric-binary '> ">")
    (numeric-binary '<= "<=")
    (numeric-binary '>= ">=")
    (numeric-binary '= "===")
    ;; Pairs: the representation is a plain class, so these are direct.
    (list 'car 1
          (lambda (ops) (js (car ops) " instanceof R.Cons"))
          (lambda (ops) (js (car ops) ".car")))
    (list 'cdr 1
          (lambda (ops) (js (car ops) " instanceof R.Cons"))
          (lambda (ops) (js (car ops) ".cdr")))
    (total 'cons 2 (lambda (ops) (js "new R.Cons(" (car ops) ", " (cadr ops) ")")))
    (total 'pair? 1 (lambda (ops) (js (car ops) " instanceof R.Cons")))
    (total 'null? 1 (lambda (ops) (js (car ops) " === null")))
    (total 'not 1 (lambda (ops) (js (car ops) " === false")))
    (total 'eq? 2 (lambda (ops) (js (car ops) " === " (cadr ops))))
    ;; Only against a constant `===` is exact for. That is what `case` tests
    ;; its key with, one datum at a time; anything else calls the primitive.
    (list 'eqv? 2
          (lambda (ops) #f)
          (lambda (ops) (js (car ops) " === " (cadr ops)))
          (lambda (args) (any identity-constant? args)))))

;; /**
;;  * The expansion for a call, if this primitive has one for these operands.
;;  * @param {symbol} name - The global called.
;;  * @param {list} args - The operands' IR nodes.
;;  * @returns {list|boolean} The table entry, or #f.
;;  */
(define (inline-expansion name args)
  (let ((entry (assq name inline-expansions)))
    (and entry
         (= (cadr entry) (length args))
         (let ((applies? (cddddr entry)))
           (or (null? applies?) ((car applies?) args)))
         entry)))

;; /**
;;  * The names that have an expansion at any arity, for the JavaScript side
;;  * to find out which of them are bound to their primitive.
;;  */
(define (inline-expansion-names) (map car inline-expansions))
