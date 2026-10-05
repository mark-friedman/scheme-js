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
;;; is; `value` the fast path itself. A `value` that is a symbol instead names
;;; the runtime helper the fast path calls with the operands, by the local name
;;; generated code knows it by (`runtime-constants` in `emit.scm`), so that the
;;; emitter declares the helper where it is used. `applies?`, where there is
;;; one, takes the operands' IR nodes and decides at compile time whether to
;;; expand the call at all, for an expansion that is only worth having for some
;;; operands.

;; /**
;;  * The condition that both operands are JavaScript numbers: exact integers in
;;  * the safe range, or inexact reals that are not integers
;;  * (src/core/interpreter/number_representation.js). Every other operand -- a
;;  * BigInt, an inexact integer, which is boxed, a rational, a complex, a wrong
;;  * type -- takes the primitive.
;;  * @param {list} a - An operand expression.
;;  * @param {list} b - An operand expression.
;;  * @returns {list} An expression.
;;  */
(define (both-numbers a b)
  (js "typeof " a " === 'number' && typeof " b " === 'number'"))

;; /**
;;  * A numeric comparison: the JavaScript operator when both operands are
;;  * numbers, which compares across exactness as the tower does: `=` as `===`
;;  * agrees on NaN, and every ordering with a NaN is false.
;;  * @param {string} name - The Scheme name.
;;  * @param {string} op - The JavaScript operator.
;;  * @returns {list} A table entry.
;;  */
(define (numeric-comparison name op)
  (list name 2
        (lambda (ops) (both-numbers (car ops) (cadr ops)))
        (lambda (ops) (js (car ops) " " op " " (cadr ops)))))

;; /**
;;  * A binary arithmetic operation: when both operands are numbers, the
;;  * runtime's arithmetic on two numbers, which gives an exact result where both
;;  * are exact and boxes an inexact integer (`addNumbers` in
;;  * number_representation.js); the tower otherwise.
;;  * @param {string} name - The Scheme name.
;;  * @param {symbol} local - The runtime helper's local name.
;;  * @returns {list} A table entry.
;;  */
(define (numeric-arithmetic name local)
  (list name 2 (lambda (ops) (both-numbers (car ops) (cadr ops))) local))

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
;;  * An expansion whose fast path calls a runtime helper with the operands, the
;;  * helper doing the primitive's work for every input or handing it to the
;;  * primitive, so only the binding guard applies.
;;  * @param {string} name - The Scheme name.
;;  * @param {integer} arity - Its argument count.
;;  * @param {symbol} local - The helper's local name in generated code.
;;  * @returns {list} A table entry.
;;  */
(define (helper name arity local)
  (total name arity local))

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
    ;; Arithmetic: fast paths for two numbers, tower fallback.
    (numeric-arithmetic '+ '$add)
    (numeric-arithmetic '- '$sub)
    (numeric-arithmetic '* '$mul)
    (numeric-comparison '< "<")
    (numeric-comparison '> ">")
    (numeric-comparison '<= "<=")
    (numeric-comparison '>= ">=")
    (numeric-comparison '= "===")
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
    ;; Vectors are JavaScript arrays. An access calls a runtime helper rather
    ;; than the primitive through the generic call path, which cost
    ;; vector-heavy programs up to half their time; the helper checks the
    ;; common case and passes everything else to the primitive, so every error
    ;; is its own.
    (helper 'vector-ref 2 '$vectorRef)
    (helper 'vector-set! 3 '$vectorSet)
    (list 'vector-length 1
          (lambda (ops) (js "Array.isArray(" (car ops) ")"))
          (lambda (ops) (js (car ops) ".length")))
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
