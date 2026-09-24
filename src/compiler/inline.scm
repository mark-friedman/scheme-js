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
;;; An entry is (name arity test value). `test` and `value` take the operands,
;;; as expressions in the emitter's representation (see `emit.scm`), and
;;; return an expression: `test` the condition under which the fast path is
;;; valid, or #f when it always is; `value` the fast path itself.

;; /**
;;  * The condition that both operands are exact integers, the dominant case.
;;  * @param {list} a - An operand expression.
;;  * @param {list} b - An operand expression.
;;  * @returns {list} An expression.
;;  */
(define (both-exact a b)
  (js "typeof " a " === 'bigint' && typeof " b " === 'bigint'"))

;; /**
;;  * An exact-integer binary operator: the operator applied when both operands
;;  * are BigInts, the tower otherwise.
;;  * @param {string} name - The Scheme name.
;;  * @param {string} op - The JavaScript operator.
;;  * @returns {list} A table entry.
;;  */
(define (exact-binary name op)
  (list name 2
        (lambda (ops) (both-exact (car ops) (cadr ops)))
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
;;  * Every primitive with an inline expansion.
;;  */
(define inline-expansions
  (list
    ;; Arithmetic: exact-integer fast path, tower fallback.
    (exact-binary '+ "+")
    (exact-binary '- "-")
    (exact-binary '* "*")
    (exact-binary '< "<")
    (exact-binary '> ">")
    (exact-binary '<= "<=")
    (exact-binary '>= ">=")
    (exact-binary '= "===")
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
    (total 'eq? 2 (lambda (ops) (js (car ops) " === " (cadr ops))))))

;; /**
;;  * The expansion for a call, if there is one for this primitive at this
;;  * argument count.
;;  * @param {symbol} name - The global called.
;;  * @param {integer} argc - The number of arguments.
;;  * @returns {list|boolean} The table entry, or #f.
;;  */
(define (inline-expansion name argc)
  (let ((entry (assq name inline-expansions)))
    (if (and entry (= (cadr entry) argc)) entry #f)))

;; /**
;;  * The names that have an expansion at any arity, for the JavaScript side
;;  * to find out which of them are bound to their primitive.
;;  */
(define (inline-expansion-names) (map car inline-expansions))
