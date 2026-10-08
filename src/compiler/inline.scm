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
;;; emitter declares the helper where it is used; and one that is a pair, of
;;; such a name and a procedure, is an expression the procedure makes of the
;;; operands, the helper's name and the operands' IR nodes, for a fast path
;;; that calls the helper only for some of its operands. `applies?`, where there is
;;; one, takes the operands' IR nodes and decides at compile time whether to
;;; expand the call at all, for an expansion that is only worth having for some
;;; operands.

;; /**
;;  * The JavaScript literal of an operand that is an inexact real constant,
;;  * which is a double in arithmetic, though an integral one is boxed where it
;;  * is a value (`constant` in emit.scm); #f for any other operand.
;;  * @param {list} node - The operand's IR node.
;;  * @returns {list|boolean} An expression, or #f.
;;  */
(define (inexact-literal node)
  (and (eq? (car node) 'const)
       (let ((v (cadr node)))
         (and (real? v) (inexact? v) (js "(" (js-number v) ")")))))

;; /**
;;  * Expressions joined by `&&`, or `true` for none.
;;  * @param {list} exprs - The expressions.
;;  * @returns {list} An expression.
;;  */
(define (all-of exprs)
  (if (null? exprs)
      (js "true")
      (fold (lambda (e acc) (js acc " && " e)) (car exprs) (cdr exprs))))

;; /**
;;  * A binary operation one of whose operands is an inexact constant: inline
;;  * on the other's double, whether that operand is a number or a box, since
;;  * the result is inexact whatever it is -- `fibfp` in benchmarks/r7rs/src
;;  * passes boxes to `(- n 1.)` and `(< n 2.)` on every call -- and otherwise,
;;  * for a BigInt, a rational, a complex number or a wrong type, the runtime's.
;;  * Two constants are computed on their doubles alone.
;;  * @param {list} ops - The operand expressions.
;;  * @param {list} nodes - Their IR nodes, at least one an inexact constant.
;;  * @param {string} op - The JavaScript operator.
;;  * @param {symbol} helper - The runtime operation's local name.
;;  * @param {procedure} finish - The result made of the operator's expression
;;  *   on the two doubles.
;;  * @returns {list} An expression.
;;  */
(define (against-inexact-constant ops nodes op helper finish)
  (let* ((ka (inexact-literal (car nodes)))
         (kb (inexact-literal (cadr nodes)))
         (on (lambda (a b) (finish (js (or ka a) " " op " " (or kb b))))))
    (if (and ka kb)
        (on ka kb)
        (let ((x (if ka (cadr ops) (car ops)))
              (on-x (lambda (v) (if ka (on ka v) (on v kb)))))
          (js "(typeof " x " === 'number' ? " (on-x x)
              " : " x " instanceof R.Flonum ? " (on-x (js x ".value"))
              " : " helper "(" (car ops) ", " (cadr ops) "))")))))

;; /**
;;  * Whether either of two operands is an inexact constant.
;;  * @param {list} nodes - The operands' IR nodes.
;;  * @returns {boolean}
;;  */
(define (inexact-constant-operand? nodes)
  (or (inexact-literal (car nodes)) (inexact-literal (cadr nodes))))

;; /**
;;  * A binary arithmetic operation: inline, the JavaScript operator on two
;;  * numbers whose result needs no deciding -- one that is not an integer, an
;;  * inexact real, or an integer in the safe range of two integers, an exact
;;  * integer -- which V8 keeps in a register; otherwise the runtime's, which
;;  * decides exactness, boxes an inexact integer, takes BigInts and boxes, and
;;  * hands anything else to the primitive (`add` and the rest in
;;  * src/compiler/runtime.js). A product of zero is decided by the runtime,
;;  * which gives exact zero rather than JavaScript's -0. Against an inexact
;;  * constant the result is inexact, and is boxed where it is an integer
;;  * (`against-inexact-constant`).
;;  * @param {string} name - The Scheme name.
;;  * @param {string} op - The JavaScript operator.
;;  * @param {symbol} local - The runtime operation's local name.
;;  * @returns {list} A table entry.
;;  */
(define (numeric-arithmetic name op local)
  (list name 2 (lambda (ops) #f)
        (cons local
              (lambda (ops helper nodes)
                (if (inexact-constant-operand? nodes)
                    (against-inexact-constant ops nodes op helper
                                              (lambda (e) (js "R.inexactReal(" e ")")))
                    (let* ((a (car ops)) (b (cadr ops))
                           (r (js "(" a " " op " " b ")")))
                      (js "(typeof " a " === 'number' && typeof " b " === 'number'"
                          " && (!Number.isInteger(" r ")"
                          " || (Number.isSafeInteger(" r ")"
                          (if (string=? op "*") (js " && " r " !== 0") (js ""))
                          " && Number.isInteger(" a ") && Number.isInteger(" b "))"
                          ")) ? " r " : " helper "(" a ", " b ")")))))))

;; /**
;;  * A numeric comparison: inline, the JavaScript operator on two numbers,
;;  * which compares across exactness as the tower does -- `=` as `===` agrees
;;  * on NaN, and every ordering with a NaN is false -- and against an inexact
;;  * constant on a box's double too (`against-inexact-constant`); otherwise
;;  * the runtime's (`lt` and the rest in src/compiler/runtime.js).
;;  * @param {string} name - The Scheme name.
;;  * @param {string} op - The JavaScript operator.
;;  * @param {symbol} local - The runtime operation's local name.
;;  * @returns {list} A table entry.
;;  */
(define (numeric-comparison name op local)
  (list name 2 (lambda (ops) #f)
        (cons local
              (lambda (ops helper nodes)
                (if (inexact-constant-operand? nodes)
                    (against-inexact-constant ops nodes op helper (lambda (e) e))
                    (let ((a (car ops)) (b (cadr ops)))
                      (js "(typeof " a " === 'number' && typeof " b " === 'number') ? "
                          a " " op " " b " : " helper "(" a ", " b ")")))))))

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
;;  * When an exact division's fast path holds: two integers held as numbers --
;;  * not an integral inexact, which is a box, nor a BigInt -- and a divisor
;;  * that is not zero, for which `%` is exact.
;;  * @param {list} ops - The operands.
;;  * @returns {list} An expression.
;;  */
(define (exact-division-test ops)
  (let ((a (car ops)) (b (cadr ops)))
    (js "typeof " a " === 'number' && typeof " b " === 'number'"
        " && Number.isInteger(" a ") && Number.isInteger(" b ") && " b " !== 0")))

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
;;  * An operand as what a property is read of: in parentheses if it is a
;;  * number written out, since JavaScript reads `1.car` as a malformed number
;;  * and `-2.cdr` as the negation of a property of 2.
;;  * @param {list} op - The operand's expression.
;;  * @returns {list} An expression.
;;  */
(define (property-object op)
  (let ((first (and (pair? op) (car op))))
    (if (and (string? first)
             (> (string-length first) 0)
             (let ((c (string-ref first 0)))
               (or (char-numeric? c) (char=? c #\-))))
        (js "(" op ")")
        op)))

;; /**
;;  * Every primitive with an inline expansion.
;;  */
(define inline-expansions
  (list
    ;; Arithmetic and comparison: inline on two numbers, the runtime's
    ;; otherwise, tower fallback.
    (numeric-arithmetic '+ "+" '$add)
    (numeric-arithmetic '- "-" '$sub)
    (numeric-arithmetic '* "*" '$mul)
    (numeric-comparison '< "<" '$lt)
    (numeric-comparison '> ">" '$gt)
    (numeric-comparison '<= "<=" '$le)
    (numeric-comparison '>= ">=" '$ge)
    (numeric-comparison '= "===" '$numEq)
    ;; Pairs: the representation is a plain class, so these are direct.
    (list 'car 1
          (lambda (ops) (js (car ops) " instanceof R.Cons"))
          (lambda (ops) (js (property-object (car ops)) ".car")))
    (list 'cdr 1
          (lambda (ops) (js (car ops) " instanceof R.Cons"))
          (lambda (ops) (js (property-object (car ops)) ".cdr")))
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
          (lambda (ops) (js (property-object (car ops)) ".length")))
    ;; Exact division, inline on two integers and a divisor that is not zero,
    ;; as the primitives' own fast paths are (`dividingIntegers` in
    ;; src/core/primitives/math.js), the primitive otherwise: V8 in Node inlines
    ;; the primitive into compiled code, and V8 in Chrome does not, where
    ;; `benchmarks/r7rs/src/bv2string.scm` spent nearly half its time calling
    ;; it. `+ 0` makes JavaScript's -0 exact zero; `modulo` takes the sign of
    ;; the divisor, adding it to a remainder of the other sign.
    (list 'quotient 2 exact-division-test
          (lambda (ops) (let ((a (car ops)) (b (cadr ops)))
                          (js "((" a " - " a " % " b ") / " b " + 0)"))))
    (list 'remainder 2 exact-division-test
          (lambda (ops) (js "(" (car ops) " % " (cadr ops) " + 0)")))
    (list 'modulo 2 exact-division-test
          (lambda (ops) (let ((a (car ops)) (b (cadr ops)))
                          (js "((" a " % " b ") * " b " < 0 ? " a " % " b " + " b " : " a " % " b " + 0)"))))
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
