;; The emitter's text and the lifting plan (src/compiler/emit.scm, lift.scm)
;;
;; Runs in the compiler's own environment. What the emitter generates as a
;; whole is checked by running it: every compiled procedure in the test suite
;; and the benchmark programs. These check the pieces whose mistakes would not
;; always show up that way -- a string escaped wrongly, a literal repeated that
;; should not be -- and the lifting plan's decisions directly.

(test-group "emit - JavaScript text"
  (test "a plain name is prefixed" "s_x_$1" (js-name 'x_$1))
  (test "a character JavaScript does not allow becomes its code" "s_null_3f" (js-name 'null?))
  (test "each disallowed character is replaced" "s_a_2d_3eb" (js-name 'a->b))
  (test "a string is quoted" "\"abc\"" (js-string "abc"))
  (test "a quote and a backslash are escaped" "\"a\\\"b\\\\c\"" (js-string "a\"b\\c"))
  (test "a newline and a tab are escaped" "\"a\\nb\\tc\"" (js-string "a\nb\tc"))
  (test "another control character is escaped by code" "\"\\u0001\"" (js-string (string (integer->char 1))))
  (test "an integral flonum is written as JavaScript writes it" "1" (js-number 1.))
  (test "a fraction is written in full" "0.5" (js-number .5))
  (test "negative zero keeps its sign" "-0" (js-number -0.))
  (test "infinity has no literal" "Number(\"Infinity\")" (js-number (/ 1. 0)))
  (test "nor has NaN" "Number(\"NaN\")" (js-number (/ 0. 0))))

(test-group "emit - the runtime values a procedure's code uses"
  (test "each runtime value used is declared, in the order they are listed"
        "const $TailCall = R.TailCall, $stack = R.stack;"
        (runtime-prelude '($stack $TailCall)))
  (test "a unit that uses none declares nothing" "" (runtime-prelude '()))
  ;; Each name is noted where the emitter writes it, so what a unit declares
  ;; is compared with what its code names, found by reading the code back.
  (define (named-in source)
    (filter (lambda (c)
              (let ((name (symbol->string (car c))))
                (let loop ((from 0))
                  (let ((at (string-contains source name from)))
                    (and at
                         (let ((end (+ at (string-length name))))
                           (or (= end (string-length source))
                               (let ((next (string-ref source end)))
                                 (not (or (char-alphabetic? next) (char-numeric? next) (char=? next #\_))))
                               (loop end))))))))
            runtime-constants))
  (define (declared-in source)
    (filter (lambda (c) (string-contains source (string-append (symbol->string (car c)) " = " (cdr c))))
            runtime-constants))
  (define (declares-what-it-names? ast globals guarded)
    (let ((source (car (generate-unit (lowered-ir (lower-lambda ast)) globals '() "f" guarded))))
      (equal? (named-in source) (declared-in source))))
  (test "a procedure that calls nothing declares none" '()
        (declared-in (car (generate-unit (lowered-ir (lower-lambda '(lambda (x) #f #f (var x)))) '() '() "f" '()))))
  (test "a call not in tail position declares what it names" #t
        (declares-what-it-names? '(lambda (x) #f #f (app (var g) ((app (var g) ((var x)))))) '(g) '()))
  (test "and so does a tail call" #t
        (declares-what-it-names? '(lambda (x) #f #f (app (var g) ((var x)))) '(g) '()))
  (test "a nested procedure's code is declared for as well" #t
        (declares-what-it-names? '(lambda (x) #f #f (lambda (y) #f #f (app (var g) ((app (var g) ((var y))))))) '(g) '()))
  (test "a capture" #t
        (declares-what-it-names? '(lambda () #f #f (app (var call/cc) ((lambda (k) #f #f (app (var k) ((lit 1)))))))
                                 '() '()))
  (test "and one not in tail position" #t
        (declares-what-it-names?
          '(lambda () #f #f (app (var g) ((app (var call/cc) ((lambda (k) #f #f (app (var k) ((lit 1)))))))))
          '(g) '()))
  (test "an inline expansion calling a helper" #t
        (declares-what-it-names? '(lambda (v) #f #f (app (var vector-set!) ((var v) (lit 0) (app (var vector-ref) ((var v) (lit 1))))))
                                 '(vector-set! vector-ref) '(vector-set! vector-ref))))

(test-group "emit - expressions"
  (test "an expression renders its parts" "f(s_a, 1)" (expr->string (js "f(" 's_a ", " "1" ")")))
  (test "a nested expression is spliced in" '("(" s_a ")") (js "(" (js 's_a) ")"))
  (test "an expression's locals are its symbols" '(s_a $t1) (expr-locals (js "f(" 's_a ", " '$t1 ")")))
  (test "a lone local can be written twice" #t (repeatable? (js 's_a)))
  (test "an exact integer can be written twice" #t (repeatable? (js "-12n")))
  (test "a pooled constant can be written twice" #t (repeatable? (js "K[3]")))
  (test "a boxed read cannot" #f (repeatable? (js 's_a "[0]")))
  (test "a flonum literal is evaluated once, into a temporary" #f (repeatable? (js "0.5")))
  (test "a string literal is evaluated once, into a temporary" #f (repeatable? (js "\"a\"")))
  (test "a temporary is settled" #t (settled? (js '$t4)))
  (test "undefined is settled" #t (settled? (js "undefined")))
  (test "a parameter is not settled" #f (settled? (js 's_a))))

(test-group "emit - statements"
  (test "a goto" "$pc = 3; continue;" (render-statement #f '(goto 3)))
  (test "a branch" "if (s_c !== false) { $pc = 1; continue; } $pc = 2; continue;"
        (render-statement #f (list 'branch (js 's_c) 1 2)))
  (test "an assignment" "$t1 = s_a[0];" (render-statement #f (list 'assign (js '$t1) (js 's_a "[0]"))))
  (test "a raw statement has no semicolon added" "while (x) { }"
        (render-statement #f (list 'raw (js "while (x) { }")))))

;; /**
;;  * Lowers a lambda written as analyzed-AST data, for the lifting tests.
;;  * @param {list} ast - An analyzed lambda node.
;;  * @returns {list} Its IR.
;;  */
(define (lowered ast) (lowered-ir (lower-lambda ast)))

(test-group "lift - the plan"
  ;; (lambda (a) (lambda (x) (f a x)))
  (let* ((ir (lowered '(lambda (a) #f #f (lambda (x) #f #f (app (var f) ((var a) (var x)))))))
         (plan (plan-lifting ir))
         (inner (car (outermost-lambdas (lambda-body ir)))))
    (test "a nested lambda is passed its free variables" '(a) (plan-free-of plan inner))
    (test "nothing is boxed without an assignment" '() (plan-boxed plan)))
  ;; (lambda (a) (set! a 1) (lambda () a))
  (let ((plan (plan-lifting
                (lowered '(lambda (a) #f #f
                            (seq ((set a (lit 1)) (lambda () #f #f (var a)))))))))
    (test "an assigned local is boxed" '(a) (plan-boxed plan)))
  ;; (lambda () (letrec ((e (lambda () (o))) (o (lambda () (e)))) (e)))
  (let ((plan (plan-lifting
                (lowered '(lambda () #f #f
                            (letrec (e o)
                                    ((lambda () #f #f (app (var o) ()))
                                     (lambda () #f #f (app (var e) ())))
                                    (app (var e) ())))))))
    (test "letrec names a sibling refers to are boxed" #t
          (and (boxed? plan 'e) (boxed? plan 'o) #t)))
  ;; (lambda () (letrec ((loop (lambda (n) (loop n)))) (loop 1)))
  (let* ((ir (lowered '(lambda () #f #f
                         (letrec (loop)
                                 ((lambda (n) #f #f (app (var loop) ((var n)))))
                                 (app (var loop) ((lit 1)))))))
         (plan (plan-lifting ir)))
    (test "a name only its own lambda refers to is not boxed" #f (boxed? plan 'loop))
    (test "and that lambda binds it inside its own factory" '(loop)
          (plan-self-of plan (car (outermost-lambdas (lambda-body ir)))))))

;; `eqv?` is `===` when either operand is a constant whose identity is its
;; value -- a symbol, a boolean, the empty list -- and needs the primitive
;; otherwise: numbers compare by value and exactness, characters by code point.
;; That is the shape `case` produces, one test per datum.
(test-group "inline - eqv? against a constant"
  (define x '(local x #f #f))
  (define y '(local y #f #f))
  (define (expands? name . args) (if (inline-expansion name args) #t #f))
  (test "against a symbol it expands" #t (expands? 'eqv? x '(const a #f)))
  (test "with the constant first as well" #t (expands? 'eqv? '(const a #f) x))
  (test "against a boolean" #t (expands? 'eqv? x '(const #f #f)))
  (test "against the empty list" #t (expands? 'eqv? x '(const () #f)))
  (test "not against an exact integer" #f (expands? 'eqv? x '(const 1 #f)))
  (test "not against an inexact number" #f (expands? 'eqv? x '(const 1.5 #f)))
  (test "not against a character" #f (expands? 'eqv? x (list 'const #\a #f)))
  (test "not between two variables" #f (expands? 'eqv? x y))
  (test "and to identity when it does" "s_x === K[0]"
        (let ((entry (inline-expansion 'eqv? (list x '(const a #f)))))
          (expr->string ((cadddr entry) (list (js 's_x) (js "K[0]"))))))
  (test "other expansions still apply by arity alone" #t (expands? 'car x))
  (test "and not at another arity" #f (expands? 'car x y)))

;; An arithmetic expansion's fast path is taken when both operands are exact
;; integers or both are inexact reals, which are JavaScript `bigint` and
;; `number`: for either pair the JavaScript operator computes what the numeric
;; tower would. Any other pair -- mixed exactness, a rational, a complex, a
;; wrong type -- takes the primitive.
(test-group "inline - arithmetic on two JavaScript numbers"
  ;; An exact integer in the safe range and a non-integral inexact real are
  ;; JavaScript numbers (src/core/interpreter/number_representation.js).
  (define locals '((local a #f #f) (local b #f #f)))
  (define (expansion name nodes)
    (let* ((entry (inline-expansion name nodes))
           (value (cadddr entry)))
      (list ((caddr entry) (list (js 's_a) (js 's_b)))
            (car value)
            (expr->string ((cdr value) (list (js 's_a) (js 's_b)) "$h" nodes)))))
  (test "each is guarded only on its binding, and names the runtime's operation"
        '((#f $add) (#f $sub) (#f $mul) (#f $lt) (#f $numEq))
        (map (lambda (name) (list (car (expansion name locals)) (cadr (expansion name locals))))
             '(+ - * < =)))
  (test "a sum is inline for two numbers whose sum needs no deciding, the runtime's otherwise"
        (string-append "(typeof s_a === 'number' && typeof s_b === 'number' && (!Number.isInteger((s_a + s_b))"
                       " || (Number.isSafeInteger((s_a + s_b)) && Number.isInteger(s_a) && Number.isInteger(s_b))))"
                       " ? (s_a + s_b) : $h(s_a, s_b)")
        (caddr (expansion '+ locals)))
  (test "a product of zero is the runtime's, which makes it exact zero" #t
        (and (string-contains (caddr (expansion '* locals)) "(s_a * s_b) !== 0") #t))
  (test "a comparison is the operator on two numbers"
        "(typeof s_a === 'number' && typeof s_b === 'number') ? s_a < s_b : $h(s_a, s_b)"
        (caddr (expansion '< locals)))
  (test "numeric equality is ===, which agrees on NaN"
        "(typeof s_a === 'number' && typeof s_b === 'number') ? s_a === s_b : $h(s_a, s_b)"
        (caddr (expansion '= locals)))
  (test "against an inexact constant, its double and the other's, a number's or a box's, the result boxed if integral"
        (string-append "(typeof s_a === 'number' ? R.inexactReal(s_a * (2))"
                       " : s_a instanceof R.Flonum ? R.inexactReal(s_a.value * (2)) : $h(s_a, s_b))")
        (caddr (expansion '* '((local a #f #f) (const 2.)))))
  (test "the constant on the left"
        (string-append "(typeof s_b === 'number' ? R.inexactReal((1) - s_b)"
                       " : s_b instanceof R.Flonum ? R.inexactReal((1) - s_b.value) : $h(s_a, s_b))")
        (caddr (expansion '- '((const 1.) (local b #f #f)))))
  (test "a constant that is not an integer"
        (string-append "(typeof s_a === 'number' ? R.inexactReal(s_a + (" (js-number .5) "))"
                       " : s_a instanceof R.Flonum ? R.inexactReal(s_a.value + (" (js-number .5) ")) : $h(s_a, s_b))")
        (caddr (expansion '+ '((local a #f #f) (const .5)))))
  (test "two inexact constants" "R.inexactReal((1) + (2))"
        (caddr (expansion '+ '((const 1.) (const 2.)))))
  (test "a comparison against an inexact constant"
        "(typeof s_a === 'number' ? s_a < (2) : s_a instanceof R.Flonum ? s_a.value < (2) : $h(s_a, s_b))"
        (caddr (expansion '< '((local a #f #f) (const 2.)))))
  (test "a comparison of two inexact constants" "(1) === (2)"
        (caddr (expansion '= '((const 1.) (const 2.)))))
  (test "an exact constant is an operand like any other"
        (string-append "(typeof s_a === 'number' && typeof s_b === 'number') ? s_a < s_b : $h(s_a, s_b)")
        (caddr (expansion '< '((local a #f #f) (const 2))))))

;; Vectors are JavaScript arrays. An access calls a runtime helper, which reads or
;; writes the array when the vector is an array and the index an exact integer in
;; range, and passes any other operand to the primitive -- so every error is still
;; the primitive's. The helper is the whole fast path, so there is no run-time
;; test beside the binding guard.
(test-group "inline - vector access"
  (define (entry-of name args)
    (inline-expansion name (map (lambda (a) (list 'local a #f #f)) args)))
  (define (parts name args)
    (let* ((entry (entry-of name args))
           (test ((caddr entry) (map js args))))
      (list (and test (expr->string test))
            (expr->string ((cadddr entry) (map js args))))))
  (test "vector-ref is the helper, with no test" '(#f $vectorRef)
        (let ((entry (entry-of 'vector-ref '(v i)))) (list ((caddr entry) (map js '(v i))) (cadddr entry))))
  (test "and so is vector-set!" '(#f $vectorSet)
        (let ((entry (entry-of 'vector-set! '(v i x)))) (list ((caddr entry) (map js '(v i x))) (cadddr entry))))
  (test "a helper is called with the operands"
        #t
        (let ((source (car (generate-unit (lowered-ir (lower-lambda '(lambda (v) #f #f (app (var vector-ref) ((var v) (lit 0))))))
                                          '(vector-ref) '() "f" '(vector-ref)))))
          (and (string-contains source "$vectorRef(s_v, 0)") #t)))
  (test "vector-length needs an array and is inline" '("Array.isArray(v)" "v.length")
        (parts 'vector-length '(v)))
  ;; `1.car` does not parse, and a JavaScript rejection is no procedure.
  (test "a number written out is in parentheses where a property of it is read"
        '(#t #t #t)
        (map (lambda (name operand property)
               (let ((source (car (generate-unit (lowered-ir (lower-lambda `(lambda () #f #f (app (var ,name) ((lit ,operand))))))
                                                 (list name) '() "f" (list name)))))
                 (and (string-contains source (string-append "(" (number->string operand) ")." property)) #t)))
             '(car cdr vector-length) '(1 -2 5) '("car" "cdr" "length")))
  (test "a procedure using the helper declares it"
        #t
        (let ((source (car (generate-unit (lowered-ir (lower-lambda '(lambda (v) #f #f (app (var vector-ref) ((var v) (lit 0))))))
                                          '(vector-ref) '() "f" '(vector-ref)))))
          (and (string-contains source "const $vectorRef = R.vectorRef") #t))))

;; A tail call to anything but the procedure itself is made directly while
;; compiled frames have room left on the stack, and returned to the trampoline as
;; a `TailCall` otherwise. Room is in stack rather than calls: each procedure
;; takes the size of its own frame from the room it is entered with.
(test-group "emit - tail calls between procedures"
  (define (unit-source ast globals)
    (car (generate-unit (lowered-ir (lower-lambda ast)) globals '() "f" '())))
  (define (position source text) (string-contains source text))
  ;; (lambda (x) (g x))
  (define small (unit-source '(lambda (x) #f #f (app (var g) ((var x)))) '(g)))
  (test "the call is made directly, leaving the callee this procedure's room" #t
        (and (string-contains small "$stack.room = $d; return $t") #t))
  (test "or else returned to the trampoline" #t (and (string-contains small "return $tailCall($t") #t))
  (test "the room is tested before the callee, which a call with none need not load" #t
        (< (position small "$d > 0 &&") (position small "?.[$PRIM] === true")))
  (test "a procedure that only tail-calls never moves its frames" #f
        (and (string-contains small "$stack.flushable") #t))
  (test "the procedure declares the room, the marker it tests and the fallback" #t
        (and (string-contains small "$stack = R.stack")
             (string-contains small "$PRIM = R.SCHEME_PRIMITIVE")
             (string-contains small "$tailCall = R.tailCall")
             #t)))

;; Compiled frames are on the JavaScript stack, which holds a few thousand of
;; them. So a procedure that calls takes its frame from the room it is entered
;; with, stores what is left for each callee, and when entered with none moves
;; the compiled frames beneath it to the interpreter's heap stack instead of
;; running.
(test-group "emit - room on the stack, and moving frames to the heap"
  (define (unit-source ast globals)
    (car (generate-unit (lowered-ir (lower-lambda ast)) globals '() "f" '())))
  (define (number-after source marker)
    (let ((start (+ (string-contains source marker) (string-length marker))))
      (let scan ((end start))
        (if (char-numeric? (string-ref source end))
            (scan (+ end 1))
            (string->number (substring source start end))))))
  (define (count-of source text)
    (let loop ((from 0) (n 0))
      (let ((at (string-contains source text from)))
        (if at (loop (+ at 1) (+ n 1)) n))))
  ;; (lambda (x) (g x) (g x) 1): two calls whose values are discarded.
  (define calls
    (unit-source '(lambda (x) #f #f (seq ((app (var g) ((var x))) (app (var g) ((var x))) (lit 1))))
                 '(g)))
  ;; The same with six more calls, whose frame is larger.
  (define more
    (unit-source `(lambda (x) #f #f (seq (,@(make-list 8 '(app (var g) ((var x)))) (lit 1)))) '(g)))
  ;; (lambda (x) x): calls nothing.
  (define leaf (unit-source '(lambda (x) #f #f (var x)) '()))
  ;; (lambda (a . r) (g a) 1): a rest parameter.
  (define rest (unit-source '(lambda (a) r #f (seq ((app (var g) ((var a))) (lit 1)))) '(g)))
  (test "a procedure that calls takes its frame from the room it is entered with" #t
        (and (string-contains calls "const $d = $stack.room - ") #t))
  (test "a larger frame takes more" #t
        (> (number-after more "const $d = $stack.room - ") (number-after calls "const $d = $stack.room - ")))
  (test "every call stores the room it leaves for its callee, in both forms" 4
        (count-of calls "$stack.room = $d;\n"))
  (test "and so does each step of a pending tail call" #t
        (and (string-contains calls "{ $stack.room = $d; $t") #t))
  (test "entered with no room, the fast form moves the frames beneath it, passing its arguments" #t
        (and (string-contains calls "if ($d < 0 && $stack.flushable) return $flush($proc$js, [s_x]);") #t))
  (test "the resumable form never does" 1 (count-of calls "$stack.flushable"))
  (test "a rest parameter is passed on as it arrived" #t
        (and (string-contains rest "return $flush($proc$js, [s_a, ...s_r$raw]);") #t))
  (test "and its arguments, which arrive on the stack, take room too" #t
        (and (string-contains rest " - s_r$raw.length;") #t))
  (test "a procedure that calls nothing takes no room" #f (and (string-contains leaf "$d") #t)))

;; A call whose value is wanted calls the callee directly, and JavaScript's
;; own complaint about calling what is not a function names a temporary --
;; "$t0 is not a function" -- where the interpreter says "application: not a
;; procedure". So the callee is tested first, which also keeps the raw entry
;; from being read off the empty list, JavaScript `null`.
(test-group "emit - a call to a value that is not a procedure"
  (define (unit-source ast globals)
    (car (generate-unit (lowered-ir (lower-lambda ast)) globals '() "f" '())))
  (define (count-of source text)
    (let loop ((from 0) (n 0))
      (let ((at (string-contains source text from)))
        (if at (loop (+ at 1) (+ n 1)) n))))
  ;; (lambda (x) (x 1) 2): a call whose value is discarded.
  (define call (unit-source '(lambda (x) #f #f (seq ((app (var x) ((lit 1))) (lit 2)))) '()))
  (test "a callee that is not a function is reported as the interpreter reports it, in both forms" 2
        (count-of call "if (typeof $t0 !== 'function') $notProc($t0);"))
  (test "before its raw entry is read" #t
        (< (string-contains call "$notProc($t0)") (string-contains call "[$RAW];")))
  (test "which the procedure declares" #t
        (and (string-contains call "$notProc = R.notAProcedure") #t))
  (test "and the call itself is made through the raw entry, directly, or through $foreign" 2
        (count-of call "[$PRIM] === true ? $t")))

;; A callee with no raw entry is a primitive or a compiled procedure, which
;; takes Scheme values and is called directly, or a JavaScript function, which
;; is called as the interpreter calls one -- its arguments converted -- through
;; `$foreign`.
(test-group "emit - a call to a JavaScript function"
  (define (unit-source ast globals)
    (car (generate-unit (lowered-ir (lower-lambda ast)) globals '() "f" '())))
  (define call (unit-source '(lambda (x) #f #f (seq ((app (var x) ((lit 1))) (lit 2)))) '()))
  (test "goes through $foreign" #t (and (string-contains call "$foreign($t0, [") #t))
  (test "which the procedure declares" #t (and (string-contains call "$foreign = R.callForeign") #t)))

;; Operands are evaluated left to right, the procedure first, as the
;; interpreter evaluates them: a global read or a boxed local's read written
;; into the call itself would happen after every operand to its right, and see
;; what they assign.
(test-group "emit - operands in order"
  (define (unit-source ast globals)
    (car (generate-unit (lowered-ir (lower-lambda ast)) globals '() "f" '())))
  (define (position source text) (string-contains source text))
  ;; (lambda () (g (h))): the procedure is a global, the argument a call.
  (define nested (unit-source '(lambda () #f #f (app (var g) ((app (var h) ())))) '(g h)))
  ;; (lambda () (g k (h))): a global operand before a call.
  (define global-first (unit-source '(lambda () #f #f (app (var g) ((var k) (app (var h) ())))) '(g h k)))
  ;; (lambda (x) (g x 1)): nothing after the operands can change them.
  (define settled (unit-source '(lambda (x) #f #f (app (var g) ((var x) (lit 1)))) '(g)))
  (define (count-of source text)
    (let loop ((from 0) (n 0))
      (let ((at (string-contains source text from)))
        (if at (loop (+ at 1) (+ n 1)) n))))
  (test "the procedure is read before its argument's call is made" #t
        (< (position nested "= (C0.v ?? G0());") (position nested "(C1.v ?? G1())")))
  (test "a global operand is read before a later operand's call" #t
        (< (position global-first "= (C2.v ?? G2());") (position global-first "(C1.v ?? G1())")))
  (test "operands nothing can change are not copied" 0
        (count-of settled "= s_x;")))

;; A loop whose variables provably stay inexact runs on raw doubles first,
;; before the ordinary loop, when they arrive inexact; boxes, where an
;; inexact real is an integer (src/core/interpreter/number_representation.js),
;; are made only where a value leaves it. The answers are checked in both tiers
;; by tests/tiers/double_loop_tests.scm.
(test-group "emit - loops on raw doubles"
  (define (source-of definition)
    (let ((lowered (lower-lambda (analyze-lambda definition))))
      (car (generate-unit (lowered-ir lowered) (lowered-globals lowered)
                          (lowered-library-globals lowered) "f" '(+ - * < > = vector-ref)))))
  ;; Locals are named for a counter the file shares, so a pattern names a
  ;; local by its name as written and leaves its number out.
  (define (contains? text part) (and (string-contains text part) #t))
  (define summing
    (source-of '(define (f n) (let loop ((i n) (acc 0.)) (if (< i 0.) acc (loop (- i 1.) (+ i acc)))))))
  (test "a loop over doubles is run on them, raw" #t (contains? summing "$doubles"))
  (test "entered only when its variables arrive inexact" #t
        (contains? summing "instanceof R.Flonum"))
  (test "its arithmetic is JavaScript's, with no test" #t
        (contains? summing " + s_acc_$"))
  (test "and a value leaving it is boxed" #t
        (contains? summing "return R.inexactReal(s_acc_$"))
  (test "it leaves at the head of an iteration if an operator is rebound, on a test that is JavaScript's" #t
        (contains? summing ".intact)) { break $rebound"))
  ;; A box made, or any call made, in a branch of the loop keeps V8 from
  ;; holding the loop's variables raw: such a loop took 6.6 ns an iteration
  ;; against 0.9.
  (test "and boxes its variables after the loop, not in it" #t
        (< (string-contains summing "return R.inexactReal(s_acc_$")
           (string-contains summing "= R.inexactReal(s_i_$")))
  (test "it leaves by a break, and makes what it returns after the loop" #t
        (< (string-contains summing "continue $doubles")
           (string-contains summing "return R.inexactReal(s_acc_$")))
  (test "several ways out are told apart after the loop" #t
        (contains? (source-of '(define (f x)
                                 (let loop ((x x) (k 0))
                                   (cond ((> x 100.) x) ((= k 10) (* x -1.)) (else (loop (* x 2.) (+ k 1)))))))
                   " === 0) { return R.inexactReal(s_x_$"))
  (test "a loop counting exactly is not" #f
        (contains? (source-of '(define (f n) (let loop ((i n) (acc 0)) (if (< i 0) acc (loop (- i 1) (+ i acc))))))
                   "$doubles"))
  (test "nor one that calls a procedure, where a continuation could be captured" #f
        (contains? (source-of '(define (f n g) (let loop ((x n)) (if (< x 0.) x (loop (- (g x) 1.))))))
                   "$doubles"))
  (test "a loop reading a double bound outside it holds that raw too" #t
        (contains? (source-of '(define (f c) (let loop ((z c) (k 0)) (if (= k 3) z (loop (+ (* 2. (* z z)) c) (+ k 1))))))
                   "'number' ? s_c_$"))
  (test "a loop with no inexact constant, whose variables could be doubles only if they arrived so, is not" #f
        (contains? (source-of '(define (f c) (let loop ((z c) (k 0)) (if (= k 3) z (loop (+ (* z z) c) (+ k 1))))))
                   "$doubles"))
  (test "but not an exact bound it only compares with" #f
        (contains? (source-of '(define (f n) (let loop ((x 0.) (i 0)) (if (= i n) x (loop (+ x 1.) (+ i 1))))))
                   "'number' ? s_n_$")))

;; A local bound to a constant and never assigned is the constant where it is
;; read (`constant-binding` in ir.scm): an inexact integer in arithmetic is then
;; a double rather than a box read from a variable.
(test-group "lowering - a let-bound constant"
  (define (code-of definition)
    (let ((lowered (lower-lambda (analyze-lambda definition))))
      (car (generate-unit (lowered-ir lowered) (lowered-globals lowered)
                          (lowered-library-globals lowered) "f" '(*)))))
  (test "is read as the constant" #t
        (and (string-contains (code-of '(define (f x) (let ((k 2.)) (* x k)))) " * (2))") #t))
  (test "but not when it is assigned" #f
        (and (string-contains (code-of '(define (f x) (let ((k 2.)) (set! k 3.) (* x k)))) " * (2))") #t))
  (test "nor when a procedure inside its body assigns it" #f
        (and (string-contains
              (code-of '(define (f x) (let ((k 2.)) (let ((g (lambda () (set! k 3.)))) (g) (* x k)))))
              " * (2))")
             #t))
  (test "and another name's assignment leaves it a constant" #t
        (and (string-contains (code-of '(define (f x) (let ((k 2.) (j 1)) (set! j 5) (+ (* x k) j))))
                              " * (2))")
             #t))
  (test "nor when only one branch of an if assigns it" #f
        (and (string-contains (code-of '(define (f x c) (let ((k 2.)) (if c (set! k 3.) #f) (* x k))))
                              " * (2))")
             #t))
  (test "nor when an internal definition's procedure assigns it" #f
        (and (string-contains (code-of '(define (f x) (let ((k 2.)) (define (g) (set! k 3.)) (g) (* x k))))
                              " * (2))")
             #t))
  (test "nor when an operand assigns it" #f
        (and (string-contains (code-of '(define (f x) (let ((k 2.)) (list (set! k 3.)) (* x k))))
                              " * (2))")
             #t)))

;; A lambda keeps its parameters as written beside their renamed names, and a
;; letrec its names, for a debugger to show. A list of them is not a core
;; form, though its first name may be a core form's tag: `(set)` is the
;; parameters of `(lambda (set) ...)`, not an assignment.
(test-group "lowering - names that are core forms' tags"
  (define (lowers? definition)
    (lowered-lambda? (lower-lambda (analyze-lambda definition))))
  (test "a parameter named set" #t (lowers? '(define (f set) set)))
  (test "one named lit" #t (lowers? '(define (f lit) lit)))
  (test "ones named app and var" #t (lowers? '(define (f app var) (list app var))))
  (test "an internal definition named set" #t (lowers? '(define (f x) (define (set) x) (set))))
  (test "a set named set, assigned" #t (lowers? '(define (f set) (set! set 1) set))))
