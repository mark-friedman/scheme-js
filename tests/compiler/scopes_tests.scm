;; Where each Scheme variable is in the generated code (src/compiler/scopes.scm)
;;
;; Runs in the compiler's own environment. A unit's scopes are those of the
;; Scheme it was compiled from -- a procedure's, a `let`'s, the file's -- each
;; with its variables by the names the source gives them; and its ranges say,
;; for each stretch of the generated code, which scope it is and the
;; JavaScript that reads each variable there. A debugger that reads them shows
;; a paused frame's variables as Scheme names them, the compiler's temporaries
;; left out (see sourcemap.scm for their encoding). That DevTools does is
;; tested by driving it (tests/devtools/stepping_tests.js).

;; /**
;;  * A definition, as data, lowered.
;;  * @param {list} definition - A `define` or `lambda` form.
;;  * @returns {lowered-lambda}
;;  */
(define (lowered definition) (lower-lambda (analyze-lambda definition)))

;; /**
;;  * The scopes of the Scheme a definition is compiled from.
;;  * @param {list} definition - A `define` or `lambda` form.
;;  * @returns {original-scope} The file's scope.
;;  */
(define (root-of definition)
  (let ((l (lowered definition)))
    (original-scopes-root
      (original-scopes (lowered-ir l) (lowered-globals l) (lowered-library-globals l)))))

;; /**
;;  * A scope as a list: its kind, name, variables and children.
;;  * @param {original-scope} s - The scope.
;;  * @returns {list}
;;  */
(define (shape s)
  (list (original-scope-kind s) (original-scope-name s) (original-scope-variables s)
        (map shape (original-scope-children s))))

;; /**
;;  * The scope of the procedure a definition defines, as a list (`shape`).
;;  */
(define (procedure-shape definition)
  (shape (car (original-scope-children (root-of definition)))))

(test-group "scopes - of the Scheme"
  (test "a procedure is a function scope in the file's, by its name, its parameters its variables"
        '("global" #f () (("function" "f" ("items" "n") ())))
        (shape (root-of '(define (f items n) items))))
  (test "a variable by its name as the source writes it"
        '("function" "f" ("found?" "x->y") ())
        (procedure-shape '(define (f found? x->y) found?)))
  (test "a let is a block of its own, with all its variables"
        '("function" "f" ("x") (("block" #f ("a" "b") ())))
        (procedure-shape '(define (f x) (let ((a (car x)) (b (cdr x))) (cons a b)))))
  (test "an internal definition is a variable of the body it is in"
        '("function" "f" ("x" "y") ())
        (procedure-shape '(define (f x) (define y (car x)) (cons x y))))
  (test "a named let's loop is a procedure, by its name"
        '("function" "f" ("n") (("function" "loop" ("i") ())))
        (procedure-shape '(define (f n) (let loop ((i 0)) (if (< i n) (loop (+ i 1)) i)))))
  (test "procedures a letrec binds as values: a block, and theirs inside it"
        '("function" "f" ("x") (("block" #f ("g" "h") (("function" "g" ("y") ()) ("function" "h" ("z") ())))))
        (procedure-shape '(define (f x) (letrec ((g (lambda (y) (h y))) (h (lambda (z) z))) (list g h)))))
  (test "an anonymous procedure has no name"
        '("function" "f" ("x") (("function" #f ("y") ())))
        (procedure-shape '(define (f x) (lambda (y) (+ x y)))))
  (test "the globals the code reads are the file's scope's variables" #t
        (lset= string=? '("car" "g") (original-scope-variables (root-of '(define (f x) (car (g x)))))))
  ;; A procedure reading `this` binds the receiver under a name the lowering
  ;; makes, which begins with `%`, as the system's own names do.
  (test "a name the system made is not shown, nor a block it alone would make"
        '("function" "f" () ())
        (procedure-shape '(define (f) this))))

;; /**
;;  * The scopes a definition's unit is compiled with.
;;  * @param {list} definition - A `define` or `lambda` form.
;;  * @returns {unit-scopes}
;;  */
(define (unit-scopes-of definition)
  (let ((l (lowered definition)))
    (cadddr (generate-unit (lowered-ir l) (lowered-globals l) (lowered-library-globals l) "f" '() #f #t))))

;; /**
;;  * The text of a definition's unit.
;;  */
(define (unit-text-of definition)
  (let ((l (lowered definition)))
    (car (generate-unit (lowered-ir l) (lowered-globals l) (lowered-library-globals l) "f" '() #f #t))))

;; /**
;;  * A range as a list: its scope's kind and name, what reads each variable,
;;  * whether it is a frame, and its children.
;;  * @param {generated-range} r - The range.
;;  * @returns {list}
;;  */
(define (range-shape r)
  (let ((s (generated-range-scope r)))
    (list (original-scope-kind s) (original-scope-name s) (generated-range-bindings r)
          (generated-range-stack-frame? r) (map range-shape (generated-range-children r)))))

;; /**
;;  * The unit's one outermost range, the file's scope's.
;;  */
(define (unit-range definition)
  (let ((ranges (unit-scopes-ranges (unit-scopes-of definition))))
    (and (= (length ranges) 1) (car ranges))))

;; /**
;;  * The ranges of a unit, every one, outermost first, as lists (`range-shape`)
;;  * without their children.
;;  */
(define (all-ranges definition)
  (let walk ((ranges (unit-scopes-ranges (unit-scopes-of definition))))
    (append-map (lambda (r)
                  (cons (reverse (cdr (reverse (range-shape r)))) (walk (generated-range-children r))))
                ranges)))

(test-group "scopes - of the generated code"
  (test "a procedure's two functions, fast and resumable, are each a range of its scope, a frame"
        '(("function" "f" ("x") #t ()) ("function" "f" ("x") #t ()))
        (map range-shape (generated-range-children (unit-range '(define (f x) (x))))))
  (test "the whole unit is the file's scope, each global read through its cell"
        '("global" #f ("(C0.v ?? G0())") #f)
        (let ((shape (range-shape (unit-range '(define (f x) (car x))))))
          (list (car shape) (cadr shape) (caddr shape) (cadddr shape))))
  (test "a let's body is a range of its block, its variable by its JavaScript name" #t
        (and (member '("block" #f ("y") #f) (all-ranges '(define (f x) (let ((y (car x))) (cdr y)))))
             #t))
  (test "a variable a body binds again, by the name it is told apart by" #t
        (and (member '("block" #f ("x_2") #f) (all-ranges '(define (f x) (let ((x (car x))) (cdr x)))))
             #t))
  (let ((ranges (all-ranges '(define (f items)
                               (let ((total 0))
                                 (for-each (lambda (y) (set! total (+ total y))) items)
                                 total)))))
    (test "a variable a procedure assigns and captures, read through its box"
          #t (and (member '("block" #f ("total[0]") #f) ranges) #t))
    (test "and in the procedure that captures it, through its factory's parameter, the procedure's own in its scope"
          '(("function" "f" (#f) #f) ("block" #f ("total[0]") #f) ("function" #f ("y") #t))
          (let ((from (find-tail (lambda (r) (equal? r '("function" "f" (#f) #f))) ranges)))
            (and from (list (car from) (cadr from) (caddr from))))))
  ;; A function's range begins at its head's `function`: DevTools names a
  ;; frame by the scope at its function's start, and in a factory the
  ;; declaration before it is a statement of the factory's.
  (test "a function's range runs from its head's function to the line after its end" #t
        (let* ((text (unit-text-of '(define (f x) (car x))))
               (lines (let split ((from 0))
                        (let ((at (string-index text (lambda (c) (char=? c #\newline)) from)))
                          (if at (cons (substring text from at) (split (+ at 1)))
                              (list (substring text from (string-length text)))))))
               (index (lambda (prefix)
                        (list-index (lambda (line) (string-prefix? prefix line)) lines)))
               (fast (car (generated-range-children (unit-range '(define (f x) (car x)))))))
          (and (equal? (generated-range-start fast)
                       (cons (index "const $proc = {")
                             (string-contains (list-ref lines (index "const $proc = {")) "function")))
               (equal? (generated-range-end fast) (cons (+ (index "} }[\"f\"];") 1) 0))))))
