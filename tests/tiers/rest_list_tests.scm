;; rest_list_tests.scm -- a rest parameter compiled code makes a list only
;; where it must, in either tier.
;;
;; A procedure's rest parameter that it neither assigns nor lets a procedure
;; inside it capture is, in compiled code, the array of the arguments it was
;; called with, until it is used as a list: `null?`, `pair?` and `car` of it,
;; and of its tails by `cdr`, read the array; anything else -- passing it on,
;; returning it, saving the frame -- makes the list, once ("A rest parameter"
;; in src/compiler/emit.scm). These check that the answers are the
;; interpreter's: taken apart, passed on, changed by whoever it was passed to
;; and read after, taken apart past its end, and kept across a continuation
;; captured while the list was not yet made.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

;; /**
;;  * An optional argument, its default, and whether more were given.
;;  * @param {*} a - The first argument.
;;  * @param {list} more - The others.
;;  * @returns {list}
;;  */
(define (optional a . more)
  (list a
        (if (null? more) 'none (car more))
        (and (pair? more) (pair? (cdr more)) (car (cdr more)))
        (cond ((null? more) 0) ((null? (cdr more)) 1) (else 'many))))

;; /**
;;  * The rest list itself, passed on, then taken apart.
;;  * @param {list} more - The arguments.
;;  * @returns {list}
;;  */
(define (passed-on . more)
  (let ((all (length more)))
    (list all more (if (pair? more) (car more) 'none))))

;; /**
;;  * The rest list changed by the procedure it is passed to, then read.
;;  * @param {list} more - The arguments.
;;  * @returns {*}
;;  */
(define (changed . more)
  (set-car! more 'changed)
  (car more))

;; /**
;;  * The first of the arguments, whatever there are: none is car's error.
;;  * @param {list} more - The arguments.
;;  * @returns {*}
;;  */
(define (first . more) (car more))

;; /**
;;  * The second's tail, through two cdrs: past the end is cdr's error.
;;  * @param {list} more - The arguments.
;;  * @returns {boolean}
;;  */
(define (none-after-two? . more) (null? (cdr (cdr more))))

;; A continuation captured beneath a procedure whose rest list is not yet
;; made, and taken again: the procedure's frame is saved and resumed.
(define taken #f)
(define times 0)

;; /**
;;  * The arguments after a capture, read from the frame it was resumed with.
;;  * @param {list} more - The arguments.
;;  * @returns {list}
;;  */
(define (across-a-capture . more)
  (call/cc (lambda (k) (set! taken k)))
  (set! times (+ times 1))
  (list (car more) (null? (cdr more)) more))

;; A procedure that takes its rest list apart through a cdr before a call
;; whose callee it read first, holding it across the call, beneath which a
;; continuation is captured and taken again: its frame, saved by the fast form,
;; is resumed by the resumable form, which must find the callee where the fast
;; form saved it, under the same temporary.
(define again #f)
(define (capture x) (call/cc (lambda (k) (set! again k) x)))
(define (callee x) (list 'callee x))

;; /**
;;  * The second argument, through a procedure that captures a continuation.
;;  * @param {list} more - The arguments.
;;  * @returns {list}
;;  */
(define (through-a-capture . more)
  (if (and (pair? more) (pair? (cdr more)))
      (callee (capture (car (cdr more))))
      'short))

;; /**
;;  * Whether a thunk raises an error.
;;  * @param {procedure} thunk - The thunk.
;;  * @returns {boolean}
;;  */
(define (refuses? thunk)
  (guard (e ((error-object? e) #t)) (thunk) #f))

(optional 1)
(optional 1)
(passed-on 1)
(passed-on 1)
(changed 1)
(changed 1)
(first 1)
(first 1)
(none-after-two? 1 2)
(none-after-two? 1 2)
(through-a-capture 1 2)
(through-a-capture 1 2)

(test-group "A rest parameter"
  (test "the tier compiled them" (list *tier-attached* *tier-attached* *tier-attached*)
        (map (lambda (p) (eq? #t (js-ref p "$compiled"))) (list optional passed-on across-a-capture)))
  (test "left out" '(1 none #f 0) (optional 1))
  (test "given once" '(1 2 #f 1) (optional 1 2))
  (test "given twice, the second through a cdr" '(1 2 3 many) (optional 1 2 3))
  (test "passed on, and taken apart after" '(3 (a b c) a) (passed-on 'a 'b 'c))
  (test "and none" '(0 () none) (passed-on))
  (test "changed by whoever it was passed to, and read after" 'changed (changed 'a 'b))
  (test "taken apart past its end is the primitive's error" '(#t #t)
        (list (refuses? (lambda () (first))) (refuses? (lambda () (none-after-two? 1)))))
  (test "within its end" '(#t #f) (list (none-after-two? 1 2) (none-after-two? 1 2 3)))
  (test "taken apart through a cdr before a call whose frame is resumed, the callee where it was saved"
        '((callee 3) 2)
        (let* ((count 0)
               (result (through-a-capture 1 2)))
          (set! count (+ count 1))
          (if (= count 1) (again 3))
          (list result count)))
  (test "kept across a continuation captured before it was made, and taken again" '((a #t (a)) 2)
        (let ((first-time (across-a-capture 'a)))
          (if (= times 1) (taken #f))
          (list first-time times))))
