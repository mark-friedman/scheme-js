;;; ----------------------------------------------------------------------------
;;; R7RS-small Compliance Test Suite
;;; Chapter 5: Program Structure
;;; ----------------------------------------------------------------------------

(test-group "5 Program Structure"

  ;; 5.1 Programs, 5.2 Import declarations, 5.6 Libraries, 5.7 The REPL
  ;; These sections define the static structure of Scheme code and are difficult
  ;; to test dynamically within a single script without an eval/load harness.
  ;; We focus here on definitions and record types which are testable constructs.

  (test-group "5.3 Variable definitions"
    
    (test-group "5.3.1 Top level definitions"
      (define add3 (lambda (x) (+ x 3)))
      (test "Basic define" 6 (add3 3))
      
      (define first car)
      (test "Define alias" 1 (first '(1 2))))

    (test-group "5.3.2 Internal definitions"
      (test "Internal define visibility" 45
            (let ((x 5))
              (define foo (lambda (y) (bar x y)))
              (define bar (lambda (a b) (+ (* a b) a)))
              (foo (+ x 3)))))

    (test-group "5.3.3 Multiple-value definitions"
      (define-values (x y) (exact-integer-sqrt 17))
      (test "define-values 1" 4 x)
      (test "define-values 2" 1 y)
      
      (test "define-values internal" 3
            (let ()
              (define-values (x y) (values 1 2))
              (+ x y))))
  )

  (test-group "5.4 Syntax definitions"
    (define-syntax swap!
      (syntax-rules ()
        ((swap! a b)
         (let ((tmp a))
           (set! a b)
           (set! b tmp)))))
    
    (test "define-syntax swap!" '(2 1)
          (let ((x 1) (y 2))
            (swap! x y)
            (list x y))))

  (test-group "5.5 Record-type definitions"
    ;; Definition from R7RS example
    (define-record-type <pare>
      (kons x y)
      pare?
      (x kar set-kar!)
      (y kdr))

    (test "Record constructor and predicate" #t (pare? (kons 1 2)))
    (test "Record predicate failure" #f (pare? (cons 1 2)))
    (test "Record accessor kar" 1 (kar (kons 1 2)))
    (test "Record accessor kdr" 2 (kdr (kons 1 2)))
    (test "Record modifier" 3
          (let ((k (kons 1 2)))
            (set-kar! k 3)
            (kar k)))

    ;; Test disjointness from other types
    (test "Record disjoint from pair" #f (pair? (kons 1 2)))
    (test "Record disjoint from vector" #f (vector? (kons 1 2)))
  )
)