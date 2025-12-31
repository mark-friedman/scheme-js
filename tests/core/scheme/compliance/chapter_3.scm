;;; ----------------------------------------------------------------------------
;;; R7RS-small Compliance Test Suite
;;; Chapter 3: Basic concepts
;;; ----------------------------------------------------------------------------

(test-group "3 Basic concepts"

  ;; --------------------------------------------------------------------------
  ;; 3.1 Variables, syntactic keywords, and regions
  ;; --------------------------------------------------------------------------
  (test-group "3.1 Variables, syntactic keywords, and regions"
    ;; Test circular/shared structure notation mentioned in 3.1
    (test "Circular structure notation #0=" 
          #t 
          (let ((x (read (open-input-string "#0=(a b c #0#)"))))
            (eq? x (cadddr x))))
    
    (test "Circular structure (let)"
          #t
          (let ((x (list 'a 'b 'c)))
            (set-cdr! (cddr x) x)
            (eq? x (cdddr x))))
  )

  ;; --------------------------------------------------------------------------
  ;; 3.2 Disjointness of types
  ;; --------------------------------------------------------------------------
  (test-group "3.2 Disjointness of types"
    ;; "No object satisfies more than one of the following predicates:"
    ;; boolean? bytevector? char? eof-object? null? number? pair? port? 
    ;; procedure? string? symbol? vector?
    
    (define (type-check x)
      (let ((count 0))
        (if (boolean? x) (set! count (+ count 1)))
        (if (bytevector? x) (set! count (+ count 1)))
        (if (char? x) (set! count (+ count 1)))
        (if (eof-object? x) (set! count (+ count 1)))
        (if (null? x) (set! count (+ count 1)))
        (if (number? x) (set! count (+ count 1)))
        (if (pair? x) (set! count (+ count 1)))
        (if (port? x) (set! count (+ count 1)))
        (if (procedure? x) (set! count (+ count 1)))
        (if (string? x) (set! count (+ count 1)))
        (if (symbol? x) (set! count (+ count 1)))
        (if (vector? x) (set! count (+ count 1)))
        count))

    (test "Boolean disjointness" 1 (type-check #t))
    (test "Bytevector disjointness" 1 (type-check #u8(1 2)))
    (test "Char disjointness" 1 (type-check #\a))
    (test "EOF disjointness" 1 (type-check (eof-object)))
    (test "Null disjointness" 1 (type-check '()))
    (test "Number disjointness" 1 (type-check 3))
    (test "Pair disjointness" 1 (type-check '(a . b)))
    (test "Port disjointness" 1 (type-check (current-input-port)))
    (test "Procedure disjointness" 1 (type-check car))
    (test "String disjointness" 1 (type-check "abc"))
    (test "Symbol disjointness" 1 (type-check 'abc))
    (test "Vector disjointness" 1 (type-check #(1 2)))
  )

  ;; --------------------------------------------------------------------------
  ;; 3.4 Storage model
  ;; --------------------------------------------------------------------------
  (test-group "3.4 Storage model"
    ;; "Variables and objects... implicitly denote locations or sequences of locations."
    
    (test "New location allocation" 
          #t 
          (let ((x (list 1 2 3)))
            (not (eq? x (list 1 2 3)))))

    (test "Mutation affects location"
          '(a b c)
          (let ((x (list 'a 'b 'd)))
             (set-car! (cddr x) 'c)
             x))
    
    ;; "It is an error to attempt to store a new value into a location 
    ;; that is denoted by an immutable object."
    ;; Note: We cannot safely test "error" situations that might crash the runtime 
    ;; without a robust error handling test harness, but we can verify that
    ;; mutable constructors return mutable objects.
    (test "cons produces mutable pair" 
          'mutated
          (let ((x (cons 1 2)))
            (set-car! x 'mutated)
            (car x)))
            
    ;; R7RS allows strings to be immutable.
    ;; (test \"string produces mutable string\"
    ;;       #\\M
    ;;       (let ((s (string #\\m #\\u #\\t #\\e)))
    ;;         (string-set! s 0 #\\M)
    ;;         (string-ref s 0)))
  )

  ;; --------------------------------------------------------------------------
  ;; 3.5 Proper tail recursion
  ;; --------------------------------------------------------------------------
  (test-group "3.5 Proper tail recursion"
    ;; "Implementations of Scheme are required to be properly tail-recursive."
    
    ;; A simple loop that would overflow the stack if TCO were not present.
    ;; 100,000 frames is usually enough to blow a default stack.
    (define (loop-tail n)
      (if (zero? n)
          'done
          (loop-tail (- n 1))))
          
    (test "Recursive tail call (TCO check)" 
          'done 
          (loop-tail 100000))

    ;; Mutual tail recursion
    (letrec ((even? (lambda (n)
                      (if (zero? n) #t (odd? (- n 1)))))
             (odd? (lambda (n)
                     (if (zero? n) #f (even? (- n 1))))))
      (test "Mutual tail recursion (TCO check)" 
            #t 
            (even? 100000)))
  )
)