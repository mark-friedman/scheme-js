(test-group "parameter objects"

  ;; ==========================================================================
  ;; Basic Parameter Creation and Access
  ;; ==========================================================================

  (test "make-parameter initial value" 10
        (let ((p (make-parameter 10)))
          (p)))

  (test "make-parameter with converter" "42"
        (let ((p (make-parameter 42 number->string)))
          (p)))

  (test "parameter set value" 20
        (let ((p (make-parameter 10)))
          (p 20)
          (p)))

  (test "parameter set with converter" "999"
        (let ((p (make-parameter 1 number->string)))
          (p 999)
          (p)))

  ;; ==========================================================================
  ;; Basic parameterize
  ;; ==========================================================================

  (test "parameterize basic" 100
        (let ((p (make-parameter 10)))
          (parameterize ((p 100))
            (p))))

  (test "parameterize restores value" 10
        (let ((p (make-parameter 10)))
          (parameterize ((p 100))
            (p))
          (p)))

  (test "parameterize with converter" "200"
        (let ((p (make-parameter 0 number->string)))
          (parameterize ((p 200))
            (p))))

  (test "parameterize multiple" '(2 3)
        (let ((a (make-parameter 0))
              (b (make-parameter 0)))
          (parameterize ((a 2) (b 3))
            (list (a) (b)))))

  ;; ==========================================================================
  ;; Nested parameterize
  ;; ==========================================================================

  (test "nested parameterize inner" 30
        (let ((p (make-parameter 10)))
          (parameterize ((p 20))
            (parameterize ((p 30))
              (p)))))

  (test "nested parameterize middle" 20
        (let ((p (make-parameter 10)))
          (parameterize ((p 20))
            (parameterize ((p 30))
              (p))
            (p))))

  (test "nested parameterize outer" 10
        (let ((p (make-parameter 10)))
          (parameterize ((p 20))
            (parameterize ((p 30))
              (p))
            (p))
          (p)))

  ;; ==========================================================================
  ;; parameterize with Control Flow (call/cc)
  ;; ==========================================================================

  (test "parameterize with call/cc escape" 10
        (let ((p (make-parameter 10)))
          (call/cc
            (lambda (k)
              (parameterize ((p 20))
                (k 'escaped))))
          (p)))

  (test "parameterize call/cc captures inner" 100
        (let ((p (make-parameter 10))
              (k #f))
          (parameterize ((p 100))
            (call/cc (lambda (c) (set! k c)))
            (p))))

  ;; ==========================================================================
  ;; parameterize with dynamic-wind interaction
  ;; ==========================================================================

  (test "parameterize with dynamic-wind" '(after inner before)
        (let ((p (make-parameter 0))
              (log '()))
          (parameterize ((p 42))
            (dynamic-wind
              (lambda () (set! log (cons 'before log)))
              (lambda () (set! log (cons 'inner log)))
              (lambda () (set! log (cons 'after log)))))
          log))

)
