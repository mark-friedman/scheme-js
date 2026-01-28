(test-group "cond-expand"
  (test-group "features"
    (test #t (cond-expand (r7rs #t) (else #f)))
    (test #t (cond-expand (scheme-js #t) (else #f)))
    (test #f (cond-expand (unknown-feature #t) (else #f)))
  )

  (test-group "logic"
    (test #t (cond-expand ((and r7rs scheme-js) #t) (else #f)))
    (test #t (cond-expand ((or r7rs unknown-feature) #t) (else #f)))
    (test #t (cond-expand ((not unknown-feature) #t) (else #f)))
    (test #f (cond-expand ((not r7rs) #t) (else #f)))
  )

  (test-group "else"
    (test 'fallback (cond-expand (unknown-feature 1) (else 'fallback)))
    (test 'first (cond-expand (r7rs 'first) (else 'fallback)))
  )

  (test-group "library"
    (test #t (cond-expand ((library (scheme base)) #t) (else #f)))
    (test #f (cond-expand ((library (scheme unknown)) #t) (else #f)))
  )
  
  (test-group "expressions"
    (define x 10)
    (test 20 (cond-expand (r7rs (+ x 10)) (else 0)))
    ;; Multiple expressions in body
    (test 30 (cond-expand (r7rs (set! x 20) (+ x 10)) (else 0)))
  )

  (test-group "nested"
    (test 'nested (cond-expand 
                    (r7rs (cond-expand 
                            (scheme-js 'nested) 
                            (else 'outer-else)))
                    (else 'outer-else)))
  )
)
