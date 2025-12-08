(test-group "dynamic-wind"

  (test "basic dynamic-wind"
        '(after in before)
        (let ((log '()))
          (dynamic-wind
            (lambda () (set! log (cons 'before log)))
            (lambda () (set! log (cons 'in log)) 'result)
            (lambda () (set! log (cons 'after log))))
          log))

  (test "dynamic-wind return value"
        'result
        (dynamic-wind
          (lambda () 'before)
          (lambda () 'result)
          (lambda () 'after)))

  (test "call/cc escape"
        '(after in before)
        (let ((log '()))
          (call-with-current-continuation
            (lambda (k)
              (dynamic-wind
                (lambda () (set! log (cons 'before log)))
                (lambda () 
                    (set! log (cons 'in log))
                    (k 'escape)) ; Escape!
                (lambda () (set! log (cons 'after log))))))
          log))

  (test "nested dynamic-wind"
        '(outer-after inner-after inner-in inner-before outer-before)
        (let ((log '()))
          (dynamic-wind
             (lambda () (set! log (cons 'outer-before log)))
             (lambda ()
               (dynamic-wind
                 (lambda () (set! log (cons 'inner-before log)))
                 (lambda () (set! log (cons 'inner-in log)))
                 (lambda () (set! log (cons 'inner-after log)))))
             (lambda () (set! log (cons 'outer-after log))))
          log))

  (test "re-entry with call/cc"
        '(after in before after in before)
        (let ((log '())
              (cont #f)
              (count 0))
          (call-with-current-continuation
            (lambda (k)
               (set! cont k)))
          
          (if (< count 2)
              (dynamic-wind
                (lambda () (set! log (cons 'before log)))
                (lambda () 
                    (set! log (cons 'in log))
                    (set! count (+ count 1))
                    (if (= count 2)
                        'done
                        (cont #f))) ; Jump back out
                (lambda () (set! log (cons 'after log))))
              'ignored)
          
          log))

  ;; Test "re-entry into dynamic-wind body from outside"
  (test "full unwind/rewind"
        '(after before after in before)
          (let ((log '())
                (out-k #f)
                (in-k #f)
                (stage 0))
            
            (call-with-current-continuation
              (lambda (k)
                 (set! out-k k)))
            
            (set! stage (+ stage 1))
            
            (cond
              ((= stage 1)
                 ;; First pass
                 (dynamic-wind
                    (lambda () (set! log (cons 'before log)))
                    (lambda () 
                        (set! log (cons 'in log))
                        (call-with-current-continuation
                           (lambda (k)
                              (set! in-k k)
                              (out-k 'first-exit)))) ; Unwind 'after'
                    (lambda () (set! log (cons 'after log)))))
              
              ((= stage 2)
                 ;; We are back at top after first exit.
                 ;; log should be (before in after)
                 ;; Now jump back IN
                 (in-k 're-enter)) ; Rewind 'before'
              
              ((= stage 3)
                 ;; We came back from in-k.
                 'done
              ))
              
              log))

)
