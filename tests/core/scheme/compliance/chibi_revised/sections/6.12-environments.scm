(test-group "6.12 Environments and evaluation"

;; (test 21 (eval '(* 7 3) (scheme-report-environment 5)))

;; null-environment 5 now works with BigInt version
(test 20
    (let ((f (eval '(lambda (f x) (f x x)) (null-environment 5))))
      (f + 10)))

(test 1024 (eval '(expt 2 10) (environment '(scheme base))))
;; (sin 0) may return exact number
(test 0.0 (inexact (eval '(sin 0) (environment '(scheme inexact)))))
;; ditto
(test 1024.0 (eval '(+ (expt 2 10) (inexact (sin 0)))
                   (environment '(scheme base) '(scheme inexact))))

)
