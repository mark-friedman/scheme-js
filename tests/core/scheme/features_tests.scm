;; Features procedure tests
;;
;; Tests for (features) returning a proper list of R7RS feature symbols.

(test-group "(features) tests"
  
  ;; ===== Basic tests =====
  
  (test "features returns list"
    #t
    (list? (features)))
  
  (test "features returns non-empty list"
    #t
    (pair? (features)))
  
  (test "features contains r7rs"
    #t
    (if (memq 'r7rs (features)) #t #f))
  
  ;; ===== Type tests =====
  
  (test "all features are symbols"
    #t
    (let check-all ((lst (features)))
      (cond
        ((null? lst) #t)
        ((not (symbol? (car lst))) #f)
        (else (check-all (cdr lst))))))
  
  ;; ===== Standard features =====
  
  (test "features includes ieee-float"
    #t
    (if (memq 'ieee-float (features)) #t #f))
  
  (test "features includes full-unicode"
    #t
    (if (memq 'full-unicode (features)) #t #f))
  
  ;; ===== Implementation-specific =====
  
  (test "features includes scheme-js"
    #t
    (if (memq 'scheme-js (features)) #t #f))

  ;; ===== Arity =====

  (test "features takes no arguments"
    'raised
    (guard (e (#t 'raised))
      (features 'r7rs)))

  ;; ===== Agreement with cond-expand =====
  ;; R7RS 6.14: `(features)` is the list of feature identifiers `cond-expand`
  ;; treats as true, so a feature is in it exactly when `cond-expand` takes it.

  ;; /**
  ;;  * Whether a feature is in the list `(features)` returns.
  ;;  * @param {symbol} feature - The feature.
  ;;  * @returns {boolean}
  ;;  */
  (define (in-features? feature)
    (if (memq feature (features)) #t #f))

  (test "r7rs: features agrees with cond-expand"
    (cond-expand (r7rs #t) (else #f))
    (in-features? 'r7rs))

  (test "scheme-js: features agrees with cond-expand"
    (cond-expand (scheme-js #t) (else #f))
    (in-features? 'scheme-js))

  (test "exact-closed: features agrees with cond-expand"
    (cond-expand (exact-closed #t) (else #f))
    (in-features? 'exact-closed))

  (test "ratios: features agrees with cond-expand"
    (cond-expand (ratios #t) (else #f))
    (in-features? 'ratios))

  (test "ieee-float: features agrees with cond-expand"
    (cond-expand (ieee-float #t) (else #f))
    (in-features? 'ieee-float))

  (test "full-unicode: features agrees with cond-expand"
    (cond-expand (full-unicode #t) (else #f))
    (in-features? 'full-unicode))

  (test "node: features agrees with cond-expand"
    (cond-expand (node #t) (else #f))
    (in-features? 'node))

  (test "browser: features agrees with cond-expand"
    (cond-expand (browser #t) (else #f))
    (in-features? 'browser))

  (test "features names the host, node or browser"
    (cond-expand ((or node browser) #t) (else #f))
    (or (in-features? 'node) (in-features? 'browser)))

  (test "a feature cond-expand does not take is not in features"
    (cond-expand (no-such-feature #t) (else #f))
    (in-features? 'no-such-feature))

  ;; The converse: `cond-expand` takes every feature in the list.
  (test "cond-expand takes every feature in features"
    '()
    (let loop ((fs (features)) (not-taken '()))
      (cond ((null? fs) (reverse not-taken))
            ((eval (list 'cond-expand (list (car fs) #t) '(else #f))
                   (interaction-environment))
             (loop (cdr fs) not-taken))
            (else (loop (cdr fs) (cons (car fs) not-taken))))))

  (test "features names no feature twice"
    #t
    (let loop ((fs (features)))
      (cond ((null? fs) #t)
            ((memq (car fs) (cdr fs)) #f)
            (else (loop (cdr fs))))))

  ;; A program that changes the list it was given changes no other: not the
  ;; next `(features)`, nor what `cond-expand` finds.
  (test "features returns a list of its own"
    #f
    (let ((fs (features)))
      (set-cdr! (list-tail fs (- (length fs) 1)) (list 'added-by-a-program))
      (in-features? 'added-by-a-program)))

  ) ;; end test-group
