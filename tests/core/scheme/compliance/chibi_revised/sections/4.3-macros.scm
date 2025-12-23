
;; R7RS Compliance Tests - Section 4.3: Macros
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn

(test-group "4.3 Macros"

  ;; (test 'now (let-syntax
  ;;                ((when (syntax-rules ()
  ;;                          ((when test stmt1 stmt2 ...)
  ;;                           (if test
  ;;                               (begin stmt1
  ;;                                      stmt2 ...))))))
  ;;              (let ((if #t))
  ;;                (when if (set! if 'now))
  ;;                if)))

  ;; (test 'outer (let ((x 'outer))
  ;;   (let-syntax ((m (syntax-rules () ((m) x))))
  ;;     (let ((x 'inner))
  ;;       (m)))))

  ;; (test 7 (letrec-syntax
  ;;   ((my-or (syntax-rules ()
  ;;             ((my-or) #f)
  ;;             ((my-or e) e)
  ;;             ((my-or e1 e2 ...)
  ;;              (let ((temp e1))
  ;;                (if temp
  ;;                    temp
  ;;                    (my-or e2 ...)))))))
  ;;   (let ((x #f)
  ;;         (y 7)
  ;;         (temp 8)
  ;;         (let odd?)
  ;;         (if even?))
  ;;     (my-or x
  ;;            (let temp)
  ;;            (if y)
  ;;            y))))

  (define-syntax be-like-begin1
    (syntax-rules ()
      ((be-like-begin1 name)
       (define-syntax name
         (syntax-rules ()
           ((name expr (... ...))
            (begin expr (... ...))))))))
  (be-like-begin1 sequence1)
  (test 3 (sequence1 0 1 2 3))

  (define-syntax be-like-begin2
    (syntax-rules ()
      ((be-like-begin2 name)
       (define-syntax name
         (... (syntax-rules ()
                ((name expr ...)
                 (begin expr ...))))))))
  (be-like-begin2 sequence2)
  (test 4 (sequence2 1 2 3 4))

  (define-syntax be-like-begin3
    (syntax-rules ()
      ((be-like-begin3 name)
       (define-syntax name
         (syntax-rules dots ()
           ((name expr dots)
            (begin expr dots)))))))
  (be-like-begin3 sequence3)
  (test 5 (sequence3 2 3 4 5))

  ;; ellipsis escape
  (define-syntax elli-esc-1
    (syntax-rules ()
      ((_)
       '(... ...))
      ((_ x)
       '(... (x ...)))
      ((_ x y)
       '(... (... x y)))))

  (test '... (elli-esc-1))
  (test '(100 ...) (elli-esc-1 100))
  (test '(... 100 200) (elli-esc-1 100 200))

  ;; Syntax pattern with ellipsis in middle of proper list.
  (define-syntax part-2
    (syntax-rules ()
      ((_ a b (m n) ... x y)
       (vector (list a b) (list m ...) (list n ...) (list x y)))
      ((_ . rest) 'error)))
  (test '#((10 43) (31 41 51) (32 42 52) (63 77))
      (part-2 10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77))

  ;; Syntax pattern with ellipsis in middle of improper list.
  (define-syntax part-2x
    (syntax-rules ()
      ((_ (a b (m n) ... x y . rest))
       (vector (list a b) (list m ...) (list n ...) (list x y)
               (cons "rest:" 'rest)))
      ((_ . rest) 'error)))
  (test '#((10 43) (31 41 51) (32 42 52) (63 77) ("rest:"))
      (part-2x (10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77)))
  (test '#((10 43) (31 41 51) (32 42 52) (63 77) ("rest:" . "tail"))
      (part-2x (10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77 . "tail")))

  ;; underscore
  (define-syntax underscore
    (syntax-rules ()
      ((foo _) '_)))
  (test '_ (underscore foo))

  (let ()
    (define-syntax underscore2
      (syntax-rules ()
        ((underscore2 (a _) ...) 42)))
    (test 42 (underscore2 (1 2))))

  (define-syntax count-to-2
    (syntax-rules ()
      ((_) 0)
      ((_ _) 1)
      ((_ _ _) 2)
      ((_ . _) 'many)))
  (test '(2 0 many)
      (list (count-to-2 a b) (count-to-2) (count-to-2 a b c d)))

  (define-syntax count-to-2_
    (syntax-rules (_)
      ((_) 0)
      ((_ _) 1)
      ((_ _ _) 2)
      ((x . y) 'fail)))
  (test '(2 0 fail fail)
      (list (count-to-2_ _ _) (count-to-2_)
            (count-to-2_ a b) (count-to-2_ a b c d)))

  (define-syntax jabberwocky
    (syntax-rules ()
      ((_ hatter)
       (begin
         (define march-hare 42)
         (define-syntax hatter
           (syntax-rules ()
             ((_) march-hare)))))))
  (jabberwocky mad-hatter)
  (test 42 (mad-hatter))

  (test 'ok (let ((=> #f)) (cond (#t => 'ok))))

  (let ()
    (define x 1)
    (let-syntax ()
      (define x 2)
      #f)
    (test 1 x))

  (let ()
   (define-syntax foo
     (syntax-rules ()
       ((foo bar y)
        (define-syntax bar
          (syntax-rules ()
            ((bar x) 'y))))))
   (foo bar x)
   (test 'x (bar 1)))

  (begin
    (define-syntax ffoo
      (syntax-rules ()
        ((ffoo ff)
         (begin
           (define (ff x)
             (gg x))
           (define (gg x)
             (* x x))))))
    (ffoo ff)
    (test 100 (ff 10)))

  (let-syntax ((vector-lit
                 (syntax-rules ()
                   ((vector-lit)
                    '#(b)))))
    (test '#(b) (vector-lit)))

  (let ()
    ;; forward hygienic refs
    (define-syntax foo399
      (syntax-rules () ((foo399) (bar399))))
    (define (quux399)
      (foo399))
    (define (bar399)
      42)
    (test 42 (quux399)))

  (let-syntax
      ((m (syntax-rules ()
            ((m x) (let-syntax
                       ((n (syntax-rules (k)
                             ((n x) 'bound-identifier=?)
                             ((n y) 'free-identifier=?))))
                     (n z))))))
    (test 'bound-identifier=? (m k)))

  ;; literal has priority to ellipsis (R7RS 4.3.2)
  (let ()
    (define-syntax elli-lit-1
      (syntax-rules ... (...)
        ((_ x)
         '(x ...))))
    (test '(100 ...) (elli-lit-1 100)))
)
