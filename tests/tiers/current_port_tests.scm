;; current_port_tests.scm -- the current ports, parameterized, in either tier.
;;
;; The procedures that read or write the current port by default look the
;; parameter up at each call, so a compiled procedure writing with no port
;; writes where `parameterize` says, as an interpreted one does. The file runs
;; twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js); each procedure loops, so the tier
;; compiles it when it is bound.

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

(define (write-digits n)
  (let loop ((i 0))
    (when (< i n)
      (display i)
      (write-char #\space)
      (loop (+ i 1))))
  (newline))

(define (read-all-chars)
  (let loop ((chars '()))
    (let ((c (read-char)))
      (if (eof-object? c) (list->string (reverse chars)) (loop (cons c chars))))))

(define (captured thunk)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port)) (thunk))
    (get-output-string port)))

(test-group "The current ports, parameterized"
  (test "the tier compiled the procedures, and only in the run with it attached"
        *tier-attached* (and (compiled? write-digits) (compiled? read-all-chars)))
  (test "a procedure writing with no port writes to the port parameterize binds"
        "0 1 2 \n" (captured (lambda () (write-digits 3))))
  (test "a procedure reading with no port reads the port parameterize binds"
        "abc" (parameterize ((current-input-port (open-input-string "abc"))) (read-all-chars)))
  (test "nested, the inner binding is the one written to"
        '("0 \n" "") (let ((outer (open-output-string)) (inner #f))
                       (parameterize ((current-output-port outer))
                         (set! inner (captured (lambda () (write-digits 1)))))
                       (list inner (get-output-string outer)))))
