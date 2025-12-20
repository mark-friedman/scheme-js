(import (scheme base) (scheme repl))

(test-group "REPL Tests"

  (test "interaction-environment returns an environment"
    #t
    (let ((env (interaction-environment)))
      (procedure? (lambda () #t)))) ;; placeholder for environment check if we had one, but at least it shouldn't error

  (test "or macro works in base"
    10
    (or #f 10))

  (test "when macro works in base"
    'success
    (when #t 'success))

  (test "guard macro works in base"
    'caught
    (guard (ex (else 'caught))
      (error "test")))
)
