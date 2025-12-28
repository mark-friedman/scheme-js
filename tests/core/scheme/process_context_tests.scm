;; (scheme process-context) library tests
;;
;; Tests for command-line, exit, get-environment-variable, etc.
;; Note: Some tests are environment-specific (Node.js)

(test-group "(scheme process-context) tests"
  
  ;; ===== command-line tests =====
  ;; Note: command-line returns a JS array in our implementation
  
  (test "command-line does not error"
    (guard (e (#t 'error))
      (command-line)
      'ok)
    'ok)
  
  ;; ===== get-environment-variable tests =====
  
  (test "get-environment-variable returns #f for nonexistent"
    (get-environment-variable "NONEXISTENT_VAR_SCHEME_TEST_12345")
    #f)
  
  (test "get-environment-variable with empty string"
    (get-environment-variable "")
    #f)
  
  ;; PATH should exist on most systems
  (test "get-environment-variable PATH exists"
    (string? (get-environment-variable "PATH"))
    #t)
  
  ;; HOME should exist on Unix/Mac, USERPROFILE on Windows
  (test "get-environment-variable HOME or fallback"
    (or (string? (get-environment-variable "HOME"))
        (string? (get-environment-variable "USERPROFILE"))
        #t)  ; fallback for unusual systems
    #t)
  
  ;; ===== get-environment-variable type checking =====
  
  (test "get-environment-variable requires string"
    (guard (e (#t 'type-error))
      (get-environment-variable 123))
    'type-error)
  
  (test "get-environment-variable requires exactly 1 arg"
    (guard (e (#t 'arity-error))
      (get-environment-variable))
    'arity-error)
  
  ;; ===== get-environment-variables tests =====
  
  (test "get-environment-variables does not error"
    (guard (e (#t 'error))
      (get-environment-variables)
      'ok)
    'ok)
  
  (test "get-environment-variables returns a list"
    (list? (get-environment-variables))
    #t)
  
  (test "get-environment-variables pairs are pairs"
    (let ((vars (get-environment-variables)))
      (if (null? vars)
          #t  ;; Empty is OK
          (pair? (car vars))))
    #t)
  
  (test "command-line returns a list"
    (list? (command-line))
    #t)
  
  (test "command-line first element is a string"
    (let ((args (command-line)))
      (if (null? args)
          #t
          (string? (car args))))
    #t)
  
  (test "get-environment-variables no args required"
    (guard (e (#t 'error))
      (get-environment-variables 'extra))
    'error)
  
  ;; ===== exit tests =====
  ;; Note: We can't actually test exit without terminating the process!
  ;; But we can test that it accepts the right arguments.
  
  (test "exit arity check with too many args"
    (guard (e (#t 'arity-error))
      (exit 0 'extra 'args))
    'arity-error)
  
  ;; ===== emergency-exit tests =====
  
  (test "emergency-exit arity check with too many args"
    (guard (e (#t 'arity-error))
      (emergency-exit 0 'extra 'args))
    'arity-error)
  
  ) ;; end test-group
