;; Tests for error checking in Scheme procedures
;; Tests that use Scheme's `error` procedure which integrates properly with guard

(test-group "Scheme Error Tests"
  ;; Test that the test-error macro works  
  (test-error "error raises" "test error" (error "test error"))
  
  ;; Test map with non-procedure
  (test-error "map-non-proc" "procedure" (map 5 '(1 2 3)))
  
  ;; Test memq with non-list
  (test-error "memq-non-list" "list" (memq 'a 5))
  
  ;; Test memv with non-list
  (test-error "memv-non-list" "list" (memv 'a "string"))
  
  ;; Test member with non-list
  (test-error "member-non-list" "list" (member 'a 42))
)

;; Return test summary
(test-report)
