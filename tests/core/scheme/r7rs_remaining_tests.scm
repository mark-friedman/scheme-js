;; The last of R7RS-small's identifiers and libraries
;;
;; `rationalize` and `read-bytevector!` (R7RS 6.2.6, 6.13.2), the binary file
;; ports (6.13.1), `load` and `(scheme load)` (6.14), `environment` with its
;; import sets (6.12), and `(scheme r5rs)` (Appendix A). Files are Node's only,
;; since a browser has none.

(import (scheme file) (scheme eval) (scheme load) (prefix (scheme r5rs) r5:))

(test-group "rationalize"
  (test "the simplest rational within the tolerance, exact" 1/3 (rationalize (exact .3) 1/10))
  (test "inexact if either argument is" #t
        (let ((r (rationalize .3 1/10))) (and (inexact? r) (= r (inexact 1/3)))))
  (test "a range holding an integer gives the smallest" 3 (rationalize 5 2))
  (test "below zero, mirrored" -1/3 (rationalize -3/10 1/10))
  (test "a range holding zero gives zero" 0 (rationalize 1/10 1/5))
  (test "no tolerance, the number itself" 1/3 (rationalize 1/3 0))
  (test "a negative tolerance is as its magnitude" 1/3 (rationalize 3/10 -1/10))
  (test "an infinite number, itself" +inf.0 (rationalize +inf.0 3))
  (test "an infinite tolerance, zero" 0.0 (rationalize 3 +inf.0))
  (test "both infinite, not a number" #t (nan? (rationalize +inf.0 +inf.0)))
  (test "a non-real is an error" 'raised (guard (e (#t 'raised)) (rationalize 'a 1))))

(test-group "read-bytevector!"
  (test "reads into the range, returning how many it read" '(3 #u8(0 1 2 3 0))
        (let ((bv (make-bytevector 5 0)))
          (list (read-bytevector! bv (open-input-bytevector (bytevector 1 2 3)) 1) bv)))
  (test "no more than the range holds" '(2 #u8(7 8 0))
        (let ((bv (make-bytevector 3 0)))
          (list (read-bytevector! bv (open-input-bytevector (bytevector 7 8 9)) 0 2) bv)))
  (test "at the end of the port, the end-of-file object" #t
        (eof-object? (read-bytevector! (make-bytevector 2 0) (open-input-bytevector (bytevector)))))
  (test "an empty range reads nothing" 0
        (read-bytevector! (make-bytevector 2 0) (open-input-bytevector (bytevector 1)) 1 1))
  (test "from the current input port by default, which is textual" 'raised
        (guard (e (#t 'raised)) (read-bytevector! (make-bytevector 2 0))))
  (test "a range outside the bytevector is an error" 'raised
        (guard (e (#t 'raised)) (read-bytevector! (make-bytevector 2 0) (open-input-bytevector (bytevector 1)) 0 3)))
  (test "a textual port is an error" 'raised
        (guard (e (#t 'raised)) (read-bytevector! (make-bytevector 2 0) (open-input-string "ab")))))

(test-group "binary file ports"
  (cond-expand
    (node
      (define file "r7rs_remaining_binary.tmp")
      (test "what is written is read back"
        #u8(0 1 254 255 7)
        (let ((out (open-binary-output-file file)))
          (write-u8 0 out)
          (write-bytevector (bytevector 1 254 255) out)
          (write-u8 7 out)
          (close-port out)
          (let* ((in (open-binary-input-file file)) (bytes (read-bytevector 100 in)))
            (close-port in)
            bytes)))
      (test "the ports are binary" '(#t #f #t #f)
        (let ((out (open-binary-output-file file)) (in (open-binary-input-file file)))
          (let ((kinds (list (binary-port? out) (textual-port? out) (binary-port? in) (textual-port? in))))
            (close-port out) (close-port in)
            kinds)))
      (test "a file that is not there is an error" 'raised
        (guard (e (#t 'raised)) (open-binary-input-file "r7rs_remaining_no_such_file.tmp")))
      (delete-file file))
    (else
      (test "no files in a browser" 'raised
        (guard (e (#t 'raised)) (open-binary-input-file "anything"))))))

(test-group "load"
  (cond-expand
    (node
      (define file "r7rs_remaining_load.tmp")
      (call-with-output-file file
        (lambda (p) (write '(define r7rs-remaining-loaded 42) p) (write '(set! r7rs-remaining-loaded (+ r7rs-remaining-loaded 1)) p)))
      (load file)
      (test "evaluates a file's forms in order, in the interaction environment" 43 r7rs-remaining-loaded)
      (let ((env (environment '(scheme base))))
        (call-with-output-file file (lambda (p) (write '(define r7rs-remaining-elsewhere 7) p)))
        (load file env)
        (test "or in the environment given" 7 (eval 'r7rs-remaining-elsewhere env)))
      (delete-file file))
    (else #f))
  (test "a name that is not a string is an error" 'raised (guard (e (#t 'raised)) (load 'file))))

(test-group "environment"
  (test "the import sets' bindings" 3 (eval '(+ 1 2) (environment '(scheme base))))
  (test "filtered" 3 (eval '(b:+ 1 2) (environment '(prefix (scheme base) b:))))
  (test "a macro imported" 'yes (eval '(when #t 'yes) (environment '(scheme base))))
  (test "a macro imported under another name" 'renamed
        (eval '(w #t 'renamed) (environment '(rename (scheme base) (when w)))))
  (test "several sets" 'a (eval '(car (list 'a)) (environment '(only (scheme base) car) '(only (scheme base) list quote))))
  (test "each a new environment" #f (eq? (environment '(scheme base)) (environment '(scheme base))))
  (test "an import set naming no library is an error" 'raised
        (guard (e (#t 'raised)) (environment '(no such library)))))

(test-group "(scheme r5rs)"
  (test "the exactness procedures, under their R5RS names" '(0.5 1/2)
        (list (r5:exact->inexact 1/2) (r5:inexact->exact 0.5)))
  (test "the report's environment, for version 5" 3 (eval '(+ 1 2) (r5:scheme-report-environment 5)))
  (test "the null environment, for version 5" 1 (eval '(if #t 1 2) (r5:null-environment 5)))
  (test "another version is an error" 'raised (guard (e (#t 'raised)) (r5:scheme-report-environment 7)))
  (test "R5RS procedures" '(3 #\a "ab") (list (r5:length '(1 2 3)) (r5:string-ref "a" 0) (r5:string-append "a" "b"))))
