;; interpreted.scm -- runs `kernels.scm` on the interpreter as it is, for
;; `benchmarks/run_evaluator.js`, which runs this with the tier off
;; (`--no-compile`), so the kernels are interpreted: each kernel's best time of
;; several runs, in milliseconds, a line each, as `label<TAB>ms<TAB>answer`.
;;
;;     node repl.js --no-compile benchmarks/evaluator/interpreted.scm [runs]

(import (scheme base) (scheme write) (scheme process-context) (scheme-js interop))

;; Named from the repository, where the scripts run.
(include "benchmarks/evaluator/kernels.scm")

;; How many times each kernel is run; the best is kept.
(define runs
  (or (string->number (car (reverse (command-line)))) 5))

;; /**
;;  * Milliseconds, as precisely as the host keeps them: `current-jiffy`
;;  * counts whole milliseconds here.
;;  * @returns {number}
;;  */
(define (now) (js-invoke (js-eval "performance") "now"))

;; /**
;;  * A thunk's best time over the runs, in milliseconds, and its answer.
;;  * @param {procedure} thunk - The kernel.
;;  * @returns {pair} (ms . answer)
;;  */
(define (best thunk)
  (let loop ((i 0) (best-ms #f) (answer #f))
    (if (= i runs)
        (cons best-ms answer)
        (let* ((start (now))
               (value (thunk))
               (ms (- (now) start)))
          (loop (+ i 1) (if (or (not best-ms) (< ms best-ms)) ms best-ms) value)))))

;; Every kernel once before any is timed, as the evaluator in Scheme does
;; (`benchmarks/run_evaluator.scm`), so that both are measured alike.
(for-each (lambda (kernel) ((cadr kernel))) kernels)

(for-each (lambda (kernel)
            (let ((timed (best (cadr kernel))))
              (display (car kernel)) (display "\t") (display (car timed)) (display "\t")
              (write (equal? (cdr timed) (caddr kernel))) (newline)))
          kernels)
