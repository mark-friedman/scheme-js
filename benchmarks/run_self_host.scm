;; run_self_host.scm -- what the compiler costs to run on itself.
;;
;; The compiler's lowering pass is Scheme (src/compiler/ir.scm), so the tier
;; has a customer whose performance is the project's own. This measures that
;; customer -- `lower-lambda`, over a corpus of lambdas from real Scheme --
;; three ways, and checks that all three agree about every answer.
;;
;; The three configurations are three loads of the compiler's library, each in
;; a library registry of this program's own, from source:
;;
;;   - interpreted: the library and everything it imports run by the
;;     interpreter -- the bootstrap, and the CSP-safe mode. Whatever else
;;     changes, this has to keep working: it is what lets a checkout with no
;;     prebuilt code compile itself from nothing.
;;   - compiled: the library's own procedures compiled, as the build compiles
;;     them, the standard library it is written with interpreted.
;;   - compiled, with the library compiled too: the standard library restored
;;     from its prebuilt tables as well -- what ships.
;;
;; The gap between the second and the third is the finding this measurement
;; was built to produce, and it is worth restating whenever it is read:
;; lowering calls `memq` and `assq` on every scope lookup and every global it
;; records, and those are themselves Scheme. Almost all of a compiled module's
;; cost can be the interpreted library underneath it.
;;
;; Agreement, first: every lambda is lowered in all three configurations and
;; the answers compared, because a faster wrong answer is not a result. The
;; interpreted run is the reference semantics, and a disagreement means the
;; compiler changed the meaning of the compiler. Only the lowering is timed;
;; the agreement check reads what it answered.
;;
;; Against JavaScript: the pass used to be JavaScript. On the commit that
;; removed it, the two agreed on all 952 lambdas the corpus then held and the
;; Scheme one was 18x slower -- 70 ms a pass against 3.8 ms, plus 7 ms of
;; marshalling -- a number fixed in history rather than recomputed, because
;; keeping a second lowering to re-measure it is the cost it was used to
;; decide about.
;;
;;     node repl.js --no-compile -I scripts/lib benchmarks/run_self_host.scm [--reps N]

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (srfi 1)
        (srfi 152)
        (scheme-js interop)
        (only (scheme primitives) %read-forms %environment-own-value)
        (scheme-js compiler)
        (scheme-js compiler build)
        (scheme-js prebuild))

;; /**
;;  * How many passes over the corpus each configuration is timed for.
;;  */
(define reps
  (let ((given (member "--reps" (command-line))))
    (or (and given (pair? (cdr given)) (string->number (cadr given))) 3)))

;; /**
;;  * Where the compiler's files and the libraries it imports live.
;;  */
(define read-source (source-reader '("src/compiler" "src/core/scheme" "src/extras/scheme")))

(define compiler-name '("scheme-js" "compiler"))

;; ---------------------------------------------------------------------------
;; The configurations
;; ---------------------------------------------------------------------------

;; /**
;;  * The compiler's library, loaded from source in a registry of its own, as
;;  * one configuration has it.
;;  * @param {boolean} compiled? - Whether its own procedures are compiled.
;;  * @param {boolean} shipped? - Whether the libraries it imports are restored
;;  *   from their prebuilt tables.
;;  * @returns {pair} `(lower-lambda . compiled)`: its lowering, and how many of
;;  *   its procedures were compiled.
;;  */
(define (load-compiler compiled? shipped?)
  (let ((lowering #f) (compiled 0))
    (with-private-libraries
     (library-resolver read-source)
     (lambda (name env)
       (let ((forms (take-noted!)))
         (cond ((equal? name compiler-name)
                (if compiled?
                    (let* ((outcome (generate-environment env #t #f #f))
                           (table (library-table-for read-source name env forms (car outcome) '())))
                      (install-table-code! env table)
                      (set! compiled (length (library-table-entries table)))))
                (set! lowering (%environment-own-value env 'lower-lambda)))
               (shipped? (install-table! name env read-source)))))
     (lambda ()
       (register-compiler-host!)
       (load-library '(scheme-js compiler) note!)))
    (cons lowering compiled)))

;; ---------------------------------------------------------------------------
;; The corpus
;; ---------------------------------------------------------------------------

;; /**
;;  * The files lambdas are taken from: the canonical benchmarks, the core of
;;  * the standard library, and the lowering itself.
;;  * @returns {list} Their paths.
;;  */
(define (corpus-files)
  (let* ((fs (js-invoke process "getBuiltinModule" "node:fs"))
         (benchmarks (filter (lambda (file) (string-suffix? ".scm" file))
                             (vector->list (js-invoke (js-invoke fs "readdirSync" "benchmarks/r7rs/src") "sort")))))
    (append (map (lambda (file) (string-append "benchmarks/r7rs/src/" file)) benchmarks)
            (map (lambda (file) (string-append "src/core/scheme/" file ".scm"))
                 '("macros" "equality" "cxr" "numbers" "list" "control" "case_lambda"))
            '("src/compiler/ir.scm"))))

;; /**
;;  * The lambdas a file defines procedures with, at its top level, as the
;;  * lowering takes them: each top-level form expanded, and kept where it is
;;  * `(define name (lambda ...))`. A file that cannot be read, or a form that
;;  * cannot be expanded at the top level, gives none.
;;  * @param {string} path - The file.
;;  * @returns {list} Each `(label . lambda-core-form)`.
;;  */
(define (file-lambdas path)
  (define (lambda-of core)
    (and (pair? core) (eq? (car core) 'define) (pair? (cddr core))
         (pair? (caddr core)) (eq? (car (caddr core)) 'lambda)
         (cons (string-append (last (string-split path "/")) ":" (symbol->string (cadr core)))
               (caddr core))))
  (guard (e (#t '()))
    (filter-map (lambda (form) (guard (e (#t #f)) (lambda-of (expand form))))
                (%read-forms (file-text path) path #f))))

;; ---------------------------------------------------------------------------
;; Lowering, and what it answered
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether two configurations' lowerings of a lambda agree: both failed for
;;  * the same reason, or both lowered it to the same code, with the same
;;  * globals -- in any order, since the order each configuration first saw
;;  * them in is no difference that matters -- and the same flags. Each
;;  * configuration's records are its own library's, so their fields are read
;;  * as the objects they are.
;;  * @param {*} a - One `lower-lambda`'s answer.
;;  * @param {*} b - The other's.
;;  * @returns {boolean}
;;  */
(define (same-lowering? a b)
  (define (field x name) (js-ref x name))
  (define (failed? x) (not (js-undefined? (field x "reason"))))
  (if (or (failed? a) (failed? b))
      (and (failed? a) (failed? b) (equal? (field a "reason") (field b "reason")))
      (and (equal? (field a "ir") (field b "ir"))
           (lset= eq? (field a "globals") (field b "globals"))
           (eq? (field a "calls-unknown?") (field b "calls-unknown?"))
           (eq? (field a "captures?") (field b "captures?")))))

;; /**
;;  * Milliseconds, as precisely as the host keeps them.
;;  * @returns {number}
;;  */
(define (now)
  (js-invoke (js-eval "performance") "now"))

;; /**
;;  * The milliseconds one pass over the corpus takes, averaged over `reps`.
;;  * @param {procedure} lower - A `lower-lambda`.
;;  * @param {list} lambdas - The corpus's lambda core forms.
;;  * @returns {number}
;;  */
(define (time-lowering lower lambdas)
  (let ((start (now)))
    (do ((r 0 (+ r 1))) ((= r reps)) (for-each lower lambdas))
    (/ (- (now) start) reps)))

;; /**
;;  * A number of milliseconds to one decimal place, right-aligned in a column.
;;  * @param {number} ms - The number.
;;  * @param {integer} width - The column's width.
;;  * @returns {string}
;;  */
(define (column ms width)
  (string-pad (number->string (/ (round (* ms 10)) 10.0)) width))

(let* ((interpreted (load-compiler #f #f))
       (compiled (load-compiler #t #f))
       (shipped (load-compiler #t #t))
       (corpus (append-map file-lambdas (corpus-files)))
       (lambdas (map cdr corpus))
       (differences
        (filter-map (lambda (item)
                      (let ((reference ((car interpreted) (cdr item))))
                        (cond ((not (same-lowering? reference ((car compiled) (cdr item))))
                               (string-append (car item) ": compiled differs from interpreted"))
                              ((not (same-lowering? reference ((car shipped) (cdr item))))
                               (string-append (car item) ": compiled + stdlib differs from interpreted"))
                              (else #f))))
                    corpus)))
  (say "=== The compiler's own Scheme ===")
  (say "")
  (say "the build compiles " (cdr compiled) " of the compiler's procedures")
  (say "")
  (say "=== Agreement, over " (length corpus) " lambdas from real source ===")
  (say "")
  (if (pair? differences)
      (begin
        (say "DISAGREEMENT on " (length differences) " of " (length corpus) ":")
        (for-each (lambda (d) (say "  " d)) (take differences (min 20 (length differences))))
        (say "")
        (say "Not timing a compiler that changes its own meaning.")
        (exit 1)))
  (say "the tier does not change what the lowering answers: " (length corpus) " of " (length corpus))
  (say "")
  (say "=== Speed, over " (length corpus) " lambdas ===")
  (say "")
  (let ((measured (map (lambda (label configuration)
                         (cons label (time-lowering (car configuration) lambdas)))
                       '("interpreted" "compiled" "compiled, + compiled stdlib")
                       (list interpreted compiled shipped))))
    (say "                                   per pass    vs interpreted")
    (for-each (lambda (row)
                (say "  " (string-pad-right (car row) 30) " " (column (cdr row) 8) " ms  "
                     (string-pad (number->string (/ (round (* 100 (/ (cdr (first measured)) (cdr row)))) 100.0)) 9)
                     "x"))
              measured)
    (say "")
    (say "the compiler is worth " (/ (round (* 100 (/ (cdr (first measured)) (cdr (second measured))))) 100.0)
         "x on this workload, " (/ (round (* 100 (/ (cdr (first measured)) (cdr (third measured))))) 100.0)
         "x with the standard library compiled too")
    (say "Compiling the library is not an optimization of compiling the compiler --")
    (say "lowering spends its time in memq and assq, which are themselves Scheme.")))
