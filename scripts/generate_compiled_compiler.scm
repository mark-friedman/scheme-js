;; generate_compiled_compiler.scm -- compiles the compiler's own Scheme at
;; build time: the step where the compiler compiles itself.
;;
;; Writes src/packaging/compiled_compiler.js: the JavaScript the compiler
;; generates for each procedure the `(scheme-js compiler)` library defines --
;; src/compiler/compiler.sld and the files it includes -- and how to restore the
;; library without running its source.
;;
;; Why it has to happen, and why at build time: the interpreter can run the
;; compiler's library from source, which is what makes the bootstrap terminate
;; without a compiler written in another language. But interpreted it is about
;; 300x the JavaScript it replaced, too slow to compile anything with, and
;; compiled 16x, fast enough not to notice. At build time rather than start-up,
;; nothing calls `new Function`, so a page under a strict
;; Content-Security-Policy gets a compiled compiler, and compile speed is a
;; slower build rather than a deployment concern.
;;
;; The table has the same shape as the libraries' -- the compiler is a library
;; -- but is a module of its own, so that a bundle loads it only when something
;; asks to compile.
;;
;; Run after scripts/generate_compiled_libraries.scm, and the order is not
;; cosmetic: the lowering calls `memq` and `assq` on every scope lookup and
;; every global it records, which are themselves Scheme, and compiling the
;; compiler against an interpreted standard library is worth 1.5x, against a
;; compiled one 20x.
;;
;;     node repl.js -I scripts/lib scripts/generate_compiled_compiler.scm

(import (scheme base)
        (srfi 1)
        (srfi 152)
        (scheme-js compiler)
        (scheme-js compiler build)
        (scheme-js prebuild))

;; /**
;;  * Where the compiler's files, and the libraries it imports, live.
;;  */
(define source-dirs '("src/compiler" "src/core/scheme" "src/extras/scheme"))

;; /**
;;  * The module the table is written to.
;;  */
(define output "src/packaging/compiled_compiler.js")

;; /**
;;  * The compiler's library's name, as strings and as declared.
;;  */
(define compiler-name '("scheme-js" "compiler"))

(define read-source (source-reader source-dirs))

;; /**
;;  * The procedures reachable from a library's exports, the entry points
;;  * other code calls it through, by the globals each one's code refers to: a
;;  * procedure nothing reaches is left out of the table rather than shipped,
;;  * and the compiler's Scheme tests, which call internal procedures
;;  * directly, run such a one interpreted.
;;  * @param {list} generated - What the compiler generated, `generated`
;;  *   records.
;;  * @param {list} exports - The library's exports, as symbols.
;;  * @returns {list} Those of `generated` reachable, in its order.
;;  */
(define (reachable-from generated exports)
  (let ((by-name (map (lambda (g) (cons (generated-name g) g)) generated)))
    (let loop ((pending (map symbol->string exports)) (reached '()))
      (if (null? pending)
          (filter (lambda (g) (member (generated-name g) reached)) generated)
          (let ((entry (assoc (car pending) by-name)))
            (if (or (not entry) (member (car pending) reached))
                (loop (cdr pending) reached)
                (loop (append (map symbol->string (generated-globals (cdr entry))) (cdr pending))
                      (cons (car pending) reached))))))))

;; The libraries the compiler imports, which are what it is written with, are
;; installed from their prebuilt tables as they load. Not only for speed:
;; `generate-environment` reads whichever bindings are still interpreted
;; closures, and the compiler's own procedures are the only ones it should
;; find.
(define stale '())

;; /**
;;  * The compiler's table, from its library loaded from source in a registry
;;  * of this run's own, with what was generated and what was left out.
;;  * @returns {list} `(table generated reached declined)`.
;;  */
(define (compile-compiler)
  (let ((loaded #f))
    (with-private-libraries
     (library-resolver read-source)
     (lambda (name env)
       (let ((forms (take-noted!)))
         (cond ((equal? name compiler-name) (set! loaded (cons env forms)))
               ((install-table! name env read-source) (set! stale (cons (library-key name) stale))))))
     (lambda ()
       (register-compiler-host!)
       (let* ((exports (load-library '(scheme-js compiler) note!))
              (env (car loaded))
              (outcome (generate-environment env #t #f #f))
              (reached (reachable-from (car outcome) exports)))
         (list (library-table-for read-source compiler-name env (cdr loaded) reached '())
               (car outcome) reached (cdr outcome)))))))

(let* ((built (compile-compiler))
       (table (first built))
       (generated (second built))
       (reached (third built))
       (declined (fourth built))
       (bytes (fold + 0 (map (lambda (entry) (string-length (list-ref entry 4)))
                             (library-table-entries table)))))
  (if (pair? stale)
      (begin
        (say "  prebuilt tables are stale for " (string-join (reverse stale) ", ")
             ", so those stay interpreted;")
        (say "  run scripts/generate_compiled_libraries.scm first for a much faster build")))
  (write-tables! output "scripts/generate_compiled_compiler.scm"
                 "The compiler's own library, compiled -- the step where it compiles itself."
                 (list table))
  (say "Compiled the compiler -> " output)
  (say "  " (length (library-table-entries table)) " procedures, " (round (/ bytes 1024))
       " KB of generated code")
  (say "  fingerprint " (library-table-fingerprint table))
  (report-restoring table "  ")
  (let ((unreached (- (length generated) (length reached))))
    (if (> unreached 0) (say "  " unreached " not reachable from the exports, left out")))
  (report-unserializable table "  ")
  (if (pair? declined)
      (begin
        (say "  " (length declined) " declined by the compiler:")
        (for-each (lambda (d) (say "    " (declined-name d) ": " (declined-reason d))) declined))))
