;; generate_compiled_libraries.scm -- compiles every library the bundle
;; ships, at build time.
;;
;; Writes src/packaging/compiled_libraries.js: for each library in
;; src/core/scheme/ and src/extras/scheme/, the JavaScript the compiler
;; generates for each procedure the library defines, and how to restore the
;; library without running its source. The runtime restores a library from its
;; table, or installs the table as the library loads where it cannot
;; (`libraryRestorer` and `installLibraryTable` in src/compiler/prebuilt.js).
;;
;; Why every library, and why at build time: compiling at run time needs the
;; compiler, and the compiler is most of the bundle's weight. A page that runs
;; its program interpreted still wants the libraries compiled -- `map`,
;; `assoc` and SRFI 125's lookups are what compiled and interpreted code alike
;; spend their time in, and interpreted they cost their callers 10-17x -- and
;; with every shipped library prebuilt such a page needs no compiler, which a
;; page that asks for it loads apart. And nothing calls `new Function` at run
;; time, so a page under a strict Content-Security-Policy gets the compiled
;; libraries rather than interpreted ones.
;;
;; How: each library is loaded by name, from its source, in a library registry
;; of this run's own, so it is compiled in the environment it will have at run
;; time. Its load hook has the compiler generate code for the procedures the
;; library itself defines -- its environment also holds what it imported,
;; which belongs to the libraries that defined it -- and installs that code at
;; once, so that a library that imports it sees compiled procedures here as it
;; will in the bundle. Loading a library loads what it imports first, so the
;; hook sees every library after those it depends on.
;;
;; Run by `npm run prebuild`, after the source-text bundling and before the
;; compiler compiles itself:
;;
;;     node repl.js -I scripts/lib scripts/generate_compiled_libraries.scm

(import (scheme base)
        (srfi 1)
        (srfi 152)
        (scheme-js interop)
        (scheme-js compiler)
        (scheme-js compiler build)
        (scheme-js prebuild))

;; /**
;;  * Where the shipped libraries live, as scripts/generate_bundled_libraries.js
;;  * reads them.
;;  */
(define library-dirs '("src/core/scheme" "src/extras/scheme"))

;; /**
;;  * The module the tables are written to.
;;  */
(define output "src/packaging/compiled_libraries.js")

(define read-source (source-reader library-dirs))

;; /**
;;  * Sorts strings: a merge sort, for the few dozen file names here, since no
;;  * library this build imports has a sort.
;;  * @param {list} strings - The strings.
;;  * @returns {list} Them, in `string<?` order.
;;  */
(define (sort-strings strings)
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((string<? (car b) (car a)) (cons (car b) (merge a (cdr b))))
          (else (cons (car a) (merge (cdr a) b)))))
  (let ((half (quotient (length strings) 2)))
    (if (= half 0)
        strings
        (merge (sort-strings (take strings half)) (sort-strings (drop strings half))))))

;; /**
;;  * The `.sld` files of the shipped libraries, sorted, so that they load in
;;  * the same order on every machine.
;;  * @returns {list} Their names.
;;  */
(define (library-files-declared)
  (let ((fs (js-invoke process "getBuiltinModule" "node:fs")))
    (sort-strings
     (append-map (lambda (dir)
                   (filter (lambda (file) (string-suffix? ".sld" file))
                           (vector->list (js-invoke fs "readdirSync" dir))))
                 library-dirs))))

;; /**
;;  * Has the compiler generate code for a library just loaded, makes its
;;  * table, and installs the code in it.
;;  * @param {list} name - The library's name, as strings.
;;  * @param {object} env - Its environment.
;;  * @returns {library-table}
;;  */
(define (compile-library name env)
  (let* ((outcome (generate-environment env #t #f #f))
         (table (library-table-for read-source name env (take-noted!) (car outcome) (cdr outcome))))
    (install-table-code! env table)
    table))

;; /**
;;  * Says what each table holds.
;;  * @param {list} tables - The tables.
;;  */
(define (report tables)
  (say "Compiled libraries -> " output)
  (for-each
   (lambda (table)
     (let ((bytes (fold + 0 (map (lambda (entry) (string-length (list-ref entry 4)))
                                 (library-table-entries table)))))
       (say "  " (library-table-key table) ": " (length (library-table-entries table)) " procedures, "
            (round (/ bytes 1024)) " KB, fingerprint " (library-table-fingerprint table))
       (report-restoring table "    ")
       (report-unserializable table "    ")
       (for-each (lambda (d) (say "    declined " (declined-name d) ": " (declined-reason d)))
                 (library-table-declined table))))
   tables))

(define tables '())

(with-private-libraries
 (library-resolver read-source)
 (lambda (name env) (set! tables (cons (compile-library name env) tables)))
 (lambda ()
   (for-each (lambda (file) (load-library (declared-library-name read-source file) note!))
             (library-files-declared))))

;; A library that runs no form of its own -- one that only re-exports --
;; still has a table, restoring it from its `define-library` form, so that
;; its file need not be read (`restoring` in library_system.scm); one whose
;; forms cannot be written down and that defines no procedure has none.
(let ((kept (filter (lambda (table) (or (pair? (library-table-entries table))
                                        (library-table-restore table)))
                    tables)))
  (let ((sorted (map (lambda (key) (find (lambda (table) (string=? (library-table-key table) key)) kept))
                     (sort-strings (map library-table-key kept)))))
    (write-tables! output "scripts/generate_compiled_libraries.scm"
                   "The libraries the bundle ships, compiled." sorted (source-locator library-dirs))
    (report sorted)))
