;;; prebuild.scm -- what the build steps that write the prebuilt tables share.
;;;
;;; A table holds, for one library, the code the compiler generates for the
;;; procedures the library defines, and how to restore the library without
;;; running its source (`installLibraryTable` and `libraryRestorer` in
;;; src/compiler/prebuilt.js). A build step loads the library from its source,
;;; noting each top-level form loading runs with the core form it expanded
;;; into, has the compiler generate code for the library's own procedures
;;; (`generate-environment`), and makes the table from what it generated
;;; (`library-table-for`). The table writer, (scheme-js table-writer), writes
;;; the tables down as a JavaScript module.

;; ---------------------------------------------------------------------------
;; The libraries' files
;; ---------------------------------------------------------------------------

;; /**
;;  * The whole text of a file.
;;  * @param {string} path - The file.
;;  * @returns {string}
;;  */
(define (file-text path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((chunks '()))
        (let ((chunk (read-string 65536 port)))
          (if (eof-object? chunk)
              (apply string-append (reverse chunks))
              (loop (cons chunk chunks))))))))

;; /**
;;  * What reads the files of some directories: a file's text, by its name,
;;  * from the first of them that has it, or #f.
;;  * @param {list} dirs - The directories, as paths.
;;  * @returns {procedure}
;;  */
(define (source-reader dirs)
  (lambda (file)
    (let ((dir (find (lambda (dir) (file-exists? (string-append dir "/" file))) dirs)))
      (and dir (file-text (string-append dir "/" file))))))

;; /**
;;  * The resolver a registry finds a library's file, or one it includes, by:
;;  * by the last part of its name, `foo.sld` before `foo`, as every resolver
;;  * of the bundle's does.
;;  * @param {procedure} read-source - A `source-reader`.
;;  * @returns {procedure} From a name or path, as strings, to the file's text.
;;  */
(define (library-resolver read-source)
  (lambda (path)
    (let ((name (last path)))
      (or (read-source (string-append name ".sld"))
          (read-source name)
          (error "no library file for" (string-join path "/"))))))

;; /**
;;  * A library's key, as tables and registries are keyed: `scheme.base`.
;;  * @param {list} name - Its name, as strings.
;;  * @returns {string}
;;  */
(define (library-key name)
  (string-join name "."))

;; /**
;;  * A library's `define-library` form, read as the loader reads a library's
;;  * file: dot notation off, each list carrying its place in the file.
;;  * @param {procedure} read-source - A `source-reader`.
;;  * @param {list} name - The library's name, as strings.
;;  * @returns {pair}
;;  */
(define (library-declaration read-source name)
  (car (%read-forms (read-source (string-append (last name) ".sld")) (string-join name "/") #f)))

;; /**
;;  * The name a library's file declares.
;;  * @param {procedure} read-source - A `source-reader`.
;;  * @param {string} file - The file's name, `base.sld`.
;;  * @returns {list} The name, its parts symbols and numbers as written.
;;  */
(define (declared-library-name read-source file)
  (cadr (car (%read-forms (read-source file) file #f))))

;; /**
;;  * Whether a feature requirement holds, for reading a library's declaration:
;;  * against this run's features. No shipped library's declaration asks
;;  * whether a library is available, so none is.
;;  * @param {*} requirement - The requirement.
;;  * @returns {boolean}
;;  */
(define (feature-met? requirement)
  (requirement-met? requirement (features) (lambda (name) #f)))

;; /**
;;  * The files a library is made of, in the order its fingerprint covers them:
;;  * its `.sld`, then what it includes and the files of declarations it
;;  * includes.
;;  * @param {procedure} read-source - A `source-reader`.
;;  * @param {list} name - The library's name, as strings.
;;  * @returns {list} The files' names.
;;  */
(define (library-files read-source name)
  (let ((definition (parse-define-library (library-declaration read-source name) feature-met?)))
    (cons (string-append (last name) ".sld")
          (append (library-definition-includes definition)
                  (library-definition-includes-ci definition)
                  (library-definition-declaration-files definition)))))

;; ---------------------------------------------------------------------------
;; The forms loading runs
;; ---------------------------------------------------------------------------

;; The forms noted since they were last taken, latest first, each
;; `(form . core-form)`.
(define noted '())

;; /**
;;  * Notes a top-level form of a library's body, and the core form it
;;  * expanded into, as loading runs it (`load-library`): every library's,
;;  * each library's after those it imports have loaded, and before its load
;;  * hook is called.
;;  * @param {*} form - The form.
;;  * @param {*} core - Its core form.
;;  */
(define (note! form core)
  (set! noted (cons (cons form core) noted)))

;; /**
;;  * The forms noted since this was last asked, in the order they ran, which
;;  * it forgets: in a load hook, the forms of the library just loaded.
;;  * @returns {list} Each `(form . core-form)`.
;;  */
(define (take-noted!)
  (let ((forms (reverse noted)))
    (set! noted '())
    forms))

;; ---------------------------------------------------------------------------
;; Tables
;; ---------------------------------------------------------------------------

;; /**
;;  * A library's table, with what was left out of it.
;;  * @property {string} key - The library's key.
;;  * @property {string} fingerprint - Its sources' fingerprint.
;;  * @property {list} files - Its files.
;;  * @property {list} entries - Each procedure the table holds,
;;  *   `(name params rest constants source span)`, as the table writer takes
;;  *   them (`entry-text`).
;;  * @property {list|boolean} restore - Its restore sequence, or #f where a
;;  *   form in it cannot be written down.
;;  * @property {list} restored - The names its code restores.
;;  * @property {list} forms - The forms its loading ran.
;;  * @property {pair} declaration - Its `define-library` form.
;;  * @property {list} declined - What the compiler declined, `declined` records.
;;  * @property {list} unserializable - The names of procedures left out for a
;;  *   constant that cannot be written down.
;;  */
(define-record-type library-table
  (make-library-table key fingerprint files entries restore restored forms declaration
                      declined unserializable)
  library-table?
  (key library-table-key)
  (fingerprint library-table-fingerprint)
  (files library-table-files)
  (entries library-table-entries)
  (restore library-table-restore)
  (restored library-table-restored)
  (forms library-table-forms)
  (declaration library-table-declaration)
  (declined library-table-declined)
  (unserializable library-table-unserializable))

;; /**
;;  * The span of source a datum the reader made, or an interpreted closure,
;;  * came from, a JavaScript object; or #f.
;;  * @param {*} x - The datum or closure.
;;  * @returns {object|boolean}
;;  */
(define (source-span x)
  (and (or (pair? x) (procedure? x))
       (let ((span (js-ref x "source")))
         (and (not (js-undefined? span)) (not (null? span)) span))))

;; /**
;;  * Whether one span of source lies within another.
;;  * @param {object|boolean} inner - The inner span, or #f.
;;  * @param {object|boolean} outer - The outer span, or #f.
;;  * @returns {boolean}
;;  */
(define (span-within? inner outer)
  (define (before? l1 c1 l2 c2) (or (< l1 l2) (and (= l1 l2) (<= c1 c2))))
  (and inner outer
       (equal? (js-ref inner "filename") (js-ref outer "filename"))
       (before? (js-ref outer "line") (js-ref outer "column") (js-ref inner "line") (js-ref inner "column"))
       (before? (js-ref inner "endLine") (js-ref inner "endColumn")
                (js-ref outer "endLine") (js-ref outer "endColumn"))))

;; /**
;;  * The test `restore-sequence` asks of each procedure definition: whether
;;  * it made its name's final binding, so that the table's code can bind the
;;  * name in its place -- the closure bound now made from source inside the
;;  * form, in the library's own environment, and compiled in an entry.
;;  * @param {object} env - The library's environment, before its code is
;;  *   installed.
;;  * @param {list} generated - The `generated` records its table holds.
;;  * @returns {procedure} From a name and its defining form to a boolean.
;;  */
(define (made-final? env generated)
  (let ((by-name (map (lambda (g) (cons (generated-name g) g)) generated)))
    (lambda (name form)
      (let ((closure (%environment-own-value env name))
            (entry (assoc (symbol->string name) by-name)))
        (and entry
             (eq? (generated-closure (cdr entry)) closure)
             (eq? (js-ref closure "env") env)
             (span-within? (source-span closure) (source-span form)))))))

;; /**
;;  * A procedure's entry in its table: its name, its parameters, its rest
;;  * parameter or #f, its constants, its code, and -- where the table's code
;;  * restores it -- the span of its source as JSON, or #f.
;;  * @param {generated} g - What the compiler generated for it.
;;  * @param {list} restored - The names the table restores.
;;  * @returns {list}
;;  */
(define (table-entry g restored)
  (let* ((closure (generated-closure g))
         (rest (js-ref closure "restParam")))
    (list (generated-name g)
          (vector->list (js-ref closure "params"))
          (if (string? rest) rest #f)
          (generated-constants g)
          (generated-source g)
          (and (member (generated-name g) restored)
               (js-invoke (js-eval "JSON") "stringify" (source-span closure))))))

;; /**
;;  * The table of a library just loaded, from code generated for the
;;  * procedures it defines.
;;  * @param {procedure} read-source - A `source-reader` of its files.
;;  * @param {list} name - Its name, as strings.
;;  * @param {object} env - Its environment.
;;  * @param {list} forms - The forms its loading ran (`take-noted!`).
;;  * @param {list} generated - What the compiler generated for it, `generated`
;;  *   records: all its procedures', or those worth shipping.
;;  * @param {list} declined - What the compiler declined.
;;  * @returns {library-table}
;;  */
(define (library-table-for read-source name env forms generated declined)
  (let*-values (((files) (library-files read-source name))
                ((fingerprint) (fingerprint-sources (map read-source files)))
                ;; Not every value can be written down (`constants-expression`):
                ;; a procedure whose constant cannot be is left out, and runs
                ;; interpreted.
                ((writable unserializable)
                 (partition (lambda (g) (constants-expression (generated-constants g))) generated))
                ;; Decided before the code is installed, while the closures
                ;; are bound.
                ((restore) (restore-sequence forms (made-final? env writable)))
                ((restored) (filter-map (lambda (item) (and (eq? (car item) 'procedure)
                                                           (symbol->string (cadr item))))
                                        restore))
                ((entries) (map (lambda (g) (table-entry g restored)) writable)))
    (make-library-table (library-key name) fingerprint files entries
                        (and (restore-writable? restore) restore)
                        restored forms (library-declaration read-source name) declined
                        (map generated-name unserializable))))

;; /**
;;  * Installs a table's code in its library, as the table will be installed
;;  * at run time, so that a library loaded after it calls it compiled, as it
;;  * will in the bundle.
;;  * @param {object} env - The library's environment.
;;  * @param {library-table} table - Its table.
;;  */
(define (install-table-code! env table)
  (install-generated! env (library-table-fingerprint table) (library-table-files table)
                      (library-table-entries table)))

;; /**
;;  * A table as the table writer takes it (`table-text`).
;;  * @param {library-table} table - The table.
;;  * @returns {list} `(key fingerprint files entries restore declaration)`.
;;  */
(define (library-table-list table)
  (list (library-table-key table) (library-table-fingerprint table) (library-table-files table)
        (library-table-entries table) (library-table-restore table) (library-table-declaration table)))

;; /**
;;  * Writes tables down as a JavaScript module.
;;  * @param {string} path - The module's file.
;;  * @param {string} generator - The build step that wrote it.
;;  * @param {string} title - One line saying what the tables hold.
;;  * @param {list} tables - The tables, `library-table` records.
;;  */
(define (write-tables! path generator title tables)
  (let ((text (render-tables generator title (runtime-interface) (map library-table-list tables))))
    (call-with-output-file path (lambda (port) (write-string text port)))))

;; /**
;;  * Writes a line, its parts displayed one after another.
;;  * @param {...*} parts - The parts.
;;  */
(define (say . parts)
  (for-each display parts)
  (newline))

;; /**
;;  * Says whether a table restores its library, and how much of it.
;;  * @param {library-table} table - The table.
;;  * @param {string} indent - What the line begins with.
;;  */
(define (report-restoring table indent)
  (say indent (if (library-table-restore table)
                  (string-append "restores " (number->string (length (library-table-restored table)))
                                 " procedures, and runs "
                                 (number->string (- (length (library-table-forms table))
                                                    (length (library-table-restored table))))
                                 " forms")
                  "cannot be restored: a form it runs cannot be written down")))

;; /**
;;  * Says which procedures a table left out for a constant that cannot be
;;  * written down, if any.
;;  * @param {library-table} table - The table.
;;  * @param {string} indent - What the line begins with.
;;  */
(define (report-unserializable table indent)
  (let ((left-out (library-table-unserializable table)))
    (if (pair? left-out)
        (say indent (length left-out) " left out for a constant that cannot be written down: "
             (string-join left-out " ")))))
