;; Writing prebuilt tables as JavaScript modules.
;;
;; A table maps each library's key to the procedures compiled from it: for
;; each, its parameters' names, its constant pool, and the JavaScript the
;; compiler generated, as a factory taking the runtime, the environment its
;; globals resolve in, and the pool. Two build steps write one -- the shipped
;; libraries' and the compiler's own -- with this, so the shape they write and
;; the one `installLibraryTable` reads stay one shape.

;; ---------------------------------------------------------------------------
;; Strings, as JSON writes them
;; ---------------------------------------------------------------------------

;; /**
;;  * A string as a JavaScript string literal, as `JSON.stringify` writes it:
;;  * a quote and a backslash escaped, the control characters as `\b`, `\f`,
;;  * `\n`, `\r`, `\t` or a `\u` escape, and every other character as it is.
;;  * @param {string} s - The string.
;;  * @returns {string}
;;  */
(define (json-string s)
  (let ((out (open-output-string)))
    (write-char #\" out)
    (string-for-each
      (lambda (c)
        (let ((code (char->integer c)))
          (cond ((char=? c #\") (write-string "\\\"" out))
                ((char=? c #\\) (write-string "\\\\" out))
                ((= code 8) (write-string "\\b" out))
                ((= code 12) (write-string "\\f" out))
                ((char=? c #\newline) (write-string "\\n" out))
                ((char=? c #\return) (write-string "\\r" out))
                ((char=? c #\tab) (write-string "\\t" out))
                ((< code 32)
                 (write-string (if (< code 16) "\\u000" "\\u00") out)
                 (write-string (number->string code 16) out))
                (else (write-char c out)))))
      s)
    (write-char #\" out)
    (get-output-string out)))

;; /**
;;  * Strings as a JavaScript array literal, as `JSON.stringify` writes one.
;;  * @param {list} strings - The strings.
;;  * @returns {string}
;;  */
(define (json-strings strings)
  (string-append "[" (string-join (map json-string strings) ",") "]"))

;; ---------------------------------------------------------------------------
;; Constant pools
;; ---------------------------------------------------------------------------
;;
;; The emitter writes immediates straight into the code it generates, and
;; pools everything else -- symbols, pairs, characters -- since those have
;; identity that `eq?` can observe: the pool is built once and handed to the
;; procedure's factory. A symbol written as `intern("lambda")` is the same
;; object read back. A pair, a character, and what is inside them are made
;; anew, once, when the module loads, and that object is then the constant
;; every call sees, which is all a literal promises; what cannot be kept is
;; identity with the interpreted procedure's literal, which nothing could see
;; unless the literal escaped before the compiled procedure was installed.
;; Anything else -- a vector, a record -- cannot be written down, and the
;; procedure holding it is left out of the table, interpreted.

;; /**
;;  * JavaScript that rebuilds a constant, or #f if it cannot be written down.
;;  * @param {*} value - The constant.
;;  * @returns {string|boolean}
;;  */
(define (constant-expression value)
  (cond ((null? value) "null")
        ((eq? value #t) "true")
        ((eq? value #f) "false")
        ((exact-integer? value) (string-append (number->string value) "n"))
        ((and (real? value) (inexact? value))
         (and (finite? value) (number->string value)))
        ((string? value) (json-string value))
        ((symbol? value) (string-append "intern(" (json-string (symbol->string value)) ")"))
        ((char? value) (string-append "new Char(" (number->string (char->integer value)) ")"))
        ((pair? value)
         (let ((car-expression (constant-expression (car value)))
               (cdr-expression (constant-expression (cdr value))))
           (and car-expression cdr-expression
                (string-append "new Cons(" car-expression ", " cdr-expression ")"))))
        (else #f)))

;; /**
;;  * JavaScript that rebuilds a constant pool, as an array, or #f if some
;;  * constant in it cannot be written down.
;;  * @param {list} constants - The pool.
;;  * @returns {string|boolean}
;;  */
(define (constants-expression constants)
  (let ((expressions (map constant-expression constants)))
    (and (not (memq #f expressions))
         (string-append "[" (string-join expressions ", ") "]"))))

;; ---------------------------------------------------------------------------
;; Modules
;; ---------------------------------------------------------------------------

;; /**
;;  * One procedure's entry in a table, as the text of an object property.
;;  *
;;  * The generated code is a function body that declares the procedure, marks
;;  * it and returns it; wrapped in an arrow, it is a value the module can
;;  * export, with no `new Function` anywhere.
;;  * @param {list} entry - `(name params rest constants source)`: the
;;  *   procedure's name, its parameters' names, its rest parameter's name or
;;  *   #f, its constant pool, and its code. Every constant can be written down.
;;  * @returns {string}
;;  */
(define (entry-text entry)
  (let ((name (list-ref entry 0))
        (params (list-ref entry 1))
        (rest (list-ref entry 2))
        (constants (list-ref entry 3))
        (source (list-ref entry 4)))
    (string-append
      "      " (json-string name) ": {\n"
      "        params: " (json-strings params) ",\n"
      "        rest: " (if rest (json-string rest) "null") ",\n"
      "        constants: " (constants-expression constants) ",\n"
      "        make: (R, E, K) => {\n"
      (string-join (map (lambda (line) (string-append "        " line)) (string-split source "\n")) "\n")
      "\n        }\n"
      "      }")))

;; /**
;;  * One library's table, as the text of an object property.
;;  * @param {string} runtime - The fingerprint of the runtime interface.
;;  * @param {list} library - `(key fingerprint files entries)`: the library's
;;  *   key, the fingerprint of its sources, the sources in the order the
;;  *   fingerprint covers them -- its `.sld` and then each file it includes --
;;  *   and its entries (`entry-text`).
;;  * @returns {string}
;;  */
(define (table-text runtime library)
  (string-append
    "  " (json-string (list-ref library 0)) ": {\n"
    "    fingerprint: " (json-string (list-ref library 1)) ",\n"
    "    runtime: " (json-string runtime) ",\n"
    "    files: " (json-strings (list-ref library 2)) ",\n"
    "    procedures: {\n" (string-join (map entry-text (list-ref library 3)) ",\n") "\n    }\n"
    "  }"))

;; /**
;;  * Whether a constant is, or holds inside its pairs, a value a predicate is
;;  * true of.
;;  * @param {procedure} kind? - The predicate.
;;  * @param {*} value - The constant.
;;  * @returns {boolean}
;;  */
(define (holds? kind? value)
  (or (kind? value)
      (and (pair? value) (or (holds? kind? (car value)) (holds? kind? (cdr value))))))

;; /**
;;  * The import lines the constant pools need: each constructor only when
;;  * some constant is written with it, so a module of procedures with no
;;  * pooled constants has no dependencies.
;;  * @param {list} libraries - The libraries (`table-text`).
;;  * @returns {list} The lines.
;;  */
(define (import-lines libraries)
  (let ((constants (apply append (map (lambda (entry) (list-ref entry 3))
                                      (apply append (map (lambda (library) (list-ref library 3)) libraries))))))
    (define (used? kind?)
      (let loop ((cs constants)) (and (pair? cs) (or (holds? kind? (car cs)) (loop (cdr cs))))))
    (append (if (used? symbol?) '("import { intern } from '../core/interpreter/symbol.js';") '())
            (if (used? pair?) '("import { Cons } from '../core/interpreter/cons.js';") '())
            (if (used? char?) '("import { Char } from '../core/primitives/char_class.js';") '()))))

;; /**
;;  * The prebuilt tables of a set of libraries, as a JavaScript module.
;;  * @param {string} generator - The script writing it, for the banner.
;;  * @param {string} title - One line saying what the tables hold.
;;  * @param {string} runtime - The fingerprint of the runtime interface the
;;  *   generated code calls (`RUNTIME_INTERFACE` in src/compiler/prebuilt.js).
;;  * @param {list} libraries - One table per library (`table-text`).
;;  * @returns {string} The module's text.
;;  */
(define (render-tables generator title runtime libraries)
  (let ((imports (import-lines libraries)))
    (string-append
      "// Auto-generated by " generator " - do not edit manually\n"
      "//\n"
      "// " title "\n"
      "//\n"
      "// One table per library, keyed by its name. Each entry holds the JavaScript\n"
      "// the compiler would otherwise generate when the library loads, as a factory\n"
      "// taking the runtime, the environment its globals resolve in, and its constant\n"
      "// pool.\n"
      "//\n"
      "// Each table's `fingerprint` is of the sources it was generated from, and\n"
      "// `runtime` of the runtime interface its code calls.\n"
      "// `installLibraryTable` in src/compiler/prebuilt.js recomputes it and installs\n"
      "// nothing if it differs, so a stale build leaves those procedures interpreted\n"
      "// rather than running code for source that has since changed.\n"
      (if (null? imports) "" (string-append "\n" (string-join imports "\n") "\n"))
      "\n"
      "/** @type {Object<string, {fingerprint: string, runtime: string, files: string[], procedures: Object<string, {params: string[], rest: (string|null), constants: Array<*>, make: Function}>}>} */\n"
      "export const LIBRARIES = {\n"
      (string-join (map (lambda (library) (table-text runtime library)) libraries) ",\n")
      "\n};\n"
      "\n"
      "export default LIBRARIES;\n")))
