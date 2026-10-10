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
;; pools everything else -- symbols, pairs, characters, exact ratios, complex
;; numbers -- since those have
;; identity that `eq?` can observe: the pool is built once and handed to the
;; procedure's factory. A symbol written as `intern("lambda")` is the same
;; object read back. A pair, a character, and what is inside them are made
;; anew, once, when the module loads, and that object is then the constant
;; every call sees, which is all a literal promises; what cannot be kept is
;; identity with the interpreted procedure's literal, which nothing could see
;; unless the literal escaped before the compiled procedure was installed.
;; Anything else -- a record, a procedure -- cannot be written down, and the
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
        ;; An exact integer is a JavaScript number in the safe range, a BigInt
        ;; beyond it; an inexact real a number, boxed if its value is an
        ;; integer (src/core/interpreter/number_representation.js).
        ((exact-integer? value)
         (if (<= (abs value) largest-safe-integer)
             (number->string value)
             (string-append (number->string value) "n")))
        ((inexact-integer? value) (string-append "new Flonum(" (number->string value) ")"))
        ;; An exact ratio is a `Rational` of two BigInts
        ;; (src/core/primitives/rational.js).
        ((exact-ratio? value)
         (string-append "new Rational(" (number->string (numerator value)) "n, "
                        (number->string (denominator value)) "n)"))
        ;; A complex number is a `Complex` of its parts, exact or not as it is
        ;; (src/core/primitives/complex.js).
        ((nonreal? value)
         (string-append "new Complex(" (complex-part (real-part value)) ", "
                        (complex-part (imag-part value)) ", " (if (exact? value) "true" "false") ")"))
        ((and (real? value) (inexact? value))
         (cond ((finite? value) (number->string value))
               ((nan? value) "NaN")
               ((positive? value) "Infinity")
               (else "-Infinity")))
        ((js-undefined? value) "undefined")
        ((string? value) (json-string value))
        ((symbol? value) (string-append "intern(" (json-string (symbol->string value)) ")"))
        ((char? value) (string-append "new Char(" (number->string (char->integer value)) ")"))
        ((pair? value)
         (let ((car-expression (constant-expression (car value)))
               (cdr-expression (constant-expression (cdr value))))
           (and car-expression cdr-expression
                (string-append "new Cons(" car-expression ", " cdr-expression ")"))))
        ;; A vector is a JavaScript array.
        ((vector? value)
         (let ((expressions (map constant-expression (vector->list value))))
           (and (not (memq #f expressions))
                (string-append "[" (string-join expressions ", ") "]"))))
        ;; A library's environment, which code that refers to a library's own
        ;; binding reads it from, is written as the library's name, and found
        ;; where the procedure is installed (`poolOf` in src/compiler/prebuilt.js).
        ((library-environment-name value)
         => (lambda (name) (string-append "{library: " (json-strings name) "}")))
        (else #f)))

;; /**
;;  * The name of the library an environment is, as a list of strings, or #f
;;  * for any other value.
;;  * @param {*} value - The value.
;;  * @returns {list|boolean}
;;  */
(define (library-environment-name value)
  (let ((name (js-ref value "libraryName")))
    (and (vector? name) (vector->list name))))

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
;; Data, as JSON
;; ---------------------------------------------------------------------------
;;
;; What restores a library -- its forms, as core forms, and its
;; `define-library` form -- is data the module holds as JSON text, in a string,
;; which costs next to nothing to load and is made into Scheme data only for a
;; library that is loaded (`decodeDatum` in src/compiler/prebuilt.js). A symbol
;; is a JSON string; `null` the empty list; an exact integer a JSON number, or
;; `["n", digits]` where JavaScript's numbers could not hold it; `true` and
;; `false` themselves; and anything else an array whose first element says
;; what it is: `["s", text]` a string, `["f", text]` an inexact number,
;; `["c", code]` a character, `["u"]` JavaScript's `undefined`, `["l", item
;; ...]` a list, `["d", tail, item ...]` a dotted list, `["v", item ...]` a
;; vector, and `["e", part ...]` the environment of the library of that name,
;; which a core form names where a library's macro refers to its own binding.

;; /**
;;  * The largest integer JavaScript's numbers hold exactly.
;;  */
(define largest-safe-integer 9007199254740991)

;; /**
;;  * A datum as JSON text, or #f if it cannot be written down.
;;  * @param {*} value - The datum.
;;  * @returns {string|boolean}
;;  */
(define (json-datum value)
  (define (tagged tag parts)
    (and (not (memq #f parts))
         (string-append "[" (json-string tag) (apply string-append (map (lambda (p) (string-append "," p)) parts)) "]")))
  (cond ((null? value) "null")
        ((eq? value #t) "true")
        ((eq? value #f) "false")
        ((symbol? value) (json-string (symbol->string value)))
        ((exact-integer? value)
         (if (<= (abs value) largest-safe-integer)
             (number->string value)
             (tagged "n" (list (json-string (number->string value))))))
        ((and (real? value) (inexact? value))
         (tagged "f" (list (json-string (cond ((nan? value) "NaN")
                                              ((infinite? value) (if (positive? value) "Infinity" "-Infinity"))
                                              (else (number->string value)))))))
        ((string? value) (tagged "s" (list (json-string value))))
        ((char? value) (tagged "c" (list (number->string (char->integer value)))))
        ((js-undefined? value) (tagged "u" '()))
        ((pair? value)
         (let loop ((rest value) (items '()))
           (cond ((pair? rest) (loop (cdr rest) (cons (json-datum (car rest)) items)))
                 ((null? rest) (tagged "l" (reverse items)))
                 (else (tagged "d" (cons (json-datum rest) (reverse items)))))))
        ((vector? value) (tagged "v" (map json-datum (vector->list value))))
        ((library-environment-name value) => (lambda (name) (tagged "e" (map json-string name))))
        (else #f)))

;; ---------------------------------------------------------------------------
;; What restores a library
;; ---------------------------------------------------------------------------
;;
;; A library's table restores it when every top-level form loading it runs is
;; either a procedure its code can bind at once, with no source run, or a form
;; the table holds to run in its place. Nearly every form is a procedure's
;; definition; the rest -- macros, record types, values -- are run in their
;; places, so that the library ends as its source would leave it. Each is
;; held as the core form it expanded into, which the evaluator runs with no
;; expander, so that the libraries the expander is written with can be
;; restored before it; or, where that holds something that cannot be written
;; down -- another library's environment, from its macro -- as the form,
;; expanded as it is run. A macro's definition, whose core form is nothing,
;; is held as one that binds the macro pending (`DefineSyntaxNode` in
;; src/core/interpreter/ast_nodes.js), its transformer made from the
;; definition the first time it is used.

;; /**
;;  * The name a form defines a procedure under, if it is a plain top-level
;;  * definition of one -- `(define (name . params) body ...)`, or `(define
;;  * name (lambda ...))` or with `case-lambda` -- which does nothing but bind
;;  * the name; else #f.
;;  * @param {*} form - The form.
;;  * @returns {symbol|boolean}
;;  */
(define (procedure-definition-name form)
  (and (pair? form) (eq? (car form) 'define) (pair? (cdr form))
       (let ((target (cadr form)))
         (cond ((pair? target) (and (symbol? (car target)) (car target)))
               ((and (symbol? target) (pair? (cddr form)) (null? (cdddr form))
                     (pair? (caddr form)) (memq (car (caddr form)) '(lambda case-lambda)))
                target)
               (else #f)))))

;; /**
;;  * The name a form defines a macro under, if it is a `define-syntax` or a
;;  * `define-macro`; else #f.
;;  * @param {*} form - The form.
;;  * @returns {symbol|boolean}
;;  */
(define (macro-definition-name form)
  (and (pair? form) (pair? (cdr form))
       (case (car form)
         ((define-syntax) (and (symbol? (cadr form)) (cadr form)))
         ((define-macro)
          (let ((target (cadr form)))
            (cond ((pair? target) (and (symbol? (car target)) (car target)))
                  ((symbol? target) target)
                  (else #f))))
         (else #f))))

;; /**
;;  * A library's restore sequence: the top-level forms its loading ran, in
;;  * order, each `(procedure name)` where it is a procedure's definition that
;;  * made the name's final binding, which the table's code restores; else
;;  * `(core core-form)`, a macro's definition as one that binds it pending, or
;;  * the form's core form where that can be written down; else `(form
;;  * form)`. A definition whose binding a later form replaced runs in its
;;  * place, so that what the forms between them see is what they saw.
;;  * @param {list} forms - The forms, in the order loading ran them, each
;;  *   `(form . core-form)`.
;;  * @param {procedure} made-final? - From a name and the form defining it to
;;  *   whether that form made the name's final binding and the table holds its
;;  *   procedure.
;;  * @returns {list}
;;  */
(define (restore-sequence forms made-final?)
  (map (lambda (noted)
         (let* ((form (car noted))
                (core (cdr noted))
                (name (procedure-definition-name form))
                (macro (macro-definition-name form)))
           (cond ((and name (made-final? name form)) (list 'procedure name))
                 ((and macro (equal? core '(lit ())))
                  (list 'core (list 'define-syntax macro form)))
                 ((json-datum core) (list 'core core))
                 (else (list 'form form)))))
       forms))

;; /**
;;  * Whether every form a restore sequence holds can be written down.
;;  * @param {list} restore - The sequence.
;;  * @returns {boolean}
;;  */
(define (restore-writable? restore)
  (let loop ((items restore))
    (or (null? items)
        (and (or (eq? (car (car items)) 'procedure) (json-datum (cadr (car items))))
             (loop (cdr items))))))

;; ---------------------------------------------------------------------------
;; Modules
;; ---------------------------------------------------------------------------

;; /**
;;  * One procedure's entry in a table, as the text of an object property.
;;  *
;;  * The generated code is a function body that declares the procedure, marks
;;  * it and returns it; wrapped in an arrow, it is a value the module can
;;  * export, with no `new Function` anywhere.
;;  * @param {list} entry - `(name params rest constants source span [lines])`:
;;  *   the procedure's name, its parameters' names, its rest parameter's name
;;  *   or #f, its constant pool, its code, for a procedure restored from the
;;  *   table the span of its source as a JavaScript object literal, or #f, and,
;;  *   if the module is to have a source map, its code's lines' spans, as the
;;  *   compiler gave them. Every constant can be written down.
;;  * @returns {list} Pieces (`code-piece`).
;;  */
(define (entry-pieces entry)
  (let ((name (list-ref entry 0))
        (params (list-ref entry 1))
        (rest (list-ref entry 2))
        (constants (list-ref entry 3))
        (source (list-ref entry 4))
        (span (list-ref entry 5))
        (lines (if (> (length entry) 6) (list-ref entry 6) '())))
    (list
      (string-append
        "      " (json-string name) ": {\n"
        "        params: " (json-strings params) ",\n"
        "        rest: " (if rest (json-string rest) "null") ",\n"
        "        constants: " (constants-expression constants) ",\n"
        (if span (string-append "        span: " span ",\n") "")
        "        make: (R, E, K) => {\n")
      (make-code-piece source lines "        ")
      "\n        }\n      }")))

;; /**
;;  * A library's restore sequence, as the text of an array: each top-level
;;  * form loading runs, in order, as `{procedure: name}` for one the table's
;;  * code restores, `{core: ...}`, a core form the evaluator runs, or `{form:
;;  * ...}`, the form itself, to be expanded and run as source is, each as JSON.
;;  * @param {list} restore - The sequence (`restore-sequence`), every form in
;;  *   it one that can be written down (`restore-writable?`).
;;  * @returns {string}
;;  */
(define (restore-text restore)
  (string-append
    "    restore: [\n"
    (string-join
      (map (lambda (item)
             (case (car item)
               ((procedure) (string-append "      {procedure: " (json-string (symbol->string (cadr item))) "}"))
               ((core) (string-append "      {core: " (json-string (json-datum (cadr item))) "}"))
               (else (string-append "      {form: " (json-string (json-datum (cadr item))) "}"))))
           restore)
      ",\n")
    "\n    ]\n"))

;; ---------------------------------------------------------------------------
;; A module's text, and each of its lines' spans
;; ---------------------------------------------------------------------------
;;
;; A module is written as pieces: text, and generated code, each of whose
;; lines carries the span of the source it was generated from. Its text is the
;; pieces one after another; its lines' spans -- those of its code's lines,
;; none for the rest -- are what its source map is made of.

;; /**
;;  * Generated code, as a piece of a module: its text, each line indented, and
;;  * the spans of its lines, as many as it has, or fewer.
;;  */
(define-record-type code-piece
  (make-code-piece source spans indent)
  code-piece?
  (source code-piece-source)
  (spans code-piece-spans)
  (indent code-piece-indent))

;; /**
;;  * A piece of code's lines, indented.
;;  * @param {code-piece} piece - The piece.
;;  * @returns {list} The lines.
;;  */
(define (code-piece-lines piece)
  (map (lambda (line) (string-append (code-piece-indent piece) line))
       (string-split (code-piece-source piece) "\n")))

;; /**
;;  * A module's text, from its pieces.
;;  * @param {list} pieces - Strings and `code-piece`s.
;;  * @returns {string}
;;  */
(define (pieces-text pieces)
  (apply string-append
         (map (lambda (piece) (if (code-piece? piece) (string-join (code-piece-lines piece) "\n") piece))
              pieces)))

;; /**
;;  * Each line of a module's text's span, or #f, from its pieces: a piece of
;;  * code begins where the text before it left off, at the start of a line.
;;  * @param {list} pieces - Strings and `code-piece`s.
;;  * @returns {list}
;;  */
(define (pieces-spans pieces)
  ;; `current` is the span of the line being written; `done` those of the
  ;; lines before it, latest first.
  (let loop ((pieces pieces) (current #f) (done '()))
    (cond ((null? pieces) (reverse (cons current done)))
          ((code-piece? (car pieces))
           (let code ((spans (code-piece-spans (car pieces)))
                      (lines (length (code-piece-lines (car pieces))))
                      (current current) (done done) (first? #t))
             (if (zero? lines)
                 (loop (cdr pieces) current done)
                 (let ((span (and (pair? spans) (car spans))))
                   (code (if (pair? spans) (cdr spans) '()) (- lines 1) span
                         (if first? done (cons current done)) #f)))))
          (else
           (let text ((newlines (newlines-in (car pieces))) (current current) (done done))
             (if (zero? newlines)
                 (loop (cdr pieces) current done)
                 (text (- newlines 1) #f (cons current done))))))))

;; /**
;;  * How many line breaks a string holds.
;;  * @param {string} s - The string.
;;  * @returns {integer}
;;  */
(define (newlines-in s)
  (let loop ((i 0) (n 0))
    (cond ((= i (string-length s)) n)
          ((char=? (string-ref s i) #\newline) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))

;; /**
;;  * Pieces with others between them, as `string-join` puts a string.
;;  * @param {list} groups - Lists of pieces.
;;  * @param {string} between - What goes between two.
;;  * @returns {list} Pieces.
;;  */
(define (join-pieces groups between)
  (if (null? groups)
      '()
      (apply append (car groups) (map (lambda (group) (cons between group)) (cdr groups)))))

;; /**
;;  * One library's table, as the text of an object property.
;;  * @param {string} runtime - The fingerprint of the runtime interface.
;;  * @param {list} library - `(key fingerprint files entries restore
;;  *   declaration)`: the library's key, the fingerprint of its sources, the
;;  *   sources in the order the fingerprint covers them -- its `.sld` and then
;;  *   each file it includes -- its entries (`entry-text`), its restore
;;  *   sequence (`restore-text`), or #f if it has none, and its
;;  *   `define-library` form, or #f: what the library system's seed takes
;;  *   instead of reading the `.sld`, which before the reader is loaded it
;;  *   could not.
;;  * @returns {list} Pieces.
;;  */
(define (table-pieces runtime library)
  (let ((restore (list-ref library 4))
        (declaration (list-ref library 5)))
    (append
      (list (string-append
              "  " (json-string (list-ref library 0)) ": {\n"
              "    fingerprint: " (json-string (list-ref library 1)) ",\n"
              "    runtime: " (json-string runtime) ",\n"
              "    files: " (json-strings (list-ref library 2)) ",\n"
              (if declaration (string-append "    declaration: " (json-string (json-datum declaration)) ",\n") "")
              "    procedures: {\n"))
      (join-pieces (map entry-pieces (list-ref library 3)) ",\n")
      (list (string-append
              "\n    }"
              (if restore (string-append ",\n" (restore-text restore)) "\n")
              "  }")))))

;; /**
;;  * Whether a constant is, or holds inside its pairs, a value a predicate is
;;  * true of.
;;  * @param {procedure} kind? - The predicate.
;;  * @param {*} value - The constant.
;;  * @returns {boolean}
;;  */
(define (holds? kind? value)
  (or (kind? value)
      (and (pair? value) (or (holds? kind? (car value)) (holds? kind? (cdr value))))
      (and (vector? value)
           (let loop ((i 0))
             (and (< i (vector-length value))
                  (or (holds? kind? (vector-ref value i)) (loop (+ i 1))))))))

;; /**
;;  * The import lines the constant pools need: each constructor only when
;;  * some constant is written with it, so a module of procedures with none has
;;  * no dependencies.
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
            (if (used? char?) '("import { Char } from '../core/primitives/char_class.js';") '())
            (if (used? inexact-integer?)
                '("import { Flonum } from '../core/interpreter/number_representation.js';")
                '())
            (if (used? (lambda (v) (or (exact-ratio? v)
                                       (and (nonreal? v) (or (exact-ratio? (real-part v))
                                                             (exact-ratio? (imag-part v)))))))
                '("import { Rational } from '../core/primitives/rational.js';")
                '())
            (if (used? nonreal?) '("import { Complex } from '../core/primitives/complex.js';") '()))))

;; /**
;;  * Whether a value is a number that is not real.
;;  * @param {*} value - The value.
;;  * @returns {boolean}
;;  */
(define (nonreal? value)
  (and (number? value) (not (real? value))))

;; /**
;;  * A part of a complex number as the `Complex` constructor takes it: an
;;  * exact integer a BigInt, an exact ratio a `Rational`, an inexact real a
;;  * number.
;;  * @param {real} part - The part.
;;  * @returns {string}
;;  */
(define (complex-part part)
  (cond ((exact-integer? part) (string-append (number->string part) "n"))
        ((exact? part)
         (string-append "new Rational(" (number->string (numerator part)) "n, "
                        (number->string (denominator part)) "n)"))
        ((nan? part) "NaN")
        ((infinite? part) (if (positive? part) "Infinity" "-Infinity"))
        (else (number->string part))))

;; /**
;;  * Whether a value is an exact rational that is not an integer.
;;  * @param {*} value - The value.
;;  * @returns {boolean}
;;  */
(define (exact-ratio? value)
  (and (rational? value) (exact? value) (not (integer? value))))

;; /**
;;  * Whether a value is an inexact real whose value is an integer, which
;;  * JavaScript holds boxed, as a `Flonum`.
;;  * @param {*} value - The value.
;;  * @returns {boolean}
;;  */
(define (inexact-integer? value)
  (and (real? value) (inexact? value) (integer? value)))

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
  (pieces-text (module-pieces generator title runtime libraries)))

;; /**
;;  * `render-tables`, and each line of the module's span, or #f, for its
;;  * source map: the spans of its entries' code, given as each entry's seventh
;;  * element.
;;  * @returns {pair} `(text . spans)`.
;;  */
(define (render-tables-with-spans generator title runtime libraries)
  (let ((pieces (module-pieces generator title runtime libraries)))
    (cons (pieces-text pieces) (pieces-spans pieces))))

;; /**
;;  * The pieces of a module of tables (`render-tables`).
;;  * @returns {list} Pieces.
;;  */
(define (module-pieces generator title runtime libraries)
  (let ((imports (import-lines libraries)))
    (append
     (list (string-append
      "// Auto-generated by " generator " - do not edit manually\n"
      "//\n"
      "// " title "\n"
      "//\n"
      "// One table per library, keyed by its name. Each entry holds the JavaScript\n"
      "// the compiler would otherwise generate when the library loads, as a factory\n"
      "// taking the runtime, the environment its globals resolve in, and its constant\n"
      "// pool.\n"
      "//\n"
      "// A table that can restore its library has a `restore` sequence: every\n"
      "// top-level form loading the library runs, in order, each a procedure its code\n"
      "// restores, which needs no source run, the core form it expanded into, which\n"
      "// the evaluator runs, or the form itself, expanded and run as source is.\n"
      "//\n"
      "// Each table's `fingerprint` is of the sources it was generated from, and\n"
      "// `runtime` of the runtime interface its code calls.\n"
      "// `installLibraryTable` in src/compiler/prebuilt.js recomputes it and installs\n"
      "// nothing if it differs, so a stale build leaves those procedures interpreted\n"
      "// rather than running code for source that has since changed.\n"
      (if (null? imports) "" (string-append "\n" (string-join imports "\n") "\n"))
      "\n"
      "/** @type {Object<string, {fingerprint: string, runtime: string, files: string[], procedures: Object<string, {params: string[], rest: (string|null), constants: Array<*>, span?: Object, make: Function}>, declaration?: string, restore?: Array<{procedure: string}|{core: string}|{form: string}>}>} */\n"
      "export const LIBRARIES = {\n"))
     (join-pieces (map (lambda (library) (table-pieces runtime library)) libraries) ",\n")
     (list "\n};\n"
           "\n"
           "export default LIBRARIES;\n"))))
