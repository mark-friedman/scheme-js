;; The library system
;;
;; What R7RS's libraries mean, as Scheme: a `define-library` form taken apart
;; into its declarations, an import set into the library it names and the
;; filters around it, and a `cond-expand` requirement decided. Reading files,
;; analyzing and running code, and the analyzer's tables of scopes and
;; keywords are the host's, which this is given what it needs from.
;;
;; Names are symbols here: a library's name is the list it is written as, of
;; symbols and exact integers, and the names it exports and an import set
;; filters are symbols.
;;
;; The top level is definitions of procedures and record types only, so that
;; the library can one day be installed from compiled code without running
;; this source: it is loaded at every start, before anything else.

;; ---------------------------------------------------------------------------
;; Small list helpers
;; ---------------------------------------------------------------------------

;; /**
;;  * The lists a procedure makes of each element of a list, appended: SRFI 1's
;;  * `append-map` for one list. Written here because SRFI 1 is not loaded when
;;  * the library system starts, and loading it at every start for this would
;;  * cost more than the library system itself.
;;  * @param {procedure} f - From an element to a list.
;;  * @param {list} xs - The list.
;;  * @returns {list}
;;  */
(define (append-each f xs)
  (let loop ((xs (reverse xs)) (acc '()))
    (if (null? xs)
        acc
        (loop (cdr xs) (append (f (car xs)) acc)))))

;; /**
;;  * Whether a procedure is true of every element of a list: SRFI 1's `every`,
;;  * returning only #t or #f, written here for the reason `append-each` is.
;;  * @param {procedure} ok? - The test.
;;  * @param {list} xs - The list.
;;  * @returns {boolean}
;;  */
(define (all? ok? xs)
  (or (null? xs) (and (ok? (car xs)) (all? ok? (cdr xs)))))

;; /**
;;  * Whether a procedure is true of any element of a list: SRFI 1's `any`,
;;  * returning only #t or #f, written here for the reason `append-each` is.
;;  * @param {procedure} ok? - The test.
;;  * @param {list} xs - The list.
;;  * @returns {boolean}
;;  */
(define (some? ok? xs)
  (and (pair? xs) (or (and (ok? (car xs)) #t) (some? ok? (cdr xs)))))

;; ---------------------------------------------------------------------------
;; Feature requirements
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether a `cond-expand` feature requirement is met (R7RS 4.2.1): a feature
;;  * present; `and`, `or` and `not` of requirements; or `library` and a
;;  * library that could be imported. Any other is not met.
;;  * @param {*} requirement - The requirement.
;;  * @param {list} features - The features present, as symbols.
;;  * @param {procedure} library-available? - From a library's name to whether
;;  *   it could be imported.
;;  * @returns {boolean}
;;  */
(define (requirement-met? requirement features library-available?)
  (define (met? r) (requirement-met? r features library-available?))
  (cond ((symbol? requirement) (and (memq requirement features) #t))
        ((not (pair? requirement)) #f)
        (else
         (case (car requirement)
           ((and) (all? met? (cdr requirement)))
           ((or) (some? met? (cdr requirement)))
           ((not)
            (if (not (= (length requirement) 2))
                (error "cond-expand: (not) requires exactly one requirement" requirement))
            (not (met? (cadr requirement))))
           ((library)
            (if (not (= (length requirement) 2))
                (error "cond-expand: (library) requires a library name" requirement))
            (and (library-available? (cadr requirement)) #t))
           (else #f)))))

;; ---------------------------------------------------------------------------
;; Import sets
;; ---------------------------------------------------------------------------

;; /**
;;  * An import set (R7RS 5.2): the library it imports from, and the filters
;;  * around it, innermost first, each applied to the names the one inside it
;;  * gives -- so that `(only (prefix lib p:) p:car)` keeps `p:car`, which no
;;  * fixed order of filters could say. A filter is `(only name ...)`,
;;  * `(except name ...)`, `(prefix . prefix)` or `(rename (from . to) ...)`.
;;  * @property {list} library-name - The library's name.
;;  * @property {list} steps - The filters, innermost first.
;;  */
(define-record-type import-set
  (make-import-set library-name steps)
  import-set?
  (library-name import-set-library-name)
  (steps import-set-steps))

;; /**
;;  * An import set, as written.
;;  *
;;  * A filter's keyword begins a filter only when an import set follows it,
;;  * since a library may be named `(only lib)`.
;;  * @param {list} spec - The import set as written.
;;  * @returns {import-set}
;;  */
(define (parse-import-set spec)
  (if (and (pair? spec)
           (memq (car spec) '(only except prefix rename))
           (pair? (cdr spec))
           (pair? (cadr spec)))
      (let ((inner (parse-import-set (cadr spec))))
        (make-import-set (import-set-library-name inner)
                         (append (import-set-steps inner) (list (import-filter (car spec) (cddr spec))))))
      (make-import-set spec '())))

;; /**
;;  * One filter of an import set.
;;  * @param {symbol} kind - `only`, `except`, `prefix` or `rename`.
;;  * @param {list} args - What follows the import set inside the filter.
;;  * @returns {pair} The filter.
;;  */
(define (import-filter kind args)
  (case kind
    ((only except) (cons kind args))
    ((prefix) (cons 'prefix (car args)))
    ((rename) (cons 'rename (map (lambda (renaming) (cons (car renaming) (cadr renaming))) args)))))

;; /**
;;  * The name an export is imported under, through an import set's filters,
;;  * or #f if a filter leaves it out.
;;  * @param {symbol} name - The name the library exports.
;;  * @param {list} steps - The filters, innermost first (`import-set-steps`).
;;  * @returns {symbol|boolean}
;;  */
(define (imported-name name steps)
  (cond ((not name) #f)
        ((null? steps) name)
        (else (imported-name (filtered-name (car steps) name) (cdr steps)))))

;; /**
;;  * A name through one filter, or #f if the filter leaves it out.
;;  * @param {pair} step - The filter.
;;  * @param {symbol} name - The name.
;;  * @returns {symbol|boolean}
;;  */
(define (filtered-name step name)
  (case (car step)
    ((only) (and (memq name (cdr step)) name))
    ((except) (and (not (memq name (cdr step))) name))
    ((prefix) (string->symbol (string-append (symbol->string (cdr step)) (symbol->string name))))
    ((rename) (let ((renaming (assq name (cdr step)))) (if renaming (cdr renaming) name)))))

;; ---------------------------------------------------------------------------
;; define-library
;; ---------------------------------------------------------------------------

;; /**
;;  * A `define-library` form taken apart (R7RS 5.6.1), its `cond-expand`
;;  * declarations decided. Each part lists its declarations' contents in the
;;  * order they were written.
;;  * @property {list} name - The library's name.
;;  * @property {list} exports - Each export, `(internal . external)`.
;;  * @property {list} imports - The import sets.
;;  * @property {list} body - The forms of its `begin` declarations.
;;  * @property {list} includes - The files `include` names.
;;  * @property {list} includes-ci - The files `include-ci` names.
;;  * @property {list} declaration-files - The files
;;  *   `include-library-declarations` names.
;;  */
(define-record-type library-definition
  (make-library-definition name exports imports body includes includes-ci declaration-files)
  library-definition?
  (name library-definition-name)
  (exports library-definition-exports)
  (imports library-definition-imports)
  (body library-definition-body)
  (includes library-definition-includes)
  (includes-ci library-definition-includes-ci)
  (declaration-files library-definition-declaration-files))

;; /**
;;  * A `define-library` form taken apart.
;;  * @param {list} form - The form.
;;  * @param {procedure} met? - From a feature requirement to whether it is
;;  *   met, for `cond-expand` (`requirement-met?` with the features and
;;  *   libraries there are).
;;  * @returns {library-definition}
;;  */
(define (parse-define-library form met?)
  (if (not (and (pair? form) (eq? (car form) 'define-library)))
      (error "define-library: expected a define-library form" form))
  (if (not (pair? (cdr form)))
      (error "define-library: requires a library name" form))
  (let ((declarations (decided-declarations (cddr form) met?)))
    (define (contents kind)
      (append-each (lambda (d) (if (eq? (car d) kind) (cdr d) '())) declarations))
    (make-library-definition
      (cadr form)
      (append-each export-specs (filter-kind 'export declarations))
      (map parse-import-set (contents 'import))
      (contents 'begin)
      (contents 'include)
      (contents 'include-ci)
      (contents 'include-library-declarations))))

;; /**
;;  * The declarations of a kind.
;;  * @param {symbol} kind - The declaration's keyword.
;;  * @param {list} declarations - The declarations.
;;  * @returns {list} Those of the kind, whole.
;;  */
(define (filter-kind kind declarations)
  (append-each (lambda (d) (if (eq? (car d) kind) (list d) '())) declarations))

;; /**
;;  * A library's declarations with each `cond-expand` replaced by the
;;  * declarations of the clause it takes -- the first whose requirement is
;;  * met, or its `else` -- decided in turn. An empty declaration is nothing,
;;  * and any other that is not one R7RS defines is an error.
;;  * @param {list} declarations - The declarations, as written.
;;  * @param {procedure} met? - As for `parse-define-library`.
;;  * @returns {list}
;;  */
(define (decided-declarations declarations met?)
  (append-each
    (lambda (d)
      (cond ((null? d) '())
            ((not (and (pair? d) (symbol? (car d))))
             (error "define-library: a declaration must be a list beginning with its keyword" d))
            ((eq? (car d) 'cond-expand) (decided-declarations (chosen-clause (cdr d) met?) met?))
            ((memq (car d) '(export import begin include include-ci include-library-declarations)) (list d))
            (else (error "define-library: unknown declaration" (car d)))))
    declarations))

;; /**
;;  * The declarations of the `cond-expand` clause taken: the first whose
;;  * requirement is met, or an `else` clause; none if neither.
;;  * @param {list} clauses - The clauses.
;;  * @param {procedure} met? - As for `parse-define-library`.
;;  * @returns {list}
;;  */
(define (chosen-clause clauses met?)
  (cond ((null? clauses) '())
        ((or (eq? (caar clauses) 'else) (met? (caar clauses))) (cdar clauses))
        (else (chosen-clause (cdr clauses) met?))))

;; /**
;;  * The exports an `export` declaration names, each `(internal . external)`:
;;  * a name exported as itself, or `(rename internal external)`.
;;  * @param {list} declaration - The declaration.
;;  * @returns {list}
;;  */
(define (export-specs declaration)
  (map (lambda (spec)
         (cond ((symbol? spec) (cons spec spec))
               ((and (pair? spec) (eq? (car spec) 'rename) (= (length spec) 3)
                     (symbol? (cadr spec)) (symbol? (caddr spec)))
                (cons (cadr spec) (caddr spec)))
               (else (error "define-library: an export is a name or (rename internal external)" spec))))
       (cdr declaration)))
