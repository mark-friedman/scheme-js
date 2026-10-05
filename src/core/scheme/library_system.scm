;; The library system
;;
;; What R7RS's libraries mean, as Scheme: a `define-library` form taken apart
;; into its declarations, an import set into the library it names and the
;; filters around it, a `cond-expand` requirement decided, the registries of
;; loaded libraries, and loading a library -- its imports, the files it
;; includes, its body, and the table of what it exports.
;;
;; What only the host can do it is given: the file resolver, a procedure from
;; a path to a file's text (and the load hook, called with each library
;; loaded by name), and through primitives the reader, environments, and the
;; analyzer's tables of scopes and syntactic keywords. Analyzing and running a
;; library's body is the host's too, the `evaluate` procedure a loader holds.
;;
;; Names are symbols here: a library's name is the list it is written as, of
;; symbols and exact integers, and the names it exports and an import set
;; filters are symbols.
;;
;; The top level is definitions of procedures and record types only, so that
;; the library can one day be installed from compiled code without running
;; this source: it is loaded at every start, before anything else. A registry
;; is made by whoever starts the system, which holds it.

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

;; /**
;;  * A list's elements folded into a value from the left: SRFI 1's `fold` for
;;  * one list, written here for the reason `append-each` is.
;;  * @param {procedure} kons - From an element and the value so far to the
;;  *   next value.
;;  * @param {*} knil - The value to start from.
;;  * @param {list} xs - The list.
;;  * @returns {*}
;;  */
(define (fold kons knil xs)
  (if (null? xs) knil (fold kons (kons (car xs) knil) (cdr xs))))

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
  (parse-declarations (cadr form) (cddr form) met?))

;; /**
;;  * Library declarations taken apart, as `parse-define-library` takes apart
;;  * a library's: its own, or those a file `include-library-declarations`
;;  * names holds.
;;  * @param {list|boolean} name - The library's name, or #f for a file's.
;;  * @param {list} declarations - The declarations, as written.
;;  * @param {procedure} met? - As for `parse-define-library`.
;;  * @returns {library-definition}
;;  */
(define (parse-declarations name declarations met?)
  (let ((declarations (decided-declarations declarations met?)))
    (define (contents kind)
      (append-each (lambda (d) (if (eq? (car d) kind) (cdr d) '())) declarations))
    (make-library-definition
      name
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

;; ---------------------------------------------------------------------------
;; Library names
;; ---------------------------------------------------------------------------

;; /**
;;  * The parts of a library's name as strings, as the file resolver is given
;;  * them: an identifier's name, an exact integer in decimal.
;;  * @param {list} name - The library's name.
;;  * @returns {list} Strings.
;;  */
(define (name-strings name)
  (define (wrong) (error "library: a library's name is a list of identifiers and exact integers" name))
  (if (not (pair? name)) (wrong))
  (map (lambda (part)
         (cond ((symbol? part) (symbol->string part))
               ((and (exact-integer? part) (not (negative? part))) (number->string part))
               (else (wrong))))
       name))

;; /**
;;  * The key a library is registered under: the parts of its name joined by
;;  * periods, "scheme.base" for `(scheme base)`. A part that is a number and
;;  * one that is the identifier written the same are one key, as they are one
;;  * file to the resolver.
;;  * @param {list} name - The library's name.
;;  * @returns {string}
;;  */
(define (library-key name)
  (joined (name-strings name) "."))

;; /**
;;  * The name a library's source is read under, for the locations of its
;;  * forms: "scheme/base" for `(scheme base)`.
;;  * @param {list} name - The library's name.
;;  * @returns {string}
;;  */
(define (library-path name)
  (joined (name-strings name) "/"))

;; /**
;;  * The path the resolver is given for a file a library includes: the file's
;;  * name in place of the last part of the library's.
;;  * @param {list} name - The library's name.
;;  * @param {string} file - The file's name, as the library writes it.
;;  * @returns {list} Strings.
;;  */
(define (include-path name file)
  (let loop ((parts (name-strings name)))
    (if (null? (cdr parts))
        (list file)
        (cons (car parts) (loop (cdr parts))))))

;; /**
;;  * Strings joined by a separator.
;;  * @param {list} strings - At least one string.
;;  * @param {string} separator - What goes between them.
;;  * @returns {string}
;;  */
(define (joined strings separator)
  (if (null? (cdr strings))
      (car strings)
      (string-append (car strings) separator (joined (cdr strings) separator))))

;; ---------------------------------------------------------------------------
;; Registries
;; ---------------------------------------------------------------------------

;; /**
;;  * A library loaded.
;;  * @property {list} exports - Each export, `(name . value)`; a syntactic
;;  *   keyword's value is a `syntactic-keyword`.
;;  * @property {object} environment - The environment its body ran in.
;;  */
(define-record-type library
  (make-library exports environment)
  library?
  (exports library-exports)
  (environment library-environment))

;; /**
;;  * A syntactic keyword a library exports: a macro, with its transformer, or
;;  * a special form or auxiliary keyword, which has none. Imported, it is bound
;;  * in the analyzer's tables rather than in an environment, under the name it
;;  * is imported as, and stays the keyword it is though the name is another.
;;  * @property {symbol} name - The keyword's own name.
;;  * @property {procedure|boolean} transformer - A macro's transformer, or #f.
;;  */
(define-record-type syntactic-keyword
  (make-syntactic-keyword name transformer)
  syntactic-keyword?
  (name syntactic-keyword-name)
  (transformer syntactic-keyword-transformer))

;; /**
;;  * The libraries loaded, and how to load more: one for a program and the
;;  * libraries it imports, and others made for a while by tools that run
;;  * Scheme on a program's behalf, apart from it.
;;  * @property {list} libraries - Each library, `(key . library)`, the latest
;;  *   first.
;;  * @property {procedure|boolean} resolver - The host's file resolver, called
;;  *   through `%resolve`; or #f for none.
;;  * @property {procedure|boolean} load-hook - The host's procedure called
;;  *   with each library loaded by name, through `%call-load-hook`; or #f.
;;  * @property {list} features - The features `cond-expand` finds, as symbols.
;;  * @property {object} compiled-over - Each compiled procedure installed over
;;  *   an interpreted closure while the registry was current, in an `eq`
;;  *   store, mapped to `(closure . environment)`: the closure, and the
;;  *   environment the procedure was installed into (`record-compiled-over!`).
;;  * @property {procedure|boolean} restorer - The host's restorer, or #f:
;;  *   from a library's name and its files' text, as strings, to
;;  *   `(bind . items)` if a prebuilt table built from that text restores the
;;  *   library, else #f. `items` are the library's top-level forms in the
;;  *   order loading runs them, each `(procedure name)`, bound by `(bind env
;;  *   name)` from compiled code, or `(form form)`, run as source is.
;;  */
(define-record-type library-registry
  (make-registry libraries resolver load-hook features compiled-over restorer)
  library-registry?
  (libraries registry-libraries set-registry-libraries!)
  (resolver registry-resolver set-registry-resolver!)
  (load-hook registry-load-hook set-registry-load-hook!)
  (features registry-features set-registry-features!)
  (compiled-over registry-compiled-over)
  (restorer registry-restorer set-registry-restorer!))

;; /**
;;  * A registry with no libraries in it.
;;  * @param {procedure|boolean} resolver - The host's file resolver, or #f.
;;  * @param {procedure|boolean} load-hook - The host's load hook, or #f.
;;  * @param {list} features - The features `cond-expand` finds.
;;  * @returns {library-registry}
;;  */
(define (make-library-registry resolver load-hook features)
  (make-registry '() resolver load-hook features (%make-hash-store 'eq) #f))

;; /**
;;  * The features this implementation has, on a host: R7RS's, its own name,
;;  * the numbers it has, and `node` or `browser`.
;;  * @param {symbol} host - The host's feature.
;;  * @returns {list}
;;  */
(define (standard-features host)
  (list 'r7rs 'scheme-js 'exact-closed 'ratios 'ieee-float 'full-unicode host))

;; /**
;;  * The features `cond-expand` finds in a registry, as `(features)` returns
;;  * them (R7RS 6.14): in a list of the caller's own, so that a program
;;  * changing the list changes nothing `cond-expand` finds.
;;  * @param {library-registry} registry - The registry.
;;  * @returns {list} The features, as symbols.
;;  */
(define (registry-feature-list registry)
  (list-copy (registry-features registry)))

;; /**
;;  * Adds a feature `cond-expand` finds.
;;  * @param {library-registry} registry - The registry.
;;  * @param {symbol} feature - The feature.
;;  */
(define (add-feature! registry feature)
  (if (not (memq feature (registry-features registry)))
      (set-registry-features! registry (append (registry-features registry) (list feature)))))

;; /**
;;  * The library registered under a key, or #f.
;;  * @param {library-registry} registry - The registry.
;;  * @param {string} key - The library's key (`library-key`).
;;  * @returns {library|boolean}
;;  */
(define (registered-library registry key)
  (let ((entry (assoc key (registry-libraries registry))))
    (and entry (cdr entry))))

;; /**
;;  * The exports of the library registered under a key, or #f.
;;  * @param {library-registry} registry - The registry.
;;  * @param {string} key - The library's key.
;;  * @returns {list|boolean}
;;  */
(define (registered-exports registry key)
  (let ((library (registered-library registry key)))
    (and library (library-exports library))))

;; /**
;;  * The environment of the library registered under a key, or #f.
;;  * @param {library-registry} registry - The registry.
;;  * @param {string} key - The library's key.
;;  * @returns {object|boolean}
;;  */
(define (registered-environment registry key)
  (let ((library (registered-library registry key)))
    (and library (library-environment library))))

;; /**
;;  * Registers a library under a key, in place of any registered there.
;;  * @param {library-registry} registry - The registry.
;;  * @param {string} key - The library's key.
;;  * @param {library} library - The library.
;;  */
(define (register-library! registry key library)
  (let ((entry (assoc key (registry-libraries registry))))
    (if entry
        (set-cdr! entry library)
        (set-registry-libraries! registry (cons (cons key library) (registry-libraries registry))))))

;; /**
;;  * Registers a library the host made: its exports and environment.
;;  * @param {library-registry} registry - The registry.
;;  * @param {string} key - The library's key.
;;  * @param {list} exports - Its exports, `(name . value)`.
;;  * @param {object} environment - Its environment.
;;  */
(define (register-exports! registry key exports environment)
  (register-library! registry key (make-library exports environment)))

;; /**
;;  * The keys of the libraries registered, in the order they were.
;;  * @param {library-registry} registry - The registry.
;;  * @returns {list} Strings.
;;  */
(define (registered-keys registry)
  (reverse (map car (registry-libraries registry))))

;; /**
;;  * Forgets every library registered.
;;  * @param {library-registry} registry - The registry.
;;  */
(define (clear-registry! registry)
  (set-registry-libraries! registry '()))

;; ---------------------------------------------------------------------------
;; Loading
;; ---------------------------------------------------------------------------

;; /**
;;  * What a load needs besides the registry: where files come from, and the
;;  * host's environment and evaluator for it.
;;  * @property {library-registry} registry - Where libraries are found and
;;  *   registered.
;;  * @property {procedure} resolve - From a path, a list of strings, to the
;;  *   file's text, or #f if the host can answer only later.
;;  * @property {object|boolean} base-environment - The environment a
;;  *   library's own is made inside, or #f where nothing is to be loaded.
;;  * @property {procedure|boolean} evaluate - From a form and a library's
;;  *   environment: analyzes and runs the form there, the library's scope the
;;  *   one its definitions are made in. Or #f where nothing is to be loaded.
;;  */
(define-record-type loader
  (make-loader registry resolve base-environment evaluate)
  loader?
  (registry loader-registry)
  (resolve loader-resolve)
  (base-environment loader-base-environment)
  (evaluate loader-evaluate))

;; /**
;;  * A loader that reads files through its registry's resolver.
;;  * @param {library-registry} registry - The registry.
;;  * @param {object|boolean} base-environment - As for `make-loader`.
;;  * @param {procedure|boolean} evaluate - As for `make-loader`.
;;  * @returns {loader}
;;  */
(define (registry-loader registry base-environment evaluate)
  (make-loader registry
               (let ((resolver (registry-resolver registry)))
                 (if resolver
                     (lambda (path) (%resolve resolver path))
                     (lambda (path) (error "library: no file resolver is set" (joined path "/")))))
               base-environment
               evaluate))

;; /**
;;  * Whether a feature requirement is met for a loader: its registry's
;;  * features, and the libraries it could load.
;;  * @param {loader} loader - The loader.
;;  * @returns {procedure} From a requirement to whether it is met.
;;  */
(define (feature-test loader)
  (lambda (requirement)
    (requirement-met? requirement
                      (registry-features (loader-registry loader))
                      (lambda (name) (library-available? loader name)))))

;; /**
;;  * Whether a feature requirement is met in a registry, for a `cond-expand`
;;  * in a program.
;;  * @param {library-registry} registry - The registry.
;;  * @param {*} requirement - The requirement.
;;  * @returns {boolean}
;;  */
(define (registry-requirement-met? registry requirement)
  ((feature-test (registry-loader registry #f #f)) requirement))

;; /**
;;  * Whether a library could be imported: whether it is loaded, or the
;;  * resolver finds, at once, a file declaring it. Nothing is loaded.
;;  *
;;  * `cond-expand` is decided as its form is analyzed, so a resolver that
;;  * would have to fetch the file cannot answer in time; for one, a library
;;  * not loaded yet is not available. A file declaring another library is not
;;  * this one, though a resolver finding libraries by the last part of their
;;  * names returns one. A name that is not a library's names none.
;;  * @param {loader} loader - The loader.
;;  * @param {*} name - The library's name, as written.
;;  * @returns {boolean}
;;  */
(define (library-available? loader name)
  (guard (condition (else #f))
    (let ((key (library-key name)))
      (or (and (registered-library (loader-registry loader) key) #t)
          (let ((source ((loader-resolve loader) (name-strings name))))
            (and (string? source)
                 (let ((form (first-define-library (%read-forms source #f #f))))
                   (and form (pair? (cdr form)) (equal? (library-key (cadr form)) key)))))))))

;; /**
;;  * The first `define-library` form among forms, or #f.
;;  * @param {list} forms - The forms.
;;  * @returns {pair|boolean}
;;  */
(define (first-define-library forms)
  (cond ((null? forms) #f)
        ((and (pair? (car forms)) (eq? (caar forms) 'define-library)) (car forms))
        (else (first-define-library (cdr forms)))))

;; /**
;;  * The forms of a file, read.
;;  * @param {loader} loader - The loader.
;;  * @param {list} path - The path the resolver is given.
;;  * @param {string} filename - The name the forms' locations give.
;;  * @param {boolean} fold-case? - Whether to read as `#!fold-case` does.
;;  * @returns {list}
;;  */
(define (read-library-file loader path filename fold-case?)
  (let ((source ((loader-resolve loader) path)))
    (if (not (string? source))
        (error "library: async resolver not supported in sync load" (joined path "/")))
    (%read-forms source filename fold-case?)))

;; /**
;;  * The exports of a library, loaded from its file if it is not loaded yet:
;;  * restored from its prebuilt table, if the registry's restorer has one
;;  * built from the library's files as they are, or else from its source. A
;;  * library loaded so is given to the load hook.
;;  * @param {loader} loader - The loader.
;;  * @param {list} name - The library's name.
;;  * @returns {list} Its exports, `(name . value)`.
;;  */
(define (load-library loader name)
  (let ((registry (loader-registry loader)))
    (cond ((registered-library registry (library-key name)) => library-exports)
          (else
           (let* ((path (name-strings name))
                  (restoring (restoring loader name path))
                  (form (if (and restoring (car restoring))
                            (car restoring)
                            (let ((forms (read-library-file loader path (library-path name) #f)))
                              (if (null? forms) (error "library: empty library file" (library-key name)))
                              (car forms))))
                  (definition (parse-define-library form (feature-test loader)))
                  (library (evaluate-definition! loader definition (and restoring (cdr restoring)))))
             (let ((hook (registry-load-hook registry)))
               (if hook
                   (%call-load-hook hook (name-strings (library-definition-name definition))
                                    (library-environment library))))
             (library-exports library))))))

;; /**
;;  * How the registry's restorer restores a library, if it can. It names the
;;  * files its table was built from -- the file declaring the library, then
;;  * those it includes, then its files of library declarations -- and, given
;;  * their text as it is now, restores the library if the table was built from
;;  * that text: giving its `define-library` form too, where the table has it,
;;  * so that the file is fetched, to be fingerprinted, but never read.
;;  * @param {loader} loader - The loader.
;;  * @param {list} name - The library's name.
;;  * @param {list} path - The path its file is found at.
;;  * @returns {pair|boolean} `(declaration bind . items)`, as the restorer
;;  *   gives it, the declaration #f where the table has none; or #f.
;;  */
(define (restoring loader name path)
  (let ((restorer (registry-restorer (loader-registry loader))))
    (and restorer
         (let ((files (restorer (name-strings name) #f)))
           (and (pair? files)
                (restorer (name-strings name)
                          (map (loader-resolve loader)
                               (cons path (map (lambda (file) (include-path name file)) (cdr files))))))))))

;; /**
;;  * Defines a library from a `define-library` form a program holds. The
;;  * load hook is not called, and no table restores it: the library is the
;;  * program's own code.
;;  * @param {loader} loader - The loader.
;;  * @param {list} form - The form.
;;  * @returns {list} Its exports.
;;  */
(define (define-library! loader form)
  (library-exports (evaluate-definition! loader (parse-define-library form (feature-test loader)) #f)))

;; /**
;;  * Makes a library of a definition, and registers it: its environment, its
;;  * imports, its body, and what it exports.
;;  *
;;  * Its body is its top-level forms in order: restored, each procedure bound
;;  * from the table's compiled code and each other form run, in its place; or
;;  * else read from its files and run. Its `begin` forms run before the files
;;  * it includes, whatever order they are declared in, as they always have
;;  * here -- `(scheme lazy)` declares its file before the macros its `begin`
;;  * defines -- and then each file of library declarations' forms, in turn.
;;  * @param {loader} loader - The loader.
;;  * @param {library-definition} definition - The definition.
;;  * @param {pair|boolean} restoring - How to restore it (`restoring`), or #f
;;  *   to run its source.
;;  * @returns {library}
;;  */
(define (evaluate-definition! loader definition restoring)
  (let* ((name (library-definition-name definition))
         (env (%make-library-environment (loader-base-environment loader) (name-strings name)))
         (definitions (cons definition (declared-definitions loader name definition)))
         (evaluate (lambda (form) ((loader-evaluate loader) form env))))
    (for-each (lambda (spec) (import! loader env spec))
              (append-each library-definition-imports definitions))
    (if restoring
        (for-each (lambda (item)
                    (if (eq? (car item) 'procedure)
                        ((car restoring) env (cadr item))
                        (evaluate (cadr item))))
                  (cdr restoring))
        (for-each evaluate (append-each (lambda (part) (body-forms loader name part)) definitions)))
    (let ((library (make-library (map (lambda (spec) (cons (cdr spec) (export-value env (car spec))))
                                      (append-each library-definition-exports definitions))
                                 env)))
      (register-library! (loader-registry loader) (library-key name) library)
      library)))

;; /**
;;  * The declarations of the files a definition's
;;  * `include-library-declarations` names, each followed by those of the
;;  * files it names in turn.
;;  * @param {loader} loader - The loader.
;;  * @param {list} name - The library's name, which the files are found by.
;;  * @param {library-definition} definition - The definition.
;;  * @returns {list} Definitions without names.
;;  */
(define (declared-definitions loader name definition)
  (append-each
    (lambda (file)
      (let ((declared (parse-declarations #f (read-library-file loader (include-path name file) file #f)
                                          (feature-test loader))))
        (cons declared (declared-definitions loader name declared))))
    (library-definition-declaration-files definition)))

;; /**
;;  * The forms a definition's body runs: its `begin` forms, then those of the
;;  * files it includes, then those of the files it includes folding case.
;;  * @param {loader} loader - The loader.
;;  * @param {list} name - The library's name, which the files are found by.
;;  * @param {library-definition} definition - The definition.
;;  * @returns {list}
;;  */
(define (body-forms loader name definition)
  (define (included fold-case?)
    (lambda (file) (read-library-file loader (include-path name file) file fold-case?)))
  (append (library-definition-body definition)
          (append-each (included #f) (library-definition-includes definition))
          (append-each (included #t) (library-definition-includes-ci definition))))

;; /**
;;  * The value a library exports for a name it binds: a variable's value; else
;;  * a keyword, under its own name or the one the library imported it as, a
;;  * macro with the transformer the name has here; else a JavaScript global,
;;  * which a variable's lookup falls back to -- last, since browsers define
;;  * globals named like keywords, `when` among them.
;;  * @param {object} env - The library's environment.
;;  * @param {symbol} internal - The name, as the library binds it.
;;  * @returns {*}
;;  */
(define (export-value env internal)
  (let* ((bound (%keyword-binding (%environment-scope env) internal))
         (keyword (if bound (car bound) internal)))
    (cond ((%environment-bound? env internal) (%environment-ref env internal))
          ((and bound (cdr bound)) (make-syntactic-keyword keyword (cdr bound)))
          ((and (not bound) (%global-macro keyword))
           => (lambda (transformer) (make-syntactic-keyword keyword transformer)))
          ((%special-keyword? keyword) (make-syntactic-keyword keyword #f))
          (else (%environment-ref env internal)))))

;; ---------------------------------------------------------------------------
;; Importing
;; ---------------------------------------------------------------------------

;; /**
;;  * Imports an import set into an environment, loading its library if need
;;  * be.
;;  * @param {loader} loader - The loader.
;;  * @param {object} env - The environment.
;;  * @param {import-set} spec - The import set.
;;  */
(define (import! loader env spec)
  (import-into! env (load-library loader (import-set-library-name spec)) (import-set-steps spec)))

;; /**
;;  * Imports import sets, as written, into an environment: an `import` form's.
;;  * @param {loader} loader - The loader.
;;  * @param {object} env - The environment.
;;  * @param {list} specs - The import sets.
;;  */
(define (import-sets! loader env specs)
  (for-each (lambda (spec) (import! loader env (parse-import-set spec))) specs))

;; /**
;;  * A program taken apart (R7RS 5.1): the import sets of the `import`
;;  * declarations it begins with, in order, and the forms after them. A
;;  * program that begins with none has no import sets; whoever runs it decides
;;  * what it sees then.
;;  * @param {list} forms - The program's forms, as read.
;;  * @returns {pair} (import-sets . forms)
;;  */
(define (program-parts forms)
  (if (and (pair? forms) (pair? (car forms)) (eq? (caar forms) 'import))
      (let ((rest (program-parts (cdr forms))))
        (cons (append (cdar forms) (car rest)) (cdr rest)))
      (cons '() forms)))

;; /**
;;  * Binds a library's exports in an environment, under the names an import
;;  * set's filters give them. A variable is defined in the environment; a
;;  * syntactic keyword is bound in the analyzer's tables, in the library whose
;;  * environment it is, or else at a program's top level.
;;  * @param {object} env - The environment.
;;  * @param {list} exports - The exports, `(name . value)`.
;;  * @param {list} steps - The filters, innermost first (`import-set-steps`).
;;  */
(define (import-into! env exports steps)
  (let ((scope (%environment-scope env)))
    (for-each (lambda (export)
                (let ((name (imported-name (car export) steps))
                      (value (cdr export)))
                  (cond ((not name))
                        ((syntactic-keyword? value)
                         (%define-keyword! scope name (syntactic-keyword-name value)
                                           (syntactic-keyword-transformer value)))
                        (else (%environment-define! env name value)))))
              exports)))

;; ---------------------------------------------------------------------------
;; Closures run compiled, and run as themselves for a debugger
;; ---------------------------------------------------------------------------
;;
;; A closure compiled -- by the compiler tier, or from a library's prebuilt
;; table installed over it -- stays the object every holder of it has and runs
;; compiled (`runCompiled` in src/core/interpreter/values.js): nothing that
;; holds it, a library's exports, a program's data, needs to change.
;;
;; A debugger pauses only between the interpreter's steps, which compiled code
;; never takes, so a breakpoint inside a compiled procedure cannot fire. So
;; while a program is being debugged, the closures run compiled that the
;; debugger chooses run as themselves again -- those holding a breakpoint, or
;; every one while it steps: the declining-to-optimize every toolchain offers
;; beside its debug info. (One reached beneath compiled code still pauses: its
;; run moves the compiled frames to the heap and takes the step again where it
;; can wait, `Interpreter.step`.) Each is recorded, with the compiled procedure
;; it runs as, by the registry current when it was compiled, so that a
;; registry made for a while takes its records with it.
;;
;; Which programs are being debugged, in which registry and which of their
;; closures run as themselves, the host keeps in an `eq` store from each
;; program's global environment to a pair of the registry and the choice
;; (`make-debugged-programs`), one for the process.

;; /**
;;  * An empty store of the programs being debugged.
;;  * @returns {object}
;;  */
(define (make-debugged-programs)
  (%make-hash-store 'eq))

;; /**
;;  * Records closures just made to run compiled, so a debugger can run them as
;;  * themselves: each with the compiled procedure it runs as, and the
;;  * environment it was compiled in -- a program's, whose closures follow that
;;  * program's debugging alone, or a library's, shared by the registry's
;;  * programs.
;;  *
;;  * Compiled in the global environment of a program being debugged, those
;;  * its debugger chooses run as themselves at once. A library loaded while a
;;  * program is being debugged is switched when the program next runs
;;  * (`interpret-compiled-over!`).
;;  * A tool's own closures -- the compiler's, in its own registry -- are
;;  * recorded too, and never switched: switching reaches only the records of
;;  * the registry it is made in.
;;  * @param {library-registry} registry - The registry current now.
;;  * @param {object} debugged - The programs being debugged.
;;  * @param {list} compiled - Each `(closure . procedure)`: a closure run
;;  *   compiled, and the procedure it runs as.
;;  * @param {object} env - The environment they were compiled in.
;;  */
(define (record-compiled-over! registry debugged compiled env)
  (let ((records (registry-compiled-over registry))
        (debugging (%hash-store-ref debugged env #f)))
    (for-each (lambda (pair) (%hash-store-set! records (car pair) (cons (cdr pair) env))) compiled)
    (if debugging
        (for-each (lambda (pair)
                    (if (chosen? (cdr debugging) (car pair)) (%run-interpreted! (car pair))))
                  compiled))))

;; /**
;;  * Whether a debugger's choice includes a closure: #t is every one, a
;;  * procedure says which.
;;  */
(define (chosen? which closure)
  (or (eq? which #t) (and (procedure? which) (which closure) #t)))

;; /**
;;  * Whether a procedure is a closure run compiled, which can run as itself.
;;  * @param {library-registry} registry - The registry current now.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled-over? registry procedure)
  (%hash-store-contains? (registry-compiled-over registry) procedure))

;; /**
;;  * Runs the recorded closures of one program, and of the registry's
;;  * libraries, that its debugger chooses as themselves while the program is
;;  * debugged, the others compiled, and every one compiled again once it is
;;  * not. The libraries' are the registry's other programs' too, so they are
;;  * compiled again only once none of those is being debugged; other programs'
;;  * own are left as they are.
;;  *
;;  * Switching again is harmless, and catches what was compiled since.
;;  * Whatever holds a closure, a name or a program's data, holds the object
;;  * switched, and compiled code still running calls the closure from its
;;  * next call on.
;;  * @param {library-registry} registry - The registry current now, which a
;;  *   program starting to be debugged is debugged in.
;;  * @param {object} debugged - The programs being debugged.
;;  * @param {boolean|procedure} which - Which closures run as themselves: #t
;;  *   every one, #f none -- the program is not being debugged -- or a
;;  *   procedure saying of a closure whether it does.
;;  * @param {object} program - The program's global environment.
;;  */
(define (interpret-compiled-over! registry debugged which program)
  (let* ((debugging? (not (eq? which #f)))
         (before (%hash-store-ref debugged program #f))
         (in (if debugging? registry (and before (car before)))))
    (when in
      (if debugging?
          (%hash-store-set! debugged program (cons in which))
          (%hash-store-delete! debugged program))
      (let ((libraries-too? (or debugging? (not (memq in (map car (%hash-store-values debugged)))))))
        (for-each (lambda (closure record)
                    (let ((env (cdr record)))
                      ;; A library's environment has a scope of its own; a
                      ;; program's is the top level's, 0.
                      (when (or (eq? env program)
                                (and libraries-too? (not (zero? (%environment-scope env)))))
                        (if (and debugging? (chosen? which closure))
                            (%run-interpreted! closure)
                            (%run-compiled! closure (car record))))))
                  (%hash-store-keys (registry-compiled-over in))
                  (%hash-store-values (registry-compiled-over in)))))))

;; /**
;;  * Runs one closure as itself again, for good: for a procedure whose saved
;;  * frames continuations keep re-entering, which costs more compiled than
;;  * interpreted (`note-resume` in `src/compiler/tier.scm`). It is then no
;;  * longer recorded, so a debugger's switching leaves it as it is.
;;  *
;;  * Found by its compiled procedure's resumable form, which is what the frames
;;  * resumed carry; a procedure nested in a compiled one, which has no closure
;;  * of its own, is left as it is.
;;  * @param {library-registry} registry - The registry current now.
;;  * @param {object} debugged - The programs being debugged.
;;  * @param {procedure} twin - The compiled procedure's resumable form.
;;  * @returns {boolean} Whether a closure was switched back.
;;  */
(define (switch-back-to-closure! registry debugged twin)
  (let* ((records (registry-compiled-over registry))
         (closure (let loop ((closures (%hash-store-keys records)))
                    (cond ((null? closures) #f)
                          ((eq? (%resumable-form (car (%hash-store-ref records (car closures) #f))) twin)
                           (car closures))
                          (else (loop (cdr closures)))))))
    (and closure
         (begin
           (%hash-store-delete! records closure)
           (%run-interpreted! closure)
           #t))))

;; ---------------------------------------------------------------------------
;; The files a load would read
;; ---------------------------------------------------------------------------
;;
;; A file resolver that has to fetch files answers with promises, and a load
;; cannot wait for one. So the host fetches first every file a load will read:
;; it asks which files a library needs that it does not have, from the files
;; it has, fetches those, and asks again, until it lacks none -- each round
;; learning what the files just fetched import, include and declare -- and
;; then loads from them.

;; /**
;;  * What a walk over a library's files has found so far.
;;  * @property {list} paths - The paths wanted, the latest first.
;;  * @property {list} seen - The keys of the libraries walked.
;;  */
(define-record-type wants
  (make-wants paths seen)
  wants?
  (paths wants-paths)
  (seen wants-seen))

;; /**
;;  * The files loading a library would read that a loader cannot read now,
;;  * as far as the files it can read tell: its own file, and, from those at
;;  * hand, the files of the libraries it imports and the files it includes,
;;  * in turn. A library loaded already reads none.
;;  * @param {loader} loader - A loader whose `resolve` gives the text of a
;;  *   file at hand and #f for any other.
;;  * @param {list} name - The library's name.
;;  * @returns {list} The paths wanted, each a list of strings, each once.
;;  */
(define (files-wanted loader name)
  (reverse (wants-paths (library-wants loader name (make-wants '() '())))))

;; /**
;;  * The files defining a library from a `define-library` form would read
;;  * that a loader cannot read now, as `files-wanted` finds them.
;;  * @param {loader} loader - As for `files-wanted`.
;;  * @param {list} form - The form.
;;  * @returns {list} The paths wanted.
;;  */
(define (definition-files-wanted loader form)
  (let ((definition (parse-define-library form (feature-test loader))))
    (reverse (wants-paths (definition-wants loader (library-definition-name definition) definition
                                            (make-wants '() '()))))))

;; /**
;;  * What a library's files want, added to what was found so far.
;;  * @param {loader} loader - The loader.
;;  * @param {list} name - The library's name.
;;  * @param {wants} found - What was found so far.
;;  * @returns {wants}
;;  */
(define (library-wants loader name found)
  (let ((key (library-key name))
        (path (name-strings name)))
    (cond ((or (member key (wants-seen found)) (registered-library (loader-registry loader) key)) found)
          ((not (at-hand? loader path)) (want path found))
          (else
           (let ((forms (read-library-file loader path (library-path name) #f))
                 (found (make-wants (wants-paths found) (cons key (wants-seen found)))))
             ;; A file that is not a library's says so when it is loaded.
             (if (and (pair? forms) (pair? (car forms)) (eq? (caar forms) 'define-library))
                 (definition-wants loader name (parse-define-library (car forms) (feature-test loader)) found)
                 found))))))

;; /**
;;  * What a definition wants: what the libraries it imports want, the files
;;  * it includes, and, of each file of library declarations, the file, or
;;  * what its declarations want.
;;  * @param {loader} loader - The loader.
;;  * @param {list} name - The library's name, which its files are found by.
;;  * @param {library-definition} definition - The definition.
;;  * @param {wants} found - What was found so far.
;;  * @returns {wants}
;;  */
(define (definition-wants loader name definition found)
  (define (file-wants file found)
    (let ((path (include-path name file)))
      (if (at-hand? loader path) found (want path found))))
  (define (declarations-want file found)
    (let ((path (include-path name file)))
      (if (at-hand? loader path)
          (definition-wants loader name
                            (parse-declarations #f (read-library-file loader path file #f) (feature-test loader))
                            found)
          (want path found))))
  (let* ((found (fold (lambda (spec found) (library-wants loader (import-set-library-name spec) found))
                      found
                      (library-definition-imports definition)))
         (found (fold file-wants found
                      (append (library-definition-includes definition)
                              (library-definition-includes-ci definition)))))
    (fold declarations-want found (library-definition-declaration-files definition))))

;; /**
;;  * Whether a loader can read a file now.
;;  * @param {loader} loader - The loader.
;;  * @param {list} path - The path.
;;  * @returns {boolean}
;;  */
(define (at-hand? loader path)
  (string? ((loader-resolve loader) path)))

;; /**
;;  * What was found, with a path wanted, if it was not already.
;;  * @param {list} path - The path.
;;  * @param {wants} found - What was found so far.
;;  * @returns {wants}
;;  */
(define (want path found)
  (if (member path (wants-paths found))
      found
      (make-wants (cons path (wants-paths found)) (wants-seen found))))

;; /**
;;  * A `define-library` form's parts, for the host's tools that read which
;;  * files a library is made of: its name, its exports as `(internal .
;;  * external)`, the names of the libraries it imports, its body's forms, and
;;  * the files of its three kinds of `include`, its `cond-expand` declarations
;;  * decided in a registry.
;;  * @param {library-registry} registry - The registry.
;;  * @param {list} form - The form.
;;  * @returns {list}
;;  */
(define (define-library-parts registry form)
  (let ((definition (parse-define-library form (feature-test (registry-loader registry #f #f)))))
    (list (library-definition-name definition)
          (library-definition-exports definition)
          (map import-set-library-name (library-definition-imports definition))
          (library-definition-body definition)
          (library-definition-includes definition)
          (library-definition-includes-ci definition)
          (library-definition-declaration-files definition))))
