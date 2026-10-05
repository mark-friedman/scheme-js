;; er-macro-transformer
;;
;; The transformer of a macro defined by explicit renaming (Clinger, "Hygienic
;; Macros Through Explicit Renaming", 1991): a procedure of the use, `rename`
;; and `compare`, which returns what the use expands into. Nothing in what it
;; returns is touched: a symbol it made up is the user's, found where the
;; macro is used, as a `define-macro`'s are; one it renamed is the macro's.
;;
;; To rename an identifier is to do what a `syntax-rules` template does to an
;; identifier it introduces (`transcribe-identifier` in syntax_rules.scm): mark
;; it with the expansion's scope, and with the library's, if a library defined
;; the macro, so that it binds nothing of the user's and refers to the binding
;; visible where the macro was defined; or, if it names a local there, make it
;; that local. Two renamings of one name in an expansion are the same
;; identifier. `compare` is `free-identifier=?` where the macro is used: two
;; identifiers that would mean the same local, keyword or global there.

;; /**
;;  * The transformer of an explicit-renaming macro: a procedure of a use and
;;  * where it is used, that gives what the use expands into.
;;  * @param {procedure} procedure - The macro's procedure, of the use, `rename`
;;  *   and `compare`.
;;  * @param {number} defining-scope - The scope of the library or program the
;;  *   macro is defined in.
;;  * @param {syntactic-env} definition-env - Where the macro is defined.
;;  * @returns {procedure}
;;  */
(define (er-transformer procedure defining-scope definition-env)
  (let* ((library-env (%library-environment defining-scope))
         (library-scope (and library-env defining-scope)))
    (lambda (form use-env)
      ;; The macro may outlive the registry its library was loaded in, which
      ;; takes the library's scope with it.
      (if library-env (%reregister-library! library-env))
      (let ((x (make-expansion '() '... use-env (%fresh-scope) definition-env
                               library-scope defining-scope)))
        (procedure form
                   (lambda (id) (renamed id x))
                   (lambda (a b) (same-binding? a b x)))))))

;; /**
;;  * An identifier, renamed in an expansion.
;;  * @param {identifier} id - The identifier.
;;  * @param {expansion} x - The expansion.
;;  * @returns {identifier}
;;  */
(define (renamed id x)
  (if (identifier? id)
      (transcribe-identifier id '() x)
      (raise-syntax-error "rename: not an identifier" id 'er-macro-transformer)))

;; /**
;;  * Whether two identifiers mean the same where a macro is used: both the
;;  * same local there, or neither a local and both naming the same keyword or
;;  * global. A local is known by its renamed name, unique to it, which is the
;;  * name a renamed identifier that names a local where the macro was defined
;;  * has.
;;  * @param {*} a - One.
;;  * @param {*} b - The other.
;;  * @param {expansion} x - The expansion.
;;  * @returns {boolean}
;;  */
(define (same-binding? a b x)
  (and (identifier? a) (identifier? b)
       (eq? (binding-of a x) (binding-of b x))))

;; /**
;;  * What an identifier means where a macro is used, by name: the renamed
;;  * name of the local it is there, or else the keyword or global it names --
;;  * where the macro was defined, for one the expansion introduced.
;;  * @param {identifier} id - The identifier.
;;  * @param {expansion} x - The expansion.
;;  * @returns {symbol}
;;  */
(define (binding-of id x)
  (or ((expansion-use-env x) id)
      (keyword-name id (and (syntax-object? id)
                            (memv (expansion-scope x) (%identifier-scopes id))
                            (expansion-defining-scope x)))))
