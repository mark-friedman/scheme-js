;; syntax-rules
;;
;; The transformer of a macro `syntax-rules` defines (R7RS 4.3.2): a use is
;; matched against each clause's pattern in turn, and the first that matches
;; gives the template the use is transcribed into.
;;
;; Hygiene is by marks, as Dybvig's: each expansion makes a fresh scope, and
;; everything the template introduces is marked with it, and so kept apart
;; from the user's identifiers of the same name -- the binding a template
;; introduces captures nothing of the user's, and the user's captures nothing
;; of the template's. What a pattern variable matched is substituted as it was
;; written, its pairs the user's own, which keep the spans the reader gave
;; them. A macro a library
;; defined also marks what its template introduces with the library's scope,
;; which is how a reference it makes finds the library's binding of its name
;; where it is used (`library-binding-env` in expander.scm). A template's free
;; identifier that names a local of where the macro was defined is the local,
;; by its renamed name.

;; ---------------------------------------------------------------------------
;; A macro, and one expansion of it
;; ---------------------------------------------------------------------------

;; /**
;;  * One expansion of a `syntax-rules` macro: what its clauses are matched and
;;  * transcribed with.
;;  * @property {list} literals - The macro's literals.
;;  * @property {symbol} ellipsis - Its ellipsis' name.
;;  * @property {procedure} use-env - Where it is used: a procedure of an
;;  *   identifier giving the local that binds it there, or #f.
;;  * @property {number} scope - The expansion's scope.
;;  * @property {syntactic-env} definition-env - Where the macro was defined.
;;  * @property {number|boolean} library-scope - The scope of the library that
;;  *   defined it, or #f.
;;  * @property {number} defining-scope - Where it was defined, where its
;;  *   literals name the keywords they do.
;;  */
(define-record-type expansion
  (make-expansion literals ellipsis use-env scope definition-env library-scope defining-scope)
  expansion?
  (literals expansion-literals)
  (ellipsis expansion-ellipsis)
  (use-env expansion-use-env)
  (scope expansion-scope)
  (definition-env expansion-definition-env)
  (library-scope expansion-library-scope)
  (defining-scope expansion-defining-scope))

;; /**
;;  * The matches of a pattern followed by an ellipsis, one for each item it
;;  * matched, in order.
;;  * @property {list} items - What a pattern variable matched each time.
;;  */
(define-record-type sequence
  (make-sequence items)
  sequence?
  (items sequence-items))

;; /**
;;  * The transformer of a `syntax-rules` macro: a procedure of a use and where
;;  * it is used, that gives what the use expands into.
;;  * @param {list} literals - The literals.
;;  * @param {list} clauses - The clauses, each `(pattern . template)`.
;;  * @param {number} defining-scope - The scope of the library or program the
;;  *   macro is defined in.
;;  * @param {symbol} ellipsis - The ellipsis' name.
;;  * @param {syntactic-env} definition-env - Where the macro is defined.
;;  * @returns {procedure}
;;  */
(define (syntax-rules-transformer literals clauses defining-scope ellipsis definition-env)
  (let* ((library-env (%library-environment defining-scope))
         (library-scope (and library-env defining-scope))
         ;; The libraries the expansions name by scope: the macro's own, and
         ;; those whose macros' expansions defined it, whose scopes its
         ;; identifiers carry. The macro may outlive the registry they were
         ;; loaded in, which takes its libraries' scopes with it, so it
         ;; holds them itself.
         (libraries (libraries-named-in (cons literals clauses)
                                        (if library-env (list library-env) '()))))
    (lambda (form use-env)
      (for-each %reregister-library! libraries)
      (let ((x (make-expansion literals ellipsis use-env (%fresh-scope) definition-env
                               library-scope defining-scope)))
        (let try ((clauses clauses))
          (if (null? clauses)
              (no-matching-clause form)
              (let* ((pattern (caar clauses))
                     ;; A pattern's first item, the macro's keyword, is ignored.
                     (bindings (if (and (pair? pattern) (pair? form))
                                   (match (cdr pattern) (cdr form) x)
                                   (match pattern form x))))
                (if bindings
                    (transcribe (cdar clauses) bindings x)
                    (try (cdr clauses))))))))))

;; /**
;;  * Raises the error of a use no clause matches.
;;  * @param {*} form - The use.
;;  */
(define (no-matching-clause form)
  (let ((name (if (and (pair? form) (identifier? (car form)))
                  (symbol->string (identifier-name (car form)))
                  "unknown")))
    (raise-syntax-error (string-append "No matching clause for macro '" name "'") form name)))

;; /**
;;  * Adds to `libraries` the environment of each library whose scope an
;;  * identifier in a datum carries.
;;  * @param {*} datum - Patterns and templates.
;;  * @param {list} libraries - The environments found so far.
;;  * @returns {list}
;;  */
(define (libraries-named-in datum libraries)
  (let ((seen (%make-hash-store 'eq)))
    (let walk ((x datum) (libraries libraries))
      (cond ((syntax-object? x)
             (let each ((scopes (%identifier-scopes x)) (libraries libraries))
               (if (null? scopes)
                   libraries
                   (let ((env (%library-environment (car scopes))))
                     (each (cdr scopes)
                             (if (and env (not (memq env libraries))) (cons env libraries) libraries))))))
            ((not (or (pair? x) (vector? x))) libraries)
            ((%hash-store-contains? seen x) libraries)
            (else
             (%hash-store-set! seen x #t)
             (if (pair? x)
                 (walk (cdr x) (walk (car x) libraries))
                 (let elements ((i 0) (libraries libraries))
                   (if (< i (vector-length x))
                       (elements (+ i 1) (walk (vector-ref x i) libraries))
                       libraries))))))))

;; /**
;;  * Whether an identifier is one of a macro's literals.
;;  * @param {identifier} id - The identifier.
;;  * @param {list} literals - The literals.
;;  * @returns {boolean}
;;  */
(define (literal? id literals)
  (and (pair? literals)
       (or (bound-identifier=? (car literals) id) (literal? id (cdr literals)))))

;; /**
;;  * Whether an identifier is bound locally where a macro is used.
;;  * @param {procedure} use-env - Where: a procedure of an identifier giving
;;  *   the local that binds it there, or #f.
;;  * @param {identifier} id - The identifier.
;;  * @returns {boolean}
;;  */
(define (bound-at-use? use-env id)
  (and (use-env id) #t))

;; ---------------------------------------------------------------------------
;; Matching
;; ---------------------------------------------------------------------------

;; /**
;;  * Matches a use's part against a pattern's.
;;  * @param {*} pattern - The pattern.
;;  * @param {*} input - The use's part.
;;  * @param {expansion} x - The expansion.
;;  * @returns {list|boolean} What each pattern variable matched, as
;;  *   `(variable . match)`, or #f if it does not match.
;;  */
(define (match pattern input x)
  (cond ((identifier? pattern) (match-identifier pattern input x))
        ((null? pattern) (and (null? input) '()))
        ((%atomic-datum? pattern) (and (eq? input pattern) '()))
        ((vector? pattern)
         (and (vector? input) (match (vector->list pattern) (vector->list input) x)))
        ((pair? pattern) (match-list pattern input x))
        (else #f)))

;; /**
;;  * Matches against an identifier of a pattern. A literal matches an
;;  * identifier naming what it does -- the same keyword, though imported
;;  * under different names where each is -- and not bound locally where the
;;  * macro is used, where it would mean that binding instead. `_` matches
;;  * anything; any other identifier is a pattern variable, and binds what it
;;  * matches.
;;  */
(define (match-identifier pattern input x)
  (cond ((literal? pattern (expansion-literals x))
         (and (identifier? input)
              (eq? (keyword-name input #f) (keyword-name pattern (expansion-defining-scope x)))
              (not (bound-at-use? (expansion-use-env x) input))
              '()))
        ((eq? (identifier-name pattern) '_) '())
        (else (list (cons pattern input)))))

;; /**
;;  * Whether the item after a pattern's or template's first is its ellipsis.
;;  * @param {pair} pattern - The pattern.
;;  * @param {symbol} ellipsis - The ellipsis' name.
;;  * @returns {boolean}
;;  */
(define (ellipsis-follows? pattern ellipsis)
  (and (pair? (cdr pattern)) (named? (cadr pattern) ellipsis)))

;; /**
;;  * How many pairs a list or improper list has.
;;  */
(define (pair-count x)
  (if (pair? x) (+ 1 (pair-count (cdr x))) 0))

;; /**
;;  * A list past its first `n` pairs.
;;  */
(define (drop-pairs x n)
  (if (= n 0) x (drop-pairs (cdr x) (- n 1))))

;; /**
;;  * Matches against a list pattern: item by item, a `p <ellipsis>` taking as
;;  * many items as leaves enough for what follows it, and a dotted tail
;;  * matching what is left.
;;  */
(define (match-list pattern input x)
  (let loop ((pattern pattern) (input input) (bindings '()))
    (cond ((and (pair? pattern) (ellipsis-follows? pattern (expansion-ellipsis x)))
           (let* ((item (car pattern))
                  (tail (cddr pattern))
                  (count (- (pair-count input) (pair-count tail))))
             (and (>= count 0)
                  (let ((matches (match-each item input count x)))
                    (and matches
                         (loop tail (drop-pairs input count)
                               (append (sequences (pattern-variables item x) matches) bindings)))))))
          ((pair? pattern)
           (and (pair? input)
                (let ((matched (match (car pattern) (car input) x)))
                  (and matched (loop (cdr pattern) (cdr input) (merge matched bindings))))))
          ((null? pattern) (and (null? input) bindings))
          (else
           (let ((matched (match pattern input x)))
             (and matched (merge matched bindings)))))))

;; /**
;;  * Matches each of a list's first `count` items against a pattern.
;;  * @returns {list|boolean} Each item's bindings, in order, or #f.
;;  */
(define (match-each item input count x)
  (let loop ((input input) (count count) (matches '()))
    (if (= count 0)
        (reverse matches)
        (let ((matched (match item (car input) x)))
          (and matched (loop (cdr input) (- count 1) (cons matched matches)))))))

;; /**
;;  * The bindings of a pattern under an ellipsis: each of its variables bound
;;  * to the sequence of what it matched in each match.
;;  * @param {list} variables - The pattern's variables.
;;  * @param {list} matches - Each match's bindings, in order.
;;  * @returns {list}
;;  */
(define (sequences variables matches)
  (map (lambda (variable)
         (cons variable (make-sequence (map (lambda (matched) (cdr (assq variable matched))) matches))))
       variables))

;; /**
;;  * Adds a part's bindings to a pattern's, a variable bound twice being an
;;  * error.
;;  */
(define (merge matched bindings)
  (if (null? matched)
      bindings
      (let ((variable (caar matched)))
        (if (assq variable bindings)
            (raise-syntax-error (string-append "Duplicate pattern variable '"
                                               (symbol->string (identifier-name variable)) "'")
                                '() 'syntax-rules)
            (merge (cdr matched) (cons (car matched) bindings))))))

;; /**
;;  * The pattern variables of a pattern: its identifiers, but its ellipsis,
;;  * `_` and its literals.
;;  * @param {*} pattern - The pattern.
;;  * @param {expansion} x - The expansion.
;;  * @returns {list}
;;  */
(define (pattern-variables pattern x)
  (let walk ((p pattern) (variables '()))
    (cond ((identifier? p)
           (let ((name (identifier-name p)))
             (if (or (eq? name '_) (eq? name (expansion-ellipsis x))
                     (literal? p (expansion-literals x)) (memq p variables))
                 variables
                 (cons p variables))))
          ((pair? p) (walk (cdr p) (walk (car p) variables)))
          ((vector? p) (walk (vector->list p) variables))
          (else variables))))

;; ---------------------------------------------------------------------------
;; Transcribing
;; ---------------------------------------------------------------------------

;; /**
;;  * What a pattern variable matched, as data: a sequence, used where the
;;  * template does not repeat it, is a vector of its items.
;;  */
(define (matched-datum value)
  (if (sequence? value)
      (list->vector (map matched-datum (sequence-items value)))
      value))

;; /**
;;  * A template, transcribed with what its pattern variables matched.
;;  * @param {*} template - The template.
;;  * @param {list} bindings - The bindings.
;;  * @param {expansion} x - The expansion.
;;  * @returns {*}
;;  */
(define (transcribe template bindings x)
  (cond ((null? template) '())
        ((identifier? template) (transcribe-identifier template bindings x))
        ((vector? template) (list->vector (transcribe (vector->list template) bindings x)))
        ((pair? template) (transcribe-pair template bindings x))
        (else template)))

;; /**
;;  * The names of special forms, which a template's identifier never takes for
;;  * a local where the macro was defined.
;;  */
(define special-form-names
  '(if let letrec lambda set! define begin quote quasiquote unquote unquote-splicing
    define-syntax let-syntax letrec-syntax define-macro call/cc call-with-current-continuation
    import cond-expand))

;; /**
;;  * A template's identifier, transcribed: what a pattern variable matched,
;;  * as it was written; a local of where the macro was defined, by its renamed
;;  * name; or else the identifier, marked as introduced.
;;  */
(define (transcribe-identifier id bindings x)
  (let ((bound (assq id bindings)))
    (if bound
        (matched-datum (cdr bound))
        (let ((local (local-where-defined id x)))
          (cond ((not local) (mark-introduced id x))
                ((syntax-object? id)
                 (%identifier-flip-scope (%make-identifier local (%identifier-scopes id)) (expansion-scope x)))
                (else (%make-identifier local (list (expansion-scope x)))))))))

;; /**
;;  * The renamed name of the local a template's identifier names where the
;;  * macro was defined, or #f: not a special form's name, nor a macro's for
;;  * the process.
;;  */
(define (local-where-defined id x)
  (let ((name (identifier-name id)))
    (and (expansion-definition-env x)
         (not (memq name special-form-names))
         (not (%process-macro name))
         (env-lookup (expansion-definition-env x) id))))

;; /**
;;  * Marks an identifier a template introduces: with the expansion's scope,
;;  * and, if a library defined the macro, with the library's, unless it
;;  * carries a library's already -- it was written in that library, by a
;;  * macro of its that wrote this macro's template, and refers there.
;;  */
(define (mark-introduced id x)
  (let* ((library-scope (expansion-library-scope x))
         (in-library? (and library-scope (not (library-scope-of id)))))
    (if (syntax-object? id)
        (%identifier-flip-scope (if in-library? (%identifier-add-scope id library-scope) id)
                                (expansion-scope x))
        (%make-identifier id (if in-library?
                                 (list library-scope (expansion-scope x))
                                 (list (expansion-scope x)))))))

;; /**
;;  * A list template: `(<ellipsis> template)`, the template transcribed as
;;  * it is written; `item <ellipsis> . rest`, the item once for each match of
;;  * its pattern variables; or a pair of transcriptions.
;;  */
(define (transcribe-pair template bindings x)
  (let ((ellipsis (expansion-ellipsis x)))
    (cond ((and (named? (car template) ellipsis) (pair? (cdr template)) (null? (cddr template)))
           (transcribe-escaped (cadr template) bindings x))
          ((and (ellipsis-follows? template ellipsis)
                (not (literal? (cadr template) (expansion-literals x))))
           (transcribe-repeated (car template) (cddr template) bindings x))
          (else
           (cons (transcribe (car template) bindings x) (transcribe (cdr template) bindings x))))))

;; /**
;;  * The pattern variables a template uses that are bound, in the order they
;;  * appear.
;;  */
(define (template-variables template bindings)
  (reverse
   (let walk ((t template) (variables '()))
     (cond ((identifier? t)
            (if (and (assq t bindings) (not (memq t variables))) (cons t variables) variables))
           ((pair? t) (walk (cdr t) (walk (car t) variables)))
           ((vector? t) (walk (vector->list t) variables))
           (else variables)))))

;; /**
;;  * `item <ellipsis> . rest`: the item transcribed once for each match of the
;;  * sequences it uses, which must all be as long, before `rest`.
;;  */
(define (transcribe-repeated item rest bindings x)
  (let ((repeated (let keep ((variables (template-variables item bindings)))
                    (cond ((null? variables) '())
                          ((sequence? (cdr (assq (car variables) bindings)))
                           (cons (car variables) (keep (cdr variables))))
                          (else (keep (cdr variables)))))))
    (if (null? repeated)
        (raise-syntax-error "Ellipsis template must contain at least one pattern variable bound to a list"
                            '() 'syntax-rules))
    (let ((columns (map (lambda (variable) (sequence-items (cdr (assq variable bindings)))) repeated)))
      (if (not (same-lengths? columns))
          (raise-syntax-error "Ellipsis expansion: variable lengths do not match" '() 'syntax-rules))
      (let ((tail (transcribe rest bindings x)))
        (let loop ((columns columns) (items '()))
          (if (null? (car columns))
              (append (reverse items) tail)
              (loop (map cdr columns)
                    (cons (transcribe item
                                      (append (map (lambda (variable column) (cons variable (car column)))
                                                   repeated columns)
                                              bindings)
                                      x)
                          items))))))))

;; /**
;;  * Whether lists are all as long as each other.
;;  */
(define (same-lengths? lists)
  (let ((n (length (car lists))))
    (let loop ((lists (cdr lists)))
      (or (null? lists)
          (and (= (length (car lists)) n) (loop (cdr lists)))))))

;; /**
;;  * `(<ellipsis> template)`'s template, transcribed as written: an ellipsis
;;  * in it is itself, and a pattern variable what it matched, marked with the
;;  * expansion's scope as the identifiers the template introduces are.
;;  */
(define (transcribe-escaped template bindings x)
  (cond ((identifier? template)
         (let ((bound (assq template bindings)))
           (cond ((eq? (identifier-name template) '...) template)
                 (bound (%flip-scope (matched-datum (cdr bound)) (expansion-scope x)))
                 (else (mark-introduced template x)))))
        ((vector? template)
         (list->vector (map (lambda (item) (transcribe-escaped item bindings x)) (vector->list template))))
        ((pair? template)
         (cons (transcribe-escaped (car template) bindings x) (transcribe-escaped (cdr template) bindings x)))
        (else template)))
