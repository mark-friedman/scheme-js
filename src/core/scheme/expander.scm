;; The expander
;;
;; A form into the core form it means, where it is: a variable into the name
;; it is bound under, a special form into its core form, a macro's use into
;; what its transformer makes of it, expanded in turn, and anything else into
;; an application. expander.sld lists the core forms.
;;
;; Locals are renamed as they are bound, each to a name no other local has --
;; the name written and a number, `x_$12` -- so that what reaches the
;; evaluator and the compiler binds every name once. Where a local is looked
;; up, an identifier is compared as `bound-identifier=?` compares: by name and
;; by the scopes a macro's expansion marks what it introduces with
;; (syntax_rules.scm), so that an identifier a macro introduced and one the
;; user wrote are different variables. A macro's use is found first among the
;; macros defined around it, then among the syntactic keywords bound where it
;; is used -- in the library whose macro introduced it, or the library or
;; program being expanded -- and last, where that sees everything, among the
;; macros defined by name for the process.

;; ---------------------------------------------------------------------------
;; Identifiers
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether a datum is an identifier: a symbol, or a syntax object, the
;;  * identifier a macro's expansion makes of one, marked with scopes.
;;  * @param {*} x - The datum.
;;  * @returns {boolean}
;;  */
(define (identifier? x)
  (or (symbol? x) (syntax-object? x)))

;; /**
;;  * An identifier's name.
;;  * @param {identifier} id - The identifier.
;;  * @returns {symbol}
;;  */
(define (identifier-name id)
  (if (symbol? id) id (%identifier-name id)))

;; /**
;;  * Whether a datum is an identifier of a name, whatever its scopes.
;;  * @param {*} x - The datum.
;;  * @param {symbol} name - The name.
;;  * @returns {boolean}
;;  */
(define (named? x name)
  (and (identifier? x) (eq? (identifier-name x) name)))

;; /**
;;  * Whether two identifiers would bind the same, bound at the same place: the
;;  * same name and the same scopes, a symbol having none. Identifiers are
;;  * interned, so two that are the same are one object, nearly always.
;;  * @param {identifier} a - One.
;;  * @param {identifier} b - The other.
;;  * @returns {boolean}
;;  */
(define (bound-identifier=? a b)
  (or (eq? a b)
      (and (not (and (symbol? a) (symbol? b)))
           (%bound-identifier=? a b))))

;; /**
;;  * The scope of the library whose macro introduced an identifier, or #f.
;;  * @param {identifier} id - The identifier.
;;  * @returns {number|boolean}
;;  */
(define (library-scope-of id)
  (and (syntax-object? id) (%identifier-library-scope id)))

;; /**
;;  * A name for a local that no other has: the name it was written with and a
;;  * number.
;;  * @param {symbol} name - The name written.
;;  * @returns {symbol}
;;  */
(define (rename name)
  (string->symbol (string-append (symbol->string name) "_$" (number->string (%fresh-unique-id)))))

;; ---------------------------------------------------------------------------
;; Syntactic environments
;; ---------------------------------------------------------------------------

;; /**
;;  * Where a form is expanded: frames of the locals bound around it, the
;;  * innermost first, each identifier mapped to the name it was renamed to;
;;  * whether it is a program's or a library's top level, where a definition
;;  * shadows a macro of its name for what follows; and the macros defined by
;;  * the innermost body or `let-syntax` around it, which lead to those of the
;;  * ones around that.
;;  * @property {syntactic-env|boolean} parent - The frame around this one, or #f.
;;  * @property {list} bindings - `(identifier . name)` pairs, looked through in order.
;;  * @property {boolean} top-level? - Whether this is a top level.
;;  * @property {macro-table|boolean} macros - The innermost macros, or #f.
;;  * @property {object|boolean} runtime - In the outermost frame, the
;;  *   environment the forms expanded in it will run in, where whoever
;;  *   expands them says (`expand`); otherwise #f.
;;  */
(define-record-type syntactic-env
  (make-syntactic-env parent bindings top-level? macros runtime)
  syntactic-env?
  (parent env-parent)
  (bindings env-bindings)
  (top-level? env-top-level?)
  (macros env-macros)
  (runtime env-runtime))

;; /**
;;  * The macros a body or a `let-syntax` defines, by name, the latest first.
;;  * @property {list} entries - `(name . transformer)` pairs.
;;  * @property {macro-table|boolean} parent - The macros of the body or
;;  *   `let-syntax` around it, or #f.
;;  */
(define-record-type macro-table
  (make-macro-table entries parent)
  macro-table?
  (entries macro-table-entries set-macro-table-entries!)
  (parent macro-table-parent))

;; /**
;;  * A program's or a library's top level, where nothing is bound locally.
;;  * @param {object|boolean} runtime - The environment the forms expanded
;;  *   there will run in, or #f if that is not known.
;;  * @returns {syntactic-env}
;;  */
(define (top-level-env runtime)
  (make-syntactic-env #f '() #t #f runtime))

;; /**
;;  * The environment the forms expanded where `env` is will run in, as the
;;  * outermost frame says, or #f.
;;  * @param {syntactic-env|boolean} env - Where; #f for nowhere in particular.
;;  * @returns {object|boolean}
;;  */
(define (runtime-environment env)
  (cond ((not env) #f)
        ((env-parent env) (runtime-environment (env-parent env)))
        (else (env-runtime env))))

;; /**
;;  * A frame inside one, binding nothing yet: a procedure's, which is never a
;;  * top level.
;;  * @param {syntactic-env} env - The frame around it.
;;  * @returns {syntactic-env}
;;  */
(define (env-child env)
  (make-syntactic-env env '() #f (env-macros env) #f))

;; /**
;;  * A frame binding an identifier, inside one.
;;  * @param {syntactic-env} env - The frame around it.
;;  * @param {identifier} id - The identifier.
;;  * @param {symbol} name - The name it is bound under.
;;  * @returns {syntactic-env}
;;  */
(define (env-extend env id name)
  (make-syntactic-env env (list (cons id name)) #f (env-macros env) #f))

;; /**
;;  * Frames binding identifiers, one each, in order, inside one.
;;  * @param {syntactic-env} env - The frame around them.
;;  * @param {list} ids - The identifiers.
;;  * @param {list} names - The names each is bound under.
;;  * @returns {syntactic-env}
;;  */
(define (env-extend-all env ids names)
  (if (null? ids)
      env
      (env-extend-all (env-extend env (car ids) (car names)) (cdr ids) (cdr names))))

;; /**
;;  * A frame where macros are defined -- a body's, a `let-syntax`'s -- which
;;  * is a top level if the frame around it is.
;;  * @param {syntactic-env} env - The frame around it.
;;  * @returns {syntactic-env} The frame, its macros a table of none yet.
;;  */
(define (env-with-macros env)
  (make-syntactic-env env '() (env-top-level? env) (make-macro-table '() (env-macros env)) #f))

;; /**
;;  * The name an identifier is bound under locally, where `env` is, or #f.
;;  * Most identifiers are symbols, and a symbol is `bound-identifier=?` only
;;  * to itself -- a syntax object with no scopes, which would be too, is never
;;  * made -- so a symbol is looked for by `eq?`.
;;  * @param {syntactic-env|boolean} env - Where it is used.
;;  * @param {identifier} id - The identifier.
;;  * @returns {symbol|boolean}
;;  */
(define (env-lookup env id)
  (if (symbol? id)
      (let frames ((env env))
        (and env
             (let ((binding (assq id (env-bindings env))))
               (if binding (cdr binding) (frames (env-parent env))))))
      (let frames ((env env))
        (and env
             (let entries ((bindings (env-bindings env)))
               (if (null? bindings)
                   (frames (env-parent env))
                   (let ((binding (car bindings)))
                     (if (bound-identifier=? (car binding) id)
                         (cdr binding)
                         (entries (cdr bindings))))))))))

;; /**
;;  * The transformer of a macro defined around where `env` is, by name, or #f.
;;  * @param {syntactic-env|boolean} env - Where it is used.
;;  * @param {symbol} name - The name.
;;  * @returns {procedure|boolean}
;;  */
(define (local-macro env name)
  (let loop ((table (and env (env-macros env))))
    (and table
         (let ((entry (assq name (macro-table-entries table))))
           (if entry (cdr entry) (loop (macro-table-parent table)))))))

;; /**
;;  * The innermost frame's macros, where `env` is, or #f if no body or
;;  * `let-syntax` is around it.
;;  * @param {syntactic-env|boolean} env - Where a macro is defined.
;;  * @returns {macro-table|boolean}
;;  */
(define (innermost-macros env)
  (and env (env-macros env)))

;; /**
;;  * Defines a macro in a table, over one of its name there.
;;  * @param {macro-table} table - The table.
;;  * @param {symbol} name - The name.
;;  * @param {procedure} transformer - Its transformer.
;;  */
(define (table-define! table name transformer)
  (set-macro-table-entries! table (cons (cons name transformer) (macro-table-entries table))))

;; ---------------------------------------------------------------------------
;; Syntactic keywords
;; ---------------------------------------------------------------------------

;; /**
;;  * The scope of the library a pending macro was defined in, while the macro
;;  * is realized (`realize!`), or #f.
;;  * @type {parameter}
;;  */
(define realizing-in (make-parameter #f))

;; /**
;;  * The scope of the library or program being expanded -- a pending macro's
;;  * library while it is realized -- or #f at a program's top level.
;;  * @returns {number|boolean}
;;  */
(define (current-scope)
  (or (realizing-in) (%defining-scope)))

;; /**
;;  * Where a procedural macro's procedure is evaluated: the environment of
;;  * the library or program that defines the macro -- its imports, and what
;;  * it defined before -- found by its scope, or else the one the forms
;;  * expanded where `env` is will run in; #f if neither is known. Expansion
;;  * has no phase of its own, as in Chibi, Gauche and Guile: a library shares
;;  * its procedures with its macros, and nothing is loaded twice.
;;  * @param {syntactic-env} env - Where the macro is defined.
;;  * @returns {object|boolean}
;;  */
(define (defining-environment env)
  (or (let ((scope (current-scope))) (and scope (%library-environment scope)))
      (runtime-environment env)))

;; /**
;;  * The scope an identifier is used in: the library whose macro introduced
;;  * it, if one did; otherwise `scope`, if given; otherwise the library or
;;  * program being expanded; otherwise a program's top level, 0.
;;  * @param {identifier} id - The identifier.
;;  * @param {number|boolean} scope - Where it is used, if not where the
;;  *   expander is, or #f.
;;  * @returns {number}
;;  */
(define (scope-of-use id scope)
  (or (library-scope-of id) scope (current-scope) 0))

;; /**
;;  * Whether a scope sees only what it imports and defines: a library's, or
;;  * one `environment` made, or a program's that began with imports.
;;  * @param {number} scope - The scope.
;;  * @returns {boolean}
;;  */
(define (strict-scope? scope)
  (let ((env (%library-environment scope)))
    (and env (%environment-strict? env))))

;; /**
;;  * The name of the syntactic keyword an identifier is where it is used: the
;;  * keyword bound to its name there, which may have another name, as
;;  * `(import (rename (scheme base) (if when-true)))` binds `when-true` to
;;  * `if`; otherwise its own name.
;;  * @param {identifier} id - The identifier.
;;  * @param {number|boolean} scope - Where it is used, if not where the
;;  *   expander is -- for a macro's pattern literal, where the macro was
;;  *   defined -- or #f.
;;  * @returns {symbol}
;;  */
(define (keyword-name id scope)
  (let ((entry (%keyword-entry (scope-of-use id scope) (identifier-name id))))
    (or (and entry (car entry)) (identifier-name id))))

;; /**
;;  * What an identifier in operator position names, where `env` is, as
;;  * `(keyword . transformer)`: a macro, by its transformer, or a special
;;  * form, with none; the keyword #f for a variable. A macro defined around
;;  * the use comes first, by name; then what the name is bound to where the
;;  * identifier is used. Where nothing is, a strict scope -- a library's, a
;;  * program's that imports, an `environment`'s -- has a variable, since a
;;  * special form is a keyword it has only if it imports it (`(scheme-js
;;  * special-forms)`); any other has the macro defined under the name for the
;;  * process, or else the special form of the name, if there is one.
;;  * @param {identifier} id - The identifier.
;;  * @param {syntactic-env} env - Where it is used.
;;  * @returns {pair}
;;  */
(define (operator-keyword id env)
  (let* ((name (identifier-name id))
         (local (local-macro env name)))
    (if local
        (cons name local)
        (let ((scope (scope-of-use id #f)))
          (or (%keyword-entry scope name)
              (if (strict-scope? scope)
                  (cons #f #f)
                  (cons name (%process-macro name))))))))

;; /**
;;  * The keyword a form's head names where `env` is (`operator-keyword`), or
;;  * #f: for a form that is no list, whose head is no identifier or is bound
;;  * locally, or names a variable.
;;  * @param {*} form - The form.
;;  * @param {syntactic-env} env - Where it is.
;;  * @returns {symbol|boolean}
;;  */
(define (form-keyword form env)
  (and (pair? form)
       (identifier? (car form))
       (not (env-lookup env (car form)))
       (car (operator-keyword (car form) env))))

;; /**
;;  * Defines a macro where `env` is: in the body or `let-syntax` around it, if
;;  * there is one; otherwise for the process, and bound in the library being
;;  * expanded -- which keeps it from another library's macro of the name --
;;  * or, at a program's top level, unbound there, so that the name finds what
;;  * is defined for the process.
;;  * @param {syntactic-env} env - Where it is defined.
;;  * @param {symbol} name - Its name.
;;  * @param {procedure} transformer - Its transformer.
;;  */
(define (define-macro! env name transformer)
  (let ((table (innermost-macros env)))
    (if table
        (table-define! table name transformer)
        (let ((scope (current-scope)))
          (%define-process-macro! name transformer)
          (if scope
              (%bind-keyword! scope name name transformer)
              (%forget-keyword! 0 name))))))

;; /**
;;  * Notes a top-level definition where it is made, in the library being
;;  * expanded or at a program's top level: a name bound to a macro there, or
;;  * naming one for the process, now names the variable, as a local
;;  * definition's does.
;;  * @param {symbol} name - The name defined.
;;  */
(define (bind-defined-variable! name)
  (let* ((scope (or (current-scope) 0))
         (entry (%keyword-entry scope name)))
    (if (if entry (cdr entry) (%process-macro name))
        (%bind-keyword! scope name #f #f))))

;; /**
;;  * Where an identifier a library's macro introduced refers to its binding:
;;  * the library's environment, or #f where looking the name up where it is
;;  * used finds that binding itself. Macros are referentially transparent
;;  * (R7RS 4.3): the identifier means the library's binding of its name,
;;  * exported or not, whatever the use site binds the name to, now or later --
;;  * a program that assigns or redefines `eqv?` does not change what `case`
;;  * compares with. Looked up by name, it is the library's binding only where
;;  * the use is in the library itself, or where the library has no binding of
;;  * the name of its own. A reference to a library's binding is compiled as a
;;  * global's is, read through the library's environment (`library-global-key`
;;  * in src/compiler/ir.scm).
;;  * @param {identifier} id - A free identifier.
;;  * @param {number} scope - Its library's scope.
;;  * @returns {object|boolean} The library's environment, or #f.
;;  */
(define (library-binding-env id scope)
  (let ((library-env (%library-environment scope)))
    (and (%environment-binds? library-env (identifier-name id))
         (not (eqv? (current-scope) scope))
         library-env)))

;; ---------------------------------------------------------------------------
;; Errors and spans
;; ---------------------------------------------------------------------------

;; /**
;;  * Raises a syntax error.
;;  * @param {string} message - What is wrong.
;;  * @param {*} form - The form it is about.
;;  * @param {symbol|string} keyword - The keyword whose use it is.
;;  */
(define (raise-syntax-error message form keyword)
  (raise (%make-syntax-error message form keyword)))

;; /**
;;  * A core form, given the span of the form it was made of, if that has one.
;;  * @param {list} core - The core form.
;;  * @param {pair} form - The form.
;;  * @returns {list} `core`.
;;  */
(define (with-source core form)
  (let ((span (js-ref form "source")))
    (if (not (or (js-undefined? span) (js-null? span)))
        (js-set! core "source" span))
    core))

;; ---------------------------------------------------------------------------
;; Expanding a form
;; ---------------------------------------------------------------------------

;; /**
;;  * A form, expanded where a program's or library's top level is, for the
;;  * environment it will run in, if that is given.
;;  * @param {*} form - The form.
;;  * @param {object} [runtime-env] - The environment it will run in, where a
;;  *   macro it defines evaluates its procedure if no library's or program's
;;  *   scope says where (`defining-environment`).
;;  * @returns {list} Its core form.
;;  */
(define (expand form . runtime-env)
  (expand-form form (top-level-env (and (pair? runtime-env) (car runtime-env)))))

;; /**
;;  * A form, expanded inside an environment the evaluator made: an
;;  * expression typed in a paused frame, which sees the frame's locals by
;;  * the names they were written with.
;;  * @param {*} form - The form.
;;  * @param {object} runtime-env - The environment.
;;  * @returns {list} Its core form.
;;  */
(define (expand-in-environment form runtime-env)
  (let build ((frames (%environment-renamings runtime-env)) (env #f))
    (if (null? frames)
        (expand-form form env)
        (build (cdr frames) (make-syntactic-env env (car frames) #f #f (and (not env) runtime-env))))))

;; /**
;;  * A form's core form, where `env` is.
;;  * @param {*} form - The form.
;;  * @param {syntactic-env} env - Where it is.
;;  * @returns {list}
;;  */
(define (expand-form form env)
  (cond ((identifier? form) (expand-variable form env))
        ((pair? form) (expand-pair form env))
        ((null? form) (raise-syntax-error "cannot analyze null (empty list)" '() 'analyze))
        ((%node? form) (list 'node form))
        ;; A vector a macro's template built holds identifiers, not symbols.
        ((vector? form) (list 'lit (if (holds-syntax? form) (syntax->datum form) form)))
        ((%self-evaluating? form) (list 'lit form))
        (else (raise-syntax-error "Unknown expression type" form 'analyze))))

;; /**
;;  * The core forms of forms, expanded in order: an application's operands, or
;;  * a body's forms, which must be a proper list.
;;  * @param {list} forms - The forms.
;;  * @param {syntactic-env} env - Where they are.
;;  * @returns {list}
;;  */
(define (expand-each forms env)
  (cond ((pair? forms)
         (let ((first (expand-form (car forms) env)))
           (cons first (expand-each (cdr forms) env))))
        ((null? forms) '())
        (else (raise-syntax-error "forms are not a proper list" forms 'analyze))))

;; /**
;;  * Whether a datum holds a syntax object anywhere, as data a macro's
;;  * template built do. A literal may be circular (R7RS 2.4).
;;  * @param {*} datum - The datum.
;;  * @returns {boolean}
;;  */
(define (holds-syntax? datum)
  (let ((seen (%make-hash-store 'eq)))
    (let walk ((x datum))
      (cond ((syntax-object? x) #t)
            ((not (or (pair? x) (vector? x))) #f)
            ((%hash-store-contains? seen x) #f)
            (else
             (%hash-store-set! seen x #t)
             (if (pair? x)
                 (or (walk (car x)) (walk (cdr x)))
                 (let elements ((i 0))
                   (and (< i (vector-length x))
                        (or (walk (vector-ref x i)) (elements (+ i 1)))))))))))

;; /**
;;  * A variable's core form: the local it names, by its renamed name, or
;;  * else a global; for one a library's macro introduced, the library's
;;  * binding of the name.
;;  * @param {identifier} id - The variable.
;;  * @param {syntactic-env} env - Where it is used.
;;  * @returns {list}
;;  */
(define (expand-variable id env)
  (let ((local (env-lookup env id)))
    (cond (local (list 'var local))
          ((symbol? id) (list 'var id))
          (else
           (let ((name (identifier-name id))
                 (library-scope (%identifier-library-scope id)))
             (cond (library-scope
                    (let ((library-env (library-binding-env id library-scope)))
                      (if library-env (list 'library-var name library-env) (list 'var name))))
                   ;; A binding a definition registered under scopes the
                   ;; identifier carries, found as the form runs.
                   ((%scope-binding? name (%identifier-scopes id))
                    (list 'scoped-var name (%identifier-scopes id)))
                   (else (list 'var name))))))))

;; /**
;;  * A list's core form: a macro's use expanded, a special form's core form,
;;  * or an application -- by what its operator names, unless a local binds
;;  * the operator, which then shadows the keyword (R7RS 4.3).
;;  * @param {pair} form - The form.
;;  * @param {syntactic-env} env - Where it is.
;;  * @returns {list}
;;  */
(define (expand-pair form env)
  (let ((operator (car form)))
    (if (and (identifier? operator) (not (env-lookup env operator)))
        (let* ((keyword (operator-keyword operator env))
               (transformer (cdr keyword))
               (special (and (not transformer) (car keyword) (special-form (car keyword)))))
          (cond (transformer
                 (expand-form (transform transformer form env (car keyword)) env))
                (special
                 (check-operands (car keyword) form)
                 (with-source (special form env) form))
                (else (with-source (expand-application form env) form))))
        (with-source (expand-application form env) form))))

;; /**
;;  * A macro's use, transformed. A transformer's failure is reported as the
;;  * macro's; a syntax error it raised on purpose, as `syntax-error` does, and
;;  * anything wrong with what it expanded into, are reported as they are,
;;  * rather than once more for each macro the use is inside.
;;  * @param {procedure} transformer - The macro's transformer.
;;  * @param {pair} form - The use.
;;  * @param {syntactic-env} env - Where it is.
;;  * @param {symbol} keyword - The macro's name.
;;  * @returns {*} The form it expands into.
;;  */
(define (transform transformer form env keyword)
  (guard (e ((%syntax-error? e) (raise e))
            (else (raise-syntax-error (string-append "Error expanding macro: " (%error-message e))
                                      form keyword)))
    (call-transformer transformer form env)))

;; /**
;;  * Calls a macro's transformer on a use, giving it where the use is as a
;;  * procedure of an identifier that says what local, if any, binds it there.
;;  * @param {procedure|object} transformer - The transformer, or a pending
;;  *   macro, made one first (`realize!`).
;;  * @param {pair} form - The use.
;;  * @param {syntactic-env} env - Where it is.
;;  * @returns {*}
;;  */
(define (call-transformer transformer form env)
  ((if (procedure? transformer) transformer (realize! transformer))
   form
   (lambda (id) (env-lookup env id))))

;; /**
;;  * The transformer of a pending macro, made the first time it is asked for.
;;  * A library restored from its prebuilt table has run each of its macro
;;  * definitions as a core form that binds the macro pending -- its
;;  * definition, and the scope of the library -- and not made its transformer,
;;  * which would take an expander, which a library the expander is written
;;  * with is restored without. The definition is expanded as it was where the
;;  * library defined it, into a frame of its own, and the transformer it makes
;;  * is kept on the pending macro, where every library that imported it finds
;;  * it.
;;  * @param {object} pending - The pending macro.
;;  * @returns {procedure}
;;  */
(define (realize! pending)
  (or (%realized-macro pending)
      (let ((definition (%pending-macro pending))
            (env (env-with-macros (top-level-env #f))))
        (if (not definition) (raise-syntax-error "not a macro's transformer" pending 'analyze))
        (parameterize ((realizing-in (cdr definition)))
          (expand-form (car definition) env))
        (let ((made (cdar (macro-table-entries (env-macros env)))))
          (%realize-pending-macro! pending made)
          made))))

;; /**
;;  * An application's core form. A property's method called -- `(o.m x)`, read
;;  * as `((js-ref o "m") x)` -- is a call of `js-invoke`, and the
;;  * superclass's, `(super.m x)`, of `class-super-call`.
;;  * @param {pair} form - The application.
;;  * @param {syntactic-env} env - Where it is.
;;  * @returns {list}
;;  */
(define (expand-application form env)
  (let ((operator (car form))
        (operands (cdr form)))
    (if (and (pair? operator) (named? (car operator) 'js-ref))
        (let ((object (cadr operator))
              (method (caddr operator)))
          (if (named? object 'super)
              (let ((arguments (expand-each operands env)))
                (list 'app (list 'var 'class-super-call)
                      (cons (list 'var 'this) (cons (list 'lit (string->symbol method)) arguments))))
              (let* ((target (expand-form object env))
                     (arguments (expand-each operands env)))
                (list 'app (list 'var 'js-invoke) (cons target (cons (list 'lit method) arguments))))))
        (let* ((procedure (expand-form operator env))
               (arguments (expand-each operands env)))
          (list 'app procedure arguments)))))

;; ---------------------------------------------------------------------------
;; Special forms
;; ---------------------------------------------------------------------------

;; /**
;;  * The procedure that expands a special form, by its keyword, or #f.
;;  * @param {symbol} keyword - The keyword.
;;  * @returns {procedure|boolean}
;;  */
(define (special-form keyword)
  (case keyword
    ((quote) expand-quote)
    ((if) expand-if)
    ((lambda) expand-lambda)
    ((let) expand-let)
    ((letrec) expand-letrec)
    ((set!) expand-set)
    ((define) expand-define)
    ((begin) expand-begin)
    ((quasiquote) expand-quasiquote)
    ((define-syntax) expand-define-syntax)
    ((define-macro) expand-define-macro)
    ((let-syntax) expand-let-syntax)
    ((letrec-syntax) expand-letrec-syntax)
    ((import) expand-import)
    ((define-library) expand-define-library)
    ((cond-expand) expand-cond-expand)
    (else #f)))

;; /**
;;  * How many operands a special form takes, at least and at most, as
;;  * `(least . most)`, `most` #f for any number; #f for one not checked. A
;;  * procedure's definition, `(define (f x) ...)`, takes any number: its body
;;  * is checked as a `lambda`'s.
;;  * @param {symbol} keyword - The keyword.
;;  * @param {pair} form - The form.
;;  * @returns {pair|boolean}
;;  */
(define (operand-bounds keyword form)
  (case keyword
    ((quote quasiquote) '(1 . 1))
    ((if) '(2 . 3))
    ((set! define-syntax) '(2 . 2))
    ((define) (if (and (pair? (cdr form)) (pair? (cadr form))) '(2 . #f) '(1 . 2)))
    ((lambda let letrec let-syntax letrec-syntax define-macro) '(2 . #f))
    (else #f)))

;; /**
;;  * Raises a syntax error if a special form's operands are not a proper
;;  * list, or are too few or too many.
;;  * @param {symbol} keyword - The keyword.
;;  * @param {pair} form - The form.
;;  */
(define (check-operands keyword form)
  (let ((bounds (operand-bounds keyword form)))
    (if bounds
        (let count ((rest (cdr form)) (n 0))
          (cond ((pair? rest) (count (cdr rest) (+ n 1)))
                ((not (null? rest))
                 (raise-syntax-error "its operands are not a proper list" form keyword))
                ((or (< n (car bounds)) (and (cdr bounds) (> n (cdr bounds))))
                 (raise-syntax-error (operand-count-message bounds n) form keyword)))))))

;; /**
;;  * What a special form given the wrong number of operands is told.
;;  * @param {pair} bounds - `(least . most)`.
;;  * @param {number} n - How many it was given.
;;  * @returns {string}
;;  */
(define (operand-count-message bounds n)
  (let ((least (car bounds))
        (most (cdr bounds)))
    (string-append "expected "
                   (cond ((eqv? least most) (number->string least))
                         ((not most) (string-append "at least " (number->string least)))
                         (else (string-append (number->string least) " to " (number->string most))))
                   (if (eqv? most 1) " operand" " operands")
                   ", got " (number->string n))))

;; /**
;;  * `(quote datum)`: the datum, its syntax objects made symbols again.
;;  */
(define (expand-quote form env)
  (list 'lit (syntax->datum (cadr form))))

;; /**
;;  * `(if test consequent [alternative])`. With no alternative, a false test
;;  * gives the undefined value.
;;  */
(define (expand-if form env)
  (let* ((test (expand-form (cadr form) env))
         (consequent (expand-form (caddr form) env))
         (alternative (if (null? (cdddr form))
                          (list 'lit js-undefined)
                          (expand-form (cadddr form) env))))
    (list 'if test consequent alternative)))

;; /**
;;  * A lambda's core form.
;;  * @param {list} params - Its parameters, renamed.
;;  * @param {symbol|boolean} rest - Its rest parameter, renamed, or #f.
;;  * @param {string} name - Its name.
;;  * @param {list} body - Its body's core form.
;;  * @param {list} originals - Its parameters' names as written.
;;  * @param {symbol|boolean} original-rest - Its rest parameter's, or #f.
;;  * @returns {list}
;;  */
(define (make-lambda params rest name body originals original-rest)
  (list 'lambda params rest name body originals original-rest))

;; /**
;;  * Whether a core form is a lambda's.
;;  */
(define (lambda-form? core)
  (eq? (car core) 'lambda))

;; /**
;;  * A lambda's core form, named for the variable it is defined as.
;;  * @param {list} core - The core form, which no other form shares.
;;  * @param {symbol} name - The variable's name.
;;  * @returns {list} `core`.
;;  */
(define (name-lambda! core name)
  (set-car! (cdddr core) (symbol->string name))
  core)

;; /**
;;  * `(lambda formals body ...)`: fixed parameters, a rest parameter, or both,
;;  * each renamed. A tail after the fixed parameters that is not an
;;  * identifier is ignored.
;;  */
(define (expand-lambda form env)
  (let ((formals (cadr form))
        (body (cddr form)))
    (if (null? body) (raise-syntax-error "body cannot be empty" form 'lambda))
    (if (identifier? formals)
        ;; Only a rest parameter. Its body is analyzed in the macros around
        ;; it, with no frame of its own for the macros it defines.
        (let ((rest (rename (identifier-name formals))))
          (make-lambda '() rest "anonymous" (expand-body body (env-extend (env-child env) formals rest))
                       '() (identifier-name formals)))
        (let loop ((formals formals) (inner (env-child env)) (params '()) (originals '()))
          (cond ((pair? formals)
                 (let ((param (car formals)))
                   (if (not (identifier? param))
                       (raise-syntax-error "parameter must be a symbol" param 'lambda))
                   (let ((renamed (rename (identifier-name param))))
                     (loop (cdr formals) (env-extend inner param renamed)
                           (cons renamed params) (cons (identifier-name param) originals)))))
                ((identifier? formals)
                 (let ((rest (rename (identifier-name formals))))
                   (make-lambda (reverse params) rest "anonymous"
                                (expand-scoped-body body (env-extend inner formals rest))
                                (reverse originals) (identifier-name formals))))
                (else
                 (make-lambda (reverse params) #f "anonymous" (expand-scoped-body body inner)
                              (reverse originals) #f)))))))

;; /**
;;  * A body's core form: its forms in order, the one form if there is one.
;;  * The names its definitions define are bound first, as written, so that
;;  * its procedures can call each other; at a top level, they shadow macros
;;  * of their names for the rest of the program or library.
;;  * @param {list} body - The forms.
;;  * @param {syntactic-env} env - Where they are.
;;  * @returns {list}
;;  */
(define (expand-body body env)
  (let ((cores (expand-each body (hoist-definitions body env))))
    (if (and (pair? cores) (null? (cdr cores)))
        (car cores)
        (list 'seq cores))))

;; /**
;;  * Where a body's forms are expanded: `env`, with the names its definitions
;;  * define bound, each as itself.
;;  * @param {list} body - The forms.
;;  * @param {syntactic-env} env - Where the body is.
;;  * @returns {syntactic-env}
;;  */
(define (hoist-definitions body env)
  (let loop ((forms body) (inner env))
    (if (pair? forms)
        (let ((id (defined-identifier (car forms) inner)))
          (loop (cdr forms)
                (if id
                    (let ((name (identifier-name id)))
                      (if (env-top-level? env) (bind-defined-variable! name))
                      (env-extend inner id name))
                    inner)))
        inner)))

;; /**
;;  * The identifier a form defines, if it is a `define`, by the keyword its
;;  * head names; else #f.
;;  * @param {*} form - The form.
;;  * @param {syntactic-env} env - Where it is.
;;  * @returns {identifier|boolean}
;;  */
(define (defined-identifier form env)
  (and (eq? (form-keyword form env) 'define)
       (pair? (cdr form))
       (let ((head (cadr form)))
         (cond ((pair? head) (car head))
               ((identifier? head) head)
               (else #f)))))

;; /**
;;  * A procedure's body, a let's or a letrec's: a body that may define
;;  * macros of its own.
;;  */
(define (expand-scoped-body body env)
  (expand-body body (env-with-macros env)))

;; /**
;;  * `(begin form ...)`, a body's forms where it is.
;;  */
(define (expand-begin form env)
  (expand-body (cdr form) env))

;; /**
;;  * A `let`'s or `letrec`'s binding, checked to be `(variable expression)`.
;;  * @param {*} binding - The binding.
;;  * @param {symbol} keyword - The form's keyword.
;;  * @param {pair} form - The form.
;;  * @returns {pair} The binding.
;;  */
(define (checked-binding binding keyword form)
  (if (and (pair? binding) (pair? (cdr binding)) (null? (cddr binding)))
      binding
      (raise-syntax-error "a binding is (variable expression)" form keyword)))

;; /**
;;  * `(let ((variable init) ...) body ...)`, as a lambda applied to the
;;  * inits, which are expanded where the `let` is; or a named `let`.
;;  */
(define (expand-let form env)
  (if (identifier? (cadr form))
      (expand-named-let form env)
      (let loop ((bindings (cadr form)) (ids '()) (params '()) (originals '()) (inits '()))
        (if (pair? bindings)
            (let* ((binding (checked-binding (car bindings) 'let form))
                   (id (car binding))
                   (name (identifier-name id))
                   (renamed (rename name))
                   (init (expand-form (cadr binding) env)))
              (loop (cdr bindings) (cons id ids) (cons renamed params) (cons name originals) (cons init inits)))
            (let ((params (reverse params)))
              (list 'app
                    (make-lambda params #f "let"
                                 (expand-scoped-body (cddr form) (env-extend-all env (reverse ids) params))
                                 (reverse originals) #f)
                    (reverse inits)))))))

;; /**
;;  * `(let name ((variable init) ...) body ...)`, as R7RS 4.2.4 defines it:
;;  *
;;  *   ((letrec ((name (lambda (variable ...) body ...))) name) init ...)
;;  *
;;  * The inits are outside the letrec, expanded where `name` is not bound, so
;;  * that it cannot capture them: `(let - ((n (- 1))) n)` negates. That is Al
;;  * Petrofsky's pitfall 8.1, in tests/core/scheme/r7rs-pitfalls.scm.
;;  */
(define (expand-named-let form env)
  (let* ((name (cadr form))
         (bindings (let collect ((bindings (caddr form)))
                     (if (pair? bindings)
                         (let ((binding (checked-binding (car bindings) 'let form)))
                           (cons binding (collect (cdr bindings))))
                         '())))
         (procedure (cons 'lambda (cons (map car bindings) (cdddr form))))
         (loop (expand-letrec (list 'letrec (list (list name procedure)) name) env))
         (inits (expand-each (map cadr bindings) env)))
    (list 'app loop inits)))

;; /**
;;  * `(letrec ((variable init) ...) body ...)`. Every variable is bound in
;;  * every init. When each init is a lambda -- a named `let`, a group of
;;  * mutually recursive procedures -- it is a `letrec` core form: evaluating a
;;  * lambda has no side effects and cannot observe another binding, so all are
;;  * evaluated before any is assigned, as R7RS requires, trivially, and both
;;  * tiers see that each variable holds the lambda beside it (the `fix` of
;;  * Waddell, Sarkar and Dybvig, "Fixing Letrec"). Otherwise the inits are a
;;  * lambda's arguments, all evaluated before it assigns any:
;;  *
;;  *   ((lambda (v ...) ((lambda (t ...) (set! v t) ... body) init ...))
;;  *    <undefined> ...)
;;  */
(define (expand-letrec form env)
  (let loop ((bindings (cadr form)) (inner env) (names '()) (originals '()) (inits '()))
    (if (pair? bindings)
        (let* ((binding (checked-binding (car bindings) 'letrec form))
               (id (car binding))
               (name (identifier-name id))
               (renamed (rename name)))
          (loop (cdr bindings) (env-extend inner id renamed)
                (cons renamed names) (cons name originals) (cons (cadr binding) inits)))
        (let* ((names (reverse names))
               (originals (reverse originals))
               (cores (expand-each (reverse inits) inner))
               (body (expand-scoped-body (cddr form) inner)))
          (if (every-lambda? cores)
              (list 'letrec names cores body originals)
              (letrec-by-assignment names originals cores body))))))

;; /**
;;  * Whether every core form in a list is a lambda's.
;;  */
(define (every-lambda? cores)
  (or (null? cores)
      (and (lambda-form? (car cores)) (every-lambda? (cdr cores)))))

;; /**
;;  * A `letrec` whose inits are not all lambdas, its variables assigned once
;;  * every init has been evaluated.
;;  * @param {list} names - The variables, renamed.
;;  * @param {list} originals - Their names as written.
;;  * @param {list} inits - The inits' core forms.
;;  * @param {list} body - The body's core form.
;;  * @returns {list}
;;  */
(define (letrec-by-assignment names originals inits body)
  (let ((temporaries (let rename-each ((originals originals))
                       (if (null? originals)
                           '()
                           (let ((temporary (rename (string->symbol
                                                     (string-append (symbol->string (car originals)) "-init")))))
                             (cons temporary (rename-each (cdr originals))))))))
    (list 'app
          (make-lambda names #f "letrec"
                       (list 'app
                             (make-lambda temporaries #f "letrec-init"
                                          (list 'seq (append (map (lambda (name temporary)
                                                                    (list 'set name (list 'var temporary)))
                                                                  names temporaries)
                                                             (list body)))
                                          temporaries #f)
                             inits)
                       originals #f)
          (map (lambda (name) (list 'lit js-undefined)) names))))

;; /**
;;  * `(set! variable value)`: a local's, a global's, or the binding of the
;;  * library whose macro introduced the variable; `(set! o.p value)`, a
;;  * property's.
;;  */
(define (expand-set form env)
  (let* ((value (expand-form (caddr form) env))
         (target (cadr form)))
    (if (and (pair? target) (named? (car target) 'js-ref))
        (let ((object (expand-form (cadr target) env)))
          (list 'app (list 'var 'js-set!) (list object (list 'lit (caddr target)) value)))
        (let ((local (env-lookup env target)))
          (if local
              (list 'set local value)
              (let* ((name (identifier-name target))
                     (library-scope (library-scope-of target))
                     (library-env (and library-scope (library-binding-env target library-scope))))
                (if library-env
                    (list 'library-set name library-env value)
                    (list 'set name value))))))))

;; /**
;;  * `(define variable value)`, and `(define (variable . formals) body ...)`,
;;  * a procedure's, whose lambda is named for it and given the definition's
;;  * span: a place anywhere in the definition is in the procedure.
;;  */
(define (expand-define form env)
  (let ((head (cadr form)))
    (if (pair? head)
        (let ((name (identifier-name (car head))))
          ;; Before the body, which may call the procedure by its name.
          (if (env-top-level? env) (bind-defined-variable! name))
          (let ((procedure (expand-lambda (cons 'lambda (cons (cdr head) (cddr form))) env)))
            (list 'define name (name-lambda! (with-source procedure form) name))))
        (let ((name (identifier-name head)))
          (if (env-top-level? env) (bind-defined-variable! name))
          (let ((value (expand-form (caddr form) env)))
            (list 'define name (if (lambda-form? value) (name-lambda! value name) value)))))))

;; ---------------------------------------------------------------------------
;; quasiquote
;; ---------------------------------------------------------------------------

;; /**
;;  * `(quasiquote template)`: calls of `cons`, `list`, `append`, `vector` and
;;  * `list->vector` building the template's data, with what is unquoted at
;;  * the outermost level evaluated.
;;  */
(define (expand-quasiquote form env)
  (quasi (cadr form) env 0))

;; /**
;;  * Whether a datum is a list headed by an identifier of a name.
;;  */
(define (tagged? x name)
  (and (pair? x) (named? (car x) name)))

;; /**
;;  * A call of a global procedure, by its name.
;;  */
(define (call-of name arguments)
  (list 'app (list 'var name) arguments))

;; /**
;;  * The core form building a quasiquote's template, `depth` quasiquotes
;;  * inside the outermost.
;;  * @param {*} template - The template.
;;  * @param {syntactic-env} env - Where it is.
;;  * @param {number} depth - How deep.
;;  * @returns {list}
;;  */
(define (quasi template env depth)
  (cond ((tagged? template 'quasiquote)
         (call-of 'list (list (list 'lit 'quasiquote) (quasi (cadr template) env (+ depth 1)))))
        ((tagged? template 'unquote)
         (if (= depth 0)
             (expand-form (cadr template) env)
             (call-of 'list (list (list 'lit 'unquote) (quasi (cadr template) env (- depth 1))))))
        ((tagged? template 'unquote-splicing)
         (if (= depth 0)
             (raise-syntax-error "unquote-splicing not allowed at top level" template 'quasiquote)
             (call-of 'list (list (list 'lit 'unquote-splicing) (quasi (cadr template) env (- depth 1))))))
        ((pair? template)
         (if (and (= depth 0) (tagged? (car template) 'unquote-splicing))
             (let* ((spliced (expand-form (cadr (car template)) env))
                    (rest (quasi (cdr template) env depth)))
               (call-of 'append (list spliced rest)))
             (let* ((head (quasi (car template) env depth))
                    (rest (quasi (cdr template) env depth)))
               (call-of 'cons (list head rest)))))
        ((vector? template)
         (let ((items (vector->list template)))
           (if (and (= depth 0) (spliced-in? items))
               (call-of 'list->vector (list (quasi items env depth)))
               (call-of 'vector (let each ((items items))
                                  (if (null? items)
                                      '()
                                      (let ((first (quasi (car items) env depth)))
                                        (cons first (each (cdr items))))))))))
        (else (list 'lit (syntax->datum template)))))

;; /**
;;  * Whether a vector template's items splice anything in.
;;  */
(define (spliced-in? items)
  (and (pair? items)
       (or (tagged? (car items) 'unquote-splicing) (spliced-in? (cdr items)))))

;; ---------------------------------------------------------------------------
;; Macros
;; ---------------------------------------------------------------------------

;; /**
;;  * A list's items up to its end or the first thing not a pair.
;;  */
(define (list-items x)
  (if (pair? x) (cons (car x) (list-items (cdr x))) '()))

;; /**
;;  * A `syntax-rules`'s clauses, each as `(pattern . template)`.
;;  */
(define (clause-pairs clauses)
  (map (lambda (clause) (cons (car clause) (cadr clause))) (list-items clauses)))

;; /**
;;  * The library or program being expanded's scope, or a program's top
;;  * level's: where a macro defined now is defined.
;;  */
(define (defining-scope)
  (or (current-scope) 0))

;; /**
;;  * `(define-syntax name transformer)`. A transformer of `syntax-rules`, its
;;  * ellipsis given or `...`, or of `er-macro-transformer`, is defined where
;;  * the form is; anything else is a syntax error -- as `er-macro-transformer`
;;  * is where it is not imported.
;;  */
(define (expand-define-syntax form env)
  (let ((name (identifier-name (cadr form)))
        (spec (caddr form)))
    (cond ((transformer-of? spec 'syntax-rules env)
           (let* ((after (cdr spec))
                  (first (if (pair? after) (car after) '())))
             (cond ((or (pair? first) (null? first))
                    (define-syntax-rules! env name '... first (if (pair? after) (cdr after) '())))
                   ((identifier? first)
                    (define-syntax-rules! env name (identifier-name first) (cadr after) (cddr after))))))
          ((transformer-of? spec 'er-macro-transformer env)
           (define-macro! env name (er-macro-definition spec name env)))
          (else (raise-syntax-error "Transformer must be (syntax-rules ...) or (er-macro-transformer ...)"
                                    spec 'define-syntax))))
  (list 'lit '()))

;; /**
;;  * Whether a macro's transformer, as written, is made by a keyword:
;;  * `syntax-rules` or `er-macro-transformer`, as its head names it where the
;;  * definition is.
;;  * @param {*} spec - The transformer, as written.
;;  * @param {symbol} keyword - The keyword.
;;  * @param {syntactic-env} env - Where the definition is.
;;  * @returns {boolean}
;;  */
(define (transformer-of? spec keyword env)
  (eq? (form-keyword spec env) keyword))

;; /**
;;  * `(er-macro-transformer procedure)`'s transformer: the procedure,
;;  * evaluated as the macro is defined, made an explicit-renaming macro's
;;  * defined where `env` is (explicit_renaming.scm).
;;  * @param {pair} spec - The transformer, as written.
;;  * @param {symbol} name - The macro's name.
;;  * @param {syntactic-env} env - Where it is defined.
;;  * @returns {procedure}
;;  */
(define (er-macro-definition spec name env)
  (if (not (and (pair? (cdr spec)) (null? (cddr spec))))
      (raise-syntax-error "er-macro-transformer takes one procedure" spec 'er-macro-transformer))
  (let ((procedure (transformer-procedure (expand-form (cadr spec) env) name spec 'er-macro-transformer env)))
    (reflecting (er-transformer procedure (defining-scope) env) procedure)))

;; /**
;;  * A procedural macro's transformer, given the procedure a debugger finds it
;;  * by: the one its definition gave, which has the definition's span.
;;  * @param {procedure} transformer - The transformer.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {procedure} The transformer.
;;  */
(define (reflecting transformer procedure)
  (%reflect-transformer! transformer procedure)
  transformer)

;; /**
;;  * Defines a `syntax-rules` macro where `env` is.
;;  * @param {syntactic-env} env - Where it is defined.
;;  * @param {symbol} name - Its name.
;;  * @param {symbol} ellipsis - Its ellipsis.
;;  * @param {*} literals - Its literals, as written.
;;  * @param {*} clauses - Its clauses, as written.
;;  */
(define (define-syntax-rules! env name ellipsis literals clauses)
  (define-macro! env name
    (syntax-rules-transformer (list-items literals) (clause-pairs clauses) (defining-scope) ellipsis env)))

;; /**
;;  * `(define-macro (name . formals) body ...)`, or `(define-macro name
;;  * transformer)`: a procedure of the use's operands, evaluated as the macro
;;  * is defined, where it is defined (`defining-environment`), whose result is
;;  * what the use expands into. Its lambda, in the first shape, has the
;;  * definition's span.
;;  *
;;  * A legacy extension, kept for code written for other Lisps: nothing it
;;  * introduces is renamed, so a binding it makes captures the user's and a
;;  * name it refers to means whatever the use site binds it to. It is an
;;  * explicit-renaming macro that renames nothing and compares nothing, which
;;  * `er-macro-transformer` writes hygienically.
;;  */
(define (expand-define-macro form env)
  (let ((head (cadr form)))
    (let-values (((name made)
                  (cond ((pair? head)
                         (values (identifier-name (car head))
                                 (with-source (expand-lambda (cons 'lambda (cons (cdr head) (cddr form))) env)
                                              form)))
                        ((identifier? head)
                         (values (identifier-name head) (expand-form (caddr form) env)))
                        (else (raise-syntax-error "Invalid define-macro syntax" form 'define-macro)))))
      (let ((procedure (transformer-procedure made name form 'define-macro env)))
        (define-macro! env name
          (reflecting (er-transformer (lambda (use rename compare) (apply-transformer procedure name use))
                                      (defining-scope) env)
                      procedure))
        (list 'lit '())))))

;; /**
;;  * A procedural macro's procedure, evaluated.
;;  * @param {list} made - Its core form.
;;  * @param {symbol} name - The macro's name.
;;  * @param {pair} form - The definition, or the transformer as written.
;;  * @param {symbol} keyword - What defines it: `define-macro` or
;;  *   `er-macro-transformer`.
;;  * @param {syntactic-env} env - Where it is defined.
;;  * @returns {procedure}
;;  */
(define (transformer-procedure made name form keyword env)
  (guard (e (#t (raise-syntax-error (string-append "Error evaluating macro transformer for '"
                                                   (symbol->string name) "': " (%error-message e))
                                    form keyword)))
    (%evaluate-transformer made (defining-environment env))))

;; /**
;;  * A `define-macro`'s use, expanded: its procedure applied to its operands.
;;  * @param {procedure} procedure - The procedure.
;;  * @param {symbol} name - The macro's name.
;;  * @param {pair} use - The use.
;;  * @returns {*}
;;  */
(define (apply-transformer procedure name use)
  (guard (e ((%syntax-error? e) (raise e))
            (else (raise-syntax-error (string-append "Error expanding macro '" (symbol->string name) "': "
                                                     (%error-message e))
                                      use name)))
    (apply procedure (cdr use))))

;; /**
;;  * A `let-syntax`'s or `letrec-syntax`'s binding's transformer, which must
;;  * be `syntax-rules`'s, its ellipsis `...`, or `er-macro-transformer`'s.
;;  * @param {*} spec - The transformer, as written.
;;  * @param {symbol} name - The macro's name.
;;  * @param {syntactic-env} env - Where the form is.
;;  * @returns {procedure}
;;  */
(define (binding-transformer spec name env)
  (cond ((transformer-of? spec 'syntax-rules env)
         (syntax-rules-transformer (list-items (cadr spec)) (clause-pairs (cddr spec)) (defining-scope) '... env))
        ((transformer-of? spec 'er-macro-transformer env) (er-macro-definition spec name env))
        (else (raise-syntax-error "Transformer must be (syntax-rules ...) or (er-macro-transformer ...)"
                                  spec 'syntax-rules))))

;; /**
;;  * The macros a `let-syntax`'s or `letrec-syntax`'s bindings define.
;;  * @param {*} bindings - The bindings.
;;  * @param {syntactic-env} env - Where the form is.
;;  * @param {macro-table} table - Where they are defined.
;;  */
(define (define-bindings! bindings env table)
  (for-each (lambda (binding)
              (let ((name (identifier-name (car binding))))
                (table-define! table name (binding-transformer (cadr binding) name env))))
            (list-items bindings)))

;; /**
;;  * A form where macros defined around it are found: one whose head names a
;;  * macro, expanded until it does not.
;;  */
(define (expand-with-macros form env)
  (let ((transformer (and (pair? form) (identifier? (car form))
                          (cdr (operator-keyword (car form) env)))))
    (if transformer
        (expand-with-macros (call-transformer transformer form env) env)
        (expand-form form env))))

;; /**
;;  * `(let-syntax ((name transformer) ...) body ...)`: the body as a `let`'s
;;  * with no bindings, where the macros are defined.
;;  */
(define (expand-let-syntax form env)
  (if (null? (cddr form)) (raise-syntax-error "body cannot be empty" form 'let-syntax))
  (let ((inner (env-with-macros env)))
    (define-bindings! (cadr form) env (env-macros inner))
    (expand-with-macros (cons 'let (cons '() (cddr form))) inner)))

;; /**
;;  * `(letrec-syntax ((name transformer) ...) form ...)`: the forms where the
;;  * macros are defined, as forms where the `letrec-syntax` is, so that a
;;  * definition among them defines there.
;;  */
(define (expand-letrec-syntax form env)
  (if (null? (cddr form)) (raise-syntax-error "body cannot be empty" form 'letrec-syntax))
  (let ((inner (env-with-macros env)))
    (define-bindings! (cadr form) env (env-macros inner))
    (let ((cores (let each ((forms (cddr form)))
                   (if (pair? forms)
                       (let ((first (expand-with-macros (car forms) inner)))
                         (cons first (each (cdr forms))))
                       '()))))
      (if (null? (cdr cores)) (car cores) (list 'seq cores)))))

;; ---------------------------------------------------------------------------
;; Libraries and features
;; ---------------------------------------------------------------------------

;; /**
;;  * `(import import-set ...)`, imported where it runs.
;;  */
(define (expand-import form env)
  (list 'import (cdr form)))

;; /**
;;  * `(define-library name declaration ...)`, defined where it runs.
;;  */
(define (expand-define-library form env)
  (list 'define-library form))

;; /**
;;  * `(cond-expand (requirement form ...) ...)`: the forms of the first
;;  * clause whose feature requirement is met, or of its `else`, as a `begin`.
;;  */
(define (expand-cond-expand form env)
  (expand-form (cond-expand-choice form) env))

;; /**
;;  * The form a `cond-expand` chooses.
;;  * @param {pair} form - The `cond-expand`.
;;  * @returns {*}
;;  */
(define (cond-expand-choice form)
  (let loop ((clauses (cdr form)))
    (cond ((null? clauses) (raise-syntax-error "no matching clause and no else" form 'cond-expand))
          ((null? (car clauses)) (loop (cdr clauses)))
          ((or (eq? (caar clauses) 'else) (%feature-requirement-met? (caar clauses)))
           (let ((forms (cdar clauses)))
             (cond ((null? forms) (list 'begin))
                   ((null? (cdr forms)) (car forms))
                   (else (cons 'begin forms)))))
          (else (loop (cdr clauses))))))
