;; The compiler's driver: which procedures are compiled, and each reason one
;; is not.
;;
;; The passes before this file turn a lambda into JavaScript source; this one
;; decides what to hand them and what to do with what comes back. A procedure
;; is compiled when the compiler can express every form in it, and left to the
;; interpreter otherwise, with a reason -- so the tier can grow a form at a
;; time without any point at which the system is half correct, and the
;; interpreter stays the reference semantics and the execution mode where
;; generating code is not allowed.
;;
;; What it works on comes from the interpreter, which is JavaScript: analyzed
;; forms, closures, environments. It reads and changes them only through
;; `(scheme-js compiler host)` (src/compiler/host.js), and reads a form's
;; contents as the tagged lists `ir.scm` lowers. JavaScript reaches the entry
;; points -- `compile-definition`, `compile-expression`, `compile-closure`,
;; `generate-environment`, `compile-environment`, `compile-program` -- through
;; src/compiler/index.js, and reads the records they return as objects, one
;; property per field.

;; ---------------------------------------------------------------------------
;; What compiling one procedure comes to
;; ---------------------------------------------------------------------------

;; /**
;;  * A procedure's JavaScript, generated and not yet made a procedure: what the
;;  * build writes into the bundle, and what `instantiate-generated` turns into
;;  * a procedure where generating code is allowed.
;;  * @property {string} name - The name it was compiled under.
;;  * @property {procedure|boolean} closure - The closure it was compiled from, or
;;  *   #f.
;;  * @property {object} env - The environment its globals resolve in.
;;  * @property {object|boolean} span - Its source span, for the debugger, or #f.
;;  * @property {string} source - The body of a function of the runtime `R`, the
;;  *   environment `E` and the constant pool `K`.
;;  * @property {list} constants - The constant pool.
;;  * @property {list} globals - The globals it references, as symbols.
;;  * @property {list} spans - The span of each of the source's lines, or #f,
;;  *   which its source map is written from (`sourcemap.scm`).
;;  */
(define-record-type generated
  (make-generated name closure env span source constants globals spans)
  generated?
  (name generated-name)
  (closure generated-closure)
  (env generated-env)
  (span generated-span)
  (source generated-source)
  (constants generated-constants)
  (globals generated-globals)
  (spans generated-spans))

;; /**
;;  * A procedure compiled.
;;  * @property {string} name - The name it was compiled under.
;;  * @property {procedure} procedure - The compiled procedure.
;;  * @property {string} source - Its generated source, for inspection.
;;  */
(define-record-type compiled
  (make-compiled name procedure source)
  compiled?
  (name compiled-name)
  (procedure compiled-procedure)
  (source compiled-source))

;; /**
;;  * A procedure the compiler declined, and why.
;;  * @property {string|boolean} name - Its name, or #f for an expression.
;;  * @property {string} reason - Why.
;;  * @property {string|boolean} source - Its source, when it was generated and
;;  *   JavaScript would not take it; #f otherwise.
;;  */
(define-record-type declined
  (make-declined name reason source)
  declined?
  (name declined-name)
  (reason declined-reason)
  (source declined-source))

;; ---------------------------------------------------------------------------
;; What the lowering reports
;; ---------------------------------------------------------------------------
;;
;; `lower-lambda` answers a `lowering-failure`, or a `lowered-lambda` (both in
;; `ir.scm`).

;; /**
;;  * The first control-transferring global among some globals, or #f: the
;;  * forms there is no IR for, as `control-globals` in `ir.scm` names them.
;;  * @param {list} globals - Global names, as symbols.
;;  * @returns {symbol|boolean}
;;  */
(define (control-global-in globals)
  (find (lambda (g) (memq g control-globals)) globals))

;; /**
;;  * Why a lowered procedure is not to be compiled, or #f if it is.
;;  *
;;  * A procedure that names `dynamic-wind`, `guard` or the like is declined
;;  * because there is no IR for those forms, not because compiling it would be
;;  * wrong. One that captures a continuation is compiled unless captures are
;;  * declined: nearly every capture is an escape, which compiled code pays for
;;  * easily, and one whose continuations are re-entered over and over is
;;  * switched back to its closure as the program runs (`note-resume` in
;;  * `tier.scm`).
;;  *
;;  * @param {lowered-lambda|lowering-failure} lowered - What `lower-lambda`
;;  *   answered.
;;  * @param {boolean} decline-captures? - Whether to decline a capture.
;;  * @returns {string|boolean} The reason, or #f.
;;  */
(define (lowering-decline lowered decline-captures?)
  (cond
    ((lowering-failure? lowered) (lowering-failure-reason lowered))
    ((control-global-in (lowered-globals lowered))
     => (lambda (g) (string-append "references control global '" (symbol->string g) "'")))
    ((and decline-captures? (lowered-captures? lowered))
     "captures a continuation, and captures are declined")
    (else #f)))

;; ---------------------------------------------------------------------------
;; What a form contains
;; ---------------------------------------------------------------------------
;;
;; Read from the tagged lists `ir.scm` lowers. A form the marshalling does not
;; know becomes `(other description)`, whose contents are not looked into; the
;; lowering declines such a form anyway, so what it contains cannot change
;; whether it is compiled.

;; /**
;;  * The forms directly inside a form: its subexpressions, never a quoted
;;  * literal's contents.
;;  * @param {list} form - An analyzed form.
;;  * @returns {list}
;;  */
(define (subforms form)
  (case (ast-tag form)
    ((if) (list (ast-1 form) (ast-2 form) (ast-3 form)))
    ((seq) (ast-1 form))
    ((lambda) (list (ast-4 form)))
    ((let) (list (ast-2 form) (ast-3 form)))
    ((letrec) (cons (ast-3 form) (ast-2 form)))
    ((set define) (list (ast-2 form)))
    ((app) (cons (ast-1 form) (ast-2 form)))
    (else '())))

;; /**
;;  * Whether a form, or any form inside it, is one of some kinds.
;;  * @param {list} tags - The kinds, as `ir.scm`'s tags.
;;  * @param {list} form - An analyzed form.
;;  * @returns {boolean}
;;  */
(define (contains-kind? tags form)
  (let search ((form form))
    (or (and (memq (ast-tag form) tags) #t)
        (any search (subforms form)))))

;; /**
;;  * Whether a form makes a procedure or loops -- a named `let`, a `do` and a
;;  * group of local procedures are all a `letrec` by now -- which is what makes
;;  * compiling it worth what compiling costs.
;;  * @param {list} form - An analyzed form.
;;  * @returns {boolean}
;;  */
(define (makes-procedures-or-loops? form) (contains-kind? '(lambda letrec) form))

;; /**
;;  * Whether a form loops, here or in a procedure it makes.
;;  * @param {list} form - An analyzed form.
;;  * @returns {boolean}
;;  */
(define (contains-loop? form) (contains-kind? '(letrec) form))

;; /**
;;  * Whether a form defines at top level: a definition, or a `begin` whose
;;  * definitions splice into the environment it runs in.
;;  * @param {list} form - An analyzed form.
;;  * @returns {boolean}
;;  */
(define (defines-at-top-level? form)
  (case (ast-tag form)
    ((define) #t)
    ((seq) (any defines-at-top-level? (ast-1 form)))
    (else #f)))

;; /**
;;  * Whether a form defines a procedure.
;;  * @param {list} form - An analyzed form.
;;  * @returns {boolean}
;;  */
(define (procedure-definition? form)
  (and (eq? (ast-tag form) 'define) (eq? (ast-tag (ast-2 form)) 'lambda)))

;; ---------------------------------------------------------------------------
;; Generating a procedure
;; ---------------------------------------------------------------------------

;; /**
;;  * The largest generated source, in characters, a procedure may produce.
;;  *
;;  * A procedure is emitted twice, and so is every procedure nested inside it,
;;  * once within each form of its parent, so a nested closure costs a multiple
;;  * per level of nesting, measured at about 4.2x. Deep enough, that exceeds
;;  * JavaScript's own maximum string length. A `let` used to be the worst case,
;;  * since it reaches the compiler as an immediately applied lambda; lowering
;;  * now turns those into bindings that nest nothing, which took the deepest
;;  * procedure in the benchmark corpus from twenty-nine levels to seven. The
;;  * bound stays for closures that really are nested, and costs nothing real:
;;  * a procedure whose body is megabytes of JavaScript would not have been fast.
;;  * @type {integer}
;;  */
(define max-source (* 4 1024 1024))

;; /**
;;  * Why a generated source is too large to keep, or #f if it is not.
;;  * @param {string} source - The source.
;;  * @returns {string|boolean}
;;  */
(define (source-too-large source)
  (let ((size (string-length source)))
    (and (> size max-source)
         (string-append "generated source is " (number->string size) " characters, over the "
                        (number->string max-source)
                        " limit; the procedure nests too deeply to emit twice"))))

;; /**
;;  * The globals with an inline expansion.
;;  * @type {list}
;;  */
(define expandable-globals (inline-expansion-names))

;; /**
;;  * The globals among a procedure's that may be expanded inline where it will
;;  * run: those with an expansion that the environment still binds to the
;;  * primitive the expansion reproduces. A program that has already redefined
;;  * `car` must not have its `car` compiled as the primitive's.
;;  * @param {list} globals - The procedure's globals, as symbols.
;;  * @param {object} env - The environment it will run in.
;;  * @returns {list} The globals to expand.
;;  */
(define (guarded-globals globals env)
  (filter (lambda (g)
            (and (memq g expandable-globals) (bound-to-primitive? env (symbol->string g))))
          globals))

;; /**
;;  * Generates a lowered procedure's JavaScript.
;;  * @param {lowered-lambda|lowering-failure} lowered - What `lower-lambda`
;;  *   answered.
;;  * @param {string} name - The name to compile it under.
;;  * @param {procedure|boolean} closure - The closure it comes from, or #f.
;;  * @param {object} env - The environment it will run in.
;;  * @param {object|boolean} span - Its source span, or #f.
;;  * @returns {generated|declined} The code, or why it is too large to keep.
;;  */
(define (emit-lowered lowered name closure env span)
  (let* ((globals (lowered-globals lowered))
         (unit (generate-unit (lowered-ir lowered) globals name (guarded-globals globals env)))
         (source (car unit)))
    (cond ((source-too-large source) => (lambda (reason) (make-declined name reason #f)))
          (else (make-generated name closure env span source (cadr unit) globals (caddr unit))))))

;; /**
;;  * The message an error raised while generating code carries.
;;  * @param {*} e - What was raised.
;;  * @returns {string}
;;  */
(define (failure-message e)
  (if (error-object? e) (error-object-message e) "an object that is not an error was raised"))

;; /**
;;  * `emit-lowered`, declining rather than raising if generation fails. The one
;;  * procedure here the compiler leaves interpreted when it compiles itself,
;;  * since `guard` is a control form; what it calls is compiled.
;;  * @returns {generated|declined}
;;  */
(define (emit-guarded lowered name closure env span)
  (guard (e (#t (make-declined name (string-append "code generation failed: " (failure-message e)) #f)))
    (emit-lowered lowered name closure env span)))

;; /**
;;  * Lowers a lambda, as the tagged list `ir.scm` reads, and generates its
;;  * JavaScript.
;;  * @param {list} node - The lambda.
;;  * @param {string} name - The name to compile it under.
;;  * @param {procedure|boolean} closure - The closure it comes from, or #f.
;;  * @param {object} env - The environment its globals resolve in.
;;  * @param {object|boolean} span - Its source span, or #f.
;;  * @param {boolean} decline-captures? - Whether to decline a capture.
;;  * @returns {generated|declined}
;;  */
(define (generate-lambda node name closure env span decline-captures?)
  (let* ((lowered (lower-lambda node))
         (reason (lowering-decline lowered decline-captures?)))
    (if reason
        (make-declined name reason #f)
        (emit-guarded lowered name closure env span))))

;; /**
;;  * A file's name as a place in a `scheme:///` URL: a name that is itself a
;;  * URL -- a page's script with a `src` -- by its path, since its scheme and
;;  * host would only be repeated inside another URL; any other as it is.
;;  * @param {string} file - The name.
;;  * @returns {string}
;;  */
(define (file-place file)
  (let ((scheme-end (string-contains file "://")))
    (let ((path (and scheme-end
                     (string-index file (lambda (c) (char=? c #\/)) (+ scheme-end 3)))))
      (if path (substring file (+ path 1) (string-length file)) file))))

;; /**
;;  * Where generated code says it comes from, in a stack trace and in a
;;  * debugger's list of sources: `scheme:///` and the file the procedure was
;;  * read from (`file-place`) -- or else its library, or else the program --
;;  * and the procedure's name. Without it an engine names the code by where
;;  * `new Function` was called, the same for every procedure.
;;  * @param {generated} code - The code.
;;  * @returns {string} The URL.
;;  */
(define (source-url code)
  (let* ((span (generated-span code))
         (file (and span (span-file span)))
         (library (environment-library (generated-env code)))
         (place (cond (file (file-place file))
                      ;; A library's name, which the host keeps as a vector of strings.
                      (library (string-join (vector->list library) "/"))
                      (else "program"))))
    (string-append "scheme:///" (url-path-escape place) "/" (url-path-escape (generated-name code)))))

;; /**
;;  * How many lines the script `instantiate` makes has before the generated
;;  * code: the two of the function `new Function` wraps a body in, `function
;;  * anonymous(R,E,K` and `) {`, and the `'use strict';` before the body
;;  * (`instantiate` in `host.js`).
;;  */
(define lines-before-generated-code 3)

;; /**
;;  * Generated code as the script it runs as: named for a debugger by
;;  * `source-url`, and with its source map where it has one. Only as a program
;;  * runs: the build writes the same code into a module, whose own URL it has.
;;  * @param {generated} code - The code.
;;  * @returns {string}
;;  */
(define (script-of code)
  (let ((map (source-map (generated-spans code) lines-before-generated-code source-text)))
    (string-append (generated-source code)
                   "\n//# sourceURL=" (source-url code)
                   (if map (string-append "\n//# sourceMappingURL=" (source-map-url map)) ""))))

;; /**
;;  * Makes generated code a procedure.
;;  * @param {generated} code - The code.
;;  * @returns {compiled|declined} The procedure, with the script it was made
;;  *   from, or why JavaScript would not take the code.
;;  */
(define (instantiate-generated code)
  (let* ((script (script-of code))
         (procedure (instantiate script (generated-env code) (generated-constants code) (generated-span code))))
    (if (string? procedure)
        (make-declined (generated-name code) procedure script)
        (make-compiled (generated-name code) procedure script))))

;; /**
;;  * Compiles a lambda: `generate-lambda`, then `instantiate-generated`.
;;  * @returns {compiled|declined}
;;  */
(define (compile-lambda node name closure env span decline-captures?)
  (let ((result (generate-lambda node name closure env span decline-captures?)))
    (if (generated? result) (instantiate-generated result) result)))

;; ---------------------------------------------------------------------------
;; Definitions, expressions and closures
;; ---------------------------------------------------------------------------

;; /**
;;  * Compiles a top-level procedure definition.
;;  * @param {object} node - The analyzed definition.
;;  * @param {object} env - The environment it belongs to.
;;  * @param {boolean} decline-captures? - Whether to decline a capture.
;;  * @returns {compiled|declined}
;;  */
(define (compile-definition node env decline-captures?)
  (let ((form (ast->scheme node)))
    (cond
      ((not (eq? (ast-tag form) 'define)) (make-declined #f "not a top-level definition" #f))
      ((not (eq? (ast-tag (ast-2 form)) 'lambda))
       (make-declined (symbol->string (ast-1 form)) "definition is not a procedure" #f))
      (else (compile-lambda (ast-2 form) (symbol->string (ast-1 form)) #f env
                            (definition-span node) decline-captures?)))))

;; /**
;;  * A top-level expression as a lambda of no arguments, to call once.
;;  * @param {list} form - The expression, as `ir.scm` reads it.
;;  * @returns {list} The lambda.
;;  */
(define (expression-thunk form) (list 'lambda '() #f "top-level" form))

;; /**
;;  * Compiles a top-level expression, or the value of a definition that is not
;;  * a procedure, as a procedure of no arguments to call once.
;;  *
;;  * A program's own procedures are often not top-level definitions at all:
;;  * `benchmarks/r7rs/src/nboyer.scm` defines stubs, then assigns every real
;;  * procedure from inside one top-level `(let () ...)`, so compiling
;;  * definitions alone left the whole program interpreted. A definition whose
;;  * value is made by an expression -- a closure over a table, say -- is the
;;  * same case.
;;  *
;;  * Declined, and so interpreted: a form that defines at top level, since in a
;;  * procedure its definitions would become internal ones, and one that makes no
;;  * procedure and has no loop, since straight-line code runs once and
;;  * compiling it costs more than running it.
;;  *
;;  * @param {object} node - The analyzed expression.
;;  * @param {object} env - The environment its globals resolve in.
;;  * @param {string|boolean} name - The definition it is the value of, or #f.
;;  * @param {boolean} decline-captures? - Whether to decline a capture.
;;  * @returns {compiled|declined} The outcome; a compiled procedure is the thunk.
;;  */
(define (compile-expression node env name decline-captures?)
  (compile-expression-form (ast->scheme node) env (ast-span node) name decline-captures?))

;; /**
;;  * `compile-expression`, for a form already read as `ir.scm` reads it.
;;  * @param {list} form - The expression.
;;  * @param {object} env - The environment its globals resolve in.
;;  * @param {object|boolean} span - Its source span, or #f.
;;  * @param {string|boolean} name - The definition it is the value of, or #f.
;;  * @param {boolean} decline-captures? - Whether to decline a capture.
;;  * @returns {compiled|declined}
;;  */
(define (compile-expression-form form env span name decline-captures?)
  (cond
    ((defines-at-top-level? form) (make-declined name "defines at top level" #f))
    ((not (makes-procedures-or-loops? form))
     (make-declined name "makes no procedure and has no loop, so runs once" #f))
    (else (compile-lambda (expression-thunk form) "top-level" #f env span decline-captures?))))

;; /**
;;  * Compiles an interpreted closure, in its own environment, so its free
;;  * variables resolve as they did when it was interpreted: a library
;;  * procedure's globals live in that library's environment, not the program's.
;;  * @param {procedure} closure - The closure.
;;  * @param {string} name - The name to compile it under.
;;  * @param {boolean} decline-captures? - Whether to decline a capture.
;;  * @returns {compiled|declined}
;;  */
(define (compile-closure closure name decline-captures?)
  (if (interpreted-closure? closure)
      (compile-lambda (closure-lambda closure name) name closure (closure-environment closure)
                      (closure-span closure) decline-captures?)
      (make-declined name "not an interpreted closure" #f)))

;; ---------------------------------------------------------------------------
;; An environment's procedures
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether a closure was made in an environment or a scope inside it. A
;;  * procedure a library imported closes over the library that defined it,
;;  * which is never inside this one.
;;  * @param {procedure} closure - An interpreted closure.
;;  * @param {object} env - The environment.
;;  * @returns {boolean}
;;  */
(define (defined-within? closure env)
  (let walk ((scope (closure-environment closure)))
    (and scope (or (eq? scope env) (walk (environment-parent scope))))))

;; /**
;;  * Whether a closure was made at the top level of a program or a library,
;;  * rather than inside a procedure or a `let`. One made inside closes over
;;  * locals, which generated code reaches by their names, and a local's name
;;  * carries the counter the expander renamed it with in this run: code
;;  * generated for a prebuilt table, in one run, would look for it under a name
;;  * that the run installing the table did not give it.
;;  * @param {procedure} closure - An interpreted closure.
;;  * @returns {boolean}
;;  */
(define (made-at-top-level? closure)
  (let ((scope (closure-environment closure)))
    (if (or (not (environment-parent scope)) (environment-library scope)) #t #f)))

;; /**
;;  * The interpreted closures an environment binds, as (name . closure) with the
;;  * name a symbol, in the environment's order.
;;  * @param {object} env - The environment.
;;  * @param {boolean} own-only? - Only those made in the environment itself,
;;  *   leaving out what it imported: a library's environment holds a copy of
;;  *   everything it imports, and a table built for one library must not carry
;;  *   a second compiled copy of another's procedures.
;;  * @returns {list}
;;  */
(define (environment-closures env own-only?)
  (filter-map (lambda (binding)
                (let ((value (cdr binding)))
                  (and (interpreted-closure? value)
                       (or (not own-only?) (defined-within? value env))
                       (cons (string->symbol (car binding)) value))))
              (environment-bindings env)))

;; /**
;;  * Generates code for every procedure in an environment worth compiling,
;;  * without making procedures of it.
;;  *
;;  * Separate from making them so that one policy serves both callers: the
;;  * build step that writes generated source into the bundle, and
;;  * `compile-environment`, which makes the procedures at once. Two answers to
;;  * "which procedures do we compile" would drift, and the build's has to match
;;  * the running system's or the bundle would hold code for procedures the
;;  * system does not expect.
;;  *
;;  * @param {object} env - The environment.
;;  * @param {boolean} own-only? - As for `environment-closures`.
;;  * @param {boolean} decline-captures? - Decline every procedure a capture
;;  *   could unwind through, and every one that captures (`safety.scm`).
;;  * @param {boolean} strict? - With it, also decline one that calls a callee it
;;  *   cannot name.
;;  * @returns {pair} `(generated . declined)`, each in the environment's order.
;;  */
(define (generate-environment env own-only? decline-captures? strict?)
  (let* ((closures (environment-closures env own-only?))
         (unsafe (if decline-captures? (unsafe-closures closures env strict?) '())))
    (define (attempt entry)
      (let ((name (symbol->string (car entry)))
            (closure (cdr entry)))
        (cond ((assq (car entry) unsafe) => (lambda (u) (make-declined name (cdr u) #f)))
              ((not (made-at-top-level? closure))
               (make-declined name
                              (string-append "made inside a procedure, so it closes over that "
                                             "procedure's locals, which generated code finds by "
                                             "the names this run's renaming gave them")
                              #f))
              (else (generate-lambda (closure-lambda closure name) name closure
                                     (closure-environment closure) (closure-span closure)
                                     decline-captures?)))))
    (call-with-values (lambda () (partition generated? (map attempt closures))) cons)))

;; /**
;;  * Compiles the interpreted procedures already in an environment, replacing
;;  * each in place.
;;  *
;;  * The standard library is loaded and interpreted before anything considers
;;  * compiling it, so it cannot be compiled from source; a closure keeps its
;;  * parameters, body and environment, so it can be compiled afterwards
;;  * instead. `memq`, `assq`, `map` and `assoc` are themselves Scheme, and a
;;  * compiled procedure calling one interpreted crosses into the interpreter on
;;  * what is often its hottest path: on the compiler's own lowering, compiling
;;  * the library too was worth 10.8x. Callers pick up each compiled procedure
;;  * without being compiled again: they hold the closure, which runs compiled.
;;  *
;;  * @param {object} env - The environment.
;;  * @param {boolean} strict? - As for `generate-environment`.
;;  * @returns {pair|boolean} `(compiled . declined)`, or #f if generating code
;;  *   is not permitted here, so everything stays interpreted.
;;  */
(define (compile-environment env strict?)
  (and (code-generation-allowed?)
       (let* ((generation (generate-environment env #f #f strict?))
              (codes (car generation))
              (outcomes (map instantiate-generated codes))
              (replaced (filter-map (lambda (code outcome)
                                      (and (compiled? outcome)
                                           (cons (generated-closure code) (compiled-procedure outcome))))
                                    codes outcomes)))
         ;; Each closure runs compiled, staying the object whatever holds it
         ;; has -- a library that imported it, a value made with it -- and is
         ;; recorded for a debugger to run as itself.
         (for-each (lambda (pair) (run-compiled! (car pair) (cdr pair))) replaced)
         (record-compiled-over! replaced env)
         (cons (filter compiled? outcomes) (append (cdr generation) (remove compiled? outcomes))))))

;; ---------------------------------------------------------------------------
;; A program
;; ---------------------------------------------------------------------------

;; /**
;;  * What running one top-level form of a program came to.
;;  * @property {symbol} kind - `procedure` for a procedure definition,
;;  *   `expression` for a form compiled as a thunk and called, `interpreted`
;;  *   for one the interpreter ran.
;;  * @property {compiled|declined} outcome - What compiling it came to.
;;  * @property {*} value - Its value; undefined for a definition.
;;  */
(define-record-type step
  (make-step kind outcome value)
  step?
  (kind step-kind)
  (outcome step-outcome)
  (value step-value))

;; /**
;;  * What compiling a program came to.
;;  * @property {list} compiled - The procedures compiled, by name.
;;  * @property {list} declined - Each definition declined.
;;  * @property {list} unsafe - What the capture rule declined, as (name .
;;  *   reason) with the name a symbol; empty unless it was asked for.
;;  * @property {integer} expressions - How many forms were compiled as thunks.
;;  * @property {*} value - The last form's value.
;;  */
(define-record-type program-run
  (make-program-run compiled declined unsafe expressions value)
  program-run?
  (compiled program-run-compiled)
  (declined program-run-declined)
  (unsafe program-run-unsafe)
  (expressions program-run-expressions)
  (value program-run-value))

;; /**
;;  * Runs one top-level form, compiling it first where that is worth it.
;;  *
;;  * A procedure definition runs as the interpreter runs it, and is then
;;  * compiled from the closure that made, so that the pair is recorded: a
;;  * debugger runs the closure while the program is debugged, and a procedure
;;  * whose continuations are re-entered is switched back to it for good. Any
;;  * other form is compiled as a thunk and called once where
;;  * `compile-expression` accepts it, and run by the interpreter otherwise.
;;  *
;;  * @param {object} node - The analyzed form.
;;  * @param {object} env - The program's environment.
;;  * @param {object} interpreter - The interpreter.
;;  * @param {list} unsafe - What the capture rule declined.
;;  * @param {boolean} decline-captures? - Whether to decline a capture.
;;  * @returns {step}
;;  */
(define (run-top-level node env interpreter unsafe decline-captures?)
  (let* ((form (ast->scheme node))
         (name (and (eq? (ast-tag form) 'define) (symbol->string (ast-1 form))))
         (ruled-out (and name (assq (ast-1 form) unsafe))))
    (cond
      ((procedure-definition? form)
       (run-form interpreter node env)
       (let* ((closure (environment-value env name))
              (outcome (if ruled-out
                           (make-declined name (cdr ruled-out) #f)
                           (compile-closure closure name decline-captures?))))
         (if (compiled? outcome)
             (let ((procedure (compiled-procedure outcome)))
               (run-compiled! closure procedure)
               (record-compiled-over! (list (cons closure procedure)) env)))
         (make-step 'procedure outcome js-undefined)))
      (else
       (let ((outcome (if ruled-out
                          (make-declined name (cdr ruled-out) #f)
                          (compile-expression-form (if name (ast-2 form) form) env
                                                   (if name (definition-span node) (ast-span node))
                                                   name decline-captures?))))
         (cond
           ((not (compiled? outcome)) (make-step 'interpreted outcome (run-form interpreter node env)))
           (name
            (environment-define! env name (run-thunk interpreter env (compiled-procedure outcome)))
            (make-step 'expression outcome js-undefined))
           (else
            (make-step 'expression outcome (run-thunk interpreter env (compiled-procedure outcome))))))))))

;; /**
;;  * Runs a program's top-level forms in order, compiling what is worth it as
;;  * it goes: in one pass, so the forms run in the program's order.
;;  *
;;  * @param {vector} nodes - The analyzed top-level forms, in order.
;;  * @param {object} env - The environment to define into.
;;  * @param {object} interpreter - The interpreter.
;;  * @param {boolean} decline-captures? - As for `generate-environment`,
;;  *   decided over the whole program before anything runs, since the answer
;;  *   for one procedure depends on what its callees do.
;;  * @param {boolean} strict? - As for `generate-environment`.
;;  * @returns {program-run}
;;  */
(define (compile-program nodes env interpreter decline-captures? strict?)
  (let* ((nodes (vector->list nodes))
         (unsafe (if decline-captures? (unsafe-definitions (map ast->scheme nodes) env strict?) '()))
         (steps (let run ((nodes nodes) (steps '()))
                  (if (null? nodes)
                      (reverse steps)
                      (run (cdr nodes)
                           (cons (run-top-level (car nodes) env interpreter unsafe decline-captures?)
                                 steps)))))
         (outcomes (map step-outcome steps)))
    (make-program-run
      (map compiled-name
           (filter compiled? (map step-outcome (filter (lambda (s) (eq? (step-kind s) 'procedure)) steps))))
      (filter (lambda (o) (and (declined? o) (declined-name o))) outcomes)
      unsafe
      (count (lambda (s) (eq? (step-kind s) 'expression)) steps)
      (if (null? steps) js-undefined (step-value (last steps))))))
