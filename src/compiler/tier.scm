;; Compiling a program's own code as it runs.
;;
;; The standard library is compiled at build time; a program's own procedures
;; are compiled by a tier attached to its interpreter (src/compiler/tiering.js
;; attaches it). The interpreter tells the tier two things -- a closure has
;; been bound to a top-level name, by `define` or `set!`, and a waiting
;; closure's calls have run out -- and asks it one: whether to run a top-level
;; form compiled. This file answers. The interpreter holds the tier's record
;; and calls the procedure in it for each, directly, with compiled frames kept
;; from moving (`callSchemeProcedure` in src/core/interpreter/values.js): so
;; the tier's Scheme runs compiled, or in the compiler's own interpreter, and
;; never where the program's debugger could pause it.
;;
;; ## When
;;
;; Compiling a procedure that runs once costs more than interpreting it: a
;; small one takes 0.1 to 0.2 ms to compile, and its one call microseconds to
;; interpret. Compiling every definition as it is made measured 0.5 to 2.5%
;; slower over `benchmarks/run_tier.js`'s programs, never faster. So:
;;
;;  - A top-level procedure whose body loops or makes procedures is compiled
;;    when it is bound. A loop inside a procedure called once is where a
;;    program spends its time, and a count of calls could never catch it.
;;  - Any other is compiled on its second call, which runs compiled, so a
;;    procedure called once is never compiled.
;;  - A top-level expression is compiled only if it loops.
;;
;; Except while the program is debugged in DevTools, which steps only into
;; compiled code -- the interpreter is the system's own code, which it skips
;; -- and so would pass over a procedure's first call. With the tier set to
;; compile eagerly (`tier-compile-eagerly!`), every procedure is compiled as
;; it is bound, a library's at its first call once the library has loaded,
;; and every top-level form but a definition; turned on as the program runs,
;; the procedures it has bound at top level are compiled at once, and one
;; waiting elsewhere at its next call. The call that compiles a procedure
;; runs compiled. The host sets it: a page from its URL or a call in the
;; console, the CLI when Node's inspector is on.
;;
;; Switching needs no on-stack replacement. Both tiers look a top-level name up
;; at every call, so once the compiled procedure is bound, the next call -- a
;; recursive call below frames already made, or the next iteration of a loop
;; written as a tail call -- runs compiled, and the frames already made finish
;; interpreted. A procedure nested in a top-level one is compiled with it.
;;
;; ## Over the closure, for the debugger
;;
;; Each procedure is compiled from the interpreted closure the program made,
;; and the pair is recorded, so that while the program is being debugged it
;; runs as that closure again and its breakpoints fire, as the standard
;; library's do. Nothing is compiled while the program is being debugged; a
;; procedure that would have been is compiled on its first call after.
;;
;; Nor while a library is being loaded: the compiler's own definitions would
;; be registered with the scopes that library's macros resolve their free
;; identifiers through. So a library's procedures wait for calls once it has
;; loaded, however they loop -- ten, since most of what a library defines is
;; not hot in any one program (`library-calls-before-compiling`). An expression compiled
;; as a thunk has no closure to go back to, which is why only loops are: a
;; procedure such a loop makes and keeps stays compiled while debugging.

;; /**
;;  * How many calls a procedure that neither loops nor makes procedures waits
;;  * before it is compiled. A variable, so that `benchmarks/run_tier.js` can
;;  * measure another count.
;;  * @type {integer}
;;  */
(define calls-before-compiling 2)

;; /**
;;  * Whether a top-level procedure is compiled as soon as it is bound rather
;;  * than after `calls-before-compiling` calls: when its body loops or makes
;;  * procedures. A procedure of its own, like the count a variable, so that
;;  * `benchmarks/run_tier.js` can measure another rule in its place.
;;  * @param {list} body - The procedure's analyzed body.
;;  * @returns {boolean}
;;  */
(define (compiled-when-bound? body) (makes-procedures-or-loops? body))

;; /**
;;  * How many calls a procedure a library defines waits, once the library has
;;  * loaded, before it is compiled, however it loops: compiling waits while a
;;  * library loads (see the notes at the head of this file).
;;  *
;;  * Ten, measured over the test programs of 22 libraries that are not
;;  * shipped (`benchmarks/run_tier.js --set corpus`): compiled at their first
;;  * call, as they were, 21 of the programs ran slower with the tier than
;;  * without, compiling half the time; at their tenth, 28% faster, none more
;;  * than 15% slower. At their hundredth they ran faster still, but a program
;;  * whose library's loops were hot ran 31% slower. Keeping the program's own
;;  * rule, compiling at the first call what loops or makes procedures, kept
;;  * only 5-7%: most procedures a library defines do one or the other. A
;;  * variable, so that `benchmarks/run_tier.js` can measure another count.
;;  * @type {integer}
;;  */
(define library-calls-before-compiling 10)

;; /**
;;  * The compiler tier of one program.
;;  * @property {object} interpreter - The program's interpreter.
;;  * @property {object} env - The program's global environment.
;;  * @property {procedure} prebuilt? - From a library's name to whether its
;;  *   procedures are compiled at build time, and so are not the tier's.
;;  * @property {boolean} decline-captures? - Decline what a capture could unwind
;;  *   through (`safety.scm`), as the tier once did.
;;  * @property {object} waiting - Each closure waiting to be compiled, weakly,
;;  *   mapped to (name . environment) for the binding it waits under.
;;  * @property {object} outcomes - A JavaScript `Map` from each name the tier
;;  *   tried to "compiled" or why not, for whoever attached it to read.
;;  * @property {integer} expressions - How many top-level forms it compiled.
;;  * @property {boolean} eager - Whether it compiles every procedure as it is
;;  *   bound, for a debugger that steps only into compiled code; the
;;  *   interpreter reads it, to find a waiting closure due at any call.
;;  * @property {procedure} bound - What the interpreter calls when a closure is
;;  *   bound to a top-level name: `tier-bound!` on this tier.
;;  * @property {procedure} due - What it calls when a waiting closure's calls
;;  *   have run out: `tier-due!` on this tier.
;;  * @property {procedure} form - What it asks for each top-level form, and its
;;  *   environment: `tier-top-level-procedure` on this tier.
;;  */
(define-record-type tier
  (make-tier-record interpreter env prebuilt? decline-captures? waiting outcomes expressions
                    eager bound due form)
  tier?
  (interpreter tier-interpreter)
  (env tier-env)
  (prebuilt? tier-prebuilt-test)
  (decline-captures? tier-declines-captures?)
  (waiting tier-waiting)
  (outcomes tier-outcomes)
  (expressions tier-expressions set-tier-expressions!)
  (eager tier-eager? set-tier-eager!)
  (bound tier-bound-hook)
  (due tier-due-hook)
  (form tier-form-hook))

;; /**
;;  * Makes a program's tier, and takes on the procedures the program has
;;  * already bound at top level as if they were bound now.
;;  * @param {object} interpreter - The program's interpreter.
;;  * @param {object} env - Its global environment.
;;  * @param {procedure} prebuilt? - As for the record.
;;  * @param {boolean} decline-captures? - As for the record.
;;  * @param {object} outcomes - As for the record.
;;  * @param {boolean} eager? - Whether to compile every procedure as it is
;;  *   bound, as for the record.
;;  * @returns {tier|boolean} The tier, or #f if code cannot be generated here --
;;  *   a Content-Security-Policy forbids it -- and the program runs interpreted.
;;  */
(define (make-tier interpreter env prebuilt? decline-captures? outcomes eager?)
  (and (code-generation-allowed?)
       (letrec ((tier (make-tier-record
                       interpreter env prebuilt? decline-captures? (make-weak-table) outcomes 0 eager?
                       (lambda (name closure env) (tier-bound! tier name closure env))
                       (lambda (closure) (tier-due! tier closure))
                       (lambda (node env) (tier-top-level-procedure tier node env)))))
         (for-each (lambda (binding)
                     (let ((value (cdr binding)))
                       (if (and (interpreted-closure? value) (programs-own? value env))
                           (tier-bound! tier (car binding) value env))))
                   (environment-bindings env))
         tier)))

;; /**
;;  * Whether a closure is a program's own: made in its global environment or a
;;  * scope inside it, and not inside a library's, whose environment is inside
;;  * the global one too.
;;  * @param {procedure} closure - An interpreted closure.
;;  * @param {object} env - The program's global environment.
;;  * @returns {boolean}
;;  */
(define (programs-own? closure env)
  (let walk ((scope (closure-environment closure)))
    (and scope
         (or (eq? scope env)
             (and (not (environment-library scope)) (walk (environment-parent scope)))))))

;; /**
;;  * Whether an environment is a program's top level: the global environment
;;  * the tier was attached to, or one of import sets alone that is not a
;;  * library's -- a program's that began with import declarations, which sees
;;  * only them, or one `environment` made.
;;  * @param {tier} tier - The tier.
;;  * @param {object} env - The environment.
;;  * @returns {boolean}
;;  */
(define (program-environment? tier env)
  (or (eq? env (tier-env tier))
      (and (environment-strict? env) (not (environment-library env)))))

;; /**
;;  * Whether the procedures an environment binds at top level are the tier's:
;;  * the program's own, or those of a library the program loaded that has no
;;  * prebuilt table.
;;  * @param {tier} tier - The tier.
;;  * @param {object|boolean} env - The environment binding a name, or #f.
;;  * @returns {boolean}
;;  */
(define (tier-manages? tier env)
  (and env
       (or (program-environment? tier env)
           (let ((library (environment-library env)))
             (and library (not ((tier-prebuilt-test tier) library)))))))

;; /**
;;  * Whether compiling must wait: while the program is being debugged, or a
;;  * library is being loaded.
;;  * @param {tier} tier - The tier.
;;  * @returns {boolean}
;;  */
(define (tier-deferring? tier)
  (or (debugging? (tier-interpreter tier)) (library-loading?)))

;; /**
;;  * A closure has been bound to a top-level name: compiled now if its body
;;  * loops or makes procedures, or the tier compiles eagerly, and set to wait
;;  * for its second call otherwise. While a library loads, nothing can be
;;  * compiled; compiling eagerly, its procedures wait for one call.
;;  * @param {tier} tier - The tier.
;;  * @param {string} name - The name.
;;  * @param {procedure} closure - The closure.
;;  * @param {object|boolean} env - The environment that binds the name, or #f.
;;  */
(define (tier-bound! tier name closure env)
  (when (tier-manages? tier env)
    (weak-table-set! (tier-waiting tier) closure (cons name env))
    (cond ((debugging? (tier-interpreter tier)) (wait-calls! closure 1))
          ((library-loading?)
           (wait-calls! closure (if (tier-eager? tier) 1 library-calls-before-compiling)))
          ((or (tier-eager? tier) (compiled-when-bound? (closure-body closure)))
           (tier-compile! tier closure))
          (else (wait-calls! closure calls-before-compiling)))))

;; /**
;;  * A waiting closure's calls have run out. While compiling must wait, it
;;  * waits for one more call.
;;  * @param {tier} tier - The tier.
;;  * @param {procedure} closure - The closure.
;;  */
(define (tier-due! tier closure)
  (if (tier-deferring? tier)
      (wait-calls! closure 1)
      (tier-compile! tier closure)))

;; /**
;;  * Turns compiling every procedure as it is bound on or off. Turned on, the
;;  * procedures the program has bound at top level that wait to be compiled
;;  * are compiled now, so that a debugger can bind breakpoints in them before
;;  * they run: compiled by the call that hits one, the code would arrive too
;;  * late for it. One bound elsewhere -- in a library, or in a program of its
;;  * own imports -- is compiled at its next call, which runs compiled.
;;  * @param {tier} tier - The tier.
;;  * @param {boolean} eager? - Whether to.
;;  */
(define (tier-compile-eagerly! tier eager?)
  (set-tier-eager! tier eager?)
  (when (and eager? (not (tier-deferring? tier)))
    (for-each (lambda (binding)
                (let ((value (cdr binding)))
                  (if (and (procedure? value) (weak-table-ref (tier-waiting tier) value))
                      (tier-compile! tier value))))
              (environment-bindings (tier-env tier)))))

;; /**
;;  * Compiles a waiting closure and binds the compiled procedure in its place,
;;  * if its name still holds it and the compiler accepts it, recording the
;;  * outcome under the name.
;;  * @param {tier} tier - The tier.
;;  * @param {procedure} closure - The closure.
;;  */
(define (tier-compile! tier closure)
  (let ((entry (weak-table-ref (tier-waiting tier) closure)))
    (weak-table-delete! (tier-waiting tier) closure)
    (wait-calls! closure 0)
    ;; A name rebound since belongs to another closure, with its own count.
    (when (and entry (eq? (environment-value (cdr entry) (car entry)) closure))
      (let* ((name (car entry))
             (env (cdr entry))
             (outcome (compile-waiting tier closure name env)))
        (when (compiled? outcome) (install-compiled! tier closure (compiled-procedure outcome) name env))
        (js-invoke (tier-outcomes tier) "set" name
                   (if (compiled? outcome) "compiled" (declined-reason outcome)))))))

;; /**
;;  * Compiles a closure the tier was waiting on: over the closure, and captures
;;  * included unless the tier declines them. One whose continuations are
;;  * re-entered over and over is switched back as the program runs
;;  * (`note-resume`).
;;  * @returns {compiled|declined}
;;  */
(define (compile-waiting tier closure name env)
  (let ((ruled-out (and (tier-declines-captures? tier)
                        (unsafe-closures (list (cons (string->symbol name) closure)) env #f))))
    (if (pair? ruled-out)
        (make-declined name (cdar ruled-out) #f)
        (compile-closure closure name (tier-declines-captures? tier)))))

;; /**
;;  * Makes a closure run as what it compiled to, staying the object every
;;  * holder of it has -- the name that binds it, any other name, a library
;;  * that imported it, the program's data -- and records it for a debugger.
;;  * @param {tier} tier - The tier.
;;  * @param {procedure} closure - The closure.
;;  * @param {procedure} procedure - What it compiled to.
;;  * @param {string} name - The name that binds it.
;;  * @param {object} env - The environment that binds it.
;;  */
(define (install-compiled! tier closure procedure name env)
  (run-compiled! closure procedure)
  (record-compiled-over! (list (cons closure procedure)) (tier-env tier)))

;; /**
;;  * The compiled procedure to run a top-level form as, or #f to interpret it.
;;  *
;;  * Only a form that loops, unless the tier compiles eagerly. One that only
;;  * makes procedures is interpreted: the procedures it binds are compiled when
;;  * bound, over their closures, and one it makes and keeps elsewhere would,
;;  * compiled here, have no closure for the REPL's debugger to go back to --
;;  * which compiling eagerly, for DevTools, leaves aside. Definitions run
;;  * interpreted, which is where the tier sees what they bind.
;;  *
;;  * @param {tier} tier - The tier.
;;  * @param {object} node - The analyzed form.
;;  * @param {object} env - The environment it is to run in.
;;  * @returns {procedure|boolean} A thunk, or #f.
;;  */
(define (tier-top-level-procedure tier node env)
  (and (program-environment? tier env)
       (not (tier-deferring? tier))
       (let ((form (ast->scheme node)))
         (and (not (defines-at-top-level? form))
              (or (tier-eager? tier) (contains-loop? form))
              (let ((outcome (if (tier-eager? tier)
                                 (compile-thunk form env (ast-span node) (tier-declines-captures? tier))
                                 (compile-expression-form form env (ast-span node) #f
                                                          (tier-declines-captures? tier)))))
                (and (compiled? outcome)
                     (begin
                       (set-tier-expressions! tier (+ (tier-expressions tier) 1))
                       (compiled-procedure outcome))))))))

;; ---------------------------------------------------------------------------
;; Procedures whose saved frames are re-entered
;; ---------------------------------------------------------------------------
;;
;; A procedure is compiled whether or not a continuation may be captured
;; through it: nearly every capture in real code is an escape, taken now and
;; then, and each saved frame is resumed once, which compiled code pays for
;; easily. What costs more compiled than interpreted is a continuation
;; re-entered over and over, as a backtracking search re-enters its choice
;; points: every re-entry resumes each compiled frame in it through its
;; resumable form. So the runtime counts each procedure's frames as they are
;; saved and as they are resumed (src/core/interpreter/unwind.js), and asks
;; here whether to switch the procedure back for good to the interpreted
;; closure it was compiled from -- and, when not, at which resume to ask
;; again, since a frame is resumed at almost every call in some programs and
;; the question need not be asked that often.
;;
;; The thresholds are set by the programs that capture: `btsearch` resumes its
;; frames 200 times for each save, and every program for which compiling its
;; captures pays -- `quicksort`, `puzzle`, `maze`, `contfib`, `threads` --
;; exactly once. A frame moved to the heap to make room on the JavaScript
;; stack is saved and resumed once too, so deep recursion is not mistaken for
;; re-entry.

;; /**
;;  * How many resumes a procedure's frames take before it may be switched back:
;;  * enough that a few re-entries of one continuation do not do it.
;;  * @type {integer}
;;  */
(define reentry-minimum 1024)

;; /**
;;  * How many resumes per save mark a procedure's frames as re-entered.
;;  * @type {integer}
;;  */
(define reentry-ratio 4)

;; /**
;;  * Whether a procedure's frames are re-entered, from how often they have been
;;  * saved and resumed.
;;  * @param {number} saved - Saves.
;;  * @param {number} resumed - Resumes.
;;  * @returns {boolean}
;;  */
(define (re-entered? saved resumed)
  (and (>= resumed reentry-minimum) (>= resumed (* reentry-ratio saved))))

;; /**
;;  * The first resume at which a procedure not yet re-entered could be. Saves
;;  * only grow, so its frames cannot count as re-entered before their resumes
;;  * reach both the minimum and the ratio times the saves so far.
;;  * @param {number} saved - Saves so far.
;;  * @param {number} resumed - Resumes so far.
;;  * @returns {number}
;;  */
(define (next-resume-to-ask saved resumed)
  (max (+ resumed 1) reentry-minimum (* reentry-ratio saved)))

;; /**
;;  * The first resume at which to ask about a procedure.
;;  * @returns {number}
;;  */
(define (first-resume-to-ask) (next-resume-to-ask 0 0))

;; /**
;;  * A procedure's saved frame is being resumed: switches the procedure back if
;;  * its frames are re-entered.
;;  * @param {procedure} twin - The procedure's resumable form, which the frame
;;  *   carries.
;;  * @param {number} saved - Its frames' saves so far.
;;  * @param {number} resumed - Their resumes, this one included.
;;  * @returns {boolean|number} #t if it was judged re-entered, whether or not
;;  *   there was a closure to switch back to, after which the runtime does not
;;  *   ask again; otherwise the resume at which to ask next.
;;  */
(define (note-resume twin saved resumed)
  (if (re-entered? saved resumed)
      (begin (switch-back-to-closure! twin) #t)
      (next-resume-to-ask saved resumed)))
