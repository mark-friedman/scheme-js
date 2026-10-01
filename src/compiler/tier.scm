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
;; Generating a procedure's code costs about a millisecond, so compiling every
;; definition as it is made would cost a page with five hundred of them half a
;; second before anything ran, much of it for code run once. So:
;;
;;  - A top-level procedure whose body loops or makes procedures is compiled
;;    when it is bound. A loop inside a procedure called once is where a
;;    program spends its time, and a count of calls could never catch it.
;;  - Any other is compiled on its second call, so a procedure called once is
;;    never compiled.
;;  - A top-level expression is compiled only if it loops.
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
;; identifiers through. So a library's procedures are compiled from their
;; first call once it has loaded, however they loop. An expression compiled
;; as a thunk has no closure to go back to, which is why only loops are: a
;; procedure such a loop makes and keeps stays compiled while debugging.

;; /**
;;  * How many calls a procedure that neither loops nor makes procedures waits
;;  * before it is compiled.
;;  * @type {integer}
;;  */
(define calls-before-compiling 2)

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
;;  * @property {procedure} bound - What the interpreter calls when a closure is
;;  *   bound to a top-level name: `tier-bound!` on this tier.
;;  * @property {procedure} due - What it calls when a waiting closure's calls
;;  *   have run out: `tier-due!` on this tier.
;;  * @property {procedure} form - What it asks for each top-level form, and its
;;  *   environment: `tier-top-level-procedure` on this tier.
;;  */
(define-record-type tier
  (make-tier-record interpreter env prebuilt? decline-captures? waiting outcomes expressions
                    bound due form)
  tier?
  (interpreter tier-interpreter)
  (env tier-env)
  (prebuilt? tier-prebuilt-test)
  (decline-captures? tier-declines-captures?)
  (waiting tier-waiting)
  (outcomes tier-outcomes)
  (expressions tier-expressions set-tier-expressions!)
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
;;  * @returns {tier|boolean} The tier, or #f if code cannot be generated here --
;;  *   a Content-Security-Policy forbids it -- and the program runs interpreted.
;;  */
(define (make-tier interpreter env prebuilt? decline-captures? outcomes)
  (and (code-generation-allowed?)
       (letrec ((tier (make-tier-record
                       interpreter env prebuilt? decline-captures? (make-weak-table) outcomes 0
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
;;  * Whether the procedures an environment binds at top level are the tier's:
;;  * the program's own, or those of a library the program loaded that has no
;;  * prebuilt table.
;;  * @param {tier} tier - The tier.
;;  * @param {object|boolean} env - The environment binding a name, or #f.
;;  * @returns {boolean}
;;  */
(define (tier-manages? tier env)
  (and env
       (or (eq? env (tier-env tier))
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
;;  * loops or makes procedures, and set to wait for its second call otherwise.
;;  * @param {tier} tier - The tier.
;;  * @param {string} name - The name.
;;  * @param {procedure} closure - The closure.
;;  * @param {object|boolean} env - The environment that binds the name, or #f.
;;  */
(define (tier-bound! tier name closure env)
  (when (tier-manages? tier env)
    (weak-table-set! (tier-waiting tier) closure (cons name env))
    (cond ((tier-deferring? tier) (wait-calls! closure 1))
          ((makes-procedures-or-loops? (closure-body closure)) (tier-compile! tier closure))
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
;;  * Binds a compiled procedure wherever its closure was bound: under its name,
;;  * under any other name in the program holding the closure, and -- for a
;;  * library's procedure -- in every library and program that imported it,
;;  * since an import copies the value.
;;  * @param {tier} tier - The tier.
;;  * @param {procedure} closure - The closure.
;;  * @param {procedure} procedure - What it compiled to.
;;  * @param {string} name - The name that binds it.
;;  * @param {object} env - The environment that binds it.
;;  */
(define (install-compiled! tier closure procedure name env)
  (let ((program (tier-env tier))
        (replaced (list (cons closure procedure))))
    (environment-rebind! env name procedure)
    (for-each (lambda (binding)
                (if (eq? (cdr binding) closure) (environment-rebind! program (car binding) procedure)))
              (environment-bindings program))
    (if (not (eq? env program)) (substitute-library-values! replaced))
    (record-compiled-over! replaced program)))

;; /**
;;  * The compiled procedure to run a top-level form as, or #f to interpret it.
;;  *
;;  * Only a form that loops. One that only makes procedures is interpreted: the
;;  * procedures it binds are compiled when bound, over their closures, and one
;;  * it makes and keeps elsewhere would, compiled here, have no closure for a
;;  * debugger to go back to. Definitions run interpreted, which is where the
;;  * tier sees what they bind.
;;  *
;;  * @param {tier} tier - The tier.
;;  * @param {object} node - The analyzed form.
;;  * @param {object} env - The environment it is to run in.
;;  * @returns {procedure|boolean} A thunk, or #f.
;;  */
(define (tier-top-level-procedure tier node env)
  (and (eq? env (tier-env tier))
       (not (tier-deferring? tier))
       (let ((form (ast->scheme node)))
         (and (not (defines-at-top-level? form))
              (contains-loop? form)
              (let ((outcome (compile-expression-form form env (ast-span node) #f
                                                      (tier-declines-captures? tier))))
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
