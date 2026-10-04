;; A rule for declining procedures a capture could unwind through, applied
;; only on request (`decline-captures?`).
;;
;; By default every procedure is compiled, those that capture included, and
;; one whose saved frames continuations keep re-entering is switched back to
;; its interpreted closure as the program runs (`note-resume` in `tier.scm`).
;; This rule is what the tier did before that, kept because it measures the
;; trade-off: it wins where captures are frequent or re-entered -- `btsearch`
;; 4.5x, `fibc` 1.8x, `ctak` 1.1-1.2x -- and loses where a capture is an escape
;; taken now and then -- `quicksort` 21x, `puzzle` 4x, `maze` 3.8x, `contfib`
;; 2.9x, `threads` 1.35x -- which is nearly every capture in real libraries
;; (docs/corpus_decline_results.md).
;;
;; It was once a correctness rule. A compiled procedure's frame could not be
;; put into a continuation, so a capture beneath one dropped what it had left
;; to do, and the program got a wrong answer: `btsearch`'s `enumerate`, which
;; backtracking re-enters, and `maze`'s `make-maze`, which an escape from
;; `dig-maze` unwinds past. Neither names `call/cc`, which is why the rule
;; follows the call graph rather than what a procedure names. Compiled frames
;; save themselves now (src/core/interpreter/unwind.js), so what is left is
;; the cost of saving and resuming them.
;;
;; A procedure is declined if it can reach a control-transferring global or a
;; capture: itself, through another procedure being decided with it, or
;; through an interpreted closure already in the environment, which is how a
;; standard library procedure that captures is found. With `strict?`, one that
;; calls a procedure it was handed is declined too, since it cannot know what
;; that does; that was once the only way to catch `btsearch`, where the capture
;; arrives as an argument, and it declines most higher-order code for it.
;;
;; A global rebound after compilation to something that captures is not
;; caught, and need not be: compiled code calls the new binding, and its
;; capture is handled like any other.

;; /**
;;  * What a procedure references, as its lowering reports it.
;;  * @property {list} globals - The globals it references, as symbols.
;;  * @property {boolean} calls-unknown? - Whether it calls a callee it cannot
;;  *   name.
;;  * @property {symbol|boolean} control - A control global it names, or #f.
;;  * @property {boolean} captures? - Whether it captures a continuation.
;;  */
(define-record-type facts
  (make-facts globals calls-unknown? control captures?)
  facts?
  (globals facts-globals)
  (calls-unknown? facts-calls-unknown?)
  (control facts-control)
  (captures? facts-captures?))

;; /**
;;  * The facts of a lambda, or #f if it cannot be lowered: a procedure that
;;  * cannot be vetted is taken to be safe, as every primitive is.
;;  * @param {list} node - The lambda, as `ir.scm` reads it.
;;  * @returns {facts|boolean}
;;  */
(define (lambda-facts node)
  (let ((lowered (lower-lambda node)))
    (and (lowered-lambda? lowered)
         (let ((globals (lowered-globals lowered)))
           (make-facts globals (lowered-calls-unknown? lowered)
                       (control-global-in globals) (lowered-captures? lowered))))))

;; /**
;;  * The facts of an interpreted closure, or #f.
;;  * @param {procedure} closure - The closure.
;;  * @param {symbol} name - Its name.
;;  * @returns {facts|boolean}
;;  */
(define (closure-facts closure name)
  (lambda-facts (closure-lambda closure (symbol->string name))))

;; /**
;;  * Looks up the facts of whatever an environment binds a name to: an
;;  * interpreted closure's, and #f for anything else -- a primitive not on the
;;  * control list, a procedure already compiled, a name not yet bound.
;;  * @param {object} env - The environment.
;;  * @returns {procedure} From a name, as a symbol, to facts or #f.
;;  */
(define (environment-facts env)
  (lambda (name)
    (let ((value (environment-value env (symbol->string name))))
      (and (interpreted-closure? value) (closure-facts value name)))))

;; /**
;;  * Decides which of some procedures a capture could unwind through.
;;  *
;;  * Outside the procedures being decided, each name is looked into once and
;;  * the verdict kept, so the standard library is walked once rather than once
;;  * per reference; a cycle there is not itself evidence.
;;  *
;;  * @param {list} local - The procedures being decided, as (name . facts).
;;  * @param {procedure} external - From a name outside them to its facts, or #f.
;;  * @param {boolean} strict? - Also decline calling a procedure handed in.
;;  * @returns {list} Those declined, as (name . reason), in `local`'s order;
;;  *   each reason a path the reader can follow to a control global.
;;  */
(define (unsafe-from-facts local external strict?)
  (define verdicts '())

  (define (outside-reason name visiting)
    (cond
      ((assq name verdicts) => cdr)
      ((memq name visiting) #f)
      (else
       (let* ((facts (external name))
              (reason (and facts (outside-facts-reason name facts (cons name visiting)))))
         (set! verdicts (cons (cons name reason) verdicts))
         reason))))

  (define (outside-facts-reason name facts visiting)
    (let ((label (symbol->string name)))
      (cond
        ((facts-control facts)
         => (lambda (control) (string-append label " references '" (symbol->string control) "'")))
        ((facts-captures? facts) (string-append label " captures a continuation"))
        ((and strict? (facts-calls-unknown? facts)) (string-append label " calls a procedure it is given"))
        (else (any (lambda (g)
                     (let ((inner (outside-reason g visiting)))
                       (and inner (string-append label " -> " inner))))
                   (facts-globals facts))))))

  (define (own-reason facts)
    (cond
      ((facts-control facts)
       => (lambda (control) (string-append "references control global '" (symbol->string control) "'")))
      ((facts-captures? facts) "captures a continuation, which costs more compiled than interpreted")
      ((and strict? (facts-calls-unknown? facts))
       "calls a procedure it is given, which may capture a continuation")
      (else (any (lambda (g)
                   (and (not (assq g local))
                        (let ((reason (outside-reason g '())))
                          (and reason (string-append "reaches " reason)))))
                 (facts-globals facts)))))

  ;; A caller of a declined procedure is declined, until none is left to add.
  (define (spread unsafe)
    (let ((more (filter-map
                  (lambda (entry)
                    (and (not (assq (car entry) unsafe))
                         (let ((callee (find (lambda (g) (assq g unsafe)) (facts-globals (cdr entry)))))
                           (and callee
                                (cons (car entry)
                                      (string-append "reaches " (symbol->string callee) ", which "
                                                     (cdr (assq callee unsafe))))))))
                  local)))
      (if (null? more) unsafe (spread (append unsafe more)))))

  (let ((unsafe (spread (filter-map (lambda (entry)
                                      (let ((reason (own-reason (cdr entry))))
                                        (and reason (cons (car entry) reason))))
                                    local))))
    (filter-map (lambda (entry) (assq (car entry) unsafe)) local)))

;; /**
;;  * Decides which of a program's procedure definitions a capture could unwind
;;  * through.
;;  * @param {list} forms - The program's analyzed forms, as `ir.scm` reads them.
;;  * @param {object} env - The environment they are defined into.
;;  * @param {boolean} strict? - As for `unsafe-from-facts`.
;;  * @returns {list} Those declined, as (name . reason) with the name a symbol.
;;  */
(define (unsafe-definitions forms env strict?)
  (unsafe-from-facts
    (filter-map (lambda (form)
                  (and (procedure-definition? form)
                       (let ((facts (lambda-facts (ast-2 form))))
                         (and facts (cons (ast-1 form) facts)))))
                forms)
    (environment-facts env)
    strict?))

;; /**
;;  * `unsafe-definitions`, for a program's analyzed forms as the interpreter
;;  * made them.
;;  * @param {vector} nodes - The forms.
;;  * @returns {list}
;;  */
(define (program-unsafe-definitions nodes env strict?)
  (unsafe-definitions (map ast->scheme (vector->list nodes)) env strict?))

;; /**
;;  * Decides which of some interpreted closures a capture could unwind through:
;;  * the question `unsafe-definitions` asks, for procedures that exist as
;;  * values, as a library's do once it has loaded.
;;  * @param {list} closures - The closures, as (name . closure) with the name a
;;  *   symbol.
;;  * @param {object} env - The environment they live in.
;;  * @param {boolean} strict? - As for `unsafe-from-facts`.
;;  * @returns {list} Those declined, as (name . reason).
;;  */
(define (unsafe-closures closures env strict?)
  (unsafe-from-facts
    (filter-map (lambda (entry)
                  (let ((facts (closure-facts (cdr entry) (car entry))))
                    (and facts (cons (car entry) facts))))
                closures)
    (environment-facts env)
    strict?))
