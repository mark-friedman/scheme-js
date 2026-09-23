;;; ir.scm -- lowering the analyzed AST to compiler IR.
;;;
;;; This is the compiler's lowering pass, and it is Scheme because the compiler
;;; is meant to end up in Scheme. `src/compiler/lowering.js` is the door into
;;; it: it marshals the analyzed AST in, calls `lower-lambda`, and marshals the
;;; IR back out for code generation, which is still JavaScript.
;;;
;;; ## Running this at a useful speed
;;;
;;; A self-hosted pass has to be compiled by the tier it is part of, and the
;;; tier has to reach the standard library too. Lowering calls `memq` and `assq`
;;; on every scope lookup and every global it records, and those are themselves
;;; Scheme; with them interpreted, this module crosses into the interpreter on
;;; its hottest path and the tier is worth 1.5x. With them compiled it is worth
;;; 20x. Both this file and the library are therefore compiled at build time
;;; into `src/packaging/`, and `lowering.js` installs that code rather than
;;; generating any.
;;;
;;; The bootstrap underneath is the interpreter, which can always run this file
;;; from source. That is what makes the chain terminate without a compiler
;;; written in another language: a checkout with no prebuilt code at all still
;;; compiles, slowly, and the first thing it produces is the code that makes it
;;; fast.
;;;
;;; ## Two constraints the language imposes
;;;
;;;   - Sets and maps here are lists, although SRFI 125 hash tables exist,
;;;     because the lists are short. Measured over the 993 lambdas in
;;;     `npm run benchmark:self-host`, a scope lookup's `assq` scans 1.85
;;;     entries on average and a `memq` on the lowering state 6.8; even
;;;     `earley:make-parser`, the deepest nesting in the corpus, averages 2.5.
;;;     A compiled hash-table lookup costs about 100 ns, which a scan that
;;;     short should never need. What the lists do cost is the compiled `assq`
;;;     and `memq` themselves: about 300 ns a call, almost none of it spent
;;;     scanning, because each call and each step of its loop goes through the
;;;     tier's trampoline. That is 39% of lowering time, and a matter for the
;;;     code the tier generates for loops, not for this file.
;;;   - Nothing here calls `apply`, `values`, or anything else that transfers
;;;     control, because a procedure that does is declined by the compiler
;;;     tier. Self-hosting means writing in the subset the tier accepts, and
;;;     this file is the first evidence of what that subset costs to work in.
;;;
;;; ## Representation
;;;
;;; An analyzed AST node is a list whose head is a tag:
;;;
;;;     (lit value) (var name) (if test then else) (seq exprs)
;;;     (lambda params rest name body) (let var init body)
;;;     (letrec names inits body) (set name value) (define name value)
;;;     (app fn args) (other description)
;;;
;;; An IR node is the same idea, with the fields the JavaScript version stores
;;; in an object, in the same order:
;;;
;;;     (const value tail) (local name tail callable) (global name tail callable)
;;;     (if test then else tail callable) (seq exprs tail callable)
;;;     (lambda params rest name body tail callable)
;;;     (let name init body tail callable) (letrec names inits body tail callable)
;;;     (set name local value tail) (define name value tail) (call fn args tail)
;;;
;;; Names are symbols, so membership tests are pointer comparisons. The
;;; JavaScript version compares strings, which V8 also makes a pointer
;;; comparison for interned strings, so neither side is favoured.
;;;
;;; Lowering failure is `#f`. No IR node is `#f` -- every one is a pair -- so
;;; the sentinel cannot be confused with a result.

;; /**
;;  * Globals whose primitives transfer control, or whose semantics involve the
;;  * continuation. A procedure that references one is left to the interpreter.
;;  */
(define control-globals
  '(call/cc call-with-current-continuation dynamic-wind
    call-with-values eval
    with-exception-handler raise raise-continuable guard
    call-with-escape-continuation exit emergency-exit
    make-parameter parameterize))

;; `values` is not here. It builds a multiple-values object and returns it,
;; which is an ordinary value; it never transfers control. `apply` is not here
;; either, although it returns a tail call rather than a value: it transfers to
;; an ordinary procedure with ordinary arguments, and a compiled trampoline can
;; continue that itself. What used to keep `apply` on the list was the shape of
;; the tail call it built -- one carrying an expression for the interpreter to
;; evaluate, which compiled code has no evaluator for. It names the procedure
;; and the arguments now.
;;
;; `call-with-values` stays, because its primitive hands the interpreter an
;; expression to evaluate. A direct two-argument call to it is rewritten during
;; lowering into calls the compiler can already make, and that rewrite records
;; no name, so reaching it by any other route still declines.

;; ---------------------------------------------------------------------------
;; Lexical scope
;; ---------------------------------------------------------------------------
;;
;; A scope is a list of frames, innermost first. A frame is a one-element
;; vector holding an association list from name to "is this bound to a lambda
;; we lowered?". The vector exists because a frame is extended in place: an
;; internal `define` adds to the scope it is already being lowered in.
;;
;; Holding both facts in one alist entry is what makes shadowing fall out for
;; free. A name bound plainly in an inner scope hides a callable binding
;; outside it, because the inner entry is found first.

;; /**
;;  * Creates a scope nested inside another.
;;  * @param {list} parent - The enclosing scope, or '() at the outermost level.
;;  * @returns {list} The new scope.
;;  */
(define (make-scope parent)
  (cons (vector '()) parent))

;; /**
;;  * Records a name as bound in the innermost frame of a scope.
;;  * @param {list} scope - The scope to extend.
;;  * @param {symbol} name - The renamed variable.
;;  * @param {boolean} callable - Whether it is bound to a lambda we lowered.
;;  * @returns {unspecified}
;;  */
(define (scope-declare! scope name callable)
  (let ((frame (car scope)))
    (vector-set! frame 0 (cons (cons name callable) (vector-ref frame 0)))))

;; /**
;;  * Finds a name's binding entry, innermost frame first.
;;  * @param {list} scope - The scope to search.
;;  * @param {symbol} name - The renamed variable.
;;  * @returns {pair|boolean} The (name . callable) entry, or #f if unbound.
;;  */
(define (scope-lookup scope name)
  (if (null? scope)
      #f
      (let ((hit (assq name (vector-ref (car scope) 0))))
        (if hit hit (scope-lookup (cdr scope) name)))))

;; /**
;;  * @param {list} scope - The scope to search.
;;  * @param {symbol} name - The renamed variable.
;;  * @returns {boolean} Whether the name is lexically bound.
;;  */
(define (scope-has? scope name)
  (if (scope-lookup scope name) #t #f))

;; /**
;;  * @param {list} scope - The scope to search.
;;  * @param {symbol} name - The renamed variable.
;;  * @returns {boolean} Whether the name is bound to a lambda we lowered.
;;  */
(define (scope-callable? scope name)
  (let ((hit (scope-lookup scope name)))
    (if hit (cdr hit) #f)))

;; ---------------------------------------------------------------------------
;; Lowering state
;; ---------------------------------------------------------------------------
;;
;; One mutable record threaded through the whole traversal, as a vector:
;;
;;   0  globals           symbols referenced free anywhere in the form
;;   1  calls-unknown?    whether any call has a callee this pass cannot name
;;   2  called-locals     locals that were called, having been bound to a lambda
;;   3  assigned-locals   locals that are the target of a set!
;;   4  reason            why lowering failed, or #f
;;   5  synthesized       counter for names this pass invents rather than reads
;;   6  captures?         whether the form captures a continuation itself
;;   7  suspends?         whether it has a point it can be suspended at

;; /**
;;  * Creates an empty lowering state.
;;  * @returns {vector} The state.
;;  */
(define (make-state) (vector '() #f '() '() #f 0 #f #f))

(define (state-globals st) (vector-ref st 0))
(define (state-calls-unknown? st) (vector-ref st 1))
(define (state-reason st) (vector-ref st 4))
(define (state-captures? st) (vector-ref st 6))

;; /**
;;  * Records a free reference to a global, without duplicating it.
;;  * @param {vector} st - The lowering state.
;;  * @param {symbol} name - The global's renamed name.
;;  * @returns {unspecified}
;;  */
(define (state-add-global! st name)
  (if (memq name (vector-ref st 0))
      #f
      (vector-set! st 0 (cons name (vector-ref st 0)))))

;; /**
;;  * Records that some call in this form has a callee the pass cannot name.
;;  * @param {vector} st - The lowering state.
;;  * @returns {unspecified}
;;  */
(define (state-calls-unknown! st) (vector-set! st 1 #t))

;; /**
;;  * Records that the form captures a continuation itself.
;;  * @param {vector} st - The lowering state.
;;  * @returns {unspecified}
;;  */
(define (state-captures! st) (vector-set! st 6 #t))

;; /**
;;  * Records that the form has a point it can be suspended at -- a non-tail
;;  * call or a capture. A form with neither is never reified into a frame.
;;  * @param {vector} st - The lowering state.
;;  * @returns {unspecified}
;;  */
(define (state-suspends! st) (vector-set! st 7 #t))

;; /**
;;  * Records that a local bound to a lambda was called.
;;  * @param {vector} st - The lowering state.
;;  * @param {symbol} name - The local's name.
;;  * @returns {unspecified}
;;  */
(define (state-called-local! st name)
  (if (memq name (vector-ref st 2))
      #f
      (vector-set! st 2 (cons name (vector-ref st 2)))))

;; /**
;;  * Records that a local is assigned by set!.
;;  * @param {vector} st - The lowering state.
;;  * @param {symbol} name - The local's name.
;;  * @returns {unspecified}
;;  */
(define (state-assigned-local! st name)
  (if (memq name (vector-ref st 3))
      #f
      (vector-set! st 3 (cons name (vector-ref st 3)))))

;; /**
;;  * Records why lowering failed and yields the failure sentinel. The first
;;  * reason is kept, since it is the innermost and most specific one.
;;  * @param {vector} st - The lowering state.
;;  * @param {string} reason - Human-readable cause.
;;  * @returns {boolean} #f, the failure sentinel.
;;  */
(define (fail! st reason)
  (if (vector-ref st 4) #f (vector-set! st 4 reason))
  #f)

;; ---------------------------------------------------------------------------
;; Reading the analyzed AST
;; ---------------------------------------------------------------------------

(define (ast-tag node) (car node))
(define (ast-1 node) (cadr node))
(define (ast-2 node) (caddr node))
(define (ast-3 node) (cadddr node))
(define (ast-4 node) (car (cddddr node)))

;; /**
;;  * Whether an IR node denotes something this pass can name as a callee.
;;  *
;;  * A global can be looked up and followed; a lambda, and a local bound to
;;  * one, were lowered here so their references are already recorded. Anything
;;  * else -- a bare parameter, most often -- is whatever the caller handed
;;  * over, and may capture a continuation without this procedure's text saying
;;  * so.
;;  *
;;  * @param {list} node - An IR node.
;;  * @returns {boolean} Whether the callee is nameable.
;;  */
(define (ir-callable? node)
  (let ((tag (car node)))
    (cond ((eq? tag 'local) (ast-3 node))
          ((eq? tag 'global) (ast-3 node))
          ((eq? tag 'lambda) (car (cdr (cddddr (cdr node)))))
          ((eq? tag 'if) (car (cddddr (cdr node))))
          ((eq? tag 'seq) (ast-3 node))
          ((eq? tag 'let) (car (cddddr (cdr node))))
          ((eq? tag 'letrec) (car (cddddr (cdr node))))
          (else #f))))

;; ---------------------------------------------------------------------------
;; Lowering
;; ---------------------------------------------------------------------------

;; /**
;;  * Lowers an analyzed AST node to IR.
;;  * @param {list} node - An analyzed AST node.
;;  * @param {list} scope - The enclosing lexical scope.
;;  * @param {boolean} tail - Whether the node is in tail position.
;;  * @param {vector} st - Lowering state.
;;  * @returns {list|boolean} An IR node, or #f if the form is unsupported.
;;  */
(define (lower-node node scope tail st)
  (let ((tag (ast-tag node)))
    (cond
      ((eq? tag 'lit)
       (list 'const (ast-1 node) tail))

      ((eq? tag 'var)
       (let ((name (ast-1 node)))
         (if (scope-has? scope name)
             (list 'local name tail (scope-callable? scope name))
             (begin
               (state-add-global! st name)
               ;; A global callee is nameable in the sense this flag means: the
               ;; safety analysis can look it up and follow it. Not that it is
               ;; safe.
               (list 'global name tail #t)))))

      ((eq? tag 'if)
       (let ((test (lower-node (ast-1 node) scope #f st)))
         (if (not test)
             #f
             (let ((then (lower-node (ast-2 node) scope tail st)))
               (if (not then)
                   #f
                   (let ((other (lower-node (ast-3 node) scope tail st)))
                     (if (not other)
                         #f
                         (list 'if test then other tail
                               (if (ir-callable? then) (if (ir-callable? other) #t #f) #f)))))))))

      ((eq? tag 'seq)
       (let ((body (lower-sequence (ast-1 node) scope tail st)))
         (if (not body)
             #f
             (list 'seq body tail
                   (if (null? body) #f (if (ir-callable? (last-of body)) #t #f))))))

      ((eq? tag 'lambda)
       (let ((inner (make-scope scope)))
         (declare-all! inner (ast-1 node))
         (if (ast-2 node) (scope-declare! inner (ast-2 node) #f) #f)
         (let ((body (lower-body (ast-4 node) inner st)))
           (if (not body)
               #f
               (list 'lambda (ast-1 node) (ast-2 node) (ast-3 node) body tail #t)))))

      ((eq? tag 'let)
       (let ((init (lower-node (ast-2 node) scope #f st)))
         (if (not init)
             #f
             (let ((inner (make-scope scope)))
               (scope-declare! inner (ast-1 node) (eq? (car init) 'lambda))
               (let ((body (lower-node (ast-3 node) inner tail st)))
                 (if (not body)
                     #f
                     (list 'let (ast-1 node) init body tail
                           (if (ir-callable? body) #t #f))))))))

      ((eq? tag 'letrec)
       ;; Every name is in scope in every initializer, which is what makes the
       ;; group mutually recursive, and every initializer is a lambda -- the
       ;; analyzer only builds this shape. Declaring them all callable before
       ;; lowering any of them is what lets a recursive or mutually recursive
       ;; call be recognised as a callee this pass can name.
       (let ((inner (make-scope scope)))
         (declare-all-callable! inner (ast-1 node))
         (let ((inits (lower-each (ast-2 node) inner st)))
           (if (not inits)
               #f
               (let ((body (lower-node (ast-3 node) inner tail st)))
                 (if (not body)
                     #f
                     (list 'letrec (ast-1 node) inits body tail
                           (if (ir-callable? body) #t #f))))))))

      ((eq? tag 'set)
       (let ((value (lower-node (ast-2 node) scope #f st)))
         (if (not value)
             #f
             (let* ((name (ast-1 node))
                    (local (scope-has? scope name)))
               (if local
                   (state-assigned-local! st name)
                   (state-add-global! st name))
               (list 'set name local value tail)))))

      ((eq? tag 'define)
       ;; Only reachable for an internal definition; the top-level case is
       ;; handled by the caller. The analyzer has already hoisted the name.
       (let ((value (lower-node (ast-2 node) scope #f st)))
         (if (not value)
             #f
             (begin
               (scope-declare! scope (ast-1 node) (eq? (car value) 'lambda))
               (list 'define (ast-1 node) value tail)))))

      ((eq? tag 'app)
       (let ((direct (lower-direct-application node scope tail st)))
         (if (not (eq? direct 'not-this-shape))
             direct
             (let ((values-call (lower-call-with-values node scope tail st)))
               (if (not (eq? values-call 'not-this-shape))
                   values-call
                   (let ((captured (lower-call-cc node scope tail st)))
                     (if (not (eq? captured 'not-this-shape))
                         captured
                         (lower-ordinary-application node scope tail st))))))))

      (else (fail! st (ast-1 node))))))

;; /**
;;  * Lowers `(call/cc receiver)` as a capture made by compiled code.
;;  *
;;  * A continuation is the interpreter's frame stack, which the compiled frames
;;  * between the call and the interpreter do not appear in, so the primitive
;;  * cannot be called. The capture is emitted as a call site that suspends
;;  * instead: the procedure records what the capture needs, spills its locals
;;  * and reports the unwind outward, and the captured value arrives at the
;;  * resume point rather than from the call.
;;  *
;;  * @param {list} node - An `app` AST node.
;;  * @param {list} scope - The enclosing lexical scope.
;;  * @param {boolean} tail - Whether the application is in tail position.
;;  * @param {vector} st - Lowering state.
;;  * @returns {list|boolean|symbol} IR, #f if unsupported, or 'not-this-shape.
;;  */
(define (lower-call-cc node scope tail st)
  (let ((fn (ast-1 node)))
    (if (not (eq? (ast-tag fn) 'var))
        'not-this-shape
        (if (not (if (eq? (ast-1 fn) 'call/cc)
                     #t
                     (eq? (ast-1 fn) 'call-with-current-continuation)))
            'not-this-shape
            (if (scope-has? scope (ast-1 fn))
                'not-this-shape
                (if (not (= (length (ast-2 node)) 1))
                    'not-this-shape
                    (let ((receiver (lower-node (car (ast-2 node)) scope #f st)))
                      (if (not receiver)
                          #f
                          (begin
                            ;; The receiver is handed the continuation and
                            ;; called, so it is a callee this pass cannot name.
                            (state-calls-unknown! st)
                            (state-captures! st)
                            (state-suspends! st)
                            (list 'capture receiver tail))))))))))

;; /**
;;  * Lowers `(call-with-values producer consumer)` without the primitive.
;;  *
;;  * The primitive returns something only the interpreter can continue, and it
;;  * cannot be a plain procedure either, because the pending consumer
;;  * application would then sit in a frame a captured continuation could not
;;  * restore. Rewriting it as `(apply consumer (%values->list (producer)))`
;;  * avoids both: every part is something the compiler already emits, and the
;;  * producer call becomes an ordinary call site, which is what gives a capture
;;  * inside it somewhere to resume.
;;  *
;;  * The producer expression is bound first so the operands are still evaluated
;;  * left to right, matching the interpreter.
;;  *
;;  * @param {list} node - An `app` AST node.
;;  * @param {list} scope - The enclosing lexical scope.
;;  * @param {boolean} tail - Whether the application is in tail position.
;;  * @param {vector} st - Lowering state.
;;  * @returns {list|boolean|symbol} IR, #f if unsupported, or 'not-this-shape.
;;  */
(define (lower-call-with-values node scope tail st)
  (let ((fn (ast-1 node)))
    (if (not (eq? (ast-tag fn) 'var))
        'not-this-shape
        (if (not (eq? (ast-1 fn) 'call-with-values))
            'not-this-shape
            ;; A local of the same name is not the primitive at all.
            (if (scope-has? scope (ast-1 fn))
                'not-this-shape
                (if (not (= (length (ast-2 node)) 2))
                    'not-this-shape
                    (let ((producer (lower-node (car (ast-2 node)) scope #f st)))
                      (if (not producer)
                          #f
                          (let ((consumer (lower-node (cadr (ast-2 node)) scope #f st)))
                            (if (not consumer)
                                #f
                                (begin
                                  ;; Not the name being rewritten away: recording
                                  ;; it would decline the procedure for mentioning
                                  ;; something it no longer mentions.
                                  (state-add-global! st 'apply)
                                  (state-add-global! st '%values->list)
                                  ;; The producer is whatever the caller was
                                  ;; handed, so calling it is calling something
                                  ;; this pass cannot name.
                                  (state-calls-unknown! st)
                                  (let ((nm (synthesized-name! st)))
                                    (list 'let nm producer
                                          (list 'call (list 'global 'apply #f #t)
                                                (list consumer
                                                      (list 'call
                                                            (list 'global '%values->list #f #t)
                                                            (list (list 'call
                                                                        (list 'local nm #f #f)
                                                                        '() #f))
                                                            #f))
                                                tail)
                                          tail #f)))))))))))))

;; /**
;;  * A name this pass invents, distinct from anything the analyzer produces.
;;  * @param {vector} st - The lowering state.
;;  * @returns {symbol} A fresh name.
;;  */
(define (synthesized-name! st)
  (let ((n (vector-ref st 5)))
    (vector-set! st 5 (+ n 1))
    (string->symbol (string-append "%cwv" (number->string n)))))

;; /**
;;  * Lowers `((lambda (a b) body) x y)` as bindings rather than as a call.
;;  *
;;  * Every `let` reaches the compiler in this shape, because that is what the
;;  * analyzer expands it to, so a chain of bindings would otherwise be a chain
;;  * of nested procedures -- one per clause of a `let*`. Reducing it is sound
;;  * because the operator is a literal lambda applied exactly here: nothing
;;  * else can call it and nothing can capture it.
;;  *
;;  * The body takes on the *call's* tail position rather than being a procedure
;;  * body of its own, which is the part that has to be right: inlined into a
;;  * caller that wants a value, a call in the body must produce one.
;;  *
;;  * @param {list} node - An `app` AST node.
;;  * @param {list} scope - The enclosing lexical scope.
;;  * @param {boolean} tail - Whether the application is in tail position.
;;  * @param {vector} st - Lowering state.
;;  * @returns {list|boolean|symbol} IR, #f if unsupported, or the symbol
;;  *   'not-this-shape if the caller should emit an ordinary call.
;;  */
(define (lower-direct-application node scope tail st)
  (let ((fn (ast-1 node)))
    (if (not (eq? (ast-tag fn) 'lambda))
        'not-this-shape
        ;; A rest parameter would have to be built from the arguments, and a
        ;; mismatched count is an arity error the ordinary path reports.
        (if (ast-2 fn)
            'not-this-shape
            (let ((params (ast-1 fn))
                  (args (ast-2 node)))
              (if (not (= (length params) (length args)))
                  'not-this-shape
                  (let ((inits (lower-each args scope st)))
                    (if (not inits)
                        #f
                        (let ((inner (make-scope scope)))
                          (declare-bindings! inner params inits)
                          (let ((body (lower-body-in (ast-4 fn) inner st tail)))
                            (if (not body)
                                #f
                                (wrap-bindings params inits body tail))))))))))))

;; /**
;;  * Declares each parameter, noting the ones bound to a lambda.
;;  * @param {list} scope - The scope to extend.
;;  * @param {list} params - Renamed parameter names.
;;  * @param {list} inits - Their lowered initializers, in the same order.
;;  * @returns {unspecified}
;;  */
(define (declare-bindings! scope params inits)
  (if (null? params)
      #f
      (begin
        (scope-declare! scope (car params) (eq? (car (car inits)) 'lambda))
        (declare-bindings! scope (cdr params) (cdr inits)))))

;; /**
;;  * Wraps a body in one binding per parameter, the first outermost.
;;  * @param {list} params - Renamed parameter names.
;;  * @param {list} inits - Their lowered initializers.
;;  * @param {list} body - The lowered body.
;;  * @param {boolean} tail - Whether the whole form is in tail position.
;;  * @returns {list} An IR node.
;;  */
(define (wrap-bindings params inits body tail)
  (if (null? params)
      body
      (let ((inner (wrap-bindings (cdr params) (cdr inits) body tail)))
        (list 'let (car params) (car inits) inner tail
              (if (ir-callable? inner) #t #f)))))

;; /**
;;  * Lowers an ordinary application, whose callee is evaluated like any other
;;  * expression.
;;  * @param {list} node - An `app` AST node.
;;  * @param {list} scope - The enclosing lexical scope.
;;  * @param {boolean} tail - Whether the application is in tail position.
;;  * @param {vector} st - Lowering state.
;;  * @returns {list|boolean} An IR node, or #f if unsupported.
;;  */
(define (lower-ordinary-application node scope tail st)
  (let ((fn (lower-node (ast-1 node) scope #f st)))
    (if (not fn)
        #f
        (let ((args (lower-each (ast-2 node) scope st)))
          (if (not args)
              #f
              (begin
                (if (ir-callable? fn)
                    (if (eq? (car fn) 'local) (state-called-local! st (ast-1 fn)) #f)
                    (state-calls-unknown! st))
                (if tail #f (state-suspends! st))
                (list 'call fn args tail)))))))

;; /**
;;  * Declares each of a lambda's parameters as a plain binding.
;;  * @param {list} scope - The scope to extend.
;;  * @param {list} names - Renamed parameter names.
;;  * @returns {unspecified}
;;  */
(define (declare-all! scope names)
  (if (null? names)
      #f
      (begin (scope-declare! scope (car names) #f)
             (declare-all! scope (cdr names)))))

;; /**
;;  * Declares each name as bound to a lambda this pass lowered.
;;  * @param {list} scope - The scope to extend.
;;  * @param {list} names - Renamed names.
;;  * @returns {unspecified}
;;  */
(define (declare-all-callable! scope names)
  (if (null? names)
      #f
      (begin (scope-declare! scope (car names) #t)
             (declare-all-callable! scope (cdr names)))))

;; /**
;;  * @param {list} lst - A non-empty list.
;;  * @returns {*} Its last element.
;;  */
(define (last-of lst)
  (if (null? (cdr lst)) (car lst) (last-of (cdr lst))))

;; /**
;;  * Lowers every node in a list, none of them in tail position.
;;  * @param {list} nodes - Analyzed nodes.
;;  * @param {list} scope - Enclosing scope.
;;  * @param {vector} st - Lowering state.
;;  * @returns {list|boolean} IR nodes, or #f if any was unsupported.
;;  */
(define (lower-each nodes scope st)
  (if (null? nodes)
      '()
      (let ((head (lower-node (car nodes) scope #f st)))
        (if (not head)
            #f
            (let ((rest (lower-each (cdr nodes) scope st)))
              (if (not rest) #f (cons head rest)))))))

;; /**
;;  * Lowers a sequence, marking only its last expression as tail.
;;  * @param {list} nodes - Analyzed nodes.
;;  * @param {list} scope - Enclosing scope.
;;  * @param {boolean} tail - Whether the sequence is in tail position.
;;  * @param {vector} st - Lowering state.
;;  * @returns {list|boolean} IR nodes, or #f if any was unsupported.
;;  */
(define (lower-sequence nodes scope tail st)
  (if (null? nodes)
      '()
      (let* ((last? (null? (cdr nodes)))
             (head (lower-node (car nodes) scope (if last? tail #f) st)))
        (if (not head)
            #f
            (let ((rest (lower-sequence (cdr nodes) scope tail st)))
              (if (not rest) #f (cons head rest)))))))

;; /**
;;  * Lowers a procedure body, which is in tail position by definition.
;;  *
;;  * Internal definitions are declared before the body is lowered, or a forward
;;  * reference between two internal procedures would be mistaken for a global.
;;  * One naming a procedure is a callee this pass can name, so calling it is not
;;  * calling an unknown.
;;  *
;;  * @param {list} node - The body node.
;;  * @param {list} scope - The procedure's scope.
;;  * @param {vector} st - Lowering state.
;;  * @returns {list|boolean} An IR node, or #f if the body is unsupported.
;;  */
(define (lower-body node scope st)
  (lower-body-in node scope st #t))

;; /**
;;  * Lowers a body in a caller-supplied tail position.
;;  *
;;  * A real procedure body is in tail position by definition; a body being
;;  * inlined into its caller is in whatever position the caller was.
;;  *
;;  * @param {list} node - The body node.
;;  * @param {list} scope - The scope to lower in.
;;  * @param {vector} st - Lowering state.
;;  * @param {boolean} tail - Whether the body is in tail position.
;;  * @returns {list|boolean} An IR node, or #f if the body is unsupported.
;;  */
(define (lower-body-in node scope st tail)
  (let ((tag (ast-tag node)))
    (cond ((eq? tag 'seq) (predeclare-definitions! (ast-1 node) scope))
          ((eq? tag 'define)
           (scope-declare! scope (ast-1 node) (eq? (car (ast-2 node)) 'lambda)))
          (else #f))
    (lower-node node scope tail st)))

;; /**
;;  * Declares the internal definitions among a body's expressions.
;;  * @param {list} exprs - The body's expressions.
;;  * @param {list} scope - The procedure's scope.
;;  * @returns {unspecified}
;;  */
(define (predeclare-definitions! exprs scope)
  (if (null? exprs)
      #f
      (begin
        (if (eq? (ast-tag (car exprs)) 'define)
            (scope-declare! scope (ast-1 (car exprs))
                            (eq? (car (ast-2 (car exprs))) 'lambda))
            #f)
        (predeclare-definitions! (cdr exprs) scope))))

;; /**
;;  * Lowers a lambda to IR, reporting what it references.
;;  *
;;  * Lowering failure and safety are kept apart, because they are different
;;  * questions. A form the compiler cannot express is a failure, reported as a
;;  * reason. A form it can express but should not compile is a judgement the
;;  * caller makes, and needs more than this one lambda to make.
;;  *
;;  * @param {list} node - An analyzed lambda node.
;;  * @returns {list} Either (ok ir globals calls-unknown captures) or
;;  *   (fail reason).
;;  */
(define (lower-lambda node)
  (let* ((st (make-state))
         (ir (lower-node node (make-scope '()) #f st)))
    (if (not ir)
        (list 'fail (let ((r (state-reason st))) (if r r "unsupported form")))
        ;; A local that was both called and assigned is not the lambda we
        ;; lowered, so the call does not reach a callee this pass can name.
        (list 'ok ir (reverse (state-globals st))
              (if (state-calls-unknown? st)
                  #t
                  (any-assigned? (vector-ref st 2) (vector-ref st 3)))
              (state-captures? st)))))

;; /**
;;  * Whether any called local is also assigned.
;;  * @param {list} called - Locals that were called.
;;  * @param {list} assigned - Locals that are assigned.
;;  * @returns {boolean} True if the two lists intersect.
;;  */
(define (any-assigned? called assigned)
  (if (null? called)
      #f
      (if (memq (car called) assigned)
          #t
          (any-assigned? (cdr called) assigned))))

