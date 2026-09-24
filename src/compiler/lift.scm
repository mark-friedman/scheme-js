;;; lift.scm -- deciding which nested procedures to emit once, at top level.
;;;
;;; ## The problem
;;;
;;; A procedure is emitted twice -- a fast form and a resumable one -- and each
;;; of those used to emit both forms of every procedure nested inside it. So a
;;; lambda at nesting depth d appeared about 4^d times. Measured, that was 4.2x
;;; per level: `earley.scm:make-parser` reached 3.6 MB of generated source, and
;;; `map`, `for-each`, `vector-map` and `string-map` together were a third of the
;;; compiled standard library's bundle.
;;;
;;; ## The fix
;;;
;;; Each nested procedure is emitted once, at the top level of the generated
;;; unit, as a factory taking its free variables:
;;;
;;;     function $mk$fn0(s_a, s_b) {
;;;       function $fn0(s_x) { ... uses s_a, s_b, and $fn0 for recursion ... }
;;;       function $fn0$r($pc, $f) { ... }
;;;       $fn0.$resume = $fn0$r;
;;;       return $fn0;
;;;     }
;;;
;;; and created with `$t5 = $mk$fn0(s_a, s_b)`. Every form of every parent
;;; shares that one emission, so code size is linear in the program rather than
;;; exponential in its nesting. Variable *references* do not change, which is
;;; what makes this cheap: the inner function closes over the factory's
;;; parameters, and those have the names the body already used.
;;;
;;; ## Why free variables can be passed by value
;;;
;;; A free variable that is never assigned cannot be observed to differ from a
;;; copy, so passing it is safe. One that *is* assigned is held in a
;;; one-element array, so what gets passed is the array, and every reader still
;;; shares one binding.
;;;
;;; ## `letrec`, which is the whole difficulty
;;;
;;; A `letrec`-bound lambda refers to names its group is still defining, so
;;; they cannot be passed by value when it is created -- they have no value yet.
;;;
;;; Self-reference needs nothing: the factory declares the name and assigns the
;;; procedure to it before returning, so the recursive call resolves lexically
;;; inside the factory. That is the common case and the hot one.
;;;
;;; Mutual reference does need indirection, so a name referenced by one of its
;;; *siblings* is boxed. The boxes exist before any initializer runs and are
;;; passed to each factory, so the group can refer to itself in any order. Only
;;; such names pay for it: in `map`, `loop` reads `any-null?`, `all-cars` and
;;; `all-cdrs`, so those three are boxed while `loop` itself is not.

;; /**
;;  * What lifting decided about one unit.
;;  *
;;  * `free` maps each nested lambda node to its free variables, in the order
;;  * first seen -- its factory's parameter list, which both forms of its parent
;;  * must pass in the same order. `boxed` is every local held in a box. `self`
;;  * maps a `letrec` initializer to the name it binds inside its own factory.
;;  * Lambda nodes are keys by identity, in association lists: a unit has few.
;;  */
(define-record-type lift-plan
  (make-lift-plan free boxed self)
  lift-plan?
  (free plan-free)
  (boxed plan-boxed)
  (self plan-self))

;; /**
;;  * A nested lambda's free variables, which become its factory's parameters.
;;  * @param {lift-plan} plan - The plan.
;;  * @param {list} lam - A nested lambda IR node.
;;  * @returns {list} The names.
;;  */
(define (plan-free-of plan lam)
  (cond ((assq lam (plan-free plan)) => cdr) (else '())))

;; /**
;;  * The group names a `letrec` initializer binds inside its own factory.
;;  * @param {lift-plan} plan - The plan.
;;  * @param {list} lam - A lambda IR node.
;;  * @returns {list} Its own name, or '().
;;  */
(define (plan-self-of plan lam)
  (cond ((assq lam (plan-self plan)) => cdr) (else '())))

;; /**
;;  * Whether a local is held in a box.
;;  * @param {lift-plan} plan - The plan.
;;  * @param {symbol} name - A renamed local.
;;  * @returns {boolean}
;;  */
(define (boxed? plan name) (memq name (plan-boxed plan)))

;; ---------------------------------------------------------------------------
;; Walking the IR
;; ---------------------------------------------------------------------------
;;
;; Every walk here goes through `ir-children`, so none can disagree with
;; another about order. Order matters: a factory lists its parameters in the
;; order its free variables are first seen, and both forms of its parent pass
;; them in that order.

;; /**
;;  * The IR nodes directly inside a node, in the order they are evaluated.
;;  * @param {list} node - An IR node.
;;  * @returns {list} Its children.
;;  */
(define (ir-children node)
  (case (car node)
    ((if) (list (cadr node) (caddr node) (cadddr node)))
    ((seq) (cadr node))
    ((lambda) (list (lambda-body node)))
    ((let) (list (caddr node) (cadddr node)))
    ((letrec) (append (caddr node) (list (cadddr node))))
    ((set) (list (cadddr node)))
    ((define) (list (caddr node)))
    ((call) (cons (cadr node) (caddr node)))
    ((capture) (list (cadr node)))
    (else '())))

(define (lambda-params lam) (cadr lam))
(define (lambda-rest lam) (caddr lam))
(define (lambda-name lam) (cadddr lam))
(define (lambda-body lam) (car (cddddr lam)))

;; /**
;;  * The names a lambda binds on entry: its parameters and rest parameter.
;;  * @param {list} lam - A lambda IR node.
;;  * @returns {list} The names.
;;  */
(define (lambda-bound lam)
  (if (lambda-rest lam)
      (cons (lambda-rest lam) (lambda-params lam))
      (lambda-params lam)))

;; /**
;;  * Every node in a subtree satisfying a predicate, parents before children.
;;  * @param {procedure} keep? - The predicate.
;;  * @param {list} node - An IR node.
;;  * @returns {list} The nodes.
;;  */
(define (ir-find keep? node)
  (let ((below (append-map (lambda (child) (ir-find keep? child)) (ir-children node))))
    (if (keep? node) (cons node below) below)))

;; /**
;;  * The lambdas directly inside a subtree: found, but not searched inside,
;;  * since what they contain belongs to them.
;;  * @param {list} node - An IR node.
;;  * @returns {list} The lambda nodes.
;;  */
(define (outermost-lambdas node)
  (if (eq? (car node) 'lambda)
      (list node)
      (append-map outermost-lambdas (ir-children node))))

(define (tagged? tag) (lambda (node) (eq? (car node) tag)))

;; /**
;;  * Whether a subtree reads or assigns a local, nested lambdas included.
;;  * Names are unique after the analyzer's renaming, so any mention is a
;;  * mention of that one binding.
;;  * @param {list} node - An IR node.
;;  * @param {symbol} name - A renamed local.
;;  * @returns {boolean}
;;  */
(define (mentions? node name)
  (case (car node)
    ((local) (eq? (cadr node) name))
    ((set) (or (and (caddr node) (eq? (cadr node) name))
               (mentions? (cadddr node) name)))
    (else (any (lambda (child) (mentions? child name)) (ir-children node)))))

;; ---------------------------------------------------------------------------
;; Free variables
;; ---------------------------------------------------------------------------

;; /**
;;  * The locals a lambda reads or assigns without binding, in the order first
;;  * seen.
;;  * @param {list} lam - A lambda IR node.
;;  * @returns {list} Its free variables.
;;  */
(define (free-variables lam)
  (reverse (free-in (lambda-body lam) (lambda-bound lam) '())))

;; /**
;;  * Adds a subtree's free variables to those found so far.
;;  *
;;  * Every internal definition in a body is in scope throughout it, not only
;;  * after itself -- that is what lets two internal procedures call each other
;;  * -- so a sequence binds all its definitions before scanning any of its
;;  * expressions. Binding them in order instead once reported a self-recursive
;;  * internal definition as free in its own procedure, and left the caller
;;  * passing a variable it never declared.
;;  *
;;  * @param {list} node - An IR node.
;;  * @param {list} bound - Names bound by enclosing forms.
;;  * @param {list} found - Free variables so far, most recent first.
;;  * @returns {list} The free variables, most recent first.
;;  */
(define (free-in node bound found)
  (define (scan-all nodes bound found)
    (fold (lambda (n acc) (free-in n bound acc)) found nodes))
  (define (note name)
    (if (or (memq name bound) (memq name found)) found (cons name found)))
  (case (car node)
    ((local) (note (cadr node)))
    ((set) (free-in (cadddr node) bound (if (caddr node) (note (cadr node)) found)))
    ((lambda) (free-in (lambda-body node) (append (lambda-bound node) bound) found))
    ((let) (free-in (cadddr node) (cons (cadr node) bound)
                    (free-in (caddr node) bound found)))
    ((letrec) (let ((inner (append (cadr node) bound)))
                (scan-all (ir-children node) inner found)))
    ((seq) (let ((inner (append (map cadr (filter (tagged? 'define) (cadr node))) bound)))
             (scan-all (cadr node) inner found)))
    (else (scan-all (ir-children node) bound found))))

;; ---------------------------------------------------------------------------
;; The plan
;; ---------------------------------------------------------------------------

;; /**
;;  * Plans lifting for a lowered procedure.
;;  *
;;  * Which nested procedures to lift, what each is passed, and which locals
;;  * must be shared rather than copied are one question asked three ways, so
;;  * every reason a local is boxed is decided here.
;;  *
;;  * @param {list} ir - The unit's top-level lambda IR node.
;;  * @returns {lift-plan} The plan.
;;  */
(define (plan-lifting ir)
  (let* ((groups (ir-find (tagged? 'letrec) ir))
         (boxed (delete-duplicates (append (assigned-locals ir)
                                           (sibling-references groups)
                                           (defined-and-captured ir))
                                   eq?))
         (self (self-bindings groups boxed)))
    (make-lift-plan (factory-parameters ir self) boxed self)))

;; /**
;;  * The locals assigned by `set!` anywhere in a unit.
;;  *
;;  * A spilled frame copies each local's value, while Scheme shares the
;;  * binding: an assignment made after a continuation is captured is seen when
;;  * that continuation is invoked again, and by every closure over the
;;  * variable. Copying is right for temporaries, which are always written before
;;  * they are read, and wrong for a variable the program can name.
;;  *
;;  * @param {list} ir - An IR node.
;;  * @returns {list} The names.
;;  */
(define (assigned-locals ir)
  (map cadr (filter caddr (ir-find (tagged? 'set) ir))))

;; /**
;;  * The `letrec` names one of their siblings refers to. The sibling is
;;  * created before the name has a value, so it has to be handed a box.
;;  * @param {list} groups - Every `letrec` node in the unit.
;;  * @returns {list} The names.
;;  */
(define (sibling-references groups)
  (append-map
    (lambda (group)
      (let ((names (cadr group)) (inits (caddr group)))
        (filter (lambda (name)
                  (any (lambda (own init) (and (not (eq? own name)) (mentions? init name)))
                       names inits))
                names)))
    groups))

;; /**
;;  * The internal definitions a nested procedure refers to.
;;  *
;;  * An internal definition has the difficulty a `letrec` group has: its name
;;  * takes its value when the definition runs, so a procedure created earlier
;;  * in the same body that refers to it -- the way mutually recursive internal
;;  * definitions are written -- would capture nothing.
;;  *
;;  * @param {list} ir - The unit's top-level lambda.
;;  * @returns {list} The names.
;;  */
(define (defined-and-captured ir)
  (let ((nested (ir-find (tagged? 'lambda) (lambda-body ir))))
    (filter (lambda (name) (any (lambda (lam) (mentions? lam name)) nested))
            (map cadr (ir-find (tagged? 'define) ir)))))

;; /**
;;  * The name each `letrec` initializer may bind inside its own factory rather
;;  * than receive: its own, when that is not boxed. A boxed name is shared
;;  * through its box, so the factory has to be handed the box instead.
;;  * @param {list} groups - Every `letrec` node in the unit.
;;  * @param {list} boxed - Every boxed name.
;;  * @returns {list} Alist from initializer to a list of names.
;;  */
(define (self-bindings groups boxed)
  (append-map
    (lambda (group)
      (filter-pairs (lambda (name init)
                      (and (eq? (car init) 'lambda) (not (memq name boxed))))
                    (cadr group) (caddr group)))
    groups))

;; /**
;;  * `(init . (name))` for each name and initializer that satisfy a predicate.
;;  * @param {procedure} keep? - Takes a name and its initializer.
;;  * @param {list} names - Names.
;;  * @param {list} inits - Their initializers.
;;  * @returns {list} The alist.
;;  */
(define (filter-pairs keep? names inits)
  (cond ((null? names) '())
        ((keep? (car names) (car inits))
         (cons (cons (car inits) (list (car names)))
               (filter-pairs keep? (cdr names) (cdr inits))))
        (else (filter-pairs keep? (cdr names) (cdr inits)))))

;; /**
;;  * Every nested lambda's factory parameters: its free variables, less the
;;  * name it binds itself.
;;  * @param {list} lam - A lambda IR node.
;;  * @param {list} self - The self-binding alist.
;;  * @returns {list} Alist from lambda node to parameter names.
;;  */
(define (factory-parameters lam self)
  (append-map
    (lambda (nested)
      (let ((own (cond ((assq nested self) => cdr) (else '()))))
        (cons (cons nested (remove (lambda (v) (memq v own)) (free-variables nested)))
              (factory-parameters nested self))))
    (outermost-lambdas (lambda-body lam))))
