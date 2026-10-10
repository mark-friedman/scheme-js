;;; scopes.scm -- where each variable of the Scheme is in the generated code,
;;; for a debugger.
;;;
;;; A source map's `scopes` (ECMA-426's scopes proposal, which sourcemap.scm
;;; writes) give a debugger two trees. One is the scopes of the source: each
;;; procedure's, each `let`'s and `letrec`'s, and the file's, with the
;;; variables each binds by the names the source gives them. The other is the
;;; ranges of the generated code: for each stretch of it, the scope it is, and
;;; for each of that scope's variables the JavaScript that reads it there -- a
;;; local by the name the emitter gave it, one a procedure assigns and a
;;; closure captures through its box, a global through its cell -- or nothing,
;;; where the code cannot reach it. A debugger paused in compiled code then
;;; shows the frame's variables as Scheme names them, each scope as the source
;;; nests them, and none of the compiler's temporaries. DevTools does with its
;;; `use-source-map-scopes` experiment on, and otherwise shows the generated
;;; names, which `local-name` in emit.scm keeps readable.
;;;
;;; The source's scopes are made from a unit's IR before its code is
;;; generated (`original-scopes`), each node that binds a variable noted with
;;; its scope. The emitter notes the scope each statement it emits is in
;;; (`form-scope` in emit.scm) and wraps each function and factory it renders
;;; in the scopes it is (`ranged-items`); `render-items` makes the ranges from
;;; them as it writes the lines, a line at a time, so a range is whole lines
;;; but for a function's, which begins at its head's `function`. That is
;;; exact enough: the emitter writes each statement on a line of its own.
;;; DevTools names a frame by the scope at its function's start, and a
;;; factory's statement that makes the function begins before it.
;;;
;;; A macro's expansion is not a scope of its own. DevTools could show one as
;;; a procedure inlined at its use, but steps over source-mapped JavaScript by
;;; its mappings alone, so it would step through the template's code on a
;;; step over the use; an expansion's code is placed at its use instead
;;; (`with-use-span` in expander.scm).

;; ---------------------------------------------------------------------------
;; The source's scopes
;; ---------------------------------------------------------------------------

;; /**
;;  * A scope of the Scheme source.
;;  * @property {string} kind - "function", "block" or "global", as a
;;  *   debugger titles it.
;;  * @property {string|boolean} name - A procedure's name, or #f.
;;  * @property {pair} start - Where it begins, (line . column) from zero.
;;  * @property {pair} end - Where it ends.
;;  * @property {original-scope|boolean} parent - The scope it is in, or #f.
;;  * @property {list} locals - What it binds, as the code knows them: renamed
;;  *   locals, or, the file's, globals.
;;  * @property {list} variables - Their names, as the source writes them.
;;  * @property {list} children - The scopes in it, in order.
;;  */
(define-record-type original-scope
  (%make-original-scope kind name start end parent locals variables children)
  original-scope?
  (kind original-scope-kind)
  (name original-scope-name)
  (start original-scope-start)
  (end original-scope-end)
  (parent original-scope-parent)
  (locals original-scope-locals set-original-scope-locals!)
  (variables original-scope-variables set-original-scope-variables!)
  (children original-scope-children set-original-scope-children!))

;; /**
;;  * A scope with nothing in it yet.
;;  * @returns {original-scope}
;;  */
(define (make-original-scope kind name start end parent)
  (%make-original-scope kind name start end parent '() '() '()))

;; /**
;;  * Whether a scope is a procedure's, which a debugger shows as a frame.
;;  * @param {original-scope} s - The scope.
;;  * @returns {boolean}
;;  */
(define (original-scope-stack-frame? s) (string=? (original-scope-kind s) "function"))

;; /**
;;  * The scopes of the Scheme a unit is compiled from.
;;  * @property {string|boolean} file - The file they are of, or #f.
;;  * @property {original-scope} root - The file's scope.
;;  * @property {weak-table} table - Each IR node that makes a scope, by node.
;;  */
(define-record-type original-scopes
  (make-original-scopes file root table)
  original-scopes?
  (file original-scopes-file)
  (root original-scopes-root)
  (table original-scopes-table))

;; /**
;;  * The scopes of a unit's Scheme: the file's, whose variables are the
;;  * globals the unit reads, and inside it the procedure's and the scopes
;;  * within that.
;;  * @param {list} lam - The unit's lambda IR node.
;;  * @param {list} globals - The globals it reads, as symbols.
;;  * @param {list} library-globals - (key name . env) for each that is a
;;  *   library's own binding.
;;  * @returns {original-scopes}
;;  */
(define (original-scopes lam globals library-globals)
  (let* ((span (node-span lam))
         (root (make-original-scope "global" #f '(0 . 0) (span-end span) #f))
         (table (make-weak-table)))
    (procedure-scope! lam root table)
    (finish-scope! root)
    ;; The globals the lowering introduces are the runtime's, which the
    ;; source never names (`runtime-globals` in emit.scm).
    (let ((read (remove (lambda (g) (assq g runtime-globals)) globals)))
      (set-original-scope-locals! root read)
      (set-original-scope-variables! root (map (lambda (g) (global-shown g library-globals)) read)))
    (make-original-scopes (and span (span-file span)) root table)))

;; /**
;;  * A global's name as the source writes it: a library's own binding by its
;;  * name in the library, any other by its own.
;;  * @param {symbol} g - The global, as the lowering keys it.
;;  * @param {list} library-globals - As for `original-scopes`.
;;  * @returns {string}
;;  */
(define (global-shown g library-globals)
  (let ((library (assq g library-globals)))
    (symbol->string (if library (cadr library) g))))

;; /**
;;  * Where a span begins, (line . column) from zero, or the file's start if
;;  * there is no span.
;;  * @param {object|boolean} span - A source span, or #f.
;;  * @returns {pair}
;;  */
(define (span-start span)
  (if span (cons (- (js-ref span "line") 1) (- (js-ref span "column") 1)) '(0 . 0)))

;; /**
;;  * Where a span ends, (line . column) from zero: where it begins, if the
;;  * span does not say.
;;  * @param {object|boolean} span - A source span, or #f.
;;  * @returns {pair}
;;  */
(define (span-end span)
  (if (and span (not (js-undefined? (js-ref span "endLine"))))
      (cons (- (js-ref span "endLine") 1) (- (js-ref span "endColumn") 1))
      (span-start span)))

;; /**
;;  * A new scope in another, noted as the scope of the node that makes it, if
;;  * one does.
;;  * @param {string} kind - Its kind.
;;  * @param {string|boolean} name - Its name, or #f.
;;  * @param {object|boolean} span - The span of the form that makes it.
;;  * @param {original-scope} parent - The scope it is in.
;;  * @param {weak-table} table - The unit's scopes by node.
;;  * @param {list|boolean} node - The IR node that makes it, or #f.
;;  * @returns {original-scope}
;;  */
(define (new-scope! kind name span parent table node)
  (let ((s (make-original-scope kind name (span-start span) (span-end span) parent)))
    (set-original-scope-children! parent (cons s (original-scope-children parent)))
    (if node (weak-table-set! table node s))
    s))

;; /**
;;  * Notes a local as a variable of a scope, unless the system made it.
;;  * @param {original-scope} scope - The scope.
;;  * @param {symbol} local - The renamed local.
;;  * @returns {unspecified}
;;  */
(define (add-local! scope local)
  (if (not (system-name? local))
      (set-original-scope-locals! scope (cons local (original-scope-locals scope)))))

;; /**
;;  * Whether a local is one the system made, not the source: its name begins
;;  * with `%`, as the lowering's own locals' do (`%cwv0`, `%this0`).
;;  * @param {symbol} local - The local.
;;  * @returns {boolean}
;;  */
(define (system-name? local)
  (char=? (string-ref (symbol->string local) 0) #\%))

;; /**
;;  * Puts a scope's children and variables in order, and names its variables
;;  * as the source writes them, and its children's.
;;  * @param {original-scope} scope - The scope.
;;  * @returns {unspecified}
;;  */
(define (finish-scope! scope)
  (let ((locals (reverse (original-scope-locals scope))))
    (set-original-scope-locals! scope locals)
    (set-original-scope-variables! scope (map (lambda (local) (written-name (symbol->string local))) locals))
    (set-original-scope-children! scope (reverse (original-scope-children scope)))
    (for-each finish-scope! (original-scope-children scope))))

;; /**
;;  * Makes the scopes inside an IR node, in a scope.
;;  * @param {list} node - The node.
;;  * @param {original-scope} scope - The scope it is in.
;;  * @param {weak-table} table - The unit's scopes by node.
;;  * @returns {unspecified}
;;  */
(define (scopes-within! node scope table)
  (case (car node)
    ((lambda) (procedure-scope! node scope table))
    ((let) (let-scope! node scope table))
    ((letrec) (letrec-scope! node scope table))
    ;; An internal definition binds a variable of the body it is in.
    ((define) (add-local! scope (cadr node)) (scopes-within! (caddr node) scope table))
    (else (for-each (lambda (child) (scopes-within! child scope table)) (ir-children node)))))

;; /**
;;  * Makes a procedure's scope, its parameters its first variables.
;;  * @param {list} lam - Its lambda IR node.
;;  * @param {original-scope} parent - The scope it is in.
;;  * @param {weak-table} table - The unit's scopes by node.
;;  * @param {symbol} [variable] - The variable it is the value of, which
;;  *   names it if its definition does not.
;;  * @returns {unspecified}
;;  */
(define (procedure-scope! lam parent table . variable)
  (let ((scope (new-scope! "function" (shown-procedure-name lam variable) (node-span lam) parent table lam)))
    (for-each (lambda (p) (add-local! scope p)) (parameters-in-order lam))
    (scopes-within! (lambda-body lam) scope table)))

;; /**
;;  * A procedure's name: the one its definition gives it, or else that of the
;;  * variable a `let` or `letrec` binds to it, as JavaScript names a function
;;  * by the variable it initializes; or #f. The expander names only a
;;  * definition's lambda, and any other `anonymous`.
;;  * @param {list} lam - Its lambda IR node.
;;  * @param {list} variable - The variable it is bound to, in a list, or '().
;;  * @returns {string|boolean}
;;  */
(define (shown-procedure-name lam variable)
  (let ((name (lambda-name lam)))
    (cond ((and (string? name) (not (string=? name "anonymous"))) name)
          ((pair? variable) (written-name (symbol->string (car variable))))
          (else #f))))

;; /**
;;  * Makes the scopes inside a variable's initial value: a procedure's, named
;;  * for the variable, if the value is a lambda.
;;  * @param {list} init - The value's IR node.
;;  * @param {symbol} variable - The variable.
;;  * @param {original-scope} scope - The scope the value is in.
;;  * @param {weak-table} table - The unit's scopes by node.
;;  * @returns {unspecified}
;;  */
(define (value-scopes! init variable scope table)
  (if (eq? (car init) 'lambda)
      (procedure-scope! init scope table variable)
      (scopes-within! init scope table)))

;; /**
;;  * The `let` nodes one `let` of the source was lowered to: a node, and each
;;  * `let` with no span of its own that is the body of the one before -- a
;;  * `let` of several variables lowers to one node a variable, the outermost
;;  * given its span (`wrap-bindings` in ir.scm).
;;  * @param {list} node - A `let` IR node.
;;  * @returns {list} The nodes, outermost first.
;;  */
(define (let-group node)
  (cons node
        (let more ((body (cadddr node)))
          (if (and (eq? (car body) 'let) (not (node-span body)))
              (cons body (more (cadddr body)))
              '()))))

;; /**
;;  * Makes a `let`'s block: one for the variables of its group (`let-group`),
;;  * whose initial values are outside it, as they are evaluated before any is
;;  * bound. A group binding only names the system made makes none.
;;  * @param {list} node - A `let` IR node.
;;  * @param {original-scope} scope - The scope it is in.
;;  * @param {weak-table} table - The unit's scopes by node.
;;  * @returns {unspecified}
;;  */
(define (let-scope! node scope table)
  (let ((group (let-group node)))
    (for-each (lambda (n) (value-scopes! (caddr n) (cadr n) scope table)) group)
    (let ((block (if (every system-name? (map cadr group))
                     scope
                     (new-scope! "block" #f (node-span node) scope table #f))))
      (if (not (eq? block scope))
          (for-each (lambda (n) (weak-table-set! table n block)) group))
      (for-each (lambda (n) (add-local! block (cadr n))) group)
      (scopes-within! (cadddr (last group)) block table))))

;; /**
;;  * Makes a `letrec`'s block, its procedures' scopes inside it -- but a loop
;;  * the emitter places inside the procedure around it binds no variable
;;  * there, its name never one, and its procedure's scope is in the scope
;;  * around it.
;;  * @param {list} node - A `letrec` IR node.
;;  * @param {original-scope} scope - The scope it is in.
;;  * @param {weak-table} table - The unit's scopes by node.
;;  * @returns {unspecified}
;;  */
(define (letrec-scope! node scope table)
  (if (letrec-inline? node)
      (begin (for-each (lambda (name lam) (procedure-scope! lam scope table name)) (cadr node) (caddr node))
             (scopes-within! (cadddr node) scope table))
      (let ((block (new-scope! "block" #f (node-span node) scope table node)))
        (for-each (lambda (name) (add-local! block name)) (cadr node))
        (for-each (lambda (name init) (value-scopes! init name block table)) (cadr node) (caddr node))
        (scopes-within! (cadddr node) block table))))

;; /**
;;  * The scope an IR node makes, or #f.
;;  * @param {original-scopes|boolean} scopes - The unit's, or #f.
;;  * @param {list} node - The node.
;;  * @returns {original-scope|boolean}
;;  */
(define (scope-of-node scopes node)
  (and scopes (weak-table-ref (original-scopes-table scopes) node)))

;; /**
;;  * The scopes a scope is in, outermost first, but the file's.
;;  * @param {original-scope} scope - The scope.
;;  * @returns {list}
;;  */
(define (enclosing-scopes scope)
  (let up ((s (original-scope-parent scope)) (outer '()))
    (if (or (not s) (not (original-scope-parent s)))
        outer
        (up (original-scope-parent s) (cons s outer)))))

;; ---------------------------------------------------------------------------
;; The generated code's ranges
;; ---------------------------------------------------------------------------

;; /**
;;  * A range of the generated code.
;;  * @property {pair} start - Where it begins, (line . column) from zero.
;;  * @property {pair} end - The line after its last, as (line . 0).
;;  * @property {original-scope} scope - The scope it is.
;;  * @property {list} bindings - For each of the scope's variables, the
;;  *   JavaScript that reads it here, or #f.
;;  * @property {boolean} stack-frame? - Whether it is a function's, which an
;;  *   engine makes a frame of.
;;  * @property {list} children - The ranges in it, in order.
;;  */
(define-record-type generated-range
  (make-generated-range start end scope bindings stack-frame? children)
  generated-range?
  (start generated-range-start)
  (end generated-range-end)
  (scope generated-range-scope)
  (bindings generated-range-bindings)
  (stack-frame? generated-range-stack-frame?)
  (children generated-range-children))

;; /**
;;  * The scopes a unit's source map gives (`source-map` in sourcemap.scm).
;;  * @property {string|boolean} file - The file of the source's scopes, or #f.
;;  * @property {original-scope} root - The file's scope.
;;  * @property {list} ranges - The unit's outermost ranges.
;;  */
(define-record-type unit-scopes
  (make-unit-scopes file root ranges)
  unit-scopes?
  (file unit-scopes-file)
  (root unit-scopes-root)
  (ranges unit-scopes-ranges))

;; /**
;;  * Items the emitter renders whose lines are in a block of the function
;;  * they are in (`render-items` in emit.scm).
;;  * @property {original-scope} scope - The innermost scope they are in.
;;  * @property {list} items - The items.
;;  */
(define-record-type scoped-items
  (make-scoped-items scope items)
  scoped-items?
  (scope scoped-items-scope)
  (items scoped-items-items))

;; /**
;;  * Items the emitter renders that are a function, a factory or a unit: a
;;  * range for each of some scopes, the last a frame if the items are a
;;  * function's.
;;  * @property {list} scopes - The scopes, outermost first.
;;  * @property {boolean} frame? - Whether the last is a frame.
;;  * @property {procedure} binder - The JavaScript that reads a local there,
;;  *   or #f; a block inside takes it too.
;;  * @property {integer} column - Where in their first line's text the
;;  *   ranges begin.
;;  * @property {list} items - The items.
;;  */
(define-record-type ranged-items
  (make-ranged-items scopes frame? binder column items)
  ranged-items?
  (scopes ranged-items-scopes)
  (frame? ranged-items-frame?)
  (binder ranged-items-binder)
  (column ranged-items-column)
  (items ranged-items-items))

;; /**
;;  * A range still being rendered.
;;  * @property {original-scope} scope - The scope it is.
;;  * @property {procedure} binder - As for `ranged-items`.
;;  * @property {boolean} frame? - Whether it is a frame.
;;  * @property {boolean} block? - Whether it is a block of the function it is in.
;;  * @property {pair} start - Where it begins, (line . column).
;;  * @property {list} children - The ranges finished in it, latest first.
;;  */
(define-record-type open-range
  (make-open-range scope binder frame? block? start children)
  open-range?
  (scope open-range-scope)
  (binder open-range-binder)
  (frame? open-range-frame?)
  (block? open-range-block?)
  (start open-range-start)
  (children open-range-children))

;; /**
;;  * A range finished at a line.
;;  * @param {open-range} open - The range.
;;  * @param {integer} line - The line after its last.
;;  * @returns {generated-range}
;;  */
(define (finished-range open line)
  (let ((scope (open-range-scope open)))
    (make-generated-range (open-range-start open) (cons line 0) scope
                          (map (open-range-binder open) (original-scope-locals scope))
                          (open-range-frame? open)
                          (reverse (open-range-children open)))))

;; /**
;;  * An open range with one more range finished in it.
;;  * @param {open-range} open - The range.
;;  * @param {generated-range} child - The range finished in it.
;;  * @returns {open-range}
;;  */
(define (open-range-adding open child)
  (make-open-range (open-range-scope open) (open-range-binder open) (open-range-frame? open)
                   (open-range-block? open) (open-range-start open)
                   (cons child (open-range-children open))))

;; /**
;;  * The scopes a line in a scope is in below a function's, outermost first:
;;  * none for a line in the function's own scope.
;;  * @param {original-scope|boolean} scope - The line's innermost scope, or
;;  *   #f for the function's.
;;  * @param {original-scope} function-scope - The function's.
;;  * @returns {list}
;;  */
(define (blocks-between scope function-scope)
  (let up ((s scope) (blocks '()))
    (cond ((not s) '())
          ((eq? s function-scope) blocks)
          (else (up (original-scope-parent s) (cons s blocks))))))
