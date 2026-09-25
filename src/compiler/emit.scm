;;; emit.scm -- JavaScript for a lowered procedure.
;;;
;;; ## Two forms of every procedure
;;;
;;; A compiled procedure runs as straight-line JavaScript -- the *fast form*:
;;; ordinary `if`s, a non-tail call as a JavaScript call, a tail call returned
;;; to the trampoline as a `TailCall`. One live Scheme frame is one JavaScript
;;; frame, which is what lets a debugger show a Scheme stack.
;;;
;;; A JavaScript function cannot be resumed part-way through, and Scheme needs
;;; exactly that when a continuation captured beneath a compiled frame is
;;; invoked. So each procedure is also emitted as its *resumable form*, the
;;; twin: the same body as a state machine,
;;;
;;;     function name$r($pc, $f) {
;;;       let a, $t1, ..., $r;
;;;       ({ a, $t1, ..., $r } = $f);
;;;       for (;;) switch ($pc) {
;;;         case 0: ...            // ordinary entry
;;;         case 1: $t3 = $r; ...  // resumed just after call site 1
;;;       }
;;;     }
;;;
;;; in which every branch and every call ends a block, so every point the
;;; procedure can be suspended at begins one. When a callee reports a capture,
;;; the fast form saves its live locals and the block to resume at, and
;;; returns; the twin later restores them and carries on. The arrangement is
;;; Marshall's, after Pettyjohn et al.: a capture is signalled by *returning* a
;;; distinguished value rather than throwing, which costs orders of magnitude
;;; less on a JavaScript engine.
;;;
;;; The two forms differ only in control flow -- `if`, calls, captures, loop
;;; heads -- so one emitter produces both, with a mode. Everything else is the
;;; same code, and that is what keeps them agreeing: the fast form spills
;;; locals by name into a frame the twin restores by name, so both must give
;;; every value the same name, and they do because they allocate names in the
;;; same order.
;;;
;;; ## Statements are data
;;;
;;; The emitter builds statements as tagged lists and renders them to text
;;; last. An expression is a list of parts: a string is JavaScript text, and a
;;; symbol is a local the frame can hold. So which locals a statement reads is
;;; in the data, and the liveness that decides what a frame saves reads it
;;; there rather than scanning text -- where a local's name inside a string
;;; literal used to count as a read.
;;;
;;;   (assign target expr)     target = expr;   (a symbol target defines it)
;;;   (eval expr)              expr;
;;;   (return expr)            return expr;
;;;   (raw expr)               a statement written out whole, such as a loop
;;;   (text string)            a fixed line: a loop label, a closing brace
;;;   (if test then else)      a JavaScript if, in the fast form only
;;;   (goto n)                 $pc = n; continue;
;;;   (branch test n m)        to block n if test is not #f, else to m
;;;   (spill result n)         if result is the unwind sentinel, save the frame
;;;                            to resume at block n and return; with result #f,
;;;                            unconditionally
;;;   (guarded test stmts)     if (test) { stmts }, for a guarded loop jump
;;;   (suspend result n slots) the fast form's spill, into its twin's block n
;;;
;;; ## Nested procedures
;;;
;;; Every nested lambda is emitted once, at the top of the unit, as a factory
;;; over its free variables (`lift.scm`), and created by calling it. Both forms
;;; of a parent ask for the same node and get the same factory.

;; ---------------------------------------------------------------------------
;; JavaScript text
;; ---------------------------------------------------------------------------

;; /**
;;  * The JavaScript identifier for a renamed Scheme local. The analyzer's names
;;  * look like `x_$147`, already close; any character JavaScript does not allow
;;  * in an identifier becomes `_` and its code in hex.
;;  * @param {symbol} name - A renamed Scheme identifier.
;;  * @returns {string} A JavaScript identifier.
;;  */
(define (js-name name)
  (define (plain? c)
    (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\$)))
  (define (ascii-plain? c) (and (< (char->integer c) 128) (plain? c)))
  (let ((s (symbol->string name)))
    (string-append
      "s_"
      (if (string-every ascii-plain? s)
          s
          (apply string-append
                 (map (lambda (c)
                        (if (ascii-plain? c)
                            (string c)
                            (string-append "_" (number->string (char->integer c) 16))))
                      (string->list s)))))))

;; /**
;;  * The symbol that stands for a Scheme local in an expression.
;;  * @param {symbol} name - A renamed Scheme identifier.
;;  * @returns {symbol} Its JavaScript name, as a symbol.
;;  */
(define (js-local name) (string->symbol (js-name name)))

;; /**
;;  * A string as a JavaScript string literal, escaped as `JSON.stringify` does.
;;  * @param {string} s - The string.
;;  * @returns {string} The literal.
;;  */
(define (js-string s)
  (let ((out (open-output-string)))
    (write-char #\" out)
    (string-for-each
      (lambda (c)
        (let ((code (char->integer c)))
          (cond ((char=? c #\") (write-string "\\\"" out))
                ((char=? c #\\) (write-string "\\\\" out))
                ((= code 8) (write-string "\\b" out))
                ((= code 9) (write-string "\\t" out))
                ((= code 10) (write-string "\\n" out))
                ((= code 12) (write-string "\\f" out))
                ((= code 13) (write-string "\\r" out))
                ((< code 32)
                 (write-string "\\u00" out)
                 (if (< code 16) (write-char #\0 out))
                 (write-string (number->string code 16) out))
                (else (write-char c out)))))
      s)
    (write-char #\" out)
    (get-output-string out)))

;; /**
;;  * A flonum as a JavaScript number literal. Scheme writes an integral flonum
;;  * as `1.0`, which JavaScript reads the same, but JavaScript's own spelling
;;  * drops the `.0`, and that is kept. The infinities and NaN have no literal.
;;  * @param {number} x - An inexact real.
;;  * @returns {string} A JavaScript expression.
;;  */
(define (js-number x)
  (cond ((not (= x x)) "Number(\"NaN\")")
        ((= x (/ 1. 0)) "Number(\"Infinity\")")
        ((= x (/ -1. 0)) "Number(\"-Infinity\")")
        (else (let* ((s (number->string x))
                     (n (string-length s)))
                (if (and (> n 2) (string=? (substring s (- n 2) n) ".0"))
                    (substring s 0 (- n 2))
                    s)))))

;; ---------------------------------------------------------------------------
;; Expressions and statements
;; ---------------------------------------------------------------------------

;; /**
;;  * Builds an expression from parts: strings, locals as symbols, integers,
;;  * and other expressions, which are spliced in.
;;  * @param {...*} parts - The parts.
;;  * @returns {list} The expression.
;;  */
(define (js . parts)
  (append-map (lambda (part)
                (cond ((or (string? part) (symbol? part)) (list part))
                      ((number? part) (list (number->string part)))
                      (else part)))
              parts))

;; /**
;;  * An expression rendered as JavaScript text.
;;  * @param {list} expr - The expression.
;;  * @returns {string} Its text.
;;  */
(define (expr->string expr)
  (apply string-append
         (map (lambda (part) (if (symbol? part) (symbol->string part) part)) expr)))

;; /**
;;  * The locals an expression reads.
;;  * @param {list} expr - The expression.
;;  * @returns {list} The symbols in it.
;;  */
(define (expr-locals expr) (filter symbol? expr))

;; /**
;;  * Whether an expression is a literal or a lone variable -- something that
;;  * can be written twice without evaluating anything twice.
;;  * @param {list} expr - The expression.
;;  * @returns {boolean}
;;  */
(define (repeatable? expr)
  (and (pair? expr)
       (null? (cdr expr))
       (or (symbol? (car expr)) (literal-text? (car expr)))))

;; /**
;;  * Whether an expression is a literal, or a temporary: something no parameter
;;  * assignment can change, so a loop's jump can assign it without first
;;  * copying it aside.
;;  * @param {list} expr - The expression.
;;  * @returns {boolean}
;;  */
(define (settled? expr)
  (and (pair? expr)
       (null? (cdr expr))
       (if (symbol? (car expr))
           (temporary? (car expr))
           (or (literal-text? (car expr)) (string=? (car expr) "undefined")))))

;; /**
;;  * Whether a symbol names one of this emitter's temporaries, `$t` and digits.
;;  * @param {symbol} sym - A local.
;;  * @returns {boolean}
;;  */
(define (temporary? sym)
  (let ((s (symbol->string sym)))
    (and (> (string-length s) 2)
         (string=? (substring s 0 2) "$t")
         (string-every char-numeric? (substring s 2 (string-length s))))))

;; /**
;;  * Whether text is a literal the emitter writes inline: an exact integer, a
;;  * `null`/`true`/`false`, or a reference into the constant pool.
;;  * @param {string} s - The text.
;;  * @returns {boolean}
;;  */
(define (literal-text? s)
  (define (digits? str) (and (> (string-length str) 0) (string-every char-numeric? str)))
  (let ((n (string-length s)))
    (or (member s '("null" "true" "false"))
        (let* ((unsigned (if (and (> n 0) (char=? (string-ref s 0) #\-)) (substring s 1 n) s))
               (m (string-length unsigned)))
          (or (digits? unsigned)
              (and (> m 1) (char=? (string-ref unsigned (- m 1)) #\n)
                   (digits? (substring unsigned 0 (- m 1))))))
        (and (> n 3)
             (string=? (substring s 0 2) "K[")
             (char=? (string-ref s (- n 1)) #\])
             (digits? (substring s 2 (- n 1)))))))

;; /**
;;  * A statement as JavaScript text.
;;  * @param {form} form - The emission it belongs to, which names the
;;  *   procedure and says what a spill saves.
;;  * @param {list} st - The statement.
;;  * @returns {string} Its text, which may span lines.
;;  */
(define (render-statement form st)
  (let ((expr (lambda (e) (expr->string e))))
    (case (car st)
      ((assign) (string-append (expr (cadr st)) " = " (expr (caddr st)) ";"))
      ((eval) (string-append (expr (cadr st)) ";"))
      ((return) (string-append "return " (expr (cadr st)) ";"))
      ((raw) (expr (cadr st)))
      ((text) (cadr st))
      ((if) (string-append "if (" (expr (cadr st)) " !== false) {\n"
                           (render-block form (caddr st)) "\n} else {\n"
                           (render-block form (cadddr st)) "\n}"))
      ((goto) (goto-text (cadr st)))
      ((branch) (string-append "if (" (expr (cadr st)) " !== false) { "
                               (goto-text (caddr st)) " } " (goto-text (cadddr st))))
      ((spill)
       (let ((reify (string-append "R.reify(" (form-name form) ", "
                                   (number->string (caddr st)) ", "
                                   (frame-literal (form-frame form (caddr st))) ");")))
         (if (cadr st)
             (string-append "if (" (expr (cadr st)) " === $UNWIND) { " reify " return $UNWIND; }")
             reify)))
      ((guarded) (string-append "if (" (expr (cadr st)) ") { "
                                (string-join (map (lambda (s) (render-statement form s)) (caddr st)) " ")
                                " }"))
      ((suspend)
       (let ((spill (string-append "R.reify(" (form-name form) "$r, " (number->string (caddr st))
                                   ", " (frame-literal (cadddr st)) "); return $UNWIND;")))
         (if (cadr st)
             (string-append "if (" (expr (cadr st)) " === $UNWIND) { " spill " }")
             spill)))
      (else (error "emit: unknown statement" st)))))

(define (goto-text n) (string-append "$pc = " (number->string n) "; continue;"))

(define (frame-literal slots)
  (string-append "{ " (string-join (map symbol->string slots) ", ") " }"))

;; /**
;;  * Statements rendered as the body of a JavaScript block, each indented.
;;  * @param {form} form - The emission.
;;  * @param {list} stmts - The statements.
;;  * @returns {string} The lines.
;;  */
(define (render-block form stmts)
  (string-join (map (lambda (s) (string-append "  " (render-statement form s))) stmts) "\n"))

;; ---------------------------------------------------------------------------
;; The unit
;; ---------------------------------------------------------------------------

;; /**
;;  * What every procedure in one compilation unit shares: the lifting plan,
;;  * how each global is reached, the constant pool, the factories emitted so
;;  * far, and where each call site resumes.
;;  *
;;  * A call site's resume block and saved locals are decided by the twin,
;;  * which is generated first, and read by the fast form, which has to spill
;;  * into exactly the frame the twin restores.
;;  */
(define-record-type unit
  (make-unit plan globals guarded constants factories emitted resume-points)
  unit?
  (plan unit-plan)
  (globals unit-globals)
  (guarded unit-guarded)
  (constants unit-constants set-unit-constants!)
  (factories unit-factories set-unit-factories!)
  (emitted unit-emitted set-unit-emitted!)
  (resume-points unit-resume-points set-unit-resume-points!))

;; /**
;;  * The JavaScript expression that reads a global: its cell's value, or, before
;;  * the first read resolves the cell -- or when the value is `null` or
;;  * `undefined`, which the cell cannot tell from unresolved -- its resolver's
;;  * answer. See `R.globalCell` for why a global is read through a cell.
;;  * @param {unit} u - The unit.
;;  * @param {symbol} name - The global.
;;  * @returns {string} The expression.
;;  */
(define (global-read u name)
  (let ((i (global-index u name)))
    (string-append "(C" i ".v ?? G" i "())")))

;; /**
;;  * A global's position in the unit's list, which numbers its accessor.
;;  * @param {unit} u - The unit.
;;  * @param {symbol} name - The global.
;;  * @returns {string} The position, as text.
;;  */
(define (global-index u name)
  (number->string (list-index (lambda (g) (eq? g name)) (unit-globals u))))

;; /**
;;  * A value that survived to compile time, as a JavaScript expression.
;;  *
;;  * Only immediates are written inline. Pairs, symbols, vectors and the like
;;  * go into the constant pool: they have identity `eq?` can observe, so
;;  * re-creating them per evaluation would be wrong as well as slow.
;;  *
;;  * @param {unit} u - The unit, holding the pool.
;;  * @param {*} v - The value.
;;  * @returns {string} A JavaScript expression.
;;  */
(define (constant u v)
  (cond ((null? v) "null")
        ((js-undefined? v) "undefined")
        ((eq? v #t) "true")
        ((eq? v #f) "false")
        ((exact-integer? v) (string-append (number->string v) "n"))
        ((and (real? v) (inexact? v)) (js-number v))
        ((string? v) (js-string v))
        (else
         (let ((index (length (unit-constants u))))
           (set-unit-constants! u (cons v (unit-constants u)))
           (string-append "K[" (number->string index) "]")))))

;; ---------------------------------------------------------------------------
;; One emission of one procedure
;; ---------------------------------------------------------------------------

;; /**
;;  * The state of one emission -- one form of one procedure.
;;  *
;;  * Each emission counts its temporaries from zero, the two forms of the same
;;  * procedure included. That is the point: they must arrive at the same name
;;  * for the same value, and they only do if neither inherits a count.
;;  *
;;  * `out` is the statements so far, most recent first -- in the twin, those of
;;  * the current block. `declared` is every local the emission introduced, most
;;  * recent first. `loop-targets` is the stack of loops a looping call can jump
;;  * to. `blocks` holds the twin's finished blocks by number, and `sites` its
;;  * suspension points with the block each resumes at.
;;  */
(define-record-type form
  (make-form name ir unit mode path counter labels loops loop-targets declared
             out blocks block-count current sites frames)
  form?
  (name form-name)
  (ir form-ir)
  (unit form-unit)
  (mode form-mode)
  (path form-path)
  (counter form-counter set-form-counter!)
  (labels form-labels set-form-labels!)
  (loops form-loops set-form-loops!)
  (loop-targets form-loop-targets set-form-loop-targets!)
  (declared form-declared set-form-declared!)
  (out form-out set-form-out!)
  (blocks form-blocks set-form-blocks!)
  (block-count form-block-count set-form-block-count!)
  (current form-current set-form-current!)
  (sites form-sites set-form-sites!)
  (frames form-frames set-form-frames!))

;; /**
;;  * A fresh emission of a procedure.
;;  * @param {string} name - The JavaScript function's name.
;;  * @param {list} ir - The procedure's lambda IR node.
;;  * @param {unit} u - The unit.
;;  * @param {symbol} mode - 'fast or 'twin.
;;  * @param {string} path - Where the procedure sits in the tree of procedures.
;;  * @returns {form} The emission.
;;  */
(define (new-form name ir u mode path)
  (make-form name ir u mode path 0 0 #f '() '() '() '() 1 0 '() '()))

(define (twin? form) (eq? (form-mode form) 'twin))
(define (plan-of form) (unit-plan (form-unit form)))

;; /**
;;  * Appends a statement to the emission.
;;  * @param {form} form - The emission.
;;  * @param {list} st - The statement.
;;  * @returns {unspecified}
;;  */
(define (emit! form st) (set-form-out! form (cons st (form-out form))))

;; /**
;;  * Records a local as introduced by this emission, keeping first-seen order.
;;  * @param {form} form - The emission.
;;  * @param {symbol} sym - The local.
;;  * @returns {unspecified}
;;  */
(define (declare! form sym)
  (if (not (memq sym (form-declared form)))
      (set-form-declared! form (cons sym (form-declared form)))))

;; /**
;;  * Takes the next number from the emission's counter, which names both
;;  * temporaries and nested procedures.
;;  * @param {form} form - The emission.
;;  * @returns {integer} The number.
;;  */
(define (next-number! form)
  (let ((n (form-counter form)))
    (set-form-counter! form (+ n 1))
    n))

;; /**
;;  * A fresh temporary.
;;  * @param {form} form - The emission.
;;  * @returns {symbol} Its name.
;;  */
(define (temp! form)
  (let ((sym (string->symbol (string-append "$t" (number->string (next-number! form))))))
    (declare! form sym)
    sym))

;; /**
;;  * The name and position of the next procedure nested directly in this one.
;;  * The name spells out where it sits -- `$fn2_0` is the first inside the
;;  * third inside the outermost -- which makes it unique in the unit without a
;;  * shared counter. It must be: a procedure refers to its own resumable form
;;  * by name, and a nested name that collided with its parent's would shadow it.
;;  * @param {form} form - The emission.
;;  * @returns {pair} (name . path).
;;  */
(define (nested-name! form)
  (let* ((index (number->string (next-number! form)))
         (path (if (string=? (form-path form) "") index
                   (string-append (form-path form) "_" index))))
    (cons (string-append "$fn" path) path)))

;; /**
;;  * The statements a thunk emits, collected apart from the emission's own and
;;  * returned in order. The fast form emits the branches of a JavaScript `if`
;;  * this way.
;;  * @param {form} form - The emission.
;;  * @param {procedure} thunk - Emits the statements.
;;  * @returns {list} The statements.
;;  */
(define (collect-statements form thunk)
  (let ((saved (form-out form)))
    (set-form-out! form '())
    (thunk)
    (let ((collected (reverse (form-out form))))
      (set-form-out! form saved)
      collected)))

;; --- Locals ----------------------------------------------------------------

(define (boxed-local? form name) (boxed? (plan-of form) name))

;; /**
;;  * An expression reading a local; a boxed one is read through its box.
;;  * @param {form} form - The emission.
;;  * @param {symbol} name - A renamed local.
;;  * @returns {list} The expression.
;;  */
(define (read-local form name)
  (if (boxed-local? form name)
      (js (js-local name) "[0]")
      (js (js-local name))))

;; /**
;;  * The statement binding a local to its first value. A boxed local's box is
;;  * made here, so every reader -- a closure, a resumed frame -- reaches it.
;;  * @param {form} form - The emission.
;;  * @param {symbol} name - A renamed local.
;;  * @param {list} value - Its value.
;;  * @returns {list} The statement.
;;  */
(define (bind-local form name value)
  (declare! form (js-local name))
  (list 'assign (js (js-local name))
        (if (boxed-local? form name) (js "[" value "]") value)))

;; /**
;;  * Makes the boxes for the internal definitions in a body, before any of it
;;  * runs. A procedure defined earlier in a body can refer to one defined later,
;;  * so it has to be handed something that will hold that procedure once the
;;  * later definition runs. Only names some nested procedure refers to are
;;  * boxed, so an ordinary internal helper stays a plain variable. A loop that
;;  * stands for a fresh call makes them again each iteration.
;;  * @param {form} form - The emission.
;;  * @param {list} body - The body.
;;  * @returns {unspecified}
;;  */
(define (emit-define-boxes! form body)
  (for-each (lambda (def)
              (let ((name (cadr def)))
                (if (boxed-local? form name)
                    (begin (declare! form (js-local name))
                           (emit! form (list 'assign (js (js-local name)) (js "[undefined]")))))))
            (definitions-in body)))

;; /**
;;  * The internal definitions in a procedure's own body -- in any branch, but
;;  * not inside a nested procedure, which makes its own.
;;  * @param {list} node - An IR node.
;;  * @returns {list} The `define` nodes.
;;  */
(define (definitions-in node)
  (case (car node)
    ((lambda) '())
    ((define) (cons node (definitions-in (caddr node))))
    (else (append-map definitions-in (ir-children node)))))

;; ---------------------------------------------------------------------------
;; Expressions
;; ---------------------------------------------------------------------------

;; /**
;;  * Emits an IR node whose value is wanted, returning an expression for it.
;;  * @param {form} form - The emission.
;;  * @param {list} node - An IR node.
;;  * @returns {list} The expression.
;;  */
(define (emit-value! form node)
  (case (car node)
    ((const) (js (constant (form-unit form) (cadr node))))
    ((local) (read-local form (cadr node)))
    ;; Read through its cell on every reference, because a top-level binding
    ;; can be redefined after this code was compiled -- by the REPL running it,
    ;; for one.
    ((global) (js (global-read (form-unit form) (cadr node))))
    ((lambda) (emit-closure! form node))
    ((set) (emit-assignment! form node))
    ((define) (emit-definition! form node))
    ((seq) (fold (lambda (expr last) (emit-value! form expr)) (js "undefined") (cadr node)))
    ((if) (if (twin? form) (emit-twin-value-if! form node) (emit-value-if! form node)))
    ((let)
     (declare! form (js-local (cadr node)))
     (let ((init (emit-value! form (caddr node))))
       (emit! form (bind-local form (cadr node) init))
       (emit-value! form (cadddr node))))
    ((letrec)
     (emit-letrec-bindings! form node)
     (emit-value! form (cadddr node)))
    ((capture) (emit-capture! form node))
    ((call) (or (emit-inline! form node) (emit-call! form node)))
    (else (error "emit: cannot emit IR node" (car node)))))

;; /**
;;  * Creates a nested procedure by calling its factory with its free variables.
;;  * @param {form} form - The emission.
;;  * @param {list} lam - A lambda IR node.
;;  * @returns {list} The expression holding the procedure.
;;  */
(define (emit-closure! form lam)
  (let* ((named (nested-name! form))
         (factory (factory-for! (form-unit form) (car named) (cdr named) lam))
         (args (map js-local (plan-free-of (plan-of form) lam)))
         (result (temp! form)))
    (emit! form (list 'assign (js result)
                      (js factory "(" (join-exprs (map js args) ", ") ")")))
    (js result)))

;; /**
;;  * Expressions joined by a separator, as one expression.
;;  * @param {list} exprs - The expressions.
;;  * @param {string} separator - The separator.
;;  * @returns {list} The expression.
;;  */
(define (join-exprs exprs separator)
  (if (null? exprs)
      '()
      (fold (lambda (e acc) (js acc separator e)) (car exprs) (cdr exprs))))

;; /**
;;  * Emits `set!`, whose value is unspecified.
;;  * @param {form} form - The emission.
;;  * @param {list} node - A `set` IR node.
;;  * @returns {list} The expression for its value.
;;  */
(define (emit-assignment! form node)
  (let ((value (emit-value! form (cadddr node)))
        (name (cadr node)))
    (emit! form (if (caddr node)
                    (list 'assign (read-local form name) value)
                    (list 'eval (js "E.set(" (js-string (symbol->string name)) ", " value ")"))))
    (js "undefined")))

;; /**
;;  * Emits an internal definition. A boxed name's box already exists -- made
;;  * at the head of the body, since something defined earlier may hold it --
;;  * so the definition fills it rather than making one.
;;  * @param {form} form - The emission.
;;  * @param {list} node - A `define` IR node.
;;  * @returns {list} The expression for its value.
;;  */
(define (emit-definition! form node)
  (let ((name (cadr node)))
    (if (boxed-local? form name)
        (begin
          (declare! form (js-local name))
          (let ((value (emit-value! form (caddr node))))
            (emit! form (list 'assign (js (js-local name) "[0]") value))))
        (let ((value (emit-value! form (caddr node))))
          (emit! form (bind-local form name value))))
    (js "undefined")))

;; /**
;;  * Emits a `letrec` group's bindings.
;;  *
;;  * Every name is declared, and every boxed name's box made, before any
;;  * initializer, so an initializer can refer to a sibling in either order: a
;;  * name a sibling refers to is boxed exactly so that the sibling, built first,
;;  * can be handed something that will hold it later. Every initializer is a
;;  * lambda, so their order is unobservable.
;;  * @param {form} form - The emission.
;;  * @param {list} node - A `letrec` IR node.
;;  * @returns {unspecified}
;;  */
(define (emit-letrec-bindings! form node)
  (let ((names (cadr node)))
    (for-each (lambda (name) (declare! form (js-local name))) names)
    (for-each (lambda (name)
                (if (boxed-local? form name)
                    (emit! form (list 'assign (js (js-local name)) (js "[undefined]")))))
              names)
    (for-each (lambda (name init)
                (let ((value (emit-value! form init)))
                  (emit! form (list 'assign
                                    (if (boxed-local? form name)
                                        (js (js-local name) "[0]")
                                        (js (js-local name)))
                                    value))))
              names (caddr node))))

;; /**
;;  * Emits an `if` whose value is wanted, in the fast form: a JavaScript `if`
;;  * assigning a temporary in each branch.
;;  * @param {form} form - The emission.
;;  * @param {list} node - An `if` IR node.
;;  * @returns {list} The expression holding its value.
;;  */
(define (emit-value-if! form node)
  (let* ((test (emit-value! form (cadr node)))
         (result (temp! form))
         (arm (lambda (branch)
                (collect-statements form
                  (lambda ()
                    (let ((value (emit-value! form branch)))
                      (emit! form (list 'assign (js result) value)))))))
         (then (arm (caddr node)))
         (other (arm (cadddr node))))
    (emit! form (list 'if test then other))
    (js result)))

;; /**
;;  * Attempts to expand a call to a primitive inline, returning #f when the
;;  * call is not one: the operator must be a global with an expansion at this
;;  * argument count, bound to its primitive when this was compiled.
;;  *
;;  * The expansion is guarded on the binding, since Scheme allows the
;;  * primitive to be redefined afterwards: it is used while the primitive's
;;  * cell says the name has never been rebound, or failing that while the
;;  * binding is the primitive anyway, and otherwise the call goes to whatever
;;  * the name is bound to now. Operands are evaluated into temporaries first,
;;  * since an expansion may mention one more than once.
;;  *
;;  * @param {form} form - The emission.
;;  * @param {list} node - A `call` IR node.
;;  * @returns {list|boolean} The expression holding the value, or #f.
;;  */
(define (emit-inline! form node)
  (let* ((fn (cadr node))
         (u (form-unit form))
         (entry (and (eq? (car fn) 'global)
                     (memq (cadr fn) (unit-guarded u))
                     (inline-expansion (cadr fn) (caddr node)))))
    (and entry
         (let* ((operands (map-in-order (lambda (arg)
                                 (let ((value (emit-value! form arg)))
                                   (if (repeatable? value)
                                       value
                                       (let ((t (temp! form)))
                                         (emit! form (list 'assign (js t) value))
                                         (js t)))))
                               (caddr node)))
                (index (global-index u (cadr fn)))
                (read (global-read u (cadr fn)))
                (shape ((caddr entry) operands))
                (fast ((cadddr entry) operands))
                (binding (js "(W" index ".intact || " read " === P" index ")"))
                (result (temp! form)))
           (emit! form (list 'assign (js result)
                             (js (if shape (js binding " && (" shape ")") binding)
                                 " ? (" fast ") : R.callBinding(" read ", ["
                                 (join-exprs operands ", ") "])")))
           (js result)))))

;; /**
;;  * Emits a call whose value is wanted.
;;  *
;;  * A compiled procedure or a primitive takes Scheme values directly. An
;;  * interpreted closure is a callable function too, but calling it that way
;;  * enters Scheme from JavaScript and converts -- exact integers to doubles --
;;  * so it is called through its raw entry. Which kind the callee is belongs to
;;  * the value, not the name, so it is tested at the call. The callee may
;;  * return a pending tail call, which is run out here, or report a capture,
;;  * which this frame then joins.
;;  *
;;  * @param {form} form - The emission.
;;  * @param {list} node - A `call` IR node.
;;  * @returns {list} The expression holding the value.
;;  */
(define (emit-call! form node)
  (let* ((args (map-in-order (lambda (arg) (emit-value! form arg)) (caddr node)))
         (callee (temp! form))
         (raw (temp! form))
         (result (temp! form))
         (fn (emit-value! form (cadr node)))
         (arglist (join-exprs args ", ")))
    (emit! form (list 'assign (js callee) fn))
    (emit! form (list 'assign (js raw) (js callee "[$RAW]")))
    (emit! form (list 'assign (js result)
                      (js raw " === undefined ? " callee "(" arglist ") : " raw "(" arglist ")")))
    (emit! form (list 'raw (js "while (" result " instanceof $TailCall) { "
                               result " = $step(" result "); }")))
    (if (twin? form)
        (resume-after! form node result)
        (emit! form (suspension form node (js result))))
    (js result)))

;; /**
;;  * Emits a continuation capture. It always suspends; the value the
;;  * continuation is later invoked with arrives where the procedure resumes.
;;  * @param {form} form - The emission.
;;  * @param {list} node - A `capture` IR node.
;;  * @returns {list} The expression holding the captured value.
;;  */
(define (emit-capture! form node)
  (let* ((receiver (emit-value! form (cadr node)))
         (result (temp! form)))
    (emit! form (list 'eval (js "R.capture(" receiver ")")))
    (if (twin? form)
        (let ((resume (new-block! form)))
          (note-resume-site! form node resume)
          (emit! form (list 'spill #f resume))
          (emit! form (list 'return (js "$UNWIND")))
          (switch-to! form resume)
          (emit! form (list 'assign (js result) (js "$r"))))
        (emit! form (suspension form node #f)))
    (js result)))

;; /**
;;  * The fast form's statement for when a callee reports a capture beneath this
;;  * frame: save the locals the twin needs at the block it resumes at, and pass
;;  * the report outward, so every frame between the capture and the
;;  * interpreter records itself.
;;  * @param {form} form - The emission.
;;  * @param {list} node - The call or capture IR node.
;;  * @param {list|boolean} result - The call's result, or #f for a capture,
;;  *   which suspends unconditionally.
;;  * @returns {list} The statement.
;;  */
(define (suspension form node result)
  (let ((point (assq node (unit-resume-points (form-unit form)))))
    (if point
        (list 'suspend result (cadr point) (cddr point))
        ;; No resumable form to suspend into. Refusing is the point: carrying on
        ;; with the sentinel as the call's value would assemble an answer from a
        ;; continuation with a hole in it.
        (list 'text (if result
                        (string-append "if (" (expr->string result)
                                       " === $UNWIND) R.captureWithoutResume();")
                        "R.captureWithoutResume();")))))

;; ---------------------------------------------------------------------------
;; Statements
;; ---------------------------------------------------------------------------

;; /**
;;  * Emits an IR node as a statement. A node in tail position ends the
;;  * procedure; anything else contributes its effects and its value is
;;  * discarded.
;;  * @param {form} form - The emission.
;;  * @param {list} node - An IR node.
;;  * @returns {unspecified}
;;  */
(define (emit-statement! form node)
  (if (not (node-tail? node))
      (emit! form (list 'eval (emit-value! form node)))
      (case (car node)
        ((if) (if (twin? form) (emit-twin-tail-if! form node) (emit-tail-if! form node)))
        ((seq)
         (if (null? (cadr node))
             (emit! form (list 'return (js "undefined")))
             (for-each (lambda (expr) (emit-statement! form expr)) (cadr node))))
        ((let)
         (let ((init (emit-value! form (caddr node))))
           (emit! form (bind-local form (cadr node) init))
           (emit-statement! form (cadddr node))))
        ((letrec)
         (if (letrec-inline? node)
             (emit-inline-loop! form node)
             (begin (emit-letrec-bindings! form node)
                    (emit-statement! form (cadddr node)))))
        ((call) (emit-tail-call! form node))
        (else (emit! form (list 'return (emit-value! form node)))))))

;; /**
;;  * Whether an IR node is in tail position. Every node records it, in the
;;  * field after its own.
;;  * @param {list} node - An IR node.
;;  * @returns {boolean}
;;  */
(define (node-tail? node)
  (case (car node)
    ((const local global capture) (caddr node))
    ((call) (cadddr node))
    ((set) (car (cddddr node)))
    ((define) (cadddr node))
    ((if) (car (cddddr node)))
    ((seq) (caddr node))
    ((lambda) (cadr (cddddr node)))
    ((let letrec) (car (cddddr node)))
    (else #f)))

(define (letrec-inline? node) (eq? (list-ref node 6) #t))

;; /**
;;  * A tail `if` in the fast form: a JavaScript `if` whose branches each end
;;  * the procedure.
;;  * @param {form} form - The emission.
;;  * @param {list} node - An `if` IR node.
;;  * @returns {unspecified}
;;  */
(define (emit-tail-if! form node)
  (let* ((test (emit-value! form (cadr node)))
         (then (collect-statements form (lambda () (emit-statement! form (caddr node)))))
         (other (collect-statements form (lambda () (emit-statement! form (cadddr node))))))
    (emit! form (list 'if test then other))))

;; /**
;;  * Emits a call in tail position.
;;  *
;;  * A primitive needs no trampoline: it cannot tail-call, so its value is
;;  * this procedure's value, returned directly. A call the lowering found to be
;;  * to the loop it is in reassigns that loop's parameters and jumps back to
;;  * its head; a call to this procedure's own global does so only while the
;;  * global still names this procedure. Anything else returns a `TailCall`, so
;;  * whoever is trampolining -- compiled or interpreted -- continues it, and
;;  * tail recursion runs in constant space.
;;  *
;;  * @param {form} form - The emission.
;;  * @param {list} node - A `call` IR node in tail position.
;;  * @returns {unspecified}
;;  */
(define (emit-tail-call! form node)
  (let ((inlined (emit-inline! form node)))
    (if inlined
        (emit! form (list 'return inlined))
        (let* ((fn (emit-value! form (cadr node)))
               (args (map-in-order (lambda (arg) (emit-value! form arg)) (caddr node)))
               (kind (call-loop node))
               (target (loop-target form)))
          (cond
            ((and kind (loop-fixed-arity? target) (= (length args) (length (loop-params target))))
             (let ((jump (loop-back! form args target)))
               (if (loop-procedure? target) (set-form-loops! form #t))
               (if (eq? kind 'local)
                   (for-each (lambda (st) (emit! form st)) jump)
                   (begin
                     (emit! form (list 'guarded (js fn " === " (procedure-name form)) jump))
                     (emit-trampoline-return! form fn args)))))
            (else (emit-trampoline-return! form fn args)))))))

(define (emit-trampoline-return! form fn args)
  (emit! form (list 'return (js "new $TailCall(" fn ", [" (join-exprs args ", ") "])"))))

;; /**
;;  * The identifier of this procedure's fast form, which a global self-call is
;;  * compared against: it is what the global holds.
;;  * @param {form} form - The emission.
;;  * @returns {string} The identifier.
;;  */
(define (procedure-name form)
  (let* ((name (form-name form)) (n (string-length name)))
    (if (and (twin? form) (> n 2) (string=? (substring name (- n 2) n) "$r"))
        (substring name 0 (- n 2))
        name)))

;; ---------------------------------------------------------------------------
;; Loops
;; ---------------------------------------------------------------------------
;;
;; A loop target is what a looping tail call jumps to: this procedure itself,
;; or a loop emitted inline inside it. Reassigning parameters in place is sound
;; because every nested procedure receives its free variables by value or by
;; box, so a closure made in one iteration keeps that iteration's bindings.

(define-record-type loop-target
  (make-loop-target params fixed-arity procedure assign jump)
  loop-target?
  (params loop-params)
  (fixed-arity loop-fixed-arity?)
  (procedure loop-procedure?)
  (assign loop-assign)
  (jump loop-jump))

;; /**
;;  * The loop a looping call made here jumps to: the innermost inline loop, or
;;  * else this procedure. The lowering tags a call as looping only from inside
;;  * the lambda it loops to, and an inline loop's body is that lambda's body.
;;  * @param {form} form - The emission.
;;  * @returns {loop-target} The target.
;;  */
(define (loop-target form)
  (if (pair? (form-loop-targets form))
      (car (form-loop-targets form))
      (let ((ir (form-ir form)))
        (make-loop-target
          (lambda-params ir)
          (not (lambda-rest ir))
          #t
          ;; The fast form boxes its parameters at the top of its loop, as on
          ;; entry. The twin receives them from a frame already boxed and never
          ;; boxes on entry, so it gives a boxed parameter a fresh box here --
          ;; either way each iteration has its own, as a fresh call would.
          (lambda (param value)
            (list 'assign (js (js-local param))
                  (if (and (twin? form) (boxed-local? form param)) (js "[" value "]") value)))
          (if (twin? form) (list 'goto 0) (list 'text "continue $loop;"))))))

;; /**
;;  * The statements that make a tail call a jump: new values into the
;;  * parameters, then the jump. Every argument is evaluated before any
;;  * parameter is assigned, since an argument may read a parameter an earlier
;;  * assignment would clobber -- a loop that swaps two arguments is the plain
;;  * case. A value no assignment can disturb is used as it is, and a parameter
;;  * passed back unchanged is not assigned at all.
;;  * @param {form} form - The emission.
;;  * @param {list} args - The argument expressions.
;;  * @param {loop-target} target - The loop.
;;  * @returns {list} The statements, in order.
;;  */
(define (loop-back! form args target)
  (let walk ((args args) (params (loop-params target)) (copies '()) (assigns '()))
    (if (null? args)
        (append (reverse copies) (reverse assigns) (list (loop-jump target)))
        (let ((value (car args)) (param (car params)))
          (cond ((and (equal? value (js (js-local param))) (not (boxed-local? form param)))
                 (walk (cdr args) (cdr params) copies assigns))
                ((settled? value)
                 (walk (cdr args) (cdr params) copies
                       (cons ((loop-assign target) param value) assigns)))
                (else
                 (let ((t (temp! form)))
                   (walk (cdr args) (cdr params)
                         (cons (list 'assign (js t) value) copies)
                         (cons ((loop-assign target) param (js t)) assigns)))))))))

;; /**
;;  * A call's loop tag: 'local or 'global when it is a tail call to the loop it
;;  * is in, else #f. The calls lowering synthesizes carry no tag at all.
;;  * @param {list} node - A `call` IR node.
;;  * @returns {symbol|boolean} The tag.
;;  */
(define (call-loop node)
  (let ((rest (cddddr node)))
    (and (pair? rest) (car rest))))

;; /**
;;  * Emits a `letrec` loop inside this procedure rather than as a procedure of
;;  * its own. The lowering marks a group so when its one lambda is only ever
;;  * running as a loop entered from here (`inline-loop?` in `ir.scm`), so its
;;  * parameters become this procedure's locals, entering is their first
;;  * assignment, and its looping calls jump back to its head. Nothing is
;;  * allocated to enter it -- for `assq`, whose loop usually runs twice,
;;  * entering was nearly the whole cost. The head does what a fresh call would:
;;  * boxes boxed parameters and makes the boxes for internal definitions.
;;  * @param {form} form - The emission.
;;  * @param {list} node - A `letrec` IR node marked inline.
;;  * @returns {unspecified}
;;  */
(define (emit-inline-loop! form node)
  (let* ((lam (car (caddr node)))
         (params (lambda-params lam))
         (entry (map-in-order (lambda (arg) (emit-value! form arg)) (caddr (cadddr node)))))
    (for-each (lambda (param value)
                (declare! form (js-local param))
                (emit! form (list 'assign (js (js-local param)) value)))
              params entry)
    (let ((target (enter-inline-loop! form params)))
      (for-each (lambda (param)
                  (if (boxed-local? form param)
                      (emit! form (list 'assign (js (js-local param)) (js "[" (js-local param) "]")))))
                params)
      (emit-define-boxes! form (lambda-body lam))
      (set-form-loop-targets! form (cons target (form-loop-targets form)))
      (emit-statement! form (lambda-body lam))
      (set-form-loop-targets! form (cdr (form-loop-targets form)))
      ;; The fast form's loop is a labelled `for`, closed here; every path
      ;; through its body has returned or jumped back by then. The twin's head
      ;; is a block, and its blocks already end in jumps.
      (if (not (twin? form)) (emit! form (list 'text "}"))))))

;; /**
;;  * Opens an inline loop and returns its target. In the fast form it is a
;;  * labelled `for`; in the twin its head is a block of its own, since block
;;  * zero is the procedure's entry, which a loop inside it must not re-run.
;;  * @param {form} form - The emission.
;;  * @param {list} params - The loop's parameters.
;;  * @returns {loop-target} The target.
;;  */
(define (enter-inline-loop! form params)
  (let ((plain (lambda (param value) (list 'assign (js (js-local param)) value))))
    (if (twin? form)
        (let ((head (new-block! form)))
          (emit! form (list 'goto head))
          (switch-to! form head)
          (make-loop-target params #t #f plain (list 'goto head)))
        (let ((label (begin (set-form-labels! form (+ (form-labels form) 1))
                            (string-append "$loop" (number->string (form-labels form))))))
          (emit! form (list 'text (string-append label ": for (;;) {")))
          (make-loop-target params #t #f plain (list 'text (string-append "continue " label ";")))))))

;; ---------------------------------------------------------------------------
;; The twin's blocks
;; ---------------------------------------------------------------------------

;; /**
;;  * Allocates a block of the twin, returning its number, which is its `$pc`.
;;  * @param {form} form - The emission.
;;  * @returns {integer} The block number.
;;  */
(define (new-block! form)
  (let ((n (form-block-count form)))
    (set-form-block-count! form (+ n 1))
    n))

;; /**
;;  * Finishes the current block and directs further statements into another.
;;  * A block is never returned to once left, so finishing it is final.
;;  * @param {form} form - The emission.
;;  * @param {integer} n - The block to continue in.
;;  * @returns {unspecified}
;;  */
(define (switch-to! form n)
  (set-form-blocks! form (cons (cons (form-current form) (reverse (form-out form)))
                               (form-blocks form)))
  (set-form-current! form n)
  (set-form-out! form '()))

;; /**
;;  * Records a suspension point and the block it resumes at, where the fast
;;  * form of this procedure will find it.
;;  * @param {form} form - The emission.
;;  * @param {list} node - The call or capture IR node.
;;  * @param {integer} block - The resume block.
;;  * @returns {unspecified}
;;  */
(define (note-resume-site! form node block)
  (set-form-sites! form (cons (cons node block) (form-sites form))))

;; /**
;;  * Ends a call in the twin: if the callee reported a capture, spill and
;;  * return; otherwise pass the value to the block after the call, which is
;;  * also where a resumed frame receives it.
;;  * @param {form} form - The emission.
;;  * @param {list} node - The call IR node.
;;  * @param {symbol} result - The temporary holding the call's value.
;;  * @returns {unspecified}
;;  */
(define (resume-after! form node result)
  (let ((resume (new-block! form)))
    (note-resume-site! form node resume)
    (emit! form (list 'spill (js result) resume))
    (emit! form (list 'assign (js "$r") (js result)))
    (emit! form (list 'goto resume))
    (switch-to! form resume)
    (emit! form (list 'assign (js result) (js "$r")))))

;; /**
;;  * A tail `if` in the twin: a branch to one block per arm.
;;  * @param {form} form - The emission.
;;  * @param {list} node - An `if` IR node.
;;  * @returns {unspecified}
;;  */
(define (emit-twin-tail-if! form node)
  (let* ((test (emit-value! form (cadr node)))
         (then (new-block! form))
         (other (new-block! form)))
    (emit! form (list 'branch test then other))
    (switch-to! form then)
    (emit-statement! form (caddr node))
    (switch-to! form other)
    (emit-statement! form (cadddr node))))

;; /**
;;  * An `if` whose value is wanted, in the twin: each arm assigns a temporary
;;  * and jumps to a join block. The assignment is emitted after the arm's
;;  * value, into whatever block is current then -- a call in the arm resumes in
;;  * a block of its own, and the value is only available there.
;;  * @param {form} form - The emission.
;;  * @param {list} node - An `if` IR node.
;;  * @returns {list} The expression holding its value.
;;  */
(define (emit-twin-value-if! form node)
  (let* ((test (emit-value! form (cadr node)))
         (result (temp! form))
         (then (new-block! form))
         (other (new-block! form))
         (join (new-block! form))
         (arm (lambda (block branch)
                (switch-to! form block)
                (let ((value (emit-value! form branch)))
                  (emit! form (list 'assign (js result) value))
                  (emit! form (list 'goto join))))))
    (emit! form (list 'branch test then other))
    (arm then (caddr node))
    (arm other (cadddr node))
    (switch-to! form join)
    (js result)))

;; ---------------------------------------------------------------------------
;; Whole procedures
;; ---------------------------------------------------------------------------

;; /**
;;  * The fast form of a procedure, as a JavaScript function declaration.
;;  *
;;  * A boxed parameter arrives as a plain value and is boxed on entry, so the
;;  * body, its closures and any frame it spills all reach one binding. A
;;  * rest parameter arrives as JavaScript arguments and is made a Scheme list.
;;  * A procedure with a tail call to itself runs its body in a loop that takes
;;  * in everything a fresh call would redo; only the declarations stay outside.
;;  *
;;  * @param {string} name - The function's name.
;;  * @param {list} ir - The lambda IR node.
;;  * @param {unit} u - The unit.
;;  * @param {string} path - Its position in the tree of procedures.
;;  * @returns {string} JavaScript source.
;;  */
(define (fast-form name ir u path)
  (let ((form (new-form name ir u 'fast path)))
    (emit-define-boxes! form (lambda-body ir))
    (emit-statement! form (lambda-body ir))
    (let* ((rest (lambda-rest ir))
           (params (map js-name (lambda-params ir)))
           (signature (string-join (append params
                                           (if rest (list (string-append "..." (js-name rest) "$raw")) '()))
                                   ", "))
           (prologue
             (append
               (if rest
                   (let ((list-expr (string-append "R.listFrom(" (js-name rest) "$raw)")))
                     (list (string-append "let " (js-name rest) " = "
                                          (if (boxed-local? form rest)
                                              (string-append "[" list-expr "]")
                                              list-expr)
                                          ";")))
                   '())
               (map (lambda (p) (string-append (js-name p) " = [" (js-name p) "];"))
                    (filter (lambda (p) (boxed-local? form p)) (lambda-params ir)))))
           (declared (reverse (form-declared form)))
           (declaration (if (null? declared)
                            '()
                            (list (string-append "let " (string-join (map symbol->string declared) ", ") ";"))))
           (body (map (lambda (st) (render-statement form st)) (reverse (form-out form))))
           (indent (lambda (lines) (map (lambda (l) (string-append "  " l)) lines)))
           (lines (if (form-loops form)
                      (append declaration (list "$loop: for (;;) {")
                              (indent prologue) (indent body) (list "}"))
                      (append declaration prologue body))))
      (string-append "function " name "(" signature ") {\n"
                     (string-join (indent lines) "\n") "\n}"))))

;; /**
;;  * The resumable form of a procedure, as a JavaScript function declaration.
;;  *
;;  * It takes no argument list: everything, a rest parameter included, arrives
;;  * in the frame `$f`. Generating it records, in the unit, where each call site
;;  * resumes and which locals a frame suspended there must save -- those live
;;  * at the resume block, and no others -- and the fast form reads both.
;;  * Restoring names every local; one that was not saved comes back undefined,
;;  * which is safe because it is dead there.
;;  *
;;  * @param {string} name - The function's name.
;;  * @param {list} ir - The lambda IR node.
;;  * @param {unit} u - The unit.
;;  * @param {string} path - Its position in the tree of procedures.
;;  * @returns {string} JavaScript source.
;;  */
(define (twin-form name ir u path)
  (let ((form (new-form name ir u 'twin path)))
    ;; In block zero, so a fresh entry makes the boxes and a resume, which
    ;; always enters later, takes them from the frame.
    (emit-define-boxes! form (lambda-body ir))
    (emit-statement! form (lambda-body ir))
    (switch-to! form -1)
    (let* ((blocks (map cdr (sort-blocks (filter (lambda (b) (>= (car b) 0)) (form-blocks form)))))
           (rest (lambda-rest ir))
           (slots (delete-duplicates (append (reverse (form-declared form))
                                             (map js-local (lambda-params ir))
                                             (if rest (list (js-local rest)) '()))
                                     eq?))
           (live (live-in blocks))
           (sites (reverse (form-sites form))))
      (set-form-frames! form
        (map (lambda (site)
               (let ((at (vector-ref live (cdr site))))
                 (cons (cdr site) (filter (lambda (s) (memq s at)) slots))))
             sites))
      ;; The fast form spills into the frame this one restores, so each of its
      ;; suspension points saves exactly what this one expects there.
      (for-each (lambda (site)
                  (set-unit-resume-points! u
                    (cons (cons (car site) (cons (cdr site) (form-frame form (cdr site))))
                          (unit-resume-points u))))
                sites)
      (let ((names (string-join (map symbol->string (append slots '($r))) ", "))
            (cases (let number ((blocks blocks) (i 0))
                     (if (null? blocks)
                         '()
                         (cons (string-append
                                 "      case " (number->string i) ":\n"
                                 (string-join (map (lambda (st) (string-append "        " (render-statement form st)))
                                                   (car blocks))
                                              "\n"))
                               (number (cdr blocks) (+ i 1)))))))
        (string-append "function " name "($pc, $f) {\n"
                       "  let " names ";\n"
                       "  ({ " names " } = $f);\n"
                       "  for (;;) switch ($pc) {\n"
                       (string-join cases "\n") "\n"
                       "      default: throw new Error('" name ": bad resume point ' + $pc);\n"
                       "  }\n"
                       "}")))))

;; /**
;;  * The locals a frame suspended at a resume block saves.
;;  * @param {form} form - A twin emission, after liveness.
;;  * @param {integer} block - The resume block.
;;  * @returns {list} The locals, in declaration order.
;;  */
(define (form-frame form block)
  (cond ((assv block (form-frames form)) => cdr) (else '())))

;; /**
;;  * Blocks sorted by number.
;;  * @param {list} blocks - (number . statements) pairs.
;;  * @returns {list} The same pairs, in order.
;;  */
(define (sort-blocks blocks)
  (let ((table (make-vector (length blocks) #f)))
    (for-each (lambda (b) (vector-set! table (car b) b)) blocks)
    (vector->list table)))

;; ---------------------------------------------------------------------------
;; Nested procedures and the unit
;; ---------------------------------------------------------------------------

;; /**
;;  * The factory for a nested procedure, emitting it the first time it is asked
;;  * for. Both forms of a parent ask for the same node and get the same one.
;;  * Recorded before emitting, so anything reached while emitting it finds the
;;  * name instead of starting again.
;;  * @param {unit} u - The unit.
;;  * @param {string} proc - The nested procedure's name.
;;  * @param {string} path - Its position in the tree of procedures.
;;  * @param {list} lam - Its lambda IR node.
;;  * @returns {string} The factory's name.
;;  */
(define (factory-for! u proc path lam)
  (or (cond ((assq lam (unit-emitted u)) => cdr) (else #f))
      (let ((factory (string-append "$mk" proc)))
        (set-unit-emitted! u (cons (cons lam factory) (unit-emitted u)))
        (let ((source (render-factory u factory proc path lam)))
          (set-unit-factories! u (cons source (unit-factories u))))
        factory)))

;; /**
;;  * A nested procedure as a factory over its free variables. Both forms go
;;  * inside, and the closure they share is what the factory returns. A `letrec`
;;  * name the procedure refers to only itself by is declared here and assigned
;;  * before returning, so the recursive call resolves lexically -- a named
;;  * `let` is this shape, and usually a hot loop.
;;  * @param {unit} u - The unit.
;;  * @param {string} factory - The factory's name.
;;  * @param {string} proc - The procedure's name.
;;  * @param {string} path - Its position in the tree of procedures.
;;  * @param {list} lam - Its lambda IR node.
;;  * @returns {string} JavaScript source.
;;  */
(define (render-factory u factory proc path lam)
  (let* ((plan (unit-plan u))
         (params (map js-name (plan-free-of plan lam)))
         (own (map js-name (plan-self-of plan lam)))
         (twin (twin-form (string-append proc "$r") lam u path))
         (fast (fast-form proc lam u path))
         (lines (append
                  (if (null? own) '() (list (string-append "let " (string-join own ", ") ";")))
                  (list fast
                        (string-append "R.markProcedure(" proc ", " (js-string (or (lambda-name lam) "anonymous")) ");")
                        twin
                        (string-append proc ".$resume = " proc "$r;"))
                  (map (lambda (self) (string-append self " = " proc ";")) own)
                  (list (string-append "return " proc ";")))))
    (string-append "function " factory "(" (string-join params ", ") ") {\n"
                   (string-join (map (lambda (l) (string-append "  " l)) lines) "\n")
                   "\n}")))

;; /**
;;  * The runtime values call sites use, each with the local generated code
;;  * names it by.
;;  *
;;  * Read from `R` once per procedure rather than at every call site: loading a
;;  * property of the runtime module on every call was worth up to 1.08x on
;;  * call-heavy code.
;;  */
(define runtime-constants
  '(("$TailCall" . "R.TailCall") ("$step" . "R.step")
    ("$UNWIND" . "R.UNWIND") ("$RAW" . "R.SCHEME_RAW_CALL")))

;; /**
;;  * The declaration of the runtime values a procedure's code uses.
;;  *
;;  * Found by searching the code rather than recorded as it is emitted, since
;;  * the names are fixed and cannot be mistaken for anything the emitter
;;  * generates. A string constant spelling one out only declares a local the
;;  * code does not use.
;;  *
;;  * @param {string} code - The procedure's code.
;;  * @returns {string} A `const` declaration, or "" if it uses none.
;;  */
(define (runtime-prelude code)
  (let ((used (filter (lambda (c) (string-contains code (car c))) runtime-constants)))
    (if (null? used)
        ""
        (string-append "const "
                       (string-join (map (lambda (c) (string-append (car c) " = " (cdr c))) used) ", ")
                       ";"))))

;; /**
;;  * Generates the JavaScript for one lowered top-level procedure.
;;  *
;;  * The result is the body of a function taking the runtime `R`, the
;;  * environment `E` globals resolve in, and the constant pool `K`, and
;;  * returning the procedure. Each global is read through a cell, which its
;;  * resolver `G` finds on the first read, so a forward reference compiles and
;;  * a later redefinition or assignment is seen. A global with an inline
;;  * expansion also gets its primitive's cell and the primitive, for the
;;  * expansion's guard.
;;  *
;;  * @param {list} ir - The procedure's lambda IR node.
;;  * @param {list} globals - The globals it references, as symbols.
;;  * @param {string} name - Its display name.
;;  * @param {list} guarded - The globals with an expansion that are bound to
;;  *   their primitive here, which the caller finds out from the environment.
;;  * @returns {list} (source constants).
;;  */
(define (generate-unit ir globals name guarded)
  (let* ((u (make-unit (plan-lifting ir) globals guarded '() '() '() '()))
         ;; The twin first: generating it decides where each call site resumes
         ;; and what a frame saves there, which the fast form needs in order to
         ;; suspend itself.
         (twin (twin-form "$proc$r" ir u ""))
         (fast (fast-form "$proc" ir u ""))
         (accessors
           (string-join
             (map (lambda (g)
                    (let ((i (number->string (list-index (lambda (x) (eq? x g)) globals)))
                          (literal (js-string (symbol->string g))))
                      (string-append
                        "let C" i " = R.UNRESOLVED; const G" i " = () => (C" i " = R.globalCell(E, " literal ")).v;"
                        (if (memq g guarded)
                            (string-append "\nconst W" i " = R.primitiveCell(" literal "), P" i " = W" i ".primitive;")
                            ""))))
                  globals)
             "\n"))
         (factories (string-join (reverse (unit-factories u)) "\n")))
    (list (string-join (filter (lambda (s) (not (string=? s "")))
                               (list (runtime-prelude (string-append factories fast twin))
                                     accessors factories fast twin
                                     (string-append "R.markProcedure($proc, " (js-string name) ");")
                                     "$proc.$resume = $proc$r;"
                                     "return $proc;"))
                       "\n")
          (reverse (unit-constants u)))))
