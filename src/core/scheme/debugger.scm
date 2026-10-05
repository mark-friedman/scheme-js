;; The debugger
;;
;; What a program under the debugger is doing, and what it does next: its
;; breakpoints and which one a location hits, the procedure calls it is in,
;; whether it runs, is paused or is being stepped and whether a step or an
;; exception stops it, which compiled procedure or macro transformer holds a
;; location -- where no breakpoint can fire -- what a frame's bindings show,
;; and the REPL's debug commands.
;;
;; A runtime (src/debug/scheme_debug_runtime.js) holds a `debugger` record and
;; calls these procedures from the evaluator's hooks. What only the host can do
;; it gives as a `debugger-host`, whose procedures are called here: after
;; every change, with what the host keeps for the evaluator to read at every
;; step; to release the asynchronous run waiting on a pause; to say the program
;; resumed, or paused and where; and to list an environment's bindings, or the
;; compiled procedures and transformers there are.
;;
;; A location is a source span as the reader makes it, a JavaScript object
;; with a `filename`, a `line`, a `column` and perhaps an `endLine` and
;; `endColumn`; JavaScript's null or undefined, or #f, is no location.

;; ---------------------------------------------------------------------------
;; Small helpers
;; ---------------------------------------------------------------------------
;;
;; This library loads beside the library system, with (scheme core) and
;; (scheme control) alone, so it has no SRFI 1 or 13: these are the few parts
;; of them it needs.

;; /**
;;  * The first element of a list a predicate holds of, or #f.
;;  * @param {procedure} found? - The predicate.
;;  * @param {list} items - The list.
;;  * @returns {*}
;;  */
(define (first-that found? items)
  (cond ((null? items) #f)
        ((found? (car items)) (car items))
        (else (first-that found? (cdr items)))))

;; /**
;;  * A list without its first element a predicate holds of.
;;  * @param {procedure} found? - The predicate.
;;  * @param {list} items - The list.
;;  * @returns {list}
;;  */
(define (without-first found? items)
  (cond ((null? items) '())
        ((found? (car items)) (cdr items))
        (else (cons (car items) (without-first found? (cdr items))))))

;; /**
;;  * The words of a string, as split by whitespace.
;;  * @param {string} text - The string.
;;  * @returns {list} The words, as strings.
;;  */
(define (words text)
  (let split ((chars (string->list text)) (word '()) (done '()))
    (let ((ended (if (null? word) done (cons (list->string (reverse word)) done))))
      (cond ((null? chars) (reverse ended))
            ((char-whitespace? (car chars)) (split (cdr chars) '() ended))
            (else (split (cdr chars) (cons (car chars) word) done))))))

;; /**
;;  * A string without the whitespace at its ends.
;;  * @param {string} text - The string.
;;  * @returns {string}
;;  */
(define (trimmed text)
  (let* ((chars (string->list text))
         (drop (lambda (chars)
                 (let loop ((chars chars))
                   (if (and (pair? chars) (char-whitespace? (car chars))) (loop (cdr chars)) chars)))))
    (list->string (reverse (drop (reverse (drop chars)))))))

;; /**
;;  * What follows the first word of a string, its spacing kept.
;;  * @param {string} text - The string, with no whitespace before its first
;;  *   word.
;;  * @returns {string}
;;  */
(define (after-first-word text)
  (let skip ((chars (string->list text)))
    (cond ((null? chars) "")
          ((char-whitespace? (car chars)) (trimmed (list->string chars)))
          (else (skip (cdr chars))))))

;; /**
;;  * Strings joined, each after a separator but the first.
;;  * @param {list} strings - The strings.
;;  * @param {string} separator - The separator.
;;  * @returns {string}
;;  */
(define (joined strings separator)
  (if (null? strings)
      ""
      (apply string-append
             (car strings)
             (map (lambda (s) (string-append separator s)) (cdr strings)))))

;; /**
;;  * A value as `write` writes it.
;;  * @param {*} value - The value.
;;  * @returns {string}
;;  */
(define (written value)
  (let ((port (open-output-string)))
    (write value port)
    (get-output-string port)))

;; ---------------------------------------------------------------------------
;; Locations
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether a value is a location: not #f, and not JavaScript's null, which
;;  * is the empty list here, or undefined.
;;  * @param {*} value - The value.
;;  * @returns {boolean}
;;  */
(define (location? value)
  (not (or (not value) (null? value) (js-undefined? value))))

;; /**
;;  * A line or column, as an exact integer, or #f if there is none:
;;  * JavaScript gives a line as a number, inexact here, and no column as
;;  * null or undefined.
;;  * @param {*} value - The line or column.
;;  * @returns {integer|boolean}
;;  */
(define (position value)
  (and (real? value) (exact (round value))))

;; /**
;;  * A location's file, or #f.
;;  */
(define (location-filename location)
  (let ((filename (js-ref location "filename")))
    (and (string? filename) filename)))

;; /**
;;  * A location's line, first column, last line and the column after it, each
;;  * as an exact integer or #f.
;;  */
(define (location-line location) (position (js-ref location "line")))
(define (location-column location) (position (js-ref location "column")))
(define (location-end-line location) (position (js-ref location "endLine")))
(define (location-end-column location) (position (js-ref location "endColumn")))

;; /**
;;  * A location as `file:line`, or "unknown location".
;;  * @param {*} location - The location, or no location.
;;  * @returns {string}
;;  */
(define (location-text location)
  (if (and (location? location) (location-filename location) (location-line location))
      (string-append (location-filename location) ":" (number->string (location-line location)))
      "unknown location"))

;; /**
;;  * Whether a span holds a location: a whole line, if no column is given.
;;  * The span's end column is the column after its last, as the reader
;;  * records it.
;;  * @param {*} span - The span, or no location.
;;  * @param {string} filename - The location's file.
;;  * @param {integer} line - Its line.
;;  * @param {integer|boolean} column - Its column, or #f for the whole line.
;;  * @returns {boolean}
;;  */
(define (span-contains? span filename line column)
  (and (location? span)
       (equal? (location-filename span) filename)
       (let* ((first-line (location-line span))
              (last-line (or (location-end-line span) first-line))
              (first-column (location-column span))
              (end-column (location-end-column span)))
         (and first-line
              (<= first-line line last-line)
              (or (not column)
                  (and (or (not (= line first-line)) (not first-column) (>= column first-column))
                       (or (not (= line last-line)) (not end-column) (< column end-column))))))))

;; /**
;;  * How many lines a span covers.
;;  */
(define (span-lines span)
  (+ 1 (- (or (location-end-line span) (location-line span)) (location-line span))))

;; /**
;;  * Of named spans, the one of fewest lines that holds a location -- the
;;  * innermost, should a redefinition leave two holding it -- or #f.
;;  * @param {list} named - The spans, as (name . span).
;;  * @param {string} filename - The location's file.
;;  * @param {integer} line - Its line.
;;  * @param {integer|boolean} column - Its column, or #f.
;;  * @returns {pair|boolean} (name . span), or #f.
;;  */
(define (innermost-holding named filename line column)
  (let loop ((named named) (best #f))
    (cond ((null? named) best)
          ((and (span-contains? (cdar named) filename line column)
                (or (not best) (< (span-lines (cdar named)) (span-lines (cdr best)))))
           (loop (cdr named) (car named)))
          (else (loop (cdr named) best)))))

;; ---------------------------------------------------------------------------
;; The host, and the debugger
;; ---------------------------------------------------------------------------

;; /**
;;  * What only the host can do, as procedures.
;;  * @property {procedure} changed - Called after every change, with whether
;;  *   debugging is enabled, whether the program is being debugged, whether
;;  *   it is paused and whether it was aborted, which the evaluator reads.
;;  * @property {procedure} release - Lets the asynchronous run waiting on a
;;  *   pause go on.
;;  * @property {procedure} resumed - Says the program resumed, and how: as a
;;  *   string, "resume", "stepInto", "stepOver" or "stepOut".
;;  * @property {procedure} paused - Says the program paused, with what
;;  *   `pause-info` makes.
;;  * @property {procedure} bindings - An environment's own bindings, as a list
;;  *   of (name . value).
;;  * @property {procedure} compiled-procedures - The compiled procedures that
;;  *   do not run as closures while the program is debugged, as a list of
;;  *   (name . span).
;;  * @property {procedure} transformers - The macro transformers, as a list of
;;  *   (name . span).
;;  */
(define-record-type debugger-host
  (make-debugger-host changed release resumed paused bindings compiled-procedures transformers)
  debugger-host?
  (changed host-changed)
  (release host-release)
  (resumed host-resumed)
  (paused host-paused)
  (bindings host-bindings)
  (compiled-procedures host-compiled-procedures)
  (transformers host-transformers))

;; /**
;;  * A breakpoint: where it is, the column #f for a whole line.
;;  */
(define-record-type breakpoint
  (make-breakpoint id filename line column)
  breakpoint?
  (id breakpoint-id)
  (filename breakpoint-filename)
  (line breakpoint-line)
  (column breakpoint-column))

;; /**
;;  * A procedure call the program is in: the procedure's name, where it is,
;;  * the environment of the call, and how many tail calls have replaced it.
;;  */
(define-record-type activation
  (make-activation name source env tail-calls)
  activation?
  (name activation-name)
  (source activation-source)
  (env activation-env)
  (tail-calls activation-tail-calls))

;; /**
;;  * A program's debugger.
;;  * @property {debugger-host} host - Its host.
;;  * @property {boolean} enabled? - Whether debugging is on.
;;  * @property {list} breakpoints - The breakpoints, in the order set.
;;  * @property {integer} next-breakpoint - The number the next one's id has.
;;  * @property {list} activations - The calls the program is in, newest
;;  *   first.
;;  * @property {integer} depth - How many.
;;  * @property {symbol} mode - running, paused, or stepping into, over or
;;  *   out: `into`, `over` or `out`.
;;  * @property {integer|boolean} target-depth - For a step over or out, the
;;  *   depth it began at.
;;  * @property {string|boolean} reason - Why it is paused.
;;  * @property {*} data - The id of the breakpoint it paused at, or #f.
;;  * @property {boolean} aborted? - Whether the run was aborted.
;;  * @property {boolean} breaks-on-caught? - Whether an exception a handler
;;  *   will catch pauses it.
;;  * @property {boolean} breaks-on-uncaught? - Whether one none will does.
;;  * @property {integer|boolean} selected-frame - The REPL's selected frame,
;;  *   counted from the oldest, or #f for the newest.
;;  */
(define-record-type debugger
  (%make-debugger host enabled? breakpoints next-breakpoint activations depth mode target-depth
                  reason data aborted? breaks-on-caught? breaks-on-uncaught? selected-frame)
  debugger?
  (host debugger-host)
  (enabled? debugger-enabled? %set-debugger-enabled!)
  (breakpoints debugger-breakpoints set-debugger-breakpoints!)
  (next-breakpoint debugger-next-breakpoint set-debugger-next-breakpoint!)
  (activations debugger-activations set-debugger-activations!)
  (depth debugger-depth set-debugger-depth!)
  (mode debugger-mode set-debugger-mode!)
  (target-depth debugger-target-depth set-debugger-target-depth!)
  (reason debugger-pause-reason set-debugger-pause-reason!)
  (data debugger-pause-data set-debugger-pause-data!)
  (aborted? debugger-aborted? set-debugger-aborted!)
  (breaks-on-caught? debugger-breaks-on-caught? %set-debugger-breaks-on-caught!)
  (breaks-on-uncaught? debugger-breaks-on-uncaught? %set-debugger-breaks-on-uncaught!)
  (selected-frame debugger-selected-frame set-debugger-selected-frame!))

;; /**
;;  * A debugger, disabled, running, with no breakpoints, which pauses on an
;;  * exception no handler will catch.
;;  * @param {debugger-host} host - Its host.
;;  * @returns {debugger}
;;  */
(define (make-debugger host)
  (%make-debugger host #f '() 1 '() 0 'running #f #f #f #f #f #t #f))

;; /**
;;  * Whether the program is being debugged: debugging is on, and there is a
;;  * breakpoint to stop at, a step in progress, or a pause. While it is,
;;  * compiled code runs as the closures it replaced, so that every breakpoint
;;  * can fire; with none of those, debugging costs nothing.
;;  * @param {debugger} dbg - The debugger.
;;  * @returns {boolean}
;;  */
(define (debugger-debugging? dbg)
  (and (debugger-enabled? dbg)
       (or (pair? (debugger-breakpoints dbg))
           (not (eq? (debugger-mode dbg) 'running)))))

;; /**
;;  * Tells the host what it keeps for the evaluator, after a change.
;;  * @param {debugger} dbg - The debugger.
;;  */
(define (debugger-changed! dbg)
  ((host-changed (debugger-host dbg))
   (debugger-enabled? dbg) (debugger-debugging? dbg) (debugger-paused? dbg) (debugger-aborted? dbg)))

;; /**
;;  * Turns debugging on or off.
;;  */
(define (set-debugger-enabled! dbg enabled?)
  (%set-debugger-enabled! dbg enabled?)
  (debugger-changed! dbg))

;; /**
;;  * Forgets every breakpoint, call, step and pause, and the exception
;;  * settings.
;;  */
(define (reset-debugger! dbg)
  (set-debugger-breakpoints! dbg '())
  (set-debugger-activations! dbg '())
  (set-debugger-depth! dbg 0)
  (set-debugger-aborted! dbg #f)
  (%set-debugger-breaks-on-caught! dbg #f)
  (%set-debugger-breaks-on-uncaught! dbg #t)
  (set-debugger-selected-frame! dbg #f)
  (run! dbg)
  ((host-release (debugger-host dbg)))
  (debugger-changed! dbg))

;; ---------------------------------------------------------------------------
;; Breakpoints
;; ---------------------------------------------------------------------------

;; /**
;;  * Sets a breakpoint.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} filename - Its file.
;;  * @param {number} line - Its line, from one.
;;  * @param {*} column - Its column, from one, or none for the whole line.
;;  * @returns {string} Its id.
;;  */
(define (add-breakpoint! dbg filename line column)
  (let* ((n (debugger-next-breakpoint dbg))
         (id (string-append "bp-" (number->string n))))
    (set-debugger-next-breakpoint! dbg (+ n 1))
    (set-debugger-breakpoints!
     dbg (append (debugger-breakpoints dbg)
                 (list (make-breakpoint id filename (position line) (position column)))))
    (debugger-changed! dbg)
    id))

;; /**
;;  * The breakpoint with an id, or #f.
;;  */
(define (breakpoint-with-id dbg id)
  (and (string? id)
       (first-that (lambda (bp) (string=? (breakpoint-id bp) id)) (debugger-breakpoints dbg))))

;; /**
;;  * Removes a breakpoint.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} id - Its id.
;;  * @returns {boolean} Whether there was one.
;;  */
(define (remove-breakpoint! dbg id)
  (let ((found (breakpoint-with-id dbg id)))
    (and found
         (begin
           (set-debugger-breakpoints! dbg (without-first (lambda (bp) (eq? bp found))
                                                         (debugger-breakpoints dbg)))
           (debugger-changed! dbg)
           #t))))

;; /**
;;  * Removes every breakpoint.
;;  */
(define (clear-breakpoints! dbg)
  (set-debugger-breakpoints! dbg '())
  (debugger-changed! dbg))

;; /**
;;  * The breakpoint at a file, line and column, or #f: one on the line, at the
;;  * column or at none. The line and column may be JavaScript's numbers,
;;  * inexact here, as the evaluator's hook passes them.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} filename - The file.
;;  * @param {number} line - The line.
;;  * @param {number|boolean} column - The column, or #f.
;;  * @returns {breakpoint|boolean}
;;  */
(define (breakpoint-hit dbg filename line column)
  (first-that (lambda (bp)
                (and (string=? (breakpoint-filename bp) filename)
                     (= (breakpoint-line bp) line)
                     (or (not (breakpoint-column bp))
                         (and (real? column) (= (breakpoint-column bp) column)))))
              (debugger-breakpoints dbg)))

;; /**
;;  * The breakpoint a location hits, or #f.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {*} location - The location, or no location.
;;  * @returns {breakpoint|boolean}
;;  */
(define (breakpoint-at dbg location)
  (and (location? location)
       (location-filename location)
       (location-line location)
       (breakpoint-hit dbg (location-filename location) (location-line location)
                       (location-column location))))

;; ---------------------------------------------------------------------------
;; The calls a program is in
;; ---------------------------------------------------------------------------
;;
;; The evaluator says when a call to a closure begins and ends, and when it
;; begins in tail position, where it replaces the call it was made from: the
;; stack then neither misreports a tail-recursive loop as recursion nor grows
;; with it.

;; /**
;;  * A call begun.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} name - The procedure's name.
;;  * @param {*} source - Where it is, or no location.
;;  * @param {*} env - The call's environment.
;;  */
(define (enter-activation! dbg name source env)
  (set-debugger-activations! dbg (cons (make-activation name source env 0) (debugger-activations dbg)))
  (set-debugger-depth! dbg (+ (debugger-depth dbg) 1)))

;; /**
;;  * A call begun in tail position, replacing the newest.
;;  */
(define (replace-activation! dbg name source env)
  (let ((activations (debugger-activations dbg)))
    (if (null? activations)
        (enter-activation! dbg name source env)
        (set-debugger-activations!
         dbg (cons (make-activation name source env (+ 1 (activation-tail-calls (car activations))))
                   (cdr activations))))))

;; /**
;;  * The newest call returned.
;;  */
(define (exit-activation! dbg)
  (let ((activations (debugger-activations dbg)))
    (if (pair? activations)
        (begin
          (set-debugger-activations! dbg (cdr activations))
          (set-debugger-depth! dbg (- (debugger-depth dbg) 1))))))

;; ---------------------------------------------------------------------------
;; Running, paused and stepping
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether the program is paused.
;;  */
(define (debugger-paused? dbg)
  (eq? (debugger-mode dbg) 'paused))

;; /**
;;  * Sets the mode, with no step's depth and no pause's reason.
;;  */
(define (enter-mode! dbg mode)
  (set-debugger-mode! dbg mode)
  (set-debugger-target-depth! dbg #f)
  (set-debugger-pause-reason! dbg #f)
  (set-debugger-pause-data! dbg #f))

;; /**
;;  * Runs, stepping no more.
;;  */
(define (run! dbg)
  (enter-mode! dbg 'running))

;; /**
;;  * The program resumed, which the host is told after releasing the run.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} how - "resume", "stepInto", "stepOver" or "stepOut".
;;  */
(define (resumed! dbg how)
  (debugger-changed! dbg)
  ((host-release (debugger-host dbg)))
  ((host-resumed (debugger-host dbg)) how))

;; /**
;;  * Begins a step: into whatever runs next, or over or out of the call at
;;  * the current depth.
;;  */
(define (step! dbg mode how)
  (enter-mode! dbg mode)
  (if (not (eq? mode 'into)) (set-debugger-target-depth! dbg (debugger-depth dbg)))
  (resumed! dbg how))

(define (step-into! dbg) (step! dbg 'into "stepInto"))
(define (step-over! dbg) (step! dbg 'over "stepOver"))
(define (step-out! dbg) (step! dbg 'out "stepOut"))

;; /**
;;  * Resumes the program.
;;  */
(define (resume! dbg)
  (run! dbg)
  (resumed! dbg "resume"))

;; /**
;;  * Aborts the run: released, it stops, which is not a resumption the host
;;  * is told of.
;;  */
(define (abort! dbg)
  (set-debugger-aborted! dbg #t)
  (run! dbg)
  (debugger-changed! dbg)
  ((host-release (debugger-host dbg))))

;; /**
;;  * Pauses, for a reason, with what goes with it.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} reason - Why.
;;  * @param {*} data - The id of the breakpoint paused at, or #f.
;;  */
(define (pause! dbg reason data)
  (enter-mode! dbg 'paused)
  (set-debugger-pause-reason! dbg reason)
  (set-debugger-pause-data! dbg data))

;; /**
;;  * Whether the step in progress stops here: a step into at whatever is
;;  * next, over at the depth it began or shallower, out only shallower.
;;  * @param {debugger} dbg - The debugger.
;;  * @returns {boolean}
;;  */
(define (step-stops? dbg)
  (case (debugger-mode dbg)
    ((into) #t)
    ((over) (<= (debugger-depth dbg) (debugger-target-depth dbg)))
    ((out) (< (debugger-depth dbg) (debugger-target-depth dbg)))
    (else #f)))

;; /**
;;  * Whether to pause before the evaluator's next step: at a breakpoint
;;  * there, or where a step stops, with debugging on. Asked at every step, so
;;  * given the location's parts as the hook reads them, not the location.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} filename - The step's file.
;;  * @param {number} line - Its line.
;;  * @param {number|boolean} column - Its column, or #f.
;;  * @returns {boolean}
;;  */
(define (should-pause? dbg filename line column)
  (and (debugger-enabled? dbg)
       (string? filename)
       (real? line)
       (or (and (breakpoint-hit dbg filename line column) #t)
           (step-stops? dbg))))

;; /**
;;  * What the host is told of a pause, as the JavaScript object it gives the
;;  * backend.
;;  */
(define (pause-info dbg reason breakpoint location env exception continuable?)
  (js-obj "reason" reason
          "breakpointId" (or breakpoint '())
          "source" (if (location? location) location '())
          "stack" (activations->js dbg)
          "env" (or env '())
          "exception" exception
          "continuable" continuable?))

;; /**
;;  * Pauses before the evaluator's next step, and tells the host. With no
;;  * reason given, the reason is the breakpoint the location hits, or else
;;  * the step in progress.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {*} location - Where, or no location.
;;  * @param {*} env - The environment there, or #f.
;;  * @param {string|boolean} reason - Why, or #f.
;;  */
(define (pause-at! dbg location env reason)
  (let* ((hit (and (not reason) (breakpoint-at dbg location)))
         (reason (cond (reason reason)
                       (hit "breakpoint")
                       ((memq (debugger-mode dbg) '(into over out)) "step")
                       (else "breakpoint")))
         (id (and hit (breakpoint-id hit))))
    (pause! dbg reason id)
    (debugger-changed! dbg)
    ((host-paused (debugger-host dbg)) (pause-info dbg reason id location env js-undefined #f))))

;; /**
;;  * Pauses at an exception raised, and tells the host.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {*} location - Where it was raised, or no location.
;;  * @param {*} env - The environment there.
;;  * @param {*} exception - What was raised.
;;  * @param {boolean} continuable? - Whether it was raised continuably.
;;  * @returns {boolean} #t.
;;  */
(define (pause-on-exception! dbg location env exception continuable?)
  (pause! dbg "exception" #f)
  (debugger-changed! dbg)
  ((host-paused (debugger-host dbg)) (pause-info dbg "exception" #f location env exception continuable?))
  #t)

;; ---------------------------------------------------------------------------
;; Exceptions
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether an exception raised pauses the program, with debugging on: one a
;;  * handler will catch if asked to, by default not; one none will, unless
;;  * asked not to.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {boolean} caught? - Whether a handler will catch it.
;;  * @returns {boolean}
;;  */
(define (breaks-on-exception? dbg caught?)
  (and (debugger-enabled? dbg)
       (if caught? (debugger-breaks-on-caught? dbg) (debugger-breaks-on-uncaught? dbg))))

(define (set-debugger-breaks-on-caught! dbg breaks?)
  (%set-debugger-breaks-on-caught! dbg breaks?))

(define (set-debugger-breaks-on-uncaught! dbg breaks?)
  (%set-debugger-breaks-on-uncaught! dbg breaks?))

;; ---------------------------------------------------------------------------
;; Where a breakpoint cannot fire
;; ---------------------------------------------------------------------------
;;
;; A breakpoint inside a compiled procedure that does not run as a closure
;; while the program is debugged, or inside a macro transformer -- which runs
;; while code is expanded, before any of it runs, where the debugger cannot
;; wait -- is accepted and never fires; the REPL says so rather than leave a
;; debugger that seems broken. Worked out when asked, from what there is
;; then, so one set before its procedure was compiled or its macro defined is
;; reported too.

;; /**
;;  * The compiled procedure, of those the host lists, innermost around a
;;  * location, as the JavaScript object `{name, source}`, or null.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} filename - The location's file.
;;  * @param {number} line - Its line.
;;  * @param {*} column - Its column, or none.
;;  */
(define (compiled-procedure-at dbg filename line column)
  (named-span->js (innermost-holding ((host-compiled-procedures (debugger-host dbg)))
                                     filename (position line) (position column))))

;; /**
;;  * The transformer innermost around a location, as `compiled-procedure-at`
;;  * finds a procedure.
;;  */
(define (transformer-at dbg filename line column)
  (named-span->js (innermost-holding ((host-transformers (debugger-host dbg)))
                                     filename (position line) (position column))))

;; /**
;;  * (name . span) as the JavaScript object `{name, source}`, or #f as null.
;;  */
(define (named-span->js named)
  (if named (js-obj "name" (car named) "source" (cdr named)) '()))

;; /**
;;  * Why a breakpoint will not fire, as the REPL says it, or #f.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} filename - Its file.
;;  * @param {integer} line - Its line.
;;  * @param {integer|boolean} column - Its column, or #f.
;;  * @returns {pair|boolean} (where . name): where is `procedure` or
;;  *   `transformer`, or #f if it can fire.
;;  */
(define (unfireable dbg filename line column)
  (let ((compiled (innermost-holding ((host-compiled-procedures (debugger-host dbg))) filename line column)))
    (if compiled
        (cons 'procedure (car compiled))
        (let ((transformer (innermost-holding ((host-transformers (debugger-host dbg)))
                                              filename line column)))
          (and transformer (cons 'transformer (car transformer)))))))

;; ---------------------------------------------------------------------------
;; What the host's JavaScript reads
;; ---------------------------------------------------------------------------

;; /**
;;  * The breakpoints, as an array of `{id, filename, line, column}`, the
;;  * column null for a whole line.
;;  */
(define (breakpoints->js dbg)
  (list->vector
   (map (lambda (bp)
          (js-obj "id" (breakpoint-id bp)
                  "filename" (breakpoint-filename bp)
                  "line" (inexact (breakpoint-line bp))
                  "column" (if (breakpoint-column bp) (inexact (breakpoint-column bp)) '())))
        (debugger-breakpoints dbg))))

;; /**
;;  * The calls the program is in, oldest first, as an array of
;;  * `{name, source, env, tcoCount}`.
;;  */
(define (activations->js dbg)
  (list->vector
   (map (lambda (activation)
          (js-obj "name" (activation-name activation)
                  "source" (if (location? (activation-source activation)) (activation-source activation) '())
                  "env" (activation-env activation)
                  "tcoCount" (inexact (activation-tail-calls activation))))
        (reverse (debugger-activations dbg)))))

;; /**
;;  * The run's state, as `{state, reason, data}`: the state "running",
;;  * "paused" or "stepping".
;;  */
(define (pause-state->js dbg)
  (js-obj "state" (case (debugger-mode dbg)
                    ((running) "running")
                    ((paused) "paused")
                    (else "stepping"))
          "reason" (or (debugger-pause-reason dbg) '())
          "data" (or (debugger-pause-data dbg) '())))

;; ---------------------------------------------------------------------------
;; The REPL
;; ---------------------------------------------------------------------------
;;
;; A debug command is a line beginning with a colon, which comes back as the
;; text to show -- or, for `:eval`, as an expression for the host to evaluate
;; in the selected frame's environment, which only the host can analyze
;; there. Frames are numbered from the oldest, #0.

;; /**
;;  * Whether a line is a debug command.
;;  * @param {string} line - The line.
;;  * @returns {boolean}
;;  */
(define (debugger-command? line)
  (let ((text (trimmed line)))
    (and (> (string-length text) 0) (char=? (string-ref text 0) #\:))))

;; /**
;;  * The REPL's selected frame, from the oldest, or #f if there are none.
;;  */
(define (selected-frame dbg)
  (let ((depth (debugger-depth dbg))
        (selected (debugger-selected-frame dbg)))
    (and (> depth 0)
         (if selected (min selected (- depth 1)) (- depth 1)))))

;; /**
;;  * The activation numbered so, from the oldest.
;;  */
(define (activation-numbered dbg n)
  (list-ref (debugger-activations dbg) (- (debugger-depth dbg) 1 n)))

;; /**
;;  * Selects the newest frame again.
;;  */
(define (reset-frame-selection! dbg)
  (set-debugger-selected-frame! dbg #f))

;; /**
;;  * The text of a run's evaluation in a frame, or of its failure.
;;  * @param {string} text - The value, as the REPL writes it, or the message.
;;  * @returns {string}
;;  */
(define (eval-answer text) (string-append ";; result: " text))
(define (eval-failure message) (string-append ";; Error during eval: " message))

;; /**
;;  * Runs a debug command.
;;  * @param {debugger} dbg - The debugger.
;;  * @param {string} line - The command, `:` and all.
;;  * @returns {string|object} The text to show, or `{expression, env}` for
;;  *   the host to evaluate.
;;  */
(define (debugger-command dbg line)
  (let* ((text (trimmed line))
         (body (trimmed (substring text 1 (string-length text))))
         (parts (words body))
         (name (if (null? parts) "" (string-downcase (car parts))))
         (args (if (null? parts) '() (cdr parts))))
    (cond ((assoc name command-table)
           => (lambda (entry) ((cdr entry) dbg args (after-first-word body))))
          (else (string-append ";; Unknown debug command: " name ". Type :help for commands.")))))

;; /**
;;  * A command that only a paused program can take.
;;  * @param {procedure} run - What it does when paused, given the debugger.
;;  * @returns {procedure} The command.
;;  */
(define (when-paused run)
  (lambda (dbg args rest)
    (if (debugger-paused? dbg) (run dbg args rest) ";; Not paused")))

(define (debug-command dbg args rest)
  (cond ((null? args)
         (string-append ";; Debugging is " (if (debugger-enabled? dbg) "ON" "OFF")))
        ((string=? (string-downcase (car args)) "on")
         (set-debugger-enabled! dbg #t)
         ";; Debugging enabled")
        ((string=? (string-downcase (car args)) "off")
         (set-debugger-enabled! dbg #f)
         ;; In a browser, a run that is not being debugged holds the page
         ;; until it ends.
         (if (memq 'browser (features))
             ";; Debugging disabled\n;; WARNING: Fast Mode enabled. UI will freeze during long computations."
             ";; Debugging disabled"))
        (else ";; Usage: :debug on|off")))

(define (abort-command dbg args rest)
  (abort! dbg)
  ";; Evaluation aborted")

;; /**
;;  * A breakpoint's place, as `file:line` or `file:line:column`.
;;  */
(define (place-text filename line column)
  (string-append filename ":" (number->string line)
                 (if column (string-append ":" (number->string column)) "")))

(define (break-command dbg args rest)
  (if (< (length args) 2)
      ";; Usage: :break <file> <line> [column]"
      (let ((filename (car args))
            (line (string->number (cadr args)))
            (column (and (pair? (cddr args)) (string->number (caddr args)))))
        (if (not (exact-integer? line))
            ";; Invalid line number"
            (let* ((column (and (exact-integer? column) column))
                   (id (add-breakpoint! dbg filename line column))
                   (said (string-append ";; Breakpoint " id " set at " (place-text filename line column)))
                   (why (unfireable dbg filename line column)))
              (cond ((not why) said)
                    ((eq? (car why) 'procedure)
                     (string-append said "\n;; Warning: this is inside compiled procedure '" (cdr why)
                                    "', which does not stop at breakpoints -- it will not fire"))
                    (else
                     (string-append said "\n;; Warning: this is inside macro transformer '" (cdr why)
                                    "', which runs during expansion, where the debugger cannot stop -- it will not fire"))))))))

(define (unbreak-command dbg args rest)
  (cond ((null? args) ";; Usage: :unbreak <id>")
        ((remove-breakpoint! dbg (car args)) (string-append ";; Breakpoint " (car args) " removed"))
        (else (string-append ";; Breakpoint " (car args) " not found"))))

(define (breakpoints-command dbg args rest)
  (if (null? (debugger-breakpoints dbg))
      ";; No breakpoints set"
      (string-append
       ";; Breakpoints:\n"
       (joined
        (map (lambda (bp)
               (let* ((filename (breakpoint-filename bp))
                      (line (breakpoint-line bp))
                      (column (breakpoint-column bp))
                      (why (unfireable dbg filename line column)))
                 (string-append
                  ";;   " (breakpoint-id bp) ": " (place-text filename line column) " ("
                  (cond ((not why) "enabled")
                        ((eq? (car why) 'procedure)
                         (string-append "enabled -- will not fire: inside compiled procedure '" (cdr why) "'"))
                        (else
                         (string-append "enabled -- will not fire: inside macro transformer '" (cdr why) "'")))
                  ")")))
             (debugger-breakpoints dbg))
        "\n"))))

;; /**
;;  * A command that steps, saying so.
;;  */
(define (step-command step message)
  (when-paused (lambda (dbg args rest) (step dbg) message)))

(define (backtrace-command dbg args rest)
  (let ((depth (debugger-depth dbg))
        (selected (selected-frame dbg)))
    (if (= depth 0)
        ";; No call stack info available"
        (string-append
         ";; Call Stack:\n"
         (joined
          (let number ((activations (debugger-activations dbg)) (n (- depth 1)))
            (if (null? activations)
                '()
                (let ((activation (car activations)))
                  (cons (string-append ";; " (if (eqv? n selected) "=>" "  ") " #" (number->string n) " "
                                       (activation-name activation)
                                       " at " (location-text (activation-source activation)))
                        (number (cdr activations) (- n 1))))))
          "\n")))))

;; /**
;;  * Of an environment's bindings, those with a name: an environment made
;;  * with nothing to bind holds one under none.
;;  * @param {list} bindings - The bindings, as (name . value).
;;  * @returns {list}
;;  */
(define (named-bindings bindings)
  (cond ((null? bindings) '())
        ((string? (caar bindings)) (cons (car bindings) (named-bindings (cdr bindings))))
        (else (named-bindings (cdr bindings)))))

(define (locals-command dbg args rest)
  (let ((n (selected-frame dbg)))
    (if (not n)
        ";; Invalid frame selected"
        (let ((bindings (named-bindings ((host-bindings (debugger-host dbg))
                                         (activation-env (activation-numbered dbg n))))))
          (if (null? bindings)
              (string-append ";; Frame #" (number->string n) " has no local bindings")
              (string-append
               ";; Local variables for frame #" (number->string n) ":\n"
               (joined (map (lambda (binding)
                              (string-append ";;   " (car binding) " = " (written (cdr binding))))
                            bindings)
                       "\n")))))))

(define (eval-command dbg args rest)
  (let ((n (selected-frame dbg)))
    (cond ((string=? rest "") ";; Usage: :eval <expression>")
          ((not n) ";; Invalid frame selected")
          (else (js-obj "expression" rest "env" (activation-env (activation-numbered dbg n)))))))

(define (up-command dbg args rest)
  (let ((n (selected-frame dbg)))
    (if (and n (> n 0))
        (begin
          (set-debugger-selected-frame! dbg (- n 1))
          (string-append ";; Selected frame #" (number->string (- n 1))))
        ";; Already at oldest frame")))

(define (down-command dbg args rest)
  (let ((n (selected-frame dbg)))
    (if (and n (< n (- (debugger-depth dbg) 1)))
        (begin
          (set-debugger-selected-frame! dbg (+ n 1))
          (string-append ";; Selected frame #" (number->string (+ n 1))))
        ";; Already at newest frame")))

(define (help-command dbg args rest)
  (joined
   '(";; Debug Commands:"
     ";;   :debug on|off     - Enable/disable debugging"
     ";;   :break <file> <l> [c] - Set breakpoint"
     ";;   :unbreak <id>     - Remove breakpoint"
     ";;   :breakpoints      - List all breakpoints"
     ";;   :step / :s        - Step into"
     ";;   :next / :n        - Step over"
     ";;   :finish / :fin    - Step out"
     ";;   :continue / :c    - Resume execution"
     ";;   :bt / :backtrace  - Show backtrace"
     ";;   :locals           - Show local variables"
     ";;   :eval <expr>      - Evaluate in selected frame's scope"
     ";;   :abort / :a       - Abort current evaluation and return to prompt"
     ";;   :up / :u          - Move up the stack"
     ";;   :down / :d        - Move down the stack"
     ";;   :help / :h / :?   - Show this help")
   "\n"))

;; /**
;;  * The commands, by name, each given the debugger, its arguments as words,
;;  * and what follows its name as written.
;;  */
(define command-table
  (list (cons "debug" debug-command)
        (cons "abort" abort-command) (cons "a" abort-command)
        (cons "break" break-command)
        (cons "unbreak" unbreak-command)
        (cons "breakpoints" breakpoints-command)
        (cons "step" (step-command step-into! ";; Stepping into..."))
        (cons "s" (step-command step-into! ";; Stepping into..."))
        (cons "next" (step-command step-over! ";; Stepping over..."))
        (cons "n" (step-command step-over! ";; Stepping over..."))
        (cons "finish" (step-command step-out! ";; Stepping out..."))
        (cons "fin" (step-command step-out! ";; Stepping out..."))
        (cons "continue" (step-command resume! ";; Continuing..."))
        (cons "c" (step-command resume! ";; Continuing..."))
        (cons "bt" backtrace-command) (cons "backtrace" backtrace-command)
        (cons "locals" (when-paused locals-command))
        (cons "eval" (when-paused eval-command))
        (cons "up" up-command) (cons "u" up-command)
        (cons "down" down-command) (cons "d" down-command)
        (cons "help" help-command) (cons "h" help-command) (cons "?" help-command)))

;; /**
;;  * What the REPL shows when the program pauses: where, and why, and what it
;;  * can do next.
;;  * @param {object} info - What the host was told, `pause-info`'s object.
;;  * @returns {vector} The lines.
;;  */
(define (pause-message info)
  (let* ((breakpoint (js-ref info "breakpointId"))
         (reason (js-ref info "reason"))
         (why (cond ((string? breakpoint) (string-append "breakpoint " breakpoint " hit"))
                    ((equal? reason "step") "step complete")
                    ((string? reason) reason)
                    (else "unknown reason"))))
    (vector (string-append "\n;; Paused: " why " at " (location-text (js-ref info "source")))
            ";; Use :bt for backtrace, :locals for variables, :continue to resume")))
