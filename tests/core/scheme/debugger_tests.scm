;; The debugger's Scheme (src/core/scheme/debugger.scm): breakpoints and which
;; one a location hits, the procedure calls a program is in, whether it runs,
;; is paused or is being stepped and whether a step stops it, whether an
;; exception does, which compiled procedure or transformer holds a location,
;; and the REPL's debug commands. The host a runtime gives it is made here of
;; Scheme procedures that record what they are told.

(import (scheme-js debugger))

;; /**
;;  * A host that records what it is told, and answers with the bindings,
;;  * compiled procedures and transformers it is given.
;;  * @param {list} bindings - What any environment's bindings are.
;;  * @param {list} compiled - The compiled procedures, as (name . span).
;;  * @param {list} transformers - The transformers, as (name . span).
;;  * @returns {pair} (host . log): the host, and a procedure returning what it
;;  *   was told, oldest first.
;;  */
(define (recording-host bindings compiled transformers)
  (let ((told '()))
    (define (tell! . what) (set! told (cons what told)))
    (cons (make-debugger-host
           (lambda (enabled? debugging? paused? aborted?)
             (tell! 'changed enabled? debugging? paused? aborted?))
           (lambda () (tell! 'released))
           (lambda (how) (tell! 'resumed how))
           (lambda (info) (tell! 'paused (js-ref info "reason") (js-ref info "breakpointId")))
           (lambda (env) bindings)
           (lambda () compiled)
           (lambda () transformers))
          (lambda () (reverse told)))))

;; /**
;;  * What a host was told that a predicate keeps.
;;  * @param {procedure} keep? - The predicate.
;;  * @param {list} told - What it was told.
;;  * @returns {list}
;;  */
(define (filter-told keep? told)
  (cond ((null? told) '())
        ((keep? (car told)) (cons (car told) (filter-told keep? (cdr told))))
        (else (filter-told keep? (cdr told)))))

;; /**
;;  * A debugger with a recording host and no bindings, procedures or
;;  * transformers.
;;  * @returns {debugger}
;;  */
(define (fresh-debugger)
  (make-debugger (car (recording-host '() '() '()))))

;; /**
;;  * A source span as the reader makes one.
;;  */
(define (span filename line column . end)
  (if (null? end)
      (js-obj "filename" filename "line" line "column" column)
      (js-obj "filename" filename "line" line "column" column
              "endLine" (car end) "endColumn" (cadr end))))

(test-group "debugger - breakpoints"
  (define dbg (fresh-debugger))
  (define first (add-breakpoint! dbg "test.scm" 10 #f))
  (define second (add-breakpoint! dbg "test.scm" 10 5))
  (test "each has an id of its own" #t (and (string? first) (not (string=? first second))))
  (test "ids count up" '("bp-1" "bp-2") (list first second))
  (test "listed in the order set" '(("test.scm" 10 #f) ("test.scm" 10 5))
        (map (lambda (bp) (list (breakpoint-filename bp) (breakpoint-line bp) (breakpoint-column bp)))
             (debugger-breakpoints dbg)))
  (test "a line breakpoint hits any column of its line" "bp-1"
        (breakpoint-id (breakpoint-at dbg (span "test.scm" 10 50))))
  (test "not another line" #f (breakpoint-at dbg (span "test.scm" 11 1)))
  (test "nor another file" #f (breakpoint-at dbg (span "other.scm" 10 1)))
  (test "nor no location" #f (breakpoint-at dbg #f))
  (test "removing one says so" #t (remove-breakpoint! dbg first))
  (test "and once only" #f (remove-breakpoint! dbg first))
  (test "an unknown id is not removed" #f (remove-breakpoint! dbg "no-such-id"))
  (test "a column breakpoint hits its column" "bp-2"
        (breakpoint-id (breakpoint-at dbg (span "test.scm" 10 5))))
  (test "and no other" #f (breakpoint-at dbg (span "test.scm" 10 6)))
  (test "a new id after a removal is new" "bp-3" (add-breakpoint! dbg "test.scm" 30 #f))
  (test "a line JavaScript gave as a number is a line" "bp-3"
        (breakpoint-id (breakpoint-at dbg (span "test.scm" 30.0 1))))
  (clear-breakpoints! dbg)
  (test "cleared, there are none" '() (debugger-breakpoints dbg)))

(test-group "debugger - the calls a program is in"
  (define dbg (fresh-debugger))
  (test "none at first" 0 (debugger-depth dbg))
  (enter-activation! dbg "main" #f 'main-env)
  (enter-activation! dbg "foo" (span "test.scm" 10 1) 'foo-env)
  (enter-activation! dbg "bar" #f 'bar-env)
  (test "each call entered deepens the stack" 3 (debugger-depth dbg))
  (test "newest first" '("bar" "foo" "main") (map activation-name (debugger-activations dbg)))
  (exit-activation! dbg)
  (test "a call returned leaves it" '("foo" "main") (map activation-name (debugger-activations dbg)))
  (test "an activation keeps its environment" 'foo-env (activation-env (car (debugger-activations dbg))))
  (replace-activation! dbg "loop" #f 'loop-env)
  (replace-activation! dbg "loop" #f 'loop-env)
  (test "a tail call replaces the newest, at the same depth" '(2 "loop")
        (list (debugger-depth dbg) (activation-name (car (debugger-activations dbg)))))
  (test "counting the tail calls" 2 (activation-tail-calls (car (debugger-activations dbg))))
  (exit-activation! dbg)
  (exit-activation! dbg)
  (exit-activation! dbg)
  (test "returning from none is harmless" 0 (debugger-depth dbg))
  (replace-activation! dbg "first" #f 'env)
  (test "a tail call into an empty stack enters" 1 (debugger-depth dbg)))

(test-group "debugger - running, paused and stepping"
  (define dbg (fresh-debugger))
  (test "running at first" 'running (debugger-mode dbg))
  (test "running, no step stops" #f (step-stops? dbg))
  (enter-activation! dbg "a" #f #f)
  (enter-activation! dbg "b" #f #f)
  (step-into! dbg)
  (test "stepping into" 'into (debugger-mode dbg))
  (test "stops at whatever is next" #t (step-stops? dbg))
  (step-over! dbg)
  (test "stepping over" '(over 2) (list (debugger-mode dbg) (debugger-target-depth dbg)))
  (test "stops at the same depth" #t (step-stops? dbg))
  (enter-activation! dbg "c" #f #f)
  (test "not deeper" #f (step-stops? dbg))
  (exit-activation! dbg)
  (exit-activation! dbg)
  (test "and at a shallower one" #t (step-stops? dbg))
  (enter-activation! dbg "b" #f #f)
  (step-out! dbg)
  (test "stepping out" '(out 2) (list (debugger-mode dbg) (debugger-target-depth dbg)))
  (test "does not stop at the same depth" #f (step-stops? dbg))
  (exit-activation! dbg)
  (test "but once returned" #t (step-stops? dbg))
  (pause! dbg "breakpoint" "bp-123")
  (test "paused, with the reason and its data" '(paused "breakpoint" "bp-123")
        (list (debugger-mode dbg) (debugger-pause-reason dbg) (debugger-pause-data dbg)))
  (test "pausing ends a step" #f (debugger-target-depth dbg))
  (step-into! dbg)
  (test "a step forgets the pause's reason" #f (debugger-pause-reason dbg))
  (resume! dbg)
  (test "resumed, it runs" 'running (debugger-mode dbg))
  (step-into! dbg)
  (reset-debugger! dbg)
  (test "reset, it runs with nothing to step" '(running #f) (list (debugger-mode dbg) (debugger-target-depth dbg))))

(test-group "debugger - whether to pause"
  (define dbg (fresh-debugger))
  (add-breakpoint! dbg "test.scm" 10 #f)
  (test "not while disabled" #f (should-pause? dbg "test.scm" 10 1))
  (set-debugger-enabled! dbg #t)
  (test "at a breakpoint" #t (should-pause? dbg "test.scm" 10 1))
  (test "given the line as JavaScript does" #t (should-pause? dbg "test.scm" 10.0 1.0))
  (test "not elsewhere" #f (should-pause? dbg "test.scm" 11 1))
  (test "nor with no file" #f (should-pause? dbg js-undefined 10 1))
  (step-into! dbg)
  (test "anywhere, stepping into" #t (should-pause? dbg "test.scm" 11 1)))

(test-group "debugger - pausing, and what the host is told"
  (define recording (recording-host '() '() '()))
  (define dbg (make-debugger (car recording)))
  (set-debugger-enabled! dbg #t)
  (add-breakpoint! dbg "test.scm" 10 #f)
  (pause-at! dbg (span "test.scm" 10 3) #f #f)
  (step-into! dbg)
  (pause-at! dbg (span "test.scm" 12 1) #f #f)
  (pause-at! dbg #f #f "manual pause")
  (resume! dbg)
  (abort! dbg)
  (test "a breakpoint's pause names it, a step's is a step, another keeps its reason"
        '((paused "breakpoint" "bp-1") (paused "step" ()) (paused "manual pause" ()))
        (filter-told (lambda (what) (eq? (car what) 'paused)) ((cdr recording))))
  (test "a resume releases the run and is told how; an abort only releases it"
        '((released) (resumed "stepInto") (released) (resumed "resume") (released))
        (filter-told (lambda (what) (memq (car what) '(released resumed))) ((cdr recording))))
  (test "each change tells the host what it mirrors, last of all an abort"
        '(changed #t #t #f #t)
        (car (reverse (filter-told (lambda (what) (eq? (car what) 'changed)) ((cdr recording)))))))

(test-group "debugger - whether the program is being debugged"
  (define dbg (fresh-debugger))
  (test "not until enabled" #f (debugger-debugging? dbg))
  (set-debugger-enabled! dbg #t)
  (test "enabled, not with nothing to stop at" #f (debugger-debugging? dbg))
  (add-breakpoint! dbg "test.scm" 1 #f)
  (test "with a breakpoint" #t (debugger-debugging? dbg))
  (clear-breakpoints! dbg)
  (step-into! dbg)
  (test "or stepping" #t (debugger-debugging? dbg)))

(test-group "debugger - exceptions"
  (define dbg (fresh-debugger))
  (test "none breaks while disabled" #f (breaks-on-exception? dbg #f))
  (set-debugger-enabled! dbg #t)
  (test "an uncaught one breaks" #t (breaks-on-exception? dbg #f))
  (test "a caught one does not" #f (breaks-on-exception? dbg #t))
  (set-debugger-breaks-on-caught! dbg #t)
  (test "unless asked to" #t (breaks-on-exception? dbg #t))
  (set-debugger-breaks-on-uncaught! dbg #f)
  (test "nor an uncaught one, if asked not to" #f (breaks-on-exception? dbg #f)))

(test-group "debugger - which span holds a location"
  (define outer (span "f.scm" 1 1 10 2))
  (define inner (span "f.scm" 3 3 5 9))
  (test "a line inside" #t (span-contains? outer "f.scm" 4 #f))
  (test "not after" #f (span-contains? outer "f.scm" 11 #f))
  (test "nor in another file" #f (span-contains? outer "g.scm" 4 #f))
  (test "a column before the start" #f (span-contains? inner "f.scm" 3 2))
  (test "the end column is not inside" #f (span-contains? inner "f.scm" 5 9))
  (test "the innermost of those holding it" "inner"
        (car (innermost-holding (list (cons "outer" outer) (cons "inner" inner)) "f.scm" 4 #f)))
  (test "none holding it" #f (innermost-holding (list (cons "inner" inner)) "f.scm" 9 #f)))

(test-group "debugger - the REPL's commands"
  (define recording (recording-host (list (cons "debug-var" 42) (cons "name" "x")) '() '()))
  (define dbg (make-debugger (car recording)))
  (test "a command begins with a colon" '(#t #f)
        (list (debugger-command? "  :break 10") (debugger-command? "(define x 10)")))
  (test "debugging is off" ";; Debugging is OFF" (debugger-command dbg ":debug"))
  (test "turned on" ";; Debugging enabled" (debugger-command dbg ":debug on"))
  (test "and is on" '(#t ";; Debugging is ON") (list (debugger-enabled? dbg) (debugger-command dbg ":debug")))
  (test "a breakpoint set" ";; Breakpoint bp-1 set at test.scm:10" (debugger-command dbg ":break test.scm 10"))
  (test "with a column" ";; Breakpoint bp-2 set at test.scm:12:4" (debugger-command dbg ":break test.scm 12 4"))
  (test "a line that is not a number" ";; Invalid line number" (debugger-command dbg ":break test.scm ten"))
  (test "too little" ";; Usage: :break <file> <line> [column]" (debugger-command dbg ":break test.scm"))
  (test "listed" ";; Breakpoints:\n;;   bp-1: test.scm:10 (enabled)\n;;   bp-2: test.scm:12:4 (enabled)"
        (debugger-command dbg ":breakpoints"))
  (test "removed" ";; Breakpoint bp-1 removed" (debugger-command dbg ":unbreak bp-1"))
  (test "not twice" ";; Breakpoint bp-1 not found" (debugger-command dbg ":unbreak bp-1"))
  (debugger-command dbg ":unbreak bp-2")
  (test "none left" ";; No breakpoints set" (debugger-command dbg ":breakpoints"))
  (test "no step while running" ";; Not paused" (debugger-command dbg ":step"))
  (test "nor locals" ";; Not paused" (debugger-command dbg ":locals"))
  (test "no stack" ";; No call stack info available" (debugger-command dbg ":bt"))
  (enter-activation! dbg "frame-0" (span "test.scm" 3 1) 'env-0)
  (enter-activation! dbg "frame-1" (span "test.scm" 7 1) 'env-1)
  (pause! dbg "breakpoint" #f)
  (test "a backtrace, newest first, the selected marked"
        ";; Call Stack:\n;; => #1 frame-1 at test.scm:7\n;;    #0 frame-0 at test.scm:3"
        (debugger-command dbg ":bt"))
  (test "up" ";; Selected frame #0" (debugger-command dbg ":up"))
  (test "no further" ";; Already at oldest frame" (debugger-command dbg ":up"))
  (test "the backtrace marks it" ";; Call Stack:\n;;    #1 frame-1 at test.scm:7\n;; => #0 frame-0 at test.scm:3"
        (debugger-command dbg ":bt"))
  (test "down" ";; Selected frame #1" (debugger-command dbg ":down"))
  (test "no further" ";; Already at newest frame" (debugger-command dbg ":down"))
  (test "the selected frame's bindings, as write writes them"
        ";; Local variables for frame #1:\n;;   debug-var = 42\n;;   name = \"x\""
        (debugger-command dbg ":locals"))
  (test "an expression to evaluate is the host's to evaluate, in the frame's environment"
        '("(+ debug-var   8)" env-1)
        (let ((request (debugger-command dbg ":eval (+ debug-var   8)")))
          (list (js-ref request "expression") (js-ref request "env"))))
  (test "with nothing to evaluate" ";; Usage: :eval <expression>" (debugger-command dbg ":eval"))
  (test "stepping" ";; Stepping over..." (debugger-command dbg ":next"))
  (test "the step is told" '(resumed "stepOver") (car (reverse ((cdr recording)))))
  (test "an unknown command" ";; Unknown debug command: frob. Type :help for commands."
        (debugger-command dbg ":frob"))
  ;; In a browser, a run that is not being debugged holds the page until it
  ;; ends, which turning debugging off warns of.
  (test "debugging turned off"
        (if (memq 'browser (features))
            ";; Debugging disabled\n;; WARNING: Fast Mode enabled. UI will freeze during long computations."
            ";; Debugging disabled")
        (debugger-command dbg ":debug off"))
  (reset-frame-selection! dbg)
  (test "an evaluation's answer" ";; result: 50" (eval-answer "50"))
  (test "and its failure" ";; Error during eval: boom" (eval-failure "boom")))

(test-group "debugger - a breakpoint where it cannot fire"
  (define recording (recording-host '() (list (cons "fast" (span "f.scm" 1 1 4 10)))
                                    (list (cons "my-macro" (span "f.scm" 8 1 9 5)))))
  (define dbg (make-debugger (car recording)))
  (test "inside a compiled procedure, said when set"
        ";; Breakpoint bp-1 set at f.scm:2\n;; Warning: this is inside compiled procedure 'fast', which does not stop at breakpoints -- it will not fire"
        (debugger-command dbg ":break f.scm 2"))
  (test "inside a transformer"
        ";; Breakpoint bp-2 set at f.scm:8\n;; Warning: this is inside macro transformer 'my-macro', which runs during expansion, where the debugger cannot stop -- it will not fire"
        (debugger-command dbg ":break f.scm 8"))
  (test "and when listed"
        ";; Breakpoints:\n;;   bp-1: f.scm:2 (enabled -- will not fire: inside compiled procedure 'fast')\n;;   bp-2: f.scm:8 (enabled -- will not fire: inside macro transformer 'my-macro')"
        (debugger-command dbg ":breakpoints")))

(test-group "debugger - the pause message"
  (test "at a breakpoint"
        '("\n;; Paused: breakpoint bp-1 hit at test.scm:10"
          ";; Use :bt for backtrace, :locals for variables, :continue to resume")
        (vector->list (pause-message (js-obj "reason" "breakpoint" "breakpointId" "bp-1"
                                             "source" (span "test.scm" 10 1)))))
  (test "after a step, nowhere known"
        "\n;; Paused: step complete at unknown location"
        (vector-ref (pause-message (js-obj "reason" "step" "breakpointId" '() "source" '())) 0)))
