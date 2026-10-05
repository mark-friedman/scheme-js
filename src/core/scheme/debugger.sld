;; (scheme-js debugger) library
;;
;; The debugger's logic: breakpoints and which one a location hits, the
;; procedure calls a program is in, whether it runs, is paused or is being
;; stepped and whether a step stops it, whether an exception does, which
;; compiled procedure or macro transformer holds a location, what a frame's
;; bindings show, the REPL's debug commands and the pause message. Its
;; procedures are Scheme, in debugger.scm; the evaluator's hooks that call
;; them are JavaScript, in src/debug/scheme_debug_runtime.js.
;;
;; It is written with (scheme core) and (scheme control) alone, as the library
;; system is, since it is loaded beside the library system, on its interpreter
;; (`systemLibrary` in src/core/interpreter/library_seed.js): no debugger is
;; attached there, so the debugger's own Scheme is never paused or stepped.

(define-library (scheme-js debugger)
  ;; The runtime's procedures first, so that `(scheme core)`'s and `(scheme
  ;; control)`'s of the same names are the ones bound.
  (import (scheme primitives)
          (scheme core)
          (scheme control))
  (export
    ;; The host, and the debugger
    make-debugger-host make-debugger debugger? debugger-enabled? set-debugger-enabled!
    debugger-debugging? debugger-interpretation debugger-changed! reset-debugger!
    ;; Breakpoints
    add-breakpoint! remove-breakpoint! clear-breakpoints! debugger-breakpoints breakpoint-at
    breakpoint-id breakpoint-filename breakpoint-line breakpoint-column
    ;; The calls a program is in
    enter-activation! replace-activation! exit-activation! debugger-activations debugger-depth
    activation-name activation-source activation-env activation-tail-calls
    ;; Running, paused and stepping
    debugger-mode debugger-paused? debugger-aborted? debugger-target-depth
    debugger-pause-reason debugger-pause-data
    step-into! step-over! step-out! resume! abort! pause! step-stops?
    should-pause? pause-at! pause-on-exception!
    ;; Exceptions
    breaks-on-exception? debugger-breaks-on-caught? set-debugger-breaks-on-caught!
    debugger-breaks-on-uncaught? set-debugger-breaks-on-uncaught!
    ;; Where a breakpoint cannot fire
    span-contains? innermost-holding compiled-procedure-at transformer-at
    ;; What the host's JavaScript reads
    breakpoints->js activations->js pause-state->js
    ;; The REPL
    debugger-command? debugger-command reset-frame-selection! eval-answer eval-failure
    pause-message)
  (include "debugger.scm"))
