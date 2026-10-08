;;; handlers.scm -- exception handlers, over a list the runtime keeps.
;;;
;;; The handlers in force are a list of `(handler . winds)`, innermost first,
;;; each with the winds in force where it was installed. The runtime holds it
;;; (`handlerList` in src/core/interpreter/unwind.js), and these read and set
;;; it through `%handlers` and `%set-handlers!`. A handler is in force for an
;;; extent `dynamic-wind` makes, so that leaving the extent, by returning or
;;; by a continuation, puts the handlers outside it back, and entering it
;;; again puts it back in force.
;;;
;;; They do what the interpreter does. A raise that cannot continue leaves the
;;; extents between it and its handler, running their after-thunks, before it
;;; calls the handler -- so that a `guard`'s clauses run outside them, as they
;;; do under R7RS's own `guard` -- and if the handler returns, raises an error
;;; saying so to the handlers outside. A continuable raise calls its handler
;;; where it is, leaving no extent (R7RS 6.11). In both the handler runs with
;;; the handlers outside its own in force. With none in force, a raise goes to
;;; whoever called the program, as it does with no handler under the
;;; interpreter.
;;;
;;; What `error` and the primitives raise reaches these as JavaScript throws
;;; it: compiled code running with no interpreter hands it to `raise` while a
;;; handler is in force (`ahead` in unwind.js), with the winds in force where
;;; it was thrown, as at a raise there.

;; /**
;;  * Calls a thunk with a handler installed for its extent (R7RS 6.11).
;;  * @param {procedure} handler - Called with what is raised.
;;  * @param {procedure} thunk - The extent's work.
;;  * @returns {*} What the thunk returns.
;;  */
(define (with-exception-handler handler thunk)
  (cond ((not (procedure? handler)) (error "with-exception-handler: expected procedure" handler))
        ((not (procedure? thunk)) (error "with-exception-handler: expected procedure" thunk)))
  (let ((outer (%handlers)))
    (dynamic-wind
      (lambda () (%set-handlers! (cons (cons handler (%winds)) outer)))
      thunk
      (lambda () (%set-handlers! outer)))))

;; /**
;;  * Raises an object, which the handler in force may not return from (R7RS
;;  * 6.11).
;;  * @param {*} obj - What is raised.
;;  * @returns {never}
;;  */
(define (raise obj)
  (let ((handlers (%handlers)))
    (if (null? handlers)
        (%raise-unhandled obj)
        (begin
          (leave-extents (cdar handlers))
          ((caar handlers) obj)
          (error "non-continuable exception: handler returned")))))

;; /**
;;  * Raises an object, whose handler's value is the raise's (R7RS 6.11).
;;  * @param {*} obj - What is raised.
;;  * @returns {*} What the handler returns.
;;  */
(define (raise-continuable obj)
  (let ((handlers (%handlers)))
    (if (null? handlers)
        (%raise-unhandled obj)
        (dynamic-wind
          (lambda () (%set-handlers! (cdr handlers)))
          (lambda () ((caar handlers) obj))
          (lambda () (%set-handlers! handlers))))))

;; /**
;;  * Leaves the extents in force down to those of some winds, running each
;;  * one's after-thunk, innermost first, with the winds outside it in force.
;;  * @param {list} winds - Winds in force now, or ones they were made from.
;;  */
(define (leave-extents winds)
  (let ((current (%winds)))
    (if (not (eq? current winds))
        (begin
          (%set-winds! (cdr current))
          ((cdar current))
          (leave-extents winds)))))

;; The driver hands an error JavaScript threw to `raise`.
(%set-error-raiser! raise)
