;;; winds.scm -- `dynamic-wind`, over the winds the runtime keeps.
;;;
;;; The winds in force are a list of `(before . after)`, innermost first,
;;; which the runtime holds (`windList` in src/core/interpreter/unwind.js) and
;;; these read and set through `%winds` and `%set-winds!`. A continuation
;;; records the list as it is captured, and invoking one goes from the list in
;;; force to its own, running the after-thunks of the winds it leaves and the
;;; before-thunks of those it enters (`travelTo`); returning normally, the
;;; extent's own after-thunk runs here.

;; /**
;;  * Calls a thunk within an extent: `before` as control enters it, by
;;  * returning or by a continuation, and `after` as control leaves it, by
;;  * returning or by a continuation (R7RS 6.10).
;;  * @param {procedure} before - Called with no arguments on entering.
;;  * @param {procedure} thunk - The extent's work.
;;  * @param {procedure} after - Called with no arguments on leaving.
;;  * @returns {*} What the thunk returns, every value of it.
;;  */
(define (dynamic-wind before thunk after)
  (cond ((not (procedure? before)) (error "dynamic-wind: expected procedure" before))
        ((not (procedure? thunk)) (error "dynamic-wind: expected procedure" thunk))
        ((not (procedure? after)) (error "dynamic-wind: expected procedure" after)))
  (before)
  (let ((outer (%winds)))
    (%set-winds! (cons (cons before after) outer))
    (call-with-values thunk
      (lambda results
        (%set-winds! outer)
        (after)
        (apply values results)))))
