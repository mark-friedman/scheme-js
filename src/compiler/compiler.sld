;; (scheme-js compiler) library
;;
;; The compiler's own Scheme: lowering the analyzed AST to IR (ir.scm), lifting
;; closures (lift.scm), expanding primitives inline (inline.scm), liveness over
;; frame spills (liveness.scm), and generating JavaScript (emit.scm).
;;
;; It is written with SRFI 1 and SRFI 152 and imports them like any other
;; program, so their private helpers stay private to them. The files are
;; included in dependency order: each defines what the later ones call.
;;
;; The exports are the entry points src/compiler/lowering.js calls. Everything
;; else is internal, and the compiler's Scheme tests (tests/compiler/) reach it
;; by running in this library's environment.
;;
;; The file is `compiler.sld` because every library resolver finds a library's
;; file by the last part of its name.

(define-library (scheme-js compiler)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (srfi 1)
          (srfi 152))
  (export
    ;; Lowering
    lower-lambda control-globals
    ;; Code generation
    generate-unit inline-expansion-names js-name)
  (include "ir.scm" "lift.scm" "inline.scm" "liveness.scm" "emit.scm"))
