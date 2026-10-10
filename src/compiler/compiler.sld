;; (scheme-js compiler) library
;;
;; The compiler's own Scheme: lowering the analyzed AST to IR (ir.scm), lifting
;; closures (lift.scm), expanding primitives inline (inline.scm), liveness over
;; frame spills (liveness.scm), where each variable is in the generated code,
;; for a debugger (scopes.scm), generating JavaScript (emit.scm), deciding what
;; to compile and why not (driver.scm, safety.scm), and when, for a program's
;; own code as it runs (tier.scm).
;;
;; It is written with SRFI 1, SRFI 151 and SRFI 152 and imports them like any
;; other program, so their private helpers stay private to them. What it needs from
;; the interpreter, which is JavaScript, it imports from
;; (scheme-js compiler host), src/compiler/host.js. The files are included in
;; dependency order: each defines what the later ones call.
;;
;; The exports are the entry points JavaScript calls, through
;; src/compiler/lowering.js, and the Scheme programs that import the library --
;; the build steps and the compiler's harnesses, run from the CLI -- with the
;; accessors of the records the entry points answer with. Everything else is
;; internal, and the compiler's Scheme tests (tests/compiler/) reach it by
;; running in this library's environment.
;;
;; The file is `compiler.sld` because every library resolver finds a library's
;; file by the last part of its name.

(define-library (scheme-js compiler)
  (import (scheme base)
          (only (scheme primitives) emergency-exit eval exit %record-procedure-kind)
          (scheme char)
          (scheme cxr)
          (srfi 1)
          (srfi 151)
          (srfi 152)
          (scheme-js interop)
          (scheme-js compiler host))
  (export
    ;; Lowering
    lower-lambda control-globals
    ;; Code generation
    generate-unit inline-expansion-names make-local-names local-name source-map
    ;; What to compile
    compile-definition compile-expression compile-closure
    generate-environment compile-environment compile-program
    generate-lambda expression-thunk
    program-unsafe-definitions
    ;; What they answer with
    generated? generated-name generated-closure generated-source generated-constants generated-globals
    generated-spans
    generated-env generated-library-globals
    compiled? compiled-name compiled-procedure compiled-source
    declined? declined-name declined-reason
    program-run-compiled program-run-declined program-run-unsafe program-run-expressions
    program-run-value
    ;; The compiler's own failures, which leave their procedures interpreted
    take-compiler-failures!
    ;; A program's tier
    make-tier tier-bound! tier-due! tier-top-level-procedure tier-compile-eagerly! note-resume first-resume-to-ask)
  (include "ir.scm" "lift.scm" "inline.scm" "liveness.scm" "scopes.scm" "emit.scm"
           "sourcemap.scm" "driver.scm" "safety.scm" "tier.scm"))
