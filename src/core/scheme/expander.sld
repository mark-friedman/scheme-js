;; (scheme-js expander) library
;;
;; The expander: a form, as read, into a core form, which the evaluator runs
;; once its door, the assembler (src/core/interpreter/assembler.js), has made
;; it into nodes, and the compiler lowers. Special forms, variables and
;; applications, bodies and their definitions, quasiquote, and the macros a
;; program defines -- `syntax-rules` and `define-macro` -- and uses. Its
;; procedures are Scheme, in expander.scm and syntax_rules.scm.
;;
;; A core form is a list whose head is a tag:
;;
;;   (lit datum)                  a constant
;;   (var name)                   a variable, by the name it is bound under
;;   (library-var name env)       a library's own binding, from its macro's
;;                                expansion, which the use site cannot name
;;   (scoped-var name scopes)     a binding found by name and scopes as the
;;                                form runs
;;   (if test then else)
;;   (seq forms)                  a body's forms, in order
;;   (lambda params rest name body original-params original-rest)
;;   (letrec names inits body original-names)
;;                                every init a lambda
;;   (set name value)             (library-set name env value)
;;   (define name value)
;;   (app operator operands)
;;   (import import-sets)         (define-library form)
;;   (node executable)            a node of the evaluator's, made already
;;
;; Names are symbols: a local's renamed, unique where it is bound, and a
;; global's as written. `rest` and `original-rest` are a symbol or #f. A
;; lambda's `name` is a string -- "anonymous", "let", or the name it is defined
;; under -- and `original-params` and `original-rest` are its parameters'
;; names as written, for a debugger to show. A form that has a span, the text
;; it was read from, carries it as the reader's data do: as its first pair's
;; `source` property.
;;
;; The expander keeps no state of its own between forms. What a form's
;; meaning depends on -- the scopes made so far, the library or program being
;; expanded, the syntactic keywords bound in each, the macros defined by name
;; for the process -- is in the analyzer's tables, reached through primitives
;; (src/core/primitives/expander_support.js), which the library system reaches
;; too.
;;
;; It is written with (scheme core) and (scheme control) alone, as the library
;; system and the reader are, so that it can be loaded beside them, on their
;; interpreter, before any other library.

(define-library (scheme-js expander)
  ;; The runtime's procedures first, so that `(scheme core)`'s and `(scheme
  ;; control)`'s of the same names are the ones bound.
  (import (scheme primitives)
          (scheme core)
          (scheme control))
  (export expand expand-in-environment)
  (include "expander.scm" "syntax_rules.scm"))
