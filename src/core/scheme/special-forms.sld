;; (scheme-js special-forms) library
;;
;; The special forms: the syntactic keywords the expander expands itself
;; (`special-form` in expander.scm), the transformers `define-syntax` takes,
;; and the auxiliary syntax forms take. A special form is a keyword like a
;; macro: a library, a strict program or an environment `environment` makes
;; has one only if it imports it, and may bind the name to something else if
;; it does not. A program that imports nothing, and the REPLs, have them all.
;;
;; Nothing defines them; a library exports one under its own name, as the
;; library system knows its name. `(scheme core)` imports them and passes them
;; on, and `(scheme base)` exports those R7RS gives it.

(define-library (scheme-js special-forms)
  (export
    define set! lambda if begin quote quasiquote unquote unquote-splicing
    let letrec
    define-syntax let-syntax letrec-syntax syntax-rules er-macro-transformer define-macro
    ... _ => else
    cond-expand import define-library))
