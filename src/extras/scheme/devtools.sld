;; (scheme-js devtools) -- how DevTools shows Scheme values.
;;
;; DevTools draws an object with the custom formatters a page registers, once
;; its user turns custom formatters on in its settings. This library is one:
;; a Scheme value is drawn as Scheme writes it, `(1 2 3)` rather than a chain
;; of JavaScript objects, and expanding it shows its parts and, last, the
;; value as JavaScript draws it. `install-devtools-formatters!` registers it,
;; which a page's and the CLI's start-up do; `set-devtools-display!` switches
;; between drawing Scheme's values as Scheme, by where the program is paused,
;; and drawing everything as Scheme or as JavaScript. The primitives it is
;; written over are in src/extras/primitives/devtools.js.

(define-library (scheme-js devtools)
  (import (scheme base)
          (scheme write)
          (only (scheme primitives) %install-devtools-formatter! %scheme-procedure? %record-description))
  (export install-devtools-formatters!
          devtools-display set-devtools-display!
          devtools-header devtools-has-body? devtools-body)
  (include "devtools.scm"))
