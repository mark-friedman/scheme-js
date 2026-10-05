;; (scheme-js reader) library
;;
;; The reader: Scheme's written syntax, R7RS 7.1.2, and this implementation's
;; additions -- dot notation for JavaScript properties, `#{...}` object
;; literals, the directives that switch case folding and dot notation -- read
;; into data, each list and vector carrying the span of text it was read from.
;; Its procedures are Scheme, in reader.scm. A number's syntax is
;; `string->number`'s, the numeric tower's primitive.
;;
;; It is written with (scheme core) and (scheme control) alone, as the library
;; system is, so that it can be loaded beside the library system, on its own
;; interpreter, before any other library.

(define-library (scheme-js reader)
  ;; The runtime's procedures first, so that `(scheme core)`'s and `(scheme
  ;; control)`'s of the same names are the ones bound.
  (import (scheme primitives)
          (scheme core)
          (scheme control))
  (export read-source)
  (include "reader.scm"))
