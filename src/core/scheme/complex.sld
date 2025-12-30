;; R7RS (scheme complex) library
;;
;; This library re-exports the optional complex number procedures.
;; They are already implemented in (scheme base) in this implementation,
;; but R7RS allows them to be in a separate library.

(define-library (scheme complex)
  (import (scheme base))
  (export
    make-rectangular
    make-polar
    real-part
    imag-part
    magnitude
    angle))
