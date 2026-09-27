;; R7RS (scheme inexact) library
;;
;; The procedures typically only useful with inexact values, per R7RS
;; Appendix A. All twelve are primitives; this library gives them the name
;; the standard imports them by.

(define-library (scheme inexact)
  (import (scheme primitives))
  (export
    acos asin atan
    cos sin tan
    exp log sqrt
    finite? infinite? nan?))
