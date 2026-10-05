;; (srfi 151) library
;;
;; Bitwise operations on exact integers, as infinite two's-complement bit
;; strings. The procedures are Scheme, in bitwise.scm, over the few that need
;; JavaScript's `BigInt` operators -- the associative operations, the shift,
;; and population count and length -- which are the `%`-prefixed primitives of
;; src/extras/primitives/bitwise.js, exported here under SRFI 151's names.
;;
;; The file is `151.sld` because every library resolver finds a library's
;; file by the last part of its name.

(define-library (srfi 151)
  (import (scheme base)
          (scheme case-lambda)
          (only (scheme primitives) %bitwise-and %bitwise-ior %bitwise-xor
                %arithmetic-shift %integer-length %bit-count))
  (export
    ;; Basic operations
    bitwise-not
    (rename %bitwise-and bitwise-and)
    (rename %bitwise-ior bitwise-ior)
    (rename %bitwise-xor bitwise-xor)
    bitwise-eqv
    bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2 bitwise-orc1 bitwise-orc2
    ;; Integer operations
    (rename %arithmetic-shift arithmetic-shift)
    (rename %bit-count bit-count)
    (rename %integer-length integer-length)
    bitwise-if
    ;; Single-bit operations
    bit-set? copy-bit bit-swap any-bit-set? every-bit-set? first-set-bit
    ;; Bit field operations
    bit-field bit-field-any? bit-field-every? bit-field-clear bit-field-set
    bit-field-replace bit-field-replace-same bit-field-rotate bit-field-reverse
    ;; Bits conversion
    bits->list list->bits bits->vector vector->bits bits
    ;; Fold, unfold and generate
    bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator)
  (include "bitwise.scm"))
