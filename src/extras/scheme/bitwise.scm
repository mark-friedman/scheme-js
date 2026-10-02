;; SRFI 151: Bitwise operations
;;
;; An exact integer is read as an infinite string of bits in two's complement,
;; bit 0 the least significant: a non-negative integer has finitely many 1
;; bits, a negative one finitely many 0 bits. The associative operations
;; (`bitwise-and`, `bitwise-ior`, `bitwise-xor`), `arithmetic-shift`,
;; `bit-count` and `integer-length` are JavaScript primitives, since a `BigInt`
;; already is such a bit string; everything here is written with them.
;;
;; A bit field is given by its start, inclusive, and its end, exclusive: the
;; end minus the start bits from bit `start` up.

;; ---------------------------------------------------------------------------
;; Argument checking
;; ---------------------------------------------------------------------------

;; /**
;;  * Signals an error unless an argument is an exact integer.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {*} i - The argument.
;;  * @returns {unspecified}
;;  */
(define (check-integer who i)
  (if (not (exact-integer? i))
      (error (string-append who ": expected an exact integer") i)))

;; /**
;;  * Signals an error unless an argument is a non-negative exact integer: a
;;  * bit's index, a field's bound or a length.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {*} k - The argument.
;;  * @returns {unspecified}
;;  */
(define (check-index who k)
  (if (not (and (exact-integer? k) (>= k 0)))
      (error (string-append who ": expected a non-negative exact integer") k)))

;; /**
;;  * Signals an error unless a field's bounds are indices, the end not before
;;  * the start.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {*} start - The field's first bit.
;;  * @param {*} end - The bit after its last.
;;  * @returns {unspecified}
;;  */
(define (check-field who start end)
  (check-index who start)
  (check-index who end)
  (if (< end start)
      (error (string-append who ": a field's end is before its start") start end)))

;; /**
;;  * Signals an error unless an argument is a procedure.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {*} f - The argument.
;;  * @returns {unspecified}
;;  */
(define (check-procedure who f)
  (if (not (procedure? f))
      (error (string-append who ": expected a procedure") f)))

;; /**
;;  * Signals an error unless an argument is a boolean.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {*} b - The argument.
;;  * @returns {unspecified}
;;  */
(define (check-boolean who b)
  (if (not (boolean? b))
      (error (string-append who ": expected a boolean") b)))

;; ---------------------------------------------------------------------------
;; Basic operations
;; ---------------------------------------------------------------------------

;; /**
;;  * Every bit inverted.
;;  * @param {integer} i - An exact integer.
;;  * @returns {integer} -1 - i.
;;  */
(define (bitwise-not i)
  (check-integer "bitwise-not" i)
  (- -1 i))

;; /**
;;  * The bits on which the arguments agree, folded from the left; with none,
;;  * -1, and with three, `(bitwise-eqv a (bitwise-eqv b c))`, which is the
;;  * same, since the operation is associative.
;;  * @param {...integer} is - Exact integers.
;;  * @returns {integer}
;;  */
(define (bitwise-eqv . is)
  (let loop ((is is) (acc -1))
    (if (null? is)
        acc
        (begin
          (check-integer "bitwise-eqv" (car is))
          (loop (cdr is) (- -1 (%bitwise-xor acc (car is))))))))

;; /**
;;  * The bits not set in both.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} j - An exact integer.
;;  * @returns {integer}
;;  */
(define (bitwise-nand i j)
  (check-integer "bitwise-nand" i)
  (check-integer "bitwise-nand" j)
  (- -1 (%bitwise-and i j)))

;; /**
;;  * The bits set in neither.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} j - An exact integer.
;;  * @returns {integer}
;;  */
(define (bitwise-nor i j)
  (check-integer "bitwise-nor" i)
  (check-integer "bitwise-nor" j)
  (- -1 (%bitwise-ior i j)))

;; /**
;;  * The bits set in `j` and not in `i`.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} j - An exact integer.
;;  * @returns {integer}
;;  */
(define (bitwise-andc1 i j)
  (check-integer "bitwise-andc1" i)
  (check-integer "bitwise-andc1" j)
  (%bitwise-and (- -1 i) j))

;; /**
;;  * The bits set in `i` and not in `j`.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} j - An exact integer.
;;  * @returns {integer}
;;  */
(define (bitwise-andc2 i j)
  (check-integer "bitwise-andc2" i)
  (check-integer "bitwise-andc2" j)
  (%bitwise-and i (- -1 j)))

;; /**
;;  * The bits set in `j` or not in `i`.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} j - An exact integer.
;;  * @returns {integer}
;;  */
(define (bitwise-orc1 i j)
  (check-integer "bitwise-orc1" i)
  (check-integer "bitwise-orc1" j)
  (%bitwise-ior (- -1 i) j))

;; /**
;;  * The bits set in `i` or not in `j`.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} j - An exact integer.
;;  * @returns {integer}
;;  */
(define (bitwise-orc2 i j)
  (check-integer "bitwise-orc2" i)
  (check-integer "bitwise-orc2" j)
  (%bitwise-ior i (- -1 j)))

;; ---------------------------------------------------------------------------
;; Integer operations
;; ---------------------------------------------------------------------------

;; /**
;;  * The bits of `i` where `mask` has a 1, and of `j` where it has a 0.
;;  * @param {integer} mask - Which argument each bit comes from.
;;  * @param {integer} i - The bits taken where the mask is 1.
;;  * @param {integer} j - The bits taken where it is 0.
;;  * @returns {integer}
;;  */
(define (bitwise-if mask i j)
  (check-integer "bitwise-if" mask)
  (check-integer "bitwise-if" i)
  (check-integer "bitwise-if" j)
  (%bitwise-ior (%bitwise-and mask i) (%bitwise-and (- -1 mask) j)))

;; ---------------------------------------------------------------------------
;; Single-bit operations
;; ---------------------------------------------------------------------------

;; /**
;;  * The integer with only one bit set.
;;  * @param {integer} index - The bit, non-negative.
;;  * @returns {integer} 2^index.
;;  */
(define (single-bit index) (%arithmetic-shift 1 index))

;; /**
;;  * Whether a bit is set.
;;  * @param {integer} index - The bit, non-negative.
;;  * @param {integer} i - An exact integer.
;;  * @returns {boolean}
;;  */
(define (bit-set? index i)
  (check-index "bit-set?" index)
  (check-integer "bit-set?" i)
  (odd? (%arithmetic-shift i (- index))))

;; /**
;;  * The integer with one bit set or cleared.
;;  * @param {integer} index - The bit, non-negative.
;;  * @param {integer} i - An exact integer.
;;  * @param {boolean} set - #t to set the bit, #f to clear it.
;;  * @returns {integer}
;;  */
(define (copy-bit index i set)
  (check-index "copy-bit" index)
  (check-integer "copy-bit" i)
  (check-boolean "copy-bit" set)
  (if set
      (%bitwise-ior i (single-bit index))
      (%bitwise-and i (- -1 (single-bit index)))))

;; /**
;;  * The integer with two bits exchanged.
;;  * @param {integer} index1 - One bit, non-negative.
;;  * @param {integer} index2 - The other, non-negative.
;;  * @param {integer} i - An exact integer.
;;  * @returns {integer}
;;  */
(define (bit-swap index1 index2 i)
  (check-index "bit-swap" index1)
  (check-index "bit-swap" index2)
  (check-integer "bit-swap" i)
  (let ((first (bit-set? index1 i))
        (second (bit-set? index2 i)))
    (copy-bit index2 (copy-bit index1 i second) first)))

;; /**
;;  * Whether any bit set in `test-bits` is set in `i`.
;;  * @param {integer} test-bits - The bits asked about.
;;  * @param {integer} i - An exact integer.
;;  * @returns {boolean}
;;  */
(define (any-bit-set? test-bits i)
  (check-integer "any-bit-set?" test-bits)
  (check-integer "any-bit-set?" i)
  (not (zero? (%bitwise-and test-bits i))))

;; /**
;;  * Whether every bit set in `test-bits` is set in `i`.
;;  * @param {integer} test-bits - The bits asked about.
;;  * @param {integer} i - An exact integer.
;;  * @returns {boolean}
;;  */
(define (every-bit-set? test-bits i)
  (check-integer "every-bit-set?" test-bits)
  (check-integer "every-bit-set?" i)
  (= test-bits (%bitwise-and test-bits i)))

;; /**
;;  * The index of the lowest bit set, or -1 for 0. `i` and `-i` share their
;;  * lowest set bit and differ in every bit above it, so their `and` is that
;;  * bit alone.
;;  * @param {integer} i - An exact integer.
;;  * @returns {integer}
;;  */
(define (first-set-bit i)
  (check-integer "first-set-bit" i)
  (- (%integer-length (%bitwise-and i (- i))) 1))

;; ---------------------------------------------------------------------------
;; Bit field operations
;; ---------------------------------------------------------------------------

;; /**
;;  * The integer whose low `width` bits are set and no others.
;;  * @param {integer} width - How many, non-negative.
;;  * @returns {integer}
;;  */
(define (low-bits width) (- (single-bit width) 1))

;; /**
;;  * The integer with exactly a field's bits set.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer}
;;  */
(define (field-mask start end) (%arithmetic-shift (low-bits (- end start)) start))

;; /**
;;  * A field, shifted down to bit 0.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer} A non-negative integer below 2^(end - start).
;;  */
(define (bit-field i start end)
  (check-integer "bit-field" i)
  (check-field "bit-field" start end)
  (%bitwise-and (%arithmetic-shift i (- start)) (low-bits (- end start))))

;; /**
;;  * Whether any bit of a field is set.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {boolean}
;;  */
(define (bit-field-any? i start end)
  (check-integer "bit-field-any?" i)
  (check-field "bit-field-any?" start end)
  (not (zero? (%bitwise-and i (field-mask start end)))))

;; /**
;;  * Whether every bit of a field is set.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {boolean}
;;  */
(define (bit-field-every? i start end)
  (check-integer "bit-field-every?" i)
  (check-field "bit-field-every?" start end)
  (let ((mask (field-mask start end)))
    (= mask (%bitwise-and i mask))))

;; /**
;;  * The integer with a field's bits cleared.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer}
;;  */
(define (bit-field-clear i start end)
  (check-integer "bit-field-clear" i)
  (check-field "bit-field-clear" start end)
  (%bitwise-and i (- -1 (field-mask start end))))

;; /**
;;  * The integer with a field's bits set.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer}
;;  */
(define (bit-field-set i start end)
  (check-integer "bit-field-set" i)
  (check-field "bit-field-set" start end)
  (%bitwise-ior i (field-mask start end)))

;; /**
;;  * `dest` with a field replaced by the low bits of `source`.
;;  * @param {integer} dest - An exact integer.
;;  * @param {integer} source - The bits to put there, from bit 0.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer}
;;  */
(define (bit-field-replace dest source start end)
  (check-integer "bit-field-replace" dest)
  (check-integer "bit-field-replace" source)
  (check-field "bit-field-replace" start end)
  (replace-field dest (%arithmetic-shift source start) start end))

;; /**
;;  * `dest` with a field replaced by the same field of `source`.
;;  * @param {integer} dest - An exact integer.
;;  * @param {integer} source - An exact integer.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer}
;;  */
(define (bit-field-replace-same dest source start end)
  (check-integer "bit-field-replace-same" dest)
  (check-integer "bit-field-replace-same" source)
  (check-field "bit-field-replace-same" start end)
  (replace-field dest source start end))

;; /**
;;  * `dest` with a field's bits taken from `bits`, unchecked.
;;  * @param {integer} dest - An exact integer.
;;  * @param {integer} bits - The bits, in place.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer}
;;  */
(define (replace-field dest bits start end)
  (let ((mask (field-mask start end)))
    (%bitwise-ior (%bitwise-and mask bits) (%bitwise-and (- -1 mask) dest))))

;; /**
;;  * The integer with a field rotated `count` bits towards its high end, the
;;  * bits that pass the top coming back in at the bottom; a negative count
;;  * rotates towards the low end.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} count - How far, an exact integer.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer}
;;  */
(define (bit-field-rotate i count start end)
  (check-integer "bit-field-rotate" i)
  (check-integer "bit-field-rotate" count)
  (check-field "bit-field-rotate" start end)
  (let ((width (- end start)))
    (if (zero? width)
        i
        (let* ((by (modulo count width))
               (field (bit-field i start end))
               (rotated (%bitwise-ior (%bitwise-and (%arithmetic-shift field by) (low-bits width))
                                      (%arithmetic-shift field (- by width)))))
          (replace-field i (%arithmetic-shift rotated start) start end)))))

;; /**
;;  * The integer with a field's bits in the opposite order.
;;  * @param {integer} i - An exact integer.
;;  * @param {integer} start - The field's first bit.
;;  * @param {integer} end - The bit after its last.
;;  * @returns {integer}
;;  */
(define (bit-field-reverse i start end)
  (check-integer "bit-field-reverse" i)
  (check-field "bit-field-reverse" start end)
  (let loop ((k (- end start)) (field (bit-field i start end)) (reversed 0))
    (if (zero? k)
        (replace-field i (%arithmetic-shift reversed start) start end)
        (loop (- k 1)
              (%arithmetic-shift field -1)
              (%bitwise-ior (%arithmetic-shift reversed 1) (%bitwise-and field 1))))))

;; ---------------------------------------------------------------------------
;; Bits conversion
;; ---------------------------------------------------------------------------

;; /**
;;  * The low bits of a non-negative integer as booleans, bit 0 first.
;;  * @param {integer} i - A non-negative exact integer.
;;  * @param {integer} [len] - How many bits; the integer's length by default.
;;  * @returns {list} Booleans, #t for a 1 bit.
;;  */
(define bits->list
  (case-lambda
    ((i) (check-index "bits->list" i) (low-bits->list i (%integer-length i)))
    ((i len) (check-index "bits->list" i) (check-index "bits->list" len) (low-bits->list i len))))

;; /**
;;  * The low `len` bits of a non-negative integer as booleans, bit 0 first,
;;  * unchecked.
;;  * @param {integer} i - The integer.
;;  * @param {integer} len - How many bits.
;;  * @returns {list}
;;  */
(define (low-bits->list i len)
  (let loop ((k 0) (rest i) (acc '()))
    (if (= k len)
        (reverse acc)
        (loop (+ k 1) (%arithmetic-shift rest -1) (cons (odd? rest) acc)))))

;; /**
;;  * The low bits of a non-negative integer as a vector of booleans, bit 0
;;  * first.
;;  * @param {integer} i - A non-negative exact integer.
;;  * @param {integer} [len] - How many bits; the integer's length by default.
;;  * @returns {vector} Booleans, #t for a 1 bit.
;;  */
(define bits->vector
  (case-lambda
    ((i) (check-index "bits->vector" i) (list->vector (low-bits->list i (%integer-length i))))
    ((i len) (check-index "bits->vector" i) (check-index "bits->vector" len)
             (list->vector (low-bits->list i len)))))

;; /**
;;  * The non-negative integer whose bits are a list's booleans, the first
;;  * element bit 0.
;;  * @param {list} list - Booleans.
;;  * @returns {integer}
;;  */
(define (list->bits list)
  (booleans->bits "list->bits" list))

;; /**
;;  * The non-negative integer whose bits are a vector's booleans, the first
;;  * element bit 0.
;;  * @param {vector} vector - Booleans.
;;  * @returns {integer}
;;  */
(define (vector->bits vector)
  (if (not (vector? vector))
      (error "vector->bits: expected a vector" vector))
  (booleans->bits "vector->bits" (vector->list vector)))

;; /**
;;  * The non-negative integer whose bits are the arguments, the first bit 0.
;;  * @param {...boolean} booleans - The bits.
;;  * @returns {integer}
;;  */
(define (bits . booleans)
  (booleans->bits "bits" booleans))

;; /**
;;  * The integer whose bits are a list's booleans, the first bit 0: each
;;  * boolean is checked, and the list is read last to first so that each bit
;;  * shifts the rest up.
;;  * @param {string} who - The procedure, for the message.
;;  * @param {list} booleans - The bits.
;;  * @returns {integer}
;;  */
(define (booleans->bits who booleans)
  (if (not (list? booleans))
      (error (string-append who ": expected a list") booleans))
  (let loop ((bs (reverse booleans)) (acc 0))
    (if (null? bs)
        acc
        (begin
          (check-boolean who (car bs))
          (loop (cdr bs) (%bitwise-ior (%arithmetic-shift acc 1) (if (car bs) 1 0)))))))

;; ---------------------------------------------------------------------------
;; Fold, unfold and generate
;; ---------------------------------------------------------------------------

;; /**
;;  * Folds a procedure over an integer's bits, from bit 0 up to its length:
;;  * `(proc bit acc)`, each bit a boolean.
;;  * @param {procedure} proc - The procedure.
;;  * @param {*} seed - The first accumulated value.
;;  * @param {integer} i - An exact integer.
;;  * @returns {*} The last.
;;  */
(define (bitwise-fold proc seed i)
  (check-procedure "bitwise-fold" proc)
  (check-integer "bitwise-fold" i)
  (let loop ((k (%integer-length i)) (rest i) (acc seed))
    (if (zero? k)
        acc
        (loop (- k 1) (%arithmetic-shift rest -1) (proc (odd? rest) acc)))))

;; /**
;;  * Calls a procedure on an integer's bits, from bit 0 up to its length,
;;  * each bit a boolean.
;;  * @param {procedure} proc - The procedure.
;;  * @param {integer} i - An exact integer.
;;  * @returns {unspecified}
;;  */
(define (bitwise-for-each proc i)
  (check-procedure "bitwise-for-each" proc)
  (check-integer "bitwise-for-each" i)
  (let loop ((k (%integer-length i)) (rest i))
    (if (> k 0)
        (begin
          (proc (odd? rest))
          (loop (- k 1) (%arithmetic-shift rest -1))))))

;; /**
;;  * Builds a non-negative integer a bit at a time, from bit 0: while
;;  * `(stop? state)` is false, the next bit is `(mapper state)`, true for 1,
;;  * and the next state `(successor state)`.
;;  * @param {procedure} stop? - Whether to stop.
;;  * @param {procedure} mapper - The bit for a state.
;;  * @param {procedure} successor - The state after.
;;  * @param {*} seed - The first state.
;;  * @returns {integer}
;;  */
(define (bitwise-unfold stop? mapper successor seed)
  (check-procedure "bitwise-unfold" stop?)
  (check-procedure "bitwise-unfold" mapper)
  (check-procedure "bitwise-unfold" successor)
  (let loop ((state seed) (k 0) (acc 0))
    (if (stop? state)
        acc
        (loop (successor state)
              (+ k 1)
              (if (mapper state) (%bitwise-ior acc (single-bit k)) acc)))))

;; /**
;;  * A generator, in SRFI 121's sense, of an integer's bits as booleans from
;;  * bit 0, without end: past its length, a non-negative integer's bits are
;;  * #f and a negative one's #t.
;;  * @param {integer} i - An exact integer.
;;  * @returns {procedure} A procedure of no arguments returning the next bit.
;;  */
(define (make-bitwise-generator i)
  (check-integer "make-bitwise-generator" i)
  (let ((rest i))
    (lambda ()
      (let ((bit (odd? rest)))
        (set! rest (%arithmetic-shift rest -1))
        bit))))
