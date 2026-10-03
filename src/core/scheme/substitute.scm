;; Substituting one value for another inside the data that holds it.
;;
;; Part of the library system, `(scheme-js library-system)`. A compiled
;; procedure is installed where the interpreted closure it was compiled from is
;; bound, and the library system puts it wherever an import copied the
;; closure: in every library's bindings and exports
;; (`substitute-in-libraries!` in library_system.scm). A closure can also be
;; held as data. A library can make values as it loads
;; that hold its own procedures -- SRFI 128's default comparators are records
;; holding `default-hash` and three others, and a current port's parameter cell
;; is a pair holding its converter -- and a shipped library loads from its
;; source, interpreted, before its prebuilt table is installed
;; (src/compiler/prebuilt.js). Replaced only where they are bound, those
;; procedures would be two procedures each afterwards: the binding not `eq?` to
;; the copy in the value, and the copy interpreted wherever the value is used.
;; So whenever the library system substitutes -- a library's table installed,
;; a library's procedure compiled by the compiler tier, the compiled procedures
;; switched back to their closures for a debugger and back again -- it also
;; substitutes inside the values its libraries' bindings reach, with
;; `substitute-within!`.
;;
;; It looks inside pairs, vectors and records, which is what a library makes;
;; not inside a closure's environment, which Scheme cannot see into, nor a hash
;; table's store, nor the variables a compiled procedure closed over, which only
;; JavaScript can see. A library holding a procedure there goes on holding the
;; closure; `tests/functional/prebuilt_library_tests.js` checks that no shipped
;; library holds one in an environment or a hash table.

;; /**
;;  * Substitutes values wherever the pairs, vectors and records reachable from
;;  * some values hold them: a part that has a replacement is replaced in place,
;;  * and any other is looked inside in turn, each value once, however the data
;;  * is shared or circular. The values given are only looked inside; whatever
;;  * holds them replaces them.
;;  *
;;  * @param {list} roots - The values to look inside.
;;  * @param {procedure} replacement - From a value to its replacement, or #f
;;  *   to keep it.
;;  */
(define (substitute-within! roots replacement)
  (let ((seen (%make-hash-store 'eq)))
    (let walk ((pending roots))
      (cond ((null? pending))
            ((first-look? seen (car pending))
             (walk (append (substitute-parts! (car pending) replacement) (cdr pending))))
            (else (walk (cdr pending)))))))

;; /**
;;  * Whether a value is a pair, vector or record not looked inside before, and
;;  * if so, notes that it has been.
;;  * @param {object} seen - An `eq?` store of the values looked inside.
;;  * @param {*} value - The value.
;;  * @returns {boolean}
;;  */
(define (first-look? seen value)
  (and (or (pair? value) (vector? value) (%record-type value))
       (not (%hash-store-contains? seen value))
       (%hash-store-set! seen value #t)))

;; /**
;;  * Replaces each part of a pair, vector or record that has a replacement, in
;;  * place, and returns its parts as they now are.
;;  * @param {pair|vector|record} value - The value.
;;  * @param {procedure} replacement - As for `substitute-within!`.
;;  * @returns {list} Its parts.
;;  */
(define (substitute-parts! value replacement)
  ;; A part's replacement, stored in its place, if it has one; else the part.
  (define (substituted part store!)
    (let ((new (replacement part)))
      (if new
          (begin (store! new) new)
          part)))
  (cond ((pair? value)
         (list (substituted (car value) (lambda (new) (set-car! value new)))
               (substituted (cdr value) (lambda (new) (set-cdr! value new)))))
        ((vector? value)
         (let loop ((i (- (vector-length value) 1)) (parts '()))
           (if (< i 0)
               parts
               (loop (- i 1)
                     (cons (substituted (vector-ref value i) (lambda (new) (vector-set! value i new)))
                           parts)))))
        (else
         (let ((type (%record-type value)))
           (map (lambda (field)
                  (substituted ((record-accessor type field) value)
                               (lambda (new) ((record-modifier type field) value new))))
                (%record-type-fields type))))))
