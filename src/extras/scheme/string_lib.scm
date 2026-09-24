;; SRFI 152: String library (reduced)
;;
;; The procedures SRFI 152 adds to R7RS-small. Those R7RS already defines are
;; re-exported by 152.sld instead.
;;
;; Most take optional start and end indices selecting a substring, checked
;; once by `string-range`. Strings are immutable here, so a procedure that
;; would return a copy of its whole argument may return the argument itself,
;; which SRFI 152 allows.

;; ---------------------------------------------------------------------------
;; Arguments
;; ---------------------------------------------------------------------------

;; /**
;;  * Signals an error unless an argument is a procedure.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {*} f - The argument.
;;  * @returns {unspecified}
;;  */
(define (string-check-procedure who f)
  (if (not (procedure? f))
      (error (string-append who ": expected a procedure") f)))

;; /**
;;  * The start and end indices a procedure was given, defaulting to the whole
;;  * string, after checking them.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {string} s - The string.
;;  * @param {list} range - The optional arguments: '(), (start) or (start end).
;;  * @returns {pair} (start . end).
;;  */
(define (string-range who s range)
  (if (not (string? s)) (error (string-append who ": expected a string") s))
  (let* ((n (string-length s))
         (start (if (pair? range) (car range) 0))
         (end (if (and (pair? range) (pair? (cdr range))) (cadr range) n)))
    (if (not (and (exact-integer? start) (exact-integer? end) (<= 0 start end n)))
        (error (string-append who ": index out of range") start end))
    (cons start end)))

;; /**
;;  * The index of the first character in s[start, end) satisfying a
;;  * predicate, or #f -- what most searching here comes down to.
;;  * @param {string} s - The string.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {integer} start - Where to begin.
;;  * @param {integer} end - Where to stop.
;;  * @returns {integer|boolean} The index, or #f.
;;  */
(define (index-where s pred start end)
  (let loop ((i start))
    (cond ((>= i end) #f)
          ((pred (string-ref s i)) i)
          (else (loop (+ i 1))))))

;; /**
;;  * The index of the last character in s[start, end) satisfying a predicate.
;;  * @param {string} s - The string.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {integer} start - Where the range begins.
;;  * @param {integer} end - Where it ends.
;;  * @returns {integer|boolean} The index, or #f.
;;  */
(define (index-where-right s pred start end)
  (let loop ((i (- end 1)))
    (cond ((< i start) #f)
          ((pred (string-ref s i)) i)
          (else (loop (- i 1))))))

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

(define (string-null? s) (= (string-length s) 0))

;; /**
;;  * Whether a predicate holds for every character of a range: #f at the first
;;  * failure, else the last result, or #t for an empty range.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {string} s - The string.
;;  * @param {integer} [start] - Where to begin.
;;  * @param {integer} [end] - Where to stop.
;;  * @returns {*} The result.
;;  */
(define (string-every pred s . range)
  (string-check-procedure "string-every" pred)
  (let* ((r (string-range "string-every" s range)) (end (cdr r)))
    (let loop ((i (car r)) (last #t))
      (if (>= i end)
          last
          (let ((v (pred (string-ref s i))))
            (and v (loop (+ i 1) v)))))))

;; /**
;;  * The first true result of a predicate over the characters of a range.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {string} s - The string.
;;  * @param {integer} [start] - Where to begin.
;;  * @param {integer} [end] - Where to stop.
;;  * @returns {*} The result, or #f.
;;  */
(define (string-any pred s . range)
  (string-check-procedure "string-any" pred)
  (let* ((r (string-range "string-any" s range)) (end (cdr r)))
    (let loop ((i (car r)))
      (and (< i end)
           (or (pred (string-ref s i)) (loop (+ i 1)))))))

;; ---------------------------------------------------------------------------
;; Constructors
;; ---------------------------------------------------------------------------

;; /**
;;  * A string of len characters, the ith being `(proc i)`.
;;  * @param {procedure} proc - Maps an index to a character.
;;  * @param {integer} len - The length.
;;  * @returns {string} The string.
;;  */
(define (string-tabulate proc len)
  (string-check-procedure "string-tabulate" proc)
  (if (not (and (exact-integer? len) (>= len 0)))
      (error "string-tabulate: expected a non-negative exact integer" len))
  (let loop ((i (- len 1)) (chars '()))
    (if (< i 0) (list->string chars) (loop (- i 1) (cons (proc i) chars)))))

;; /**
;;  * A piece of a string being built, as a string: the mapper of an unfold may
;;  * return a character or a string.
;;  * @param {char|string} x - The piece.
;;  * @returns {string} It, as a string.
;;  */
(define (string-piece x) (if (char? x) (string x) x))

;; /**
;;  * Builds a string from a seed, left to right: `(mapper seed)`,
;;  * `(mapper (successor seed))`, ... until `(stop? seed)` holds, after `base`
;;  * and followed by `(make-final seed)`.
;;  * @param {procedure} stop? - Whether to stop.
;;  * @param {procedure} mapper - Maps a seed to a character or string.
;;  * @param {procedure} successor - Maps a seed to the next.
;;  * @param {*} seed - The first seed.
;;  * @param {string} [base=""] - What the string begins with.
;;  * @param {procedure} [make-final] - Maps the last seed to what it ends with.
;;  * @returns {string} The string.
;;  */
(define (string-unfold stop? mapper successor seed . options)
  (let ((base (if (pair? options) (car options) ""))
        (make-final (if (and (pair? options) (pair? (cdr options))) (cadr options) (lambda (x) ""))))
    (let loop ((seed seed) (pieces (list base)))
      (if (stop? seed)
          (string-concatenate-reverse (cons (string-piece (make-final seed)) pieces))
          (loop (successor seed) (cons (string-piece (mapper seed)) pieces))))))

;; /**
;;  * Builds a string from a seed, right to left: the first piece produced is
;;  * the last in the string, before `base`, and `(make-final seed)` goes first.
;;  * @param {procedure} stop? - Whether to stop.
;;  * @param {procedure} mapper - Maps a seed to a character or string.
;;  * @param {procedure} successor - Maps a seed to the next.
;;  * @param {*} seed - The first seed.
;;  * @param {string} [base=""] - What the string ends with.
;;  * @param {procedure} [make-final] - Maps the last seed to what it begins with.
;;  * @returns {string} The string.
;;  */
(define (string-unfold-right stop? mapper successor seed . options)
  (let ((base (if (pair? options) (car options) ""))
        (make-final (if (and (pair? options) (pair? (cdr options))) (cadr options) (lambda (x) ""))))
    (let loop ((seed seed) (pieces (list base)))
      (if (stop? seed)
          (string-concatenate (cons (string-piece (make-final seed)) pieces))
          (loop (successor seed) (cons (string-piece (mapper seed)) pieces))))))

;; ---------------------------------------------------------------------------
;; Conversion
;; ---------------------------------------------------------------------------

(define (reverse-list->string chars) (list->string (reverse chars)))

;; ---------------------------------------------------------------------------
;; Selection
;; ---------------------------------------------------------------------------

;; /**
;;  * Checks a character count against a string's length.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {string} s - The string.
;;  * @param {integer} n - The count.
;;  * @returns {unspecified}
;;  */
(define (check-count-within who s n)
  (if (not (and (exact-integer? n) (<= 0 n (string-length s))))
      (error (string-append who ": count out of range") n)))

(define (string-take s n) (check-count-within "string-take" s n) (substring s 0 n))
(define (string-drop s n) (check-count-within "string-drop" s n) (substring s n (string-length s)))
(define (string-take-right s n)
  (check-count-within "string-take-right" s n)
  (substring s (- (string-length s) n) (string-length s)))
(define (string-drop-right s n)
  (check-count-within "string-drop-right" s n)
  (substring s 0 (- (string-length s) n)))

;; /**
;;  * A range of a string made exactly len characters long: padded on the left
;;  * with a character, or truncated from the left.
;;  * @param {string} s - The string.
;;  * @param {integer} len - The length wanted.
;;  * @param {char} [char=#\space] - The padding.
;;  * @param {integer} [start] - Where the range begins.
;;  * @param {integer} [end] - Where it ends.
;;  * @returns {string} The result.
;;  */
(define (string-pad s len . options)
  (let* ((char (if (pair? options) (car options) #\space))
         (r (string-range "string-pad" s (if (pair? options) (cdr options) '())))
         (start (car r)) (end (cdr r)) (n (- end start)))
    (if (<= len n)
        (substring s (- end len) end)
        (string-append (make-string (- len n) char) (substring s start end)))))

;; /**
;;  * A range of a string made exactly len characters long: padded on the
;;  * right, or truncated from the right.
;;  * @param {string} s - The string.
;;  * @param {integer} len - The length wanted.
;;  * @param {char} [char=#\space] - The padding.
;;  * @param {integer} [start] - Where the range begins.
;;  * @param {integer} [end] - Where it ends.
;;  * @returns {string} The result.
;;  */
(define (string-pad-right s len . options)
  (let* ((char (if (pair? options) (car options) #\space))
         (r (string-range "string-pad-right" s (if (pair? options) (cdr options) '())))
         (start (car r)) (end (cdr r)) (n (- end start)))
    (if (<= len n)
        (substring s start (+ start len))
        (string-append (substring s start end) (make-string (- len n) char)))))

;; /**
;;  * A range of a string without the characters at its left that satisfy a
;;  * predicate, whitespace by default.
;;  * @param {string} s - The string.
;;  * @param {procedure} [pred=char-whitespace?] - What to trim.
;;  * @param {integer} [start] - Where the range begins.
;;  * @param {integer} [end] - Where it ends.
;;  * @returns {string} The result.
;;  */
(define (string-trim s . options)
  (let* ((pred (if (pair? options) (car options) char-whitespace?))
         (r (string-range "string-trim" s (if (pair? options) (cdr options) '())))
         (from (index-where s (lambda (c) (not (pred c))) (car r) (cdr r))))
    (if from (substring s from (cdr r)) "")))

;; /**
;;  * A range of a string without the characters at its right that satisfy a
;;  * predicate, whitespace by default.
;;  * @param {string} s - The string.
;;  * @param {procedure} [pred=char-whitespace?] - What to trim.
;;  * @param {integer} [start] - Where the range begins.
;;  * @param {integer} [end] - Where it ends.
;;  * @returns {string} The result.
;;  */
(define (string-trim-right s . options)
  (let* ((pred (if (pair? options) (car options) char-whitespace?))
         (r (string-range "string-trim-right" s (if (pair? options) (cdr options) '())))
         (to (index-where-right s (lambda (c) (not (pred c))) (car r) (cdr r))))
    (if to (substring s (car r) (+ to 1)) "")))

;; /**
;;  * A range of a string trimmed at both ends.
;;  * @param {string} s - The string.
;;  * @param {procedure} [pred=char-whitespace?] - What to trim.
;;  * @param {integer} [start] - Where the range begins.
;;  * @param {integer} [end] - Where it ends.
;;  * @returns {string} The result.
;;  */
(define (string-trim-both s . options)
  (let ((pred (if (pair? options) (car options) char-whitespace?)))
    (string-trim (apply string-trim-right s pred (if (pair? options) (cdr options) '())) pred)))

;; ---------------------------------------------------------------------------
;; Replacement
;; ---------------------------------------------------------------------------

;; /**
;;  * s1 with the characters from start1 to end1 replaced by a range of s2.
;;  * @param {string} s1 - The string.
;;  * @param {string} s2 - The replacement.
;;  * @param {integer} start1 - Where the replaced range begins.
;;  * @param {integer} end1 - Where it ends.
;;  * @param {integer} [start2] - Where the replacement's range begins.
;;  * @param {integer} [end2] - Where it ends.
;;  * @returns {string} The result.
;;  */
(define (string-replace s1 s2 start1 end1 . range2)
  (let ((r1 (string-range "string-replace" s1 (list start1 end1)))
        (r2 (string-range "string-replace" s2 range2)))
    (string-append (substring s1 0 start1)
                   (substring s2 (car r2) (cdr r2))
                   (substring s1 end1 (string-length s1)))))

;; ---------------------------------------------------------------------------
;; Prefixes and suffixes
;; ---------------------------------------------------------------------------

;; /**
;;  * The ranges of two strings a procedure was given, from its optional
;;  * arguments start1 end1 start2 end2.
;;  * @param {string} who - The procedure, for messages.
;;  * @param {string} s1 - The first string.
;;  * @param {string} s2 - The second.
;;  * @param {list} range - The optional arguments.
;;  * @returns {list} (start1 end1 start2 end2).
;;  */
(define (two-ranges who s1 s2 range)
  (let ((r1 (string-range who s1 (if (pair? range) range '())))
        (r2 (string-range who s2 (if (and (pair? range) (pair? (cdr range))) (cddr range) '()))))
    (list (car r1) (cdr r1) (car r2) (cdr r2))))

;; /**
;;  * How many characters two ranges have in common at their start.
;;  * @param {string} s1 - The first string.
;;  * @param {string} s2 - The second.
;;  * @param {...integer} range - start1 end1 start2 end2, all optional.
;;  * @returns {integer} The length of the common prefix.
;;  */
(define (string-prefix-length s1 s2 . range)
  (let* ((r (two-ranges "string-prefix-length" s1 s2 range))
         (start1 (car r)) (end1 (cadr r)) (start2 (caddr r)) (end2 (cadddr r)))
    (let loop ((i 0))
      (if (and (< (+ start1 i) end1) (< (+ start2 i) end2)
               (char=? (string-ref s1 (+ start1 i)) (string-ref s2 (+ start2 i))))
          (loop (+ i 1))
          i))))

;; /**
;;  * How many characters two ranges have in common at their end.
;;  * @param {string} s1 - The first string.
;;  * @param {string} s2 - The second.
;;  * @param {...integer} range - start1 end1 start2 end2, all optional.
;;  * @returns {integer} The length of the common suffix.
;;  */
(define (string-suffix-length s1 s2 . range)
  (let* ((r (two-ranges "string-suffix-length" s1 s2 range))
         (start1 (car r)) (end1 (cadr r)) (start2 (caddr r)) (end2 (cadddr r)))
    (let loop ((i 0))
      (if (and (> (- end1 i) start1) (> (- end2 i) start2)
               (char=? (string-ref s1 (- end1 i 1)) (string-ref s2 (- end2 i 1))))
          (loop (+ i 1))
          i))))

;; /**
;;  * Whether a range of s1 is a prefix of a range of s2.
;;  * @param {string} s1 - The candidate prefix.
;;  * @param {string} s2 - The string.
;;  * @param {...integer} range - start1 end1 start2 end2, all optional.
;;  * @returns {boolean}
;;  */
(define (string-prefix? s1 s2 . range)
  (let ((r (two-ranges "string-prefix?" s1 s2 range)))
    (= (apply string-prefix-length s1 s2 r) (- (cadr r) (car r)))))

;; /**
;;  * Whether a range of s1 is a suffix of a range of s2.
;;  * @param {string} s1 - The candidate suffix.
;;  * @param {string} s2 - The string.
;;  * @param {...integer} range - start1 end1 start2 end2, all optional.
;;  * @returns {boolean}
;;  */
(define (string-suffix? s1 s2 . range)
  (let ((r (two-ranges "string-suffix?" s1 s2 range)))
    (= (apply string-suffix-length s1 s2 r) (- (cadr r) (car r)))))

;; ---------------------------------------------------------------------------
;; Searching
;; ---------------------------------------------------------------------------

;; /**
;;  * The index of the first character of a range satisfying a predicate.
;;  * @param {string} s - The string.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {integer} [start] - Where to begin.
;;  * @param {integer} [end] - Where to stop.
;;  * @returns {integer|boolean} The index, or #f.
;;  */
(define (string-index s pred . range)
  (string-check-procedure "string-index" pred)
  (let ((r (string-range "string-index" s range)))
    (index-where s pred (car r) (cdr r))))

(define (string-index-right s pred . range)
  (string-check-procedure "string-index-right" pred)
  (let ((r (string-range "string-index-right" s range)))
    (index-where-right s pred (car r) (cdr r))))

;; /**
;;  * The index of the first character of a range not satisfying a predicate.
;;  * @param {string} s - The string.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {integer} [start] - Where to begin.
;;  * @param {integer} [end] - Where to stop.
;;  * @returns {integer|boolean} The index, or #f.
;;  */
(define (string-skip s pred . range)
  (string-check-procedure "string-skip" pred)
  (apply string-index s (lambda (c) (not (pred c))) range))

(define (string-skip-right s pred . range)
  (string-check-procedure "string-skip-right" pred)
  (apply string-index-right s (lambda (c) (not (pred c))) range))

;; /**
;;  * Whether s2[start2, end2) occurs in s1 at index i.
;;  * @param {string} s1 - The string searched.
;;  * @param {integer} i - Where in it.
;;  * @param {string} s2 - The string sought.
;;  * @param {integer} start2 - Where its range begins.
;;  * @param {integer} end2 - Where it ends.
;;  * @returns {boolean}
;;  */
(define (occurs-at? s1 i s2 start2 end2)
  (let loop ((k 0))
    (or (= (+ start2 k) end2)
        (and (char=? (string-ref s1 (+ i k)) (string-ref s2 (+ start2 k)))
             (loop (+ k 1))))))

;; /**
;;  * The index in s1 at which a range of s2 first occurs within a range of s1.
;;  * @param {string} s1 - The string searched.
;;  * @param {string} s2 - The string sought.
;;  * @param {...integer} range - start1 end1 start2 end2, all optional.
;;  * @returns {integer|boolean} The index, or #f.
;;  */
(define (string-contains s1 s2 . range)
  (let* ((r (two-ranges "string-contains" s1 s2 range))
         (start1 (car r)) (end1 (cadr r)) (start2 (caddr r)) (end2 (cadddr r))
         (last (- end1 (- end2 start2))))
    (let loop ((i start1))
      (cond ((> i last) #f)
            ((occurs-at? s1 i s2 start2 end2) i)
            (else (loop (+ i 1)))))))

;; /**
;;  * The index in s1 at which a range of s2 last occurs within a range of s1.
;;  * @param {string} s1 - The string searched.
;;  * @param {string} s2 - The string sought.
;;  * @param {...integer} range - start1 end1 start2 end2, all optional.
;;  * @returns {integer|boolean} The index, or #f.
;;  */
(define (string-contains-right s1 s2 . range)
  (let* ((r (two-ranges "string-contains-right" s1 s2 range))
         (start1 (car r)) (end1 (cadr r)) (start2 (caddr r)) (end2 (cadddr r)))
    (let loop ((i (- end1 (- end2 start2))))
      (cond ((< i start1) #f)
            ((occurs-at? s1 i s2 start2 end2) i)
            (else (loop (- i 1)))))))

;; ---------------------------------------------------------------------------
;; Concatenation
;; ---------------------------------------------------------------------------

;; /**
;;  * The strings in a list, appended. Built one at a time: that is linear, not
;;  * quadratic, because a JavaScript engine joins strings as a rope and
;;  * flattens it once, when it is read.
;;  * @param {list} strings - The strings.
;;  * @returns {string} The result.
;;  */
(define (string-concatenate strings)
  (let loop ((strings strings) (acc ""))
    (if (null? strings) acc (loop (cdr strings) (string-append acc (car strings))))))

;; /**
;;  * The strings in a list, reversed and appended, followed by the first end
;;  * characters of final-string.
;;  * @param {list} strings - The strings.
;;  * @param {string} [final-string=""] - What follows them.
;;  * @param {integer} [end] - How much of it.
;;  * @returns {string} The result.
;;  */
(define (string-concatenate-reverse strings . options)
  (let* ((final (if (pair? options) (car options) ""))
         (end (if (and (pair? options) (pair? (cdr options))) (cadr options) (string-length final))))
    (string-append (string-concatenate (reverse strings)) (substring final 0 end))))

;; /**
;;  * Strings joined by a delimiter. The grammar says where delimiters go:
;;  * `infix` between the strings, `strict-infix` the same but refusing an
;;  * empty list, `prefix` before each and `suffix` after each.
;;  * @param {list} strings - The strings.
;;  * @param {string} [delimiter=" "] - The delimiter.
;;  * @param {symbol} [grammar='infix] - Where it goes.
;;  * @returns {string} The result.
;;  */
(define (string-join strings . options)
  (let ((delimiter (if (pair? options) (car options) " "))
        (grammar (if (and (pair? options) (pair? (cdr options))) (cadr options) 'infix)))
    (cond ((and (null? strings) (eq? grammar 'strict-infix))
           (error "string-join: an empty list cannot be joined with a strict-infix grammar"))
          ((null? strings) "")
          ((eq? grammar 'prefix)
           (string-concatenate (append-map-strings (lambda (s) (list delimiter s)) strings)))
          ((eq? grammar 'suffix)
           (string-concatenate (append-map-strings (lambda (s) (list s delimiter)) strings)))
          ((memq grammar '(infix strict-infix))
           (string-concatenate
             (cons (car strings) (append-map-strings (lambda (s) (list delimiter s)) (cdr strings)))))
          (else (error "string-join: unknown grammar" grammar)))))

;; /**
;;  * Maps a procedure returning a list of strings over a list and appends
;;  * the results.
;;  * @param {procedure} f - Takes an element; returns a list.
;;  * @param {list} l - The list.
;;  * @returns {list} The results, appended.
;;  */
(define (append-map-strings f l)
  (let loop ((l l) (acc '()))
    (if (null? l) (reverse acc) (loop (cdr l) (append (reverse (f (car l))) acc)))))

;; ---------------------------------------------------------------------------
;; Fold and map
;; ---------------------------------------------------------------------------

;; /**
;;  * Folds a procedure over the characters of a range, left to right:
;;  * `(kons char acc)`.
;;  * @param {procedure} kons - Takes a character and the accumulator.
;;  * @param {*} knil - The initial accumulator.
;;  * @param {string} s - The string.
;;  * @param {integer} [start] - Where to begin.
;;  * @param {integer} [end] - Where to stop.
;;  * @returns {*} The final accumulator.
;;  */
(define (string-fold kons knil s . range)
  (string-check-procedure "string-fold" kons)
  (let* ((r (string-range "string-fold" s range)) (end (cdr r)))
    (let loop ((i (car r)) (acc knil))
      (if (>= i end) acc (loop (+ i 1) (kons (string-ref s i) acc))))))

;; /**
;;  * Folds a procedure over the characters of a range, right to left.
;;  * @param {procedure} kons - Takes a character and the accumulator.
;;  * @param {*} knil - The initial accumulator.
;;  * @param {string} s - The string.
;;  * @param {integer} [start] - Where the range begins.
;;  * @param {integer} [end] - Where it ends.
;;  * @returns {*} The final accumulator.
;;  */
(define (string-fold-right kons knil s . range)
  (string-check-procedure "string-fold-right" kons)
  (let* ((r (string-range "string-fold-right" s range)) (start (car r)))
    (let loop ((i (- (cdr r) 1)) (acc knil))
      (if (< i start) acc (loop (- i 1) (kons (string-ref s i) acc))))))

;; /**
;;  * How many characters of a range satisfy a predicate.
;;  * @param {string} s - The string.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {integer} [start] - Where to begin.
;;  * @param {integer} [end] - Where to stop.
;;  * @returns {integer} The count.
;;  */
(define (string-count s pred . range)
  (string-check-procedure "string-count" pred)
  (apply string-fold (lambda (c n) (if (pred c) (+ n 1) n)) 0 s range))

;; /**
;;  * The characters of a range that satisfy a predicate, as a string.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {string} s - The string.
;;  * @param {integer} [start] - Where to begin.
;;  * @param {integer} [end] - Where to stop.
;;  * @returns {string} The result.
;;  */
(define (string-filter pred s . range)
  (string-check-procedure "string-filter" pred)
  (reverse-list->string
    (apply string-fold (lambda (c kept) (if (pred c) (cons c kept) kept)) '() s range)))

;; /**
;;  * The characters of a range that do not satisfy a predicate, as a string.
;;  * @param {procedure} pred - Takes a character.
;;  * @param {string} s - The string.
;;  * @param {integer} [start] - Where to begin.
;;  * @param {integer} [end] - Where to stop.
;;  * @returns {string} The result.
;;  */
(define (string-remove pred s . range)
  (string-check-procedure "string-remove" pred)
  (apply string-filter (lambda (c) (not (pred c))) s range))

;; ---------------------------------------------------------------------------
;; Replication and splitting
;; ---------------------------------------------------------------------------

;; /**
;;  * Characters from..to of a range of s repeated without end in both
;;  * directions: index 0 is the range's first character.
;;  * @param {string} s - The string.
;;  * @param {integer} from - The first index, which may be negative.
;;  * @param {integer} to - The index to stop before.
;;  * @param {integer} [start] - Where the range begins.
;;  * @param {integer} [end] - Where it ends.
;;  * @returns {string} The result.
;;  */
(define (string-replicate s from to . range)
  (let* ((r (string-range "string-replicate" s range))
         (start (car r)) (n (- (cdr r) start)))
    (cond ((not (and (exact-integer? from) (exact-integer? to) (<= from to)))
           (error "string-replicate: expected from <= to" from to))
          ((= from to) "")
          ((= n 0) (error "string-replicate: cannot replicate an empty string"))
          (else (string-tabulate (lambda (i) (string-ref s (+ start (modulo (+ from i) n))))
                                 (- to from))))))

;; /**
;;  * A range of a string split into the fields between occurrences of a
;;  * delimiter string. With `prefix` a leading delimiter, and with `suffix` a
;;  * trailing one, is dropped rather than making an empty field; with a limit,
;;  * at most that many splits are made. An empty range has no fields.
;;  * @param {string} s - The string.
;;  * @param {string} delimiter - What separates fields; not empty.
;;  * @param {symbol} [grammar='infix] - infix, strict-infix, prefix or suffix.
;;  * @param {integer|boolean} [limit=#f] - The most splits to make.
;;  * @param {integer} [start] - Where the range begins.
;;  * @param {integer} [end] - Where it ends.
;;  * @returns {list} The fields.
;;  */
(define (string-split s delimiter . options)
  (let* ((grammar (if (pair? options) (car options) 'infix))
         (rest (if (pair? options) (cdr options) '()))
         (limit (if (pair? rest) (car rest) #f))
         (r (string-range "string-split" s (if (pair? rest) (cdr rest) '())))
         (start (car r)) (end (cdr r))
         (d (string-length delimiter)))
    (if (= d 0) (error "string-split: the delimiter cannot be empty"))
    (if (not (memq grammar '(infix strict-infix prefix suffix)))
        (error "string-split: unknown grammar" grammar))
    (if (= start end)
        (if (eq? grammar 'strict-infix)
            (error "string-split: an empty string cannot be split with a strict-infix grammar")
            '())
        (let* ((start (if (and (eq? grammar 'prefix) (string-prefix? delimiter s 0 d start end))
                          (+ start d) start))
               (end (if (and (eq? grammar 'suffix) (string-suffix? delimiter s 0 d start end))
                        (- end d) end)))
          (let loop ((from start) (splits 0) (fields '()))
            (let ((at (and (or (not limit) (< splits limit))
                           (string-contains s delimiter from end))))
              (if at
                  (loop (+ at d) (+ splits 1) (cons (substring s from at) fields))
                  (reverse (cons (substring s from end) fields)))))))))
