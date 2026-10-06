;;; printer.scm -- how `write`, `display`, `write-shared` and `write-simple`
;;; write a datum (R7RS 6.13.3), and the text the REPLs show for a value.
;;;
;;; A datum is written as its external representation: by `write` so that it
;;; reads back as itself -- a string in quotes with its escapes, a character
;;; by its name, a symbol between bars where it would read otherwise -- and by
;;; `display` as its characters; a number as `number->string` writes it; and a
;;; pair, a vector, or an object -- a record or a JavaScript object, written
;;; #{(key value) ...} -- by the values it holds.
;;;
;;; Datum labels (R7RS 2.4) go to the objects a labelling picks: 'cycles, one
;;; object in each cycle and no others, as `write` and `display` must, so that
;;; a datum with no cycle has none; 'shared, every object written more than
;;; once, as `write-shared` does; or 'none, as `write-simple` does, which does
;;; not end on circular structure. Labels are numbered in the order they are
;;; written.
;;;
;;; What only JavaScript can say about a value -- whether it is an object
;;; written by its fields and what they are, a procedure's name, whether it is
;;; a continuation, several values, a host value's own text -- the printer's
;;; door says (src/core/primitives/io/printer.js), which also hands
;;; JavaScript this printer's text.

;; ---------------------------------------------------------------------------
;; Datum labels
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether a value is written with the values it holds: a pair, a vector, or
;;  * an object written #{...}. Asked of every value written, so a symbol, the
;;  * commonest atom in data that is code, is let go at the second test, and
;;  * only a value that is neither asks JavaScript whether it is an object.
;;  * @param {*} x - The value.
;;  * @returns {boolean}
;;  */
(define (compound? x)
  (cond ((pair? x) #t)
        ((symbol? x) #f)
        (else (or (vector? x) (%host-object? x)))))

;; /**
;;  * How many pairs, vectors and objects `write` and `display` walk a datum
;;  * as a tree, looking for nothing, before they look for its cycles as a
;;  * graph's, which takes a store of every object reached.
;;  * @type {integer}
;;  */
(define label-tree-budget 1000)

;; /**
;;  * What is left of a budget of pairs, vectors and objects once a datum is
;;  * walked as a tree, or a negative number if it runs out first. A datum
;;  * walked within the budget has no cycle, since a walk into one would not
;;  * end; one with shared structure is walked once for each way it is reached.
;;  * This is how `equal?` compares its arguments (equality.scm), after Adams
;;  * and Dybvig, "Efficient Nondestructive Equality Checking for Trees and
;;  * Graphs" (ICFP 2008).
;;  * @param {*} x - The datum.
;;  * @param {integer} k - The budget.
;;  * @returns {integer}
;;  */
(define (tree-budget-left x k)
  (cond ((< k 0) k)
        ((pair? x)
         (let loop ((x x) (k k))
           (cond ((< k 0) k)
                 ((pair? x) (loop (cdr x) (tree-budget-left (car x) (- k 1))))
                 (else (tree-budget-left x k)))))
        ((symbol? x) k)
        ((vector? x)
         (let loop ((i 0) (k (- k 1)))
           (if (or (< k 0) (= i (vector-length x)))
               k
               (loop (+ i 1) (tree-budget-left (vector-ref x i) k)))))
        ((%host-object? x)
         (let loop ((fields (%host-object-fields x)) (k (- k 1)))
           (if (or (< k 0) (null? fields))
               k
               (loop (cdr fields) (tree-budget-left (cdr (car fields)) k)))))
        (else k)))

;; /**
;;  * The objects a datum's written form gives labels, in an `eq?` store, or #f
;;  * where there are none.
;;  *
;;  * With 'cycles, a datum small enough to walk as a tree has none. Otherwise
;;  * a depth-first walk: an object reached again while it is still being
;;  * walked, an ancestor of where it is reached, closes a cycle, and every
;;  * cycle has one such object -- the first of it the walk reaches. With
;;  * 'shared, an object reached again after its walk has ended is labelled
;;  * too. A list's pairs are walked down its cdrs in a loop, so that a long
;;  * list takes no recursion per element.
;;  * @param {*} root - A pair, vector or object.
;;  * @param {symbol} labelling - 'cycles or 'shared.
;;  * @returns {object|boolean} The store, or #f.
;;  */
(define (labelled-objects root labelling)
  (if (and (eq? labelling 'cycles) (>= (tree-budget-left root label-tree-budget) 0))
      #f
      (labelled-in-graph root labelling)))

;; /**
;;  * The objects a datum's written form gives labels, found by the
;;  * depth-first walk `labelled-objects` describes.
;;  * @param {*} root - A pair, vector or object.
;;  * @param {symbol} labelling - 'cycles or 'shared.
;;  * @returns {object|boolean} The store, or #f.
;;  */
(define (labelled-in-graph root labelling)
  (let ((state (%make-hash-store 'eq))
        (labelled (%make-hash-store 'eq)))
    (define (visit x)
      (if (compound? x)
          (let ((reached (%hash-store-ref state x #f)))
            (cond (reached
                   (if (or (eq? reached 'walking) (eq? labelling 'shared))
                       (%hash-store-set! labelled x #t)))
                  ((pair? x) (visit-list x))
                  (else
                   (%hash-store-set! state x 'walking)
                   (if (vector? x)
                       (vector-for-each visit x)
                       (for-each (lambda (field) (visit (cdr field))) (%host-object-fields x)))
                   (%hash-store-set! state x 'walked))))))
    (define (visit-list first)
      (let loop ((pair first) (count 0))
        (if (and (pair? pair) (not (%hash-store-contains? state pair)))
            (begin
              (%hash-store-set! state pair 'walking)
              (visit (car pair))
              (loop (cdr pair) (+ count 1)))
            (begin
              (visit pair)
              ;; The list's pairs, walked, are no longer being walked.
              (let mark ((pair first) (count count))
                (if (> count 0)
                    (begin
                      (%hash-store-set! state pair 'walked)
                      (mark (cdr pair) (- count 1)))))))))
    (visit root)
    (if (> (%hash-store-size labelled) 0) labelled #f)))

;; ---------------------------------------------------------------------------
;; Writing a datum
;; ---------------------------------------------------------------------------

;; /**
;;  * Writes a datum to an output port.
;;  * @param {*} x - The datum.
;;  * @param {port} port - The port.
;;  * @param {boolean} display? - Whether it is displayed rather than written.
;;  * @param {symbol} labelling - 'cycles, 'shared or 'none.
;;  * @returns {unspecified}
;;  */
(define (print-datum x port display? labelling)
  (if (compound? x)
      (print-compound x port display?
                      (if (eq? labelling 'none) #f (labelled-objects x labelling)))
      (print-atom x port display?)))

;; /**
;;  * Writes a pair, vector or object, with the labels a store gives.
;;  * @param {*} root - The datum.
;;  * @param {port} port - The port.
;;  * @param {boolean} display? - Whether it is displayed.
;;  * @param {object|boolean} labelled - The objects to label, or #f.
;;  * @returns {unspecified}
;;  */
(define (print-compound root port display? labelled)
  (let ((numbers (and labelled (%make-hash-store 'eq))))
    (define (put text) (%write-string text port))
    (define (labelled? x) (and labelled (%hash-store-contains? labelled x)))
    (define (emit x)
      (cond ((not (compound? x)) (print-atom x port display?))
            ((not (labelled? x)) (body x))
            ((%hash-store-ref numbers x #f)
             => (lambda (n) (put "#") (put (number->string n)) (put "#")))
            (else
             (let ((n (%hash-store-size numbers)))
               (%hash-store-set! numbers x n)
               (put "#") (put (number->string n)) (put "=")
               (body x)))))
    (define (body x)
      (cond ((pair? x)
             (put "(")
             (emit (car x))
             ;; A pair in the list's tail that has a label ends the list after
             ;; a dot, so that its label can be written.
             (let loop ((rest (cdr x)))
               (cond ((null? rest) (put ")"))
                     ((and (pair? rest) (not (labelled? rest)))
                      (put " ")
                      (emit (car rest))
                      (loop (cdr rest)))
                     (else (put " . ") (emit rest) (put ")")))))
            ((vector? x)
             (put "#(")
             (let loop ((i 0))
               (if (< i (vector-length x))
                   (begin
                     (if (> i 0) (put " "))
                     (emit (vector-ref x i))
                     (loop (+ i 1)))))
             (put ")"))
            (else
             (put "#{")
             (let loop ((fields (%host-object-fields x)) (first? #t))
               (if (pair? fields)
                   (begin
                     (if (not first?) (put " "))
                     (put "(")
                     (put (object-key-text (car (car fields))))
                     (put " ")
                     (emit (cdr (car fields)))
                     (put ")")
                     (loop (cdr fields) #f))))
             (put "}"))))
    (emit root)))

;; /**
;;  * Writes a value that holds no other values.
;;  * @param {*} x - The value.
;;  * @param {port} port - The port.
;;  * @param {boolean} display? - Whether it is displayed.
;;  * @returns {unspecified}
;;  */
(define (print-atom x port display?)
  (cond ((symbol? x)
         (%write-string (if display? (symbol->string x) (symbol-text (symbol->string x))) port))
        ((number? x) (%write-string (number->string x) port))
        ((string? x) (if display? (%write-string x port) (print-string-literal x port)))
        ((char? x) (if display? (%write-char x port) (%write-string (char-text x) port)))
        ((null? x) (%write-string "()" port))
        ((eq? x #t) (%write-string "#t" port))
        ((eq? x #f) (%write-string "#f" port))
        ((eof-object? x) (%write-string "#<eof>" port))
        ((bytevector? x) (print-bytevector x port))
        ((procedure? x) (%write-string (procedure-text x) port))
        (else (%write-string (%host-text x) port))))

;; ---------------------------------------------------------------------------
;; The text of an atom
;; ---------------------------------------------------------------------------

;; /**
;;  * The names `write` gives characters, R7RS 6.6's, by code point.
;;  */
(define character-names
  '((0 . "null") (7 . "alarm") (8 . "backspace") (9 . "tab") (10 . "newline")
    (13 . "return") (27 . "escape") (32 . "space") (127 . "delete")))

;; /**
;;  * A character as `write` writes it: by its name, or by its code if it is
;;  * another control character, or itself.
;;  * @param {char} c - The character.
;;  * @returns {string}
;;  */
(define (char-text c)
  (let* ((code (char->integer c))
         (name (assv code character-names)))
    (cond (name (string-append "#\\" (cdr name)))
          ((< code 32) (string-append "#\\x" (number->string code 16)))
          (else (string-append "#\\" (string c))))))

;; /**
;;  * The characters `write` writes escaped in a string: a double quote, a
;;  * backslash, and the control characters.
;;  */
(define string-escaped-characters
  (let loop ((code 31) (chars (list #\" #\\ (integer->char 127))))
    (if (< code 0)
        (list->string chars)
        (loop (- code 1) (cons (integer->char code) chars)))))

;; /**
;;  * The escape `write` writes a character of a string as, one of
;;  * `string-escaped-characters`: R7RS 6.7's escapes, or the character's code.
;;  * @param {char} c - The character.
;;  * @returns {string}
;;  */
(define (string-escape c)
  (let ((code (char->integer c)))
    (cond ((char=? c #\") "\\\"")
          ((char=? c #\\) "\\\\")
          ((= code 7) "\\a")
          ((= code 8) "\\b")
          ((= code 9) "\\t")
          ((= code 10) "\\n")
          ((= code 13) "\\r")
          (else (string-append "\\x" (number->string code 16) ";")))))

;; /**
;;  * Writes a string as `write` writes it, between quotes and with its
;;  * escapes: the run of characters up to the next that needs one at a time,
;;  * found in one call.
;;  * @param {string} s - The string.
;;  * @param {port} port - The port.
;;  * @returns {unspecified}
;;  */
(define (print-string-literal s port)
  (let ((n (string-length s)))
    (%write-string "\"" port)
    (let loop ((start 0))
      (let ((i (%string-find-any s string-escaped-characters start)))
        (if (< start i) (%write-string s port start i))
        (if (< i n)
            (begin
              (%write-string (string-escape (string-ref s i)) port)
              (loop (+ i 1))))))
    (%write-string "\"" port)))

;; /**
;;  * A string as `write` writes it.
;;  * @param {string} s - The string.
;;  * @returns {string}
;;  */
(define (string-literal s)
  (let ((port (open-output-string)))
    (print-string-literal s port)
    (get-output-string port)))

;; /**
;;  * The characters a symbol's name cannot hold unless it is written between
;;  * bars: the delimiters of R7RS 7.1.1, the other characters the reader gives
;;  * a meaning, and white space, as `char-whitespace?` has it.
;;  */
(define symbol-delimiters
  (list->string
   (append (string->list "\"'`,;()[]{}|\\")
           (map integer->char
                '(9 10 11 12 13 32 #x85 #xA0 #x1680
                  #x2000 #x2001 #x2002 #x2003 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A
                  #x2028 #x2029 #x202F #x205F #x3000)))))

;; /**
;;  * Whether a symbol's name would not read back as the symbol unless it is
;;  * written between bars: empty or ".", holding a delimiter, beginning with
;;  * #, or looking like a number or the start of one. Asked of every symbol
;;  * `write` writes, so the delimiters are looked for in one call, and the
;;  * rest asks about the first characters only.
;;  * @param {string} name - The name.
;;  * @returns {boolean}
;;  */
(define (symbol-needs-bars? name)
  (let ((n (string-length name)))
    (or (= n 0)
        (< (%string-find-any name symbol-delimiters 0) n)
        (let ((first (char->integer (string-ref name 0))))
          (or (= first 35)                     ; #
              (and (= n 1) (= first 46))       ; .
              (looks-like-number? name n first))))))

;; /**
;;  * Whether a name would read as a number, or begins as one would: after an
;;  * optional sign, a digit, or a dot and a digit, or an infinity or NaN; or
;;  * a sign and i alone. Its characters are compared by code, which compiled
;;  * code does inline, where `char=?` and `char-ci=?` are calls.
;;  * @param {string} name - The name, not empty.
;;  * @param {integer} n - Its length.
;;  * @param {integer} first - The code of its first character.
;;  * @returns {boolean}
;;  */
(define (looks-like-number? name n first)
  (let* ((start (if (or (= first 43) (= first 45)) 1 0))     ; + or -
         (c (if (< start n) (char->integer (string-ref name start)) 0)))
    (or (ascii-digit? c)
        (and (= c 46)                                         ; .
             (< (+ start 1) n)
             (ascii-digit? (char->integer (string-ref name (+ start 1)))))
        (and (= start 1) (= n 2) (or (= c 105) (= c 73)))     ; i or I
        (and (>= (- n start) 5)
             (or (= c 105) (= c 73) (= c 110) (= c 78))       ; i, I, n or N
             (let ((special (string-downcase (substring name start (+ start 5)))))
               (or (string=? special "inf.0") (string=? special "nan.0")))))))

;; /**
;;  * Whether a character code is an ASCII digit's: the only digits a number's
;;  * text can begin with.
;;  * @param {integer} code - The code.
;;  * @returns {boolean}
;;  */
(define (ascii-digit? code)
  (and (>= code 48) (<= code 57)))

;; /**
;;  * A symbol's name as `write` writes it: between bars, a bar or backslash
;;  * in it escaped, where it needs them.
;;  * @param {string} name - The name.
;;  * @returns {string}
;;  */
(define (symbol-text name)
  (if (symbol-needs-bars? name)
      (let loop ((chars (string->list name)) (out '()))
        (cond ((null? chars) (list->string (cons #\| (reverse (cons #\| out)))))
              ((memv (car chars) '(#\| #\\)) (loop (cdr chars) (cons (car chars) (cons #\\ out))))
              (else (loop (cdr chars) (cons (car chars) out)))))
      name))

;; /**
;;  * An object's key as #{...} writes it: as it is where it would read as a
;;  * symbol, and as a string otherwise.
;;  * @param {string} key - The key.
;;  * @returns {string}
;;  */
(define (object-key-text key)
  (if (symbol-needs-bars? key) (string-literal key) key))

;; /**
;;  * Writes a bytevector as #u8(...).
;;  * @param {bytevector} bv - The bytevector.
;;  * @param {port} port - The port.
;;  * @returns {unspecified}
;;  */
(define (print-bytevector bv port)
  (%write-string "#u8(" port)
  (let loop ((i 0))
    (if (< i (bytevector-length bv))
        (begin
          (if (> i 0) (%write-string " " port))
          (%write-string (number->string (bytevector-u8-ref bv i)) port)
          (loop (+ i 1)))))
  (%write-string ")" port))

;; /**
;;  * A procedure's text: a continuation as one, any other by its name where it
;;  * has one.
;;  * @param {procedure} p - The procedure.
;;  * @returns {string}
;;  */
(define (procedure-text p)
  (if (%continuation? p)
      "#<continuation>"
      (let ((name (%procedure-name p)))
        (if name (string-append "#<procedure " name ">") "#<procedure>"))))

;; ---------------------------------------------------------------------------
;; Text, for the REPLs and JavaScript
;; ---------------------------------------------------------------------------

;; /**
;;  * The text a datum is written as.
;;  * @param {*} x - The datum.
;;  * @param {boolean} display? - Whether it is displayed.
;;  * @param {symbol} labelling - 'cycles, 'shared or 'none.
;;  * @returns {string}
;;  */
(define (datum->string x display? labelling)
  (let ((port (open-output-string)))
    (print-datum x port display? labelling)
    (get-output-string port)))

;; /**
;;  * The text the REPLs show for the value of an expression: as `write`
;;  * writes it, and several values one to a line.
;;  * @param {*} x - The value.
;;  * @returns {string}
;;  */
(define (repl-text x)
  (let ((port (open-output-string))
        (several (%values-list x)))
    (if several
        (let loop ((vs several) (first? #t))
          (if (pair? vs)
              (begin
                (if (not first?) (newline port))
                (print-datum (car vs) port #f 'cycles)
                (loop (cdr vs) #f))))
        (print-datum x port #f 'cycles))
    (get-output-string port)))
