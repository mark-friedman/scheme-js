;; devtools.scm -- the custom formatter DevTools draws Scheme values with.
;;
;; DevTools asks a formatter, for each object it draws, for a header -- the
;; one line it shows -- whether the object has a body, and the body, shown
;; when the line is expanded. The answers here are markup, which the
;; formatter's JavaScript turns into DevTools' JsonML (devtools.js): a string;
;; `(tag style child ...)`, `tag` one of `span`, `div`, `ol` and `li`, `style`
;; a CSS declaration or #f; or `(object value javascript?)`, a value DevTools
;; is to draw itself, with these formatters unless `javascript?`. Each is asked
;; with the value and where the program is paused: the URL of the paused
;; frame's script, or "" with nothing paused.
;;
;; Formatters are asked only of objects, so a value JavaScript has a primitive
;; for -- a boolean, the empty list, which is `null`, an exact integer, a
;; string JavaScript holds -- is drawn as JavaScript draws it, whatever these
;; say.

;; ---------------------------------------------------------------------------
;; Which values are Scheme's to draw
;; ---------------------------------------------------------------------------

;; /**
;;  * How Scheme's values are drawn: `auto`, a value only Scheme has as Scheme
;;  * and one JavaScript has too -- a vector is an array, a bytevector a
;;  * `Uint8Array` -- as Scheme only while paused in Scheme; `scheme`, every one
;;  * as Scheme; `javascript`, none.
;;  */
(define display-mode 'auto)

;; /**
;;  * How Scheme's values are drawn (`display-mode`).
;;  * @returns {symbol}
;;  */
(define (devtools-display) display-mode)

;; /**
;;  * Switches how Scheme's values are drawn: called in the console, as
;;  * `schemeJS.values('scheme')`, and the like.
;;  * @param {symbol} mode - `auto`, `scheme` or `javascript`.
;;  * @returns {symbol} The mode.
;;  */
(define (set-devtools-display! mode)
  (unless (memq mode '(auto scheme javascript))
    (error "set-devtools-display!: expected auto, scheme or javascript" mode))
  (set! display-mode mode)
  mode)

;; /**
;;  * Whether the program is paused in Scheme: in code the tier compiled, whose
;;  * scripts it names `scheme:///<file>/<procedure>`.
;;  * @param {string} paused-in - The paused frame's script's URL, or "".
;;  * @returns {boolean}
;;  */
(define (paused-in-scheme? paused-in)
  (and (>= (string-length paused-in) 7) (string=? (substring paused-in 0 7) "scheme:")))

;; /**
;;  * Whether a value only Scheme has.
;;  * @param {*} value - An object DevTools is drawing.
;;  * @returns {boolean}
;;  */
(define (schemes-own? value)
  (or (pair? value) (symbol? value) (char? value) (string? value) (number? value)
      (eof-object? value) (%scheme-procedure? value) (and (%record-description value) #t)))

;; /**
;;  * Whether a value Scheme and JavaScript both have, as an array.
;;  * @param {*} value - An object DevTools is drawing.
;;  * @returns {boolean}
;;  */
(define (shared? value) (or (vector? value) (bytevector? value)))

;; /**
;;  * Whether a value is drawn as Scheme, where the program is paused.
;;  * @param {*} value - An object DevTools is drawing.
;;  * @param {string} paused-in - Where the program is paused, or "".
;;  * @returns {boolean}
;;  */
(define (drawn-as-scheme? value paused-in)
  (case display-mode
    ((javascript) #f)
    ((scheme) (or (schemes-own? value) (shared? value)))
    (else (or (schemes-own? value) (and (shared? value) (paused-in-scheme? paused-in))))))

;; ---------------------------------------------------------------------------
;; The header
;; ---------------------------------------------------------------------------

;; /**
;;  * How much of a value a header shows: so many elements of a list or
;;  * vector, lists nested so deep, so many characters of a string and of the
;;  * whole. A header is drawn for every value DevTools lists, so it is kept
;;  * short, and stops however large or circular the value.
;;  */
(define header-elements 10)
(define header-depth 3)
(define header-string 60)
(define header-length 100)

;; /**
;;  * A value's header: the value as `write` writes it, cut short; or #f for a
;;  * value left to DevTools.
;;  * @param {*} value - An object DevTools is drawing.
;;  * @param {string} paused-in - Where the program is paused, or "".
;;  * @returns {list|boolean} Markup.
;;  */
(define (devtools-header value paused-in)
  (and (drawn-as-scheme? value paused-in)
       (list 'span #f (cut-short (header-text value) header-length))))

;; /**
;;  * Text cut to a length, ending in "..." if it was cut.
;;  * @param {string} text - The text.
;;  * @param {integer} length - The longest it may be.
;;  * @returns {string}
;;  */
(define (cut-short text length)
  (if (<= (string-length text) length)
      text
      (string-append (substring text 0 (- length 3)) "...")))

;; /**
;;  * A value written as a header shows it.
;;  * @param {*} value - The value.
;;  * @returns {string}
;;  */
(define (header-text value)
  (let ((out (open-output-string)))
    (write-shortened value 0 out)
    (get-output-string out)))

;; /**
;;  * Writes a value as `write` does, but for the parts of it a header leaves
;;  * out: the elements of a list or vector past `header-elements`, lists
;;  * nested past `header-depth`, the characters of a string past
;;  * `header-string`. A record is written with its type and fields.
;;  * @param {*} value - The value.
;;  * @param {integer} depth - How deep in the header it is.
;;  * @param {port} out - Where to write.
;;  */
(define (write-shortened value depth out)
  (cond ((pair? value)
         (if (>= depth header-depth)
             (write-string "(...)" out)
             (begin (write-char #\( out)
                    (write-elements value depth out)
                    (write-char #\) out))))
        ((vector? value)
         (write-string "#(" out)
         (write-elements (vector->list value 0 (min (vector-length value) (+ header-elements 1))) depth out)
         (write-char #\) out))
        ((bytevector? value)
         (write-string "#u8(" out)
         (write-elements (bytevector-prefix value (+ header-elements 1)) depth out)
         (write-char #\) out))
        ((string? value)
         (write (if (> (string-length value) header-string)
                    (string-append (substring value 0 header-string) "...")
                    value)
                out))
        ((%record-description value)
         => (lambda (record)
              (write-string "#<" out)
              (write-string (car record) out)
              (for-each (lambda (field)
                          (write-char #\space out)
                          (write-string (symbol->string (car field)) out)
                          (write-string ": " out)
                          (write-shortened (cdr field) (+ depth 1) out))
                        (cdr record))
              (write-char #\> out)))
        (else (write value out))))

;; /**
;;  * Writes the elements of a list, `header-elements` of them at most, and
;;  * an improper list's tail after a dot.
;;  * @param {list} elements - The list.
;;  * @param {integer} depth - How deep the list is.
;;  * @param {port} out - Where to write.
;;  */
(define (write-elements elements depth out)
  (let loop ((rest elements) (written 0))
    (cond ((null? rest))
          ((= written header-elements) (write-string " ..." out))
          ((pair? rest)
           (if (> written 0) (write-char #\space out))
           (write-shortened (car rest) (+ depth 1) out)
           (loop (cdr rest) (+ written 1)))
          (else
           (write-string " . " out)
           (write-shortened rest (+ depth 1) out)))))

;; /**
;;  * The first bytes of a bytevector, as a list.
;;  * @param {bytevector} bytes - The bytevector.
;;  * @param {integer} count - How many at most.
;;  * @returns {list}
;;  */
(define (bytevector-prefix bytes count)
  (let loop ((i (- (min count (bytevector-length bytes)) 1)) (prefix '()))
    (if (< i 0) prefix (loop (- i 1) (cons (bytevector-u8-ref bytes i) prefix)))))

;; ---------------------------------------------------------------------------
;; The body
;; ---------------------------------------------------------------------------

;; /**
;;  * How many of a value's parts a body lists.
;;  */
(define body-rows 100)

;; /**
;;  * Whether a value has a body: every one drawn as Scheme has, if only to
;;  * offer it as JavaScript draws it.
;;  * @param {*} value - An object DevTools is drawing.
;;  * @param {string} paused-in - Where the program is paused, or "".
;;  * @returns {boolean}
;;  */
(define (devtools-has-body? value paused-in)
  (drawn-as-scheme? value paused-in))

;; /**
;;  * A value's body: its parts, each a row DevTools draws -- a list's elements
;;  * and an improper list's tail, a vector's elements, a record's fields --
;;  * and, last, the value as JavaScript draws it.
;;  * @param {*} value - An object DevTools is drawing.
;;  * @param {string} paused-in - Where the program is paused, or "".
;;  * @returns {list|boolean} Markup.
;;  */
(define (devtools-body value paused-in)
  (and (drawn-as-scheme? value paused-in)
       (cons* 'ol "list-style-type: none; padding-left: 1em; margin: 0"
              (append (part-rows value)
                      (list (list 'li #f "JavaScript: " (list 'object value #t)))))))

;; /**
;;  * The rows of a value's parts.
;;  * @param {*} value - The value.
;;  * @returns {list} Markup.
;;  */
(define (part-rows value)
  (define (row label part) (list 'li #f label (list 'object part #f)))
  (define (indexed parts)
    (let loop ((rest parts) (i 0) (rows '()))
      (cond ((null? rest) (reverse rows))
            ((= i body-rows) (reverse (cons (list 'li #f "...") rows)))
            ((pair? rest) (loop (cdr rest) (+ i 1) (cons (row (string-append (number->string i) ": ") (car rest)) rows)))
            (else (reverse (cons (row ". " rest) rows))))))
  (cond ((pair? value) (indexed value))
        ((vector? value) (indexed (vector->list value 0 (min (vector-length value) (+ body-rows 1)))))
        ((bytevector? value) (indexed (bytevector-prefix value (+ body-rows 1))))
        ((%record-description value)
         => (lambda (record)
              (map (lambda (field) (row (string-append (symbol->string (car field)) ": ") (cdr field)))
                   (cdr record))))
        (else '())))

;; /**
;;  * A list of its arguments, the last of them its tail.
;;  * @param {...*} items - The elements, then the tail.
;;  * @returns {list}
;;  */
(define (cons* first . rest)
  (if (null? rest) first (cons first (apply cons* rest))))

;; ---------------------------------------------------------------------------
;; Registering
;; ---------------------------------------------------------------------------

;; /**
;;  * Registers the formatter with DevTools, in place of any registered before.
;;  * @returns {boolean} True.
;;  */
(define (install-devtools-formatters!)
  (%install-devtools-formatter! devtools-header devtools-has-body? devtools-body))
