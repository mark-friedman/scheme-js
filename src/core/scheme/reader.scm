;; The reader
;;
;; Text into data: R7RS's written syntax (7.1.2) -- lists and dotted lists,
;; vectors, bytevectors, strings, characters, `|symbols|`, numbers, booleans,
;; the quote forms, datum labels, block and datum comments, `#!fold-case` --
;; and this implementation's additions: dot notation, `obj.prop` read as
;; `(js-ref obj "prop")`; `#{(key value) ...}` object literals, read as a
;; `js-obj` form; and `#!dot-notation` and `#!no-dot-notation`, which turn the
;; first on and off for the rest of the text.
;;
;; Each list and vector read carries the span of text it was read from, as the
;; JavaScript object `{filename, line, column, endLine, endColumn}` under its
;; `source` property: lines and columns from one, the end column the one after
;; the datum's last character. A quote form carries the span of its quote mark.
;; The evaluator, the debugger and the source maps read them.
;;
;; The text is read a character at a time, by a recursive descent over a
;; `reader`, which holds where in the text it is, with no tokens between: a
;; datum ends where its syntax says. A number's syntax is `string->number`'s.

;; ---------------------------------------------------------------------------
;; Characters
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether a character is whitespace between tokens: a space, a tab, or a
;;  * line ending.
;;  */
(define (blank? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline) (char=? c #\return)))

;; /**
;;  * Whether a character ends an atom (R7RS 7.1.1): whitespace, a parenthesis,
;;  * a brace or a bracket, a double quote, a semicolon or a vertical line.
;;  * The start of a block comment ends one too (`at-block-comment?`).
;;  */
(define (delimiter? c)
  (or (blank? c)
      (memv c '(#\( #\) #\{ #\} #\[ #\] #\; #\" #\|))))

;; /**
;;  * Whether a character is an ASCII letter, as a character's name is made of.
;;  */
(define (ascii-letter? c)
  (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))

;; /**
;;  * Whether a character is a decimal digit.
;;  */
(define (digit? c)
  (char<=? #\0 c #\9))

;; /**
;;  * The characters named by `#\name`, the name compared without case.
;;  */
(define character-names
  (list (cons "alarm" (integer->char 7)) (cons "backspace" (integer->char 8))
        (cons "delete" (integer->char 127)) (cons "escape" (integer->char 27))
        (cons "newline" (integer->char 10)) (cons "null" (integer->char 0))
        (cons "return" (integer->char 13)) (cons "space" (integer->char 32))
        (cons "tab" (integer->char 9))))

;; ---------------------------------------------------------------------------
;; Where the reader is
;; ---------------------------------------------------------------------------

;; /**
;;  * A reader: the text, where it has got to -- as an index and as a line
;;  * and column -- the name its spans give the text, whether symbols are read
;;  * folding case and dot notation is on, and the datum labels read so far.
;;  */
(define-record-type reader
  (make-reader text length position line column filename fold-case? dot-notation? labels)
  reader?
  (text reader-text)
  (length reader-length)
  (position reader-position set-reader-position!)
  (line reader-line set-reader-line!)
  (column reader-column set-reader-column!)
  (filename reader-filename)
  (fold-case? reader-fold-case? set-reader-fold-case!)
  (dot-notation? reader-dot-notation? set-reader-dot-notation!)
  (labels reader-labels set-reader-labels!))

;; /**
;;  * The character `ahead` characters on, or #f past the end of the text.
;;  */
(define (peek r . ahead)
  (let ((index (+ (reader-position r) (if (null? ahead) 0 (car ahead)))))
    (and (< index (reader-length r)) (string-ref (reader-text r) index))))

;; /**
;;  * Whether the text goes on with a string, from where the reader is.
;;  */
(define (looking-at? r prefix)
  (let ((start (reader-position r))
        (end (+ (reader-position r) (string-length prefix))))
    (and (<= end (reader-length r))
         (string=? (substring (reader-text r) start end) prefix))))

;; /**
;;  * Steps over one character. A line ending moves to the next line, CR LF
;;  * being one; a character outside the Basic Multilingual Plane is two units
;;  * of the text, and two columns, as the text's other readers count it.
;;  */
(define (advance! r)
  (let* ((position (reader-position r))
         (c (string-ref (reader-text r) position)))
    (cond ((char=? c #\newline)
           (new-line! r)
           (set-reader-position! r (+ position 1)))
          ((char=? c #\return)
           (new-line! r)
           (set-reader-position! r (if (eqv? (peek r 1) #\newline) (+ position 2) (+ position 1))))
          ((> (char->integer c) #xFFFF)
           (set-reader-column! r (+ (reader-column r) 2))
           (set-reader-position! r (+ position 2)))
          (else
           (set-reader-column! r (+ (reader-column r) 1))
           (set-reader-position! r (+ position 1))))))

;; /**
;;  * Moves to the start of the next line.
;;  */
(define (new-line! r)
  (set-reader-line! r (+ (reader-line r) 1))
  (set-reader-column! r 1))

;; /**
;;  * Steps over so many characters.
;;  */
(define (advance-by! r count)
  (if (> count 0)
      (begin (advance! r) (advance-by! r (- count 1)))))

;; /**
;;  * The span from a place to where the reader is, as the JavaScript object
;;  * a datum's `source` is.
;;  */
(define (span-from r line column)
  (js-obj "filename" (reader-filename r)
          "line" (inexact line)
          "column" (inexact column)
          "endLine" (inexact (reader-line r))
          "endColumn" (inexact (reader-column r))))

;; /**
;;  * Gives a list or vector the span it was read from.
;;  */
(define (with-span! datum span)
  (js-set! datum "source" span)
  datum)

;; ---------------------------------------------------------------------------
;; Errors
;; ---------------------------------------------------------------------------

;; /**
;;  * Raises a read error, where the reader is.
;;  * @param {reader} r - The reader.
;;  * @param {string} message - What is wrong.
;;  * @param {string|boolean} context - What was being read, or #f.
;;  */
(define (read-error r message context)
  (%read-error message context #f (reader-line r) (reader-column r) #f))

;; /**
;;  * Raises a read error for text that ended inside a datum, which more text
;;  * could complete: a REPL asks for another line rather than report it.
;;  * @param {reader} r - The reader.
;;  * @param {string} message - What is wrong.
;;  * @param {string} context - What was being read.
;;  * @param {pair|boolean} start - Where the unfinished token began, as
;;  *   `(position line . column)`, or #f.
;;  */
(define (end-of-text r message context start)
  (if start
      (%read-error message context #t (cadr start) (cddr start) (car start))
      (%read-error message context #t #f #f #f)))

;; /**
;;  * Where the reader is, as `end-of-text` takes the start of a token.
;;  */
(define (here r)
  (cons (reader-position r) (cons (reader-line r) (reader-column r))))

;; ---------------------------------------------------------------------------
;; What is not data
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether a block comment starts where the reader is.
;;  */
(define (at-block-comment? r)
  (and (eqv? (peek r) #\#) (eqv? (peek r 1) #\|)))

;; /**
;;  * Skips whitespace, line comments and block comments, the comments nested
;;  * in them too. Inside a block comment only `#|` and `|#` mean anything
;;  * (R7RS 2.2), so a string or character there is not read as one.
;;  * @returns {boolean} Whether anything was skipped.
;;  */
(define (skip-atmosphere! r)
  (let loop ((skipped #f))
    (let ((c (peek r)))
      (cond ((not c) skipped)
            ((blank? c) (advance! r) (loop #t))
            ((char=? c #\;)
             (let line ((c (peek r)))
               (if (and c (not (char=? c #\newline)) (not (char=? c #\return)))
                   (begin (advance! r) (line (peek r)))))
             (loop #t))
            ((at-block-comment? r)
             (let ((start (here r)))
               (advance-by! r 2)
               (let nest ((depth 1))
                 (cond ((= depth 0))
                       ((not (peek r)) (end-of-text r "unterminated block comment" "block comment" start))
                       ((at-block-comment? r) (advance-by! r 2) (nest (+ depth 1)))
                       ((and (eqv? (peek r) #\|) (eqv? (peek r 1) #\#)) (advance-by! r 2) (nest (- depth 1)))
                       (else (advance! r) (nest depth)))))
             (loop #t))
            (else skipped)))))

;; /**
;;  * Skips a script header (SRFI 22), the first line of a program written to
;;  * be run from a shell: `#!/usr/bin/env ...`, or `#! ` and a path. R7RS has
;;  * no such syntax, and its directives begin with neither.
;;  */
(define (skip-script-header! r)
  (if (or (looking-at? r "#!/") (looking-at? r "#! "))
      (let line ((c (peek r)))
        (if (and c (not (char=? c #\newline)) (not (char=? c #\return)))
            (begin (advance! r) (line (peek r)))))))

;; ---------------------------------------------------------------------------
;; Data
;; ---------------------------------------------------------------------------
;;
;; `read-item` reads whatever is next: a datum, or one of the markers below for
;; what only the datum around it can make sense of -- a closing parenthesis or
;; brace, the dot of a dotted list -- or for the end of the text.

(define close-paren (list 'close-paren))
(define close-brace (list 'close-brace))
(define lone-dot (list 'dot))
(define end-of-input (list 'end))

;; /**
;;  * Whether something `read-item` returned is a marker, not a datum.
;;  */
(define (marker? item)
  (or (eq? item close-paren) (eq? item close-brace) (eq? item lone-dot) (eq? item end-of-input)))

;; /**
;;  * Reads what is next: a datum, or a marker. A directive, which sets how the
;;  * rest of the text is read, and a datum comment, which skips the datum
;;  * after it, are not data, and what follows them is read instead.
;;  * @param {reader} r - The reader.
;;  * @returns {*}
;;  */
(define (read-item r)
  (skip-atmosphere! r)
  (let ((c (peek r)))
    (cond ((not c) end-of-input)
          ((char=? c #\#) (read-hash r))
          (else (dotted r (read-plain r c))))))

;; /**
;;  * Reads a datum, which must be there: what a quote mark, a datum label or
;;  * a datum comment is followed by.
;;  * @param {reader} r - The reader.
;;  * @param {string} context - What it is for, for an error.
;;  */
(define (read-required r context)
  (let ((item (read-item r)))
    (cond ((eq? item end-of-input) (end-of-text r "unexpected end of input" context #f))
          ((eq? item close-paren) (read-error r "unexpected ')' - unbalanced parentheses" context))
          ((eq? item close-brace) (read-error r "unexpected '}' - unbalanced braces" context))
          ((eq? item lone-dot) (read-error r "unexpected '.'" context))
          (else item))))

;; /**
;;  * Reads what begins with a character other than `#`.
;;  */
(define (read-plain r c)
  (let ((line (reader-line r))
        (column (reader-column r)))
    (case c
      ((#\() (advance! r) (read-list-rest r line column))
      ((#\)) (advance! r) close-paren)
      ((#\}) (advance! r) close-brace)
      ((#\[ #\])
       (advance! r)
       (%read-error (string-append "'" (string c) "' is reserved for future extensions (R7RS 2.3); write '"
                                   (if (char=? c #\[) "(" ")") "' instead")
                    #f #f line column #f))
      ((#\' #\` #\,)
       (advance! r)
       (let ((name (cond ((char=? c #\') 'quote)
                         ((char=? c #\`) 'quasiquote)
                         ((eqv? (peek r) #\@) (advance! r) 'unquote-splicing)
                         (else 'unquote))))
         (let ((span (span-from r line column)))
           (with-span! (list name (read-required r "expression")) span))))
      ((#\") (read-string r))
      ((#\|) (read-bar-symbol r))
      ((#\{) (advance! r) (string->symbol "{"))
      (else (atom (read-atom-text r) r)))))

;; /**
;;  * Reads what begins with `#`.
;;  */
(define (read-hash r)
  (let ((line (reader-line r))
        (column (reader-column r))
        (next (peek r 1)))
    (cond ((eqv? next #\() (advance-by! r 2) (dotted r (read-vector-rest r line column)))
          ((eqv? next #\{) (advance-by! r 2) (dotted r (read-object-literal r)))
          ((eqv? next #\;)
           (advance-by! r 2)
           (read-required r "datum comment")
           (read-item r))
          ((looking-at? r "#u8(") (advance-by! r 4) (dotted r (read-bytevector-rest r)))
          ((eqv? next #\\) (dotted r (read-character r)))
          ((directive r) => (lambda (apply-directive) (apply-directive r) (read-item r)))
          ((and next (digit? next) (label-end r)) => (lambda (end) (read-label r end)))
          (else (dotted r (atom (read-atom-text r) r))))))

;; /**
;;  * The directive where the reader is, as the procedure that sets what it
;;  * says after stepping over it, or #f.
;;  */
(define (directive r)
  (let loop ((directives (list (cons "#!fold-case" (lambda (r) (set-reader-fold-case! r #t)))
                               (cons "#!no-fold-case" (lambda (r) (set-reader-fold-case! r #f)))
                               (cons "#!dot-notation" (lambda (r) (set-reader-dot-notation! r #t)))
                               (cons "#!no-dot-notation" (lambda (r) (set-reader-dot-notation! r #f))))))
    (cond ((null? directives) #f)
          ((looking-at? r (caar directives))
           (let ((name (caar directives)) (set (cdar directives)))
             (lambda (r) (advance-by! r (string-length name)) (set r))))
          (else (loop (cdr directives))))))

;; /**
;;  * Applies dot notation to a datum just read: a property name right after
;;  * it, `.prop`, with nothing between, makes it `(js-ref datum "prop")`, as
;;  * many times as there are names.
;;  * @param {reader} r - The reader.
;;  * @param {*} datum - What was read, or a marker, which is returned as it is.
;;  */
(define (dotted r datum)
  (if (and (reader-dot-notation? r)
           (not (marker? datum))
           (eqv? (peek r) #\.)
           (let ((after (peek r 1))) (and after (not (delimiter? after)))))
      (let ((parts (dot-parts (read-atom-text r))))
        (dotted r (fold-left (lambda (object property) (list 'js-ref object property))
                             datum (cdr parts))))
      datum))

;; ---------------------------------------------------------------------------
;; Lists, vectors, bytevectors and object literals
;; ---------------------------------------------------------------------------

;; /**
;;  * Reads the rest of a list, its `(` read, with the span from it.
;;  */
(define (read-list-rest r line column)
  (let loop ((items '()))
    (let ((item (read-item r)))
      (cond ((eq? item close-paren) (finish-list r (reverse items) '() line column))
            ((eq? item end-of-input) (end-of-text r "missing ')'" "list" #f))
            ((eq? item close-brace) (read-error r "unexpected '}' - unbalanced braces" "list"))
            ((eq? item lone-dot)
             (if (null? items)
                 (read-error r "illegal use of '.' - no elements before dot" "dotted list"))
             (let ((tail (read-item r)))
               (cond ((eq? tail end-of-input)
                      (end-of-text r "illegal use of '.' - no datum after dot" "dotted list" #f))
                     ((marker? tail) (read-error r "illegal use of '.' - no datum after dot" "dotted list"))
                     (else
                      (let ((close (read-item r)))
                        (cond ((eq? close close-paren) (finish-list r (reverse items) tail line column))
                              ((eq? close end-of-input)
                               (end-of-text r "expected ')' after improper list tail" "dotted list" #f))
                              (else (read-error r "expected ')' after improper list tail" "dotted list"))))))))
            (else (loop (cons item items)))))))

;; /**
;;  * A list of items and a tail, with the span from its start to here; the
;;  * empty list, which can carry none, as it is.
;;  */
(define (finish-list r items tail line column)
  (let ((made (append items tail)))
    (if (pair? made) (with-span! made (span-from r line column)) made)))

;; /**
;;  * Reads the rest of a vector, its `#(` read.
;;  */
(define (read-vector-rest r line column)
  (let loop ((items '()))
    (let ((item (read-item r)))
      (cond ((eq? item close-paren) (with-span! (list->vector (reverse items)) (span-from r line column)))
            ((eq? item end-of-input) (end-of-text r "missing ')'" "vector" #f))
            ((marker? item) (read-error r "unexpected token in vector" "vector"))
            (else (loop (cons item items)))))))

;; /**
;;  * Reads the rest of a bytevector, its `#u8(` read: exact integers from 0
;;  * to 255, `#x41` and `#e65` among them, `65.5` and `1e2` not.
;;  */
(define (read-bytevector-rest r)
  (let loop ((bytes '()))
    (let ((item (read-item r)))
      (cond ((eq? item close-paren) (apply bytevector (reverse bytes)))
            ((eq? item end-of-input) (end-of-text r "missing ')'" "bytevector" #f))
            ((and (exact-integer? item) (<= 0 item 255)) (loop (cons item bytes)))
            (else (read-error r "invalid byte value" "bytevector"))))))

;; /**
;;  * Reads the rest of an object literal, its `#{` read: entries `(key value)`
;;  * and `(... object)`, read as `(js-obj key value ...)`, a symbol key quoted,
;;  * or, with an object spread into it, `(js-obj-merge part ...)`.
;;  */
(define (read-object-literal r)
  (let loop ((entries '()))
    (skip-atmosphere! r)
    (let ((c (peek r)))
      (cond ((not c) (end-of-text r "missing '}'" "object literal" #f))
            ((char=? c #\}) (advance! r) (object-form (reverse entries)))
            ((char=? c #\()
             (advance! r)
             (let entry ((items '()))
               (let ((item (read-item r)))
                 (cond ((eq? item close-paren) (loop (cons (object-entry r (reverse items)) entries)))
                       ((eq? item end-of-input)
                        (end-of-text r "missing ')' in property entry" "object literal" #f))
                       ((marker? item) (read-error r "unexpected token in property entry" "object literal"))
                       (else (entry (cons item items)))))))
            (else (read-error r "expected '(' for property entry" "object literal"))))))

;; /**
;;  * What marks an object literal's spread entry, `(... object)`, as
;;  * `(spread-entry . object)`: an object no key can be.
;;  */
(define spread-entry (list 'spread))

;; /**
;;  * An object literal's entry: `(spread-entry . object)` for `(... object)`,
;;  * or `(key . value)`.
;;  */
(define (object-entry r items)
  (cond ((and (pair? items) (eq? (car items) '...))
         (if (= (length items) 2)
             (cons spread-entry (cadr items))
             (read-error r "spread syntax (... obj) requires exactly one object" "object literal")))
        ((= (length items) 2) (cons (car items) (cadr items)))
        (else (read-error r "property entry must be (key value) or (... obj)" "object literal"))))

;; /**
;;  * The form an object literal's entries are read as.
;;  */
(define (object-form entries)
  (define (spread? entry) (eq? (car entry) spread-entry))
  (define (pair-arguments pairs)
    (append-map (lambda (entry)
                  (list (if (symbol? (car entry)) (list 'quote (car entry)) (car entry)) (cdr entry)))
                pairs))
  (if (not (any? spread? entries))
      (cons 'js-obj (pair-arguments entries))
      ;; Each run of pairs between spreads is an object of its own.
      (let loop ((entries entries) (run '()) (parts '()))
        (define (with-run) (if (null? run) parts (cons (cons 'js-obj (pair-arguments (reverse run))) parts)))
        (cond ((null? entries) (cons 'js-obj-merge (reverse (with-run))))
              ((spread? (car entries)) (loop (cdr entries) '() (cons (cdar entries) (with-run))))
              (else (loop (cdr entries) (cons (car entries) run) parts))))))

;; ---------------------------------------------------------------------------
;; Atoms
;; ---------------------------------------------------------------------------

;; /**
;;  * Reads the text of an atom, up to the next delimiter.
;;  */
(define (read-atom-text r)
  (let ((start (reader-position r)))
    (let loop ()
      (let ((c (peek r)))
        (if (and c (not (delimiter? c)) (not (at-block-comment? r)))
            (begin (advance! r) (loop)))))
    (substring (reader-text r) start (reader-position r))))

;; /**
;;  * An atom's text as the datum it is: a number, a boolean, or a symbol --
;;  * folding case if asked -- or, with dot notation on, `a.b` as
;;  * `(js-ref a "b")`. A lone dot is the dot of a dotted list.
;;  */
(define (atom text r)
  (cond ((string->number text))
        ((member text '("#t" "#true")) #t)
        ((member text '("#f" "#false")) #f)
        ((string=? text ".") lone-dot)
        (else
         (let ((name (if (reader-fold-case? r) (string-downcase text) text)))
           (if (and (reader-dot-notation? r) (property-access? name))
               (let ((parts (dot-parts name)))
                 (fold-left (lambda (object property) (list 'js-ref object property))
                            (string->symbol (car parts)) (cdr parts)))
               (string->symbol name))))))

;; /**
;;  * Whether a name is a property access: dots inside it, between names.
;;  */
(define (property-access? name)
  (let ((parts (dot-parts name)))
    (and (pair? (cdr parts)) (not (member "" parts)))))

;; /**
;;  * A name split at its dots, every part as a literal string.
;;  */
(define (dot-parts name)
  (let loop ((chars (string->list name)) (part '()) (parts '()))
    (cond ((null? chars) (reverse (cons (%literal-string (list->string (reverse part))) parts)))
          ((char=? (car chars) #\.)
           (loop (cdr chars) '() (cons (%literal-string (list->string (reverse part))) parts)))
          (else (loop (cdr chars) (cons (car chars) part) parts)))))

;; /**
;;  * Reads a character, `#\a`, `#\newline` or `#\x41`, where the reader is at
;;  * its `#`.
;;  */
(define (read-character r)
  (if (not (peek r 2))
      (end-of-text r "unexpected end of input after #\\" "character" (here r)))
  (advance-by! r 2)
  (let ((start (reader-position r)))
    (define (take-while ok?)
      (let loop () (let ((c (peek r))) (if (and c (ok? c)) (begin (advance! r) (loop))))))
    (cond ((memv (peek r) '(#\x #\X))
           (advance! r)
           (take-while (lambda (c) (string->number (string c) 16))))
          ((ascii-letter? (peek r)) (take-while ascii-letter?))
          (else (advance! r)))
    (named-character r (substring (reader-text r) start (reader-position r)))))

;; /**
;;  * The character a `#\` names: `x` and hex digits, a name, or itself.
;;  */
(define (named-character r name)
  (cond ((and (> (string-length name) 1) (char=? (string-ref name 0) #\x))
         (let ((code (string->number (substring name 1 (string-length name)) 16)))
           (if code
               (integer->char code)
               (read-error r (string-append "invalid character hex escape: #\\" name) "character"))))
        ((assoc (string-downcase name) character-names) => cdr)
        ((= (string-length name) 1) (string-ref name 0))
        ((and (= (string-length name) 2) (> (char->integer (string-ref name 0)) #xFFFF)) (string-ref name 0))
        (else (read-error r (string-append "unknown character name: #\\" name) "character"))))

;; ---------------------------------------------------------------------------
;; Strings and |symbols|
;; ---------------------------------------------------------------------------

;; /**
;;  * Reads characters up to a closing one, where the reader is at the opening
;;  * one, giving the text between with each escape as `escape` reads it.
;;  * @param {reader} r - The reader.
;;  * @param {char} close - The closing character.
;;  * @param {procedure} escape - Given the reader after a backslash, and the
;;  *   output, writes what the escape stands for.
;;  * @param {string} message - The error if the text ends first.
;;  * @param {string} context - What is being read.
;;  * @returns {string}
;;  */
(define (read-delimited r close escape message context)
  (let ((start (here r))
        (out (open-output-string)))
    (advance! r)
    (let loop ()
      (let ((c (peek r)))
        (cond ((not c) (end-of-text r message context start))
              ((char=? c close) (advance! r) (get-output-string out))
              ((char=? c #\\)
               (advance! r)
               (if (not (peek r)) (end-of-text r message context start))
               (escape r out)
               (loop))
              (else (write-char c out) (advance! r) (loop)))))))

;; /**
;;  * Reads a string, which is a literal and so cannot be changed.
;;  */
(define (read-string r)
  (%literal-string (read-delimited r #\" string-escape "unterminated string" "string")))

;; /**
;;  * Reads a `|symbol|`, its name as written, with its escapes and without
;;  * folding case.
;;  */
(define (read-bar-symbol r)
  (string->symbol (read-delimited r #\| symbol-escape "unterminated |symbol|" "symbol")))

;; /**
;;  * Writes what a string's escape stands for, the reader after its
;;  * backslash: `\a`, `\b`, `\t`, `\n`, `\r`, `\"`, `\\`, `\|`, `\x41;`, and a
;;  * line ending with the whitespace around it, which stands for nothing.
;;  * Any other character stands for itself.
;;  */
(define (string-escape r out)
  (let ((c (peek r)))
    (cond ((assv c '((#\a . 7) (#\b . 8) (#\t . 9) (#\n . 10) (#\r . 13)))
           => (lambda (entry) (advance! r) (write-char (integer->char (cdr entry)) out)))
          ((char=? c #\x) (hex-escape r out))
          ((blank? c) (skip-line-continuation! r))
          (else (advance! r) (write-char c out)))))

;; /**
;;  * Writes what a `|symbol|`'s escape stands for: `\x41;`, or the character
;;  * after the backslash.
;;  */
(define (symbol-escape r out)
  (if (char=? (peek r) #\x)
      (hex-escape r out)
      (begin (write-char (peek r) out) (advance! r))))

;; /**
;;  * Writes the character a `\x41;` escape stands for, the reader at its `x`.
;;  * One that is not hex digits and a semicolon stands for a backslash, the
;;  * `x` then read as itself.
;;  */
(define (hex-escape r out)
  (let loop ((ahead 1) (digits '()))
    (let ((c (peek r ahead)))
      (cond ((and (eqv? c #\;) (pair? digits)
                  (string->number (list->string (reverse digits)) 16))
             => (lambda (code) (advance-by! r (+ ahead 1)) (write-char (integer->char code) out)))
            ((and c (not (char=? c #\;)) (string->number (string c) 16)) (loop (+ ahead 1) (cons c digits)))
            (else (write-char #\\ out))))))

;; /**
;;  * Skips a line continuation in a string, the reader after its backslash:
;;  * the whitespace before a line ending, the line ending, and the whitespace
;;  * after it.
;;  */
(define (skip-line-continuation! r)
  (define (skip-intraline!)
    (let loop () (if (memv (peek r) '(#\space #\tab)) (begin (advance! r) (loop)))))
  (skip-intraline!)
  (if (memv (peek r) '(#\return #\newline)) (advance! r))
  (skip-intraline!))

;; ---------------------------------------------------------------------------
;; Datum labels
;; ---------------------------------------------------------------------------
;;
;; `#0=datum` labels a datum, and `#0#` stands for it, even inside it, which
;; makes a circular datum. A reference read before its datum is finished is a
;; placeholder until then; the data read are fixed up afterwards.

(define-record-type placeholder
  (make-placeholder id value resolved?)
  placeholder?
  (id placeholder-id)
  (value placeholder-value set-placeholder-value!)
  (resolved? placeholder-resolved? set-placeholder-resolved!))

;; /**
;;  * Where a datum label's `#n` ends, if `=` or `#` follows its digits, as
;;  * the number of characters to its last.
;;  */
(define (label-end r)
  (let loop ((ahead 1))
    (let ((c (peek r ahead)))
      (cond ((and c (digit? c)) (loop (+ ahead 1)))
            ((memv c '(#\= #\#)) ahead)
            (else #f)))))

;; /**
;;  * Reads a datum label's definition, `#n=datum`, or reference, `#n#`.
;;  */
(define (read-label r end)
  (let ((id (string->number (substring (reader-text r) (+ (reader-position r) 1) (+ (reader-position r) end))))
        (kind (peek r end)))
    (advance-by! r (+ end 1))
    (if (char=? kind #\=)
        (let ((placeholder (make-placeholder id #f #f)))
          (set-reader-labels! r (cons (cons id placeholder) (reader-labels r)))
          (let ((datum (read-required r "datum label")))
            (set-placeholder-value! placeholder datum)
            (set-placeholder-resolved! placeholder #t)
            (dotted r datum)))
        (let ((found (assv id (reader-labels r))))
          (if (not found)
              (read-error r (string-append "reference to undefined label #" (number->string id) "#") "datum label"))
          (%note-label-reference!)
          (dotted r (cdr found))))))

;; /**
;;  * A datum read with its placeholders replaced by the data they stand for,
;;  * in place, each pair and vector visited once, so that a circular one is
;;  * walked.
;;  */
(define (fix-up datum)
  (let ((visited (%make-hash-store 'eq)))
    (define (resolved placeholder)
      (if (placeholder-resolved? placeholder)
          (placeholder-value placeholder)
          (%read-error (string-append "reference to undefined label #"
                                      (number->string (placeholder-id placeholder)) "#")
                       "datum label" #f #f #f #f)))
    (define (walk! x)
      (cond ((%hash-store-contains? visited x))
            ((pair? x)
             (%hash-store-set! visited x #t)
             (if (placeholder? (car x)) (set-car! x (resolved (car x))) (walk! (car x)))
             (if (placeholder? (cdr x)) (set-cdr! x (resolved (cdr x))) (walk! (cdr x))))
            ((vector? x)
             (%hash-store-set! visited x #t)
             (let loop ((i 0))
               (if (< i (vector-length x))
                   (let ((item (vector-ref x i)))
                     (if (placeholder? item) (vector-set! x i (resolved item)) (walk! item))
                     (loop (+ i 1))))))))
    (if (placeholder? datum) (resolved datum) (begin (walk! datum) datum))))

;; ---------------------------------------------------------------------------
;; Reading a text
;; ---------------------------------------------------------------------------

;; /**
;;  * Reads every datum of a text.
;;  * @param {string} text - The text.
;;  * @param {string} filename - The name its spans give it.
;;  * @param {boolean} fold-case? - Whether symbols are read folding case, as
;;  *   `#!fold-case` would have them, at first.
;;  * @param {boolean} dot-notation? - Whether dot notation is on at first.
;;  * @returns {list} The data.
;;  */
(define (read-source text filename fold-case? dot-notation?)
  (let ((r (make-reader text (string-length text) 0 1 1 filename fold-case? dot-notation? '())))
    (skip-script-header! r)
    (let loop ((data '()))
      (let ((item (read-item r)))
        (cond ((eq? item end-of-input) (reverse data))
              ((eq? item close-paren) (read-error r "unexpected ')' - unbalanced parentheses" "list"))
              ((eq? item close-brace) (read-error r "unexpected '}' - unbalanced braces" "object literal"))
              ((eq? item lone-dot) (read-error r "unexpected '.'" "symbol"))
              ((null? (reader-labels r)) (loop (cons item data)))
              (else (loop (cons (fix-up item) data))))))))

;; ---------------------------------------------------------------------------
;; Small list helpers
;; ---------------------------------------------------------------------------
;;
;; This library loads beside the library system, with (scheme core) and
;; (scheme control) alone, so it has no SRFI 1: these are the few list
;; procedures it needs.

;; /**
;;  * R6RS's `fold-left`: a procedure applied to an accumulator and each
;;  * element, left to right. SRFI 1's `fold` takes the two the other way
;;  * round.
;;  */
(define (fold-left combine initial items)
  (if (null? items) initial (fold-left combine (combine initial (car items)) (cdr items))))

;; /**
;;  * SRFI 1's `append-map`: the lists a procedure makes of each element,
;;  * appended.
;;  */
(define (append-map make items)
  (if (null? items) '() (append (make (car items)) (append-map make (cdr items)))))

;; /**
;;  * SRFI 1's `any`, as a predicate: whether a predicate holds of an element.
;;  */
(define (any? ok? items)
  (and (pair? items) (or (and (ok? (car items)) #t) (any? ok? (cdr items)))))
