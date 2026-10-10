;;; sourcemap.scm -- where generated code came from, for a debugger.
;;;
;;; A source map tells a debugger, for each position in a script, the position
;;; in a source it was generated from (Source Map Revision 3). With one, a
;;; debugger shows a compiled procedure's frames at their places in its Scheme
;;; source, and takes a breakpoint set in the source at the code generated
;;; from it.
;;;
;;; The emitter renders a procedure as lines, each carrying the source span of
;;; the statement it came from, or #f (`emit.scm`); a line maps, from its
;;; start, to the start of its span. So a frame, which a debugger places at the
;;; call it is in, shows at the start of the Scheme expression whose code holds
;;; that call -- a statement's, not the call's own column within it -- and a
;;; breakpoint set there stops at the line's first statement. A line with no
;;; span maps nothing.
;;;
;;; The map of code the tier compiles is written into the script itself, as a
;;; `data:` URL: a script made with `new Function` has no file a map could sit
;;; beside. Its JSON goes into the URL as it is, not in base 64: a URL's parser
;;; percent-encodes what it must and a `data:` URL's body is percent-decoded,
;;; so only what would end the URL or change its meaning is escaped
;;; (`url-path-escape`), which only the JSON's strings can hold. A prebuilt
;;; table's map, and a built program's, is a file of its own.

;; ---------------------------------------------------------------------------
;; The encoding
;; ---------------------------------------------------------------------------

(define base64-digits
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;; /**
;;  * An integer as a base-64 variable-length quantity, as a source map's
;;  * fields are written: its sign in the lowest bit, then five bits to a digit,
;;  * least significant first, each digit but the last with its continuation
;;  * bit, 32, set.
;;  * @param {integer} n - The integer.
;;  * @returns {string}
;;  */
(define (encode-vlq n)
  (unsigned-vlq (if (negative? n) (+ (* -2 n) 1) (* 2 n))))

;; /**
;;  * A non-negative integer as a base-64 variable-length quantity with no
;;  * sign bit, as most fields of a map's scopes are written: five bits to a
;;  * digit, least significant first, each digit but the last with its
;;  * continuation bit, 32, set.
;;  * @param {integer} n - The integer.
;;  * @returns {string}
;;  */
(define (unsigned-vlq n)
  (let loop ((v n) (digits '()))
    (let ((low (remainder v 32)) (high (quotient v 32)))
      (if (zero? high)
          (list->string (reverse (cons (string-ref base64-digits low) digits)))
          (loop high (cons (string-ref base64-digits (+ low 32)) digits))))))

;; /**
;;  * The quantities of the integers from -1024 to 1023, each made when first
;;  * asked for: nearly every field of a map is a small difference, and making
;;  * them each time was a third of what writing a map cost.
;;  */
(define vlq-table (make-vector 2048 #f))

;; /**
;;  * `encode-vlq`, remembered for the integers `vlq-table` holds.
;;  * @param {integer} n - The integer.
;;  * @returns {string}
;;  */
(define (vlq n)
  (if (and (>= n -1024) (< n 1024))
      (let ((i (+ n 1024)))
        (or (vector-ref vlq-table i)
            (let ((text (encode-vlq n)))
              (vector-set! vlq-table i text)
              text)))
      (encode-vlq n)))

;; /**
;;  * Whether a character would end a URL's path or begin an escape in it, or
;;  * is a space.
;;  * @param {char} c - The character.
;;  * @returns {boolean}
;;  */
(define (url-path-special? c)
  (case c ((#\% #\? #\# #\space) #t) (else #f)))

;; /**
;;  * Text escaped for a URL: only the characters `url-path-special?` names, so
;;  * that a Scheme name stays readable where it can. Most text has none, and is
;;  * returned as it is.
;;  * @param {string} text - The text.
;;  * @returns {string}
;;  */
(define (url-path-escape text)
  (if (string-index text url-path-special?)
      (string-concatenate
        (map (lambda (c)
               (case c
                 ((#\%) "%25") ((#\?) "%3F") ((#\#) "%23") ((#\space) "%20")
                 (else (string c))))
             (string->list text)))
      text))

;; ---------------------------------------------------------------------------
;; A map of rendered lines
;; ---------------------------------------------------------------------------

;; /**
;;  * The file a span is from, or #f: the reader records `<unknown>` for
;;  * source it was given no file for.
;;  * @param {object} span - A source span.
;;  * @returns {string|boolean}
;;  */
(define (span-file span)
  (let ((file (js-ref span "filename")))
    (and (string? file) (not (string=? file "<unknown>")) file)))

;; /**
;;  * The source map of a script, as JSON, or #f if no line maps anything.
;;  *
;;  * Its `mappings` hold, for each line, a segment if the line has a span:
;;  * the column it maps from, always 0; the index of the span's file in the
;;  * map's `sources`; and the span's line and column, counted from zero. Lines
;;  * are separated by `;`, and each field is written as its difference from
;;  * the same field of the segment before -- the generated column only within
;;  * its line, so always 0.
;;  *
;;  * A file a debugger can fetch by its name it fetches. One it cannot, whose
;;  * text the caller knows -- a page's inline script -- has the text in the
;;  * map's `sourcesContent`, and any other file a null beside it.
;;  *
;;  * A map written to a file of its own -- a prebuilt table's, a built
;;  * program's -- names each file as a path from the map (`name-of`), and may
;;  * ignore-list some (`x_google_ignoreList`): the system's own.
;;  *
;;  * A map given a unit's scopes (`scopes.scm`) has them, and the names they
;;  * use, if the file of the source's scopes is one of its sources.
;;  *
;;  * @param {list} spans - Each line's span, or #f, as the emitter renders
;;  *   them (`render-items` in `emit.scm`).
;;  * @param {integer} offset - How many lines the script has before the first.
;;  * @param {procedure} text-of - A file's text, where the map should hold it,
;;  *   or #f.
;;  * @param {procedure} [name-of] - The name a file goes into `sources` by;
;;  *   its own, by default.
;;  * @param {procedure} [ignored?] - Whether a file is ignore-listed; none,
;;  *   by default.
;;  * @param {unit-scopes|boolean} [scopes] - The unit's scopes, or #f, the
;;  *   default.
;;  * @returns {string|boolean}
;;  */
(define (source-map spans offset text-of . options)
  ;; `sources` is newest first, so a file's index in the order first named is
  ;; the length of what follows it. The mappings are appended to as they go,
  ;; as `render-items` in `emit.scm` builds its text.
  (let ((name-of (optional options 0 (lambda (file) file)))
        (ignored? (optional options 1 (lambda (file) #f)))
        (scopes (optional options 2 #f)))
   (let loop ((spans spans) (first? #t) (mappings (make-string offset #\;))
              (sources '()) (source 0) (line 0) (column 0) (previous #f))
    (if (null? spans)
        (and (pair? sources)
             (let* ((files (reverse sources))
                    (field (and scopes (member (unit-scopes-file scopes) files)
                                (scopes-field (map (lambda (file)
                                                     (and (equal? file (unit-scopes-file scopes))
                                                          (unit-scopes-root scopes)))
                                                   files)
                                              (unit-scopes-ranges scopes) offset)))
                    (texts (map text-of files))
                    (ignore-list (filter-map (lambda (file index) (and (ignored? file) index))
                                             files (iota (length files))))
                    (json-list (lambda (strings)
                                 (string-join (map (lambda (s) (if s (js-string s) "null"))
                                                   strings)
                                              ","))))
               ;; The mappings need no quoting: base-64 digits and `;`.
               (string-append "{\"version\":3,\"sources\":[" (json-list (map name-of files)) "]"
                              (if (any (lambda (text) text) texts)
                                  (string-append ",\"sourcesContent\":[" (json-list texts) "]")
                                  "")
                              (if (pair? ignore-list)
                                  (string-append ",\"x_google_ignoreList\":["
                                                 (string-join (map number->string ignore-list) ",") "]")
                                  "")
                              ",\"names\":[" (if field (json-list (cdr field)) "") "]"
                              ",\"mappings\":\"" mappings "\""
                              ;; A scopes field is base-64 digits and commas.
                              (if field (string-append ",\"scopes\":\"" (car field) "\"") "")
                              "}")))
        (let* ((span (car spans))
               (mappings (if first? mappings (string-append mappings ";"))))
          (cond
            ;; The span of the line before -- the lines of a statement, or
            ;; the statements of one call -- maps where it did: every field
            ;; the same.
            ((and span (eq? span previous))
             (loop (cdr spans) #f (string-append mappings "AAAA") sources source line column span))
            ((not (and span (span-file span)))
             (loop (cdr spans) #f mappings sources source line column previous))
            (else
             (let* ((file (span-file span))
                    (sources (if (member file sources) sources (cons file sources)))
                    (index (- (length (member file sources)) 1))
                    (span-line (- (js-ref span "line") 1))
                    (span-column (- (js-ref span "column") 1)))
               (loop (cdr spans) #f
                     (string-append mappings "A" (vlq (- index source))
                                    (vlq (- span-line line)) (vlq (- span-column column)))
                     sources index span-line span-column span)))))))))

;; /**
;;  * A source map as a URL a script can name its map by, in a
;;  * `//# sourceMappingURL=` comment, escaped for it (`url-path-escape`).
;;  * @param {string} json - The map, from `source-map`.
;;  * @returns {string}
;;  */
(define (source-map-url json)
  (string-append "data:application/json;charset=utf-8," (url-path-escape json)))

;; /**
;;  * An optional argument: the one at a position among those given, or a
;;  * default.
;;  * @param {list} options - The optional arguments given.
;;  * @param {integer} n - The position.
;;  * @param {*} default - The default.
;;  * @returns {*}
;;  */
(define (optional options n default)
  (if (> (length options) n) (list-ref options n) default))

;; ---------------------------------------------------------------------------
;; Scopes
;; ---------------------------------------------------------------------------
;;
;; A map's `scopes`, as ECMA-426's scopes proposal writes them: one string of
;; items separated by commas, each a tag and its fields. First the source's
;; scopes, a tree a source, in the order of `sources` -- `B` begins a scope,
;; with its flags, position, name and kind; `D` lists its variables; `C` ends
;; it -- or `A` for a source with none. Then the ranges of the generated code
;; -- `E` begins one, with its flags, position and the scope it is, by its
;; number among the scopes in the order written; `G` gives what reads each of
;; the scope's variables there, a name's index plus one, or 0 for nothing;
;; `F` ends it. A name, kind or variable is an index into the map's `names`.
;; Nearly every field is written as its difference from the same field of the
;; items before (`scope-cursor`): a scope's position from the last position
;; written in its source's tree, a range's from the last range's.

;; /**
;;  * A map's `scopes` field, and the names it uses.
;;  * @param {list} roots - For each of the map's sources, in order, the file's
;;  *   scope, or #f.
;;  * @param {list} ranges - The outermost ranges.
;;  * @param {integer} offset - How many lines the script has before the
;;  *   generated code the ranges are of.
;;  * @returns {pair} (field . names), the names in index order.
;;  */
(define (scopes-field roots ranges offset)
  (let* ((items (append (append-map (lambda (root) (if root (scope-items root #t) '((none)))) roots)
                        (append-map range-items ranges)))
         (names (indexed-names items))
         (numbers (scope-numbers (append-map preorder (filter (lambda (root) root) roots))))
         (texts (let write ((items items) (at initial-scope-cursor))
                  (if (null? items)
                      '()
                      (let ((written (scope-item-text (car items) at (cdr names) numbers offset)))
                        (cons (car written) (write (cdr items) (cdr written))))))))
    (cons (string-join texts ",") (car names))))

;; /**
;;  * The names items use, in the order the map's `names` takes them, and each
;;  * one's index there. A large unit's items name hundreds, and finding each
;;  * in a list made writing its scopes cost twenty times its mappings.
;;  * @param {list} items - The items.
;;  * @returns {pair} (names . indices): the names in index order, and a weak
;;  *   table of each one's index by the name as a symbol, since a string
;;  *   cannot key one.
;;  */
(define (indexed-names items)
  (let ((indices (make-weak-table)))
    (let collect ((names (append-map item-names items)) (kept '()) (count 0))
      (cond ((null? names) (cons (reverse kept) indices))
            ((weak-table-ref indices (string->symbol (car names))) (collect (cdr names) kept count))
            (else (weak-table-set! indices (string->symbol (car names)) count)
                  (collect (cdr names) (cons (car names) kept) (+ count 1)))))))

;; /**
;;  * Each scope's number, which a range names its scope by: its position
;;  * among every scope, each before those inside it.
;;  * @param {list} scopes - The scopes, in that order.
;;  * @returns {weak-table} Each one's number, by scope.
;;  */
(define (scope-numbers scopes)
  (let ((numbers (make-weak-table)))
    (for-each (lambda (scope n) (weak-table-set! numbers scope n)) scopes (iota (length scopes)))
    numbers))

;; /**
;;  * The items of a scope and those inside it, in the order they are written.
;;  * @param {original-scope} scope - The scope.
;;  * @param {boolean} file? - Whether it is a file's, whose position counts
;;  *   from the file's start.
;;  * @returns {list}
;;  */
(define (scope-items scope file?)
  (append (list (list 'start scope file?))
          (if (null? (original-scope-variables scope))
              '()
              (list (list 'variables (original-scope-variables scope))))
          (append-map (lambda (child) (scope-items child #f)) (original-scope-children scope))
          (list (list 'end scope))))

;; /**
;;  * The items of a range and those inside it, in the order they are written.
;;  * @param {generated-range} range - The range.
;;  * @returns {list}
;;  */
(define (range-items range)
  (append (list (list 'range-start range))
          (if (null? (generated-range-bindings range))
              '()
              (list (list 'bindings (generated-range-bindings range))))
          (append-map range-items (generated-range-children range))
          (list (list 'range-end range))))

;; /**
;;  * The names an item uses, in the order it writes them, which is the order
;;  * the map's `names` takes them in.
;;  * @param {list} item - The item.
;;  * @returns {list} The names.
;;  */
(define (item-names item)
  (case (car item)
    ((start) (let ((scope (cadr item)))
               (filter string? (list (original-scope-name scope) (original-scope-kind scope)))))
    ((variables) (cadr item))
    ((bindings) (filter string? (cadr item)))
    (else '())))

;; /**
;;  * A scope and every scope inside it, each before those inside it: the
;;  * order a range's scope is numbered in.
;;  * @param {original-scope} scope - The scope.
;;  * @returns {list}
;;  */
(define (preorder scope)
  (cons scope (append-map preorder (original-scope-children scope))))

;; /**
;;  * What the items before wrote, which each field is written as its
;;  * difference from: the last scope position, (line . column), the last
;;  * name, kind and variable, the last range position, and the last scope
;;  * number.
;;  */
(define-record-type scope-cursor
  (make-scope-cursor position name kind variable range-position definition)
  scope-cursor?
  (position cursor-position)
  (name cursor-name)
  (kind cursor-kind)
  (variable cursor-variable)
  (range-position cursor-range-position)
  (definition cursor-definition))

(define initial-scope-cursor (make-scope-cursor '(0 . 0) 0 0 0 '(0 . 0) 0))

;; /**
;;  * A cursor after a scope's start or end: at its position, and, at its
;;  * start, its name and kind.
;;  * @param {scope-cursor} at - The cursor before.
;;  * @param {pair} position - (line . column).
;;  * @param {integer} name - The last name's index.
;;  * @param {integer} kind - The last kind's index.
;;  * @returns {scope-cursor}
;;  */
(define (cursor-at-scope at position name kind)
  (make-scope-cursor position name kind (cursor-variable at) (cursor-range-position at) (cursor-definition at)))

;; /**
;;  * A cursor after a variable.
;;  * @param {scope-cursor} at - The cursor before.
;;  * @param {integer} variable - The variable's index.
;;  * @returns {scope-cursor}
;;  */
(define (cursor-at-variable at variable)
  (make-scope-cursor (cursor-position at) (cursor-name at) (cursor-kind at) variable
                     (cursor-range-position at) (cursor-definition at)))

;; /**
;;  * A cursor after a range's start or end.
;;  * @param {scope-cursor} at - The cursor before.
;;  * @param {pair} position - (line . column).
;;  * @param {integer} definition - The last scope number.
;;  * @returns {scope-cursor}
;;  */
(define (cursor-at-range at position definition)
  (make-scope-cursor (cursor-position at) (cursor-name at) (cursor-kind at) (cursor-variable at)
                     position definition))

;; /**
;;  * An item's text, and the cursor after it.
;;  * @param {list} item - The item.
;;  * @param {list} at - The cursor before it.
;;  * @param {weak-table} indices - Each name's index in the map's names
;;  *   (`indexed-names`).
;;  * @param {weak-table} numbers - Each scope's number (`scope-numbers`).
;;  * @param {integer} offset - As for `scopes-field`.
;;  * @returns {pair} (text . cursor).
;;  */
(define (scope-item-text item at indices numbers offset)
  (let ((index-of (lambda (name) (weak-table-ref indices (string->symbol name)))))
    (case (car item)
      ((none) (cons "A" at))
      ((start)
       (scope-start-text (cadr item)
                         (if (caddr item) (cursor-at-scope at '(0 . 0) (cursor-name at) (cursor-kind at)) at)
                         index-of))
      ((variables)
       (let each ((variables (cadr item)) (text "D") (at at))
         (if (null? variables)
             (cons text at)
             (let ((i (index-of (car variables))))
               (each (cdr variables) (string-append text (vlq (- i (cursor-variable at))))
                     (cursor-at-variable at i))))))
      ((end)
       (let ((end (not-before (original-scope-end (cadr item)) at)))
         (cons (string-append "C" (scope-position-text end at))
               (cursor-at-scope at end (cursor-name at) (cursor-kind at)))))
      ((range-start)
       (let* ((range (cadr item))
              (start (generated-position (generated-range-start range) offset))
              (number (weak-table-ref numbers (generated-range-scope range)))
              (flags (+ (if (> (car start) (car (cursor-range-position at))) 1 0)
                        2
                        (if (generated-range-stack-frame? range) 4 0))))
         (cons (string-append "E" (unsigned-vlq flags) (range-position-text start at)
                              (vlq (- number (cursor-definition at))))
               (cursor-at-range at start number))))
      ((bindings)
       (cons (string-concatenate
               (cons "G" (map (lambda (b) (if b (unsigned-vlq (+ (index-of b) 1)) "A")) (cadr item))))
             at))
      ((range-end)
       (let ((end (generated-position (generated-range-end (cadr item)) offset)))
         (cons (string-append "F" (range-position-text end at))
               (cursor-at-range at end (cursor-definition at))))))))

;; /**
;;  * The text of a scope's start, and the cursor after it.
;;  * @param {original-scope} scope - The scope.
;;  * @param {list} at - The cursor.
;;  * @param {procedure} index-of - A name's index in the map's names.
;;  * @returns {pair} (text . cursor).
;;  */
(define (scope-start-text scope at index-of)
  (let* ((start (not-before (original-scope-start scope) at))
         (name (original-scope-name scope))
         (kind (original-scope-kind scope))
         (name-index (and name (index-of name)))
         (kind-index (index-of kind))
         (flags (+ (if name 1 0) 2 (if (original-scope-stack-frame? scope) 4 0))))
    (cons (string-append "B" (unsigned-vlq flags) (scope-position-text start at)
                         (if name (vlq (- name-index (cursor-name at))) "")
                         (vlq (- kind-index (cursor-kind at))))
          (cursor-at-scope at start (or name-index (cursor-name at)) kind-index))))

;; /**
;;  * A scope's position, or the last one written if it would come before it.
;;  * A macro can put the forms it is given in another order, so that a scope
;;  * inside one comes before a scope already written; the items' positions
;;  * must not go back.
;;  * @param {pair} position - (line . column).
;;  * @param {list} at - The cursor.
;;  * @returns {pair}
;;  */
(define (not-before position at)
  (let ((last (cursor-position at)))
    (if (or (< (car position) (car last)) (and (= (car position) (car last)) (< (cdr position) (cdr last))))
        last
        position)))

;; /**
;;  * A scope's position as written: its line's difference from the last,
;;  * then its column, as a difference on the same line.
;;  * @param {pair} position - (line . column).
;;  * @param {list} at - The cursor.
;;  * @returns {string}
;;  */
(define (scope-position-text position at)
  (let* ((last (cursor-position at))
         (lines (- (car position) (car last))))
    (string-append (unsigned-vlq lines)
                   (unsigned-vlq (if (zero? lines) (- (cdr position) (cdr last)) (cdr position))))))

;; /**
;;  * A range's position as written: its line's difference from the last, if
;;  * it differs, which the range's flags say, then its column, as a
;;  * difference on the same line.
;;  * @param {pair} position - (line . column).
;;  * @param {list} at - The cursor.
;;  * @returns {string}
;;  */
(define (range-position-text position at)
  (let* ((last (cursor-range-position at))
         (lines (- (car position) (car last))))
    (if (> lines 0)
        (string-append (unsigned-vlq lines) (unsigned-vlq (cdr position)))
        (unsigned-vlq (- (cdr position) (cdr last))))))

;; /**
;;  * A position in the generated code as a position in the script, which has
;;  * lines before it.
;;  * @param {pair} position - (line . column).
;;  * @param {integer} offset - The lines before.
;;  * @returns {pair}
;;  */
(define (generated-position position offset)
  (cons (+ (car position) offset) (cdr position)))
