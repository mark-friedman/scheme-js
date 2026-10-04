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
;;; The map is written into the script itself, as a `data:` URL: a script made
;;; with `new Function` has no file a map could sit beside. Its JSON goes into
;;; the URL as it is, not in base 64: a URL's parser percent-encodes what it
;;; must and a `data:` URL's body is percent-decoded, so only what would end
;;; the URL or change its meaning is escaped (`url-path-escape`) -- and only a
;;; file's name can hold any of it.

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
  (let loop ((v (if (negative? n) (+ (* -2 n) 1) (* 2 n))) (digits '()))
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
;;  * map's `sourcesContent`, and any other file a null beside it. The names and
;;  * the text are escaped for the URL the map goes into (`source-map-url`),
;;  * whose reader undoes it.
;;  *
;;  * @param {list} spans - Each line's span, or #f, as the emitter renders
;;  *   them (`render-items` in `emit.scm`).
;;  * @param {integer} offset - How many lines the script has before the first.
;;  * @param {procedure} text-of - A file's text, where the map should hold it,
;;  *   or #f.
;;  * @returns {string|boolean}
;;  */
(define (source-map spans offset text-of)
  ;; `sources` is newest first, so a file's index in the order first named is
  ;; the length of what follows it. The mappings are appended to as they go,
  ;; as `render-items` in `emit.scm` builds its text.
  (let loop ((spans spans) (first? #t) (mappings (make-string offset #\;))
             (sources '()) (source 0) (line 0) (column 0) (previous #f))
    (if (null? spans)
        (and (pair? sources)
             (let* ((files (reverse sources))
                    (texts (map text-of files))
                    (json-list (lambda (strings)
                                 (string-join (map (lambda (s) (if s (url-path-escape (js-string s)) "null"))
                                                   strings)
                                              ","))))
               ;; The mappings need no quoting: base-64 digits and `;`.
               (string-append "{\"version\":3,\"sources\":[" (json-list files) "]"
                              (if (any (lambda (text) text) texts)
                                  (string-append ",\"sourcesContent\":[" (json-list texts) "]")
                                  "")
                              ",\"names\":[],\"mappings\":\"" mappings "\"}")))
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
                     sources index span-line span-column span))))))))

;; /**
;;  * A source map as a URL a script can name its map by, in a
;;  * `//# sourceMappingURL=` comment.
;;  * @param {string} json - The map, from `source-map`.
;;  * @returns {string}
;;  */
(define (source-map-url json)
  (string-append "data:application/json;charset=utf-8," json))
