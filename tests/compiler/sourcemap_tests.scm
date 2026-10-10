;; Source maps for generated code (src/compiler/sourcemap.scm)
;;
;; Runs in the compiler's own environment. What a source map says, in its own
;; encoding: base-64 variable-length quantities, each field the difference from
;; the one before. That a map attached to compiled code takes a stack frame to
;; the Scheme it came from is checked from JavaScript, where the frame's
;; position can be read: tests/functional/compiled_stack_tests.js.

;; /**
;;  * A source span, as the reader records one.
;;  * @param {string} file - The file.
;;  * @param {integer} line - Its line, from one.
;;  * @param {integer} column - Its column, from one.
;;  * @returns {object}
;;  */
(define (span file line column)
  (js-obj "filename" file "line" line "column" column))

(test-group "source maps - variable-length quantities"
  (test "zero" "A" (vlq 0))
  (test "one" "C" (vlq 1))
  (test "minus one, the sign in the lowest bit" "D" (vlq -1))
  (test "fifteen, the largest in one digit" "e" (vlq 15))
  (test "sixteen takes a continuation digit" "gB" (vlq 16))
  (test "made again, the same" "gB" (vlq 16))
  (test "a thousand" "w+B" (vlq 1000))
  (test "minus a thousand" "x+B" (vlq -1000)))

(test-group "source maps - escaping for a URL"
  (test "what would end a URL's path or begin an escape, and spaces" "a%25b%3Fc%23d%20e"
        (url-path-escape "a%b?c#d e"))
  (test "nothing else" "count-down->list!" (url-path-escape "count-down->list!")))

;; /**
;;  * A file's text, as the source map's caller is asked for it: unknown.
;;  * @param {string} file - The file.
;;  * @returns {boolean} #f.
;;  */
(define (no-text file) #f)

(test-group "source maps - from the spans of lines"
  ;; A line maps from its start; the script has `offset` lines before the
  ;; first.
  (test "a line with no span maps nothing; a span's line and column count from zero"
        "{\"version\":3,\"sources\":[\"a.scm\"],\"names\":[],\"mappings\":\";;;;AAEI;AAAA\"}"
        (source-map (list #f (span "a.scm" 3 5) (span "a.scm" 3 5)) 3 no-text))
  (test "each field the difference from the segment before"
        "{\"version\":3,\"sources\":[\"a.scm\"],\"names\":[],\"mappings\":\"AAAA;AAEK;AAFL\"}"
        (source-map (list (span "a.scm" 1 1) (span "a.scm" 3 6) (span "a.scm" 1 1)) 0 no-text))
  (test "each file a span names is a source, in the order first named"
        "{\"version\":3,\"sources\":[\"a.scm\",\"b.scm\"],\"names\":[],\"mappings\":\"AAAA;ACAA;ADAA\"}"
        (source-map (list (span "a.scm" 1 1) (span "b.scm" 1 1) (span "a.scm" 1 1)) 0 no-text))
  (test "a file's name as it is"
        "{\"version\":3,\"sources\":[\"my file.scm\"],\"names\":[],\"mappings\":\"AAAA\"}"
        (source-map (list (span "my file.scm" 1 1)) 0 no-text))
  (test "a span from no file maps nothing" #f
        (source-map (list (span "<unknown>" 1 1)) 0 no-text))
  (test "no spans, no map" #f (source-map (list #f) 0 no-text))
  ;; A source a debugger cannot fetch -- a page's inline script -- has its
  ;; text in the map, and any other a null beside it.
  (test "a file's text, where it is known, in the map's sourcesContent"
        "{\"version\":3,\"sources\":[\"page.html#scheme-1\",\"b.scm\"],\"sourcesContent\":[\"(f #t)\\n\",null],\"names\":[],\"mappings\":\"AAAA;ACAA\"}"
        (source-map (list (span "page.html#scheme-1" 1 1) (span "b.scm" 1 1)) 0
                    (lambda (file) (and (string=? file "page.html#scheme-1") "(f #t)\n"))))
  (test "as a URL a script can name its map by"
        "data:application/json;charset=utf-8,{}"
        (source-map-url "{}"))
  (test "escaped for it: what would end the URL or change its meaning"
        "data:application/json;charset=utf-8,{\"sources\":[\"my%20file%23%3F%25.scm\"]}"
        (source-map-url "{\"sources\":[\"my file#?%.scm\"]}"))
  ;; A map written to a file of its own, a prebuilt table's or a built
  ;; program's, names a file by its path from the map, and ignore-lists the
  ;; system's.
  (test "a file named by a path of the caller's"
        "{\"version\":3,\"sources\":[\"../core/a.scm\"],\"names\":[],\"mappings\":\"AAAA\"}"
        (source-map (list (span "a.scm" 1 1)) 0 no-text (lambda (file) (string-append "../core/" file))))
  (test "and ignore-listed, those the caller says"
        "{\"version\":3,\"sources\":[\"a.scm\",\"b.scm\"],\"x_google_ignoreList\":[1],\"names\":[],\"mappings\":\"AAAA;ACAA\"}"
        (source-map (list (span "a.scm" 1 1) (span "b.scm" 1 1)) 0 no-text (lambda (file) file)
                    (lambda (file) (string=? file "b.scm")))))

;; ---------------------------------------------------------------------------
;; Scopes
;; ---------------------------------------------------------------------------
;;
;; A map's `scopes` (ECMA-426's scopes proposal) tell a debugger the scopes of
;; the source, each with its variables, and for each range of the generated
;; code the scope it is and the JavaScript that reads each variable there.
;; The strings expected here were checked against the encoder and decoder
;; DevTools carries (`third_party/source-map-scopes-codec`), which is what
;; reads them.

;; /**
;;  * A source scope, its children's parent set.
;;  * @param {string} kind - "function", "block" or "global".
;;  * @param {string|boolean} name - Its name, or #f.
;;  * @param {pair} start - (line . column), from zero.
;;  * @param {pair} end - Likewise.
;;  * @param {list} variables - Their names, as shown.
;;  * @param {list} children - Its child scopes, made by `child-scope`.
;;  * @returns {original-scope}
;;  */
(define (scope kind name start end variables . children)
  (let ((s (make-original-scope kind name start end #f)))
    (set-original-scope-variables! s variables)
    (set-original-scope-locals! s variables)
    (set-original-scope-children! s (map (lambda (make) (make s)) children))
    s))

;; /**
;;  * A scope to be made inside another, by `scope`.
;;  * @returns {procedure} From its parent to the scope.
;;  */
(define (child-scope kind name start end variables . children)
  (lambda (parent)
    (let ((s (make-original-scope kind name start end parent)))
      (set-original-scope-variables! s variables)
      (set-original-scope-locals! s variables)
      (set-original-scope-children! s (map (lambda (make) (make s)) children))
      s)))

(test-group "source maps - scopes"
  (test "an unsigned quantity, as scopes write most fields: no sign bit" '("A" "f" "gB" "of")
        (map unsigned-vlq '(0 31 32 1000)))
  (let* ((root (scope "global" #f '(0 . 0) '(10 . 0) '()
                      (child-scope "function" "f" '(1 . 2) '(3 . 4) '("x" "y"))))
         (f (car (original-scope-children root)))
         (ranges (list (make-generated-range '(0 . 0) '(5 . 0) root '() #f
                                             (list (make-generated-range '(1 . 0) '(3 . 0) f '("x" #f) #t '()))))))
    (test "a scope and its variables, the range it is, and what reads each variable there"
          '("BCAAA,BHBCCE,DGC,CCE,CHA,ECAA,EHBAC,GEA,FCA,FCA" "global" "f" "function" "x" "y")
          (let ((field (scopes-field (list root) ranges 0)))
            (cons (car field) (cdr field))))
    (test "a source with no scopes, after one with them" "BCAAA,BHBCCE,DGC,CCE,CHA,A,ECAA,EHBAC,GEA,FCA,FCA"
          (car (scopes-field (list root #f) ranges 0)))
    (test "the generated code's lines counted from the script's first, after those before it"
          "BCAAA,BHBCCE,DGC,CCE,CHA,EDDAA,EHBAC,GEA,FCA,FCA"
          (car (scopes-field (list root) ranges 3))))
  ;; A macro can put what it is given in another order, so that a scope
  ;; inside it comes before one already written; it is written where the
  ;; last ended, which keeps the field's positions in order, as it must be.
  (test "a scope out of order is written where the one before ended"
        '("BCAAA,BCCAC,CDA,BCAAA,CAA,CFA" "global" "block")
        (let ((field (scopes-field (list (scope "global" #f '(0 . 0) '(10 . 0) '()
                                                (child-scope "block" #f '(2 . 0) '(5 . 0) '())
                                                (child-scope "block" #f '(1 . 0) '(3 . 0) '())))
                                   '() 0)))
          (cons (car field) (cdr field))))
  (test "a map with scopes has them, and the names they use"
        "{\"version\":3,\"sources\":[\"a.scm\"],\"names\":[\"global\",\"f\",\"function\",\"x\"],\"mappings\":\"AAAA\",\"scopes\":\"BCAAA,BHBCCE,DG,CCE,CHA,ECAA,EHBAC,GE,FCA,FCA\"}"
        (let* ((root (scope "global" #f '(0 . 0) '(10 . 0) '()
                            (child-scope "function" "f" '(1 . 2) '(3 . 4) '("x"))))
               (f (car (original-scope-children root))))
          (source-map (list (span "a.scm" 1 1)) 0 no-text (lambda (file) file) (lambda (file) #f)
                      (make-unit-scopes "a.scm" root
                                        (list (make-generated-range '(0 . 0) '(5 . 0) root '() #f
                                                                    (list (make-generated-range '(1 . 0) '(3 . 0) f '("x") #t '()))))))))
  (test "but none when its scopes are of a file no line maps"
        "{\"version\":3,\"sources\":[\"a.scm\"],\"names\":[],\"mappings\":\"AAAA\"}"
        (source-map (list (span "a.scm" 1 1)) 0 no-text (lambda (file) file) (lambda (file) #f)
                    (make-unit-scopes "b.scm" (scope "global" #f '(0 . 0) '(1 . 0) '()) '()))))
