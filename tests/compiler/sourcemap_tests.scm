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
  (test "a file's name escaped for the URL the map goes into"
        "{\"version\":3,\"sources\":[\"my%20file.scm\"],\"names\":[],\"mappings\":\"AAAA\"}"
        (source-map (list (span "my file.scm" 1 1)) 0 no-text))
  (test "a span from no file maps nothing" #f
        (source-map (list (span "<unknown>" 1 1)) 0 no-text))
  (test "no spans, no map" #f (source-map (list #f) 0 no-text))
  ;; A source a debugger cannot fetch -- a page's inline script -- has its
  ;; text in the map, and any other a null beside it; the text escaped for
  ;; the URL as a file's name is.
  (test "a file's text, where it is known, in the map's sourcesContent"
        "{\"version\":3,\"sources\":[\"page.html%23scheme-1\",\"b.scm\"],\"sourcesContent\":[\"(f%20%23t)\\n\",null],\"names\":[],\"mappings\":\"AAAA;ACAA\"}"
        (source-map (list (span "page.html#scheme-1" 1 1) (span "b.scm" 1 1)) 0
                    (lambda (file) (and (string=? file "page.html#scheme-1") "(f #t)\n"))))
  (test "as a URL a script can name its map by"
        "data:application/json;charset=utf-8,{}"
        (source-map-url "{}")))
