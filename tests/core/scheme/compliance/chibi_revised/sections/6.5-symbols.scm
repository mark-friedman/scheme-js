;; R7RS Compliance Tests - Section 6.5: Symbols
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn

(test-group "6.5 Symbols"

  (test #t (symbol? 'foo))
  (test #t (symbol? (car '(a b))))
  (test #f (symbol? "bar"))
  (test #t (symbol? 'nil))
  (test #f (symbol? '()))
  (test #f (symbol? #f))

  (test #t (symbol=? 'a 'a))
  (test #f (symbol=? 'a 'A))
  (test #t (symbol=? 'a 'a 'a))
  (test #f (symbol=? 'a 'a 'A))

  (test "flying-fish"     
  (symbol->string 'flying-fish))
  (test "Martin" (symbol->string 'Martin))
  (test "Malvina" (symbol->string (string->symbol "Malvina")))

  (test 'mISSISSIppi (string->symbol "mISSISSIppi"))
  (test #t (eq? 'bitBlt (string->symbol "bitBlt")))
  (test #t (eq? 'LollyPop (string->symbol (symbol->string 'LollyPop))))
  (test #t (string=? "K. Harper, M.D."
                     (symbol->string (string->symbol "K. Harper, M.D."))))
)
