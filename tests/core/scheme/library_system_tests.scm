;; The library system's Scheme (src/core/scheme/library_system.scm): parsing
;; `define-library` and import sets, the names an import set gives, and the
;; feature requirements `cond-expand` tests.

(import (scheme base)
        (scheme-js library-system))

;; /**
;;  * Whether a feature requirement is met, with the features `r7rs` and
;;  * `scheme-js` and only the library `(present lib)` available.
;;  * @param {*} requirement - The requirement.
;;  * @returns {boolean}
;;  */
(define (met? requirement)
  (requirement-met? requirement '(r7rs scheme-js)
                    (lambda (name) (equal? name '(present lib)))))

(test-group "library system - feature requirements"
  (test "a feature present" #t (met? 'r7rs))
  (test "a feature absent" #f (met? 'chicken))
  (test "and of present features" #t (met? '(and r7rs scheme-js)))
  (test "and with one absent" #f (met? '(and r7rs chicken)))
  (test "and of none" #t (met? '(and)))
  (test "or with one present" #t (met? '(or chicken r7rs)))
  (test "or of none" #f (met? '(or)))
  (test "not of an absent feature" #t (met? '(not chicken)))
  (test "nested" #t (met? '(or (and chicken r7rs) (not (not scheme-js)))))
  (test "a library available" #t (met? '(library (present lib))))
  (test "a library not available" #f (met? '(library (absent lib))))
  (test "an unknown requirement is not met" #f (met? '(frobnicate r7rs)))
  (test-error "not takes one requirement" "not" (met? '(not r7rs scheme-js)))
  (test-error "library takes one name" "library" (met? '(library))))

(test-group "library system - import sets"
  (define (steps spec) (import-set-steps (parse-import-set spec)))
  (define (library spec) (import-set-library-name (parse-import-set spec)))
  (test "a library name alone" '(scheme base) (library '(scheme base)))
  (test "has no filters" '() (steps '(scheme base)))
  (test "a name with a number in it" '(srfi 1) (library '(srfi 1)))
  (test "only" '((only car cdr)) (steps '(only (scheme base) car cdr)))
  (test "except" '((except car)) (steps '(except (scheme base) car)))
  (test "prefix" '((prefix . b:)) (steps '(prefix (scheme base) b:)))
  (test "rename" '((rename (car . first) (cdr . rest))) (steps '(rename (scheme base) (car first) (cdr rest))))
  (test "filters nest, innermost first" '((prefix . p:) (only p:car))
        (steps '(only (prefix (scheme base) p:) p:car)))
  (test "and the library is the innermost set's" '(scheme base)
        (library '(only (prefix (scheme base) p:) p:car)))
  ;; A library may be named by a filter's keyword: it is a filter only when
  ;; what follows the keyword is an import set.
  (test "a library whose name begins with a filter's keyword" '(only lib) (library '(only lib)))
  (test "is not filtered" '() (steps '(only lib))))

(test-group "library system - a program's import declarations"
  (test "the import sets of those it begins with, in order, and the forms after"
        '(((scheme base) (only (scheme write) display)) (display 1) (import (scheme char)))
        (program-parts '((import (scheme base)) (import (only (scheme write) display))
                         (display 1) (import (scheme char)))))
  (test "a program that begins with none has no import sets"
        '(() (define x 1) (import (scheme base)))
        (program-parts '((define x 1) (import (scheme base)))))
  (test "nor has an empty one" '(()) (program-parts '())))

(test-group "library system - the names an import set gives"
  (define (named name spec) (imported-name name (import-set-steps (parse-import-set spec))))
  (test "without filters, every name as it is" 'car (named 'car '(scheme base)))
  (test "only keeps a name listed" 'car (named 'car '(only (scheme base) car)))
  (test "and leaves out one that is not" #f (named 'cdr '(only (scheme base) car)))
  (test "except leaves out a name listed" #f (named 'car '(except (scheme base) car)))
  (test "and keeps one that is not" 'cdr (named 'cdr '(except (scheme base) car)))
  (test "prefix" 'b:car (named 'car '(prefix (scheme base) b:)))
  (test "rename" 'first (named 'car '(rename (scheme base) (car first))))
  (test "rename leaves other names" 'cdr (named 'cdr '(rename (scheme base) (car first))))
  (test "only around prefix names prefixed names" 'p:car (named 'car '(only (prefix (scheme base) p:) p:car)))
  (test "and leaves out the rest" #f (named 'cdr '(only (prefix (scheme base) p:) p:car)))
  (test "prefix around rename prefixes the new name" 'p:first
        (named 'car '(prefix (rename (scheme base) (car first)) p:))))

(test-group "library system - define-library"
  (define (parsed form) (parse-define-library form met?))
  (define simple
    (parsed '(define-library (my lib)
               (export a (rename b c))
               (import (scheme base) (only (srfi 1) fold))
               (begin (define a 1) (define b 2))
               (include "one.scm" "two.scm")
               (include-ci "three.scm")
               (include-library-declarations "decls.scm"))))
  (test "the name" '(my lib) (library-definition-name simple))
  (test "exports, each an internal name and the name exported" '((a . a) (b . c))
        (library-definition-exports simple))
  (test "imports, as import sets" '((scheme base) (srfi 1))
        (map import-set-library-name (library-definition-imports simple)))
  (test "with their filters" '(() ((only fold)))
        (map import-set-steps (library-definition-imports simple)))
  (test "the body's forms, in order" '((define a 1) (define b 2)) (library-definition-body simple))
  (test "included files" '("one.scm" "two.scm") (library-definition-includes simple))
  (test "included files folding case" '("three.scm") (library-definition-includes-ci simple))
  (test "files of library declarations" '("decls.scm") (library-definition-declaration-files simple))
  (test "declarations of a kind gather in order" '((define a 1) (define b 2) (define c 3))
        (library-definition-body
          (parsed '(define-library (l) (begin (define a 1)) (export a) (begin (define b 2) (define c 3))))))
  (test "cond-expand takes the first clause whose requirement is met" '((define x 'r7rs))
        (library-definition-body
          (parsed '(define-library (l)
                     (cond-expand (chicken (begin (define x 'chicken)))
                                  (r7rs (begin (define x 'r7rs)))
                                  (else (begin (define x 'else))))))))
  (test "or its else clause" '((define x 'else))
        (library-definition-body
          (parsed '(define-library (l)
                     (cond-expand (chicken (begin (define x 'chicken)))
                                  (else (begin (define x 'else))))))))
  (test "or nothing" '()
        (library-definition-body
          (parsed '(define-library (l) (cond-expand (chicken (begin (define x 1))))))))
  (test "a clause's declarations may be of any kind, cond-expand included" '((x . x) (y . y))
        (library-definition-exports
          (parsed '(define-library (l)
                     (cond-expand (r7rs (export x) (cond-expand ((library (present lib)) (export y)))))))))
  (test "an empty declaration is nothing" '() (library-definition-body (parsed '(define-library (l) ()))))
  (test-error "a library needs a name" "define-library" (parsed '(define-library)))
  (test-error "a form that is not define-library" "define-library" (parsed '(define-something (l))))
  (test-error "an unknown declaration" "unknown" (parsed '(define-library (l) (frobnicate x))))
  (test-error "a declaration that is not a list" "define-library" (parsed '(define-library (l) export)))
  (test-error "an export that is neither a name nor a rename" "export"
              (parsed '(define-library (l) (export (rename a))))))

(test-group "library system - names"
  (test "a library's key" "scheme.base" (library-key '(scheme base)))
  (test "with a number in its name" "srfi.1" (library-key '(srfi 1)))
  (test-error "a name is a list" "library" (library-key 'scheme))
  (test-error "of identifiers and exact integers" "library" (library-key '(scheme "base"))))

;; /**
;;  * A loader for these tests: a registry of its own, files from an
;;  * association list of paths to texts, and each form of a library's body
;;  * run by `eval`.
;;  * @param {list} files - Each file, `(path . text)`, its path a list of
;;  *   strings.
;;  * @param {procedure} [asked] - Called with each path the resolver is asked
;;  *   for.
;;  * @returns {loader}
;;  */
(define (loader-over files . asked)
  (let ((registry (make-library-registry #f #f '(r7rs))))
    ;; A library sees only what it imports, so the test libraries that add
    ;; import `+` from a library of the registry's own.
    (register-exports! registry "test.arithmetic" (list (cons '+ +)) #f)
    (make-loader registry
                 (lambda (path)
                   (if (pair? asked) ((car asked) path))
                   (cond ((assoc path files) => cdr)
                         (else (error "no such file" path))))
                 (interaction-environment)
                 (lambda (form env) (eval form env)))))

;; The value a library exports under a name, loading it if need be.
(define (exported loader name key)
  (cdr (assq key (load-library loader name))))

(define test-files
  '((("test" "a") . "(define-library (test a) (export x (rename y z)) (begin (define x 1) (define y 2)))")
    (("test" "b") . "(define-library (test b) (export w) (import (prefix (test a) a:) (test arithmetic)) (include \"b.scm\") (begin (define w0 10)))")
    (("test" "b.scm") . "(define w (+ w0 a:x a:z))")
    (("test" "ci") . "(define-library (test ci) (export ci-value) (include-ci \"ci.scm\"))")
    (("test" "ci.scm") . "(DEFINE CI-VALUE 'Folded)")
    (("test" "decls") . "(define-library (test decls) (include-library-declarations \"decls.scm\"))")
    (("test" "decls.scm") . "(export d) (import (only (test a) x) (test arithmetic)) (begin (define d (+ x 100)))")
    (("test" "probe") . "(define-library (test probe) (export found liar missing)
                           (cond-expand ((library (test a)) (begin (define found 'yes))) (else (begin (define found 'no))))
                           (cond-expand ((library (test liar)) (begin (define liar 'yes))) (else (begin (define liar 'no))))
                           (cond-expand ((library (test missing)) (begin (define missing 'yes))) (else (begin (define missing 'no)))))")
    (("test" "liar") . "(define-library (test other) (export o) (begin (define o 0)))")
    (("test" "twice") . "(define-library (test twice) (import (test a) (prefix (test a) p:)) (include \"b.scm\" \"b.scm\"))")
    (("test" "empty") . "")))

(test-group "library system - loading"
  (define loader (loader-over test-files))
  (test "a library's exports" 1 (exported loader '(test a) 'x))
  (test "an export renamed" 2 (exported loader '(test a) 'z))
  (test "is not exported under its own name" #f (assq 'y (load-library loader '(test a))))
  (test "an import set's filters, a library's includes, and its body" 13 (exported loader '(test b) 'w))
  (test "include-ci folds case" 'folded (exported loader '(test ci) 'ci-value))
  (test "a file of library declarations" 101 (exported loader '(test decls) 'd))
  (test "cond-expand finds a library that can be loaded" 'yes (exported loader '(test probe) 'found))
  (test "not one whose file declares another library" 'no (exported loader '(test probe) 'liar))
  (test "nor one the resolver cannot find" 'no (exported loader '(test probe) 'missing))
  (test "the libraries loaded, in order" '("test.a" "test.b")
        (let ((keys (registered-keys (loader-registry loader))))
          (list (car (member "test.a" keys)) (car (member "test.b" keys)))))
  (test "a define-library form" 5
        (cdr (assq 'v (define-library! loader '(define-library (test inline) (export v) (begin (define v 5)))))))
  (test "is registered" #t (and (registered-exports (loader-registry loader) "test.inline") #t))
  (test "importing into an environment" 1
        (let ((env (%make-library-environment (interaction-environment) '("test" "importer"))))
          (import-sets! loader env '((only (test a) x)))
          (eval 'x env)))
  (test-error "an empty library file" "empty" (load-library loader '(test empty)))
  (test-error "a resolver that cannot answer now" "async"
              (load-library (make-loader (make-library-registry #f #f '()) (lambda (path) #f) #f #f) '(test a))))

(test-group "library system - a library is loaded once"
  (define asked '())
  (define loader (loader-over test-files (lambda (path) (set! asked (cons path asked)))))
  (load-library loader '(test b))
  (load-library loader '(test a))
  (load-library loader '(test b))
  (test "each file read once, a library's imports before its includes"
        '(("test" "b") ("test" "a") ("test" "b.scm"))
        (reverse asked)))

(test-group "library system - closures run compiled, and as themselves for a debugger"
  ;; A closure run compiled, here as another procedure standing in for what
  ;; the compiler would make of it, so that which ran shows.
  (define registry (make-library-registry #f #f '()))
  (define debugged (make-debugged-programs))
  (define f (lambda () 'interpreted))
  (define compiled (lambda () 'compiled))
  (define held (list f))
  (define program (%make-library-environment (interaction-environment) '("test" "program")))
  (%run-compiled! f compiled)
  (record-compiled-over! registry debugged (list (cons f compiled)) program)
  (test "a closure run compiled runs compiled" 'compiled (f))
  (test "and is recorded as such" #t (compiled-over? registry f))
  (test "what it runs as is not" #f (compiled-over? registry compiled))
  (interpret-compiled-over! registry debugged #t program)
  (test "a program being debugged runs it as itself" 'interpreted (f))
  (test "and so does whatever holds it" 'interpreted ((car held)))
  (let ((later (lambda () 'interpreted-later))
        (later-compiled (lambda () 'compiled-later)))
    (%run-compiled! later later-compiled)
    (record-compiled-over! registry debugged (list (cons later later-compiled)) program)
    (test "one compiled while the program is debugged runs as itself at once" 'interpreted-later (later))
    (interpret-compiled-over! registry debugged #f program)
    (test "and, debugging over, compiled again" 'compiled-later (later)))
  (test "as is the other" 'compiled (f))
  (test "a program not debugged is left as it is" 'compiled
        (begin (interpret-compiled-over! registry debugged #f program) (f)))
  (test "a procedure whose resumable form no record's names is not switched back" #f
        (switch-back-to-closure! registry debugged f)))

(test-group "library system - the files a load would read"
  ;; A loader that has at hand only the files named, of the test files, and
  ;; answers #f for any other, as one waiting for a fetch does.
  (define (loader-with paths)
    (let ((registry (make-library-registry #f #f '(r7rs))))
      (register-exports! registry "test.arithmetic" (list (cons '+ +)) #f)
      (make-loader registry
                   (lambda (path) (and (member path paths) (cdr (assoc path test-files))))
                   #f #f)))
  (define (wanted paths name) (files-wanted (loader-with paths) name))
  (test "with nothing at hand, the library's own file" '(("test" "b")) (wanted '() '(test b)))
  (test "with that, the files of what it imports, then what it includes"
        '(("test" "a") ("test" "b.scm"))
        (wanted '(("test" "b")) '(test b)))
  (test "with those too, nothing" '() (wanted '(("test" "b") ("test" "a") ("test" "b.scm")) '(test b)))
  (test "include-ci's files" '(("test" "ci.scm")) (wanted '(("test" "ci")) '(test ci)))
  (test "a file of library declarations" '(("test" "decls.scm")) (wanted '(("test" "decls")) '(test decls)))
  (test "and then what its declarations import" '(("test" "a"))
        (wanted '(("test" "decls") ("test" "decls.scm")) '(test decls)))
  (test "each file once" '(("test" "a") ("test" "b.scm")) (wanted '(("test" "twice")) '(test twice)))
  (test "nothing of a library loaded already" '(("test" "b.scm"))
        (let ((loader (loader-with '(("test" "b")))))
          (register-exports! (loader-registry loader) "test.a" (list (cons 'x 1) (cons 'z 2)) #f)
          (files-wanted loader '(test b))))
  (test "a define-library form's" '(("test" "a") ("test" "b.scm"))
        (definition-files-wanted (loader-with '())
                                 '(define-library (test form) (import (test a)) (include "b.scm")))))

(test-group "library system - restoring a library from its table"
  ;; A restorer standing for a table of (test a): given the library's name
  ;; and its files' text, it answers with what binds a procedure the table
  ;; restores and the library's forms in order -- x such a procedure, bound
  ;; here to 1000, and y's definition a form -- or #f for any other library,
  ;; or for text it was not built from.
  (define bound '())
  (define (restorer name texts)
    (and (equal? name '("test" "a"))
         (equal? texts (list (cdr (assoc '("test" "a") test-files))))
         (cons (lambda (env name)
                 (set! bound (cons name bound))
                 (%environment-define! env name 1000))
               '((procedure x) (form (define y 2000))))))
  (define loader (loader-over test-files))
  (set-registry-restorer! (loader-registry loader) restorer)
  (test "a procedure the table restores, bound by it" 1000 (exported loader '(test a) 'x))
  (test "and the forms run in their places, not its source" 2000 (exported loader '(test a) 'z))
  (test "each procedure bound once" '(x) bound)
  (test "a library the restorer declines loads from its source, importing the restored one"
        3010 (exported loader '(test b) 'w))
  (test "text other than the table's is declined" 1
        (let ((other (loader-over (cons (cons '("test" "a") "(define-library (test a) (export x (rename y z)) (begin (define x 1) (define y 2) 'edited))")
                                        test-files))))
          (set-registry-restorer! (loader-registry other) restorer)
          (exported other '(test a) 'x)))
  (test "a define-library form is the program's own, never restored" 5
        (let ((inline (loader-over test-files)))
          (set-registry-restorer! (loader-registry inline)
                                  (lambda (name texts) (cons (lambda (env name) (%environment-define! env name 0))
                                                             '((procedure v)))))
          (cdr (assq 'v (define-library! inline '(define-library (test a) (export v) (begin (define v 5)))))))))
