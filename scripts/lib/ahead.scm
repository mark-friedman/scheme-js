;;; ahead.scm -- compiling a whole program ahead of time.
;;;
;;; A program compiled ahead of time runs with no interpreter beneath it
;;; (`runProgram` in src/compiler/ahead.js), so every form it runs has to be
;;; compiled: each top-level form of the program, and of every library it
;;; uses, which run as the libraries load -- procedures, and the few values a
;;; library makes, such as `equal-tree-budget` and the current ports' cells.
;;; Macros are not needed: an expanded program uses none.
;;;
;;; The build loads the program's libraries from their source, in a library
;;; registry of its own, noting each top-level form loading runs with the core
;;; form it expanded into, as the prebuilt tables' build does
;;; (scripts/lib/prebuild.scm), and expands the program's forms where its
;;; imports put it, without running them. Each core form is then items
;;; (`top-level-items`), and each item compiled.
;;;
;;; Then it follows what the code refers to, from every form that runs as its
;;; unit loads, other than a procedure's definition, which only binds it. Each
;;; name a piece of code refers to is found where it would be bound as the
;;; program runs -- defined by the unit the code is in, imported from another,
;;; through the libraries that re-export it, or a primitive (`bindings-at`) --
;;; and what defines it is reached in turn. A procedure nothing reaches is left
;;; out. A piece of code reached that the compiler declined -- one that
;;; names `dynamic-wind`, `guard`, `parameterize` or another form there is no
;;; compiled code for yet -- a constant reached that cannot be written down, or
;;; a primitive reached that the runtime does not carry, refuses the program, by
;;; name: it could not run.
;;;
;;; What is written is a table of units -- each library in the order it
;;; loaded, then the program -- each with the bindings it imports and its
;;; items, in order (`render-program`).

;; ---------------------------------------------------------------------------
;; Items
;; ---------------------------------------------------------------------------

;; /**
;;  * The items a top-level core form makes, as `(kind name form)`, in the
;;  * order they run: `(procedure name lambda)` for the definition of a
;;  * procedure, bound to compiled code as its unit loads; `(define name
;;  * expression)` for any other definition, its value computed then; and
;;  * `(run #f form)` for a form run for its effect. A body's forms, `(seq
;;  * forms)`, are each one's items in turn; a constant, which is what a
;;  * macro's definition expands into, makes none.
;;  * @param {list} core - The core form.
;;  * @returns {list}
;;  */
(define (top-level-items core)
  (case (car core)
    ((seq) (append-map top-level-items (cadr core)))
    ((define)
     (let ((value (caddr core)))
       (list (list (if (eq? (car value) 'lambda) 'procedure 'define) (cadr core) value))))
    ((lit) '())
    (else (list (list 'run #f core)))))

;; /**
;;  * An item, compiled.
;;  * @property {symbol} kind - `procedure`, `define` or `run`.
;;  * @property {symbol|boolean} name - The name it defines, or #f.
;;  * @property {generated|declined} code - Its code: a procedure's, or a
;;  *   thunk's that computes the value or runs the form.
;;  */
(define-record-type item
  (make-item kind name code)
  item?
  (kind item-kind)
  (name item-name)
  (code item-code))

;; /**
;;  * Compiles an item in the environment it will run in.
;;  * @param {list} spec - `(kind name form)` (`top-level-items`).
;;  * @param {object} env - The environment.
;;  * @returns {item}
;;  */
(define (compile-item spec env)
  (let ((kind (car spec))
        (name (cadr spec))
        (form (caddr spec)))
    (make-item kind name
               (generate-lambda (if (eq? kind 'procedure) form (expression-thunk form))
                                (if name (symbol->string name) "top-level")
                                #f env #f #f))))

;; ---------------------------------------------------------------------------
;; Units
;; ---------------------------------------------------------------------------

;; The key of the library the runtime's primitives are imported from.
(define primitives-key "scheme.primitives")

;; /**
;;  * A library, or the program, as the build sees it.
;;  * @property {string|boolean} key - The library's key, `scheme.core`, or #f
;;  *   for the program.
;;  * @property {list|boolean} name - The library's name, as strings, or #f.
;;  * @property {object} env - Its environment, as the build made it.
;;  * @property {list} imports - What each name it imports came from, (local
;;  *   from-key . external), the latest import of a name first
;;  *   (`imported-variables`).
;;  * @property {list} exports - Each name it exports, (external . internal).
;;  * @property {list} items - Its items, in order.
;;  */
(define-record-type unit
  (make-unit key name env imports exports items)
  unit?
  (key unit-key)
  (name unit-name)
  (env unit-env)
  (imports unit-imports)
  (exports unit-exports)
  (items unit-items))

;; /**
;;  * A library's key from its name as an import set gives it, its parts
;;  * symbols or numbers.
;;  * @param {list} name - The name.
;;  * @returns {string}
;;  */
(define (name-key name)
  (library-key (map (lambda (part) (if (symbol? part) (symbol->string part) (number->string part))) name)))

;; /**
;;  * The variables some import sets bind, as `import-into!` binds them, each
;;  * as (local from-key . external): every variable each set's library
;;  * exports, under the name the set's filters give it.
;;  * @param {list} specs - The import sets, parsed, in order.
;;  * @returns {list} Latest first, so that `assq` finds the binding that
;;  *   stands.
;;  */
(define (imported-variables specs)
  (fold (lambda (spec bound)
          (let ((key (name-key (import-set-library-name spec))))
            (fold (lambda (export bound)
                    (let ((local (imported-name (car export) (import-set-steps spec))))
                      (if local (cons (cons local (cons key (car export))) bound) bound)))
                  bound
                  (library-variables key))))
        '()
        specs))

;; /**
;;  * A library loaded, as a unit: its imports and exports from its
;;  * definition, and its items from the forms its loading ran.
;;  * @param {procedure} read-source - A `source-reader`.
;;  * @param {list} loaded - `(name env forms)`: its name, as strings, its
;;  *   environment, and its forms, `(form . core)`.
;;  * @returns {unit}
;;  */
(define (library-unit read-source loaded)
  (let* ((name (car loaded))
         (env (cadr loaded))
         (definition (library-definition read-source name)))
    (if (pair? (library-definition-declaration-files definition))
        (error "ahead: a library that includes library declarations is not supported yet" name))
    (make-unit (library-key name) name env
               (imported-variables (library-definition-imports definition))
               (map (lambda (spec) (cons (cdr spec) (car spec))) (library-definition-exports definition))
               (map (lambda (spec) (compile-item spec env))
                    (append-map (lambda (noted) (top-level-items (cdr noted))) (caddr loaded))))))

;; /**
;;  * The program, as a unit. The names its forms define are bound, in the
;;  * build's environment, to a placeholder before its code is generated, since
;;  * its forms are not run: the compiler expands a primitive inline only where
;;  * the environment binds the name to the primitive, and the program may
;;  * define one of the same name.
;;  * @param {list} imports - Its import sets, parsed.
;;  * @param {object} env - Its environment.
;;  * @param {list} cores - Its forms after the imports, expanded.
;;  * @returns {unit}
;;  */
(define (program-unit imports env cores)
  (let ((specs (append-map top-level-items cores)))
    (for-each (lambda (spec) (if (cadr spec) (%environment-define! env (cadr spec) #f))) specs)
    (make-unit #f #f env (imported-variables imports) '()
               (map (lambda (spec) (compile-item spec env)) specs))))

;; /**
;;  * Whether a unit defines a name with an item of its own.
;;  * @param {unit} unit - The unit.
;;  * @param {symbol} name - The name.
;;  * @returns {boolean}
;;  */
(define (unit-defines? unit name)
  (any (lambda (item) (eq? (item-name item) name)) (unit-items unit)))

;; ---------------------------------------------------------------------------
;; Where a name is bound
;; ---------------------------------------------------------------------------

;; /**
;;  * The units of a build, the program last, the names of the runtime's
;;  * primitives, as the build's registry has them, and where each item runs in
;;  * the program's load.
;;  * @property {list} units - The units, in the order they load.
;;  * @property {list} primitive-names - The primitives', as symbols.
;;  * @property {list} positions - Each `(item . position)` (`item-positions`).
;;  */
(define-record-type world
  (make-world units primitive-names positions)
  world?
  (units world-units)
  (primitive-names world-primitive-names)
  (positions world-positions))

;; /**
;;  * Each item of some units, numbered in the order the items run as the
;;  * program loads: each unit's in order, the units in the order they load.
;;  * @param {list} units - The units, in the order they load.
;;  * @returns {list} Each `(item . position)`.
;;  */
(define (item-positions units)
  (let ((items (append-map unit-items units)))
    (map cons items (iota (length items)))))

;; /**
;;  * Where an item runs in the program's load.
;;  * @param {world} world - The build's units.
;;  * @param {item} item - The item.
;;  * @returns {integer}
;;  */
(define (item-position world item)
  (cdr (assq item (world-positions world))))

;; /**
;;  * Where in the program's load a unit's first definition of a name runs, or
;;  * #f if it defines none.
;;  * @param {world} world - The build's units.
;;  * @param {unit} unit - The unit.
;;  * @param {symbol} name - The name.
;;  * @returns {integer|boolean}
;;  */
(define (definition-position world unit name)
  (let ((defining (find (lambda (item) (eq? (item-name item) name)) (unit-items unit))))
    (and defining (item-position world defining))))

;; /**
;;  * The unit of a library, by its key.
;;  * @param {world} world - The build's units.
;;  * @param {string} key - The key.
;;  * @returns {unit|boolean}
;;  */
(define (unit-by-key world key)
  (find (lambda (unit) (equal? (unit-key unit) key)) (world-units world)))

;; /**
;;  * The unit of a library, by its environment.
;;  * @param {world} world - The build's units.
;;  * @param {object} env - The environment.
;;  * @returns {unit|boolean}
;;  */
(define (unit-by-env world env)
  (find (lambda (unit) (eq? (unit-env unit) env)) (world-units world)))

;; /**
;;  * What binds a name in a unit where the unit's own definitions do not: an
;;  * import, as `(item key name)`, the item of the unit of that key that
;;  * defined what it imported, through every library that re-exported it, or
;;  * `(primitive name)`; else, the name not imported, the primitive of that
;;  * name, which the runtime's environment of primitives, inside which every
;;  * unit's is, binds; or #f, nowhere the build can see, as a JavaScript global
;;  * is.
;;  * @param {world} world - The build's units.
;;  * @param {unit} unit - The unit.
;;  * @param {symbol} name - The name.
;;  * @returns {list|boolean}
;;  */
(define (outside-binding world unit name)
  (cond ((assq name (unit-imports unit))
         => (lambda (entry)
              (let ((from (cadr entry))
                    (external (cddr entry)))
                (if (string=? from primitives-key)
                    (list 'primitive external)
                    (let* ((exporter (unit-by-key world from))
                           (internal (assq external (unit-exports exporter))))
                      (and internal (binding-of world exporter (cdr internal))))))))
        ((memq name (world-primitive-names world)) (list 'primitive name))
        (else #f)))

;; /**
;;  * What binds a name in a unit once it has loaded, which is what it exports:
;;  * its own definition, if it has one, else what binds it outside
;;  * (`outside-binding`).
;;  * @param {world} world - The build's units.
;;  * @param {unit} unit - The unit.
;;  * @param {symbol} name - The name.
;;  * @returns {list|boolean}
;;  */
(define (binding-of world unit name)
  (if (unit-defines? unit name)
      (list 'item (unit-key unit) name)
      (outside-binding world unit name)))

;; /**
;;  * The bindings a read of a name in a unit's code can find, made at a point
;;  * in the program's load: the unit's own definition, if it has one, which
;;  * every read after it finds; and, a read made before the definition has
;;  * run, or where the unit has none, what binds the name outside -- an import
;;  * a program's form reads before the program defines the name itself, as the
;;  * interpreter lets it.
;;  * @param {world} world - The build's units.
;;  * @param {unit} unit - The unit.
;;  * @param {symbol} name - The name.
;;  * @param {integer} position - Where in the load the read is made.
;;  * @returns {list}
;;  */
(define (bindings-at world unit name position)
  (let ((defined (definition-position world unit name))
        (outside (outside-binding world unit name)))
    (append (if defined (list (list 'item (unit-key unit) name)) '())
            (if (and outside (or (not defined) (<= position defined))) (list outside) '()))))

;; ---------------------------------------------------------------------------
;; What the program reaches
;; ---------------------------------------------------------------------------

;; /**
;;  * The names a piece of code refers to, each where it reads it: `(env .
;;  * name)`, the environment of the unit it is in, or of the library whose own
;;  * binding a library's macro had it read (`generated-library-globals`).
;;  * @param {generated} code - The code.
;;  * @returns {list}
;;  */
(define (references code)
  (map (lambda (global)
         (let ((library (assq global (generated-library-globals code))))
           (if library
               (cons (cddr library) (cadr library))
               (cons (generated-env code) global))))
       (generated-globals code)))

;; /**
;;  * What a program reaches.
;;  * @property {list} items - The items reached, each `(unit . item)`.
;;  * @property {list} needs - Each binding a unit's reached code finds for a
;;  *   name it reads, `(unit name . binding)` (`bindings-at`).
;;  */
(define-record-type reach
  (make-reach items needs)
  reach?
  (items reach-items)
  (needs reach-needs))

;; /**
;;  * Whether an item is reached.
;;  * @param {reach} reach - What is reached.
;;  * @param {item} item - The item.
;;  * @returns {boolean}
;;  */
(define (reached? reach item)
  (any (lambda (entry) (eq? (cdr entry) item)) (reach-items reach)))

;; /**
;;  * The items a binding is made by, other than those already reached: each
;;  * of the defining unit's items that defines the name, a later one
;;  * redefining it too.
;;  * @param {world} world - The build's units.
;;  * @param {list} binding - The binding (`bindings-at`).
;;  * @param {list} reached - The items reached so far, each `(unit . item)`.
;;  * @returns {list} Each `(unit . item)`.
;;  */
(define (items-making world binding reached)
  (if (eq? (car binding) 'item)
      (let ((defining (unit-by-key world (cadr binding))))
        (filter-map (lambda (item)
                      (and (eq? (item-name item) (caddr binding))
                           (not (any (lambda (entry) (eq? (cdr entry) item)) reached))
                           (cons defining item)))
                    (unit-items defining)))
      '()))

;; /**
;;  * Whether two needs are the same: the same binding of the same name in the
;;  * same unit.
;;  * @param {list} a - A need, `(unit name . binding)`.
;;  * @param {list} b - Another.
;;  * @returns {boolean}
;;  */
(define (same-need? a b)
  (and (eq? (car a) (car b)) (eq? (cadr a) (cadr b)) (equal? (cddr a) (cddr b))))

;; /**
;;  * Follows the code from every item that runs as its unit loads -- every
;;  * one but a procedure's definition, which only binds it -- to everything it
;;  * reads, and from that in turn.
;;  *
;;  * Depth first, from each of those items in the order they run, so that a
;;  * piece of code is first followed from the earliest point in the load that
;;  * reaches it, and each name it reads is found as it is there
;;  * (`bindings-at`): a later read can only find the unit's own definition,
;;  * which every read reaches.
;;  * @param {world} world - The build's units.
;;  * @returns {reach}
;;  */
(define (follow world)
  (let ((roots (append-map (lambda (unit)
                             (filter-map (lambda (item)
                                           (and (not (eq? (item-kind item) 'procedure)) (cons unit item)))
                                         (unit-items unit)))
                           (world-units world))))
    (let loop ((pending roots) (position 0) (items roots) (needs '()))
      (if (null? pending)
          (make-reach (reverse items) (reverse needs))
          (let* ((item (cdar pending))
                 (position (if (eq? (item-kind item) 'procedure) position (item-position world item))))
            (if (not (generated? (item-code item)))
                (loop (cdr pending) position items needs)
                (let walk ((refs (references (item-code item))) (pending (cdr pending)) (items items)
                           (needs needs))
                  (if (null? refs)
                      (loop pending position items needs)
                      (let* ((unit (unit-by-env world (caar refs)))
                             (name (cdar refs))
                             (new (if unit
                                      (filter (lambda (need) (not (any (lambda (n) (same-need? n need)) needs)))
                                              (map (lambda (binding) (cons unit (cons name binding)))
                                                   (bindings-at world unit name position)))
                                      '()))
                             (found (fold (lambda (need found)
                                            (append found (items-making world (cddr need) (append found items))))
                                          '()
                                          new)))
                        (walk (cdr refs) (append found pending) (append (reverse found) items)
                              (append (reverse new) needs)))))))))))

;; ---------------------------------------------------------------------------
;; Refusing
;; ---------------------------------------------------------------------------

;; /**
;;  * Where an item is, for a refusal: its name and its unit's.
;;  * @param {unit} unit - Its unit.
;;  * @param {item} item - The item.
;;  * @returns {string}
;;  */
(define (item-place unit item)
  (string-append (if (item-name item) (symbol->string (item-name item)) "a top-level form")
                 " in " (if (unit-key unit) (string-append "(" (string-join (unit-name unit) " ") ")") "the program")))

;; /**
;;  * Why a program cannot run compiled ahead of time, each as a line: each
;;  * item reached the compiler declined, each constant reached that cannot be
;;  * written down, and each primitive reached that the runtime does not carry.
;;  * @param {reach} reach - What the program reaches.
;;  * @param {list} carried - The names of the primitives the runtime carries.
;;  * @returns {list} The reasons; empty if it can run.
;;  */
(define (refusals reach carried)
  (append
   (filter-map (lambda (entry)
                 (let ((code (item-code (cdr entry))))
                   (cond ((declined? code)
                          (string-append (item-place (car entry) (cdr entry)) " is not compiled: "
                                         (declined-reason code)))
                         ((not (constants-expression (generated-constants code)))
                          (string-append (item-place (car entry) (cdr entry))
                                         " holds a constant that cannot be written down: "
                                         (let ((port (open-output-string)))
                                           (write (find (lambda (c) (not (constant-expression c)))
                                                        (generated-constants code))
                                                  port)
                                           (get-output-string port))))
                         (else #f))))
               (reach-items reach))
   (delete-duplicates
    (filter-map (lambda (binding)
                  (and (eq? (car binding) 'primitive) (not (memq (cadr binding) carried))
                       (string-append "the primitive " (symbol->string (cadr binding))
                                      " is not carried by the runtime of a program compiled ahead of time")))
                (map cddr (reach-needs reach))))))

;; ---------------------------------------------------------------------------
;; The build
;; ---------------------------------------------------------------------------

;; /**
;;  * A program built ahead of time.
;;  * @property {world} world - Its units.
;;  * @property {reach} reach - What it reaches.
;;  * @property {list} refusals - Why it cannot run, if it cannot (`refusals`).
;;  */
(define-record-type program-build
  (make-program-build world reach refusals)
  program-build?
  (world program-build-world)
  (reach program-build-reach)
  (refusals program-build-refusals))

;; /**
;;  * Builds a program ahead of time: loads the libraries it imports from
;;  * their source, in a registry of the build's own, expands its forms, and
;;  * compiles every form that runs.
;;  * @param {list} forms - The program's forms, as read.
;;  * @param {procedure} read-source - A `source-reader` of the libraries'
;;  *   files.
;;  * @returns {program-build}
;;  */
(define (build-program forms read-source)
  (let ((imports (map parse-import-set (car (program-parts forms))))
        (loaded '()))
    (if (null? imports)
        (make-program-build #f #f
                            '("a program compiled ahead of time begins with the import declarations of the libraries it uses"))
        (with-private-libraries
         (library-resolver read-source)
         (lambda (name env) (set! loaded (cons (list name env (take-noted!)) loaded)))
         (lambda ()
           (for-each (lambda (spec) (load-library (import-set-library-name spec) note!)) imports)
           (let* ((expanded (expand-program forms))
                  (libraries (map (lambda (l) (library-unit read-source l)) (reverse loaded)))
                  (units (append libraries (list (program-unit imports (car expanded) (cdr expanded)))))
                  (world (make-world units (map car (library-variables primitives-key)) (item-positions units)))
                  (reach (follow world)))
             (make-program-build world reach (refusals reach (ahead-primitive-names)))))))))

;; ---------------------------------------------------------------------------
;; Writing it down
;; ---------------------------------------------------------------------------

;; /**
;;  * The bindings a unit imports that its reached code reads, as the text of
;;  * an array of `[local, from, name]`: from the unit whose item defined it, or,
;;  * from `null`, a primitive under another name. A primitive under its own
;;  * name is not imported: the runtime's environment of primitives, which every
;;  * unit's is inside, binds it. One the unit also defines is imported where
;;  * something reads it before the definition runs (`bindings-at`).
;;  * @param {world} world - The build's units.
;;  * @param {reach} reach - What the program reaches.
;;  * @param {unit} unit - The unit.
;;  * @returns {string}
;;  */
(define (imports-text world reach unit)
  (let ((imports
         (filter-map (lambda (need)
                       (let ((name (cadr need))
                             (binding (cddr need)))
                         (and (eq? (car need) unit)
                              (case (car binding)
                                ((item)
                                 (and (not (equal? (cadr binding) (unit-key unit)))
                                      (string-append "[" (json-string (symbol->string name)) ", "
                                                     (json-string (cadr binding)) ", "
                                                     (json-string (symbol->string (caddr binding))) "]")))
                                ((primitive)
                                 (and (not (eq? (cadr binding) name))
                                      (string-append "[" (json-string (symbol->string name)) ", null, "
                                                     (json-string (symbol->string (cadr binding))) "]")))
                                (else #f)))))
                     (reach-needs reach))))
    (string-append "[" (string-join imports ", ") "]")))

;; /**
;;  * An item as the text of an object: what it is, the function building its
;;  * constant pool from the constructors the loader gives it, and its code, a
;;  * function of the runtime, the environment and the pool.
;;  * @param {item} item - The item, compiled, with constants that can be
;;  *   written down.
;;  * @returns {string}
;;  */
(define (item-text item)
  (let ((code (item-code item)))
    (string-append
     "        {"
     (case (item-kind item)
       ((procedure) (string-append "procedure: " (json-string (symbol->string (item-name item)))))
       ((define) (string-append "define: " (json-string (symbol->string (item-name item)))))
       (else "run: true"))
     ",\n          constants: ({intern, Cons, Char, Flonum, Rational, Complex}) => "
     (constants-expression (generated-constants code))
     ",\n          make: (R, E, K) => {\n"
     (string-join (map (lambda (line) (string-append "            " line))
                       (string-split (generated-source code) "\n"))
                  "\n")
     "\n          }}")))

;; /**
;;  * A unit as the text of an object: its library's name, if it is one, its
;;  * imports, and the items that run as it loads and the procedures reached.
;;  * @param {world} world - The build's units.
;;  * @param {reach} reach - What the program reaches.
;;  * @param {unit} unit - The unit.
;;  * @returns {string}
;;  */
(define (unit-text world reach unit)
  (string-append
   "    {\n"
   (if (unit-key unit) (string-append "      library: " (json-strings (unit-name unit)) ",\n") "")
   "      imports: " (imports-text world reach unit) ",\n"
   "      items: [\n"
   (string-join (map item-text (filter (lambda (item) (reached? reach item)) (unit-items unit))) ",\n")
   "\n      ]\n"
   "    }"))

;; /**
;;  * A program built ahead of time as a JavaScript module: its table, which
;;  * `runProgram` in src/compiler/ahead.js runs. It imports nothing.
;;  * @param {program-build} build - The build, which nothing refused.
;;  * @param {string} source - The program's file, for the banner.
;;  * @returns {string}
;;  */
(define (render-program build source)
  (string-append
   "// Auto-generated by scripts/lib/ahead.scm from " source " - do not edit manually\n"
   "//\n"
   "// A program compiled ahead of time, with what it uses of each library: run\n"
   "// it with `runProgram` in src/compiler/ahead.js.\n"
   "\n"
   "export default {\n"
   "  units: [\n"
   (string-join (map (lambda (unit)
                       (unit-text (program-build-world build) (program-build-reach build) unit))
                     (world-units (program-build-world build)))
                ",\n")
   "\n  ]\n"
   "};\n"))
