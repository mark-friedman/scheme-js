;; language_balance.scm -- how many lines of Scheme and of JavaScript a change
;; adds and removes under src/.
;;
;; Run from the repository's root, as `npm run audit:languages -- <base>`. It
;; compares the working tree -- uncommitted changes and untracked files
;; included -- with the commit <base>, or with HEAD if none is given. A task's
;; outcome names each piece of JavaScript it added and why (the "Scheme first"
;; section of .agent/rules/rules.md); this is the count to check that against.
;;
;; Generated files are left out, since the compiled tables under src/packaging/
;; would swamp the count; each one starts with an "Auto-generated" line.
;;
;; Git is run through Node's child_process, reached by interop, since the CLI
;; does not connect standard input to (current-input-port) for a program to
;; read git's output from a pipe.

(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (srfi 1)
        (srfi 152)
        (scheme-js interop))

;; ---------------------------------------------------------------------------
;; Changes
;; ---------------------------------------------------------------------------

;; /**
;;  * The lines a change adds to one file and removes from it.
;;  * @param {string} path - The file, relative to the repository's root.
;;  * @param {integer} added - Lines added.
;;  * @param {integer} removed - Lines removed.
;;  */
(define-record-type change
  (make-change path added removed)
  change?
  (path change-path)
  (added change-added)
  (removed change-removed))

;; /**
;;  * The language a file is written in, by its extension.
;;  * @param {string} path - The file.
;;  * @returns {symbol|boolean} `scheme`, `javascript`, or #f for any other.
;;  */
(define (language-of path)
  (cond ((or (string-suffix? ".scm" path) (string-suffix? ".sld" path)) 'scheme)
        ((or (string-suffix? ".js" path) (string-suffix? ".mjs" path)) 'javascript)
        (else #f)))

;; /**
;;  * A line of `git diff --numstat` as a change: lines added, lines removed and
;;  * the path, separated by tabs.
;;  * @param {string} line - The line.
;;  * @returns {change|boolean} The change, or #f for a binary file, whose counts
;;  *   git gives as "-".
;;  */
(define (numstat->change line)
  (let* ((fields (string-split line "\t"))
         (added (string->number (first fields)))
         (removed (string->number (second fields))))
    (and added removed (make-change (third fields) added removed))))

;; /**
;;  * The number of lines in a file.
;;  * @param {string} path - The file.
;;  * @returns {integer}
;;  */
(define (line-count path)
  (call-with-input-file path
    (lambda (port)
      (let count ((n 0))
        (if (eof-object? (read-line port)) n (count (+ n 1)))))))

;; /**
;;  * An untracked file as a change that adds every line of it.
;;  * @param {string} path - The file.
;;  * @returns {change}
;;  */
(define (untracked->change path)
  (make-change path (line-count path) 0))

;; /**
;;  * Whether a file was written by a build script: whether its first line says
;;  * it is auto-generated. A file the change deleted is not.
;;  * @param {string} path - The file.
;;  * @returns {boolean}
;;  */
(define (generated? path)
  (and (file-exists? path)
       (let ((first-line (call-with-input-file path read-line)))
         (and (string? first-line)
              (string-contains first-line "Auto-generated")
              #t))))

;; /**
;;  * Whether a change is one to count: to Scheme or JavaScript, written by hand.
;;  * @param {change} c - The change.
;;  * @returns {boolean}
;;  */
(define (counted? c)
  (and (language-of (change-path c))
       (not (generated? (change-path c)))))

;; ---------------------------------------------------------------------------
;; Git
;; ---------------------------------------------------------------------------

;; /**
;;  * Node's child_process module.
;;  * @type {object}
;;  */
(define child-process (js-invoke process "getBuiltinModule" "node:child_process"))

;; /**
;;  * Runs git, and answers the lines it printed.
;;  * @param {...string} arguments - Its arguments.
;;  * @returns {list} The lines, without empty ones.
;;  */
(define (git . arguments)
  (let ((output (js-invoke child-process "execFileSync" "git" (list->vector arguments)
                           #{(encoding "utf8")})))
    (remove string-null? (string-split output "\n"))))

;; /**
;;  * The counted changes under src/ since a commit, untracked files included.
;;  * @param {string} base - The commit.
;;  * @returns {list} The changes.
;;  */
(define (changes-since base)
  (filter counted?
          (append (filter-map numstat->change
                              (git "diff" "--numstat" "--no-renames" base "--" "src"))
                  (map untracked->change
                       (git "ls-files" "--others" "--exclude-standard" "--" "src")))))

;; ---------------------------------------------------------------------------
;; The report
;; ---------------------------------------------------------------------------

;; /**
;;  * The changes in one language.
;;  * @param {symbol} language - `scheme` or `javascript`.
;;  * @param {list} changes - The changes.
;;  * @returns {list}
;;  */
(define (changes-in language changes)
  (filter (lambda (c) (eq? (language-of (change-path c)) language)) changes))

;; /**
;;  * The sum of one count over some changes.
;;  * @param {procedure} count - `change-added` or `change-removed`.
;;  * @param {list} changes - The changes.
;;  * @returns {integer}
;;  */
(define (total count changes)
  (fold + 0 (map count changes)))

;; /**
;;  * A number right-aligned in a column.
;;  * @param {integer} n - The number.
;;  * @param {integer} width - The column's width.
;;  * @returns {string}
;;  */
(define (column n width)
  (string-pad (number->string n) width))

;; /**
;;  * Writes a line of text and a newline.
;;  * @param {...string} parts - The text, in pieces.
;;  */
(define (say . parts)
  (for-each display parts)
  (newline))

;; /**
;;  * Writes the totals for each language, then each JavaScript file the change
;;  * adds lines to, in git's order, by path.
;;  * @param {string} base - The commit compared with.
;;  * @param {list} changes - The counted changes.
;;  */
(define (report base changes)
  (say "Lines under src/ against " base ", uncommitted and untracked files included,")
  (say "generated files left out:")
  (say)
  (say "                added  removed")
  (for-each
   (lambda (language label)
     (let ((mine (changes-in language changes)))
       (say "  " label (column (total change-added mine) 9) (column (total change-removed mine) 9))))
   '(scheme javascript)
   '("Scheme    " "JavaScript"))
  (let ((grown (filter (lambda (c) (> (change-added c) 0))
                       (changes-in 'javascript changes))))
    (unless (null? grown)
      (say)
      (say "JavaScript files with lines added:")
      (for-each (lambda (c)
                  (say "  +" (change-added c) " -" (change-removed c) "  " (change-path c)))
                grown))))

;; /**
;;  * The arguments given to this program: those after its own file's name.
;;  * @returns {list} The arguments.
;;  */
(define (program-arguments)
  (cdr (find-tail (lambda (argument) (string-suffix? "language_balance.scm" argument))
                  (command-line))))

(let ((arguments (program-arguments)))
  (let ((base (if (pair? arguments) (car arguments) "HEAD")))
    (report base (changes-since base))))
