/**
 * @fileoverview Differential tests for the compiler tier.
 *
 * Every case is evaluated twice -- once interpreted, once with its definitions
 * compiled -- and the two results must agree. The interpreter is the reference
 * semantics, so this is the safety net the compiler is built behind: a case
 * where the two disagree is a compiler bug by definition, and no amount of
 * compiler-only testing would have caught it.
 *
 * Cases the compiler is expected to decline are asserted to be declined *and*
 * to still produce the right answer, because declining has to be safe rather
 * than merely detectable.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { tryCompileDefinition, compileProgram, compileEnvironment } from '../../src/compiler/index.js';
import { installPrebuilt, fingerprintSources } from '../../src/compiler/prebuilt.js';
import PREBUILT, { LIBRARY_FILES } from '../../src/packaging/compiled_stdlib.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { unsafeDefinitions } from '../../src/compiler/safety.js';
import { lowerLambda } from '../../src/compiler/lowering.js';
import { jsName } from '../../src/compiler/emitter.js';
import { Cons } from '../../src/core/interpreter/cons.js';
import { DefineNode } from '../../src/core/interpreter/ast_nodes.js';
import { settle } from '../../src/compiler/runtime.js';

/**
 * Programs whose final expression's value is compared between tiers.
 *
 * Each is a complete program: definitions followed by one expression. The
 * definitions are what get compiled; the trailing expression drives them.
 */
const CASES = [
  // --- arithmetic and recursion ---
  ['fib', '(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 15)'],
  ['tail loop', '(define (loop n acc) (if (< n 1) acc (loop (- n 1) (+ acc n)))) (loop 100 0)'],
  ['mutual tail recursion',
    '(define (ev? n) (if (= n 0) #t (od? (- n 1)))) (define (od? n) (if (= n 0) #f (ev? (- n 1)))) (ev? 101)'],
  ['exact integer growth', '(define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))) (fact 30)'],
  ['mixed exactness', '(define (half x) (/ x 2)) (half 5)'],
  ['nested arithmetic', '(define (f a b c) (+ (* a b) (- c a))) (f 3 4 5)'],

  // --- conditionals and sequencing ---
  ['if in value position', '(define (sign n) (+ 0 (if (< n 0) -1 1))) (sign -5)'],
  ['one-armed if', '(define (maybe n) (if (> n 0) n)) (maybe -1)'],
  ['begin in body', '(define (f x) (begin (+ x 1) (+ x 2))) (f 10)'],
  ['nested if', '(define (classify n) (if (< n 0) (quote neg) (if (= n 0) (quote zero) (quote pos)))) (classify 0)'],

  // --- binding forms ---
  ['let', '(define (f x) (let ((y (* x 2))) (+ x y))) (f 5)'],
  ['let shadowing', '(define (f x) (let ((x (* x 3))) x)) (f 4)'],
  ['nested let', '(define (f a) (let ((b (+ a 1))) (let ((c (+ b 1))) (+ a b c)))) (f 1)'],
  ['named let', '(define (sum n) (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i))))) (sum 10)'],
  ['let*', '(define (f x) (let* ((a (+ x 1)) (b (* a 2))) (+ a b))) (f 3)'],
  ['internal define', '(define (f x) (define y (* x 2)) (+ x y)) (f 6)'],
  ['internal define procedure',
    '(define (f n) (define (double k) (* k 2)) (double (+ n 1))) (f 4)'],
  ['mutually recursive internal defines',
    '(define (f n) (define (a k) (if (= k 0) 1 (b (- k 1)))) (define (b k) (if (= k 0) 0 (a (- k 1)))) (a n)) (f 7)'],

  // --- closures and higher order ---
  ['closure over parameter', '(define (adder n) (lambda (x) (+ x n))) ((adder 3) 4)'],
  ['closure returned from let', '(define (f) (let ((n 10)) (lambda () n))) ((f))'],
  ['procedure as argument', '(define (twice f x) (f (f x))) (twice (lambda (n) (* n n)) 3)'],
  ['deeply nested closures',
    '(define (f a) (lambda (b) (lambda (c) (+ a b c)))) (((f 1) 2) 3)'],

  // --- mutation ---
  ['set! on a local', '(define (f x) (set! x (+ x 1)) x) (f 5)'],
  ['set! on a closed-over local',
    '(define (counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n)))' +
    '(define c (counter)) (c) (c) (c)'],

  // --- rest parameters ---
  ['rest parameter', '(define (f . rest) rest) (f 1 2 3)'],
  ['fixed plus rest', '(define (f a . rest) (cons a rest)) (f 1 2 3)'],
  ['rest parameter empty', '(define (f a . rest) rest) (f 1)'],

  // --- pairs, lists, and other data ---
  ['list building', '(define (upto n) (if (< n 1) (quote ()) (cons n (upto (- n 1))))) (upto 4)'],
  ['list traversal', '(define (len xs) (if (null? xs) 0 (+ 1 (len (cdr xs))))) (len (quote (a b c)))'],
  ['quoted data identity', '(define (f) (quote (1 2 3))) (equal? (f) (f))'],
  ['string and char', '(define (f) (string-length "hello")) (f)'],
  ['vector', '(define (f) (vector-ref (vector 1 2 3) 1)) (f)'],
  ['symbol equality', '(define (f) (eq? (quote abc) (quote abc))) (f)'],
  ['boolean returns', '(define (f n) (> n 3)) (f 10)'],

  // --- the numeric tower through inlined operators ---
  // Arithmetic and pair access are expanded inline with an exact-integer fast
  // path and a fallback to the real primitive. Every case here takes the
  // fallback, so together they check that inlining preserved the tower rather
  // than approximating it.
  ['rational addition', '(define (f a b) (+ a b)) (f 1/3 1/6)'],
  ['rational comparison', '(define (f a b) (< a b)) (f 1/3 1/2)'],
  ['rational equality', '(define (f a b) (= a b)) (f 1/2 2/4)'],
  ['flonum arithmetic', '(define (f a b) (* a b)) (f 1.5 2.0)'],
  ['mixed exact and inexact', '(define (f a b) (+ a b)) (f 1 2.5)'],
  ['mixed comparison', '(define (f a b) (< a b)) (f 1 1.5)'],
  ['exact integer beyond double precision',
    '(define (f a b) (< a b)) (f 10000000000000000000000000001 10000000000000000000000000002)'],
  ['large exact arithmetic', '(define (f a) (* a a)) (f 123456789012345678901234567890)'],
  ['negative exact arithmetic', '(define (f a b) (- a b)) (f -5 7)'],
  ['improper pair via inlined cons', '(define (f a b) (cons a b)) (f 1 2)'],
  ['inlined predicates', '(define (f x) (list (pair? x) (null? x) (not x))) (f (quote ()))'],

  // --- loops: tail calls to the procedure itself compile to JavaScript loops ---
  // Reassigning parameters in place is only right if nothing made in one
  // iteration can see the next one's values. Each case below breaks if it can.
  ['a loop that swaps its arguments',
    '(define (f a b n) (if (= n 0) (list a b) (f b a (- n 1)))) (f 1 2 3)'],
  ['a loop whose arguments read each other',
    '(define (fib-iter a b n) (if (= n 0) a (fib-iter b (+ a b) (- n 1)))) (fib-iter 0 1 60)'],
  ['closures made in a loop keep their own iteration',
    '(define (f) (let loop ((i 0) (fs (quote ())))'
    + ' (if (= i 3) (map (lambda (g) (g)) fs) (loop (+ i 1) (cons (lambda () i) fs))))) (f)'],
  ['an assigned loop parameter is a fresh binding each iteration',
    '(define (f) (let loop ((i 0) (fs (quote ())))'
    + ' (if (= i 3) (map (lambda (g) (g)) fs)'
    + ' (loop (+ i 1) (cons (lambda () (set! i (+ i 10)) i) fs))))) (f)'],
  ['internal definitions are fresh each iteration',
    '(define (f) (let loop ((i 0) (fs (quote ())))'
    + ' (define (get) i) (define (twice) (* 2 (get)))'
    + ' (if (= i 3) (map (lambda (g) (g)) fs) (loop (+ i 1) (cons twice fs))))) (f)'],
  ['a do loop', '(define (f n) (do ((i 0 (+ i 1)) (v (make-vector n 0))) ((= i n) v)'
    + ' (vector-set! v i (* i i)))) (f 5)'],
  ['an internally defined loop',
    '(define (f n) (define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc i)))) (loop n 0)) (f 100)'],
  ['a loop that runs a long time', '(define (count n acc) (if (= n 0) acc (count (- n 1) (+ acc 1))))'
    + ' (count 200000 0)'],
  // The same, where the loop is the whole procedure rather than a named let
  // inlined into one: the fast form boxes its parameters at the top of the
  // loop, not only on entry.
  ['an assigned parameter of a self-looping procedure is fresh each iteration',
    '(define (f i fs) (if (= i 3) (map (lambda (g) (g)) fs)'
    + ' (f (+ i 1) (cons (lambda () (set! i (+ i 10)) i) fs)))) (f 0 (quote ()))'],
  ['a loop that escapes as a value',
    '(define (f) (let loop ((i 0)) (if (< i 3) (loop (+ i 1)) loop))) ((f) 5)'],
  ['a loop entered from inside another loop',
    '(define (f n) (let a ((i 0)) (if (< i n) (a (+ i 1))'
    + ' (let b ((j i) (acc (quote ()))) (if (> j 0) (b (- j 1) (cons j acc)) (list i acc)))))) (f 4)'],
  ['a loop that re-enters the loop it is inside',
    '(define (f n) (let outer ((i 0) (acc (quote ()))) (if (= i n) acc'
    + ' (let inner ((j 0) (acc acc)) (if (= j i) (outer (+ i 1) acc) (inner (+ j 1) (cons (list i j) acc)))))))'
    + ' (f 4)'],
  ['a loop whose value its caller uses',
    '(define (f n) (+ 1 (let loop ((i 0)) (if (< i n) (loop (+ i 1)) i)))) (f 10)'],
  ['a loop name that is reassigned is not looped',
    '(define (f) (let loop ((i 0)) (if (< i 3) (begin (if (= i 1) (set! loop (lambda (j) (quote swapped)))) (loop (+ i 1))) i))) (f)'],

  // --- interaction between tiers ---
  ['compiled calls interpreted',
    '(define (helper x) (apply + (list x x)))' +      // declined: uses apply
    '(define (f x) (+ (helper x) 1)) (f 5)'],
  ['interpreted calls compiled',
    '(define (double x) (* x 2))' +
    '(define (g x) (apply double (list x))) (g 21)'],
  ['deep recursion across tiers',
    '(define (even2? n) (if (= n 0) #t (odd2? (- n 1))))' +
    '(define (odd2? n) (if (= n 0) #f (apply even2? (list (- n 1)))))' +
    '(even2? 20)']
];

/**
 * Programs that use continuations, with the procedures the tier declines.
 *
 * What matters in every case is that the answer is right. The decline lists sit
 * beside it for a narrower reason: they stop a case passing because nothing was
 * compiled, and they record which procedures the default policy holds back.
 *
 * That policy is no longer about correctness. A compiled procedure can be part
 * of a captured continuation now, so any of these could be compiled and still
 * give the right answer -- `CAPTURE_CASES` compiles several of them on purpose.
 * They are declined because a procedure that a capture repeatedly unwinds
 * through is slower compiled than interpreted. `fail` and `enumerate` in the
 * backtracking case are deliberately *not* listed: neither reaches a capture by
 * a route the analysis follows, so both are compiled, and the answer is still
 * right.
 */
const CONTINUATION_CASES = [
  ['escape', '(define (f) (call/cc (lambda (k) (+ 1 (k 42))))) (f)', '42', ['f']],
  ['re-entrant capture',
    '(define saved #f)' +
    '(define counter 0)' +
    '(define (f) (let ((v (call/cc (lambda (k) (set! saved k) 1))))' +
    '  (set! counter (+ counter 1))' +
    '  (if (< counter 3) (saved (+ counter 1)) v)))' +
    '(f)', '3', ['f']],
  ['backtracking through a would-be compiled frame',
    '(define fail (lambda () #f))' +
    '(define (enumerate a b cont)' +
    '  (if (> a b) (fail)' +
    '      (let ((save fail))' +
    '        (set! fail (lambda () (set! fail save) (enumerate (+ a 1) b cont)))' +
    '        (cont a))))' +
    '(define (in-range a b) (call/cc (lambda (cont) (enumerate a b cont))))' +
    '(define (search n) (let* ((x (in-range 0 n)) (y (in-range 0 n)))' +
    '  (if (< (+ x y) (* n 2)) (fail) (cons x y))))' +
    '(search 5)', '(5 . 5)', ['in-range', 'search']],
  ['dynamic-wind', '(define (f) (dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3))) (f)', '2', ['f']],
  // The escape variant, and the one that bites in practice. `caller` never
  // mentions `call/cc`, so a per-procedure rule compiles it happily -- but the
  // escape unwinds past its frame and the escape value becomes *its* result.
  // This is how the `maze` benchmark fails: `dig-maze` quits with `(quit #f)`
  // and compiled `make-maze` returns `#f` instead of the maze.
  ['escape past a would-be compiled frame',
    '(define (escaper n)'
    + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
    + '(define (caller n) (cons (escaper n) (quote (tail))))'
    + '(caller 1)', '(escaped tail)', ['escaper', 'caller']]
];

/**
 * Programs where a continuation is captured while a compiled frame is live.
 *
 * A compiled procedure runs in a JavaScript stack frame, which nothing can read
 * back, so it cannot appear in a continuation the way an interpreted frame
 * does. It puts itself there instead: on learning that a callee is capturing,
 * it saves its locals and where it had got to, and reports the same upward, so
 * every frame between the capture and the interpreter records itself on the way
 * out. These are the shapes that has to handle.
 *
 * Each entry names the definitions to compile; the rest stay interpreted, so
 * the capture crosses the boundary on purpose rather than by accident of a
 * decline rule. Nothing states an expected value: the interpreter runs the same
 * program and its answer is the one that has to be matched, which is the only
 * definition of "right" worth testing against.
 */
const CAPTURE_CASES = [
  // The `maze` shape. `caller` never mentions `call/cc`, and the escape passes
  // through it on the way out -- so it has a `cons` still to do.
  ['an escape past a compiled frame',
    '(define (escaper n)'
    + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
    + '(define (caller n) (cons (escaper n) (quote (tail))))',
    '(caller 1)', ['caller']],

  // The same procedure with nothing to escape: the sentinel must not appear on
  // a path where no capture crosses anything.
  ['the same procedure when nothing escapes',
    '(define (escaper n)'
    + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
    + '(define (caller n) (cons (escaper n) (quote (tail))))',
    '(caller 0)', ['caller']],

  // Several compiled frames between the capture and the interpreter. Each has
  // to record itself, and they have to come back in the right order.
  ['a chain of compiled frames',
    '(define (escaper n)'
    + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
    + '(define (c n) (cons (escaper n) (quote (c))))'
    + '(define (b n) (cons (c n) (quote (b))))'
    + '(define (a n) (cons (b n) (quote (a))))',
    '(a 1)', ['a', 'b', 'c']],

  // A capture inside a compiled loop, re-entered twice. The frame suspends
  // part-way through an iteration, and on each re-entry the resumable form has
  // to finish that iteration and then loop on its own -- with the parameters
  // the suspended iteration had, not the ones the fast form reached later.
  ['a capture inside a local loop, re-entered',
    '(define k #f) (define runs 0)'
    + '(define (grab-at i) (if (= i 2) (call/cc (lambda (c) (set! k c) 0)) 0))'
    + '(define (run n) (let loop ((i 0) (acc (quote ())))'
    + '  (if (= i n) (reverse acc) (loop (+ i 1) (cons (+ i (grab-at i)) acc)))))',
    '(let ((r (run 5))) (set! runs (+ runs 1)) (if (< runs 3) (k (* runs 100)) r))', ['run']],
  ['a capture inside a global self-loop, re-entered',
    '(define k #f) (define runs 0)'
    + '(define (grab-at i) (if (= i 2) (call/cc (lambda (c) (set! k c) 0)) 0))'
    + '(define (run i n acc)'
    + '  (if (= i n) (reverse acc) (run (+ i 1) n (cons (+ i (grab-at i)) acc))))',
    '(let ((r (run 0 5 (quote ())))) (set! runs (+ runs 1)) (if (< runs 3) (k (* runs 100)) r))',
    ['run']],

  // A resumed frame that then loops. After the capture at `i` = 1 is
  // re-entered, the rest of that iteration and every later one run in the
  // resumable form, which has to give an assigned parameter a fresh box on
  // each iteration exactly as the fast form does -- or every closure made
  // after the resume shares one. It also has to keep sharing the resumed
  // iteration's own box: calling the closures set that `i` to 11, so the
  // re-entered iteration continues from 11, which is why the loop stops at
  // `>=` rather than `=`.
  ['a resumed self-loop keeps fresh bindings per iteration',
    '(define k #f) (define runs 0)'
    + '(define (grab-at i) (if (= i 1) (call/cc (lambda (c) (set! k c) 0)) 0))'
    + '(define (run i fs)'
    + '  (if (>= i 4) (map (lambda (g) (g)) (reverse fs))'
    + '      (begin (grab-at i) (run (+ i 1) (cons (lambda () (set! i (+ i 10)) i) fs)))))',
    '(let ((r (run 0 (quote ())))) (set! runs (+ runs 1)) (if (< runs 2) (k 0) r))', ['run']],

  // The call site is inside a nested procedure rather than the one that was
  // named, which is where most call sites in a program actually are.
  ['a call site inside a nested procedure',
    '(define (escaper n)'
    + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
    + '(define (caller n)'
    + '  (let ((f (lambda (m) (cons (escaper m) (quote (inner))))))'
    + '    (cons (f n) (quote (outer)))))',
    '(caller 1)', ['caller']],

  // And inside a branch, where the nested procedure is created conditionally.
  ['a nested procedure created in a branch',
    '(define (escaper n)'
    + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
    + '(define (caller n)'
    + '  (if (> n 0)'
    + '      (let ((g (lambda (x) (cons (escaper x) (quote (g)))))) (cons (g n) (quote (t))))'
    + '      (quote none)))',
    '(caller 1)', ['caller']],

  // A compiled frame that is itself recursive, so several copies of the same
  // procedure are live and each must resume at its own point.
  ['a recursive compiled procedure beneath the capture',
    '(define (escaper n)'
    + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
    + '(define (rec n) (if (= n 0) (escaper 1) (cons n (rec (- n 1)))))',
    '(rec 4)', ['rec']],

  // Capture and resume repeatedly from one compiled loop, which is what a
  // generator does.
  ['a capture on every turn of a compiled loop',
    '(define (escaper n) (call/cc (lambda (quit) (if (> n 2) (quit (quote escaped))) n)))'
    + '(define (caller n)'
    + '  (let loop ((i 0) (acc (quote ())))'
    + '    (if (> i n) acc (loop (+ i 1) (cons (escaper i) acc)))))',
    '(caller 4)', ['caller']],

  // The continuation is invoked more than once, which is the property that
  // rules out the cheaper one-shot designs. The frame has to be *copied* on the
  // way in, or the second invocation sees what the first assigned.
  ['a continuation invoked more than once',
    '(define saved #f)'
    + '(define counter 0)'
    + '(define (capturer) (call/cc (lambda (k) (set! saved k) 1)))'
    + '(define (caller)'
    + '  (let ((v (capturer)))'
    + '    (set! counter (+ counter 1))'
    + '    (if (< counter 3) (saved (+ counter 1)) (cons v (quote (tail))))))',
    '(caller)', ['caller']],

  // Work done *before* the capture must not be done again when the frame is
  // resumed, which is what distinguishes resuming a frame from re-calling it.
  ['work before the capture is not repeated on resume',
    '(define saved #f)'
    + '(define counter 0)'
    + '(define (capturer) (call/cc (lambda (k) (set! saved k) 1)))'
    + '(define (side) (set! counter (+ counter 1)) counter)'
    + '(define (caller)'
    + '  (let ((before (side)))'
    + '    (let ((v (capturer)))'
    + '      (if (< v 3) (saved (+ v 1)) (list before v counter)))))',
    '(caller)', ['caller']],

  // A suspended frame saves only what is live after the call it stopped at.
  // Each case below is built so that dropping one needed variable makes the
  // resumed continuation read `undefined` -- a wrong answer or a throw, never a
  // pass -- and each is resumed more than once, so the frame is reused.

  // A temporary, not a variable: `(one)` has already been evaluated and its
  // result sits in a JavaScript temporary while `(capturer)` runs. Nothing in
  // the source names it, so an analysis over Scheme variables alone would
  // miss it.
  ['liveness: a partly evaluated argument list survives the capture',
    '(define saved #f)'
    + '(define n 0)'
    + '(define (capturer) (call/cc (lambda (k) (set! saved k) 1)))'
    + '(define (one) 10)'
    + '(define (caller)'
    + '  (let ((r (list (one) (capturer) (+ n 100))))'
    + '    (set! n (+ n 1))'
    + '    (if (< n 3) (saved (* n 5)) r)))',
    '(caller)', ['caller']],

  // Live on only one path. Which of `x` and `y` is read after the capture
  // depends on `flag`, so both have to be kept: liveness is the union over the
  // branches that can follow, not the branch that happened to run.
  ['liveness: a variable read on only one later branch is kept',
    '(define saved #f)'
    + '(define n 0)'
    + '(define (capturer) (call/cc (lambda (k) (set! saved k) 1)))'
    + '(define (caller flag x y)'
    + '  (let ((v (capturer)))'
    + '    (set! n (+ n 1))'
    + '    (if (< n 3) (saved (+ v 1))'
    + '        (if flag (list (quote x) x v) (list (quote y) y v)))))',
    '(caller #f (quote xx) (quote yy))', ['caller']],

  // An assigned local is held in a box, and assigning it *reads* the box
  // reference -- it is not a fresh definition. Treating `(set! acc ...)` as one
  // would drop the box from the frame, and the resumed code would index into
  // `undefined`. The accumulation also shows the box is shared across every
  // resumption, as Scheme requires.
  ['liveness: an assigned local is kept across the capture',
    '(define saved #f)'
    + '(define n 0)'
    + '(define (capturer) (call/cc (lambda (k) (set! saved k) 1)))'
    + '(define (caller)'
    + '  (let ((acc (quote ())))'
    + '    (let ((v (capturer)))'
    + '      (set! acc (cons v acc))'
    + '      (set! n (+ n 1))'
    + '      (if (< n 3) (saved (+ v 10)) acc))))',
    '(caller)', ['caller']],

  // The same trap in its sharpest form. Above, `acc` is also *read* on the
  // right of its own `set!`, which keeps it live however the write is judged.
  // Here the first mention after the capture is a pure write, so the only thing
  // keeping the box in the frame is knowing that writing through it reads it.
  ['liveness: an assigned local whose next mention is a pure write',
    '(define saved #f)'
    + '(define n 0)'
    + '(define (capturer) (call/cc (lambda (k) (set! saved k) 1)))'
    + '(define (caller)'
    + '  (let ((last 0))'
    + '    (let ((v (capturer)))'
    + '      (set! last v)'
    + '      (set! n (+ n 1))'
    + '      (if (< n 3) (saved (+ v 10)) last))))',
    '(caller)', ['caller']],

  // A procedure made after the capture closes over `x`. Its free variables are
  // handed to it when it is created, so creating it is a read of `x`, and `x`
  // must be in the frame even though no ordinary expression mentions it later.
  ['liveness: a variable captured by a later closure is kept',
    '(define saved #f)'
    + '(define n 0)'
    + '(define (capturer) (call/cc (lambda (k) (set! saved k) 1)))'
    + '(define (caller x)'
    + '  (let ((v (capturer)))'
    + '    (set! n (+ n 1))'
    + '    (let ((g (lambda () (list x v))))'
    + '      (if (< n 3) (saved (+ v 1)) (g)))))',
    '(caller (quote kept))', ['caller']],

  // Two capture points with different live sets in one procedure: what is
  // live after the first is not what is live after the second, and resuming at
  // either must find exactly what it needs.
  ['liveness: two capture points with different live sets',
    '(define saved #f)'
    + '(define n 0)'
    + '(define (capturer) (call/cc (lambda (k) (set! saved k) 1)))'
    + '(define (caller a b)'
    + '  (let ((p (capturer)))'
    + '    (let ((q (+ p a)))'
    + '      (let ((r (capturer)))'
    + '        (set! n (+ n 1))'
    + '        (if (< n 4) (saved (+ r n)) (list q r b))))))',
    '(caller 100 (quote bee))', ['caller']]
];

/**
 * Cases the compiler must decline, with the reason it should give. Declining
 * is a feature, so it is tested like one.
 */
const MUST_DECLINE = [
  // Declined because compiling it is *slower*, not because it cannot be done.
  // The capability is exercised below, with the default lifted.
  ['call/cc', '(define (f) (call/cc (lambda (k) (k 1))))', 'captures a continuation'],
  ['dynamic-wind', '(define (f) (dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3)))', 'dynamic-wind'],
  // `call-with-values` is compiled when it is *called* directly, by rewriting
  // it away; a reference by any other route still has to decline, because the
  // primitive itself returns something only the interpreter can continue.
  ['call-with-values by reference',
    '(define (f g) (g call-with-values))', 'call-with-values']
];

/**
 * Programs using multiple values, which the compiler handles.
 *
 * `values` only builds an object and returns it, so it never needed to be
 * declined. `call-with-values` genuinely did: the primitive hands the
 * interpreter an expression to evaluate, and compiled code has no evaluator.
 * Making it a plain procedure that calls the producer itself would not work
 * either, since the pending consumer application would then sit in a
 * JavaScript frame that a captured continuation could not restore.
 *
 * So a direct call is rewritten during lowering into
 * `(apply consumer (%values->list (producer)))` -- every part of which the
 * compiler already emits, including the producer call site that gives a capture
 * somewhere to resume.
 *
 * These were the last declines in the benchmark corpus outside `call/cc`, and
 * they were concentrated in the two bignum programs.
 */
const VALUES_CASES = [
  ['two values into a variadic consumer',
    '(define (f) (call-with-values (lambda () (values 1 2)) +))', '(f)', '3'],
  ['two values into a fixed-arity consumer',
    '(define (f) (call-with-values (lambda () (values 1 2)) (lambda (a b) (cons a b))))',
    '(f)', '(1 . 2)'],
  ['three values into a rest parameter',
    '(define (f) (call-with-values (lambda () (values 1 2 3)) list))', '(f)', '(1 2 3)'],
  ['a single value is one value',
    '(define (f) (call-with-values (lambda () 5) (lambda (x) x)))', '(f)', '5'],
  ['not in tail position',
    '(define (f) (cons (call-with-values (lambda () (values 1 2)) +) (quote (tail))))',
    '(f)', '(3 tail)'],
  ['the producer is computed',
    '(define (p) (values 4 5))'
    + '(define (f) (call-with-values p +))', '(f)', '9'],
  ['the consumer is computed',
    '(define (c a b) (* a b))'
    + '(define (f) (call-with-values (lambda () (values 6 7)) c))', '(f)', '42'],
  ['values flowing out of a compiled procedure',
    '(define (f) (values 1 2))', '(call-with-values f +)', '3'],
  ['nested call-with-values',
    '(define (f) (call-with-values (lambda () '
    + '(call-with-values (lambda () (values 1 2)) (lambda (a b) (values (+ a b) 10)))) *))',
    '(f)', '30'],
  // The operands are evaluated left to right by the interpreter. R7RS leaves
  // the order unspecified, so this pins the two tiers to each other rather
  // than to the standard.
  ['operands are evaluated left to right',
    '(define order (quote ()))'
    + '(define (note mark v) (set! order (cons mark order)) v)'
    + '(define (f) (call-with-values (note 1 (lambda () (values 1 2))) (note 2 +)))',
    '(begin (f) order)', '(2 1)'],
  // A local of the same name is not the primitive, so the rewrite must not fire.
  ['shadowed by a local binding',
    '(define (f call-with-values) (call-with-values 7))', '(f (lambda (x) (* x 3)))', '21']
];

/**
 * Programs using `apply`, which the compiler does handle.
 *
 * `apply` looks like it transfers control -- it returns a `TailCall` rather
 * than a value -- and it was declined for that reason. It does not: it calls an
 * ordinary procedure with ordinary arguments, and a compiled trampoline can
 * continue that itself. What blocked it was the *shape* of the `TailCall`,
 * which carried an expression for the interpreter to evaluate rather than
 * naming the procedure.
 *
 * It was worth finding. `apply` was the largest single cause of declined
 * procedures across the canonical benchmarks, mostly reached indirectly through
 * `map` and `for-each`, which use it for their variadic case -- so declining it
 * declined most of the standard library and everything that called it.
 */
const APPLY_CASES = [
  ['a list of arguments', '(define (f xs) (apply + xs))', '(f (list 1 2 3))', '6'],
  ['fixed arguments before the list', '(define (f a xs) (apply + a xs))', '(f 1 (list 2 3))', '6'],
  ['an empty argument list', '(define (f) (apply + (quote ())))', '(f)', '0'],
  ['applying a Scheme procedure', '(define (g a b) (cons a b))'
    + '(define (f xs) (apply g xs))', '(f (list 1 2))', '(1 . 2)'],
  ['in tail position', '(define (g . xs) xs)'
    + '(define (f xs) (apply g xs))', '(f (list 1 2 3))', '(1 2 3)'],
  ['not in tail position', '(define (f xs) (cons (apply + xs) (quote (tail))))',
    '(f (list 1 2))', '(3 tail)'],
  ['applying a procedure that was passed in',
    '(define (f p xs) (apply p xs))', '(f + (list 4 5))', '9']
];

/**
 * Cases exercising the **compiled-to-interpreted boundary**, with only the
 * named procedures compiled.
 *
 * Three existing cases in `CASES` are called "compiled calls interpreted" and
 * friends, and every one of them compiles *nothing*: each forces its callee to
 * stay interpreted by writing it with `apply`, which trips the unit-level
 * continuation guard and declines the whole unit. They compare the interpreter
 * against itself. That is half of why the boundary-conversion bug below went
 * unnoticed for three rounds of benchmarking.
 *
 * The other half is that `render` shows a `BigInt` and a JavaScript number
 * identically, so a result silently converted from exact to inexact still
 * matched. These cases therefore ask **Scheme** about the value -- `exact?`,
 * `eqv?`, `pair?` -- rather than comparing rendered text.
 *
 * Each entry names the procedures to compile, leaving the rest interpreted, so
 * the boundary is crossed on purpose rather than by accident of a decline rule.
 */
const BOUNDARY_CASES = [
  // Tail position: already correct, because a compiled tail call returns the
  // interpreter's own `TailCall` and the interpreter applies the callee through
  // its own environment-extending path, which converts nothing. Kept so that a
  // future change to the tail path cannot regress silently.
  ['exactness survives a tail call into interpreted code',
    '(define (ident x) x) (define (f x) (ident x))', ['f'],
    '(exact? (f 33))', '#t'],
  ['the value itself is unchanged, exactness included',
    '(define (ident x) x) (define (f x) (ident x))', ['f'],
    '(eqv? (f 33) 33)', '#t'],

  // Non-tail position: this is the broken path. `(cons ... (quote ()))` is the
  // smallest wrapper that puts the call in a value position without routing it
  // through an inlined arithmetic operator.
  ['exactness survives a non-tail call into interpreted code',
    '(define (ident x) x) (define (f x) (cons (ident x) (quote ())))', ['f'],
    '(exact? (car (f 33)))', '#t'],
  ['a bignum beyond double precision survives a non-tail call',
    '(define (ident x) x) (define (f x) (cons (ident x) (quote ())))', ['f'],
    '(= (car (f 314159265358979323846264338327950288419716939937453))'
    + ' 314159265358979323846264338327950288419716939937453)', '#t'],
  ['a pair returned from a non-tail call is still a pair',
    '(define (mk) (cons 1 2)) (define (f) (cons (mk) (quote ())))', ['f'],
    '(pair? (car (f)))', '#t'],
  ['a list returned from a non-tail call is still a list',
    '(define (mk) (list 1 2 3)) (define (f) (cons (mk) (quote ())))', ['f'],
    '(equal? (car (f)) (list 1 2 3))', '#t'],
  ['a symbol survives a non-tail call',
    '(define (ident x) x) (define (f x) (cons (ident x) (quote ())))', ['f'],
    '(eq? (car (f (quote abc))) (quote abc))', '#t'],
  ['a pair passed into interpreted code survives a non-tail call',
    '(define (second p) (car (cdr p))) (define (f p) (cons (second p) (quote ())))', ['f'],
    '(exact? (car (f (list 1 2 3))))', '#t'],
  ['an interpreted procedure reached through a vector, as the benchmarks\' hide does',
    '(define (ident x) x)'
    + '(define (pick v i) (vector-ref v i))'
    + '(define (f x) (cons ((pick (vector ident) 0) x) (quote ())))', ['f', 'pick'],
    '(exact? (car (f 33)))', '#t'],
  ['two non-tail boundary crossings in a row',
    '(define (ident x) x)'
    + '(define (mid x) (ident x))'
    + '(define (f x) (cons (cons (mid x) (quote ())) (quote ())))', ['f'],
    '(exact? (car (car (f 33))))', '#t'],
  ['a compiled callee reached by a non-tail call keeps exactness',
    '(define (ident x) x)'
    + '(define (inner x) (ident x))'
    + '(define (f x) (cons (inner x) (quote ())))', ['f', 'inner'],
    '(exact? (car (f 33)))', '#t']
];

/**
 * Bootstraps a fresh interpreter with the standard library loaded.
 * @param {Object} template - An already-bootstrapped interpreter to copy from.
 * @returns {{interpreter: Object, env: Object}} A fresh pair.
 */
function freshEnvironment(template) {
  // A fresh global environment per case keeps definitions from one case out of
  // the next, which matters because both tiers define into the same namespace.
  const { interpreter, env } = createInterpreter();
  for (const [name, value] of template.bindings) {
    if (!env.bindings.has(name)) env.define(name, value);
  }
  return { interpreter, env };
}

/**
 * Bootstraps a fresh interpreter with the standard library *interpreted*.
 *
 * `freshEnvironment` copies the suite's own bindings, and when the suite runs
 * with the library already compiled those are compiled procedures -- so there
 * would be no interpreted closures for the prebuilt table to replace, and
 * these cases would silently assert nothing. Loading the sources here makes the
 * precondition the test's own rather than the runner's.
 *
 * @returns {{interpreter: Object, env: Object}} A fresh pair.
 */
function interpretedLibrary() {
  const { interpreter, env } = createInterpreter();
  for (const file of LIBRARY_FILES) {
    for (const form of parse(BUNDLED_SOURCES[file])) {
      interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    }
  }
  return { interpreter, env };
}

/**
 * Evaluates a program, optionally compiling its definitions first.
 * @param {string} source - Scheme source.
 * @param {Object} template - Environment supplying the standard library.
 * @param {boolean} useCompiler - Whether to compile definitions.
 * @returns {{value: *, compiled: Array<string>, declined: Array<Object>}} Result.
 */
function evaluate(source, template, useCompiler) {
  const { interpreter, env } = freshEnvironment(template);
  const asts = parse(source).map((form) => analyze(form));

  if (!useCompiler) {
    let value;
    for (const ast of asts) {
      value = interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
    }
    return { value, compiled: [], declined: [] };
  }

  // Definitions are compiled; the trailing expressions are run by the
  // interpreter, which is what a real program does too.
  const definitions = asts.filter((a) => a instanceof DefineNode);
  const rest = asts.filter((a) => !(a instanceof DefineNode));
  const outcome = compileProgram(definitions, env, interpreter);

  let value;
  for (const ast of rest) {
    value = settle(interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' }));
  }
  return { value, ...outcome };
}

/**
 * Evaluates a program with only the named definitions compiled.
 *
 * Models mixed-tier execution directly, rather than relying on a decline rule
 * to leave some procedure interpreted. `tryCompileDefinition` is used because
 * it compiles a single definition on request; none of these cases capture a
 * continuation, so its lack of a unit-level guard does not apply here.
 *
 * @param {string} source - Definitions.
 * @param {string} probe - A trailing expression, evaluated interpreted.
 * @param {Object} template - Environment supplying the standard library.
 * @param {Array<string>} names - Definitions to compile, by source name.
 * @param {Object} [options] - Passed through to `tryCompileDefinition`.
 * @returns {{value: *, compiled: number}} The probe's value and how many
 *   definitions were compiled.
 */
function evaluateSelective(source, probe, template, names, options = {}) {
  const { interpreter, env } = freshEnvironment(template);
  const wanted = new Set(names);
  let compiled = 0;

  for (const form of parse(source)) {
    const ast = analyze(form);
    if (ast instanceof DefineNode && wanted.has(ast.originalName || ast.name)) {
      const result = tryCompileDefinition(ast, env, options);
      if (result.compiled) {
        env.define(result.name, result.procedure);
        compiled++;
        continue;
      }
    }
    interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
  }

  const value = settle(interpreter.run(
    analyze(parse(probe)[0]), env, [], undefined, { jsAutoConvert: 'raw' }));
  return { value, compiled };
}

/**
 * Renders a Scheme value for comparison between tiers.
 * @param {*} value - A Scheme value.
 * @returns {string} A stable textual form.
 */
function render(value) {
  if (value === null) return '()';
  if (value === true) return '#t';
  if (value === false) return '#f';
  if (value === undefined) return '#<unspecified>';
  if (typeof value === 'bigint') return value.toString();
  if (typeof value === 'string') return JSON.stringify(value);
  if (typeof value === 'function') return '#<procedure>';
  if (value && typeof value === 'object') {
    if ('car' in value && 'cdr' in value) {
      const parts = [];
      let cursor = value;
      while (cursor && typeof cursor === 'object' && 'car' in cursor) {
        parts.push(render(cursor.car));
        cursor = cursor.cdr;
      }
      return cursor === null ? `(${parts.join(' ')})` : `(${parts.join(' ')} . ${render(cursor)})`;
    }
    if (Array.isArray(value)) return `#(${value.map(render).join(' ')})`;
    if (typeof value.name === 'string') return value.name;
  }
  return String(value);
}

/**
 * Runs the compiler differential tests.
 * @param {Object} interpreter - A bootstrapped interpreter (supplies the stdlib).
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runCompilerTests(interpreter, logger) {
  logger.title('Compiler - Differential Against Interpreter');

  const template = interpreter.globalEnv;
  let compiledCount = 0;

  for (const [name, source] of CASES) {
    let interpreted;
    let compiledResult;
    try {
      interpreted = evaluate(source, template, false);
    } catch (e) {
      logger.fail(`${name}: interpreted evaluation threw: ${e.message}`);
      continue;
    }
    try {
      compiledResult = evaluate(source, template, true);
    } catch (e) {
      logger.fail(`${name}: compiled evaluation threw: ${e.message}`);
      continue;
    }

    compiledCount += compiledResult.compiled.length;
    assert(logger, `${name} agrees between tiers`,
      render(compiledResult.value), render(interpreted.value));
  }

  logger.title('Compiler - Declines Unsupported Forms Safely');

  for (const [name, source, expected] of MUST_DECLINE) {
    const { declined } = evaluate(`${source} 1`, template, true);
    const mentions = declined.some((d) => d.reason.includes(expected));
    assert(logger, `declines ${name}`, mentions, true);
  }

  logger.title('Compiler - Procedures a Capture Unwinds Through Are Declined');

  for (const [name, source, expected, mustDecline] of CONTINUATION_CASES) {
    let outcome;
    try {
      outcome = evaluate(source, template, true);
    } catch (e) {
      logger.fail(`${name}: threw ${e.message}`);
      continue;
    }
    for (const required of mustDecline) {
      assert(logger, `${name} declines ${required}`,
        outcome.compiled.includes(required), false);
    }
    assert(logger, `${name} still produces the right answer`, render(outcome.value), expected);
  }

  logger.title('Compiler - The Standard Library Is Compiled at Build Time');

  // Compiling the library at startup costs about 12 ms and needs
  // `new Function`, which a strict Content-Security-Policy forbids. The build
  // generates the same code into a module instead, so installing it is a
  // property assignment and a page with a strict policy gets the compiled
  // library rather than an interpreted one.
  {
    const sources = LIBRARY_FILES.map((file) => BUNDLED_SOURCES[file]);
    const fingerprint = fingerprintSources(sources);

    // The build runs as part of `npm test`, so the generated table must match
    // the sources in the same tree. If this fails, the two are out of step and
    // every assertion below would be testing a stale artifact.
    assert(logger, 'the generated table matches the library sources',
      PREBUILT.fingerprint, fingerprint);

    {
      const { interpreter: fresh, env } = interpretedLibrary();
      const outcome = installPrebuilt(env, PREBUILT, fingerprint);
      assert(logger, 'prebuilt procedures install', outcome.installed.length > 20, true);
      assert(logger, 'and none are skipped', outcome.skipped.length, 0);

      const evalIn = (code) => render(settle(fresh.run(
        analyze(parse(code)[0]), env, [], undefined, { jsAutoConvert: 'raw' })));
      assert(logger, 'they are marked as compiled', env.lookup('map').$compiled, true);
      assert(logger, 'map over one list', evalIn('(map (lambda (x) (* x x)) (quote (1 2 3)))'), '(1 4 9)');
      assert(logger, 'map over two lists', evalIn('(map + (quote (1 2)) (quote (10 20)))'), '(11 22)');
      assert(logger, 'assoc finds a pair', evalIn('(assoc 2 (quote ((1 a) (2 b))))'), '(2 b)');
      assert(logger, 'member uses equal?', evalIn('(member (quote (a)) (quote ((b) (a))))'), '((a))');
      assert(logger, 'max is variadic', evalIn('(max 3 1 4 1 5)'), '5');
      // A capture crossing the prebuilt library, which is what the resumable
      // forms in the generated module are for.
      assert(logger, 'call/cc escapes through prebuilt library code',
        evalIn('(call/cc (lambda (k) (map (lambda (x) (if (> x 2) (k (quote out)) x)) (quote (1 2 3)))))'),
        'out');
    }

    // This is the point of moving it to build time: installing generated code
    // is not generating code, so a policy that forbids the latter does not stop
    // the library being compiled.
    {
      const { interpreter: fresh, env } = interpretedLibrary();
      const realFunction = globalThis.Function;
      let outcome;
      let runtime;
      try {
        globalThis.Function = function () { throw new Error('unsafe-eval is not allowed'); };
        outcome = installPrebuilt(env, PREBUILT, fingerprint);
        runtime = compileEnvironment(env);
      } finally {
        globalThis.Function = realFunction;
      }
      assert(logger, 'prebuilt code installs where code generation is forbidden',
        outcome.installed.length > 20, true);
      assert(logger, 'while compiling at run time reports itself unavailable',
        typeof runtime.unavailable, 'string');
      assert(logger, 'and the library still works',
        render(settle(fresh.run(analyze(parse('(map (lambda (x) (+ x 1)) (quote (1 2)))')[0]),
          env, [], undefined, { jsAutoConvert: 'raw' }))), '(2 3)');
    }

    // A stale build is the failure that would matter: prebuilt code quietly
    // doing what an older version of the source said. Nothing installs.
    {
      const { interpreter: fresh, env } = interpretedLibrary();
      const outcome = installPrebuilt(env, PREBUILT, 'deadbeef');
      assert(logger, 'a fingerprint mismatch installs nothing', outcome.installed.length, 0);
      assert(logger, 'and says the table is stale', outcome.stale, true);
      assert(logger, 'leaving the library interpreted and correct',
        render(settle(fresh.run(analyze(parse('(map (lambda (x) (+ x 1)) (quote (1 2)))')[0]),
          env, [], undefined, { jsAutoConvert: 'raw' }))), '(2 3)');
    }

    // A procedure whose arity no longer matches the generated code is skipped
    // rather than installed, since the code was generated for a different
    // signature.
    {
      const { env } = interpretedLibrary();
      const doctored = {
        fingerprint: PREBUILT.fingerprint,
        files: PREBUILT.files,
        procedures: {
          map: { ...PREBUILT.procedures.map, params: ['only-one'], rest: null }
        }
      };
      const outcome = installPrebuilt(env, doctored, fingerprint);
      assert(logger, 'a changed arity is skipped', outcome.installed.length, 0);
      assert(logger, 'with a reason naming the arity',
        /arity differs/.test(outcome.skipped[0].reason), true);
    }

    // Names are deliberately *not* compared. Renaming comes from a counter that
    // advances as the analyzer works, so a second interpreter in the same
    // process gets different names for identical source -- and comparing them
    // would silently lose every prebuilt procedure. This asserts that a second
    // bootstrap still installs, which is how that was found.
    {
      const { env: second } = interpretedLibrary();
      const { env: third } = interpretedLibrary();
      installPrebuilt(second, PREBUILT, fingerprint);
      const outcome = installPrebuilt(third, PREBUILT, fingerprint);
      assert(logger, 'a later bootstrap installs just as well',
        outcome.installed.length > 20, true);
    }
  }

  logger.title('Compiler - Compiling the Standard Library In Place');

  // The library is Scheme, so `map`, `assq` and `member` are interpreted
  // closures until something compiles them, and compiled code calling one
  // crosses into the interpreter on its hottest path. `compileEnvironment`
  // replaces them where they already sit, which is the only way to reach them:
  // they exist as values by the time anything considers compiling them.
  {
    const { interpreter: fresh, env } = freshEnvironment(template);
    const outcome = compileEnvironment(env);

    // Asserted about the resulting state rather than about what this call did,
    // because the suite also runs with the library already compiled during
    // bootstrap -- in which case there is nothing left for this call to do and
    // a count would be zero while everything is, correctly, compiled.
    const isCompiled = (name) => env.lookup(name).$compiled === true;
    assert(logger, 'the library is compiled afterwards',
      ['map', 'for-each', 'assoc', 'member', 'equal?', 'reverse'].every(isCompiled), true);
    assert(logger, 'and code generation is available here', outcome.unavailable, undefined);

    // Compiling them is worth nothing if they stop working. These exercise the
    // ones with a variadic path through `apply`, the ones that take a procedure
    // and call it, and equality, which recurses.
    const evalIn = (code) => render(settle(fresh.run(
      analyze(parse(code)[0]), env, [], undefined, { jsAutoConvert: 'raw' })));

    assert(logger, 'map over one list', evalIn('(map (lambda (x) (* x x)) (quote (1 2 3)))'), '(1 4 9)');
    assert(logger, 'map over two lists', evalIn('(map + (quote (1 2)) (quote (10 20)))'), '(11 22)');
    assert(logger, 'for-each still sequences', evalIn(
      '(let ((acc (quote ()))) (for-each (lambda (x) (set! acc (cons x acc))) (quote (1 2 3))) acc)'),
      '(3 2 1)');
    assert(logger, 'assoc finds a pair', evalIn('(assoc 2 (quote ((1 a) (2 b))))'), '(2 b)');
    assert(logger, 'member uses equal?', evalIn('(member (quote (a)) (quote ((b) (a))))'), '((a))');
    assert(logger, 'equal? recurses', evalIn('(equal? (quote (1 (2 3))) (quote (1 (2 3))))'), '#t');
    assert(logger, 'max is variadic', evalIn('(max 3 1 4 1 5)'), '5');
    assert(logger, 'reverse and length', evalIn('(cons (length (quote (1 2 3))) (reverse (quote (1 2))))'), '(3 2 1)');

    // A capture crossing the compiled library, which is the case the whole
    // capture protocol exists for and which the library now sits inside.
    assert(logger, 'call/cc escapes through compiled library code',
      evalIn('(call/cc (lambda (k) (map (lambda (x) (if (> x 2) (k (quote out)) x)) (quote (1 2 3)))))'),
      'out');
  }

  // Code generation needs `new Function`, which a strict Content-Security-Policy
  // forbids. That has to degrade to "everything stays interpreted" rather than
  // failing, because the interpreter is a permanent tier and a browser page is
  // a supported deployment.
  {
    const { interpreter: fresh, env } = freshEnvironment(template);
    const realFunction = globalThis.Function;
    let outcome;
    try {
      globalThis.Function = function () { throw new Error('unsafe-eval is not allowed'); };
      outcome = compileEnvironment(env);
    } finally {
      globalThis.Function = realFunction;
    }

    assert(logger, 'a policy forbidding code generation is reported, not thrown',
      typeof outcome.unavailable, 'string');
    assert(logger, 'and nothing is compiled', outcome.compiled.length, 0);
    // The library must be untouched and still working.
    assert(logger, 'while the library still runs interpreted',
      render(settle(fresh.run(analyze(parse('(map (lambda (x) (+ x 1)) (quote (1 2)))')[0]),
        env, [], undefined, { jsAutoConvert: 'raw' }))), '(2 3)');
  }

  logger.title('Compiler - call/cc Compiles, and Is Declined Anyway');

  // A capture is emitted as a call site that suspends: the procedure records
  // what the capture needs, spills its locals and reports the unwind outward.
  // That is the protocol a capture made by an interpreted callee already uses,
  // entered from this end rather than beneath. The captured value arrives at
  // the resume point instead of from the call, so everything after the capture
  // is emitted once and serves every invocation of the continuation.
  //
  // It is nonetheless off by default, because it is slower: `btsearch` goes
  // from 2.00x faster to 2x slower and `ctak` from 0.99x to 0.69x, against
  // `contfib` 1.03x to 1.92x the other way. These cases lift the default so the
  // capability is tested rather than merely claimed.
  {
    const captureCases = [
      ['an escape', '(define (f n) (+ 1 (call/cc (lambda (k) (if (> n 0) (k 41)) 0))))',
        '(f 1)', '42'],
      ['no escape taken', '(define (f n) (+ 1 (call/cc (lambda (k) (if (> n 0) (k 41)) 0))))',
        '(f 0)', '1'],
      ['a capture in tail position', '(define (f n) (call/cc (lambda (k) (k n))))',
        '(f 7)', '7'],
      ['work after the capture',
        '(define (f n) (cons (call/cc (lambda (k) (if (> n 0) (k (quote esc))) (quote norm)))'
        + ' (quote (tail))))', '(f 1)', '(esc tail)'],
      ['the long spelling',
        '(define (f n) (call-with-current-continuation (lambda (k) (k (* n 2)))))',
        '(f 21)', '42'],
      ['a recursive procedure capturing at each level',
        '(define (f n) (if (= n 0) 0 (+ n (call/cc (lambda (k) (k (f (- n 1))))))))',
        '(f 4)', '10'],
      // The shape the whole protocol exists for: several captures live at once,
      // each resumed in its own frame.
      ['the ctak shape',
        '(define (ctak-aux k x y z)'
        + '  (if (not (< y x)) (k z)'
        + '      (ctak-aux k'
        + '        (call/cc (lambda (k) (ctak-aux k (- x 1) y z)))'
        + '        (call/cc (lambda (k) (ctak-aux k (- y 1) z x)))'
        + '        (call/cc (lambda (k) (ctak-aux k (- z 1) x y))))))'
        + '(define (f x y z) (call/cc (lambda (k) (ctak-aux k x y z))))',
        '(f 6 4 2)', '3']
    ];

    for (const [name, source, probe, expected] of captureCases) {
      const interpretedValue = render(evaluate(`${source} ${probe}`, template, false).value);
      let got;
      let compiled = 0;
      try {
        const outcome = evaluateSelective(
          source, probe, template, ['f', 'ctak-aux'], { allowCaptures: true });
        got = render(outcome.value);
        compiled = outcome.compiled;
      } catch (e) {
        got = `threw: ${e.message}`;
      }
      assert(logger, `${name}: the capturing procedure is compiled`, compiled > 0, true);
      assert(logger, `${name}: matches the interpreter`, got, interpretedValue);
      assert(logger, `${name}: and the answer is right`, got, expected);
    }
  }

  logger.title('Compiler - An Assigned Local Is Shared, Not Copied');

  // Spilling a frame copies each local's value, and for a variable the program
  // can name that is wrong. Scheme shares the binding: an assignment made after
  // a continuation is captured is visible when that continuation is invoked
  // again, and to any closure over the same variable. So an assigned local is
  // held in a one-element array, and the frame copies the array's reference.
  //
  // Without that, this answered `(1 1 1)` where the interpreter answers
  // `(3 2 1)`, because each resume restored a copy of `n`. The `threads`
  // benchmark has the same shape in a counter shared between a scheduler and
  // the threads it runs, and returned a wrong total.
  {
    const source =
      '(define saved #f)'
      + '(define results (quote ()))'
      + '(define tries 0)'
      // The capture is *beneath* `f` rather than in it, so `f` compiles under
      // the default policy -- which is what makes this the reachable shape.
      + '(define (capturer) (call/cc (lambda (k) (set! saved k) 0)))'
      + '(define (f)'
      + '  (let ((n 0))'
      + '    (capturer)'
      + '    (set! n (+ n 1))'
      + '    n))'
      + '(define (go)'
      + '  (set! results (cons (f) results))'
      + '  (set! tries (+ tries 1))'
      + '  (if (< tries 3) (saved 0) results))';

    const outcome = evaluateSelective(source, '(go)', template, ['f']);
    assert(logger, 'the procedure with the assigned local is compiled',
      outcome.compiled, 1);
    assert(logger, 'and an assignment after the capture survives re-invocation',
      render(outcome.value), '(3 2 1)');

    // The box has to be visible to a closure too, which is the other way one
    // binding can be observed from two places.
    const shared =
      '(define saved #f)'
      + '(define tries 0)'
      + '(define (capturer) (call/cc (lambda (k) (set! saved k) 0)))'
      + '(define (f)'
      + '  (let ((n 0))'
      + '    (let ((bump (lambda () (set! n (+ n 1)))))'
      + '      (capturer)'
      + '      (bump)'
      + '      n)))'
      // The counter has to advance *after* the capture point, or resuming
      // would never reach it and the loop would not terminate.
      + '(define (go)'
      + '  (let ((v (f)))'
      + '    (set! tries (+ tries 1))'
      + '    (if (< tries 3) (saved 0) v)))';
    const sharedOutcome = evaluateSelective(shared, '(go)', template, ['f']);
    assert(logger, 'a closure over the assigned local sees the same binding',
      render(sharedOutcome.value),
      render(evaluate(`${shared} (go)`, template, false).value));

    // Boxing is only for assigned locals: an unassigned one cannot tell a copy
    // from the original, so it stays a plain variable.
    const plain = '(define (g n) (let ((acc (* n 2))) (+ acc 1)))';
    const plainOutcome = evaluateSelective(plain, '(g 20)', template, ['g']);
    assert(logger, 'an unassigned local still compiles', plainOutcome.compiled, 1);
    assert(logger, 'and computes correctly', render(plainOutcome.value), '41');
  }

  logger.title('Compiler - Multiple Values Are Compiled');

  for (const [name, source, probe, expected] of VALUES_CASES) {
    const interpretedValue = render(evaluate(`${source} ${probe}`, template, false).value);
    let got;
    try {
      got = render(evaluateSelective(source, probe, template, ['f']).value);
    } catch (e) {
      got = `threw: ${e.message}`;
    }
    assert(logger, `${name}: matches the interpreter`, got, interpretedValue);
    assert(logger, `${name}: and the answer is right`, got, expected);
  }

  // Compiling it is the point, so assert that it happened rather than trusting
  // that a matching answer means the compiled path ran.
  {
    const { compiled } = evaluateSelective(
      '(define (f) (call-with-values (lambda () (values 1 2)) +))', '(f)', template, ['f']);
    assert(logger, 'a procedure using call-with-values is compiled', compiled, 1);
  }

  logger.title('Compiler - apply Is Compiled, Not Declined');

  for (const [name, source, probe, expected] of APPLY_CASES) {
    const interpretedValue = render(evaluate(`${source} ${probe}`, template, false).value);
    let got;
    let compiledCount = 0;
    try {
      const outcome = evaluateSelective(source, probe, template, ['f']);
      got = render(outcome.value);
      compiledCount = outcome.compiled;
    } catch (e) {
      got = `threw: ${e.message}`;
    }
    assert(logger, `${name}: the procedure using apply is compiled`, compiledCount, 1);
    assert(logger, `${name}: matches the interpreter`, got, interpretedValue);
    assert(logger, `${name}: and the answer is right`, got, expected);
  }

  logger.title('Compiler - Capturing a Continuation Across a Compiled Frame');

  for (const [name, source, probe, names] of CAPTURE_CASES) {
    // The interpreter answers the same program, and that answer is the
    // contract. Writing the expected value out by hand would only record what
    // this implementation happens to do.
    let expected;
    try {
      expected = render(evaluate(`${source} ${probe}`, template, false).value);
    } catch (e) {
      logger.fail(`${name}: interpreted run threw ${e.message}`);
      continue;
    }

    let got;
    let compiled = 0;
    try {
      const outcome = evaluateSelective(source, probe, template, names);
      got = render(outcome.value);
      compiled = outcome.compiled;
    } catch (e) {
      got = `threw: ${e.message}`;
    }

    // Without this the case could pass by compiling nothing, which is exactly
    // how the earlier boundary tests managed to compare the interpreter against
    // itself for three rounds of benchmarking.
    assert(logger, `${name}: the procedures under test are compiled`,
      compiled, names.length);
    assert(logger, `${name}: matches the interpreter`, got, expected);
  }

  // What the protocol does *not* cover, asserted so that nobody mistakes the
  // cases above for completeness. Compiled and interpreted code alternating
  // more than once would need each group of frames spliced at its own boundary,
  // and getting that wrong yields a wrong answer rather than a failure -- so it
  // is refused until it is built.
  {
    const source =
      '(define (capture n)'
      + '  (call/cc (lambda (q) (if (> n 0) (q (quote esc))) (quote norm))))'
      + '(define (inner n) (cons (capture n) (quote (i))))'
      + '(define (middle n) (cons (inner n) (quote (m))))'
      + '(define (outer n) (cons (middle n) (quote (o))))';

    let message = null;
    try {
      evaluateSelective(source, '(outer 1)', template, ['outer', 'inner']);
    } catch (e) {
      message = e.message;
    }
    assert(logger, 'a capture across two compiled/interpreted boundaries is refused',
      message !== null && /more than one boundary/.test(message), true);

    // Leaving `middle` compiled too makes it one boundary again, so the refusal
    // is about the alternation rather than about the depth.
    const { value } = evaluateSelective(
      source, '(outer 1)', template, ['outer', 'middle', 'inner']);
    assert(logger, 'and the same program across one boundary is answered',
      render(value), '(((esc i) m) o)');
  }

  logger.title('Compiler - The Guard Is No Longer What Makes Backtracking Work');

  // This case is the reason the reachability analysis exists: `search` and
  // `enumerate` have to be re-entered when the search backtracks, and a
  // compiled frame could not be re-entered, so both had to be declined. Now
  // they can be, and the point of running with the guard deliberately off is
  // that with it on these procedures are never compiled and the mechanism that
  // re-enters them is never exercised at all.
  {
    const [, backtracking, expected] = CONTINUATION_CASES[2];
    const { interpreter: fresh, env } = freshEnvironment(template);
    const asts = parse(backtracking).map((form) => analyze(form));
    const definitions = asts.filter((a) => a instanceof DefineNode);
    const rest = asts.filter((a) => !(a instanceof DefineNode));
    const outcome = compileProgram(definitions, env, fresh, { allowContinuationUnsafe: true });

    let value;
    let message = null;
    try {
      for (const ast of rest) {
        value = settle(fresh.run(ast, env, [], undefined, { jsAutoConvert: 'raw' }));
      }
    } catch (e) {
      message = e.message;
    }

    assert(logger, 'bypassing the guard compiles the procedures it would decline',
      outcome.compiled.includes('search') && outcome.compiled.includes('enumerate'), true);
    assert(logger, 'and backtracking through those compiled frames is still correct',
      message === null ? render(value) : `threw: ${message}`, expected);
  }

  // `tryCompileDefinition` is the incremental entry point, and it carries *no*
  // continuation guard: it declines a lambda that mentions a control global and
  // compiles its callers regardless. Both benchmark harnesses use it. What used
  // to make that unsound was that the compiled caller could not be part of the
  // continuation; now it can, so the per-definition path is correct here too.
  {
    const source =
      '(define (escaper n)'
      + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
      + '(define (caller n) (cons (escaper n) (quote (tail))))';
    const { interpreter: fresh, env } = freshEnvironment(template);
    let compiled = 0;
    for (const form of parse(source)) {
      const ast = analyze(form);
      if (ast instanceof DefineNode) {
        const result = tryCompileDefinition(ast, env);
        if (result.compiled) {
          env.define(result.name, result.procedure);
          compiled++;
          continue;
        }
      }
      fresh.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
    }
    let value;
    let message = null;
    try {
      value = settle(fresh.run(
        analyze(parse('(caller 1)')[0]), env, [], undefined, { jsAutoConvert: 'raw' }));
    } catch (e) {
      message = e.message;
    }

    assert(logger, 'the per-definition path declines only the procedure naming call/cc',
      compiled, 1);
    // This used to return `escaped` -- the escape value, with everything
    // `caller` had left to do silently dropped -- and then, for a while, to be
    // refused outright rather than answered wrongly. Now `caller` suspends
    // itself into the continuation on the way out and finishes its `cons` when
    // the escape reinstates it.
    assert(logger, 'a capture over a compiled frame is answered correctly',
      message === null ? render(value) : `threw: ${message}`, '(escaped tail)');
  }

  logger.title('Compiler - Deep Binding Chains Compile');

  // The analyzer expands every `let` into an immediately-applied lambda, so a
  // chain of bindings used to be a chain of nested procedures -- and since each
  // procedure is emitted inside both forms of its parent, the generated code
  // doubled with every level. `let*` with six bindings is six levels; the
  // deepest in the benchmark corpus was twenty-nine, which exceeded
  // JavaScript's maximum string length and could not be compiled at all.
  //
  // Reducing the application back to bindings removes the nesting, so this now
  // compiles rather than being declined for size.
  {
    // The body reads the innermost binding, so every level is live and none of
    // it is nesting the compiler could discard.
    let body = '(g x40)';
    for (let i = 40; i >= 1; i--) body = `(let ((x${i} (g x${i - 1}))) ${body})`;
    const source = `(define (g n) (+ n 1)) (define (f x0) ${body})`;

    const { interpreter: fresh, env } = freshEnvironment(template);
    const asts = parse(source).map((form) => analyze(form));
    const outcome = compileProgram(
      asts.filter((a) => a instanceof DefineNode), env, fresh);

    assert(logger, 'a forty-deep binding chain compiles', outcome.compiled.includes('f'), true);
    const value = settle(fresh.run(
      analyze(parse('(f 0)')[0]), env, [], undefined, { jsAutoConvert: 'raw' }));
    assert(logger, 'and computes the right answer', render(value), '41');

    // `let*` is the shape that made this acute: one binding level per clause.
    const starBody = Array.from({ length: 20 }, (_, i) => `(y${i + 1} (g y${i}))`).join(' ');
    const star = `(define (h y0) (let* (${starBody}) (g y20)))`;
    const starOutcome = compileProgram(
      parse(star).map((form) => analyze(form)), env, fresh);
    assert(logger, 'and so does a twenty-clause let*', starOutcome.compiled.includes('h'), true);
    assert(logger, 'with the right answer too',
      render(settle(fresh.run(analyze(parse('(h 0)')[0]), env, [], undefined,
        { jsAutoConvert: 'raw' }))), '21');
  }

  // Procedures genuinely nested inside one another -- closures returning
  // closures, not applied where they are written -- used to cost about 4.2x per
  // level, because each was emitted inside both forms of its parent and each of
  // those emitted both of its forms. Emitting each once as a top-level factory
  // over its free variables makes it linear: sixteen levels deep went from
  // 138,801,809 characters of generated source, which could not be emitted at
  // all, to 11,478.
  {
    let body = '(g x0)';
    for (let i = 16; i >= 1; i--) body = `(lambda (x${i}) (g ${body}))`;
    const source = `(define (g n) n) (define (f x0) ${body})`;

    const { interpreter: fresh, env } = freshEnvironment(template);
    const asts = parse(source).map((form) => analyze(form));
    const outcome = compileProgram(
      asts.filter((a) => a instanceof DefineNode), env, fresh);

    assert(logger, 'sixteen levels of nested closures compile',
      outcome.compiled.includes('f'), true);

    // And still compute: each factory has to receive exactly the free variables
    // its body reads, so a chain this deep is a real test of that.
    let call = '(f 7)';
    for (let i = 1; i <= 16; i++) call = `(${call} ${i})`;
    assert(logger, 'and the whole chain returns the innermost value',
      render(settle(fresh.run(analyze(parse(call)[0]), env, [], undefined,
        { jsAutoConvert: 'raw' }))), '7');
  }

  logger.title('Compiler - Compilation Actually Happens');

  // Without this the differential tests would pass trivially if the compiler
  // silently declined everything.
  {
    const { compiled } = evaluate(
      '(define (f n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))) (f 10)', template, true);
    assert(logger, 'a plain recursive procedure is compiled', compiled.length, 1);
    assert(logger, 'the suite compiled a meaningful number of procedures',
      compiledCount > 20, true);
  }

  logger.title('Compiler - Inlined Primitives Stay Faithful');

  // Arithmetic and pair access are expanded inline with a guard on the binding,
  // because Scheme allows the primitive to be redefined after compilation. If
  // the guard were wrong, a redefinition would be silently ignored -- which is
  // exactly the kind of bug that never shows up in a benchmark.
  {
    const { interpreter: fresh, env } = freshEnvironment(template);
    const asts = parse('(define (add a b) (+ a b))').map((f) => analyze(f));
    const result = tryCompileDefinition(asts[0], env);
    assert(logger, 'arithmetic procedure compiles', result.compiled, true);
    env.define(result.name, result.procedure);

    const evalIn = (code) => render(settle(
      fresh.run(analyze(parse(code)[0]), env, [], undefined, { jsAutoConvert: 'raw' })));

    assert(logger, 'inlined addition is correct before redefinition', evalIn('(add 2 3)'), '5');

    // Redefine `+` to something observably different, then call the already
    // compiled procedure.
    fresh.run(analyze(parse('(define (+ a b) 999)')[0]), env, [], undefined,
      { jsAutoConvert: 'raw' });
    assert(logger, 'redefining an inlined primitive is observed by compiled code',
      evalIn('(add 2 3)'), '999');
  }

  {
    // A wrong type must fail the same way it does interpreted, rather than
    // producing whatever the inline fast path would compute.
    const { interpreter: fresh, env } = freshEnvironment(template);
    const asts = parse('(define (head xs) (car xs))').map((f) => analyze(f));
    const result = tryCompileDefinition(asts[0], env);
    env.define(result.name, result.procedure);

    let compiledThrew = false;
    try {
      fresh.run(analyze(parse('(head 5)')[0]), env, [], undefined, { jsAutoConvert: 'raw' });
    } catch (e) {
      compiledThrew = true;
    }

    const { interpreter: plain, env: plainEnv } = freshEnvironment(template);
    let interpretedThrew = false;
    try {
      for (const form of parse('(define (head xs) (car xs)) (head 5)')) {
        plain.run(analyze(form), plainEnv, [], undefined, { jsAutoConvert: 'raw' });
      }
    } catch (e) {
      interpretedThrew = true;
    }

    assert(logger, 'car on a non-pair fails in compiled code as it does interpreted',
      compiledThrew, interpretedThrew);
  }

  logger.title('Compiler - The Resumable Twin Matches the Fast Form');

  // Every procedure is emitted twice: once as straight-line JavaScript, and
  // once as a state machine over its own call sites so that a continuation
  // captured inside it can resume where it left off. The two must compute the
  // same thing, and they are separate code paths, so the only way to know is to
  // run both. Entered at block 0 with its arguments in a frame, the twin is an
  // ordinary call of the same procedure.
  {
    const twinCases = [
      ['recursion with two calls', '(define (f n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2)))))', [12n]],
      ['tail recursion', '(define (f n a) (if (< n 1) a (f (- n 1) (+ a n))))', [50n, 0n]],
      ['named let', '(define (f n) (let loop ((i 0) (a 0)) (if (> i n) a (loop (+ i 1) (+ a i)))))', [10n]],
      ['nested conditionals',
        '(define (f n) (if (< n 0) (quote neg) (if (= n 0) (quote zero) (quote pos))))', [0n]],
      ['allocation in a recursive call',
        '(define (f n) (if (< n 1) (quote ()) (cons n (f (- n 1)))))', [4n]],
      ['call in a let initializer',
        '(define (g x) (* x 2)) (define (f n) (let ((a (g n))) (+ a (g a))))', [5n]],
      ['let* chain', '(define (f a) (let* ((b (+ a 1)) (c (* b 2))) (if (> c 4) (cons b c) (cons c b))))', [3n]],
      ['mutually recursive letrec',
        '(define (f n) (letrec ((ev? (lambda (k) (if (= k 0) #t (od? (- k 1)))))'
        + ' (od? (lambda (k) (if (= k 0) #f (ev? (- k 1)))))) (ev? n)))', [10n]],
      ['rest parameter', '(define (f a . rest) (cons a rest))', [1n, 2n, 3n]],
      ['rest parameter, none supplied', '(define (f a . rest) (cons a rest))', [1n]]
    ];

    for (const [name, source, args] of twinCases) {
      const { interpreter: fresh, env } = freshEnvironment(template);
      let procedure = null;
      let params = null;
      let hasRest = false;

      for (const form of parse(source)) {
        const ast = analyze(form);
        const result = ast instanceof DefineNode
          ? tryCompileDefinition(ast, env) : { compiled: false };
        if (result.compiled) {
          env.define(result.name, result.procedure);
          procedure = result.procedure;
          const lowered = lowerLambda(ast.valueExpr ?? ast.value);
          hasRest = Boolean(lowered.ir.rest);
          params = [
            ...lowered.ir.params.map(jsName),
            ...(hasRest ? [jsName(lowered.ir.rest)] : [])
          ];
        } else {
          fresh.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
        }
      }

      if (procedure === null || procedure.$resume === undefined) {
        logger.fail(`${name}: no compiled procedure with a twin`);
        continue;
      }

      // The twin receives a rest parameter as the Scheme list the body expects,
      // where the fast form builds one from JavaScript varargs.
      const fixed = params.length - (hasRest ? 1 : 0);
      const frame = {};
      for (let i = 0; i < fixed; i++) frame[params[i]] = args[i];
      if (hasRest) {
        let list = null;
        for (let i = args.length - 1; i >= fixed; i--) list = new Cons(args[i], list);
        frame[params[fixed]] = list;
      }

      const attempt = (thunk) => {
        try { return render(settle(thunk())); } catch (e) { return `threw: ${e.message}`; }
      };
      assert(logger, `twin agrees with the fast form: ${name}`,
        attempt(() => procedure.$resume(0, frame)),
        attempt(() => procedure(...args)));
    }
  }

  logger.title('Compiler - Reachability Decides What Is Safe');

  // The point of the call-graph closure is that it is neither of the two rules
  // that came before it: not "decline what names call/cc", which misses
  // `make-maze` and `enumerate`, and not "decline the whole unit", which on
  // real programs compiled nothing at all.
  {
    const analyse = (source, options) => {
      const { env } = freshEnvironment(template);
      const asts = parse(source).map((form) => analyze(form));
      return unsafeDefinitions(asts, env, options);
    };

    const chain = analyse(
      '(define (c) (call/cc (lambda (k) (k 1))))'
      + '(define (b) (c))'
      + '(define (a) (b))');
    assert(logger, 'a direct reference is unsafe', chain.has('c'), true);
    assert(logger, 'one call away is unsafe', chain.has('b'), true);
    assert(logger, 'two calls away is unsafe', chain.has('a'), true);
    assert(logger, 'the reason names the path',
      /reaches b/.test(chain.get('a') ?? ''), true);

    // The improvement over the unit-level veto, stated as a test: an unrelated
    // procedure in the same unit is still compiled.
    const mixed = analyse(
      '(define (escapes) (call/cc (lambda (k) (k 1))))'
      + '(define (unrelated n) (* n 2))');
    assert(logger, 'an unrelated procedure in the same unit stays safe',
      mixed.has('unrelated'), false);
    assert(logger, 'while its neighbour is declined', mixed.has('escapes'), true);

    // Reaching *outside* the unit, into a procedure already in the environment.
    {
      const { interpreter: fresh, env } = freshEnvironment(template);
      for (const form of parse('(define (library-escape) (call/cc (lambda (k) (k 7))))')) {
        fresh.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
      }
      const asts = parse('(define (uses-library) (library-escape))')
        .map((form) => analyze(form));
      const outside = unsafeDefinitions(asts, env);
      assert(logger, 'reaching a capturing procedure outside the unit is unsafe',
        outside.has('uses-library'), true);
    }

    // `strict` treats a callee the compiler cannot name as a possible capture.
    // Asserted both ways so the option's meaning is not folklore.
    const viaParameter =
      '(define (taker f x) (f x))'
      + '(define (plain n) (+ n 1))';
    assert(logger, 'strict declines a procedure that calls what it was given',
      analyse(viaParameter, { strict: true }).has('taker'), true);
    // Off by default: it declines most higher-order code, and what it used to
    // buy -- catching a capture that arrives as an argument -- is now handled
    // by the procedure suspending itself into the continuation instead.
    assert(logger, 'and it is off unless asked for',
      analyse(viaParameter).has('taker'), false);
    assert(logger, 'strict does not decline a procedure with no unknown callee',
      analyse(viaParameter, { strict: true }).has('plain'), false);

    // The limitation, asserted so that nobody mistakes this for soundness.
    // A global rebound after compilation is invisible to an analysis that ran
    // before it. Only compiled frames that can be reified and resumed close
    // this.
    const rebound = analyse(
      '(define (helper n) (* n 2))'
      + '(define (user n) (helper n))');
    assert(logger, 'KNOWN LIMIT: a global not yet capturing is judged safe',
      rebound.has('user'), false);
  }

  logger.title('Compiler - The Compiled-to-Interpreted Boundary');

  // A compiled procedure that calls an interpreted one must not have its values
  // converted as if they were crossing into JavaScript. Ten of the 41 canonical
  // R7RS benchmarks failed on this, six of them with a silent wrong answer, and
  // nothing in this file or in the benchmark suite detected it. The two reasons
  // it went unseen are described above the boundary cases.
  for (const [name, source, names, probe, expected] of BOUNDARY_CASES) {
    let outcome;
    try {
      outcome = evaluateSelective(source, probe, template, names);
    } catch (e) {
      logger.fail(`${name}: threw: ${e.message}`);
      continue;
    }
    assert(logger, `${name} (compilation happened)`, outcome.compiled > 0, true);
    assert(logger, name, render(outcome.value), expected);
  }

  logger.title('Compiler - Generated Procedures Interoperate');

  {
    const { interpreter: fresh, env } = freshEnvironment(template);
    const asts = parse('(define (double x) (* x 2))').map((f) => analyze(f));
    const result = tryCompileDefinition(asts[0], env);
    assert(logger, 'definition compiles', result.compiled, true);

    env.define(result.name, result.procedure);
    // Called from interpreted code, a compiled procedure must behave like any
    // other, including not having its exact arguments converted to doubles.
    const value = fresh.run(
      analyze(parse('(double 21)')[0]), env, [], undefined, { jsAutoConvert: 'raw' });
    assert(logger, 'compiled procedure called from interpreted code', render(settle(value)), '42');

    const mapped = fresh.run(
      analyze(parse('(map double (list 1 2 3))')[0]), env, [], undefined, { jsAutoConvert: 'raw' });
    assert(logger, 'compiled procedure passed to an interpreted higher-order procedure',
      render(settle(mapped)), '(2 4 6)');
  }
}

export default runCompilerTests;
