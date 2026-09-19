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
import { tryCompileDefinition, compileProgram } from '../../src/compiler/index.js';
import { unsafeDefinitions } from '../../src/compiler/safety.js';
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
 * Programs that use continuations, with the procedures that must be declined.
 *
 * Every procedure in whose extent a capture can occur must be left to the
 * interpreter, and the answer must be right. Naming them individually is the
 * contract: the earlier rule -- veto the entire unit -- satisfied it only by
 * compiling nothing at all, which on real programs meant the tier never ran
 * (R28). `fail` in the backtracking case is deliberately *not* listed: it
 * captures nothing and reaches nothing that does, so it is safe and compiling
 * it is correct.
 *
 * The `backtracking` case is why this list exists. Declining only `in-range`
 * left `btsearch` and `enumerate` compiled even though both sit in the dynamic
 * extent of the capture and have to be re-entered when the search backtracks
 * (R15). `escape past a would-be compiled frame` is the second shape, found in
 * `maze`: an escape unwinding past a compiled frame whose procedure names no
 * control global at all (R34).
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
    '(search 5)', '(5 . 5)', ['enumerate', 'in-range', 'search']],
  ['dynamic-wind', '(define (f) (dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3))) (f)', '2', ['f']],
  // The escape variant, and the one that bites in practice. `caller` never
  // mentions `call/cc`, so a per-procedure rule compiles it happily -- but the
  // escape unwinds past its frame and the escape value becomes *its* result.
  // This is how the `maze` benchmark fails: `dig-maze` quits with `(quit #f)`
  // and compiled `make-maze` returns `#f` instead of the maze. See R33.
  ['escape past a would-be compiled frame',
    '(define (escaper n)'
    + '  (call/cc (lambda (quit) (if (> n 0) (quit (quote escaped))) (quote normal))))'
    + '(define (caller n) (cons (escaper n) (quote (tail))))'
    + '(caller 1)', '(escaped tail)', ['escaper', 'caller']]
];

/**
 * Cases the compiler must decline, with the reason it should give. Declining
 * is a feature, so it is tested like one.
 */
const MUST_DECLINE = [
  ['call/cc', '(define (f) (call/cc (lambda (k) (k 1))))', 'call/cc'],
  ['dynamic-wind', '(define (f) (dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3)))', 'dynamic-wind'],
  ['apply', '(define (f xs) (apply + xs))', 'apply'],
  ['values', '(define (f) (values 1 2))', 'values']
];

/**
 * Cases exercising the **compiled-to-interpreted boundary**, with only the
 * named procedures compiled.
 *
 * Three existing cases in `CASES` are called "compiled calls interpreted" and
 * friends, and every one of them compiles *nothing*: each forces its callee to
 * stay interpreted by writing it with `apply`, which trips the unit-level
 * continuation guard and declines the whole unit. They compare the interpreter
 * against itself. That is half of why R26 went unnoticed.
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
 * @returns {{value: *, compiled: number}} The probe's value and how many
 *   definitions were compiled.
 */
function evaluateSelective(source, probe, template, names) {
  const { interpreter, env } = freshEnvironment(template);
  const wanted = new Set(names);
  let compiled = 0;

  for (const form of parse(source)) {
    const ast = analyze(form);
    if (ast instanceof DefineNode && wanted.has(ast.originalName || ast.name)) {
      const result = tryCompileDefinition(ast, env);
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

  logger.title('Compiler - Procedures Reaching a Capture Are Declined');

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

  logger.title('Compiler - The Unit-Level Guard Is Load-Bearing');

  // Bypassing the guard must visibly break, otherwise the guard is untested and
  // could be weakened later by someone who sees no consequence.
  {
    const [, backtracking, expected] = CONTINUATION_CASES[2];
    const { interpreter: fresh, env } = freshEnvironment(template);
    const asts = parse(backtracking).map((form) => analyze(form));
    const definitions = asts.filter((a) => a instanceof DefineNode);
    const rest = asts.filter((a) => !(a instanceof DefineNode));
    const outcome = compileProgram(definitions, env, fresh, { allowContinuationUnsafe: true });

    let value;
    let threw = false;
    try {
      for (const ast of rest) {
        value = settle(fresh.run(ast, env, [], undefined, { jsAutoConvert: 'raw' }));
      }
    } catch (e) {
      threw = true;
    }

    assert(logger, 'bypassing the guard does compile procedures it should not',
      outcome.compiled.length > 0, true);
    assert(logger, 'and the result is then wrong or throws, which is what the guard prevents',
      threw || render(value) !== expected, true);
  }

  // `tryCompileDefinition` is the incremental entry point, and it carries *no*
  // continuation guard -- it declines a lambda that mentions a control global,
  // which is exactly the per-procedure rule R15 proved unsound. Both benchmark
  // harnesses use it. This asserts the unsoundness rather than leaving it to
  // prose, so that anyone who makes the guard sound sees this test change.
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
    // Capturing a continuation while a compiled frame is live used to return
    // `escaped` -- the escape value, with everything `caller` had left to do
    // silently dropped. It is now refused outright. A wrong answer that looks
    // plausible is the worst way for a compiler to be wrong, and this is the
    // third time this project has met one (R15, R26, R34).
    assert(logger, 'capturing over a compiled frame is refused, not answered wrongly',
      message !== null, true);
    assert(logger, 'and the refusal says what is wrong',
      message !== null && /compiled frames cannot yet be reified/.test(message), true);
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

    // `strict` is what catches the btsearch shape, where the capture arrives as
    // an argument. Asserted both ways so the option's meaning is not folklore.
    const viaParameter =
      '(define (taker f x) (f x))'
      + '(define (plain n) (+ n 1))';
    assert(logger, 'strict declines a procedure that calls what it was given',
      analyse(viaParameter, { strict: true }).has('taker'), true);
    assert(logger, 'loose does not, which is why strict is the default',
      analyse(viaParameter, { strict: false }).has('taker'), false);
    assert(logger, 'strict does not decline a procedure with no unknown callee',
      analyse(viaParameter, { strict: true }).has('plain'), false);

    // The limitation, asserted so that nobody mistakes this for soundness.
    // A global rebound after compilation is invisible to an analysis that ran
    // before it. Only re-enterable frames close this -- increment 2b.
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
  // nothing in this file or in the benchmark suite detected it -- see R26.
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
