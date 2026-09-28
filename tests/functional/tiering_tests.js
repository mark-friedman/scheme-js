/**
 * @fileoverview Compiling a program's own code as it runs (`src/compiler/tiering.js`).
 *
 * The policy under test: a top-level procedure whose body loops or makes
 * procedures is compiled when it is defined, any other on its second call,
 * each over the interpreted closure it replaces, so a debugger can switch it
 * back; a top-level expression is compiled only if it loops. The switch needs
 * no on-stack replacement, because both tiers look a top-level name up at
 * every call, so a recursion that crosses the threshold continues compiled
 * from its next call -- that is tested with a probe that looks at the binding
 * while the recursion is still running.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import { settle } from '../../src/compiler/runtime.js';
import { attachTier, detachTier } from '../../src/compiler/tiering.js';
import { isCompiledOver, withPrivateLibraries, getLibraryEnv } from '../../src/core/interpreter/library_registry.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { installLibraryTable } from '../../src/compiler/prebuilt.js';
import prebuiltLibraries from '../../src/packaging/compiled_libraries.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { loadLibrarySync, applyImports } from '../../src/core/interpreter/library_loader.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';

/**
 * An interpreter with the standard library installed compiled, as it ships,
 * and the tier attached.
 * @param {Object} [options] - For `attachTier`.
 * @returns {{interpreter: Object, env: Object, tier: Object, run: Function, compiled: Function}}
 *   The pair, the tier, a function running source form by form as a program's
 *   top level and returning the last value written out, and one saying whether
 *   a name is bound to a procedure compiled over its closure.
 */
function tiered(options) {
  const { interpreter, env } = interpretedLibrary();
  installStandardLibrary(env);
  const tier = attachTier(interpreter, env, options);
  const run = (source) => {
    let value;
    for (const form of parse(source)) {
      value = settle(interpreter.runTopLevel(analyze(form), env, { jsAutoConvert: 'raw' }));
    }
    return writeString(value);
  };
  const compiled = (name) => isCompiledOver(env.lookup(name));
  return { interpreter, env, tier, run, compiled };
}

/**
 * Runs the tiering tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runTieringTests(logger) {
  logger.title('Tiering - When a Program\'s Procedures Are Compiled');
  {
    const t = tiered();
    assert(logger, 'the tier attaches', t.tier !== null && t.interpreter.tier === t.tier, true);

    t.run('(define (sum-to n) (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i)))))');
    assert(logger, 'a procedure that loops is compiled when it is defined', t.compiled('sum-to'), true);
    assert(logger, 'and answers as before', t.run('(sum-to 100)'), '5050');

    t.run('(define (adder n) (lambda (x) (+ x n)))');
    assert(logger, 'a procedure that makes procedures is compiled when it is defined', t.compiled('adder'), true);
    assert(logger, 'and so are the procedures it makes', t.run('((adder 2) 3)'), '5');

    t.run('(define (square x) (* x x))');
    assert(logger, 'any other procedure is not compiled when it is defined', t.compiled('square'), false);
    t.run('(square 3)');
    assert(logger, 'nor after its first call', t.compiled('square'), false);
    assert(logger, 'its second call answers', t.run('(square 4)'), '16');
    assert(logger, 'and compiles it', t.compiled('square'), true);
    assert(logger, 'the outcome is recorded', t.tier.outcomes.get('square'), 'compiled');
    assert(logger, 'and it answers compiled', t.run('(square 5)'), '25');
  }

  logger.title('Tiering - A Recursion Continues Compiled From Its Next Call');
  {
    const t = tiered();
    // `probe` is JavaScript, so it runs in the middle of the recursion without
    // becoming part of it, and reports what the binding holds at that moment.
    t.env.define('probe', () => isCompiledOver(t.env.lookup('countdown')));
    t.run('(define (countdown n) (if (= n 0) (probe) (countdown (- n 1))))');
    assert(logger, 'setup: a self tail call is not a loop the definition shows', t.compiled('countdown'), false);
    assert(logger, 'a tail-recursive loop is compiled while it runs, and finishes compiled',
      t.run('(countdown 10)'), '#t');

    t.env.define('probe-depth', () => isCompiledOver(t.env.lookup('depth')));
    t.run('(define (depth n) (if (= n 0) (if (probe-depth) 0 -1000000) (+ 1 (depth (- n 1)))))');
    assert(logger, 'a non-tail recursion is compiled while it runs, frames already made finishing interpreted',
      t.run('(depth 10)'), '10');

    t.run('(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))');
    assert(logger, 'fib answers across the switch', t.run('(fib 20)'), '6765');
    assert(logger, 'and is compiled after it', t.compiled('fib'), true);

    t.run('(define (deep n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))');
    assert(logger, 'a recursion 100,000 deep answers across the switch', t.run('(deep 100000)'), '100000');
  }

  logger.title('Tiering - Which Bindings, and Which Closures');
  {
    const t = tiered();
    t.run('(define g #f)');
    t.run('(let () (set! g (lambda (x) (+ x 1))))');
    t.run('(g 1)');
    t.run('(g 2)');
    assert(logger, 'a procedure assigned to a top-level name, as nboyer assigns them, is compiled on its second call',
      t.compiled('g'), true);

    t.run('(define (h x) x)');
    t.run('(define h-old h)');
    t.run('(define (h x) (* 2 x))');
    t.run('(h-old 1)');
    t.run('(h-old 1)');
    assert(logger, 'a closure its name no longer holds is not installed under it',
      t.run('(h 3)'), '6');
    assert(logger, 'and the redefinition keeps its own count', t.compiled('h'), false);

    // Held in a list, which binds no name, so the old closure keeps the name
    // it was defined under while that name is given to another.
    t.run('(define (m x) x)');
    t.run('(define ms (list m))');
    t.run('(define (m x) (* 3 x))');
    t.run('((car ms) 1)');
    t.run('((car ms) 1)');
    assert(logger, 'a closure whose name was redefined is not compiled over the redefinition',
      t.run('(m 3)'), '9');

    t.run('(define (twice x) (* 2 x))');
    t.run('(define twice-too twice)');
    t.run('(twice 1)');
    t.run('(twice 1)');
    assert(logger, 'every top-level name holding a compiled closure is given the compiled procedure',
      [t.compiled('twice'), t.compiled('twice-too')].join(' '), 'true true');

    t.run('(define (wound x) (dynamic-wind (lambda () #f) (lambda () x) (lambda () #f)))');
    assert(logger, 'a procedure the tier declines stays interpreted', t.compiled('wound'), false);
    assert(logger, 'with its reason recorded', /dynamic-wind/.test(t.tier.outcomes.get('wound')), true);
    assert(logger, 'and answers', t.run('(wound 7)'), '7');
  }

  logger.title('Tiering - Top-Level Expressions');
  {
    const t = tiered();
    const before = t.tier.expressions;
    assert(logger, 'a top-level loop answers',
      t.run('(let loop ((i 0) (acc 0)) (if (= i 10) acc (loop (+ i 1) (+ acc i))))'), '45');
    assert(logger, 'and was compiled', t.tier.expressions, before + 1);
    t.run('(define keep (let ((n 0)) (lambda () (set! n (+ n 1)) n)))');
    t.run('(list (keep) (keep))');
    assert(logger, 'an expression that only makes procedures is not compiled', t.tier.expressions, before + 1);
    assert(logger, 'but the procedure it binds is, over its closure', t.compiled('keep'), true);
    assert(logger, 'and keeps its state', t.run('(keep)'), '3');
  }

  logger.title('Tiering - Libraries');
  {
    // The program's libraries import the shipped ones, read from the bundled
    // sources, in a registry of their own so as to leave the shared one alone.
    const bundled = (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] ?? BUNDLED_SOURCES[name[name.length - 1]];
    const seen = withPrivateLibraries({ resolver: bundled }, () => {
      const t = tiered({ isPrebuilt: (name) => name.join(' ') === 'tier prebuilt' });
      t.run(`(define-library (tier own)
               (export own-sum own-inc)
               (import (scheme base))
               (begin
                 (define (own-sum n) (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i)))))
                 (define (own-inc x) (+ x 1))))`);
      t.run('(import (tier own))');
      t.run(`(define-library (tier user)
               (export use-inc)
               (import (scheme base) (tier own))
               (begin (define (use-inc x) (own-inc x))))`);
      const ownSumLoaded = t.compiled('own-sum');
      t.run('(own-sum 1)');
      const ownSum = t.compiled('own-sum');
      t.run('(own-inc 1)');
      t.run('(own-inc 2)');
      const ownInc = t.compiled('own-inc');
      const inOther = isCompiledOver(getLibraryEnv(['tier', 'user']).bindings.get('own-inc'));
      const answers = t.run('(list (own-sum 10) (own-inc 10))');
      t.run(`(define-library (tier prebuilt)
               (export pre-sum)
               (import (scheme base))
               (begin (define (pre-sum n) (let loop ((i 0)) (if (> i n) i (loop (+ i 1)))))))`);
      t.run('(import (tier prebuilt))');
      return { ownSumLoaded, ownSum, ownInc, inOther, answers, preSum: t.compiled('pre-sum') };
    });
    // Not while the library loads: running the compiler then would register
    // its definitions with the scopes the library's macros resolve through.
    assert(logger, 'a procedure of the program\'s own library is not compiled while the library loads',
      seen.ownSumLoaded, false);
    assert(logger, 'but on its first call after', seen.ownSum, true);
    assert(logger, 'another is compiled on its second call, and the program\'s imported copy follows',
      seen.ownInc, true);
    assert(logger, 'and so does the copy another of its libraries imported', seen.inOther, true);
    assert(logger, 'and both answer', seen.answers, '(55 11)');
    assert(logger, 'a library with a prebuilt table is left to it', seen.preSum, false);
  }

  logger.title('Tiering - Debugging');
  {
    const t = tiered();
    t.interpreter.interpretForDebugger(true);
    t.run('(define (looping n) (let loop ((i 0)) (if (< i n) (loop (+ i 1)) i)))');
    assert(logger, 'nothing is compiled while the program is being debugged',
      [t.compiled('looping'), t.tier.outcomes.has('looping')].join(' '), 'false false');
    assert(logger, 'and it answers', t.run('(looping 5)'), '5');
    t.interpreter.interpretForDebugger(false);
    t.run('(looping 5)');
    assert(logger, 'it is compiled on its first call after', t.compiled('looping'), true);
    t.run('(define (waits x) (+ x 1))');
    t.interpreter.interpretForDebugger(true);
    t.run('(waits 1)');
    t.run('(waits 1)');
    // Compiled then, it would be switched straight back to its closure; the
    // tier does not compile it at all.
    assert(logger, 'a procedure whose calls run out while the program is debugged is not compiled',
      t.tier.outcomes.has('waits'), false);
    const expressionsBefore = t.tier.expressions;
    t.run('(let loop ((i 0)) (if (< i 3) (loop (+ i 1)) i))');
    assert(logger, 'nor is a top-level loop', t.tier.expressions, expressionsBefore);
    t.interpreter.interpretForDebugger(false);
    t.run('(waits 1)');
    assert(logger, 'and is compiled on its next call after', t.compiled('waits'), true);
    t.interpreter.interpretForDebugger(true);
    assert(logger, 'a compiled procedure runs as its closure while the program is debugged',
      t.compiled('looping'), false);
    t.interpreter.interpretForDebugger(false);
  }
  {
    // A breakpoint inside a procedure the tier compiled fires, as it does in
    // one the interpreter ran.
    const t = tiered();
    for (const form of parse('(define (poke x)\n  (* x 10))\n', { filename: 'tier.scm' })) {
      settle(t.interpreter.runTopLevel(analyze(form), t.env, { jsAutoConvert: 'raw' }));
    }
    t.run('(poke 1)');
    t.run('(poke 2)');
    const compiledBefore = t.compiled('poke');
    let paused = 0;
    const runtime = new SchemeDebugRuntime({
      onPause: () => { paused++; setTimeout(() => runtime.resume(), 2); }
    });
    t.interpreter.setDebugRuntime(runtime);
    runtime.enable();
    const id = runtime.setBreakpoint('tier.scm', 2);
    const result = await t.interpreter.runAsync(analyze(parse('(poke 3)')[0]), t.env, { stepsPerYield: 1000, jsAutoConvert: 'raw' });
    runtime.removeBreakpoint(id);
    t.interpreter.setDebugRuntime(null);
    assert(logger, 'setup: the procedure was compiled', compiledBefore, true);
    assert(logger, 'a breakpoint inside it pauses', paused, 1);
    assert(logger, 'and the program then finishes', writeString(result), '30');
    assert(logger, 'and it is compiled again after', t.compiled('poke'), true);
  }

  logger.title('Tiering - Attaching to a Program Already Running');
  {
    // As a browser page is: its scripts start before the compiler arrives. The
    // standard library is loaded through the library system, as a page loads
    // it, so each library's procedures live in its own environment.
    const bundled = (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] ?? BUNDLED_SOURCES[name[name.length - 1]];
    const install = (name, env) => {
      if (BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] !== undefined && env) {
        installLibraryTable(prebuiltLibraries, name, env, (file) => BUNDLED_SOURCES[file]);
      }
    };
    const seen = withPrivateLibraries({ resolver: bundled, hook: install }, () => {
      const { interpreter } = createInterpreter();
      const env = interpreter.globalEnv;
      applyImports(env, loadLibrarySync(['scheme', 'base'], analyze, interpreter, env), { libraryName: ['scheme', 'base'] });
      for (const form of parse(`(define (early n) (let loop ((i 0)) (if (< i n) (loop (+ i 1)) i)))
                                (define (early-simple x) (+ x 1))`)) {
        interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
      }
      const libraryClosures = [...env.bindings.values()].filter((v) => typeof v === 'function' && v.body !== undefined).length;
      const tier = attachTier(interpreter, env, { isPrebuilt: () => true });
      const result = {
        libraryClosures,
        early: isCompiledOver(env.lookup('early')),
        simple: isCompiledOver(env.lookup('early-simple')),
        outcomes: [...tier.outcomes.keys()].join(' ')
      };
      detachTier(interpreter);
      return result;
    });
    assert(logger, 'setup: the library leaves interpreted procedures in the program\'s environment',
      seen.libraryClosures > 2, true);
    assert(logger, 'a procedure the program defined before, that loops, is compiled on attaching', seen.early, true);
    assert(logger, 'another waits for its second call', seen.simple, false);
    assert(logger, 'and the library\'s interpreted procedures are not taken for the program\'s', seen.outcomes, 'early');
  }

  logger.title('Tiering - Detaching');
  {
    const t = tiered();
    detachTier(t.interpreter);
    assert(logger, 'the interpreter has no tier', t.interpreter.tier, null);
    t.run('(define (after-detach n) (let loop ((i 0)) (if (< i n) (loop (+ i 1)) i)))');
    t.run('(after-detach 1)');
    t.run('(after-detach 1)');
    assert(logger, 'nothing is compiled after', t.compiled('after-detach'), false);
    assert(logger, 'and a program runs as before', t.run('(after-detach 3)'), '3');
  }
}
