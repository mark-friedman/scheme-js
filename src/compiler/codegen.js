/**
 * @fileoverview JavaScript code generation for the Scheme compiler.
 *
 * Non-tail calls are ordinary
 * JavaScript calls, so one live Scheme frame is one live JavaScript frame and a
 * debugger can see the Scheme call stack; tail calls return a `TailCall` through
 * a per-call-site trampoline, so tail recursion runs in constant space.
 *
 * Reusing the interpreter's own `TailCall` as the tail-call signal, rather than
 * a private sentinel, is what makes interoperation free in both directions: the
 * interpreter already knows how to continue a returned `TailCall`, and compiled
 * trampolines already know how to continue one returned by an interpreted
 * procedure.
 *
 * Code is emitted as statements with explicit temporaries rather than as nested
 * expressions, because an immediately-invoked function per `let` would cost on
 * every evaluation what it saves once at compile time.
 */


import { inlinableFor, INLINABLE } from './inline.js';
import { currentBinding } from './runtime.js';

/**
 * Emits JavaScript for one procedure.
 */

import { ProcedureEmitter, procedureScope, jsName } from './emitter.js';
import { TwinEmitter } from './resume.js';
import { planLifting } from './lift.js';

export { jsName };

/**
 * Renders a nested procedure as a top-level factory over its free variables.
 *
 * Both forms of the procedure go inside, so a frame suspended in one can be
 * resumed in the other, and the closure they share is what the factory returns.
 *
 * A `letrec` name the procedure refers to *only itself by* is declared here and
 * assigned before returning, so the recursive call resolves lexically and stays
 * a direct call -- which matters because a named `let` is exactly this shape and
 * is usually a hot loop. A name its siblings also refer to is boxed instead and
 * arrives as a parameter.
 *
 * @param {string} factoryName - Identifier for the factory.
 * @param {string} procName - Identifier for the procedure it builds.
 * @param {string} path - The procedure's position in the tree of procedures.
 * @param {Object} node - Its `lambda` IR node.
 * @param {Object} ctx - Shared emitter context.
 * @param {Object} plan - The lifting plan.
 * @returns {string} JavaScript source for the factory.
 */
function renderFactory(factoryName, procName, path, node, ctx, plan) {
  const params = (plan.free.get(node) ?? []).map(jsName);
  const own = (plan.selfNames.get(node) ?? []).map(jsName);
  const twinName = `${procName}$r`;

  // The resumable form is generated first, because generating it is what
  // records the resume points and frame layout the fast form refers to.
  const twinSource = new TwinEmitter(twinName, node, ctx, procedureScope(path)).emit();
  const fastSource = new ProcedureEmitter(procName, node, ctx, procedureScope(path)).emit();

  const lines = [];
  if (own.length > 0) lines.push(`let ${own.join(', ')};`);
  lines.push(fastSource);
  lines.push(`R.markProcedure(${procName}, ${JSON.stringify(node.name ?? 'anonymous')});`);
  lines.push(twinSource);
  lines.push(`${procName}.$resume = ${twinName};`);
  // Assigned after the procedure exists, and before the factory returns, so it
  // is always set by the time anything can call it.
  for (const self of own) lines.push(`${self} = ${procName};`);
  lines.push(`return ${procName};`);

  return `function ${factoryName}(${params.join(', ')}) {\n`
    + lines.map((line) => '  ' + line).join('\n') + '\n}';
}

export function generate(ir, globals, name, env) {
  const globalNames = [...globals];
  const slots = new Map();
  globalNames.forEach((g, i) => slots.set(g, `G${i}`));

  // Globals with an inline expansion also get a constant naming the primitive
  // they denote right now, so generated code can guard on the binding not
  // having been replaced since.
  const guards = new Map();
  if (env) {
    globalNames.forEach((g, i) => {
      if (INLINABLE[g] === undefined) return;
      const current = currentBinding(env, g);
      if (typeof current === 'function') guards.set(g, `P${i}`);
    });
  }

  // Which nested procedures to emit once at the top level, what free variables
  // each needs, and which locals must be boxed. All three come from one pass
  // over the IR, because they are the same question asked three ways: what can
  // safely be passed by value, and what has to be shared.
  const plan = planLifting(ir);

  /** @type {Array<string>} Factory sources, in the order they were generated. */
  const factories = [];
  /** @type {Map<Object, string>} Factory name per lambda node, to emit once. */
  const emitted = new Map();

  const ctx = {
    constants: [],
    globalRef: (g) => slots.get(g),
    primitiveGuard: (g) => guards.get(g) ?? null,
    // Where each non-tail call site resumes, and which locals a suspended frame
    // carries. Both are decided by the resumable form and read by the fast one,
    // which has to spill into a frame that the resumable form can restore.
    resumePoints: new Map(),
    frameSlots: new Map(),
    // Locals held in a one-element array rather than a plain variable, so that
    // a spilled frame shares the binding instead of copying its value, and so
    // that a mutually recursive `letrec` group can be built in any order.
    boxed: plan.boxed,
    lift: plan.lift,
    free: plan.free,
    /**
     * The factory for a nested procedure, emitting it the first time it is
     * asked for. Both forms of a parent ask for the same node and get the same
     * factory, which is the whole point.
     *
     * @param {string} procName - The nested procedure's identifier.
     * @param {string} path - Its position in the tree of procedures.
     * @param {Object} node - Its `lambda` IR node.
     * @returns {string} The factory's identifier.
     */
    factoryFor: (procName, path, node) => {
      const already = emitted.get(node);
      if (already !== undefined) return already;
      const factoryName = `$mk${procName}`;
      // Recorded before emitting, so that anything reached while emitting it
      // finds the name rather than starting again.
      emitted.set(node, factoryName);
      factories.push(renderFactory(factoryName, procName, path, node, ctx, plan));
      return factoryName;
    },
    // Lets the fast-form emitter produce resumable forms for the procedures it
    // nests, without importing the module that defines them.
    twinFor: (name, node, ctx, path) =>
      new TwinEmitter(name, node, ctx, procedureScope(path)).emit()
  };

  // A direct, binding-guarded self-recursive call was tried here and removed:
  // it measured 7% *slower* than going through the global accessor (fib(30)
  // 82.7 ms against 76.7 ms). The accessor is already a single hash lookup that
  // V8 inlines, and the guard's conditional callee appears to cost more than it
  // saves. The profile that suggested the optimization was taken at a 9 ms wall
  // time, where the sampling profiler's own overhead was 66% and inflated the
  // accessor's apparent share.
  //
  // A *tail* call to the procedure itself is a different matter, and does loop:
  // it replaces an allocation and a return to the trampoline, not a lookup.
  // See `loopBack` in `emitter.js` and "Loops" in `ir.scm`.

  // The resumable form is generated first, although it is emitted second.
  // Generating it is what decides where each call site resumes and which locals
  // a suspended frame carries, and the fast form needs both in order to suspend
  // itself when a continuation is captured beneath it.
  //
  // Both are generated from the same IR by classes that differ only in control
  // flow, so they cannot drift apart in what they compute, and each numbers its
  // temporaries from zero, so they agree on what to call every value they hold.
  // That agreement is what makes a frame spilled by one restorable by the other.
  const twin = new TwinEmitter('$proc$r', ir, ctx).emit();
  const procedure = new ProcedureEmitter('$proc', ir, ctx).emit();

  // Each global gets a memoizing accessor rather than a direct value, so that a
  // forward reference compiles and a later redefinition is still observed.
  const accessors = globalNames
    .map((g, i) => {
      const lines = [`const G${i} = R.globalAccessor(E, ${JSON.stringify(g)});`];
      if (guards.has(g)) lines.push(`const P${i} = R.currentBinding(E, ${JSON.stringify(g)});`);
      return lines.join('\n');
    })
    .join('\n');

  const source = [
    accessors,
    // Hoisted function declarations, so their order relative to their callers
    // does not matter. They are collected while the two forms above are
    // generated, which is why this reads them afterwards.
    factories.join('\n'),
    procedure,
    twin,
    `R.markProcedure($proc, ${JSON.stringify(name)});`,
    // The fast form carries a reference to its resumable form, so a frame
    // suspended in one can be resumed in the other without a lookup table.
    '$proc.$resume = $proc$r;',
    'return $proc;'
  ].filter(Boolean).join('\n');

  return { source, constants: ctx.constants, globals: globalNames };
}
