/**
 * @fileoverview Intermediate representation for the Scheme-to-JavaScript compiler.
 *
 * The compiler consumes the *analyzed* AST rather than raw source, which means
 * macro expansion, hygiene, alpha-renaming and internal-definition hoisting are
 * already done by `src/core/interpreter/analyzer.js`. Compiled and interpreted
 * code therefore agree on the meaning of a program by construction, instead of
 * by two front ends being kept in step.
 *
 * Lowering to this IR computes the two things code generation needs and the
 * analyzed AST does not record:
 *
 *  - **Tail position.** The analyzer does not distinguish it; every application
 *    is a `TailAppNode` regardless. Convention B needs the distinction, because
 *    a tail call must return through the trampoline while a non-tail call is an
 *    ordinary JavaScript call.
 *  - **Local versus global reference.** The analyzer alpha-renames but keeps all
 *    lookups dynamic. A local becomes a JavaScript variable; a global stays a
 *    runtime environment lookup.
 *
 * Lowering is partial on purpose: anything it does not handle yields
 * `UNSUPPORTED`, and the caller leaves that definition to the interpreter. A
 * compiler that must handle everything before it handles anything cannot be
 * shipped incrementally or trusted early.
 */

import {
  LiteralNode, VariableNode, LambdaNode, LetNode, LetRecNode, IfNode,
  SetNode, DefineNode, TailAppNode, BeginNode
} from '../core/interpreter/ast_nodes.js';

/** Returned by `lower` when a form is outside the compiler's subset. */
export const UNSUPPORTED = Symbol('unsupported');

/**
 * Globals whose primitives transfer control by returning a `TailCall`, or whose
 * semantics involve the continuation.
 *
 * A compiled procedure that references any of these is left to the interpreter.
 * This is what keeps the first increment honest: compiled code never has to
 * reason about a continuation being captured inside it, so it needs no unwind
 * protocol, and there is no window in which it could get that subtly wrong.
 * Supporting them is the next increment's job.
 */
export const CONTROL_GLOBALS = new Set([
  'call/cc', 'call-with-current-continuation', 'dynamic-wind',
  'call-with-values', 'values', 'apply', 'eval',
  'with-exception-handler', 'raise', 'raise-continuable', 'guard',
  'call-with-escape-continuation', 'exit', 'emergency-exit',
  'make-parameter', 'parameterize'
]);

/**
 * Lexical scope used during lowering to tell locals from globals.
 */
class Scope {
  /** @param {Scope|null} parent - Enclosing scope. */
  constructor(parent = null) {
    this.parent = parent;
    this.names = new Set();
    /**
     * Locals bound directly to a lambda by `let` or `letrec`.
     *
     * A call to one of these is not an unknown callee: the lambda was lowered
     * in this same pass, so whatever it references is already accounted for.
     * This is what keeps named `let` loops compilable -- `(let loop ((i 0)) ...
     * (loop ...))` binds `loop` locally, and treating that as unknown declined
     * `sum`, `nqueens`, `puzzle` and `diviter` outright.
     * @type {Set<string>}
     */
    this.callable = new Set();
  }

  /**
   * Records a name as bound to a lambda in this scope.
   * @param {string} name - The renamed variable.
   * @returns {void}
   */
  declareCallable(name) {
    this.names.add(name);
    this.callable.add(name);
  }

  /**
   * @param {string} name - The renamed variable.
   * @returns {boolean} True if the name is bound to a lambda we lowered.
   */
  isCallable(name) {
    let scope = this;
    while (scope) {
      if (scope.callable.has(name)) return true;
      if (scope.names.has(name)) return false;   // shadowed by a plain binding
      scope = scope.parent;
    }
    return false;
  }

  /**
   * Records a name as lexically bound in this scope.
   * @param {string} name - The renamed variable.
   * @returns {void}
   */
  declare(name) {
    this.names.add(name);
  }

  /**
   * @param {string} name - The renamed variable.
   * @returns {boolean} True if the name is lexically bound.
   */
  has(name) {
    let scope = this;
    while (scope) {
      if (scope.names.has(name)) return true;
      scope = scope.parent;
    }
    return false;
  }
}

/**
 * Lowering state: tracks which globals were referenced, so the caller can
 * decide whether the definition is compilable.
 */
class Lowering {
  constructor() {
    /** @type {Set<string>} Globals referenced anywhere in the form. */
    this.globals = new Set();
    /**
     * Whether any call in the form has a callee the compiler cannot name.
     *
     * A call to a global is a call to something the caller can look up and
     * analyse. A call to a *local* -- a parameter, most often -- is a call to
     * whatever the caller was handed, which may capture a continuation without
     * anything in this procedure's text hinting at it. That is precisely how
     * `btsearch` broke: `enumerate` invokes `cont`, a parameter.
     * @type {boolean}
     */
    this.callsUnknown = false;
    /**
     * Locals that were called, having been bound to a lambda, and locals that
     * were assigned. A name in both is not safe after all: the binding we
     * lowered is not necessarily what the call reaches.
     * @type {Set<string>}
     */
    this.calledLocals = new Set();
    /** @type {Set<string>} Locals that are the target of a `set!`. */
    this.assignedLocals = new Set();
    /** @type {string|null} Why lowering failed, for diagnostics. */
    this.reason = null;
  }

  /**
   * Records a failure reason and returns the sentinel.
   * @param {string} reason - Human-readable cause.
   * @returns {symbol} UNSUPPORTED.
   */
  fail(reason) {
    if (this.reason === null) this.reason = reason;
    return UNSUPPORTED;
  }
}

/**
 * Lowers an analyzed AST node to IR.
 *
 * @param {Object} node - An analyzed AST node.
 * @param {Scope} scope - The enclosing lexical scope.
 * @param {boolean} tail - Whether the node is in tail position.
 * @param {Lowering} state - Lowering state.
 * @returns {Object|symbol} IR node, or UNSUPPORTED.
 */
function lowerNode(node, scope, tail, state) {
  if (node instanceof LiteralNode) {
    return { k: 'const', value: node.value, tail };
  }

  if (node instanceof VariableNode) {
    if (scope.has(node.name)) {
      return { k: 'local', name: node.name, tail, callable: scope.isCallable(node.name) };
    }
    state.globals.add(node.name);
    // A global callee is "known" in the sense this flag means: the safety
    // analysis can look it up and follow it. It does not mean it is safe.
    return { k: 'global', name: node.name, tail, callable: true };
  }

  if (node instanceof IfNode) {
    const test = lowerNode(node.test, scope, false, state);
    if (test === UNSUPPORTED) return UNSUPPORTED;
    const then = lowerNode(node.consequent, scope, tail, state);
    if (then === UNSUPPORTED) return UNSUPPORTED;
    const other = lowerNode(node.alternative, scope, tail, state);
    if (other === UNSUPPORTED) return UNSUPPORTED;
    return {
      k: 'if', test, then, else: other, tail,
      callable: then.callable === true && other.callable === true
    };
  }

  if (node instanceof BeginNode) {
    const body = lowerSequence(node.expressions, scope, tail, state);
    if (body === UNSUPPORTED) return UNSUPPORTED;
    return {
      k: 'seq', exprs: body, tail,
      callable: body.length > 0 && body[body.length - 1].callable === true
    };
  }

  if (node instanceof LambdaNode) {
    const inner = new Scope(scope);
    for (const p of node.params) inner.declare(p);
    if (node.restParam) inner.declare(node.restParam);
    const body = lowerBody(node.body, inner, state);
    if (body === UNSUPPORTED) return UNSUPPORTED;
    return {
      k: 'lambda', params: node.params, rest: node.restParam,
      name: node.name, body, tail, callable: true
    };
  }

  if (node instanceof LetNode) {
    const init = lowerNode(node.binding, scope, false, state);
    if (init === UNSUPPORTED) return UNSUPPORTED;
    const inner = new Scope(scope);
    if (init.k === 'lambda') inner.declareCallable(node.varName);
    else inner.declare(node.varName);
    const body = lowerNode(node.body, inner, tail, state);
    if (body === UNSUPPORTED) return UNSUPPORTED;
    return { k: 'let', name: node.varName, init, body, tail, callable: body.callable === true };
  }

  if (node instanceof LetRecNode) {
    // Every name is in scope in every initializer, which is what makes the
    // group mutually recursive, and every initializer is a lambda -- the
    // analyzer only builds this node for that shape. Declaring them all
    // callable before lowering any of them is what lets a recursive or mutually
    // recursive call be recognised as a callee the compiler can name, rather
    // than as an unknown that the safety analysis has to refuse.
    const inner = new Scope(scope);
    for (const name of node.names) inner.declareCallable(name);

    const inits = [];
    for (const lambdaExpr of node.lambdaExprs) {
      const lowered = lowerNode(lambdaExpr, inner, false, state);
      if (lowered === UNSUPPORTED) return UNSUPPORTED;
      inits.push(lowered);
    }

    const body = lowerNode(node.body, inner, tail, state);
    if (body === UNSUPPORTED) return UNSUPPORTED;
    return {
      k: 'letrec', names: node.names, inits, body, tail,
      callable: body.callable === true
    };
  }

  if (node instanceof SetNode) {
    const value = lowerNode(node.valueExpr ?? node.value, scope, false, state);
    if (value === UNSUPPORTED) return UNSUPPORTED;
    const local = scope.has(node.name);
    if (!local) state.globals.add(node.name);
    else state.assignedLocals.add(node.name);
    return { k: 'set', name: node.name, local, value, tail };
  }

  if (node instanceof DefineNode) {
    // Only reachable for an internal definition; the top-level case is handled
    // by the caller. The analyzer has already hoisted the name into scope.
    const value = lowerNode(node.valueExpr ?? node.value, scope, false, state);
    if (value === UNSUPPORTED) return UNSUPPORTED;
    if (value.k === 'lambda') scope.declareCallable(node.name);
    else scope.declare(node.name);
    return { k: 'define', name: node.name, value, tail };
  }

  if (node instanceof TailAppNode) {
    const fn = lowerNode(node.funcExpr, scope, false, state);
    if (fn === UNSUPPORTED) return UNSUPPORTED;
    const args = [];
    for (const arg of node.argExprs) {
      const lowered = lowerNode(arg, scope, false, state);
      if (lowered === UNSUPPORTED) return UNSUPPORTED;
      args.push(lowered);
    }
    // Whether the callee is something this pass can name. A global can be
    // looked up and followed; a lambda and a local bound to one were lowered
    // here, so their references are already recorded. A bare parameter is not:
    // it is whatever the caller handed over, and it may capture a continuation
    // without anything in this procedure's text saying so. That is the
    // `btsearch` shape, where `enumerate` invokes `cont`.
    if (fn.callable !== true) state.callsUnknown = true;
    else if (fn.k === 'local') state.calledLocals.add(fn.name);
    return { k: 'call', fn, args, tail };
  }

  return state.fail(`unsupported node: ${node?.constructor?.name ?? String(node)}`);
}

/**
 * Lowers a sequence of expressions, marking only the last as tail.
 * @param {Array<Object>} nodes - Analyzed nodes.
 * @param {Scope} scope - Enclosing scope.
 * @param {boolean} tail - Whether the sequence is in tail position.
 * @param {Lowering} state - Lowering state.
 * @returns {Array<Object>|symbol} IR nodes, or UNSUPPORTED.
 */
function lowerSequence(nodes, scope, tail, state) {
  const out = [];
  for (let i = 0; i < nodes.length; i++) {
    const isLast = i === nodes.length - 1;
    const lowered = lowerNode(nodes[i], scope, isLast && tail, state);
    if (lowered === UNSUPPORTED) return UNSUPPORTED;
    out.push(lowered);
  }
  return out;
}

/**
 * Lowers a procedure body, which is in tail position by definition.
 * @param {Object} node - The body node.
 * @param {Scope} scope - The procedure's scope.
 * @param {Lowering} state - Lowering state.
 * @returns {Object|symbol} IR node, or UNSUPPORTED.
 */
function lowerBody(node, scope, state) {
  // Internal definitions have to be declared before the body is lowered, or a
  // forward reference between two internal procedures would be mistaken for a
  // global. The analyzer has already hoisted them syntactically.
  if (node instanceof BeginNode) {
    for (const expr of node.expressions) {
      if (!(expr instanceof DefineNode)) continue;
      // An internal definition of a procedure is a callee this pass can name,
      // so calling it is not calling an unknown. Declared before the body is
      // lowered, or a forward reference between two internal procedures would
      // be mistaken for a global.
      const value = expr.valueExpr ?? expr.value;
      if (value instanceof LambdaNode) scope.declareCallable(expr.name);
      else scope.declare(expr.name);
    }
  } else if (node instanceof DefineNode) {
    const value = node.valueExpr ?? node.value;
    if (value instanceof LambdaNode) scope.declareCallable(node.name);
    else scope.declare(node.name);
  }
  return lowerNode(node, scope, true, state);
}

/**
 * Lowers a lambda to IR, reporting what it references.
 *
 * Lowering failure and *safety* are kept apart, because they are different
 * questions with different answers. A form the compiler cannot express is a
 * failure and is reported as `reason`. A form it can express but should not
 * compile -- because a continuation may be captured during its extent -- is a
 * judgement the caller makes, and needs more than this one lambda to make: see
 * `controlGlobalIn` for the local part of it and `compileProgram` for the
 * call-graph closure over it.
 *
 * @param {Object} lambdaNode - An analyzed `LambdaNode`.
 * @returns {{ir: Object, globals: Set<string>, callsUnknown: boolean}
 *   |{reason: string}} The IR with what it references, or why it could not be
 *   lowered.
 */
export function lowerLambda(lambdaNode) {
  const state = new Lowering();
  const ir = lowerNode(lambdaNode, new Scope(null), false, state);
  if (ir === UNSUPPORTED) {
    return { reason: state.reason ?? 'unsupported form' };
  }
  // A local that was both called and assigned is not the lambda we lowered.
  let callsUnknown = state.callsUnknown;
  if (!callsUnknown) {
    for (const name of state.calledLocals) {
      if (state.assignedLocals.has(name)) { callsUnknown = true; break; }
    }
  }
  return { ir, globals: state.globals, callsUnknown };
}

/**
 * Returns the first control-transferring global in a set, or null.
 *
 * This is the base case of the safety analysis: a procedure that names
 * `call/cc` or `dynamic-wind` itself is obviously unsafe to compile. It is only
 * the base case -- `make-maze` names neither and is still unsafe, because
 * `dig-maze` escapes through it (R34).
 *
 * @param {Set<string>} globals - Globals a form references.
 * @returns {string|null} The offending name, or null.
 */
export function controlGlobalIn(globals) {
  for (const name of globals) {
    if (CONTROL_GLOBALS.has(name)) return name;
  }
  return null;
}
