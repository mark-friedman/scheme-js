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
    if (scope.has(node.name)) return { k: 'local', name: node.name, tail };
    state.globals.add(node.name);
    return { k: 'global', name: node.name, tail };
  }

  if (node instanceof IfNode) {
    const test = lowerNode(node.test, scope, false, state);
    if (test === UNSUPPORTED) return UNSUPPORTED;
    const then = lowerNode(node.consequent, scope, tail, state);
    if (then === UNSUPPORTED) return UNSUPPORTED;
    const other = lowerNode(node.alternative, scope, tail, state);
    if (other === UNSUPPORTED) return UNSUPPORTED;
    return { k: 'if', test, then, else: other, tail };
  }

  if (node instanceof BeginNode) {
    const body = lowerSequence(node.expressions, scope, tail, state);
    return body === UNSUPPORTED ? UNSUPPORTED : { k: 'seq', exprs: body, tail };
  }

  if (node instanceof LambdaNode) {
    const inner = new Scope(scope);
    for (const p of node.params) inner.declare(p);
    if (node.restParam) inner.declare(node.restParam);
    const body = lowerBody(node.body, inner, state);
    if (body === UNSUPPORTED) return UNSUPPORTED;
    return {
      k: 'lambda', params: node.params, rest: node.restParam,
      name: node.name, body, tail
    };
  }

  if (node instanceof LetNode) {
    const init = lowerNode(node.binding, scope, false, state);
    if (init === UNSUPPORTED) return UNSUPPORTED;
    const inner = new Scope(scope);
    inner.declare(node.varName);
    const body = lowerNode(node.body, inner, tail, state);
    if (body === UNSUPPORTED) return UNSUPPORTED;
    return { k: 'let', name: node.varName, init, body, tail };
  }

  if (node instanceof LetRecNode) {
    // The bound name is visible in its own initializer, which is the point.
    const inner = new Scope(scope);
    inner.declare(node.varName);
    const init = lowerNode(node.lambdaExpr, inner, false, state);
    if (init === UNSUPPORTED) return UNSUPPORTED;
    const body = lowerNode(node.body, inner, tail, state);
    if (body === UNSUPPORTED) return UNSUPPORTED;
    return { k: 'letrec', name: node.varName, init, body, tail };
  }

  if (node instanceof SetNode) {
    const value = lowerNode(node.valueExpr ?? node.value, scope, false, state);
    if (value === UNSUPPORTED) return UNSUPPORTED;
    const local = scope.has(node.name);
    if (!local) state.globals.add(node.name);
    return { k: 'set', name: node.name, local, value, tail };
  }

  if (node instanceof DefineNode) {
    // Only reachable for an internal definition; the top-level case is handled
    // by the caller. The analyzer has already hoisted the name into scope.
    const value = lowerNode(node.valueExpr ?? node.value, scope, false, state);
    if (value === UNSUPPORTED) return UNSUPPORTED;
    scope.declare(node.name);
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
      if (expr instanceof DefineNode) scope.declare(expr.name);
    }
  } else if (node instanceof DefineNode) {
    scope.declare(node.name);
  }
  return lowerNode(node, scope, true, state);
}

/**
 * Lowers a lambda to IR, reporting which globals it references.
 *
 * @param {Object} lambdaNode - An analyzed `LambdaNode`.
 * @returns {{ir: Object, globals: Set<string>}|{reason: string}} The IR and the
 *   globals it needs, or a reason it could not be lowered.
 */
export function lowerLambda(lambdaNode) {
  const state = new Lowering();
  const ir = lowerNode(lambdaNode, new Scope(null), false, state);
  if (ir === UNSUPPORTED) {
    return { reason: state.reason ?? 'unsupported form' };
  }
  for (const name of state.globals) {
    if (CONTROL_GLOBALS.has(name)) {
      return { reason: `references control global '${name}'` };
    }
  }
  return { ir, globals: state.globals };
}
