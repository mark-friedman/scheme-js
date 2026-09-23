/**
 * @fileoverview Marshalling between the JavaScript AST and the Scheme lowering.
 *
 * `ir.scm` works on Scheme data, because that is what a compiler written in
 * Scheme would be handed. The analyzer in front of it is still JavaScript, so
 * its output is converted on the way in and the IR is converted back on the way
 * out for code generation, which is also still JavaScript.
 *
 * This module is therefore a measure of how far the port has got: every field
 * it converts is a field two languages have to agree about. It shrinks as more
 * of the compiler moves across, and disappears when the analyzer produces
 * Scheme data and the emitter consumes it.
 *
 * Its cost is small enough not to weigh on that decision: about 8 ms against
 * 66 ms of lowering, over the 952-lambda corpus the port was measured on.
 *
 * ## The two representations
 *
 * An analyzed AST node becomes a list whose head is a tag:
 *
 *     (lit value) (var name) (if test then else) (seq exprs)
 *     (lambda params rest name body) (let var init body)
 *     (letrec names inits body) (set name value) (define name value)
 *     (app fn args) (other description)
 *
 * and an IR node comes back in the same shape, with the fields in the order
 * `ir.scm` documents. Names are symbols on the Scheme side so that membership
 * tests are pointer comparisons.
 */

import {
  LiteralNode, VariableNode, LambdaNode, LetNode, LetRecNode, IfNode,
  SetNode, DefineNode, TailAppNode, BeginNode
} from '../core/interpreter/ast_nodes.js';
import { Cons } from '../core/interpreter/cons.js';
import { intern } from '../core/interpreter/symbol.js';

/**
 * Builds a Scheme list from a JavaScript array.
 * @param {Array<*>} items - The elements.
 * @returns {*} A proper Scheme list.
 */
export function toList(items) {
  let out = null;
  for (let i = items.length - 1; i >= 0; i--) out = new Cons(items[i], out);
  return out;
}

/**
 * Reads a Scheme list into a JavaScript array.
 * @param {*} list - A proper Scheme list.
 * @returns {Array<*>} Its elements.
 */
export function toArray(list) {
  const out = [];
  while (list !== null) { out.push(list.car); list = list.cdr; }
  return out;
}

/**
 * Converts an analyzed AST node into the tagged-list form `ir.scm` reads.
 *
 * The cases are in the order `lower-node` dispatches on them, so the two can be
 * read side by side.
 *
 * An unrecognised node becomes `(other description)` rather than an error. The
 * lowering is partial by design -- anything outside the compiler's subset is
 * declined and left to the interpreter -- and a node this does not know about
 * is exactly that case, reaching the decision one step earlier.
 *
 * @param {Object} node - An analyzed AST node.
 * @returns {*} A Scheme list.
 */
export function astToScheme(node) {
  if (node instanceof LiteralNode) return toList([intern('lit'), node.value]);

  if (node instanceof VariableNode) return toList([intern('var'), intern(node.name)]);

  if (node instanceof IfNode) {
    return toList([intern('if'), astToScheme(node.test),
      astToScheme(node.consequent), astToScheme(node.alternative)]);
  }

  if (node instanceof BeginNode) {
    return toList([intern('seq'), toList(node.expressions.map(astToScheme))]);
  }

  if (node instanceof LambdaNode) {
    return toList([intern('lambda'),
      toList(node.params.map((p) => intern(p))),
      node.restParam ? intern(node.restParam) : false,
      node.name === undefined || node.name === null ? false : node.name,
      astToScheme(node.body)]);
  }

  if (node instanceof LetNode) {
    return toList([intern('let'), intern(node.varName),
      astToScheme(node.binding), astToScheme(node.body)]);
  }

  if (node instanceof LetRecNode) {
    return toList([intern('letrec'),
      toList(node.names.map((n) => intern(n))),
      toList(node.lambdaExprs.map(astToScheme)),
      astToScheme(node.body)]);
  }

  if (node instanceof SetNode) {
    return toList([intern('set'), intern(node.name),
      astToScheme(node.valueExpr ?? node.value)]);
  }

  if (node instanceof DefineNode) {
    return toList([intern('define'), intern(node.name),
      astToScheme(node.valueExpr ?? node.value)]);
  }

  if (node instanceof TailAppNode) {
    return toList([intern('app'), astToScheme(node.funcExpr),
      toList(node.argExprs.map(astToScheme))]);
  }

  return toList([intern('other'),
    `unsupported node: ${node?.constructor?.name ?? String(node)}`]);
}

/**
 * Converts an IR node produced by `ir.scm` into the object form the emitter
 * consumes.
 *
 * @param {*} ir - A Scheme IR node.
 * @returns {Object} The equivalent JavaScript IR node.
 */
export function irToJs(ir) {
  const tag = ir.car.name;
  const f = toArray(ir.cdr);

  switch (tag) {
    case 'const':
      return { k: 'const', value: f[0], tail: f[1] };
    case 'local':
      return { k: 'local', name: f[0].name, tail: f[1], callable: f[2] };
    case 'global':
      return { k: 'global', name: f[0].name, tail: f[1], callable: f[2] };
    case 'if':
      return {
        k: 'if', test: irToJs(f[0]), then: irToJs(f[1]), else: irToJs(f[2]),
        tail: f[3], callable: f[4]
      };
    case 'seq':
      return {
        k: 'seq', exprs: toArray(f[0]).map(irToJs), tail: f[1], callable: f[2]
      };
    case 'lambda':
      return {
        k: 'lambda',
        params: toArray(f[0]).map((s) => s.name),
        rest: f[1] === false ? null : f[1].name,
        name: f[2] === false ? undefined : f[2],
        body: irToJs(f[3]), tail: f[4], callable: f[5]
      };
    case 'let':
      return {
        k: 'let', name: f[0].name, init: irToJs(f[1]), body: irToJs(f[2]),
        tail: f[3], callable: f[4]
      };
    case 'letrec':
      return {
        k: 'letrec',
        names: toArray(f[0]).map((s) => s.name),
        inits: toArray(f[1]).map(irToJs),
        body: irToJs(f[2]), tail: f[3], callable: f[4],
        inline: f[5] === true
      };
    case 'set':
      return {
        k: 'set', name: f[0].name, local: f[1], value: irToJs(f[2]), tail: f[3]
      };
    case 'define':
      return { k: 'define', name: f[0].name, value: irToJs(f[1]), tail: f[2] };
    case 'call':
      // `loop` is absent on the calls the lowering synthesizes, which never loop.
      return {
        k: 'call', fn: irToJs(f[0]), args: toArray(f[1]).map(irToJs), tail: f[2],
        loop: f[3] ? f[3].name : false
      };
    case 'capture':
      return { k: 'capture', receiver: irToJs(f[0]), tail: f[1] };
    default:
      throw new Error(`irToJs: unknown IR tag '${tag}'`);
  }
}
