/**
 * @fileoverview Marshalling between the JavaScript AST and the Scheme lowering.
 *
 * `ir.scm` works on Scheme data, because that is what a compiler written in
 * Scheme would be handed. The analyzer in front of it is still JavaScript, so
 * its output is converted on the way in. The IR is not converted on the way
 * out: code generation is Scheme too, and reads it as it is.
 *
 * This module is therefore a measure of how far the port has got: every field
 * it converts is a field two languages have to agree about. What is left of it
 * disappears when the analyzer produces Scheme data.
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
 *     (app fn args span) (other description)
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

  // An application carries where it was read from, for the source map of the
  // code generated for it (`sourcemap.scm`). This module goes when the
  // expander is Scheme, which will hand the compiler its spans itself; until
  // then the span is passed here, the one field added since.
  if (node instanceof TailAppNode) {
    return toList([intern('app'), astToScheme(node.funcExpr),
      toList(node.argExprs.map(astToScheme)), node.source ?? false]);
  }

  return toList([intern('other'),
    `unsupported node: ${node?.constructor?.name ?? String(node)}`]);
}

