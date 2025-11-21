import {
  Executable,
  Literal,
  Variable,
  Lambda,
  Let,
  LetRec,
  If,
  Set,
  TailApp,
  CallCC,
  Begin
} from './ast.js';

/**
 * Analyzes an S-expression and converts it to our AST object tree.
 * @param {*} exp - An S-expression (from the parser).
 * @returns {Executable} An AST node.
 */
export function analyze(exp) {
  // If the reader already gave us an AST node, just return it.
  if (exp instanceof Executable) {
    return exp;
  }

  if (exp === undefined) {
    throw new Error(`Analyzer error: received 'undefined' expression.`);
  }
  if (!Array.isArray(exp)) {
    throw new Error(`Analyzer error: expression is not a list or atom: ${exp}`);
  }
  if (exp.length === 0) { // '()
    return new Literal(null);
  }

  // It's a list, so it's a special form or an application.
  const tag = exp[0];

  // Check for special forms
  if (tag instanceof Variable) {
    switch (tag.name) {
      case 'if':
        // (if test consequent alternative)
        return new If(analyze(exp[1]), analyze(exp[2]), analyze(exp[3]));
      case 'let': {
        // (let ((var binding)) body)
        const bindingPair = exp[1][0]; // [Variable('x'), Literal(10)]
        const varName = bindingPair[0].name;
        const binding = analyze(bindingPair[1]);
        const body = analyze(exp[2]);
        return new Let(varName, binding, body);
      }
      case 'letrec': {
        // (letrec ((var lambda-exp)) body)
        const bindingPair = exp[1][0]; // [Variable('f'), [Variable('lambda'), ...]]
        const varName = bindingPair[0].name;
        const lambdaExp = analyze(bindingPair[1]);
        const body = analyze(exp[2]);
        return new LetRec(varName, lambdaExp, body);
      }
      case 'lambda':
        // (lambda (params...) body1 body2 ...)
        if (exp.length < 3) {
          throw new SyntaxError(`Malformed lambda, missing body: ${exp}`);
        }
        const params = exp[1].map(p => p.name);
        const bodyExprs = exp.slice(2).map(analyze);
        const body = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);
        return new Lambda(params, body);
      case 'set!':
        return new Set(exp[1].name, analyze(exp[2]));
      case 'call/cc':
        return new CallCC(analyze(exp[1]));
      case 'begin':
        return new Begin(exp.slice(1).map(analyze));
    }
  }

  // It's a function application
  return new TailApp(analyze(exp[0]), exp.slice(1).map(analyze));
}
