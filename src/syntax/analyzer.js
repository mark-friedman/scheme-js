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
  Begin,
  Define
} from './ast.js';
import { globalMacroRegistry } from './macro_registry.js';

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
    // 1. Check for Macro Expansion
    if (globalMacroRegistry.isMacro(tag.name)) {
      const transformer = globalMacroRegistry.lookup(tag.name);
      const expanded = transformer(exp);
      return analyze(expanded);
    }

    // 2. Check for Core Special Forms
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
      case 'define': {
        // Case 1: (define var value)
        if (exp[1] instanceof Variable) {
          return new Define(exp[1].name, analyze(exp[2]));
        }
        // Case 2: (define (func args...) body...)
        // This is sugar for (define func (lambda (args...) body...))
        if (Array.isArray(exp[1]) && exp[1].length > 0 && exp[1][0] instanceof Variable) {
          const funcVar = exp[1][0];
          const params = exp[1].slice(1).map(p => p.name);
          const bodyExprs = exp.slice(2).map(analyze);
          const body = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);
          return new Define(funcVar.name, new Lambda(params, body));
        }
        throw new SyntaxError(`Malformed define: ${exp}`);
      }
      case 'call/cc':
        return new CallCC(analyze(exp[1]));
      case 'begin':
        return new Begin(exp.slice(1).map(analyze));
      case 'quote':
        return new Literal(unwrapAST(exp[1]));
      case 'define-syntax':
        // (define-syntax name transformer)
        if (exp.length !== 3 || !(exp[1] instanceof Variable)) {
          throw new SyntaxError(`Malformed define-syntax: ${exp}`);
        }
        // TODO: Evaluate transformer and register it.
        // For Phase 1, we rely on manual registration in tests.
        return new Literal(null);
      case 'quasiquote':
        return expandQuasiquote(exp[1]);
    }
  }

  // It's a function application
  return new TailApp(analyze(exp[0]), exp.slice(1).map(analyze));
}

function unwrapAST(exp) {
  if (exp instanceof Literal) {
    return exp.value;
  }
  if (exp instanceof Variable) {
    return exp; // Symbols remain as Variable objects
  }
  if (Array.isArray(exp)) {
    return exp.map(unwrapAST);
  }
  return exp;
}

/**
 * Expands a quasiquote expression into list construction calls.
 * @param {*} exp - The expression inside the quasiquote.
 * @returns {Executable}
 */
/**
 * Expands a quasiquote expression into list construction calls.
 * @param {*} exp - The expression inside the quasiquote.
 * @param {number} nesting - The current nesting level of quasiquotes (0 = top).
 * @returns {Executable}
 */
function expandQuasiquote(exp, nesting = 0) {
  // 1. Handle (quasiquote x) - Increment nesting
  if (Array.isArray(exp) && exp.length === 2 &&
    exp[0] instanceof Variable && exp[0].name === 'quasiquote') {
    return new TailApp(new Variable('list'), [
      new Literal(new Variable('quasiquote')),
      expandQuasiquote(exp[1], nesting + 1)
    ]);
  }

  // 2. Handle (unquote x)
  if (Array.isArray(exp) && exp.length === 2 &&
    exp[0] instanceof Variable && exp[0].name === 'unquote') {
    if (nesting === 0) {
      return analyze(exp[1]);
    } else {
      return new TailApp(new Variable('list'), [
        new Literal(new Variable('unquote')),
        expandQuasiquote(exp[1], nesting - 1)
      ]);
    }
  }

  // 3. Handle (unquote-splicing x) - Error if not in list, or reconstruct if nested
  if (Array.isArray(exp) && exp.length === 2 &&
    exp[0] instanceof Variable && exp[0].name === 'unquote-splicing') {
    if (nesting === 0) {
      throw new SyntaxError("unquote-splicing not allowed at top level of quasiquote");
    } else {
      return new TailApp(new Variable('list'), [
        new Literal(new Variable('unquote-splicing')),
        expandQuasiquote(exp[1], nesting - 1)
      ]);
    }
  }

  // 4. Handle Lists (Arrays)
  if (Array.isArray(exp)) {
    const terms = [];
    let currentList = [];

    for (const item of exp) {
      // Check for unquote-splicing at nesting 0: (unquote-splicing x)
      if (nesting === 0 && Array.isArray(item) && item.length === 2 &&
        item[0] instanceof Variable && item[0].name === 'unquote-splicing') {
        // Flush current list
        if (currentList.length > 0) {
          terms.push(new TailApp(new Variable('list'), currentList));
          currentList = [];
        }
        // Add the spliced term
        terms.push(analyze(item[1]));
      } else {
        // Regular item (recursive expansion)
        currentList.push(expandQuasiquote(item, nesting));
      }
    }

    // Flush remaining items
    if (currentList.length > 0) {
      terms.push(new TailApp(new Variable('list'), currentList));
    }

    if (terms.length === 0) {
      return new Literal(null); // Empty list
    }
    if (terms.length === 1) {
      return terms[0];
    }
    return new TailApp(new Variable('append'), terms);
  }

  // 5. Handle Atoms (Literals, Variables, etc.)
  if (exp instanceof Literal) {
    return exp;
  }
  // For symbols (Variables) inside quasiquote, we must quote them
  // unless we are just returning them as Literals?
  // Wait, if I return new Literal(exp), it evaluates to exp (the Variable object).
  // Yes, that's what we want.
  return new Literal(exp);
}
