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
  Define // This was in the original, but removed in the provided snippet. Keeping it for now as other parts of the file might use it.
} from './ast.js';
import { globalMacroRegistry } from './macro_registry.js';
import { compileSyntaxRules } from './syntax_rules.js';
import { Cons, cons, list, car, cdr, mapCons, toArray } from '../data/cons.js'; // Added car, cdr, mapCons, toArray from snippet
import { Symbol, intern } from '../data/symbol.js';


/**
 * Analyzes an S-expression and converts it to our AST object tree.
 * @param {*} exp - An S-expression (Cons, Symbol, or primitive).
 * @returns {Executable} An AST node.
 */
export function analyze(exp) {
  // 1. Handle Atoms
  if (exp instanceof Symbol) {
    return new Variable(exp.name);
  }
  if (typeof exp === 'number' || typeof exp === 'string' || typeof exp === 'boolean' || exp === null) {
    return new Literal(exp);
  }
  if (Array.isArray(exp)) {
    return new Literal(exp); // Vectors (Arrays) are self-evaluating
  }
  if (exp instanceof Executable) {
    return exp; // Already analyzed (e.g. from macro expansion)
  }

  // 2. Handle Lists (Cons)
  if (exp instanceof Cons) {
    const tag = exp.car;

    // Check for special forms
    if (tag instanceof Symbol) {
      // Macro Expansion
      if (globalMacroRegistry.isMacro(tag.name)) { // Original logic
        const transformer = globalMacroRegistry.lookup(tag.name); // Original logic
        const expanded = transformer(exp);
        return analyze(expanded);
      }

      switch (tag.name) {
        case 'if':
          return analyzeIf(exp);
        case 'let':
          return analyzeLet(exp);
        case 'letrec':
          return analyzeLetRec(exp);
        case 'lambda':
          return analyzeLambda(exp);
        case 'set!':
          return analyzeSet(exp);
        case 'define':
          return analyzeDefine(exp);
        case 'call/cc':
          return new CallCC(analyze(cadr(exp)));
        case 'begin':
          return new Begin(mapCons(exp.cdr, analyze));
        case 'quote':
          return new Literal(unwrapSyntax(cadr(exp)));
        case 'define-syntax':
          return analyzeDefineSyntax(exp);
        case 'quasiquote':
          return expandQuasiquote(cadr(exp));
      }
    }

    // Function Application
    const func = analyze(exp.car);
    const args = mapCons(exp.cdr, analyze);
    return new TailApp(func, args);
  }

  throw new Error(`Analyzer error: Unknown expression type: ${exp}`);
}

// --- Special Form Handlers ---

function analyzeIf(exp) {
  // (if test consequent alternative)
  const test = analyze(cadr(exp));
  const consequent = analyze(caddr(exp));
  const alternative = cdddr(exp) !== null ? analyze(cadddr(exp)) : new Literal(null);
  return new If(test, consequent, alternative);
}

function analyzeLet(exp) {
  // (let ((var binding) ...) body...)
  const bindings = cadr(exp);
  const body = cddr(exp);

  const vars = [];
  const args = [];

  let curr = bindings;
  while (curr instanceof Cons) {
    const pair = curr.car; // (var binding)
    vars.push(car(pair).name);
    args.push(analyze(cadr(pair)));
    curr = curr.cdr;
  }

  const bodyExprs = mapCons(body, analyze);
  const bodyAST = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);

  // Desugar to Let AST node (which compiles to LetFrame)
  // We can construct the Let node directly
  // But wait, Let AST node takes (varName, binding, body).
  // Our Let AST node only supports SINGLE binding?
  // Let's check ast.js.
  // ast.js Let: constructor(varName, binding, body).
  // It seems our Let AST node is for a SINGLE let binding?
  // No, standard Scheme let allows multiple.
  // If ast.js Let only supports one, we need to nest them or change ast.js.
  // Looking at ast.js:
  // export class Let extends Executable { constructor(varName, binding, body) ... }
  // It seems it only supports ONE variable.
  // So (let ((x 1) (y 2)) body) must be desugared to nested Lets?
  // Or (let ((x 1) (y 2)) body) -> ((lambda (x y) body) 1 2).
  // The latter is the standard expansion.
  // Let's use the lambda expansion for multi-var let.

  return new TailApp(
    new Lambda(vars, bodyAST),
    args
  );
}

function analyzeLetRec(exp) {
  // (letrec ((var val) ...) body...)
  // Our LetRec AST node supports single binding?
  // ast.js LetRec: constructor(varName, lambdaExpr, body)
  // It seems it's designed for single recursive binding.
  // For multiple, we need a more complex desugaring or update AST.
  // For now, let's assume single binding for LetRec AST or implement full letrec desugaring.
  // (letrec ((f (lambda ...))) body)

  const bindings = cadr(exp);
  // Support single binding for now to match AST capability
  const pair = car(bindings);
  const varName = car(pair).name;
  const valExpr = analyze(cadr(pair));
  const body = cddr(exp);

  const bodyExprs = mapCons(body, analyze);
  const bodyAST = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);

  return new LetRec(varName, valExpr, bodyAST);
}

function analyzeLambda(exp) {
  // (lambda (params...) body...)
  const paramsList = cadr(exp);
  const body = cddr(exp);

  const params = [];
  let curr = paramsList;
  while (curr instanceof Cons) {
    params.push(curr.car.name);
    curr = curr.cdr;
  }
  // TODO: Handle improper list for rest args (curr is Symbol)

  if (body === null) {
    throw new Error("Malformed lambda: body cannot be empty");
  }

  const bodyExprs = mapCons(body, analyze);
  const bodyAST = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);

  return new Lambda(params, bodyAST);
}

function analyzeSet(exp) {
  // (set! var val)
  return new Set(cadr(exp).name, analyze(caddr(exp)));
}

function analyzeDefine(exp) {
  // (define var val) or (define (f args) body)
  const head = cadr(exp);

  if (head instanceof Symbol) {
    // (define var val)
    return new Define(head.name, analyze(caddr(exp)));
  } else if (head instanceof Cons) {
    // (define (f args...) body...)
    const funcName = car(head).name;
    const argsList = cdr(head);
    const body = cddr(exp);

    // Construct Lambda
    const params = [];
    let curr = argsList;
    while (curr instanceof Cons) {
      params.push(curr.car.name);
      curr = curr.cdr;
    }

    const bodyExprs = mapCons(body, analyze);
    const bodyAST = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);

    return new Define(funcName, new Lambda(params, bodyAST));
  }
  throw new Error("Malformed define");
}

function analyzeDefineSyntax(exp) {
  // (define-syntax name transformer)
  const name = cadr(exp).name;
  const transformerSpec = caddr(exp);

  // Check for (syntax-rules ...)
  if (transformerSpec instanceof Cons &&
    car(transformerSpec) instanceof Symbol &&
    car(transformerSpec).name === 'syntax-rules') {

    let literalsList = cadr(transformerSpec);
    const clausesList = cddr(transformerSpec);

    const literals = [];
    let curr = literalsList;
    while (curr instanceof Cons) {
      literals.push(curr.car); // Keep as Symbols
      curr = curr.cdr;
    }

    const clauses = [];
    curr = clausesList;
    while (curr instanceof Cons) {
      const clause = curr.car; // (pattern template)
      clauses.push([car(clause), cadr(clause)]);
      curr = curr.cdr;
    }

    const transformer = compileSyntaxRules(literals, clauses);
    globalMacroRegistry.define(name, transformer);
    return new Literal(null);
  }
  return new Literal(null);
}

// --- Helpers ---

// mapCons, car, cdr are imported from ../data/cons.js

function cadr(cons) { return cons.cdr.car; }
function cddr(cons) { return cons.cdr.cdr; }
function caddr(cons) { return cons.cdr.cdr.car; }
function cdddr(cons) { return cons.cdr.cdr.cdr; }
function cadddr(cons) { return cons.cdr.cdr.cdr.car; }

/**
 * Unwraps syntax (Symbol -> Variable, Cons -> Array) for Quote.
 * Actually, Quote should return the raw data (Symbol, Cons).
 * But our AST `Literal` wraps the value.
 * If we wrap `Symbol` in `Literal`, `interpreter` needs to handle it.
 * Currently `Literal` returns `this.value`.
 * So `(quote x)` -> `Literal(Symbol(x))`.
 * `(quote (1 2))` -> `Literal(Cons(1, Cons(2, null)))`.
 */
function unwrapSyntax(exp) {
  return exp; // Pass through the raw data structure
}

// --- Quasiquote Expansion (Updated for Cons) ---

function expandQuasiquote(exp, nesting = 0) {
  // 1. Handle (quasiquote x)
  if (isTaggedList(exp, 'quasiquote')) {
    return listApp('list', [
      new Literal(intern('quasiquote')),
      expandQuasiquote(cadr(exp), nesting + 1)
    ]);
  }

  // 2. Handle (unquote x)
  if (isTaggedList(exp, 'unquote')) {
    if (nesting === 0) {
      return analyze(cadr(exp));
    } else {
      return listApp('list', [
        new Literal(intern('unquote')),
        expandQuasiquote(cadr(exp), nesting - 1)
      ]);
    }
  }

  // 3. Handle (unquote-splicing x)
  if (isTaggedList(exp, 'unquote-splicing')) {
    if (nesting === 0) {
      throw new Error("unquote-splicing not allowed at top level");
    } else {
      return listApp('list', [
        new Literal(intern('unquote-splicing')),
        expandQuasiquote(cadr(exp), nesting - 1)
      ]);
    }
  }

  // 4. Handle Lists (Cons)
  if (exp instanceof Cons) {
    // Check for splicing in car
    if (isTaggedList(exp.car, 'unquote-splicing') && nesting === 0) {
      // (unquote-splicing x) . rest
      // -> (append x (expand rest))
      return listApp('append', [
        analyze(cadr(exp.car)),
        expandQuasiquote(exp.cdr, nesting)
      ]);
    }

    // Regular cons: (cons (expand car) (expand cdr))
    return listApp('cons', [
      expandQuasiquote(exp.car, nesting),
      expandQuasiquote(exp.cdr, nesting)
    ]);
  }

  // 5. Atoms
  return new Literal(exp);
}

function isTaggedList(exp, tag) {
  return (exp instanceof Cons) &&
    (exp.car instanceof Symbol) &&
    (exp.car.name === tag);
}

function listApp(funcName, args) {
  return new TailApp(new Variable(funcName), args);
}
