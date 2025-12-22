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
  Define,
  ImportNode,
  ScopedVariable
} from './ast.js';
import { getLibraryExports, applyImports, parseImportSet } from './library_loader.js';
import { globalMacroRegistry } from './macro_registry.js';
import { compileSyntaxRules } from './syntax_rules.js';
import { Cons, cons, list, car, cdr, mapCons, toArray, cadr, cddr, caddr, cdddr, cadddr } from './cons.js';
import { Symbol, intern } from './symbol.js';
import { SyntaxObject, globalScopeRegistry, freshScope, getCurrentDefiningScopes, GLOBAL_SCOPE_ID, registerBindingWithCurrentScopes, syntaxName, isSyntaxObject } from './syntax_object.js';


/**
 * Analyzes an S-expression and converts it to our AST object tree.
 * @param {*} exp - An S-expression (Cons, Symbol, or primitive).
 * @returns {Executable} An AST node.
 */
export function analyze(exp) {
  // 1. Handle Atoms

  // SyntaxObjects carry scope marks for referential transparency
  // They become ScopedVariables that check the scope registry first
  if (exp instanceof SyntaxObject) {
    return new ScopedVariable(exp.name, exp.scopes, globalScopeRegistry);
  }

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
      if (globalMacroRegistry.isMacro(tag.name)) {
        const transformer = globalMacroRegistry.lookup(tag.name);
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
        case 'call-with-current-continuation':
          return new CallCC(analyze(cadr(exp)));
        case 'begin':
          return new Begin(mapCons(exp.cdr, analyze));
        case 'quote':
          return new Literal(cadr(exp)); // Pass through raw data structure
        case 'define-syntax':
          return analyzeDefineSyntax(exp);
        case 'quasiquote':
          return expandQuasiquote(cadr(exp));
        case 'import':
          return analyzeImport(exp);
      }
    }

    // Function Application
    const func = analyze(exp.car);
    const args = mapCons(exp.cdr, analyze);
    return new TailApp(func, args);
  }

  throw new Error(`Analyzer error: Unknown expression type: ${exp}`);
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Analyzes a body (sequence of expressions) and returns appropriate AST.
 * Single expressions return unwrapped; multiple expressions wrap in Begin.
 * @param {Cons} bodyCons - The body as a Cons list.
 * @returns {Executable} Single expression or Begin node.
 */
function analyzeBody(bodyCons) {
  const exprs = mapCons(bodyCons, analyze);
  return exprs.length === 1 ? exprs[0] : new Begin(exprs);
}

// =============================================================================
// Special Form Handlers
// =============================================================================

function analyzeIf(exp) {
  // (if test consequent [alternative])
  const expArray = toArray(exp);
  if (expArray.length < 3 || expArray.length > 4) {
    throw new Error(`if: expected 2 or 3 arguments, got ${expArray.length - 1}`);
  }
  const test = analyze(cadr(exp));
  const consequent = analyze(caddr(exp));
  const alternative = cdddr(exp) !== null ? analyze(cadddr(exp)) : new Literal(null);
  return new If(test, consequent, alternative);
}

function analyzeLet(exp) {
  // (let ((var binding) ...) body...)
  const bindings = cadr(exp);
  const body = cddr(exp);

  if (body === null) {
    throw new Error('let: body cannot be empty');
  }

  const vars = [];
  const args = [];

  let curr = bindings;
  while (curr instanceof Cons) {
    const pair = curr.car; // (var expr)
    if (!(pair instanceof Cons)) {
      throw new Error('let: malformed binding - expected (var expr)');
    }
    const varObj = car(pair);
    let varName;
    try {
      varName = syntaxName(varObj);
    } catch (e) {
      throw new Error('let: binding name must be a symbol or syntax object');
    }

    vars.push(varName);
    args.push(analyze(cadr(pair)));
    curr = curr.cdr;
  }

  return new TailApp(
    new Lambda(vars, analyzeBody(body)),
    args
  );
}

function analyzeLetRec(exp) {
  // (letrec ((var val) ...) body...)
  const bindings = cadr(exp);
  if (!(bindings instanceof Cons)) {
    throw new Error('letrec: expected at least one binding');
  }
  const pair = car(bindings);
  if (!(pair instanceof Cons)) {
    throw new Error('letrec: malformed binding - expected (var expr)');
  }
  const varObj = car(pair);
  let varName;
  try {
    varName = syntaxName(varObj);
  } catch (e) {
    throw new Error('letrec: binding name must be a symbol');
  }
  const valExpr = analyze(cadr(pair));
  const body = cddr(exp);
  if (body === null) {
    throw new Error('letrec: body cannot be empty');
  }

  return new LetRec(varName, valExpr, analyzeBody(body));
}

function analyzeLambda(exp) {
  // (lambda (params...) body...) or (lambda (x y . rest) body...) or (lambda args body...)
  const paramsPart = cadr(exp);
  const body = cddr(exp);

  if (body === null) {
    throw new Error('lambda: body cannot be empty');
  }

  const params = [];
  let restParam = null;
  let curr = paramsPart;

  // Handle: (lambda single-symbol body...) - all args go to single symbol
  if (curr instanceof Symbol || isSyntaxObject(curr)) {
    restParam = syntaxName(curr);
    return new Lambda(params, analyzeBody(body), restParam);
  }

  // Handle proper list of params and improper list (with rest param)
  while (curr instanceof Cons) {
    if (!(curr.car instanceof Symbol) && !isSyntaxObject(curr.car)) {
      throw new Error('lambda: parameter must be a symbol');
    }
    params.push(syntaxName(curr.car));
    curr = curr.cdr;
  }

  // Check for rest parameter (improper list tail is a Symbol)
  if (curr !== null) {
    if (curr instanceof Symbol || isSyntaxObject(curr)) {
      restParam = syntaxName(curr);
    } else {
      throw new Error('lambda: malformed parameter list');
    }
  }

  return new Lambda(params, analyzeBody(body), restParam);
}

function analyzeSet(exp) {
  // (set! var val)
  const expArray = toArray(exp);
  if (expArray.length !== 3) {
    throw new Error(`set!: expected 2 arguments, got ${expArray.length - 1}`);
  }
  const varSym = cadr(exp);
  let varName;
  try {
    varName = syntaxName(varSym);
  } catch (e) {
    throw new Error('set!: first argument must be a symbol');
  }
  return new Set(varName, analyze(caddr(exp)));
}

function analyzeDefine(exp) {
  // (define var val) or (define (f args...) body...) or (define (f . args) body...)
  const head = cadr(exp);

  if (head instanceof Symbol || isSyntaxObject(head)) {
    // Simple variable definition: (define var val)
    const name = syntaxName(head);
    const val = caddr(exp);
    if (val === undefined) {
      throw new Error('define: missing value expression');
    }
    registerBindingWithCurrentScopes(name);
    return new Define(name, analyze(val));
  } else if (head instanceof Cons) {
    // Function definition shorthand: (define (f args...) body...)
    const funcNameSym = car(head);
    if (!(funcNameSym instanceof Symbol) && !isSyntaxObject(funcNameSym)) {
      throw new Error('define: function name must be a symbol');
    }
    const funcName = syntaxName(funcNameSym);
    registerBindingWithCurrentScopes(funcName);
    const argsList = cdr(head);
    const body = cddr(exp);

    if (body === null) {
      throw new Error('define: function body cannot be empty');
    }

    const params = [];
    let restParam = null;
    let curr = argsList;

    while (curr instanceof Cons) {
      if (!(curr.car instanceof Symbol) && !isSyntaxObject(curr.car)) {
        throw new Error('define: parameter must be a symbol');
      }
      params.push(syntaxName(curr.car));
      curr = curr.cdr;
    }

    // Check for rest parameter (improper list tail is a Symbol)
    if (curr !== null) {
      if (curr instanceof Symbol || isSyntaxObject(curr)) {
        restParam = syntaxName(curr);
      } else {
        throw new Error('define: malformed parameter list');
      }
    }

    return new Define(funcName, new Lambda(params, analyzeBody(body), restParam));
  }
  throw new Error('define: expected symbol or (name args...) form');
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

    // Get the current defining scopes from the library loader
    // If we're in a library context, there will be at least one scope on the stack
    // Free variables in the template will be marked with these scopes
    const currentScopes = getCurrentDefiningScopes();
    const definingScope = currentScopes.length > 0 ? currentScopes[currentScopes.length - 1] : GLOBAL_SCOPE_ID;

    const transformer = compileSyntaxRules(literals, clauses, definingScope);
    globalMacroRegistry.define(name, transformer);

    return new Literal(null);
  }
  return new Literal(null);
}

function analyzeImport(exp) {
  // (import import-set ...)
  const sets = toArray(exp.cdr);
  const importSpecs = sets.map(s => parseImportSet(s));

  return new ImportNode(importSpecs, getLibraryExports, applyImports);
}

// =============================================================================
// Quasiquote Expansion
// =============================================================================

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
      // (unquote-splicing x) . rest -> (append x (expand rest))
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
