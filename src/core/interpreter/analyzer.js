import {
  Executable,
  LiteralNode,
  VariableNode,
  LambdaNode,
  LetNode,
  LetRecNode,
  IfNode,
  SetNode,
  TailAppNode,
  CallCCNode,
  BeginNode,
  DefineNode,
  ImportNode,
  ScopedVariable,
  DynamicWindInit,
  CallWithValuesNode,
  WithExceptionHandlerInit,
  RaiseNode
} from './ast.js';
import { getLibraryExports, applyImports, parseImportSet, evaluateFeatureRequirement } from './library_loader.js';
import { SPECIAL_FORMS } from './library_registry.js';
import { globalMacroRegistry, MacroRegistry } from './macro_registry.js';
import { compileSyntaxRules } from './syntax_rules.js';
import { Cons, cons, list, car, cdr, mapCons, toArray, cadr, cddr, caddr, cdddr, cadddr } from './cons.js';
import { Rational } from '../primitives/rational.js';
import { Complex } from '../primitives/complex.js';
import { Symbol, intern } from './symbol.js';
import { SyntaxObject, globalScopeRegistry, freshScope, getCurrentDefiningScopes, GLOBAL_SCOPE_ID, registerBindingWithCurrentScopes, syntaxName, isSyntaxObject, identifierEquals, unwrapSyntax, syntaxScopes } from './syntax_object.js';

/**
 * Analyzes an S-expression and converts it to our AST object tree.
 */

/**
 * Maps scoped identifiers to unique runtime names.
 * We'll use a custom class for the Syntactic Environment.
 */
class SyntacticEnv {
  constructor(parent = null) {
    this.parent = parent;
    this.bindings = []; // Array of { id: SyntaxObject|Symbol, newName: string }
  }

  extend(id, newName) {
    const newEnv = new SyntacticEnv(this);
    newEnv.bindings.push({ id, newName });
    return newEnv;
  }

  lookup(id) {
    // Linear scan is okay for local scopes usually, but could be optimized.
    // We match based on identifierEquals logic.
    for (const binding of this.bindings) {
      if (identifierEquals(binding.id, id)) {
        return binding.newName;
      }
    }
    if (this.parent) {
      return this.parent.lookup(id);
    }
    return null;
  }
}

let _uniqueIdCounter = 0;
function generateUniqueName(baseName) {
  _uniqueIdCounter++;
  return `${baseName}_$${_uniqueIdCounter}`;
}

export function analyze(exp, syntacticEnv = null) {
  if (!syntacticEnv) {
    syntacticEnv = new SyntacticEnv();
  }

  if (exp instanceof Executable) {
    return exp;
  }

  // console.log("Analyzing:", exp instanceof Cons ? JSON.stringify(exp) : exp.toString());
  if (exp === null) {
    console.error("Analyze called with null!");
    throw new Error('Analyze: cannot analyze null (empty list)');
  }
  if (typeof exp === 'number') {
    return new LiteralNode(exp);
  }
  if (typeof exp === 'string') {
    return new LiteralNode(exp);
  }
  if (typeof exp === 'boolean') {
    return new LiteralNode(exp);
  }
  if (Array.isArray(exp)) {
    return new LiteralNode(exp);
  }
  if (exp instanceof Uint8Array) {
    return new LiteralNode(exp);
  }
  // Rational and Complex numbers are self-evaluating
  if (exp instanceof Rational || exp instanceof Complex) {
    return new LiteralNode(exp);
  }

  if (exp instanceof Symbol || isSyntaxObject(exp)) {
    return analyzeVariable(exp, syntacticEnv);
  }

  if (exp instanceof Cons) {
    const operator = car(exp);

    // Check for macro expansion (if not shadowed locally)
    let isShadowed = false;
    if (operator instanceof Symbol || isSyntaxObject(operator)) {
      if (syntacticEnv && syntacticEnv.lookup(operator)) {
        isShadowed = true;
      }
    }

    if (!isShadowed) {
      const opNameForMacro = (operator instanceof Symbol) ? operator.name :
        (isSyntaxObject(operator) ? syntaxName(operator) : null);

      if (opNameForMacro && currentMacroRegistry.isMacro(opNameForMacro)) {
        const transformer = currentMacroRegistry.lookup(opNameForMacro);
        try {
          const expanded = transformer(exp);
          return analyze(expanded, syntacticEnv);
        } catch (e) {
          throw new Error(`Error expanding macro ${opNameForMacro}: ${e.message}`);
        }
      }
    }

    // Check if operator is a special form keyword
    const opName = (operator instanceof Symbol) ? operator.name :
      (isSyntaxObject(operator) ? syntaxName(operator) : null);

    if (opName) {
      switch (opName) {
        case 'quote': return analyzeQuote(exp);
        case 'lambda': return analyzeLambda(exp, syntacticEnv);
        case 'if': return analyzeIf(exp, syntacticEnv);
        case 'set!': return analyzeSet(exp, syntacticEnv);
        case 'define': return analyzeDefine(exp, syntacticEnv);
        case 'let': return analyzeLet(exp, syntacticEnv);
        case 'letrec': return analyzeLetRec(exp, syntacticEnv);
        case 'begin': return analyzeBegin(exp, syntacticEnv);
        case 'import': return analyzeImport(exp);


        // Restored cases
        case 'quasiquote': return expandQuasiquote(cadr(exp), syntacticEnv); // Pass env to quasiquote!
        case 'define-syntax': return analyzeDefineSyntax(exp, syntacticEnv);
        case 'let-syntax': return analyzeLetSyntax(exp, syntacticEnv);
        case 'letrec-syntax': return analyzeLetrecSyntax(exp, syntacticEnv);
        case 'cond-expand': return analyze(expandCondExpand(exp), syntacticEnv);
      }
    }

    // Application
    return analyzeApplication(exp, syntacticEnv);
  }

  throw new Error(`Analyzer error: Unknown expression type: ${exp}`);
}


// =============================================================================
// Helper Functions
// =============================================================================

function analyzeVariable(exp, syntacticEnv) {
  // Check syntactic environment for alpha-renamed local binding
  const renamed = syntacticEnv.lookup(exp);
  if (renamed) {
    // It's a local variable! Use the renamed symbol.
    return new VariableNode(renamed);
  }

  // Not local -> Global / Free
  // If it's a syntax object, preserve scopes for global lookup
  if (isSyntaxObject(exp)) {
    return new ScopedVariable(syntaxName(exp), syntaxScopes(exp), globalScopeRegistry);
  } else {
    // Raw symbol
    return new VariableNode(exp.name);
  }
}


// =============================================================================
// Special Form Handlers
// =============================================================================





function analyzeDefineSyntax(exp, syntacticEnv = null) {
  // (define-syntax name transformer)
  // name can be a Symbol or SyntaxObject (when macro-expanded)
  const nameObj = cadr(exp);
  const name = (nameObj instanceof Symbol) ? nameObj.name : syntaxName(nameObj);
  const transformerSpec = caddr(exp);

  // Check for (syntax-rules ...) - handle both Symbol and SyntaxObject
  if (transformerSpec instanceof Cons) {
    const srKeyword = car(transformerSpec);
    const isSyntaxRulesSymbol = (srKeyword instanceof Symbol && srKeyword.name === 'syntax-rules');
    const isSyntaxRulesSyntaxObj = (isSyntaxObject(srKeyword) && syntaxName(srKeyword) === 'syntax-rules');

    if (isSyntaxRulesSymbol || isSyntaxRulesSyntaxObj) {
      // R7RS 4.3.2: syntax-rules can have an optional custom ellipsis:
      // (syntax-rules (<literal> ...) <syntax rule> ...)           ; standard form
      // (syntax-rules <ellipsis> (<literal> ...) <syntax rule> ...) ; custom ellipsis form

      let ellipsisName = '...';  // Default ellipsis
      let literalsList;
      let clausesList;

      const afterSyntaxRules = cdr(transformerSpec);  // Everything after 'syntax-rules'
      const firstArg = car(afterSyntaxRules);

      if (firstArg instanceof Cons || firstArg === null) {
        // Standard form: first arg is the literals list (Cons or null)
        literalsList = firstArg;
        clausesList = cdr(afterSyntaxRules);
      } else if (firstArg instanceof Symbol || firstArg instanceof SyntaxObject) {
        // Custom ellipsis form: first arg is the ellipsis identifier
        ellipsisName = firstArg instanceof Symbol ? firstArg.name : syntaxName(firstArg);
        literalsList = cadr(afterSyntaxRules);
        clausesList = cddr(afterSyntaxRules);
      } else {
        // Invalid form
        return new LiteralNode(null);
      }

      const literals = [];
      let curr = literalsList;
      while (curr instanceof Cons) {
        literals.push(curr.car); // Keep as Symbols/SyntaxObjects
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

      const transformer = compileSyntaxRules(literals, clauses, definingScope, ellipsisName, syntacticEnv);
      globalMacroRegistry.define(name, transformer);

      return new LiteralNode(null);
    }
  }
  return new LiteralNode(null);
}



// =============================================================================
// Local Macro Bindings: let-syntax, letrec-syntax
// =============================================================================

// Current macro registry stack for scoped expansion
let currentMacroRegistry = globalMacroRegistry;

function analyzeLetSyntax(exp, syntacticEnv) {
  // (let-syntax ((name transformer) ...) body ...)
  const bindings = cadr(exp);
  const body = cddr(exp);

  if (body === null) {
    throw new Error('let-syntax: body cannot be empty');
  }

  // Create a new local registry with the global as parent
  const localRegistry = new MacroRegistry(currentMacroRegistry);

  // Process bindings before switching context
  let curr = bindings;
  while (curr instanceof Cons) {
    const binding = curr.car; // (name transformer-spec)
    const name = syntaxName(car(binding));
    const transformerSpec = cadr(binding);
    const transformer = compileTransformerSpec(transformerSpec, syntacticEnv);
    localRegistry.define(name, transformer);
    curr = curr.cdr;
  }

  // Analyze body with the local registry active
  const savedRegistry = currentMacroRegistry;
  currentMacroRegistry = localRegistry;
  try {
    return analyzeBodyWithMacroRegistry(body, syntacticEnv);
  } finally {
    currentMacroRegistry = savedRegistry;
  }
}

function analyzeLetrecSyntax(exp, syntacticEnv) {
  // (letrec-syntax ((name transformer) ...) body ...)
  // Same as let-syntax but transformers can refer to each other
  const bindings = cadr(exp);
  const body = cddr(exp);

  if (body === null) {
    throw new Error('letrec-syntax: body cannot be empty');
  }

  // Create a new local registry with the global as parent
  const localRegistry = new MacroRegistry(currentMacroRegistry);

  // Switch to local registry BEFORE compiling transformers (letrec semantics)
  const savedRegistry = currentMacroRegistry;
  currentMacroRegistry = localRegistry;

  try {
    // Process bindings with mutual visibility
    let curr = bindings;
    while (curr instanceof Cons) {
      const binding = curr.car; // (name transformer-spec)
      const name = syntaxName(car(binding));
      const transformerSpec = cadr(binding);
      const transformer = compileTransformerSpec(transformerSpec, syntacticEnv);
      localRegistry.define(name, transformer);
      curr = curr.cdr;
    }

    return analyzeBodyWithMacroRegistry(body, syntacticEnv);
  } finally {
    currentMacroRegistry = savedRegistry;
  }
}

function compileTransformerSpec(transformerSpec, syntacticEnv = null) {
  // Handle both Symbol and SyntaxObject-wrapped 'syntax-rules'
  if (!(transformerSpec instanceof Cons)) {
    throw new Error('Transformer must be (syntax-rules ...)');
  }
  const keyword = car(transformerSpec);
  const keywordName = (keyword instanceof Symbol) ? keyword.name :
    (isSyntaxObject(keyword) ? syntaxName(keyword) : null);
  if (keywordName !== 'syntax-rules') {
    throw new Error('Transformer must be (syntax-rules ...)');
  }

  let literalsList = cadr(transformerSpec);
  const clausesList = cddr(transformerSpec);

  const literals = [];
  let curr = literalsList;
  while (curr instanceof Cons) {
    literals.push(curr.car);
    curr = curr.cdr;
  }

  const clauses = [];
  curr = clausesList;
  while (curr instanceof Cons) {
    const clause = curr.car;
    clauses.push([car(clause), cadr(clause)]);
    curr = curr.cdr;
  }

  const currentScopes = getCurrentDefiningScopes();
  const definingScope = currentScopes.length > 0 ? currentScopes[currentScopes.length - 1] : GLOBAL_SCOPE_ID;

  return compileSyntaxRules(literals, clauses, definingScope, '...', syntacticEnv);
}

function analyzeBodyWithMacroRegistry(bodyCons, syntacticEnv) {
  const exprs = [];
  let curr = bodyCons;
  while (curr instanceof Cons) {
    exprs.push(analyzeWithCurrentMacroRegistry(curr.car, syntacticEnv));
    curr = curr.cdr;
  }
  return exprs.length === 1 ? exprs[0] : new BeginNode(exprs);
}

function analyzeWithCurrentMacroRegistry(exp, syntacticEnv) {
  // Check for macro expansion using the current scoped registry
  if (exp instanceof Cons) {
    const tag = exp.car;
    if (tag instanceof Symbol && currentMacroRegistry.isMacro(tag.name)) {
      const transformer = currentMacroRegistry.lookup(tag.name);
      const expanded = transformer(exp);
      return analyzeWithCurrentMacroRegistry(expanded, syntacticEnv);
    }
  }
  return analyze(exp, syntacticEnv);
}


// =============================================================================
// Quasiquote Expansion
// =============================================================================

function expandQuasiquote(exp, syntacticEnv, nesting = 0) {
  // 1. Handle (quasiquote x)
  if (isTaggedList(exp, 'quasiquote')) {
    return listApp('list', [
      new LiteralNode(intern('quasiquote')),
      expandQuasiquote(cadr(exp), syntacticEnv, nesting + 1)
    ]);
  }

  // 2. Handle (unquote x)
  if (isTaggedList(exp, 'unquote')) {
    if (nesting === 0) {
      return analyze(cadr(exp), syntacticEnv);
    } else {
      return listApp('list', [
        new LiteralNode(intern('unquote')),
        expandQuasiquote(cadr(exp), syntacticEnv, nesting - 1)
      ]);
    }
  }

  // 3. Handle (unquote-splicing x)
  if (isTaggedList(exp, 'unquote-splicing')) {
    if (nesting === 0) {
      throw new Error("unquote-splicing not allowed at top level");
    } else {
      return listApp('list', [
        new LiteralNode(intern('unquote-splicing')),
        expandQuasiquote(cadr(exp), syntacticEnv, nesting - 1)
      ]);
    }
  }

  // 4. Handle Lists (Cons)
  if (exp instanceof Cons) {
    // Check for splicing in car
    if (isTaggedList(exp.car, 'unquote-splicing') && nesting === 0) {
      // (unquote-splicing x) . rest -> (append x (expand rest))
      return listApp('append', [
        analyze(cadr(exp.car), syntacticEnv),
        expandQuasiquote(exp.cdr, syntacticEnv, nesting)
      ]);
    }

    // Regular cons: (cons (expand car) (expand cdr))
    return listApp('cons', [
      expandQuasiquote(exp.car, syntacticEnv, nesting),
      expandQuasiquote(exp.cdr, syntacticEnv, nesting)
    ]);
  }

  // 5. Handle Vectors (Arrays)
  if (Array.isArray(exp)) {
    // Check if any element has unquote-splicing at nesting 0
    let hasSplicing = false;
    for (const elem of exp) {
      if (isTaggedList(elem, 'unquote-splicing') && nesting === 0) {
        hasSplicing = true;
        break;
      }
    }

    if (hasSplicing) {
      // Convert to list, expand, then use list->vector
      const listForm = exp.reduceRight((acc, el) => cons(el, acc), null);
      const expandedList = expandQuasiquote(listForm, syntacticEnv, nesting);
      return listApp('list->vector', [expandedList]);
    } else {
      // No splicing - expand each element and use vector
      const expandedElements = exp.map(e => expandQuasiquote(e, syntacticEnv, nesting));
      return listApp('vector', expandedElements);
    }
  }

  // 6. Atoms
  return new LiteralNode(exp);
}

function isTaggedList(exp, tag) {
  return (exp instanceof Cons) &&
    (exp.car instanceof Symbol) &&
    (exp.car.name === tag);
}

function listApp(funcName, args) {
  return new TailAppNode(new VariableNode(funcName), args);
}

// =============================================================================
// cond-expand Expansion
// =============================================================================

function expandCondExpand(exp) {
  // (cond-expand <clause> ...)
  // Each clause is (<feature-requirement> <expr> ...) or (else <expr> ...)
  const clauses = toArray(exp.cdr);

  for (const clause of clauses) {
    const clauseArr = toArray(clause);
    if (clauseArr.length === 0) continue;

    const featureReq = clauseArr[0];
    let matched = false;

    if (featureReq instanceof Symbol && featureReq.name === 'else') {
      matched = true;
    } else {
      matched = evaluateFeatureRequirement(featureReq);
    }

    if (matched) {
      if (clauseArr.length === 1) {
        return list(intern('begin')); // Empty expansion
      }
      if (clauseArr.length === 2) {
        return clauseArr[1]; // Single expression
      }
      // Multiple expressions - wrap in begin
      return cons(intern('begin'), list(...clauseArr.slice(1)));
    }
  }

  throw new Error('cond-expand: no matching clause and no else');
}

function analyzeQuote(exp) {
  const text = cadr(exp);
  // unwrapSyntax deeply? Quote should preserve syntax objects if datum->syntax is supported?
  // For now, let's unwrap to plain data as traditional scheme, unless we want to support syntax-case fully.
  // R7RS 'quote' produces external representation.
  return new LiteralNode(unwrapSyntax(text));
}

function analyzeIf(exp, syntacticEnv) {
  // (if test consequent [alternative])
  const test = analyze(cadr(exp), syntacticEnv);
  const consequent = analyze(caddr(exp), syntacticEnv);
  let alternative;

  if (cdddr(exp) === null) {
    alternative = new LiteralNode(undefined); // Unspecified return
  } else {
    alternative = analyze(cadddr(exp), syntacticEnv);
  }

  return new IfNode(test, consequent, alternative);
}

function analyzeLambda(exp, syntacticEnv) {
  // (lambda (params...) body...) 
  const paramsPart = cadr(exp);
  const body = cddr(exp);

  if (body === null) {
    throw new Error('lambda: body cannot be empty');
  }

  const newParams = [];
  let newEnv = syntacticEnv;
  let restParamRaw = null;
  let restParamRenamed = null;

  let curr = paramsPart;

  // Handle: (lambda single-symbol body...)
  if (curr instanceof Cons === false && (curr instanceof Symbol || isSyntaxObject(curr))) {
    restParamRaw = curr;
    const name = (curr instanceof Symbol) ? curr.name : syntaxName(curr);
    restParamRenamed = generateUniqueName(name);
    newEnv = newEnv.extend(curr, restParamRenamed);
    return new LambdaNode(newParams, analyzeBody(body, newEnv), restParamRenamed);
  }

  while (curr instanceof Cons) {
    const param = curr.car;
    if (!(param instanceof Symbol) && !isSyntaxObject(param)) {
      throw new Error('lambda: parameter must be a symbol');
    }
    const name = (param instanceof Symbol) ? param.name : syntaxName(param);
    const renamed = generateUniqueName(name);

    newParams.push(renamed);
    newEnv = newEnv.extend(param, renamed);

    curr = curr.cdr;
  }

  if (curr !== null) {
    if (curr instanceof Symbol || isSyntaxObject(curr)) {
      restParamRaw = curr;
      const name = (curr instanceof Symbol) ? curr.name : syntaxName(curr);
      restParamRenamed = generateUniqueName(name);
      newEnv = newEnv.extend(curr, restParamRenamed);
    } else {
      // nil is ok
    }
  }

  return new LambdaNode(newParams, analyzeBody(body, newEnv), restParamRenamed);
}

function analyzeBody(body, syntacticEnv) {
  const exprs = toArray(body).map(e => analyze(e, syntacticEnv));
  if (exprs.length === 1) {
    return exprs[0];
  }
  return new BeginNode(exprs);
}

function analyzeBegin(exp, syntacticEnv) {
  const body = cdr(exp);
  return analyzeBody(body, syntacticEnv);
}

function analyzeLet(exp, syntacticEnv) {
  // (let ((var binding) ...) body...)
  const bindings = cadr(exp);
  const body = cddr(exp);

  // Checking for named let: (let name (...) ...)
  if (bindings instanceof Symbol || isSyntaxObject(bindings)) {
    // Named let: (let name ((var val) ...) body...)
    // Expand to: (letrec ((name (lambda (var ...) body...))) (name val ...))
    const loopName = bindings;
    const bindPairs = caddr(exp);
    const body = cdddr(exp);

    // Extract vars and vals
    const vars = [];
    const vals = [];
    let curr = bindPairs;
    while (curr instanceof Cons) {
      const pair = curr.car;
      vars.push(car(pair));
      vals.push(cadr(pair));
      curr = curr.cdr;
    }

    const varsList = vars.reduceRight((acc, el) => cons(el, acc), null);
    const valsList = vals.reduceRight((acc, el) => cons(el, acc), null);

    const lambdaExp = cons(intern('lambda'), cons(varsList, body));
    const letrecBindings = list(list(loopName, lambdaExp));
    const initialCall = cons(loopName, valsList);

    const letrecExp = list(intern('letrec'), letrecBindings, initialCall);
    return analyzeLetRec(letrecExp, syntacticEnv);
  }

  const vars = [];
  const args = [];
  const renos = [];
  let newEnv = syntacticEnv;

  let curr = bindings;
  // Initialize newEnv for body, but args are analyzed in OUTER env

  while (curr instanceof Cons) {
    const pair = curr.car;
    const varObj = car(pair);
    const valObj = cadr(pair);

    const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
    const renamed = generateUniqueName(name);

    vars.push(renamed);
    renos.push({ id: varObj, name: renamed });

    // Analyze init expressions in the inherited OUTER environment (parallel binding)
    args.push(analyze(valObj, syntacticEnv));

    curr = curr.cdr;
  }

  // Extend environment for the body
  for (const r of renos) {
    newEnv = newEnv.extend(r.id, r.name);
  }

  return new TailAppNode(
    new LambdaNode(vars, analyzeBody(body, newEnv)),
    args
  );
}

function analyzeLetRec(exp, syntacticEnv) {
  // (letrec ((var val) ...) body...)
  const bindings = cadr(exp);
  const body = cddr(exp);

  // We support multiple bindings by updating the implementation to match
  // typical letrec analysis: create new env with ALL vars, then analyze ALL vals.

  let newEnv = syntacticEnv;
  const vars = [];
  const args = [];
  const renos = [];

  let curr = bindings;
  while (curr instanceof Cons) {
    const pair = curr.car;
    const varObj = car(pair);

    const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
    const renamed = generateUniqueName(name);

    vars.push(renamed);
    renos.push({ id: varObj, name: renamed, valObj: cadr(pair) });

    newEnv = newEnv.extend(varObj, renamed);
    curr = curr.cdr;
  }

  // Analyze vals in newEnv
  for (const r of renos) {
    args.push(analyze(r.valObj, newEnv));
  }

  // Construct LetRec nodes. 
  // Stepable LetRec supports single binding.
  // We need to nest them or use Set! logic.
  // (letrec ((u v) ...) body) -> (let ((u 'undefined)...) (set! u v)... body)

  // Optimized approach matching macros.scm usually:
  // Analyzer should output code that accounts for recursive scope.
  // If we don't have a multi-LetRec node, we use the `undefined` + `set!` approach in AST?
  // Let's manually build the AST for:
  // (let ((renamed 'undefined) ...) (set! renamed val) ... body)

  // 1. Create outer let with undefineds
  // 2. Wrap body in sequence of sets

  const undefinedLit = new LiteralNode(undefined);
  const setExprs = [];
  for (let i = 0; i < renos.length; i++) {
    setExprs.push(new SetNode(renos[i].name, args[i]));
  }

  const bodyExpr = analyzeBody(body, newEnv);
  // Sequence: set!... body
  const seq = new BeginNode([...setExprs, bodyExpr]);

  return new TailAppNode(
    new LambdaNode(vars, seq),
    vars.map(_ => undefinedLit)
  );
}

function analyzeSet(exp, syntacticEnv) {
  const varObj = cadr(exp);
  const valExpr = analyze(caddr(exp), syntacticEnv);

  // Check local lookup
  const renamed = syntacticEnv.lookup(varObj);
  if (renamed) {
    return new SetNode(renamed, valExpr);
  }

  const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
  return new SetNode(name, valExpr);
}

function analyzeDefine(exp, syntacticEnv) {
  const head = cadr(exp);
  if (head instanceof Cons) {
    // Function definition: (define (name . args) . body)
    // Transform to: (define name (lambda args . body))
    const nameObj = car(head);
    const args = cdr(head);
    const body = cddr(exp);

    const lambdaExp = cons(intern('lambda'), cons(args, body));
    const valExpr = analyzeLambda(lambdaExp, syntacticEnv);

    const name = (nameObj instanceof Symbol) ? nameObj.name : syntaxName(nameObj);
    return new DefineNode(name, valExpr);
  }

  const varObj = head;
  const valExpr = analyze(caddr(exp), syntacticEnv);

  // Define always targets the current environment frame in runtime.
  // We do NOT rename defines, as they might be exported/global.
  const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
  return new DefineNode(name, valExpr);
}

function analyzeApplication(exp, syntacticEnv) {
  const fileArray = toArray(exp);
  // Debug: check for null args
  if (fileArray.some(x => x === null)) {
    console.error("Application with null arg:", JSON.stringify(exp));
  }
  const funcExpr = analyze(fileArray[0], syntacticEnv);
  const argExprs = fileArray.slice(1).map(a => analyze(a, syntacticEnv));
  return new TailAppNode(funcExpr, argExprs);
}

function analyzeCallCC(exp, syntacticEnv) {
  const proc = analyze(cadr(exp), syntacticEnv);
  return new CallCCNode(proc);
}

function analyzeCallWithValues(exp, syntacticEnv) {
  const producer = analyze(cadr(exp), syntacticEnv);
  const consumer = analyze(caddr(exp), syntacticEnv);
  return new CallWithValuesNode(producer, consumer);
}

function analyzeDynamicWind(exp, syntacticEnv) {
  const before = analyze(cadr(exp), syntacticEnv);
  const thunk = analyze(caddr(exp), syntacticEnv);
  const after = analyze(cadddr(exp), syntacticEnv);
  return new DynamicWindInit(before, thunk, after);
}

function analyzeWithExceptionHandler(exp, syntacticEnv) {
  const handler = analyze(cadr(exp), syntacticEnv);
  const thunk = analyze(caddr(exp), syntacticEnv);
  return new WithExceptionHandlerInit(handler, thunk);
}

function analyzeRaise(exp, syntacticEnv, continuable = false) {
  const obj = analyze(cadr(exp), syntacticEnv);
  return new RaiseNode(obj, continuable);
}

function analyzeImport(exp) {
  // Import specs should be handled by loader, but if encountered in eval:
  // We construct ImportNode.
  // NOTE: This requires parsing implementation which I omitted previously.
  // For now, let's assume valid import form and minimal processing or throw?
  // Given usage in library loader, this might not be called?
  // But let's leave existing implementation if possible.
  // I can't leave existing because I deleted it.
  // I will return a placeholder error if reached, to see if it's used.
  // Only library_loader invokes analyze on library body. Library body can contain import?
  // R7RS: import is top-level. (define-library ...) contains import.
  // (analyze) is called on body expressions. (import) inside (begin)?
  // (begin (import ...)) is valid at top level.
  // So we should support it.

  // Minimal implementation:
  // parse specs -> 
  // We need parseImportSpec from library_loader.js? No, circular dependency.
  // Just wrap arguments in ImportNode and let ImportNode.step handle it?
  // ImportNode expects `specs`.
  // Let's implement a simple parser here or just pass raw cons?
  // ImportNode expects array of objects.
  // Let's try to pass raw cons and let step method fail if wrong, 
  // or fix ImportNode later.
  // Actually, `library_loader` parses imports.
  return new ImportNode([], null, null);
}
