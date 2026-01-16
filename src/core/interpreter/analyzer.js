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
  DefineLibraryNode,
  ScopedVariable,
  DynamicWindInit,
  CallWithValuesNode,
  WithExceptionHandlerInit,
  RaiseNode
} from './ast.js';
import { getLibraryExports, applyImports, parseImportSet, loadLibrarySync, evaluateLibraryDefinitionSync, parseDefineLibrary } from './library_loader.js';
import { globalMacroRegistry, MacroRegistry } from './macro_registry.js';
import { Cons, cons, list, car, cdr, toArray, cadr, caddr, cadddr } from './cons.js';
import { Rational } from '../primitives/rational.js';
import { Complex } from '../primitives/complex.js';
import { Symbol, intern } from './symbol.js';
import { SyntaxObject, globalScopeRegistry, GLOBAL_SCOPE_ID, syntaxName, isSyntaxObject, identifierEquals, unwrapSyntax, syntaxScopes } from './syntax_object.js';
import { globalContext } from './context.js';
import {
  initHandlers,
  registerAllHandlers,
  getHandler,
  analyzeBody,
  analyzeScopedBody
} from './analyzers/index.js';

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

// Module-level fallback counter (for backwards compatibility)
let _uniqueIdCounter = 0;

/**
 * Generates a unique name for a variable binding.
 * Uses context if provided, otherwise falls back to module-level counter.
 * @param {string} baseName - Base name for the variable
 * @param {InterpreterContext} [context] - Optional context for isolation
 * @returns {string} Unique variable name
 */
function generateUniqueName(baseName, context = null) {
  if (context) {
    return `${baseName}_$${context.freshUniqueId()}`;
  }
  // Fallback to global counter for backwards compatibility
  _uniqueIdCounter++;
  return `${baseName}_$${_uniqueIdCounter}`;
}

/**
 * Resets the unique ID counter. Used for test isolation.
 * @deprecated Use context.resetUniqueIdCounter() instead
 */
export function resetUniqueIdCounter() {
  _uniqueIdCounter = 0;
}

// (No module-local uniqueIdCounter - now in InterpreterContext)

// Initialize modular handlers
initHandlers({
  analyze,
  generateUniqueName
});
registerAllHandlers();

export function analyze(exp, syntacticEnv = null, context = null) {
  // Use global context if none provided
  const ctx = context || globalContext;

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
    return analyzeVariable(exp, syntacticEnv, ctx);
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

      if (opNameForMacro && ctx.currentMacroRegistry.isMacro(opNameForMacro)) {
        const transformer = ctx.currentMacroRegistry.lookup(opNameForMacro);
        try {
          const expanded = transformer(exp, syntacticEnv);
          return analyze(expanded, syntacticEnv, ctx);
        } catch (e) {
          throw new Error(`Error expanding macro ${opNameForMacro}: ${e.message}`);
        }
      }
    }

    // Check if operator is a special form keyword
    const opName = (operator instanceof Symbol) ? operator.name :
      (isSyntaxObject(operator) ? syntaxName(operator) : null);

    if (opName) {
      const handler = getHandler(opName);
      if (handler) {
        return handler(exp, syntacticEnv, ctx);
      }
    }

    // Application
    return analyzeApplication(exp, syntacticEnv, ctx);
  }

  throw new Error(`Analyzer error: Unknown expression type: ${exp}`);
}


// =============================================================================
// Helper Functions
// =============================================================================

function analyzeVariable(exp, syntacticEnv, ctx) {
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

// analyzeApplication handles all non-special form list expressions (function calls)
function analyzeApplication(exp, syntacticEnv, ctx) {
  const fileArray = toArray(exp);
  const operator = fileArray[0];

  // Check for JS Method Call: (js-ref obj "method")(args...) -> (js-invoke obj "method" args...)
  if (operator instanceof Cons) {
    const opCar = car(operator);
    const opName = (opCar instanceof Symbol) ? opCar.name : (isSyntaxObject(opCar) ? syntaxName(opCar) : null);
    if (opName === 'js-ref') {
      const objExpr = analyze(cadr(operator), syntacticEnv, ctx);
      const methodName = caddr(operator);
      const argExprs = fileArray.slice(1).map(a => analyze(a, syntacticEnv, ctx));
      return new TailAppNode(new VariableNode('js-invoke'), [objExpr, new LiteralNode(methodName), ...argExprs]);
    }
  }

  const funcExpr = analyze(operator, syntacticEnv, ctx);
  const argExprs = fileArray.slice(1).map(a => analyze(a, syntacticEnv, ctx));
  return new TailAppNode(funcExpr, argExprs);
}



