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
  LibraryVariableNode,
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
import { Char } from '../primitives/char_class.js';
import { Symbol, intern } from './symbol.js';
import { SyntaxObject, globalScopeRegistry, GLOBAL_SCOPE_ID, syntaxName, isSyntaxObject, identifierEquals, unwrapSyntax, syntaxScopes, libraryScopeOf, libraryBindingEnv, operatorKeyword } from './syntax_object.js';
import { globalContext } from './context.js';
import { SchemeSyntaxError } from './errors.js';
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
export class SyntacticEnv {
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

  // Helper to attach source from the original expression to the AST node
  const withSourceFrom = (node, sourceExp) => {
    if (sourceExp && sourceExp.source) {
      node.source = sourceExp.source;
    }
    return node;
  };

  // console.log("Analyzing:", exp instanceof Cons ? JSON.stringify(exp) : exp.toString());
  if (exp === null) {
    console.error("Analyze called with null!");
    throw new SchemeSyntaxError('cannot analyze null (empty list)', null, 'analyze');
  }
  if (typeof exp === 'number') {
    return new LiteralNode(exp);
  }
  if (typeof exp === 'bigint') {
    return new LiteralNode(exp);
  }
  if (typeof exp === 'string') {
    return new LiteralNode(exp);
  }
  if (typeof exp === 'boolean') {
    return new LiteralNode(exp);
  }
  if (Array.isArray(exp)) {
    // A vector a macro's template built holds identifiers, not symbols.
    return new LiteralNode(holdsSyntax(exp) ? unwrapSyntax(exp) : exp);
  }
  if (exp instanceof Uint8Array) {
    return new LiteralNode(exp);
  }
  // Rational, Complex and Char are self-evaluating
  if (exp instanceof Rational || exp instanceof Complex || exp instanceof Char) {
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

    // A macro use or special form, unless a local variable shadows the
    // keyword (R7RS: "local variable bindings may shadow keyword bindings")
    if (!isShadowed && (operator instanceof Symbol || isSyntaxObject(operator))) {
      const { keyword, transformer } = operatorKeyword(operator, ctx);

      if (transformer) {
        try {
          const expanded = transformer(exp, syntacticEnv);
          return analyze(expanded, syntacticEnv, ctx);
        } catch (e) {
          throw new SchemeSyntaxError(`Error expanding macro: ${e.message}`, exp, keyword);
        }
      }

      const handler = getHandler(keyword);
      if (handler) {
        // Call handler and attach source from the original Cons
        const node = handler(exp, syntacticEnv, ctx);
        return withSourceFrom(node, exp);
      }
    }

    // Application
    const node = analyzeApplication(exp, syntacticEnv, ctx);
    return withSourceFrom(node, exp);
  }

  throw new SchemeSyntaxError(`Unknown expression type`, exp, 'analyze');
}


// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Whether a datum holds an identifier anywhere in it, as one a macro's
 * template produced does.
 * @param {*} datum - A datum.
 * @returns {boolean}
 */
function holdsSyntax(datum, seen = new Set()) {
  if (isSyntaxObject(datum)) return true;
  if (!(datum instanceof Cons) && !Array.isArray(datum)) return false;
  // A literal may be circular (R7RS 2.4).
  if (seen.has(datum)) return false;
  seen.add(datum);
  if (datum instanceof Cons) return holdsSyntax(datum.car, seen) || holdsSyntax(datum.cdr, seen);
  return datum.some((element) => holdsSyntax(element, seen));
}

function analyzeVariable(exp, syntacticEnv, ctx) {
  // Check syntactic environment for alpha-renamed local binding
  const renamed = syntacticEnv.lookup(exp);
  if (renamed) {
    // It's a local variable! Use the renamed symbol.
    return new VariableNode(renamed);
  }

  // Not local -> Global / Free.
  if (isSyntaxObject(exp)) {
    // Introduced by a library's macro: the library's binding of the name,
    // which `libraryBindingEnv` says how to reach.
    const libraryScope = libraryScopeOf(exp);
    if (libraryScope !== null) {
      const libraryEnv = libraryBindingEnv(exp, libraryScope);
      return libraryEnv === null
        ? new VariableNode(syntaxName(exp))
        : new LibraryVariableNode(syntaxName(exp), libraryEnv);
    }
    // A macro-introduced identifier still carrying its expansion scopes.
    // Resolving it is the *expander's* job, not the evaluator's: in every
    // system this implementation draws on -- Kohlbecker et al., Clinger and
    // Rees, Dybvig et al., Flatt's sets of scopes -- binding resolution
    // completes during expansion and no hygiene information reaches run time.
    // `docs/hygiene.md` describes resolution as step 3 of expansion; only the
    // code disagreed, deferring it to `ScopedVariable.step` and repeating it on
    // every single evaluation.
    //
    // Resolving here has a second effect that matters as much: the compiler
    // tier cannot lower a `ScopedVariable`, so every procedure containing one
    // was left interpreted. That is why a named `let`, a `do` loop, a `letrec`
    // and a `case` could not be compiled at all -- their macro templates mention
    // `car`, `cdr`, `list` and `memv`, which arrive here carrying scopes.
    const resolved = globalScopeRegistry.resolve({
      name: syntaxName(exp), scopes: syntaxScopes(exp)
    });
    if (resolved === null) {
      // No scoped binding matches, so this is a free reference to a global.
      // That is sound by construction rather than by luck: locals are
      // alpha-renamed, and an identifier a library's macro introduced was
      // resolved above, so a plain name here can only denote a global of the
      // program, which is exactly what the runtime fallback did.
      return new VariableNode(syntaxName(exp));
    }
    // A scoped binding does match. Left to resolve at run time as before,
    // deliberately: this path has never been observed to fire, so there is no
    // test to tell us that moving it is safe, and an unobserved path is not one
    // to change on the strength of an argument.
    return new ScopedVariable(syntaxName(exp), syntaxScopes(exp), globalScopeRegistry);
  }
  // Raw symbol
  return new VariableNode(exp.name);
}


// =============================================================================
// Special Form Handlers
// =============================================================================

// analyzeApplication handles all non-special form list expressions (function calls)
function analyzeApplication(exp, syntacticEnv, ctx) {
  const fileArray = toArray(exp);
  const operator = fileArray[0];

  // Check for JS Method Call: (js-ref obj "method")(args...) -> (js-invoke obj "method" args...)
  // Special case: (js-ref super "method")(args...) -> (class-super-call this 'method args...)
  if (operator instanceof Cons) {
    const opCar = car(operator);
    const opName = (opCar instanceof Symbol) ? opCar.name : (isSyntaxObject(opCar) ? syntaxName(opCar) : null);
    if (opName === 'js-ref') {
      const objExpr = cadr(operator);
      const methodName = caddr(operator);

      // Check if object is 'super' - transform to class-super-call
      const objName = (objExpr instanceof Symbol) ? objExpr.name :
        (isSyntaxObject(objExpr) ? syntaxName(objExpr) : null);
      if (objName === 'super') {
        // Transform (super.methodName args...) to (class-super-call this 'methodName args...)
        const argExprs = fileArray.slice(1).map(a => analyze(a, syntacticEnv, ctx));
        return new TailAppNode(
          new VariableNode('class-super-call'),
          [new VariableNode('this'), new LiteralNode(intern(methodName)), ...argExprs]
        );
      }

      // Normal js-ref optimization
      const analyzedObj = analyze(objExpr, syntacticEnv, ctx);
      const argExprs = fileArray.slice(1).map(a => analyze(a, syntacticEnv, ctx));
      return new TailAppNode(new VariableNode('js-invoke'), [analyzedObj, new LiteralNode(methodName), ...argExprs]);
    }
  }

  const funcExpr = analyze(operator, syntacticEnv, ctx);
  const argExprs = fileArray.slice(1).map(a => analyze(a, syntacticEnv, ctx));
  return new TailAppNode(funcExpr, argExprs);
}



