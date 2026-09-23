/**
 * Core Form Handlers
 * 
 * Handlers for fundamental Scheme special forms:
 * quote, if, lambda, let, letrec, set!, define, begin
 */

import {
    LiteralNode,
    VariableNode,
    LambdaNode,
    IfNode,
    SetNode,
    TailAppNode,
    BeginNode,
    DefineNode,
    LetRecNode
} from '../ast.js';
import { Cons, cons, list, car, cdr, toArray, cadr, cddr, caddr, cdddr } from '../cons.js';
import { Symbol, intern } from '../symbol.js';
import { isSyntaxObject, syntaxName, unwrapSyntax, GLOBAL_SCOPE_ID } from '../syntax_object.js';
import { globalContext } from '../context.js';
import { MacroRegistry } from '../macro_registry.js';
import { registerHandler } from './registry.js';
import { compileSyntaxRules } from '../syntax_rules.js';
import { SchemeSyntaxError } from '../errors.js';
import { Interpreter } from '../interpreter.js';
import { createGlobalEnvironment } from '../../primitives/index.js';

// These will be set by the analyzer when it initializes
// to avoid circular dependencies between analyzer.js and core_forms.js.
let analyze;
let generateUniqueName;

/**
 * Initializes the core forms with dependencies from the main analyzer.
 * This breaks the circular dependency between core_forms and analyzer.
 * @param {Object} deps - Dependencies containing analyze and generateUniqueName functions.
 */
export function initCoreForms(deps) {
    analyze = deps.analyze;
    generateUniqueName = deps.generateUniqueName;
}

/**
 * Helper to analyze an expression while checking for macro expansion first.
 * If the head of the list is a macro, it expands it recursively before analysis.
 * 
 * @param {*} exp - The Scheme expression to analyze.
 * @param {SyntacticEnv} syntacticEnv - The current syntactic environment.
 * @param {InterpreterContext} ctx - The interpreter context.
 * @returns {ASTNode} The resulting AST node.
 */
function analyzeWithCurrentMacroRegistry(exp, syntacticEnv, ctx) {
    if (exp instanceof Cons) {
        const tag = exp.car;
        // Resolve the name of the tag (handling both Symbols and SyntaxObjects)
        const tagName = (tag instanceof Symbol) ? tag.name : (isSyntaxObject(tag) ? syntaxName(tag) : null);

        // If it's a macro, expand and re-analyze
        if (tagName && ctx.currentMacroRegistry.isMacro(tagName)) {
            const transformer = ctx.currentMacroRegistry.lookup(tagName);
            const expanded = transformer(exp, syntacticEnv);
            return analyzeWithCurrentMacroRegistry(expanded, syntacticEnv, ctx);
        }
    }
    // Fall back to standard analysis
    return analyze(exp, syntacticEnv, ctx);
}


// =============================================================================
// Handler Functions
// =============================================================================

/**
 * Analyzes (quote <datum>).
 * @param {Cons} exp - The quote expression.
 * @returns {LiteralNode}
 */
function analyzeQuote(exp, syntacticEnv, ctx) {
    const text = cadr(exp);
    // Quote strips syntax object wrappers recursively
    return new LiteralNode(unwrapSyntax(text));
}

/**
 * Analyzes (if <test> <consequent> [<alternative>]).
 * @param {Cons} exp - The if expression.
 * @returns {IfNode}
 */
function analyzeIf(exp, syntacticEnv, ctx) {
    const test = analyze(cadr(exp), syntacticEnv, ctx);
    const consequent = analyze(caddr(exp), syntacticEnv, ctx);
    let alternative;

    if (cdddr(exp) === null) {
        // R7RS: if alternative is missing, it's undefined
        alternative = new LiteralNode(undefined);
    } else {
        alternative = analyze(exp.cdr.cdr.cdr.car, syntacticEnv, ctx);
    }

    return new IfNode(test, consequent, alternative);
}

/**
 * Analyzes (lambda <formals> <body>).
 * Handles fixed, variadic, and dotted-tail parameter lists.
 * @param {Cons} exp - The lambda expression.
 * @returns {LambdaNode}
 */
function analyzeLambda(exp, syntacticEnv, ctx) {
    const paramsPart = cadr(exp);
    const body = cddr(exp);

    if (body === null) {
        throw new SchemeSyntaxError('body cannot be empty', exp, 'lambda');
    }

    const newParams = [];
    let newEnv = syntacticEnv;
    let restParamRenamed = null;

    const originalParams = [];
    let originalRestParam = null;

    let curr = paramsPart;

    // Handle variadic syntax: (lambda args body...)
    if (!(curr instanceof Cons) && (curr instanceof Symbol || isSyntaxObject(curr))) {
        const name = (curr instanceof Symbol) ? curr.name : syntaxName(curr);
        originalRestParam = name;
        restParamRenamed = generateUniqueName(name, ctx);
        newEnv = newEnv.extend(curr, restParamRenamed);
        return new LambdaNode(newParams, analyzeBody(body, newEnv, ctx), restParamRenamed, 'anonymous', [], originalRestParam);
    }

    // Handle fixed or dotted parameter lists: (lambda (x y . z) body...)
    while (curr instanceof Cons) {
        const param = curr.car;
        if (!(param instanceof Symbol) && !isSyntaxObject(param)) {
            throw new SchemeSyntaxError('parameter must be a symbol', param, 'lambda');
        }
        const name = (param instanceof Symbol) ? param.name : syntaxName(param);
        originalParams.push(name);
        const renamed = generateUniqueName(name, ctx);

        newParams.push(renamed);
        // Alpha-rename parameter and extend environment
        newEnv = newEnv.extend(param, renamed);

        curr = curr.cdr;
    }

    // Handle the tail symbol in dotted lists: (a b . c)
    if (curr !== null) {
        if (curr instanceof Symbol || isSyntaxObject(curr)) {
            const name = (curr instanceof Symbol) ? curr.name : syntaxName(curr);
            originalRestParam = name;
            restParamRenamed = generateUniqueName(name, ctx);
            newEnv = newEnv.extend(curr, restParamRenamed);
        }
    }

    // Body is analyzed in a new macro scope (for internal define-syntax)
    return new LambdaNode(newParams, analyzeScopedBody(body, newEnv, ctx), restParamRenamed, 'anonymous', originalParams, originalRestParam);
}


/**
 * Analyzes a body in a temporary macro registry scope.
 * Used for lambda and let bodies to support internal define-syntax.
 * 
 * @param {Cons} body - The body expressions.
 * @param {SyntacticEnv} syntacticEnv - The current syntactic environment.
 * @param {InterpreterContext} ctx - The interpreter context.
 * @returns {ASTNode}
 */
function analyzeScopedBody(body, syntacticEnv, ctx) {
    const localRegistry = new MacroRegistry(ctx.currentMacroRegistry);
    const savedRegistry = ctx.currentMacroRegistry;
    ctx.currentMacroRegistry = localRegistry;
    try {
        return analyzeBody(body, syntacticEnv, ctx);
    } finally {
        ctx.currentMacroRegistry = savedRegistry;
    }
}

/**
 * Analyzes a Scheme body (list of expressions).
 * Implements R7RS internal definition hoisting.
 * 
 * @param {Cons} body - The body expressions.
 * @param {SyntacticEnv} syntacticEnv - The current syntactic environment.
 * @param {InterpreterContext} ctx - The interpreter context.
 * @returns {ASTNode} A BeginNode or a single ASTNode.
 */
function analyzeBody(body, syntacticEnv, ctx) {
    const bodyArray = toArray(body);

    // Phase 1: Scan for internal definitions and extend the syntactic environment.
    // This allows nested functions to be mutually recursive.
    let extendedEnv = syntacticEnv;
    for (const exp of bodyArray) {
        if (exp instanceof Cons) {
            const head = exp.car;
            const headName = (head instanceof Symbol) ? head.name :
                (isSyntaxObject(head)) ? syntaxName(head) : null;

            if (headName === 'define') {
                const defHead = cadr(exp);
                let definedName;
                let bindKey = defHead;

                if (defHead instanceof Cons) {
                    // (define (name args) ...)
                    const nameObj = car(defHead);
                    definedName = (nameObj instanceof Symbol) ? nameObj.name : syntaxName(nameObj);
                    bindKey = nameObj;
                } else if (defHead instanceof Symbol || isSyntaxObject(defHead)) {
                    // (define name val)
                    definedName = (defHead instanceof Symbol) ? defHead.name : syntaxName(defHead);
                }

                if (definedName) {
                    extendedEnv = extendedEnv.extend(bindKey, definedName);
                }
            }
        }
    }

    // Phase 2: Analyze all expressions in the hoisted environment.
    const exprs = bodyArray.map(e => analyze(e, extendedEnv, ctx));
    if (exprs.length === 1) {
        return exprs[0];
    }
    return new BeginNode(exprs);
}

/**
 * Analyzes (begin <expr>...).
 * @param {Cons} exp - The begin expression.
 * @returns {ASTNode}
 */
function analyzeBegin(exp, syntacticEnv, ctx) {
    const body = cdr(exp);
    return analyzeBody(body, syntacticEnv, ctx);
}

/**
 * Analyzes (let <bindings> <body>) or (let <name> <bindings> <body>).
 * @param {Cons} exp - The let expression.
 * @returns {TailAppNode}
 */
function analyzeLet(exp, syntacticEnv, ctx) {
    const bindings = cadr(exp);
    const body = cddr(exp);

    // Handle Named Let: (let loop ((x 1)) (loop x))
    if (bindings instanceof Symbol || isSyntaxObject(bindings)) {
        const loopName = bindings;
        const bindPairs = caddr(exp);
        const namedBody = cdddr(exp);

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

        // R7RS 4.2.4 desugars a named let to
        //
        //   ((letrec ((tag (lambda (name ...) body ...))) tag) val ...)
        //
        // The initializers sit *outside* the letrec, and that placement is
        // load-bearing rather than stylistic: they must be evaluated in the
        // enclosing scope, where `tag` is not yet bound. Putting them inside --
        // as `(letrec ((tag ...)) (tag val ...))` -- lets the loop name capture
        // its own initializers, so `(let - ((n (- 1))) n)` calls the loop
        // instead of negating. That is Al Petrofsky's pitfall 8.1, and it is in
        // `tests/core/scheme/r7rs-pitfalls.scm`.
        const lambdaExp = cons(intern('lambda'), cons(varsList, namedBody));
        const letrecBindings = list(list(loopName, lambdaExp));

        const letrecExp = list(intern('letrec'), letrecBindings, loopName);
        const loopProc = analyzeLetRec(letrecExp, syntacticEnv, ctx);
        const argNodes = vals.map((v) => analyze(v, syntacticEnv, ctx));
        return new TailAppNode(loopProc, argNodes);
    }

    // Standard Let: desugar to immediate lambda application
    const vars = [];
    const args = [];
    const renos = [];
    const originalParams = [];
    let newEnv = syntacticEnv;

    let curr = bindings;
    while (curr instanceof Cons) {
        const pair = curr.car;
        const varObj = car(pair);
        const valObj = cadr(pair);

        const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
        originalParams.push(name);
        const renamed = generateUniqueName(name, ctx);

        vars.push(renamed);
        renos.push({ id: varObj, name: renamed });
        // Arguments are analyzed in the EXTERNAL environment
        args.push(analyze(valObj, syntacticEnv, ctx));

        curr = curr.cdr;
    }

    for (const r of renos) {
        newEnv = newEnv.extend(r.id, r.name, r.id.name || r.id);
    }

    return new TailAppNode(
        new LambdaNode(vars, analyzeScopedBody(body, newEnv, ctx), null, 'let', originalParams),
        args
    );
}

/**
 * Analyzes (letrec <bindings> <body>).
 * @param {Cons} exp - The letrec expression.
 * @returns {TailAppNode}
 */
function analyzeLetRec(exp, syntacticEnv, ctx) {
    const bindings = cadr(exp);
    // `(letrec <bindings> <body> ...)` -- the body starts at the third element.
    // This read `cdddr` and so dropped the first body expression, which went
    // unnoticed for as long as the `letrec` macro in `macros.scm` shadowed this
    // handler and nothing ever reached it.
    const body = cddr(exp);

    let newEnv = syntacticEnv;
    const vars = [];
    const args = [];
    const renos = [];
    const originalParams = [];

    // Phase 1: Alpha-rename all variables and extend the environment.
    let curr = bindings;
    while (curr instanceof Cons) {
        const pair = curr.car;
        const varObj = car(pair);

        const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
        originalParams.push(name);
        const renamed = generateUniqueName(name, ctx);

        vars.push(renamed);
        renos.push({ id: varObj, name: renamed, valObj: cadr(pair) });

        newEnv = newEnv.extend(varObj, renamed, name);
        curr = curr.cdr;
    }

    // Phase 2: Analyze all init expressions in the EXTENDED environment.
    for (const r of renos) {
        args.push(analyze(r.valObj, newEnv, ctx));
    }

    const bodyExpr = analyzeScopedBody(body, newEnv, ctx);

    // The common shape -- every initializer a lambda -- gets a node of its own.
    // A named `let`, a group of internal definitions and a set of mutually
    // recursive procedures all land here. Because evaluating a lambda has no
    // side effects and cannot observe another binding, R7RS's requirement that
    // all initializers run before any assignment is met trivially, so `letrec`
    // and `letrec*` agree and no assignment is needed at all.
    //
    // This is the `fix` treatment from Waddell, Sarkar and Dybvig, "Fixing
    // Letrec" (2005) -- unassigned lambda bindings are exactly the ones a
    // compiler can call directly, and Guile's `<fix>` node is the same idea.
    // Keeping the binding explicit rather than desugaring it is what lets both
    // tiers see that a loop variable holds the lambda beside it.
    if (args.every((a) => a instanceof LambdaNode)) {
        return new LetRecNode(vars, args, bodyExpr, originalParams);
    }

    // General case: an initializer is an arbitrary expression. R7RS requires
    // *all* of them to be evaluated before *any* variable is assigned, which is
    // the only observable difference from `letrec*`. Evaluating them in
    // argument position of an inner lambda gets that for free, and left to
    // right, without routing any value through a list.
    //
    //   ((lambda (v ...)
    //      ((lambda (t ...) (set! v t) ... body) init ...))
    //    <undefined> ...)
    const undefinedLit = new LiteralNode(undefined);
    const temps = originalParams.map((name) => generateUniqueName(`${name}-init`, ctx));
    const setExprs = vars.map((v, i) => new SetNode(v, new VariableNode(temps[i])));
    const assignAndRun = new LambdaNode(
        temps, new BeginNode([...setExprs, bodyExpr]), null, 'letrec-init');

    return new TailAppNode(
        new LambdaNode(
            vars, new TailAppNode(assignAndRun, args), null, 'letrec', originalParams),
        vars.map(() => undefinedLit)
    );
}

/**
 * Analyzes (set! <var> <expr>).
 * Also handles dot-notation desugaring for JS property assignment.
 * @param {Cons} exp - The set! expression.
 * @returns {ASTNode}
 */
function analyzeSet(exp, syntacticEnv, ctx) {
    const varObj = cadr(exp);
    const valExpr = analyze(caddr(exp), syntacticEnv, ctx);

    // Handle dot-notation desugaring: (set! obj.prop val) -> (js-set! obj "prop" val)
    if (varObj instanceof Cons) {
        const opName = (varObj.car instanceof Symbol) ? varObj.car.name :
            (isSyntaxObject(varObj.car) ? syntaxName(varObj.car) : null);

        if (opName === 'js-ref') {
            const objExpr = analyze(cadr(varObj), syntacticEnv, ctx);
            const propName = caddr(varObj);
            return new TailAppNode(
                new VariableNode('js-set!'),
                [objExpr, new LiteralNode(propName), valExpr]
            );
        }
    }

    // Standard lexical assignment
    const renamed = syntacticEnv.lookup(varObj);
    if (renamed) {
        return new SetNode(renamed, valExpr);
    }

    // Global assignment fallthrough
    const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
    return new SetNode(name, valExpr);
}

/**
 * Analyzes (define <var> <expr>) or (define (<var> <fomals>) <body>).
 * @param {Cons} exp - The define expression.
 * @returns {DefineNode}
 */
function analyzeDefine(exp, syntacticEnv, ctx) {
    const head = cadr(exp);

    // Function definition syntax: (define (f x) ...)
    if (head instanceof Cons) {
        const nameObj = car(head);
        const args = cdr(head);
        const body = cddr(exp);

        // Desugar to (define f (lambda (args) body...))
        const lambdaExp = cons(intern('lambda'), cons(args, body));
        const valExpr = analyzeLambda(lambdaExp, syntacticEnv, ctx);
        const name = (nameObj instanceof Symbol) ? nameObj.name : syntaxName(nameObj);

        // Propagate name to LambdaNode if it was just created
        if (valExpr instanceof LambdaNode) {
            valExpr.name = name;
            // The lambda above was built here rather than read, so it has no
            // source of its own. Its extent is the whole definition -- a
            // location anywhere in `(define (f x) ...)` is inside `f` -- and
            // without it every closure made this way reports no source, which
            // is most of them.
            if (exp.source && !valExpr.source) {
                valExpr.source = exp.source;
            }
        }

        return new DefineNode(name, valExpr);
    }

    // Simple variable definition: (define x 1)
    const varObj = head;
    const valExpr = analyze(caddr(exp), syntacticEnv, ctx);
    const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);

    if (valExpr instanceof LambdaNode) {
        valExpr.name = name;
    }

    return new DefineNode(name, valExpr);
}

// =============================================================================
// Registration
// =============================================================================

/**
 * Registers all core form handlers in the global analyzer handler registry.
 */
export function registerCoreForms() {
    registerHandler('quote', analyzeQuote);
    registerHandler('if', analyzeIf);
    registerHandler('lambda', analyzeLambda);
    registerHandler('let', analyzeLet);
    registerHandler('letrec', analyzeLetRec);
    registerHandler('set!', analyzeSet);
    registerHandler('define', analyzeDefine);
    registerHandler('begin', analyzeBegin);
    registerHandler('define-syntax', analyzeDefineSyntax);
    registerHandler('define-macro', analyzeDefineMacro);
    registerHandler('let-syntax', analyzeLetSyntax);
    registerHandler('letrec-syntax', analyzeLetrecSyntax);
    registerHandler('quasiquote', (exp, env, ctx) => expandQuasiquote(cadr(exp), env, ctx));
}

/**
 * Analyzes (define-syntax <name> <transformer-spec>).
 * Compiles syntax-rules and registers them in the current macro registry.
 * 
 * @param {Cons} exp - The expression.
 * @returns {LiteralNode}
 */
/**
 * Analyzes (define-syntax <name> <transformer-spec>).
 * Compiles syntax-rules and registers them in the current macro registry.
 * 
 * @param {Cons} exp - The expression.
 * @param {SyntacticEnv} [syntacticEnv=null] - The environment.
 * @param {InterpreterContext} ctx - The context.
 * @returns {LiteralNode}
 */
function analyzeDefineSyntax(exp, syntacticEnv = null, ctx) {
    const nameObj = cadr(exp);
    const name = (nameObj instanceof Symbol) ? nameObj.name : syntaxName(nameObj);
    const transformerSpec = caddr(exp);

    if (transformerSpec instanceof Cons) {
        const srKeyword = car(transformerSpec);
        const isSyntaxRules = (srKeyword instanceof Symbol && srKeyword.name === 'syntax-rules') ||
            (isSyntaxObject(srKeyword) && syntaxName(srKeyword) === 'syntax-rules');

        if (isSyntaxRules) {
            let ellipsisName = '...';
            let literalsList;
            let clausesList;
            const afterSyntaxRules = cdr(transformerSpec);
            const firstArg = car(afterSyntaxRules);

            // Handle optional ellipsis: (syntax-rules ellipsis (literals) (pattern template)...)
            // R7RS allows the first argument after syntax-rules to be a custom ellipsis identifier.
            if (firstArg instanceof Cons || firstArg === null) {
                literalsList = firstArg;
                clausesList = cdr(afterSyntaxRules);
            } else if (firstArg instanceof Symbol || isSyntaxObject(firstArg)) {
                ellipsisName = firstArg instanceof Symbol ? firstArg.name : syntaxName(firstArg);
                literalsList = cadr(afterSyntaxRules);
                clausesList = cddr(afterSyntaxRules);
            } else {
                return new LiteralNode(null);
            }

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

            // Capture the current defining scope for hygiene
            const currentScopes = globalContext.getDefiningScopes();
            const definingScope = currentScopes.length > 0 ? currentScopes[currentScopes.length - 1] : GLOBAL_SCOPE_ID;

            const transformer = compileSyntaxRules(literals, clauses, definingScope, ellipsisName, syntacticEnv);
            ctx.currentMacroRegistry.define(name, transformer);
            return new LiteralNode(null);
        }
    }
    return new LiteralNode(null);
}

/**
 * Analyzes (define-macro (name args...) body...) or (define-macro name transformer).
 * Evaluates the transformer immediately in a fresh environment and registers it.
 *
 * @param {Cons} exp - The expression.
 * @param {SyntacticEnv} [syntacticEnv=null] - The environment.
 * @param {InterpreterContext} ctx - The context.
 * @returns {LiteralNode}
 */
function analyzeDefineMacro(exp, syntacticEnv = null, ctx) {
    const head = cadr(exp);
    let name;
    let transformerLambdaAst;

    // Case 1: (define-macro (name args...) body...)
    if (head instanceof Cons) {
        const nameObj = car(head);
        const args = cdr(head);
        const body = cddr(exp);
        name = (nameObj instanceof Symbol) ? nameObj.name : syntaxName(nameObj);

        // Construct lambda: (lambda args body...)
        const lambdaExp = cons(intern('lambda'), cons(args, body));
        transformerLambdaAst = analyzeLambda(lambdaExp, syntacticEnv, ctx);
        // That lambda was built here rather than read, so it has no source of
        // its own. Its extent is the whole `(define-macro (name args...) ...)`
        // form, and without it the transformer procedure reports no source,
        // so the debugger cannot place it.
        if (exp.source && !transformerLambdaAst.source) {
            transformerLambdaAst.source = exp.source;
        }
    }
    // Case 2: (define-macro name transformer-proc)
    else if (head instanceof Symbol || isSyntaxObject(head)) {
        name = (head instanceof Symbol) ? head.name : syntaxName(head);
        const transformerExp = caddr(exp);
        transformerLambdaAst = analyze(transformerExp, syntacticEnv, ctx);
    } else {
        throw new SchemeSyntaxError('Invalid define-macro syntax', exp, 'define-macro');
    }

    // 1. Create a temporary interpreter with standard primitives
    // We use a separate context for the expansion environment to avoid polluting
    // the global state, but we might want to share state in the future.
    // For now, "expansion time" is a fresh environment with standard library.
    //
    // It is deliberately given no debug runtime. Expansion runs here, inside
    // the analyzer, which is synchronous and finishes before the interpreter
    // runs a step of the code being expanded, while the debugger can only make
    // execution wait between the steps of an asynchronous run. A breakpoint
    // that fired inside a transformer could therefore stop nothing: it would
    // report a pause and leave the debugger paused, so that the program then
    // stopped at its first step, somewhere the report did not name. Instead
    // the debugger reports such breakpoints as never firing, finding the
    // transformer by the `transformerProcedure` attached below.
    const expansionInterpreter = new Interpreter(ctx);
    const expansionEnv = createGlobalEnvironment(expansionInterpreter);
    expansionInterpreter.setGlobalEnv(expansionEnv);

    // 2. Evaluate the transformer to get a closure
    let transformerClosure;
    try {
        // Run synchronously - transformers must be available immediately
        transformerClosure = expansionInterpreter.run(transformerLambdaAst, expansionEnv);
    } catch (e) {
        throw new SchemeSyntaxError(`Error evaluating macro transformer for '${name}': ${e.message}`, exp, 'define-macro');
    }

    // 3. Create the JS transformer wrapper
    // The wrapper receives the macro call expression (name arg1 arg2 ...)
    // It extracts the arguments and calls the Scheme closure.
    const jsTransformer = (macroCallExp, useSiteEnv) => {
        const argsList = cdr(macroCallExp); // (arg1 arg2 ...)

        // Invoke the closure with the arguments list
        // Note: The transformer returns a Scheme expression (AST/Cons)
        // which the analyzer will then recursively analyze.

        // We use runWithSentinel to ensure proper stack handling if the macro calls back into JS (unlikely but possible)
        // But for simple closure invocation, we can construct an application AST.

        // However, we have a raw closure object and raw arguments (Cons list).
        // The simplest way is to use the interpreter's invokeContinuation-like logic or apply primitive logic.

        // Let's manually construct an Apply invocation or similar.
        // Or simpler: use expansionInterpreter.run with a TailAppNode if we wrap the closure in a LiteralNode.

        // We need to convert the Cons list of args into an array of AST nodes?
        // No, the closure expects Scheme values (Cons list of syntax).
        // Wait, (define-macro (f x) ...) expects x to be passed as argument.
        // The macro call is (f arg1).
        // So the transformer should be called with `arg1`.

        // If the macro is (define-macro (f . args) ...), it expects `args` as a list.
        // If the macro is (define-macro (f x) ...), it expects `x`.

        // We need to `apply` the closure to the arguments list.
        // The `apply` primitive in Scheme does exactly this.

        try {
            // Use the expansion interpreter to run (apply closure argsList)
            const applyNode = new TailAppNode(
                new VariableNode('apply'),
                [
                    new LiteralNode(transformerClosure),
                    new LiteralNode(argsList)
                ]
            );

            return expansionInterpreter.run(applyNode, expansionEnv);
        } catch (e) {
            throw new SchemeSyntaxError(`Error expanding macro '${name}': ${e.message}`, macroCallExp, name);
        }
    };

    // 4. Register the transformer. The Scheme procedure is kept on the
    // wrapper, which is all the registry holds, so that tools such as the
    // debugger can find the transformer's source and name.
    jsTransformer.transformerProcedure = transformerClosure;
    ctx.currentMacroRegistry.define(name, jsTransformer);

    return new LiteralNode(null);
}

/**
 * Analyzes (let-syntax (<binding>...) <body>).
 * @param {Cons} exp - The expression.
 * @returns {ASTNode}
 */
function analyzeLetSyntax(exp, syntacticEnv, ctx) {
    const bindings = cadr(exp);
    const body = cddr(exp);
    if (body === null) throw new SchemeSyntaxError('body cannot be empty', exp, 'let-syntax');

    const localRegistry = new MacroRegistry(ctx.currentMacroRegistry);
    let curr = bindings;
    while (curr instanceof Cons) {
        const binding = curr.car;
        const name = syntaxName(car(binding));
        const transformerSpec = cadr(binding);
        const transformer = compileTransformerSpec(transformerSpec, syntacticEnv);
        localRegistry.define(name, transformer);
        curr = curr.cdr;
    }

    const savedRegistry = ctx.currentMacroRegistry;
    ctx.currentMacroRegistry = localRegistry;
    try {
        // Desugar to let for body analysis
        const letExp = cons(intern('let'), cons(null, body));
        return analyzeWithCurrentMacroRegistry(letExp, syntacticEnv, ctx);
    } finally {
        ctx.currentMacroRegistry = savedRegistry;
    }
}

/**
 * Analyzes (letrec-syntax (<binding>...) <body>).
 * @param {Cons} exp - The expression.
 * @returns {ASTNode}
 */
function analyzeLetrecSyntax(exp, syntacticEnv, ctx) {
    const bindings = cadr(exp);
    const body = cddr(exp);
    if (body === null) throw new SchemeSyntaxError('body cannot be empty', exp, 'letrec-syntax');

    const localRegistry = new MacroRegistry(ctx.currentMacroRegistry);
    const savedRegistry = ctx.currentMacroRegistry;
    ctx.currentMacroRegistry = localRegistry;

    try {
        let curr = bindings;
        while (curr instanceof Cons) {
            const binding = curr.car;
            const name = syntaxName(car(binding));
            const transformerSpec = cadr(binding);
            // In letrec-syntax, transformer compilation can see the local bindings
            const transformer = compileTransformerSpec(transformerSpec, syntacticEnv);
            localRegistry.define(name, transformer);
            curr = curr.cdr;
        }

        const bodyExprs = toArray(body);
        const analyzedBody = bodyExprs.map(e => analyzeWithCurrentMacroRegistry(e, syntacticEnv, ctx));
        if (analyzedBody.length === 1) return analyzedBody[0];
        return new BeginNode(analyzedBody);
    } finally {
        ctx.currentMacroRegistry = savedRegistry;
    }
}

/**
 * Compiles a syntax-rules spec from a let-syntax or letrec-syntax binding.
 * @private
 */
function compileTransformerSpec(transformerSpec, syntacticEnv = null) {
    if (!(transformerSpec instanceof Cons)) throw new SchemeSyntaxError('Transformer must be (syntax-rules ...)', transformerSpec, 'syntax-rules');
    const keyword = car(transformerSpec);
    const keywordName = (keyword instanceof Symbol) ? keyword.name : (isSyntaxObject(keyword) ? syntaxName(keyword) : null);
    if (keywordName !== 'syntax-rules') throw new SchemeSyntaxError('Transformer must be (syntax-rules ...)', transformerSpec, 'syntax-rules');

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

    const currentScopes = globalContext.getDefiningScopes();
    const definingScope = currentScopes.length > 0 ? currentScopes[currentScopes.length - 1] : GLOBAL_SCOPE_ID;
    return compileSyntaxRules(literals, clauses, definingScope, '...', syntacticEnv);
}

/**
 * Expands (quasiquote <exp>) into basic application nodes (cons, list, append).
 * Handles unquote and unquote-splicing with nested levels.
 * 
 * @param {*} exp - The template.
 * @param {SyntacticEnv} syntacticEnv - environment.
 * @param {InterpreterContext} ctx - context.
 * @param {number} [nesting=0] - Nested quasiquote depth.
 * @returns {ASTNode}
 */
function expandQuasiquote(exp, syntacticEnv, ctx, nesting = 0) {
    if (isTaggedList(exp, 'quasiquote')) {
        return listApp('list', [new LiteralNode(intern('quasiquote')), expandQuasiquote(cadr(exp), syntacticEnv, ctx, nesting + 1)]);
    }
    if (isTaggedList(exp, 'unquote')) {
        if (nesting === 0) return analyze(cadr(exp), syntacticEnv, ctx);
        return listApp('list', [new LiteralNode(intern('unquote')), expandQuasiquote(cadr(exp), syntacticEnv, ctx, nesting - 1)]);
    }
    if (isTaggedList(exp, 'unquote-splicing')) {
        if (nesting === 0) throw new SchemeSyntaxError('unquote-splicing not allowed at top level', exp, 'quasiquote');
        return listApp('list', [new LiteralNode(intern('unquote-splicing')), expandQuasiquote(cadr(exp), syntacticEnv, ctx, nesting - 1)]);
    }
    if (exp instanceof Cons) {
        if (isTaggedList(exp.car, 'unquote-splicing') && nesting === 0) {
            // ,@ (splicing)
            return listApp('append', [analyze(cadr(exp.car), syntacticEnv, ctx), expandQuasiquote(exp.cdr, syntacticEnv, ctx, nesting)]);
        }
        return listApp('cons', [expandQuasiquote(exp.car, syntacticEnv, ctx, nesting), expandQuasiquote(exp.cdr, syntacticEnv, ctx, nesting)]);
    }
    if (Array.isArray(exp)) {
        // Vector quasiquotation
        let hasSplicing = false;
        for (const elem of exp) if (isTaggedList(elem, 'unquote-splicing') && nesting === 0) { hasSplicing = true; break; }
        if (hasSplicing) {
            const listForm = exp.reduceRight((acc, el) => cons(el, acc), null);
            return listApp('list->vector', [expandQuasiquote(listForm, syntacticEnv, ctx, nesting)]);
        }
        return listApp('vector', exp.map(e => expandQuasiquote(e, syntacticEnv, ctx, nesting)));
    }
    return new LiteralNode(unwrapSyntax(exp));
}

/**
 * Internal helper to check if an expression is a list starting with a specific tag (Symbol or SyntaxObject).
 * @private
 */
function isTaggedList(exp, tag) {
    if (!(exp instanceof Cons)) return false;
    const head = exp.car;
    if (head instanceof Symbol) return head.name === tag;
    if (isSyntaxObject(head)) return syntaxName(head) === tag;
    return false;
}

/**
 * Creates a TailAppNode for a variable call (helper for quasiquote expansion).
 * @private
 */
function listApp(funcName, args) {
    return new TailAppNode(new VariableNode(funcName), args);
}


// Export individual handlers for direct use if needed
export {
    analyzeQuote,
    analyzeIf,
    analyzeLambda,
    analyzeLet,
    analyzeLetRec,
    analyzeSet,
    analyzeDefine,
    analyzeBegin,
    analyzeBody,
    analyzeScopedBody
};
