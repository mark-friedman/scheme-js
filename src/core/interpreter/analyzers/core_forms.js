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
    DefineNode
} from '../ast.js';
import { Cons, cons, list, car, cdr, toArray, cadr, cddr, caddr, cdddr } from '../cons.js';
import { Symbol, intern } from '../symbol.js';
import { isSyntaxObject, syntaxName, unwrapSyntax, GLOBAL_SCOPE_ID } from '../syntax_object.js';
import { globalContext } from '../context.js';
import { MacroRegistry } from '../macro_registry.js';
import { registerHandler } from './registry.js';
import { compileSyntaxRules } from '../syntax_rules.js';

// These will be set by the analyzer when it initializes
let analyze;
let generateUniqueName;

/**
 * Initializes the core forms with dependencies from the main analyzer.
 * This breaks the circular dependency between core_forms and analyzer.
 * @param {Object} deps - Dependencies
 */
export function initCoreForms(deps) {
    analyze = deps.analyze;
    generateUniqueName = deps.generateUniqueName;
}

// (No local currentMacroRegistry - now in InterpreterContext)

function analyzeWithCurrentMacroRegistry(exp, syntacticEnv, ctx) {
    if (exp instanceof Cons) {
        const tag = exp.car;
        if (tag instanceof Symbol && ctx.currentMacroRegistry.isMacro(tag.name)) {
            const transformer = ctx.currentMacroRegistry.lookup(tag.name);
            const expanded = transformer(exp, syntacticEnv);
            return analyzeWithCurrentMacroRegistry(expanded, syntacticEnv, ctx);
        }
    }
    return analyze(exp, syntacticEnv, ctx);
}


// =============================================================================
// Handler Functions
// =============================================================================

function analyzeQuote(exp, syntacticEnv, ctx) {
    const text = cadr(exp);
    return new LiteralNode(unwrapSyntax(text));
}

function analyzeIf(exp, syntacticEnv, ctx) {
    const test = analyze(cadr(exp), syntacticEnv, ctx);
    const consequent = analyze(caddr(exp), syntacticEnv, ctx);
    let alternative;

    if (cdddr(exp) === null) {
        alternative = new LiteralNode(undefined);
    } else {
        alternative = analyze(exp.cdr.cdr.cdr.car, syntacticEnv, ctx);
    }

    return new IfNode(test, consequent, alternative);
}

function analyzeLambda(exp, syntacticEnv, ctx) {
    const paramsPart = cadr(exp);
    const body = cddr(exp);

    if (body === null) {
        throw new Error('lambda: body cannot be empty');
    }

    const newParams = [];
    let newEnv = syntacticEnv;
    let restParamRenamed = null;

    let curr = paramsPart;

    // Handle: (lambda single-symbol body...)
    if (!(curr instanceof Cons) && (curr instanceof Symbol || isSyntaxObject(curr))) {
        const name = (curr instanceof Symbol) ? curr.name : syntaxName(curr);
        restParamRenamed = generateUniqueName(name, ctx);
        newEnv = newEnv.extend(curr, restParamRenamed);
        return new LambdaNode(newParams, analyzeBody(body, newEnv, ctx), restParamRenamed);
    }

    while (curr instanceof Cons) {
        const param = curr.car;
        if (!(param instanceof Symbol) && !isSyntaxObject(param)) {
            throw new Error('lambda: parameter must be a symbol');
        }
        const name = (param instanceof Symbol) ? param.name : syntaxName(param);
        const renamed = generateUniqueName(name, ctx);

        newParams.push(renamed);
        newEnv = newEnv.extend(param, renamed);

        curr = curr.cdr;
    }

    if (curr !== null) {
        if (curr instanceof Symbol || isSyntaxObject(curr)) {
            const name = (curr instanceof Symbol) ? curr.name : syntaxName(curr);
            restParamRenamed = generateUniqueName(name, ctx);
            newEnv = newEnv.extend(curr, restParamRenamed);
        }
    }

    return new LambdaNode(newParams, analyzeScopedBody(body, newEnv, ctx), restParamRenamed);
}

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

function analyzeBody(body, syntacticEnv, ctx) {
    const bodyArray = toArray(body);

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
                    const nameObj = car(defHead);
                    definedName = (nameObj instanceof Symbol) ? nameObj.name : syntaxName(nameObj);
                    bindKey = nameObj;
                } else if (defHead instanceof Symbol || isSyntaxObject(defHead)) {
                    definedName = (defHead instanceof Symbol) ? defHead.name : syntaxName(defHead);
                }

                if (definedName) {
                    extendedEnv = extendedEnv.extend(bindKey, definedName);
                }
            }
        }
    }

    const exprs = bodyArray.map(e => analyze(e, extendedEnv, ctx));
    if (exprs.length === 1) {
        return exprs[0];
    }
    return new BeginNode(exprs);
}

function analyzeBegin(exp, syntacticEnv, ctx) {
    const body = cdr(exp);
    return analyzeBody(body, syntacticEnv, ctx);
}

function analyzeLet(exp, syntacticEnv, ctx) {
    const bindings = cadr(exp);
    const body = cddr(exp);

    // Named let check
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

        const lambdaExp = cons(intern('lambda'), cons(varsList, namedBody));
        const letrecBindings = list(list(loopName, lambdaExp));
        const initialCall = cons(loopName, valsList);

        const letrecExp = list(intern('letrec'), letrecBindings, initialCall);
        return analyzeLetRec(letrecExp, syntacticEnv, ctx);
    }

    const vars = [];
    const args = [];
    const renos = [];
    let newEnv = syntacticEnv;

    let curr = bindings;
    while (curr instanceof Cons) {
        const pair = curr.car;
        const varObj = car(pair);
        const valObj = cadr(pair);

        const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
        const renamed = generateUniqueName(name, ctx);

        vars.push(renamed);
        renos.push({ id: varObj, name: renamed });
        args.push(analyze(valObj, syntacticEnv, ctx));

        curr = curr.cdr;
    }

    for (const r of renos) {
        newEnv = newEnv.extend(r.id, r.name);
    }

    return new TailAppNode(
        new LambdaNode(vars, analyzeScopedBody(body, newEnv, ctx)),
        args
    );
}

function analyzeLetRec(exp, syntacticEnv, ctx) {
    const bindings = cadr(exp);
    const body = cddr(exp);

    let newEnv = syntacticEnv;
    const vars = [];
    const args = [];
    const renos = [];

    let curr = bindings;
    while (curr instanceof Cons) {
        const pair = curr.car;
        const varObj = car(pair);

        const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
        const renamed = generateUniqueName(name, ctx);

        vars.push(renamed);
        renos.push({ id: varObj, name: renamed, valObj: cadr(pair) });

        newEnv = newEnv.extend(varObj, renamed);
        curr = curr.cdr;
    }

    for (const r of renos) {
        args.push(analyze(r.valObj, newEnv, ctx));
    }

    const undefinedLit = new LiteralNode(undefined);
    const setExprs = [];
    for (let i = 0; i < renos.length; i++) {
        setExprs.push(new SetNode(renos[i].name, args[i]));
    }

    const bodyExpr = analyzeScopedBody(body, newEnv, ctx);
    const seq = new BeginNode([...setExprs, bodyExpr]);

    return new TailAppNode(
        new LambdaNode(vars, seq),
        vars.map(_ => undefinedLit)
    );
}

function analyzeSet(exp, syntacticEnv, ctx) {
    const varObj = cadr(exp);
    const valExpr = analyze(caddr(exp), syntacticEnv, ctx);

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

    const renamed = syntacticEnv.lookup(varObj);
    if (renamed) {
        return new SetNode(renamed, valExpr);
    }

    const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
    return new SetNode(name, valExpr);
}

function analyzeDefine(exp, syntacticEnv, ctx) {
    const head = cadr(exp);
    if (head instanceof Cons) {
        const nameObj = car(head);
        const args = cdr(head);
        const body = cddr(exp);

        const lambdaExp = cons(intern('lambda'), cons(args, body));
        const valExpr = analyzeLambda(lambdaExp, syntacticEnv, ctx);

        const name = (nameObj instanceof Symbol) ? nameObj.name : syntaxName(nameObj);
        return new DefineNode(name, valExpr);
    }

    const varObj = head;
    const valExpr = analyze(caddr(exp), syntacticEnv, ctx);

    const name = (varObj instanceof Symbol) ? varObj.name : syntaxName(varObj);
    return new DefineNode(name, valExpr);
}

// =============================================================================
// Registration
// =============================================================================

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
    registerHandler('let-syntax', analyzeLetSyntax);
    registerHandler('letrec-syntax', analyzeLetrecSyntax);
    registerHandler('quasiquote', (exp, env, ctx) => expandQuasiquote(cadr(exp), env, ctx));
}

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

            if (firstArg instanceof Cons || firstArg === null) {
                literalsList = firstArg;
                clausesList = cdr(afterSyntaxRules);
            } else if (firstArg instanceof Symbol || firstArg instanceof SyntaxObject) {
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

            const currentScopes = globalContext.getDefiningScopes();
            const definingScope = currentScopes.length > 0 ? currentScopes[currentScopes.length - 1] : GLOBAL_SCOPE_ID;
            const transformer = compileSyntaxRules(literals, clauses, definingScope, ellipsisName, syntacticEnv);
            ctx.currentMacroRegistry.define(name, transformer);
            return new LiteralNode(null);
        }
    }
    return new LiteralNode(null);
}

function analyzeLetSyntax(exp, syntacticEnv, ctx) {
    const bindings = cadr(exp);
    const body = cddr(exp);
    if (body === null) throw new Error('let-syntax: body cannot be empty');

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
        const letExp = cons(intern('let'), cons(null, body));
        return analyzeWithCurrentMacroRegistry(letExp, syntacticEnv, ctx);
    } finally {
        ctx.currentMacroRegistry = savedRegistry;
    }
}

function analyzeLetrecSyntax(exp, syntacticEnv, ctx) {
    const bindings = cadr(exp);
    const body = cddr(exp);
    if (body === null) throw new Error('letrec-syntax: body cannot be empty');

    const localRegistry = new MacroRegistry(ctx.currentMacroRegistry);
    const savedRegistry = ctx.currentMacroRegistry;
    ctx.currentMacroRegistry = localRegistry;

    try {
        let curr = bindings;
        while (curr instanceof Cons) {
            const binding = curr.car;
            const name = syntaxName(car(binding));
            const transformerSpec = cadr(binding);
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

function compileTransformerSpec(transformerSpec, syntacticEnv = null) {
    if (!(transformerSpec instanceof Cons)) throw new Error('Transformer must be (syntax-rules ...)');
    const keyword = car(transformerSpec);
    const keywordName = (keyword instanceof Symbol) ? keyword.name : (isSyntaxObject(keyword) ? syntaxName(keyword) : null);
    if (keywordName !== 'syntax-rules') throw new Error('Transformer must be (syntax-rules ...)');

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

function expandQuasiquote(exp, syntacticEnv, ctx, nesting = 0) {
    if (isTaggedList(exp, 'quasiquote')) {
        return listApp('list', [new LiteralNode(intern('quasiquote')), expandQuasiquote(cadr(exp), syntacticEnv, ctx, nesting + 1)]);
    }
    if (isTaggedList(exp, 'unquote')) {
        if (nesting === 0) return analyze(cadr(exp), syntacticEnv, ctx);
        return listApp('list', [new LiteralNode(intern('unquote')), expandQuasiquote(cadr(exp), syntacticEnv, ctx, nesting - 1)]);
    }
    if (isTaggedList(exp, 'unquote-splicing')) {
        if (nesting === 0) throw new Error("unquote-splicing not allowed at top level");
        return listApp('list', [new LiteralNode(intern('unquote-splicing')), expandQuasiquote(cadr(exp), syntacticEnv, ctx, nesting - 1)]);
    }
    if (exp instanceof Cons) {
        if (isTaggedList(exp.car, 'unquote-splicing') && nesting === 0) {
            return listApp('append', [analyze(cadr(exp.car), syntacticEnv, ctx), expandQuasiquote(exp.cdr, syntacticEnv, ctx, nesting)]);
        }
        return listApp('cons', [expandQuasiquote(exp.car, syntacticEnv, ctx, nesting), expandQuasiquote(exp.cdr, syntacticEnv, ctx, nesting)]);
    }
    if (Array.isArray(exp)) {
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

function isTaggedList(exp, tag) {
    if (!(exp instanceof Cons)) return false;
    const head = exp.car;
    if (head instanceof Symbol) return head.name === tag;
    if (isSyntaxObject(head)) return syntaxName(head) === tag;
    return false;
}

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
