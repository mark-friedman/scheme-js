import {
    Literal,
    Variable,
    Lambda,
    Let,
    LetRec,
    If,
    Set,
    Define,
    TailApp,
    CallCC,
    Begin
} from '../ast.js';
import { Cons, cons, list, car, cdr, mapCons } from '../cons.js';
import { Symbol, intern } from '../symbol.js';
import { compileSyntaxRules } from '../syntax_rules.js';

// Helper functions for easy access
function cadr(cons) { return cons.cdr.car; }
function cddr(cons) { return cons.cdr.cdr; }
function caddr(cons) { return cons.cdr.cdr.car; }
function cdddr(cons) { return cons.cdr.cdr.cdr; }
function cadddr(cons) { return cons.cdr.cdr.cdr.car; }

/**
 * Analyzes an `if` expression: (if test consequent [alternative])
 */
export function analyzeIf(exp, analyzer) {
    const test = analyzer.analyze(cadr(exp));
    const consequent = analyzer.analyze(caddr(exp));
    const alternative = cdddr(exp) !== null ? analyzer.analyze(cadddr(exp)) : new Literal(null);
    return new If(test, consequent, alternative);
}

/**
 * Analyzes a `let` expression: (let ((var val) ...) body...)
 * Desugars to: ((lambda (vars...) body...) vals...)
 */
export function analyzeLet(exp, analyzer) {
    const bindings = cadr(exp);
    const body = cddr(exp);

    const vars = [];
    const args = [];

    let curr = bindings;
    while (curr instanceof Cons) {
        const pair = curr.car; // (var binding)
        vars.push(car(pair).name);
        args.push(analyzer.analyze(cadr(pair)));
        curr = curr.cdr;
    }

    const bodyExprs = mapCons(body, (e) => analyzer.analyze(e));
    const bodyAST = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);

    return new TailApp(
        new Lambda(vars, bodyAST),
        args
    );
}

/**
 * Analyzes a `letrec` expression: (letrec ((var val) ...) body...)
 * Currently supports a single binding to verify structure.
 */
export function analyzeLetRec(exp, analyzer) {
    const bindings = cadr(exp);
    // Support single binding for now to match AST capability
    const pair = car(bindings);
    const varName = car(pair).name;
    const valExpr = analyzer.analyze(cadr(pair));
    const body = cddr(exp);

    const bodyExprs = mapCons(body, (e) => analyzer.analyze(e));
    const bodyAST = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);

    return new LetRec(varName, valExpr, bodyAST);
}

/**
 * Analyzes a `lambda` expression: (lambda (params...) body...)
 */
export function analyzeLambda(exp, analyzer) {
    const paramsList = cadr(exp);
    const body = cddr(exp);

    const params = [];
    let curr = paramsList;
    while (curr instanceof Cons) {
        params.push(curr.car.name);
        curr = curr.cdr;
    }

    if (body === null) {
        throw new Error("Malformed lambda: body cannot be empty");
    }

    const bodyExprs = mapCons(body, (e) => analyzer.analyze(e));
    const bodyAST = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);

    return new Lambda(params, bodyAST);
}

/**
 * Analyzes a `set!` expression: (set! var val)
 */
export function analyzeSet(exp, analyzer) {
    return new Set(cadr(exp).name, analyzer.analyze(caddr(exp)));
}

/**
 * Analyzes a `define` expression: (define var val) or (define (f args...) body...)
 */
export function analyzeDefine(exp, analyzer) {
    const head = cadr(exp);

    if (head instanceof Symbol) {
        // (define var val)
        return new Define(head.name, analyzer.analyze(caddr(exp)));
    } else if (head instanceof Cons) {
        // (define (f args...) body...)
        const funcName = car(head).name;
        const argsList = cdr(head);
        const body = cddr(exp);

        const params = [];
        let curr = argsList;
        while (curr instanceof Cons) {
            params.push(curr.car.name);
            curr = curr.cdr;
        }

        const bodyExprs = mapCons(body, (e) => analyzer.analyze(e));
        const bodyAST = (bodyExprs.length === 1) ? bodyExprs[0] : new Begin(bodyExprs);

        return new Define(funcName, new Lambda(params, bodyAST));
    }
    throw new Error("Malformed define");
}

/**
 * Analyzes a `call/cc` or `call-with-current-continuation` expression.
 */
export function analyzeCallCC(exp, analyzer) {
    return new CallCC(analyzer.analyze(cadr(exp)));
}

/**
 * Analyzes a `begin` expression: (begin exp...)
 */
export function analyzeBegin(exp, analyzer) {
    return new Begin(mapCons(exp.cdr, (e) => analyzer.analyze(e)));
}

/**
 * Analyzes a `quote` expression: (quote x)
 */
export function analyzeQuote(exp, analyzer) {
    return new Literal(cadr(exp));
}

/**
 * Analyzes a `define-syntax` expression: (define-syntax name transformer-spec)
 */
export function analyzeDefineSyntax(exp, analyzer) {
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

        // Register the macro in the analyzer's macro environment
        analyzer.registerMacro(name, transformer);

        return new Literal(null);
    }
    return new Literal(null);
}

/**
 * Analyzes a `quasiquote` expression by expanding it.
 */
export function analyzeQuasiquote(exp, analyzer) {
    return expandQuasiquote(cadr(exp), 0, analyzer);
}

// Helper for quasiquote expansion
function expandQuasiquote(exp, nesting, analyzer) {
    // 1. Handle (quasiquote x)
    if (isTaggedList(exp, 'quasiquote')) {
        return listApp('list', [
            new Literal(intern('quasiquote')),
            expandQuasiquote(cadr(exp), nesting + 1, analyzer)
        ]);
    }

    // 2. Handle (unquote x)
    if (isTaggedList(exp, 'unquote')) {
        if (nesting === 0) {
            return analyzer.analyze(cadr(exp));
        } else {
            return listApp('list', [
                new Literal(intern('unquote')),
                expandQuasiquote(cadr(exp), nesting - 1, analyzer)
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
                expandQuasiquote(cadr(exp), nesting - 1, analyzer)
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
                analyzer.analyze(cadr(exp.car)),
                expandQuasiquote(exp.cdr, nesting, analyzer)
            ]);
        }

        // Regular cons: (cons (expand car) (expand cdr))
        return listApp('cons', [
            expandQuasiquote(exp.car, nesting, analyzer),
            expandQuasiquote(exp.cdr, nesting, analyzer)
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
