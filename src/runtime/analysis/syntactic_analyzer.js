import { Executable, Literal, Variable, TailApp } from '../ast.js';
import { Cons, mapCons, car, cdr } from '../cons.js';
import { Symbol } from '../symbol.js';

import {
    analyzeIf,
    analyzeLet,
    analyzeLetRec,
    analyzeLambda,
    analyzeSet,
    analyzeDefine,
    analyzeDefineSyntax,
    analyzeCallCC,
    analyzeBegin,
    analyzeQuote,
    analyzeQuasiquote
} from './special_forms.js';

/**
 * A modular syntactic analyzer for Scheme S-expressions.
 */
export class SyntacticAnalyzer {
    /**
     * @param {MacroRegistry} macroRegistry - Registry for macro transformers.
     */
    constructor(macroRegistry) {
        this.macroRegistry = macroRegistry;
        this.specialForms = new Map();
        this.registerDefaultSpecialForms();
    }

    /**
     * Registers a handler for a special form.
     * @param {string} name 
     * @param {Function} handler (exp, analyzer) => AST
     */
    registerSpecialForm(name, handler) {
        this.specialForms.set(name, handler);
    }

    /**
     * Populates the registry with standard Scheme special forms.
     */
    registerDefaultSpecialForms() {
        this.registerSpecialForm('if', analyzeIf);
        this.registerSpecialForm('let', analyzeLet);
        this.registerSpecialForm('letrec', analyzeLetRec);
        this.registerSpecialForm('lambda', analyzeLambda);
        this.registerSpecialForm('set!', analyzeSet);
        this.registerSpecialForm('define', analyzeDefine);
        this.registerSpecialForm('define-syntax', analyzeDefineSyntax);
        this.registerSpecialForm('call/cc', analyzeCallCC);
        this.registerSpecialForm('call-with-current-continuation', analyzeCallCC);
        this.registerSpecialForm('begin', analyzeBegin);
        this.registerSpecialForm('quote', analyzeQuote);
        this.registerSpecialForm('quasiquote', analyzeQuasiquote);
    }

    /**
     * Analyzes an S-expression and converts it to an AST.
     * @param {*} exp 
     * @returns {Executable} AST node
     */
    analyze(exp) {
        // 1. Handle Atoms
        if (exp instanceof Symbol) {
            return new Variable(exp.name);
        }
        if (typeof exp === 'number' || typeof exp === 'string' || typeof exp === 'boolean' || exp === null) {
            return new Literal(exp);
        }
        if (Array.isArray(exp)) {
            return new Literal(exp); // Vectors
        }
        if (exp instanceof Executable) {
            return exp; // Already analyzed
        }

        // 2. Handle Lists (Cons)
        if (exp instanceof Cons) {
            const tag = exp.car;

            if (tag instanceof Symbol) {
                const name = tag.name;

                // A. Special Forms
                if (this.specialForms.has(name)) {
                    return this.specialForms.get(name)(exp, this);
                }

                // B. Macros
                if (this.macroRegistry && this.macroRegistry.isMacro(name)) {
                    const transformer = this.macroRegistry.lookup(name);
                    const expanded = transformer(exp);
                    return this.analyze(expanded); // Recursively analyze expansion
                }
            }

            // C. Function Application
            const func = this.analyze(exp.car);
            const args = mapCons(exp.cdr, (e) => this.analyze(e));
            return new TailApp(func, args);
        }

        throw new Error(`Analyzer error: Unknown expression type: ${exp}`);
    }

    /**
     * Registers a new macro.
     * @param {string} name 
     * @param {Function} transformer 
     */
    registerMacro(name, transformer) {
        if (this.macroRegistry) {
            this.macroRegistry.define(name, transformer);
        } else {
            throw new Error("Cannot register macro: No macro registry configured.");
        }
    }
}
