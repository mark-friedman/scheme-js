/**
 * Module Form Handlers
 * 
 * Handlers for module-related special forms:
 * import, define-library
 */

import { ImportNode, DefineLibraryNode } from '../ast.js';
import { cdr, toArray, Cons, cons, list } from '../cons.js';
import { loadLibrarySync, parseImportSet, applyImports, parseDefineLibrary, evaluateLibraryDefinitionSync, evaluateFeatureRequirement } from '../library_loader.js';
import { Symbol, intern } from '../symbol.js';
import { registerHandler } from './registry.js';
import { SchemeSyntaxError } from '../errors.js';

// Set by analyzer initialization
let analyze;

/**
 * Initializes the module forms with dependencies from the main analyzer.
 * @param {Object} deps - Dependencies
 */
export function initModuleForms(deps) {
    analyze = deps.analyze;
}

// =============================================================================
// Handler Functions
// =============================================================================

function analyzeImport(exp, syntacticEnv, ctx) {
    const specs = toArray(cdr(exp)).map(spec => parseImportSet(spec));
    return new ImportNode(specs, loadLibrarySync, applyImports, analyze);
}

function analyzeDefineLibrary(exp, syntacticEnv, ctx) {
    const libDef = parseDefineLibrary(exp);
    return new DefineLibraryNode(libDef, evaluateLibraryDefinitionSync, analyze);
}

function analyzeCondExpand(exp, syntacticEnv, ctx) {
    const expanded = expandCondExpand(exp);
    return analyze(expanded, syntacticEnv, ctx);
}

function expandCondExpand(exp) {
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
            if (clauseArr.length === 1) return list(intern('begin'));
            if (clauseArr.length === 2) return clauseArr[1];
            return cons(intern('begin'), list(...clauseArr.slice(1)));
        }
    }
    throw new SchemeSyntaxError('no matching clause and no else', exp, 'cond-expand');
}


// =============================================================================
// Registration
// =============================================================================

export function registerModuleForms() {
    registerHandler('import', analyzeImport);
    registerHandler('define-library', analyzeDefineLibrary);
    registerHandler('cond-expand', analyzeCondExpand);
}

export {
    analyzeImport,
    analyzeDefineLibrary
};
