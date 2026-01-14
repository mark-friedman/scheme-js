/**
 * Library Parser Module
 * 
 * Parses define-library forms and import specifications.
 * Pure parsing logic - no registry access or file I/O.
 */

import { toArray } from './cons.js';
import { Symbol } from './symbol.js';
import { evaluateFeatureRequirement } from './library_registry.js';
import { SchemeSyntaxError } from './errors.js';

// =============================================================================
// define-library Parser
// =============================================================================

/**
 * Parses a define-library form and extracts its clauses.
 * 
 * @param {Cons} form - The define-library S-expression
 * @returns {Object} { name, exports, imports, body, includes, includesCi, includeLibraryDeclarations }
 */
export function parseDefineLibrary(form) {
    const arr = toArray(form);

    if (arr.length < 2) {
        throw new SchemeSyntaxError('requires a library name', null, 'define-library');
    }

    const tag = arr[0];
    if (!(tag instanceof Symbol) || tag.name !== 'define-library') {
        throw new SchemeSyntaxError('expected define-library form', form, 'define-library');
    }

    const name = toArray(arr[1]);
    const result = {
        name,
        exports: [],
        imports: [],
        body: [],
        includes: [],
        includesCi: [],
        includeLibraryDeclarations: []
    };

    // Parse clauses using shared processor
    for (let i = 2; i < arr.length; i++) {
        processDeclaration(arr[i], result);
    }

    return result;
}

/**
 * Processes a single library declaration clause.
 * Handles recursion for cond-expand.
 * 
 * @param {Cons} decl - The declaration S-expression
 * @param {Object} result - Accumulator for library components
 */
function processDeclaration(decl, result) {
    const declArr = toArray(decl);
    if (declArr.length === 0) return;

    const tag = declArr[0];
    if (!(tag instanceof Symbol)) {
        throw new SchemeSyntaxError('invalid clause - expected symbol', decl, 'define-library');
    }

    switch (tag.name) {
        case 'export':
            // (export id ...)
            for (let j = 1; j < declArr.length; j++) {
                const spec = declArr[j];
                if (spec instanceof Symbol) {
                    result.exports.push({ internal: spec.name, external: spec.name });
                } else if (Array.isArray(toArray(spec))) {
                    // (rename internal external)
                    const renameArr = toArray(spec);
                    if (renameArr[0] instanceof Symbol && renameArr[0].name === 'rename') {
                        result.exports.push({
                            internal: renameArr[1].name,
                            external: renameArr[2].name
                        });
                    }
                }
            }
            break;

        case 'import':
            // (import import-set ...)
            for (let j = 1; j < declArr.length; j++) {
                result.imports.push(parseImportSet(declArr[j]));
            }
            break;

        case 'begin':
            // (begin expr ...)
            for (let j = 1; j < declArr.length; j++) {
                result.body.push(declArr[j]);
            }
            break;

        case 'include':
            // (include filename ...)
            for (let j = 1; j < declArr.length; j++) {
                result.includes.push(declArr[j]);
            }
            break;

        case 'include-ci':
            // (include-ci filename ...)
            for (let j = 1; j < declArr.length; j++) {
                result.includesCi.push(declArr[j]);
            }
            break;

        case 'include-library-declarations':
            // (include-library-declarations filename ...)
            for (let j = 1; j < declArr.length; j++) {
                result.includeLibraryDeclarations.push(declArr[j]);
            }
            break;

        case 'cond-expand':
            // (cond-expand <clause> ...)
            processCondExpand(declArr, result);
            break;

        default:
            throw new SchemeSyntaxError(`unknown clause: ${tag.name}`, decl, 'define-library');
    }
}

/**
 * Processes a cond-expand declaration.
 * 
 * @param {Array} declArr - The full (cond-expand ...) form as array
 * @param {Object} result - Accumulator
 */
function processCondExpand(declArr, result) {
    for (let j = 1; j < declArr.length; j++) {
        const ceClause = toArray(declArr[j]);
        if (ceClause.length === 0) continue;

        const featureReq = ceClause[0];
        let matched = false;

        // Check for 'else' clause
        if (featureReq instanceof Symbol && featureReq.name === 'else') {
            matched = true;
        } else {
            matched = evaluateFeatureRequirement(featureReq);
        }

        if (matched) {
            // Expand declarations from this clause
            for (let k = 1; k < ceClause.length; k++) {
                processDeclaration(ceClause[k], result);
            }
            // Only process first matching clause
            break;
        }
    }
}

// =============================================================================
// Import Set Parser
// =============================================================================

/**
 * Parses an import set.
 * 
 * @param {Cons|Symbol} importSet - The import specification
 * @returns {Object} { libraryName, only, except, prefix, rename }
 */
export function parseImportSet(importSet) {
    const arr = toArray(importSet);
    const first = arr[0];

    // Simple case: (library name)
    if (first instanceof Symbol && !['only', 'except', 'prefix', 'rename'].includes(first.name)) {
        return {
            libraryName: arr.map(s => s instanceof Symbol ? s.name : String(s)),
            only: null,
            except: null,
            prefix: null,
            rename: null
        };
    }

    // Filtered imports
    const result = { libraryName: null, only: null, except: null, prefix: null, rename: null };

    if (first instanceof Symbol) {
        switch (first.name) {
            case 'only':
                result.only = arr.slice(2).map(s => s.name);
                Object.assign(result, parseImportSet(arr[1]));
                result.only = arr.slice(2).map(s => s.name);
                break;
            case 'except':
                Object.assign(result, parseImportSet(arr[1]));
                result.except = arr.slice(2).map(s => s.name);
                break;
            case 'prefix':
                Object.assign(result, parseImportSet(arr[1]));
                result.prefix = arr[2].name;
                break;
            case 'rename':
                Object.assign(result, parseImportSet(arr[1]));
                result.rename = [];
                for (let i = 2; i < arr.length; i += 2) {
                    result.rename.push({ from: arr[i].name, to: arr[i + 1].name });
                }
                break;
        }
    }

    return result;
}
