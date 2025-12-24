/**
 * Library Parser Module
 * 
 * Parses define-library forms and import specifications.
 * Pure parsing logic - no registry access or file I/O.
 */

import { toArray } from './cons.js';
import { Symbol } from './symbol.js';
import { evaluateFeatureRequirement } from './library_registry.js';

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
        throw new Error('define-library requires a library name');
    }

    const tag = arr[0];
    if (!(tag instanceof Symbol) || tag.name !== 'define-library') {
        throw new Error('Expected define-library');
    }

    const name = toArray(arr[1]);
    const exports = [];
    const imports = [];
    const body = [];
    const includes = [];
    const includesCi = [];  // Case-insensitive includes
    const includeLibraryDeclarations = [];  // Library declaration includes

    // Parse clauses
    for (let i = 2; i < arr.length; i++) {
        const clause = arr[i];
        const clauseArr = toArray(clause);
        const clauseTag = clauseArr[0];

        if (!(clauseTag instanceof Symbol)) {
            throw new Error(`Invalid library clause: ${clause}`);
        }

        switch (clauseTag.name) {
            case 'export':
                // (export id ...)
                for (let j = 1; j < clauseArr.length; j++) {
                    const spec = clauseArr[j];
                    if (spec instanceof Symbol) {
                        exports.push({ internal: spec.name, external: spec.name });
                    } else if (Array.isArray(toArray(spec))) {
                        // (rename internal external)
                        const renameArr = toArray(spec);
                        if (renameArr[0] instanceof Symbol && renameArr[0].name === 'rename') {
                            exports.push({
                                internal: renameArr[1].name,
                                external: renameArr[2].name
                            });
                        }
                    }
                }
                break;

            case 'import':
                // (import import-set ...)
                for (let j = 1; j < clauseArr.length; j++) {
                    imports.push(parseImportSet(clauseArr[j]));
                }
                break;

            case 'begin':
                // (begin expr ...)
                for (let j = 1; j < clauseArr.length; j++) {
                    body.push(clauseArr[j]);
                }
                break;

            case 'include':
                // (include filename ...)
                for (let j = 1; j < clauseArr.length; j++) {
                    includes.push(clauseArr[j]);
                }
                break;

            case 'include-ci':
                // (include-ci filename ...) - case-insensitive include
                for (let j = 1; j < clauseArr.length; j++) {
                    includesCi.push(clauseArr[j]);
                }
                break;

            case 'include-library-declarations':
                // (include-library-declarations filename ...)
                for (let j = 1; j < clauseArr.length; j++) {
                    includeLibraryDeclarations.push(clauseArr[j]);
                }
                break;

            case 'cond-expand':
                // (cond-expand <clause> ...)
                // Each clause: (<feature-requirement> <declaration> ...)
                // or (else <declaration> ...)
                for (let j = 1; j < clauseArr.length; j++) {
                    const ceClause = toArray(clauseArr[j]);
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
                            const decl = ceClause[k];
                            const declArr = toArray(decl);
                            if (declArr.length === 0) continue;

                            const declTag = declArr[0];
                            if (!(declTag instanceof Symbol)) continue;

                            // Process the declaration as if it were a top-level clause
                            switch (declTag.name) {
                                case 'export':
                                    for (let m = 1; m < declArr.length; m++) {
                                        const spec = declArr[m];
                                        if (spec instanceof Symbol) {
                                            exports.push({ internal: spec.name, external: spec.name });
                                        }
                                    }
                                    break;
                                case 'import':
                                    for (let m = 1; m < declArr.length; m++) {
                                        imports.push(parseImportSet(declArr[m]));
                                    }
                                    break;
                                case 'begin':
                                    for (let m = 1; m < declArr.length; m++) {
                                        body.push(declArr[m]);
                                    }
                                    break;
                                case 'include':
                                    for (let m = 1; m < declArr.length; m++) {
                                        includes.push(declArr[m]);
                                    }
                                    break;
                                case 'cond-expand':
                                    // Nested cond-expand - recursive processing would be needed
                                    // For now, skip nested cond-expand
                                    break;
                            }
                        }
                        // Only process first matching clause
                        break;
                    }
                }
                break;

            default:
                throw new Error(`Unknown library clause: ${clauseTag.name}`);
        }
    }

    return { name, exports, imports, body, includes, includesCi, includeLibraryDeclarations };
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
