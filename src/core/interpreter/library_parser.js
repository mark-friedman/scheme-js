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
 * The filters an import set can apply to the set inside it (R7RS 5.6.1).
 * @type {Array<string>}
 */
const IMPORT_FILTERS = ['only', 'except', 'prefix', 'rename'];

/**
 * Parses an import set.
 *
 * An import set is a library name, or a filter wrapped around another import
 * set, and filters nest in any order: each applies to the names the set
 * inside it provides, so `(only (prefix lib p:) p:car)` keeps `p:car`, and a
 * fixed order of filters cannot express that. So the filters are returned as
 * steps, innermost first, to be applied in that order (`applyImports`).
 *
 * @param {Cons} importSet - The import specification.
 * @returns {{libraryName: Array<string>, steps: Array<Object>}} The library,
 *   and the filters to apply to its exports, innermost first: `{kind: 'only'
 *   | 'except', names}`, `{kind: 'prefix', prefix}` or `{kind: 'rename',
 *   renames: [{from, to}]}`.
 */
export function parseImportSet(importSet) {
    const arr = toArray(importSet);
    const first = arr[0];
    const filtered = first instanceof Symbol && IMPORT_FILTERS.includes(first.name)
        && arr.length >= 2 && arr[1] !== null && typeof arr[1] === 'object' && 'car' in arr[1];

    if (!filtered) {
        return {
            libraryName: arr.map(s => s instanceof Symbol ? s.name : String(s)),
            steps: []
        };
    }

    const inner = parseImportSet(arr[1]);
    const names = () => arr.slice(2).map(s => s.name);
    let step;
    switch (first.name) {
        case 'only':
        case 'except':
            step = { kind: first.name, names: names() };
            break;
        case 'prefix':
            step = { kind: 'prefix', prefix: arr[2].name };
            break;
        case 'rename':
            step = {
                kind: 'rename',
                renames: arr.slice(2).map((pair) => {
                    const [from, to] = toArray(pair);
                    return { from: from.name, to: to.name };
                })
            };
            break;
    }
    return { libraryName: inner.libraryName, steps: [...inner.steps, step] };
}
