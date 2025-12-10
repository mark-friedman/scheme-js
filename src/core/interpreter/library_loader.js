/**
 * R7RS Library Loader
 * 
 * Implements define-library, import, and export.
 * Manages library resolution and prevents duplicate loading.
 */

import { Cons, list, toArray } from './cons.js';
import { Symbol, intern } from './symbol.js';
import { Environment } from './environment.js';
import { parse } from './reader.js';

/**
 * Feature registry for cond-expand.
 * Standard R7RS features plus implementation-specific ones.
 */
const features = new Set([
    'r7rs',           // R7RS Scheme
    'scheme-js',      // This implementation
    'exact-closed',   // Rationals not implemented, but we can claim this for integers
    'ieee-float',     // JavaScript uses IEEE 754
]);

/**
 * Checks if a feature is supported.
 * @param {string} featureName - Feature identifier
 * @returns {boolean}
 */
export function hasFeature(featureName) {
    return features.has(featureName);
}

/**
 * Adds a feature to the registry.
 * @param {string} featureName - Feature identifier
 */
export function addFeature(featureName) {
    features.add(featureName);
}

/**
 * Gets all supported features.
 * @returns {string[]}
 */
export function getFeatures() {
    return Array.from(features);
}

/**
 * Evaluates a cond-expand feature requirement.
 * 
 * @param {Symbol|Cons} requirement - Feature requirement expression
 * @returns {boolean} True if requirement is satisfied
 */
export function evaluateFeatureRequirement(requirement) {
    // Simple feature identifier
    if (requirement instanceof Symbol) {
        return features.has(requirement.name);
    }

    // Compound requirement: (and ...), (or ...), (not ...), (library ...)
    const arr = toArray(requirement);
    if (arr.length === 0) return false;

    const tag = arr[0];
    if (!(tag instanceof Symbol)) return false;

    switch (tag.name) {
        case 'and':
            // All requirements must be true
            for (let i = 1; i < arr.length; i++) {
                if (!evaluateFeatureRequirement(arr[i])) return false;
            }
            return true;

        case 'or':
            // At least one requirement must be true
            for (let i = 1; i < arr.length; i++) {
                if (evaluateFeatureRequirement(arr[i])) return true;
            }
            return false;

        case 'not':
            // Negation
            if (arr.length !== 2) {
                throw new Error('cond-expand: (not) requires exactly one argument');
            }
            return !evaluateFeatureRequirement(arr[1]);

        case 'library':
            // Check if library is available (loaded or loadable)
            if (arr.length !== 2) {
                throw new Error('cond-expand: (library) requires a library name');
            }
            const libName = toArray(arr[1]);
            const libKey = libraryNameToKey(libName);
            return libraryRegistry.has(libKey);

        default:
            // Unknown tag - treat as false
            return false;
    }
}

/**
 * Registry of loaded libraries.
 * Key: stringified library name (e.g., "scheme.base")
 * Value: { exports: Map<string, value>, env: Environment }
 */
const libraryRegistry = new Map();

/**
 * File resolver function (set by runtime).
 * @type {(libraryName: string[]) => Promise<string>}
 */
let fileResolver = null;

/**
 * Sets the file resolver for loading library files.
 * @param {Function} resolver - (libraryName: string[]) => Promise<string>
 */
export function setFileResolver(resolver) {
    fileResolver = resolver;
}

/**
 * Converts a library name to a string key.
 * (scheme base) -> "scheme.base"
 * @param {Array|Cons} name - Library name as list or array
 * @returns {string}
 */
export function libraryNameToKey(name) {
    const parts = Array.isArray(name) ? name : toArray(name);
    return parts.map(p => p instanceof Symbol ? p.name : String(p)).join('.');
}

/**
 * Checks if a library is already loaded.
 * @param {string} key - Library key
 * @returns {boolean}
 */
export function isLibraryLoaded(key) {
    return libraryRegistry.has(key);
}

/**
 * Gets a loaded library's exports.
 * @param {string} key - Library key
 * @returns {Map|null}
 */
export function getLibraryExports(key) {
    const lib = libraryRegistry.get(key);
    return lib ? lib.exports : null;
}

/**
 * Parses a define-library form and extracts its clauses.
 * 
 * @param {Cons} form - The define-library S-expression
 * @returns {Object} { name, exports, imports, body }
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

/**
 * Loads a library by name.
 * 
 * @param {string[]} libraryName - Library name parts
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Promise<Map>} The library's exports
 */
export async function loadLibrary(libraryName, analyze, interpreter, baseEnv) {
    const key = libraryNameToKey(libraryName);

    // Return cached if already loaded
    if (libraryRegistry.has(key)) {
        return libraryRegistry.get(key).exports;
    }

    // Resolve and load file
    if (!fileResolver) {
        throw new Error('No file resolver set. Call setFileResolver first.');
    }

    const source = await fileResolver(libraryName);
    const forms = parse(source);

    if (forms.length === 0) {
        throw new Error(`Empty library file for ${key}`);
    }

    // Parse the define-library form
    const libDef = parseDefineLibrary(forms[0]);

    // Create library environment (child of base env)
    const libEnv = new Environment(baseEnv);

    // Process imports first
    for (const importSpec of libDef.imports) {
        const importExports = await loadLibrary(
            importSpec.libraryName,
            analyze,
            interpreter,
            baseEnv
        );

        // Apply import filters and add to env
        applyImports(libEnv, importExports, importSpec);
    }

    // Load standard includes
    for (const includeFile of libDef.includes) {
        const includeSource = await fileResolver(
            [...libraryName.slice(0, -1), includeFile]
        );
        const includeForms = parse(includeSource);
        for (const form of includeForms) {
            libDef.body.push(form);
        }
    }

    // Load case-insensitive includes
    for (const includeFile of libDef.includesCi) {
        const includeSource = await fileResolver(
            [...libraryName.slice(0, -1), includeFile]
        );
        // Parse with case-folding enabled
        const includeForms = parse(includeSource, { caseFold: true });
        for (const form of includeForms) {
            libDef.body.push(form);
        }
    }

    // Load library declaration includes
    // These contain additional library clauses (export, import, begin, etc.)
    for (const declFile of libDef.includeLibraryDeclarations) {
        const declSource = await fileResolver(
            [...libraryName.slice(0, -1), declFile]
        );
        const declForms = parse(declSource);

        // Process each declaration in the included file
        for (const decl of declForms) {
            const declArr = toArray(decl);
            if (declArr.length === 0) continue;

            const declTag = declArr[0];
            if (!(declTag instanceof Symbol)) continue;

            switch (declTag.name) {
                case 'export':
                    for (let j = 1; j < declArr.length; j++) {
                        const spec = declArr[j];
                        if (spec instanceof Symbol) {
                            libDef.exports.push({ internal: spec.name, external: spec.name });
                        }
                    }
                    break;
                case 'import':
                    for (let j = 1; j < declArr.length; j++) {
                        const importSpec = parseImportSet(declArr[j]);
                        const importExports = await loadLibrary(
                            importSpec.libraryName,
                            analyze,
                            interpreter,
                            baseEnv
                        );
                        applyImports(libEnv, importExports, importSpec);
                    }
                    break;
                case 'begin':
                    for (let j = 1; j < declArr.length; j++) {
                        libDef.body.push(declArr[j]);
                    }
                    break;
                case 'include':
                    for (let j = 1; j < declArr.length; j++) {
                        libDef.includes.push(declArr[j]);
                    }
                    break;
            }
        }
    }

    // Execute body
    for (const expr of libDef.body) {
        const ast = analyze(expr);
        interpreter.run(ast, libEnv);
    }

    // Build exports map
    const exports = new Map();
    for (const exp of libDef.exports) {
        const value = libEnv.lookup(exp.internal);
        exports.set(exp.external, value);
    }

    // Register library
    libraryRegistry.set(key, { exports, env: libEnv });

    return exports;
}

/**
 * Applies import filters to add bindings to an environment.
 * 
 * @param {Environment} env - Target environment
 * @param {Map} exports - Source library exports
 * @param {Object} importSpec - Import specification
 */
function applyImports(env, exports, importSpec) {
    for (const [name, value] of exports) {
        // Check only filter
        if (importSpec.only && !importSpec.only.includes(name)) {
            continue;
        }

        // Check except filter
        if (importSpec.except && importSpec.except.includes(name)) {
            continue;
        }

        // Apply rename
        let finalName = name;
        if (importSpec.rename) {
            const renameEntry = importSpec.rename.find(r => r.from === name);
            if (renameEntry) {
                finalName = renameEntry.to;
            }
        }

        // Apply prefix
        if (importSpec.prefix) {
            finalName = importSpec.prefix + finalName;
        }

        env.define(finalName, value);
    }
}

/**
 * Clears the library registry (for testing).
 */
export function clearLibraryRegistry() {
    libraryRegistry.clear();
}

/**
 * Gets all loaded library keys (for debugging).
 * @returns {string[]}
 */
export function getLoadedLibraries() {
    return Array.from(libraryRegistry.keys());
}

/**
 * Registers a builtin library with exports from JavaScript.
 * Used for (scheme base) and other libraries that need runtime primitives.
 * 
 * @param {string[]} libraryName - Library name parts (e.g., ['scheme', 'base'])
 * @param {Map<string, *>|Object} exports - Exports as Map or object
 * @param {Environment} env - The environment containing the bindings
 */
export function registerBuiltinLibrary(libraryName, exports, env) {
    const key = libraryNameToKey(libraryName);

    // Convert object to Map if needed
    const exportsMap = exports instanceof Map
        ? exports
        : new Map(Object.entries(exports));

    libraryRegistry.set(key, { exports: exportsMap, env });
}

/**
 * Creates (scheme base) exports from the global environment.
 * 
 * @param {Environment} globalEnv - The global environment with primitives
 * @returns {Map<string, *>} The exports map
 */
export function createSchemeBaseExports(globalEnv) {
    const exports = new Map();

    // List of (scheme base) exports
    const baseExports = [
        // Equivalence
        'eq?', 'eqv?', 'equal?',
        // Numbers
        '+', '-', '*', '/', '<', '>', '<=', '>=', '=',
        'number?', 'integer?', 'zero?', 'positive?', 'negative?',
        'abs', 'modulo', 'remainder', 'quotient',
        // Booleans
        'not', 'boolean?',
        // Pairs and lists
        'cons', 'car', 'cdr', 'pair?', 'null?', 'list?',
        'set-car!', 'set-cdr!',
        'list', 'length', 'append', 'reverse',
        'list-ref', 'list-tail',
        'memq', 'memv', 'member',
        'assq', 'assv', 'assoc',
        // Symbols
        'symbol?', 'symbol->string', 'string->symbol',
        // Strings
        'string?', 'string-length', 'string-ref',
        'string=?', 'string<?', 'string>?',
        'string-append', 'substring',
        'number->string', 'string->number',
        // Vectors
        'vector?', 'make-vector', 'vector', 'vector-length',
        'vector-ref', 'vector-set!',
        'vector->list', 'list->vector',
        // Control
        'procedure?', 'apply',
        'call-with-current-continuation', 'call/cc',
        'dynamic-wind',
        // I/O
        'display', 'newline',
    ];

    for (const name of baseExports) {
        // Use findEnv to check if binding exists without throwing
        if (globalEnv.findEnv(name) !== null) {
            exports.set(name, globalEnv.lookup(name));
        }
        // Bindings not found are skipped (might not be implemented yet)
    }

    return exports;
}

