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

            case 'cond-expand':
                // TODO: Implement feature-based conditional inclusion
                break;

            default:
                throw new Error(`Unknown library clause: ${clauseTag.name}`);
        }
    }

    return { name, exports, imports, body, includes };
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

    // Load includes
    for (const includeFile of libDef.includes) {
        const includeSource = await fileResolver(
            [...libraryName.slice(0, -1), includeFile]
        );
        const includeForms = parse(includeSource);
        for (const form of includeForms) {
            libDef.body.push(form);
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

