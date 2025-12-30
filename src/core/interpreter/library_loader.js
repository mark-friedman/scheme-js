/**
 * R7RS Library Loader
 * 
 * Handles loading libraries from files and orchestrating the import process.
 * Re-exports registry and parser functionality for backwards compatibility.
 */

import { toArray } from './cons.js';
import { Symbol } from './symbol.js';
import { Environment } from './environment.js';
import { globalMacroRegistry } from './macro_registry.js';
import { parse } from './reader.js';
import { freshScope, pushDefiningScope, popDefiningScope, registerLibraryScope } from './syntax_object.js';

// Import from focused modules
import {
    libraryNameToKey,
    getFileResolver,
    registerLibrary,
    getLibraryExports as _getLibraryExports,
    SYNTAX_KEYWORDS
} from './library_registry.js';
import { parseDefineLibrary, parseImportSet } from './library_parser.js';

// =============================================================================
// Re-exports for backwards compatibility
// =============================================================================

export {
    // Feature registry
    hasFeature,
    addFeature,
    getFeatures,
    evaluateFeatureRequirement,
    // Library registry
    setFileResolver,
    libraryNameToKey,
    isLibraryLoaded,
    getLibraryExports,
    getLoadedLibraries,
    clearLibraryRegistry,
    registerBuiltinLibrary,
    SYNTAX_KEYWORDS
} from './library_registry.js';

export { parseDefineLibrary, parseImportSet } from './library_parser.js';

// =============================================================================
// Library Loading
// =============================================================================

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
    const cached = _getLibraryExports(key);
    if (cached) {
        return cached;
    }

    // Resolve and load file
    const fileResolver = getFileResolver();
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

    return await evaluateLibraryDefinition(libDef, analyze, interpreter, baseEnv);
}

/**
 * Loads a library by name synchronously (Node.js only).
 * 
 * @param {string[]} libraryName - Library name parts
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Map} The library's exports
 */
export function loadLibrarySync(libraryName, analyze, interpreter, baseEnv) {
    const key = libraryNameToKey(libraryName);

    // Return cached if already loaded
    const cached = _getLibraryExports(key);
    if (cached) {
        return cached;
    }

    // Resolve and load file
    const fileResolver = getFileResolver();
    if (!fileResolver) {
        throw new Error('No file resolver set. Call setFileResolver first.');
    }

    const source = fileResolver(libraryName);
    if (source instanceof Promise) {
        throw new Error(`loadLibrarySync encountered async file resolver for ${key}`);
    }

    const forms = parse(source);

    if (forms.length === 0) {
        throw new Error(`Empty library file for ${key}`);
    }

    // Parse the define-library form
    const libDef = parseDefineLibrary(forms[0]);

    return evaluateLibraryDefinitionSync(libDef, analyze, interpreter, baseEnv);
}

/**
 * Evaluates a parsed library definition and registers it.
 * 
 * @param {Object} libDef - The parsed library definition
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Promise<Map>} The library's exports
 */
export async function evaluateLibraryDefinition(libDef, analyze, interpreter, baseEnv) {
    const libraryName = libDef.name;
    const key = libraryNameToKey(libraryName);

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

    // Resolve includes using file resolver
    const fileResolver = getFileResolver();
    if (fileResolver) {
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
    }

    // Execute body with a defining scope for referential transparency
    // Bindings defined here will be registered in the scope registry
    // so macros can reference them hygienically
    const libraryScope = freshScope();
    registerLibraryScope(libraryScope, libEnv);
    pushDefiningScope(libraryScope);

    try {
        for (const expr of libDef.body) {
            const ast = analyze(expr);
            interpreter.run(ast, libEnv);
        }
    } finally {
        popDefiningScope();
    }

    // Build exports map
    const exports = new Map();
    for (const exp of libDef.exports) {
        let value;
        try {
            value = libEnv.lookup(exp.internal);
        } catch (e) {
            // Check if it's a macro or a syntax keyword
            if (globalMacroRegistry.isMacro(exp.internal)) {
                // Return a marker so applyImports knows it's a macro
                value = { _isMacro: true, name: exp.internal };
            } else if (SYNTAX_KEYWORDS.has(exp.internal)) {
                // Return a marker so applyImports knows it's a syntax keyword
                value = { _isKeyword: true, name: exp.internal };
            } else {
                throw e;
            }
        }
        exports.set(exp.external, value);
    }

    // Register library
    registerLibrary(key, exports, libEnv);

    return exports;
}

/**
 * Evaluates a parsed library definition synchronously.
 * 
 * @param {Object} libDef - The parsed library definition
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Map} The library's exports
 */
export function evaluateLibraryDefinitionSync(libDef, analyze, interpreter, baseEnv) {
    const libraryName = libDef.name;
    const key = libraryNameToKey(libraryName);

    // Create library environment (child of base env)
    const libEnv = new Environment(baseEnv);

    // Process imports first
    for (const importSpec of libDef.imports) {
        const importExports = loadLibrarySync(
            importSpec.libraryName,
            analyze,
            interpreter,
            baseEnv
        );

        // Apply import filters and add to env
        applyImports(libEnv, importExports, importSpec);
    }

    // Resolve includes using file resolver
    const fileResolver = getFileResolver();
    if (fileResolver) {
        // Load standard includes
        for (const includeFile of libDef.includes) {
            const includeSource = fileResolver(
                [...libraryName.slice(0, -1), includeFile]
            );
            if (includeSource instanceof Promise) throw new Error("Async resolver in sync load");

            const includeForms = parse(includeSource);
            for (const form of includeForms) {
                libDef.body.push(form);
            }
        }

        // Load case-insensitive includes
        for (const includeFile of libDef.includesCi) {
            const includeSource = fileResolver(
                [...libraryName.slice(0, -1), includeFile]
            );
            if (includeSource instanceof Promise) throw new Error("Async resolver in sync load");

            // Parse with case-folding enabled
            const includeForms = parse(includeSource, { caseFold: true });
            for (const form of includeForms) {
                libDef.body.push(form);
            }
        }

        // Load library declaration includes
        for (const declFile of libDef.includeLibraryDeclarations) {
            const declSource = fileResolver(
                [...libraryName.slice(0, -1), declFile]
            );
            if (declSource instanceof Promise) throw new Error("Async resolver in sync load");

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
                            const importExports = loadLibrarySync(
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
    }

    // Execute body with a defining scope for referential transparency
    // Bindings defined here will be registered in the scope registry
    // so macros can reference them hygienically
    const libraryScope = freshScope();
    registerLibraryScope(libraryScope, libEnv);
    pushDefiningScope(libraryScope);

    try {
        for (const expr of libDef.body) {
            const ast = analyze(expr);
            interpreter.run(ast, libEnv);
        }
    } finally {
        popDefiningScope();
    }

    // Build exports map
    const exports = new Map();
    for (const exp of libDef.exports) {
        let value;
        try {
            value = libEnv.lookup(exp.internal);
        } catch (e) {
            // Check if it's a macro or a syntax keyword
            if (globalMacroRegistry.isMacro(exp.internal)) {
                // Return a marker so applyImports knows it's a macro
                value = { _isMacro: true, name: exp.internal };
            } else if (SYNTAX_KEYWORDS.has(exp.internal)) {
                // Return a marker so applyImports knows it's a syntax keyword
                value = { _isKeyword: true, name: exp.internal };
            } else {
                throw e;
            }
        }
        exports.set(exp.external, value);
    }

    // Register library
    registerLibrary(key, exports, libEnv);

    return exports;
}

// =============================================================================
// Import Application
// =============================================================================

/**
 * Applies import filters to add bindings to an environment.
 * 
 * @param {Environment} env - Target environment
 * @param {Map} exports - Source library exports
 * @param {Object} importSpec - Import specification
 */
export function applyImports(env, exports, importSpec) {
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

        // If it's a macro or keyword marker, don't define it in the environment.
        // It's already handled by the analyzer/macro registry.
        if (value && (value._isMacro || value._isKeyword)) {
            continue;
        }

        env.define(finalName, value);
    }
}

// =============================================================================
// Primitive Exports
// =============================================================================

/**
 * Creates (scheme primitives) exports from the global environment.
 * 
 * @param {Environment} globalEnv - The global environment with primitives
 * @returns {Map<string, *>} The exports map
 */
export function createPrimitiveExports(globalEnv) {
    const exports = new Map();

    // List of (scheme primitives) exports
    // This MUST match what is actually installed in the global environment by primitives/index.js
    const primitiveExports = [
        // Equivalence
        'eq?', 'eqv?', 'not', 'boolean?', 'boolean=?', 'symbol?',
        // Math - basic arithmetic
        '+', '-', '*', '/',
        // Math - binary comparisons (used by core.scm to build variadic versions)
        '%num=', '%num<', '%num>', '%num<=', '%num>=',
        // Math - integer division
        'modulo', 'quotient', 'remainder',
        // Math - type predicates
        'number?', 'real?', 'rational?', 'integer?', 'exact-integer?',
        'finite?', 'infinite?', 'nan?',
        // Math - functions requiring Math.*
        'abs', 'floor', 'ceiling', 'truncate', 'expt', 'sqrt',
        'sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'log', 'exp',
        // Pairs and lists
        'cons', 'car', 'cdr', 'pair?', 'null?', 'list?',
        'set-car!', 'set-cdr!',
        'list', 'append',
        // Symbols
        'symbol->string', 'string->symbol',
        // Strings
        'string?', 'string-append', 'number->string',
        // Vectors
        'vector?', 'make-vector', 'vector', 'vector-length',
        'vector-ref', 'vector-set!',
        'vector->list', 'list->vector',
        // Records (Low-level primitives for define-record-type)
        'make-record-type', 'record-constructor', 'record-predicate',
        'record-accessor', 'record-modifier',
        // Control
        'apply', 'values', 'call-with-values',
        'eval', 'interaction-environment',
        'dynamic-wind',
        'call-with-current-continuation', 'call/cc',
        'procedure?',
        // Exceptions
        'raise', 'raise-continuable', 'with-exception-handler',
        'error', 'error-object?', 'error-object-message', 'error-object-irritants',
        // I/O
        'display', 'newline',
    ];

    for (const name of primitiveExports) {
        // Use findEnv to check if binding exists without throwing
        if (globalEnv.findEnv(name) !== null) {
            exports.set(name, globalEnv.lookup(name));
        } else {
            // Warn about missing primitives that we expect to be there
            console.warn(`Warning: (scheme primitives) claims export '${name}' but it is not in the global environment.`);
        }
    }

    return exports;
}
