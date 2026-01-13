/**
 * Test Harness: State Control
 * 
 * Provides utilities to clear global state between tests.
 * This is crucial because the Scheme implementation relies on some global Singletons
 * (e.g., Syntax Object interning, Macro Registry) which persist across Interpreter instances.
 */

import {
    resetScopeCounter,
    resetSyntaxCache,
    clearDefiningScopes,
    globalScopeRegistry
} from '../../src/core/interpreter/syntax_object.js';

import { resetGensymCounter } from '../../src/core/interpreter/syntax_rules.js';
import { resetGlobalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';
import { resetUniqueIdCounter } from '../../src/core/interpreter/analyzer.js';
import { clearLibraryRegistry } from '../../src/core/interpreter/library_registry.js';

/**
 * Clears all global state in the interpreter subsystem.
 * Call this before running an isolated test or suite.
 */
export function clearGlobalState() {
    // Reset counters
    resetScopeCounter();
    resetGensymCounter();
    resetUniqueIdCounter();

    // Clear registries and caches
    resetSyntaxCache();
    clearDefiningScopes();
    clearLibraryRegistry();
    resetGlobalMacroRegistry();

    // Clear global scope bindings (for top-level defines)
    globalScopeRegistry.clear();
}
