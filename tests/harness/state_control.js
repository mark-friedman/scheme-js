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

import { resetGlobalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';
import { resetUniqueIdCounter } from '../../src/core/interpreter/analyzer.js';
import { clearLibraryRegistry } from '../../src/core/interpreter/library_registry.js';

/**
 * Clears all global state in the interpreter subsystem.
 * Call this before running an isolated test or suite.
 * Note: resetGensymCounter removed as part of pure marks hygiene refactor.
 */
export function clearGlobalState() {
    // Reset counters
    resetScopeCounter();
    resetUniqueIdCounter();

    // Clear registries and caches
    resetSyntaxCache();
    clearDefiningScopes();
    clearLibraryRegistry();
    resetGlobalMacroRegistry();

    // Clear global scope bindings (for top-level defines)
    globalScopeRegistry.clear();
}
