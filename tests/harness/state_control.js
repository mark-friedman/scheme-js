/**
 * Test Harness: State Control
 * 
 * Provides utilities to clear global state between tests.
 * Uses the InterpreterContext to manage all state centrally.
 */

import { globalContext } from '../../src/core/interpreter/context.js';
import { globalScopeRegistry, clearDefiningScopes } from '../../src/core/interpreter/syntax_object.js';

/**
 * Clears all global state in the interpreter subsystem.
 * Call this before running an isolated test or suite.
 * 
 * Uses the centralized InterpreterContext for most state management.
 */
export function clearGlobalState() {
    // Reset the global context (counters, caches, registries)
    globalContext.reset();

    // Clear scope binding registry (not yet managed by context)
    clearDefiningScopes();
    globalScopeRegistry.clear();
}
