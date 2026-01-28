/**
 * Test Harness: State Control
 * 
 * Provides utilities to clear global state between tests.
 * Uses the InterpreterContext to manage all state centrally.
 */

import { globalContext } from '../../src/core/interpreter/context.js';

/**
 * Clears all global state in the interpreter subsystem.
 * Call this before running an isolated test or suite.
 * 
 * All mutable state is now managed through the globalContext,
 * so a single reset() call clears everything:
 * - Scope counters
 * - Syntax intern cache
 * - Library scope environment map
 * - Scope registry
 * - Macro registry
 * - Library registry
 * - Defining scopes stack
 */
export function clearGlobalState() {
    globalContext.reset();
    // Also clear the scope registry which is exported separately
    globalContext.scopeRegistry.clear();
}

