import { assert, createTestLogger } from '../../harness/helpers.js';
import { clearGlobalState } from '../../harness/state_control.js';
import { freshScope, internSyntax, resetScopeCounter, resetSyntaxCache } from '../../../src/core/interpreter/syntax_object.js';
import { globalMacroRegistry } from '../../../src/core/interpreter/macro_registry.js';
import { resetUniqueIdCounter } from '../../../src/core/interpreter/analyzer.js';

/**
 * State Isolation Tests
 * 
 * Verifies that clearGlobalState() correctly resets all global state
 * (counters, registries, caches) to ensure test isolation.
 */
export function runStateIsolationTests(logger) {
    logger.title('State Isolation Tests');

    try {
        // Test 1: Scope Counter Reset
        clearGlobalState(); // Start clean
        const s1 = freshScope();
        const s2 = freshScope();
        assert(logger, 'Scopes increment', s2 > s1, true);

        clearGlobalState(); // Reset
        const s3 = freshScope();
        assert(logger, 'Scope counter reset', s3, 1); // Should restart at 1 (since 0 is global)

        // Test 2: Syntax Cache Clearing
        clearGlobalState();
        const syn1 = internSyntax('foo', [1]);
        const syn2 = internSyntax('foo', [1]);
        assert(logger, 'Syntax objects interned', syn1 === syn2, true);

        clearGlobalState(); // Reset
        const syn3 = internSyntax('foo', [1]);
        // After clearing, we should get a new object (though structurally equal)
        // Wait, if cache is cleared, new object is created.
        // It won't be === to syn1 unless V8 does something magic, which it won't.
        assert(logger, 'Syntax cache cleared (identity)', syn1 === syn3, false);

        // Test 3: Macro Registry Clearing
        clearGlobalState();
        globalMacroRegistry.define('my-macro', () => { });
        assert(logger, 'Macro defined', globalMacroRegistry.isMacro('my-macro'), true);

        clearGlobalState(); // Reset
        assert(logger, 'Macro registry cleared', globalMacroRegistry.isMacro('my-macro'), false);

        // Test 4: Defining Scopes Stack (Implicit)
        // verify clearDefiningScopes is called (no direct accessor to verify empty, 
        // but it shouldn't throw error if we call clearGlobalState)
        clearGlobalState();
        logger.pass('clearGlobalState ran without error');

    } catch (e) {
        logger.fail(`State isolation test failed: ${e.message}`);
        console.error(e);
    }
}
