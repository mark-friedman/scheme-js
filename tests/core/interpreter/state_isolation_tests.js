import { assert, createTestLogger } from '../../harness/helpers.js';
import { clearGlobalState } from '../../harness/state_control.js';
import { globalContext } from '../../../src/core/interpreter/context.js';
import { internSyntax } from '../../../src/core/interpreter/syntax_object.js';

/**
 * State Isolation Tests
 * 
 * Verifies that clearGlobalState() correctly resets all global state
 * (counters, registries, caches) to ensure test isolation.
 * 
 * Uses the InterpreterContext API which is the preferred way to manage state.
 */
export function runStateIsolationTests(logger) {
    logger.title('State Isolation Tests');

    try {
        // Test 1: Scope Counter Reset (using context)
        clearGlobalState(); // Start clean
        const s1 = globalContext.freshScope();
        const s2 = globalContext.freshScope();
        assert(logger, 'Scopes increment', s2 > s1, true);

        clearGlobalState(); // Reset
        const s3 = globalContext.freshScope();
        assert(logger, 'Scope counter reset', s3, 0); // Context counter starts at 0

        // Test 2: Syntax Cache Clearing (using context)
        clearGlobalState();
        const scopes1 = [globalContext.freshScope()];
        const key1 = globalContext.getSyntaxKey('foo', new Set(scopes1));
        // Manually add to cache to test clearing
        globalContext.syntaxInternCache.set(key1, { name: 'foo', scopes: scopes1 });
        assert(logger, 'Syntax cache has entry', globalContext.syntaxInternCache.has(key1), true);

        clearGlobalState(); // Reset
        assert(logger, 'Syntax cache cleared', globalContext.syntaxInternCache.size, 0);

        // Test 3: Macro Registry Clearing (using context)
        clearGlobalState();
        globalContext.macroRegistry.define('my-macro', () => { });
        assert(logger, 'Macro defined', globalContext.macroRegistry.isMacro('my-macro'), true);

        clearGlobalState(); // Reset
        assert(logger, 'Macro registry cleared', globalContext.macroRegistry.isMacro('my-macro'), false);

        // Test 4: Library Registry Clearing
        clearGlobalState();
        globalContext.registerLibrary('test.lib', new Map([['foo', 1]]), null);
        assert(logger, 'Library registered', globalContext.isLibraryLoaded('test.lib'), true);

        clearGlobalState(); // Reset
        assert(logger, 'Library registry cleared', globalContext.isLibraryLoaded('test.lib'), false);

        // Test 5: Unique ID Counter Reset
        clearGlobalState();
        const id1 = globalContext.freshUniqueId();
        const id2 = globalContext.freshUniqueId();
        assert(logger, 'IDs increment', id2 > id1, true);

        clearGlobalState();
        const id3 = globalContext.freshUniqueId();
        assert(logger, 'Unique ID counter reset', id3, 0);

        // Test 6: clearGlobalState runs without error
        clearGlobalState();
        logger.pass('clearGlobalState ran without error');

    } catch (e) {
        logger.fail(`State isolation test failed: ${e.message}`);
        console.error(e);
    }
}
