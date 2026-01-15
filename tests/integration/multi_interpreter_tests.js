/**
 * Multi-Interpreter Isolation Tests
 * 
 * Tests that multiple interpreter instances can run in isolation
 * when given separate InterpreterContext instances.
 */

import { InterpreterContext, globalContext } from '../../src/core/interpreter/context.js';
import { Interpreter } from '../../src/core/interpreter/interpreter.js';
import { run, assert } from '../harness/helpers.js';

/**
 * Creates a minimal interpreter for testing.
 * @param {InterpreterContext} [context] - Optional context
 * @returns {Object} Interpreter with run capability
 */
function createMinimalInterpreter(context = null) {
    const interpreter = new Interpreter(context);
    // For now, this is a placeholder - full isolation requires threading context
    // through all subsystems (analyzer, syntax_rules, etc.)
    return interpreter;
}

export async function runMultiInterpreterTests(interpreter, logger) {
    logger.title('Multi-Interpreter Isolation Tests');

    // Test 1: Context creation
    const ctx1 = new InterpreterContext();
    const ctx2 = new InterpreterContext();

    assert(logger, 'Separate contexts are distinct objects', ctx1 !== ctx2, true);

    // Test 2: Scope counters are independent
    const scope1a = ctx1.freshScope();
    const scope1b = ctx1.freshScope();
    const scope2a = ctx2.freshScope();

    assert(logger, 'Context 1 scope counter increments', scope1b, 1);
    assert(logger, 'Context 2 scope counter starts fresh', scope2a, 0);

    // Test 3: Unique ID counters are independent
    const id1 = ctx1.freshUniqueId();
    const id2a = ctx2.freshUniqueId();
    const id2b = ctx2.freshUniqueId();

    assert(logger, 'Context 1 ID starts at 0', id1, 0);
    assert(logger, 'Context 2 ID counter is independent', id2b, 1);

    // Test 4: Macro registries are independent
    ctx1.macroRegistry.define('my-macro-1', () => 'transformer1');
    ctx2.macroRegistry.define('my-macro-2', () => 'transformer2');

    assert(logger, 'Context 1 has my-macro-1', ctx1.macroRegistry.isMacro('my-macro-1'), true);
    assert(logger, 'Context 1 does not have my-macro-2', ctx1.macroRegistry.isMacro('my-macro-2'), false);
    assert(logger, 'Context 2 has my-macro-2', ctx2.macroRegistry.isMacro('my-macro-2'), true);
    assert(logger, 'Context 2 does not have my-macro-1', ctx2.macroRegistry.isMacro('my-macro-1'), false);

    // Test 5: Library registries are independent
    ctx1.registerLibrary('test.lib1', new Map([['foo', 1]]), null);
    ctx2.registerLibrary('test.lib2', new Map([['bar', 2]]), null);

    assert(logger, 'Context 1 has test.lib1', ctx1.isLibraryLoaded('test.lib1'), true);
    assert(logger, 'Context 1 does not have test.lib2', ctx1.isLibraryLoaded('test.lib2'), false);
    assert(logger, 'Context 2 has test.lib2', ctx2.isLibraryLoaded('test.lib2'), true);

    // Test 6: Feature sets are independent
    ctx1.addFeature('custom-feature-1');
    assert(logger, 'Context 1 has custom feature', ctx1.hasFeature('custom-feature-1'), true);
    assert(logger, 'Context 2 does not have custom feature', ctx2.hasFeature('custom-feature-1'), false);

    // Test 7: Reset clears context state
    ctx1.reset();
    assert(logger, 'After reset, scope counter is 0', ctx1.scopeCounter, 0);
    assert(logger, 'After reset, macro registry cleared', ctx1.macroRegistry.isMacro('my-macro-1'), false);
    assert(logger, 'After reset, library registry cleared', ctx1.isLibraryLoaded('test.lib1'), false);

    // Test 8: Interpreters with separate contexts
    const interp1 = new Interpreter(ctx1);
    const interp2 = new Interpreter(ctx2);

    assert(logger, 'Interpreter 1 has ctx1', interp1.context === ctx1, true);
    assert(logger, 'Interpreter 2 has ctx2', interp2.context === ctx2, true);
    assert(logger, 'Interpreters have different contexts', interp1.context !== interp2.context, true);

    // Test 9: Global context is default
    const interpDefault = new Interpreter();
    assert(logger, 'Default interpreter uses globalContext', interpDefault.context === globalContext, true);
}
