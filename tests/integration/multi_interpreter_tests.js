/**
 * Multi-Interpreter Isolation Tests
 * 
 * Tests that multiple interpreter instances can run in isolation
 * when given separate InterpreterContext instances.
 *
 * The libraries loaded and the features `cond-expand` finds are not a
 * context's: they are the library system's, in its registries
 * (library_registry.js), one for a program and others made for a while by
 * tools (`withPrivateLibraries`).
 */

import { InterpreterContext, globalContext } from '../../src/core/interpreter/context.js';
import { Interpreter } from '../../src/core/interpreter/interpreter.js';
import { run, assert } from '../harness/helpers.js';

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

    assert(logger, 'Context 1 scope counter increments', scope1b, scope1a + 1);
    assert(logger, 'Context 2 scope counter starts fresh', scope2a, scope1a);

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

    // Test 5: Reset clears context state
    ctx1.reset();
    assert(logger, 'After reset, scope counter starts again', ctx1.freshScope(), scope1a);
    assert(logger, 'After reset, macro registry cleared', ctx1.macroRegistry.isMacro('my-macro-1'), false);

    // Test 6: Interpreters with separate contexts
    const interp1 = new Interpreter(ctx1);
    const interp2 = new Interpreter(ctx2);

    assert(logger, 'Interpreter 1 has ctx1', interp1.context === ctx1, true);
    assert(logger, 'Interpreter 2 has ctx2', interp2.context === ctx2, true);
    assert(logger, 'Interpreters have different contexts', interp1.context !== interp2.context, true);

    // Test 7: Global context is default
    const interpDefault = new Interpreter();
    assert(logger, 'Default interpreter uses globalContext', interpDefault.context === globalContext, true);
}
