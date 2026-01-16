import { assert, run, createTestLogger, createTestEnv } from '../harness/helpers.js';
import { SchemeUnboundError } from '../../src/core/interpreter/errors.js';

/**
 * Runs JS Global Interop tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runJsGlobalTests(interpreter, logger) {
    logger.title("JavaScript Global Scope Access");

    // 1. Read access
    globalThis.testVar1 = "JS-Value-1";
    let result = run(interpreter, `testVar1`);
    assert(logger, "Read JS global variable", result, "JS-Value-1");

    // 2. Write access (set!)
    globalThis.testVar2 = "JS-Value-2";
    run(interpreter, `(set! testVar2 "Modified-By-Scheme")`);
    assert(logger, "Write JS global variable", globalThis.testVar2, "Modified-By-Scheme");

    // Verify return value of set! is the new value
    result = run(interpreter, `(set! testVar2 "Second-Mod")`);
    assert(logger, "set! returns new value", result, "Second-Mod");

    // 3. Shadowing
    globalThis.shadowMe = "I am JS";
    run(interpreter, `(define shadowMe "I am Scheme")`);
    result = run(interpreter, `shadowMe`);
    assert(logger, "Scheme define shadows JS global", result, "I am Scheme");
    assert(logger, "JS global remains untouched by shadow", globalThis.shadowMe, "I am JS");

    // 4. Function application (Standard JS function)
    globalThis.jsAdd = (a, b) => a + b;
    result = run(interpreter, `(jsAdd 10 20)`);
    assert(logger, "Call JS global function", result, 30);

    // 5. Accessing built-in objects
    // Note: Math.max is not a global variable named "Math.max".
    // "Math" is the global variable.
    result = run(interpreter, `Math`);
    // Result should be the Math object.
    // We can't easily assert equality on the object without strict reference check,
    // but we can check if it's truthy and (conceptually) an object.
    assert(logger, "Read Math object", !!result, true);
    assert(logger, "Math object is object", typeof result, 'object');

    // 6. Non-existent variable still throws
    try {
        run(interpreter, `nonExistentVar123`);
        logger.fail("Should throw for non-existent variable");
    } catch (e) {
        assert(logger, "Error type for unbound variable", e instanceof SchemeUnboundError || e.name === 'SchemeUnboundError', true);
    }

    // 7. set! on non-existent variable still throws
    try {
        run(interpreter, `(set! nonExistentVar456 1)`);
        logger.fail("Should throw set! for non-existent variable");
    } catch (e) {
        assert(logger, "Error type for set! unbound variable", e instanceof SchemeUnboundError || e.name === 'SchemeUnboundError', true);
    }

    // Cleanup
    delete globalThis.testVar1;
    delete globalThis.testVar2;
    delete globalThis.shadowMe;
    delete globalThis.jsAdd;
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runJsGlobalTests(interpreter, logger);
    logger.summary();
}
