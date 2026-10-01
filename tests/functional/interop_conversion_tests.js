
import { assert, run, createTestEnv, createTestLogger } from '../harness/helpers.js';

/**
 * Functional tests for JS Interop Conversion and BigInt safety.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runInteropConversionTests(interpreter, logger) {
    logger.title("JS Interop Conversion & BigInt Safety");

    // Scenario 1: calling JS global function with auto-conversion
    // Inject a JS function that throws if it gets a BigInt
    const testGlobal = {
        checkNumber: (n) => {
            if (typeof n !== 'number') {
                throw new TypeError(`Expected number, got ${typeof n}`);
            }
            return n * 2;
        }
    };
    interpreter.globalEnv.define('js-check-number', testGlobal.checkNumber);

    let result = run(interpreter, "(js-check-number 10)", { jsAutoConvert: 'raw' });
    // Compared as a type and a value: the harness's `assert` counts 20n and 20
    // as equal, so it cannot tell an exact result from an inexact one.
    assert(logger, "Auto-conversion of BigInt -> Number for foreign JS function, and its integral result back to an exact integer",
        [typeof result, String(result)], ['bigint', '20']);

    // Scenario 2: Return value from Scheme closure to JS
    const closure = run(interpreter, "(lambda (x) x)");
    // Default conversion (deep) should convert result to number
    const closureResult = closure(10);
    assert(logger, "Scheme closure returns Number to JS (default deep)", typeof closureResult, 'number');
    assert(logger, "Scheme closure returns correct value", closureResult, 10);

    // Scenario 3: Preserve BigInt for Scheme primitives (verified using 'raw' mode)
    result = run(interpreter, "(+ 10 20)", { jsAutoConvert: 'raw' });
    assert(logger, "Scheme primitive (+) still receives and returns BigInt", typeof result, 'bigint');
    assert(logger, "Scheme primitive (+) returns correct value", result, 30n);

    // Scenario 4: isNaN handles the converted value
    interpreter.globalEnv.define('isNaN', isNaN);
    result = run(interpreter, "(isNaN 10)");
    assert(logger, "isNaN(10n) works through auto-conversion", result, false);

    // Scenario 5: Nested auto-conversion in deep mode
    const echo = (obj) => obj;
    interpreter.globalEnv.define('js-echo', echo);
    result = run(interpreter, "(js-echo #(1 2 3))", { jsAutoConvert: 'raw' });
    // The arguments are converted throughout on the way out, and the result
    // one level on the way back, as js-invoke converts it: an array comes back
    // as JavaScript's own, holding JavaScript numbers.
    assert(logger, "Deep conversion of the arguments, one level of the result: Vector -> Array -> Vector of JS numbers",
        result.map((x) => typeof x), ['number', 'number', 'number']);
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runInteropConversionTests(interpreter, logger);
    logger.summary();
}
