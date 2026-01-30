
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

    let result = run(interpreter, "(js-check-number 10)");
    assert(logger, "Auto-conversion of BigInt -> Number for foreign JS function", result, 20n);

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
    result = run(interpreter, "(js-echo #(1 2 3))");
    // Result coming back from JS should be converted back to BigInts if they are integers
    assert(logger, "Deep conversion in JS call: Vector -> Array -> Vector", result, [1n, 2n, 3n]);
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runInteropConversionTests(interpreter, logger);
    logger.summary();
}
