
import { assert, run, createTestEnv, createTestLogger } from '../harness/helpers.js';
import { Flonum } from '../../src/core/interpreter/number_representation.js';

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
    // An exact integer is a JavaScript number, and an integral result from
    // JavaScript is one (src/core/interpreter/number_representation.js).
    assert(logger, "A foreign JS function is given a number, and its integral result is an exact integer",
        [typeof result, String(result)], ['number', '20']);

    // Scenario 2: Return value from Scheme closure to JS
    const closure = run(interpreter, "(lambda (x) x)");
    // Default conversion (deep) should convert result to number
    const closureResult = closure(10);
    assert(logger, "Scheme closure returns Number to JS (default deep)", typeof closureResult, 'number');
    assert(logger, "Scheme closure returns correct value", closureResult, 10);

    // Scenario 3: Preserve BigInt for Scheme primitives (verified using 'raw' mode)
    result = run(interpreter, "(+ 10 20)", { jsAutoConvert: 'raw' });
    assert(logger, "Scheme primitive (+) returns an exact integer, a number", typeof result, 'number');
    assert(logger, "Scheme primitive (+) returns correct value", result, 30);
    result = run(interpreter, "(+ 10. 20)", { jsAutoConvert: 'raw' });
    assert(logger, "and an inexact integer boxed", result instanceof Flonum && result.value, 30);

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
