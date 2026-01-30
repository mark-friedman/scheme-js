
import { Interpreter } from '../../src/core/interpreter/interpreter.js';
import { createGlobalEnvironment } from '../../src/core/primitives/index.js';
import { run } from '../harness/helpers.js';
import assert from 'assert';

async function runReproduction() {
    console.log("Running Reproduction: BigInt TypeError in interop...");
    const interpreter = new Interpreter();
    const env = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(env);

    // Scenario 1: Missing Conversion in Global Function Application (AppFrame.step)
    console.log("\nScenario 1: Calling JS global function with BigInt");
    // Inject isNaN into global env
    env.define('isNaN', isNaN);

    try {
        // (isNaN 10) -> isNaN(10n) in current implementation
        run(interpreter, "(isNaN 10)");
        console.log("Scenario 1 FAILED to reproduce (no error thrown)");
    } catch (e) {
        if (e instanceof TypeError && e.message.includes("convert a BigInt value to a number")) {
            console.log("Scenario 1 REPRODUCED: Caught expected TypeError:", e.message);
        } else {
            console.error("Scenario 1 FAILED (caught unexpected error):", e);
        }
    }

    // Scenario 2: Missing Return Value Conversion in Scheme Closures (createClosure)
    console.log("\nScenario 2: Return value from Scheme closure to JS");
    // Define a Scheme function that returns a number
    const closure = run(interpreter, "(lambda (x) x)");

    // Call it from JS
    const result = closure(10);
    console.log("Result from closure(10):", result, "Type:", typeof result);

    if (typeof result === 'bigint') {
        console.log("Scenario 2 REPRODUCED: Received raw BigInt instead of Number");

        // This is what happens in Blockly/isNaN
        try {
            isNaN(result);
            console.log("But isNaN(result) worked? (Unexpected)");
        } catch (e) {
            if (e instanceof TypeError && e.message.includes("convert a BigInt value to a number")) {
                console.log("Confirmed: isNaN(result) throws TypeError as expected in diagnosis.");
            } else {
                console.error("Scenario 2 Check FAILED (caught unexpected error):", e);
            }
        }
    } else {
        console.log("Scenario 2 FAILED to reproduce (received expected JS type)");
    }
}

runReproduction().catch(console.error);
