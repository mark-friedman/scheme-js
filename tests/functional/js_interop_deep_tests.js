
// tests/functional/js_interop_deep_tests.js
import { Interpreter } from '../../src/core/interpreter/interpreter.js';
import { createGlobalEnvironment } from '../../src/core/primitives/index.js';
import { schemeToJsDeep, jsToSchemeDeep } from '../../src/core/interpreter/js_interop.js';
import { Cons } from '../../src/core/interpreter/cons.js';
import assert from 'assert';

/**
 * Functional tests for Deep JS Interop.
 */

async function runTests() {
    console.log("Running Deep JS Interop Tests...");
    let passed = 0;
    let failed = 0;

    const interpreter = new Interpreter();
    interpreter.setGlobalEnv(createGlobalEnvironment(interpreter));

    // --------------------------------------------------------------------------
    // 1. JS -> Scheme Deep Conversion
    // --------------------------------------------------------------------------
    try {
        console.log("Test: JS Array -> Scheme Vector");
        const jsArr = [1, 2, [3, 4]];
        const schemeVec = jsToSchemeDeep(jsArr);
        assert.ok(Array.isArray(schemeVec), "Should return Vector (Array)");
        assert.strictEqual(schemeVec.length, 3);
        assert.strictEqual(schemeVec[0], 1);
        assert.ok(Array.isArray(schemeVec[2]), "Nested array should be Vector (Array)");
        passed++;
    } catch (e) { console.error("Failed:", e); failed++; }

    try {
        console.log("Test: JS Object -> Scheme js-object");
        // Use the interpreter to ensure record type is registered if needed
        // Actually jsToSchemeDeep uses logic in js_interop.js which relies on registry.
        // We must define and register `js-object` first via Scheme!

        const MockRecordCtor = class MockRecord { constructor() { } static get schemeName() { return 'js-object'; } };
        const { registerJsObjectRecord } = await import('../../src/core/interpreter/js_interop.js');
        console.log("Registering MockRecordCtor for js-object...");
        registerJsObjectRecord(MockRecordCtor);

        const jsObj = { x: 10, y: [20] };
        const schemeObj = jsToSchemeDeep(jsObj);

        assert.ok(schemeObj instanceof MockRecordCtor, "Should be instance of registered record");
        assert.strictEqual(schemeObj.x, 10, "Should have property x");
        assert.ok(Array.isArray(schemeObj.y), "Property y should be deeply converted");
        passed++;
    } catch (e) { console.error("Failed:", e); failed++; }

    // --------------------------------------------------------------------------
    // 2. Scheme -> JS Deep Conversion
    // --------------------------------------------------------------------------
    try {
        console.log("Test: Scheme Vector -> JS Array");
        const vec = [1, 2, [3]]; // Vector is Array
        const arr = schemeToJsDeep(vec);
        assert.ok(Array.isArray(arr), "Should return array");
        assert.strictEqual(arr.length, 3);
        assert.ok(Array.isArray(arr[2]), "Nested vector should be array");
        passed++;
    } catch (e) { console.error("Failed:", e); failed++; }

    try {
        console.log("Test: Scheme List -> JS Array");
        const list = new Cons(1, new Cons(2, null));
        const arr = schemeToJsDeep(list);
        assert.ok(Array.isArray(arr));
        assert.strictEqual(arr[0], 1);
        passed++;
    } catch (e) { console.error("Failed:", e); failed++; }

    try {
        console.log("Test: BigInt Safety");
        const safeBig = 9007199254740991n;
        const unsafeBig = 900719925474099200n;

        assert.strictEqual(schemeToJsDeep(safeBig), 9007199254740991);

        try {
            schemeToJsDeep(unsafeBig);
            failed++;
            console.error("Should have thrown on unsafe BigInt");
        } catch (e) {
            assert.ok(e.message.includes("outside safe"), "Error message match");
            passed++;
        }
    } catch (e) { console.error("Failed:", e); failed++; }

    console.log(`\nPassed: ${passed}, Failed: ${failed}`);
    if (failed > 0) process.exit(1);
}

runTests();
