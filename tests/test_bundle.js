import { schemeEval, schemeEvalAsync } from '../dist/scheme.js';
import assert from 'assert';

console.log("Testing dist/scheme.js...");

// Test 1: Basic Math (Sync)
console.log("Test 1: Basic Math (Sync)");
const result1 = schemeEval('(+ 1 2)');
assert.strictEqual(result1, 3);
console.log("PASS: (+ 1 2) = 3");

// Test 2: Basic Math (Async)
console.log("Test 2: Basic Math (Async)");
const result2 = await schemeEvalAsync('(* 10 20)');
assert.strictEqual(result2, 200);
console.log("PASS: (* 10 20) = 200");

// Test 3: Shared Environment
console.log("Test 3: Shared Environment");
schemeEval('(define x 42)');
const result3 = schemeEval('x');
assert.strictEqual(result3, 42);
console.log("PASS: Defined x = 42, retrieved x = 42");

// Test 4: Shared Environment (Async)
console.log("Test 4: Shared Environment (Async)");
schemeEval('(define y 100)');
const result4 = await schemeEvalAsync('(+ x y)'); // 42 + 100
assert.strictEqual(result4, 142);
console.log("PASS: (+ x y) = 142");

console.log("All Bundle Tests Passed!");
