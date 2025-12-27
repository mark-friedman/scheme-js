/**
 * Scope Marking Tests
 * 
 * Tests for the scope marking infrastructure used by macro hygiene.
 * These test the core functionality of addScopeToExpression and
 * free-identifier=? semantics.
 */

import { Cons, cons, list } from '../../src/core/interpreter/cons.js';
import { Symbol, intern } from '../../src/core/interpreter/symbol.js';
import {
    SyntaxObject,
    freshScope,
    addScopeToExpression,
    syntaxScopes
} from '../../src/core/interpreter/syntax_object.js';

/**
 * Run all scope marking tests.
 * @param {Function} loader - The library loader function
 * @param {Object} options - Test options
 * @returns {Object} Test results
 */
export async function runScopeMarkingTests(loader, options = {}) {
    const results = { passed: 0, failed: 0, tests: [] };

    function test(name, fn) {
        try {
            fn();
            results.passed++;
            results.tests.push({ name, passed: true });
            if (options.verbose) console.log(`  ✅ ${name}`);
        } catch (e) {
            results.failed++;
            results.tests.push({ name, passed: false, error: e.message });
            console.log(`  ❌ ${name}: ${e.message}`);
        }
    }

    function assertEqual(actual, expected, message = '') {
        if (actual !== expected) {
            throw new Error(`${message} Expected ${expected}, got ${actual}`);
        }
    }

    function assertTrue(condition, message = '') {
        if (!condition) {
            throw new Error(`${message} Expected true, got false`);
        }
    }

    console.log('=== Scope Marking Tests ===');

    // Test addScopeToExpression with Symbol
    test('addScopeToExpression wraps Symbol as SyntaxObject', () => {
        const scope = freshScope();
        const sym = intern('foo');
        const result = addScopeToExpression(sym, scope);

        assertTrue(result instanceof SyntaxObject, 'Result should be SyntaxObject');
        assertEqual(result.name, 'foo', 'Name should be preserved');
        assertTrue(result.scopes.has(scope), 'Should have the added scope');
    });

    // Test addScopeToExpression with existing SyntaxObject
    test('addScopeToExpression adds scope to existing SyntaxObject', () => {
        const scope1 = freshScope();
        const scope2 = freshScope();
        const syntaxObj = new SyntaxObject('bar', new Set([scope1]));
        const result = addScopeToExpression(syntaxObj, scope2);

        assertTrue(result instanceof SyntaxObject, 'Result should be SyntaxObject');
        assertEqual(result.name, 'bar', 'Name should be preserved');
        assertTrue(result.scopes.has(scope1), 'Should have original scope');
        assertTrue(result.scopes.has(scope2), 'Should have new scope');
    });

    // Test addScopeToExpression with Cons list
    test('addScopeToExpression recurses through Cons lists', () => {
        const scope = freshScope();
        const expr = list(intern('a'), intern('b'), intern('c'));
        const result = addScopeToExpression(expr, scope);

        assertTrue(result instanceof Cons, 'Result should be Cons');
        assertTrue(result.car instanceof SyntaxObject, 'car should be SyntaxObject');
        assertEqual(result.car.name, 'a');
        assertTrue(result.car.scopes.has(scope));

        assertTrue(result.cdr.car instanceof SyntaxObject, 'cadr should be SyntaxObject');
        assertEqual(result.cdr.car.name, 'b');
    });

    // Test addScopeToExpression with nested expression
    test('addScopeToExpression handles nested expressions', () => {
        const scope = freshScope();
        // (let ((x 1)) x) represented as nested cons
        const inner = list(intern('x'), 1);
        const bindings = list(inner);
        const expr = list(intern('let'), bindings, intern('x'));

        const result = addScopeToExpression(expr, scope);

        // Check that 'let' has scope
        assertTrue(result.car instanceof SyntaxObject);
        assertEqual(result.car.name, 'let');
        assertTrue(result.car.scopes.has(scope));
    });

    // Test addScopeToExpression preserves primitives
    test('addScopeToExpression preserves primitives', () => {
        const scope = freshScope();

        assertEqual(addScopeToExpression(42, scope), 42);
        assertEqual(addScopeToExpression('hello', scope), 'hello');
        assertEqual(addScopeToExpression(true, scope), true);
        assertEqual(addScopeToExpression(null, scope), null);
    });

    // Test freshScope generates unique IDs
    test('freshScope generates unique scope IDs', () => {
        const scope1 = freshScope();
        const scope2 = freshScope();
        const scope3 = freshScope();

        assertTrue(scope1 !== scope2, 'Scopes should be unique');
        assertTrue(scope2 !== scope3, 'Scopes should be unique');
        assertTrue(scope1 !== scope3, 'Scopes should be unique');
    });

    // Test scope comparison for free-identifier=? semantics
    test('SyntaxObjects with different scopes are distinguishable', () => {
        const scope1 = freshScope();
        const scope2 = freshScope();

        const id1 = new SyntaxObject('x', new Set([scope1]));
        const id2 = new SyntaxObject('x', new Set([scope2]));
        const id3 = new SyntaxObject('x', new Set([scope1, scope2]));

        // Same name but different scopes
        assertEqual(id1.name, id2.name, 'Names should match');
        assertTrue(!id1.boundIdentifierEquals(id2), 'Should not be bound-identifier=?');
        assertTrue(!id1.boundIdentifierEquals(id3), 'Subset scopes should not match');
    });

    // Test that plain symbols (no scopes) are different from scoped identifiers
    test('Plain Symbol can be distinguished from scoped SyntaxObject', () => {
        const scope = freshScope();
        const plainSym = intern('foo');
        const scopedId = new SyntaxObject('foo', new Set([scope]));

        // Plain symbols have no scopes
        const plainScopes = syntaxScopes(plainSym);
        assertTrue(plainScopes.size === 0, 'Plain symbol should have no scopes');

        // Scoped identifier has scopes
        const idScopes = syntaxScopes(scopedId);
        assertTrue(idScopes.size === 1, 'Scoped id should have one scope');
        assertTrue(idScopes.has(scope), 'Should have the right scope');
    });

    console.log(`\nScope Marking Tests: ${results.passed} passed, ${results.failed} failed`);
    return results;
}
