import {
    SyntaxObject,
    internSyntax,
    resetSyntaxCache,
    resetScopeCounter,
    freshScope,
    flipScopeInExpression,
    unwrapSyntax
} from '../../../src/core/interpreter/syntax_object.js';
import { Symbol, intern } from '../../../src/core/interpreter/symbol.js';
import { list } from '../../../src/core/interpreter/cons.js';
import { assert } from '../../harness/helpers.js';

export function runSyntaxObjectTests(logger) {
    logger.title('Running SyntaxObject Unit Tests...');

    resetSyntaxCache();
    resetScopeCounter();

    // Interning
    try {
        const s1 = internSyntax('x', new Set([1, 2]));
        const s2 = internSyntax('x', new Set([1, 2]));
        const s3 = internSyntax('x', new Set([2, 1]));

        assert(logger, "Interning: same scopes -> same object", s1 === s2, true);
        assert(logger, "Interning: order agnostic", s1 === s3, true);
        assert(logger, "Interning: name check", s1.name, 'x');

        const s4 = internSyntax('x', new Set([1]));
        assert(logger, "Interning: diff scopes -> diff object", s1 !== s4, true);

    } catch (e) {
        logger.fail(`Interning tests failed: ${e.message}`);
    }

    // Methods
    try {
        const s1 = internSyntax('y', new Set([1]));
        const s2 = s1.addScope(2);
        assert(logger, "addScope returns interned", s2 === internSyntax('y', new Set([1, 2])), true);

        const s3 = s2.removeScope(2);
        assert(logger, "removeScope returns interned", s3 === internSyntax('y', new Set([1])), true);

        const s4 = s3.flipScope(2);
        assert(logger, "flipScope adds if missing", s4 === internSyntax('y', new Set([1, 2])), true);

        const s5 = s4.flipScope(2);
        assert(logger, "flipScope remove if present", s5 === internSyntax('y', new Set([1])), true);

    } catch (e) {
        logger.fail(`Method tests failed: ${e.message}`);
    }

    // flipScopeInExpression
    try {
        const sym = new Symbol('z');
        const scope = freshScope();

        const marked = flipScopeInExpression(sym, scope);
        assert(logger, "flipScopeInExpression symbol -> SyntaxObject", marked instanceof SyntaxObject, true);
        assert(logger, "flipScopeInExpression symbol scope", marked.scopes.has(scope), true);

        const flipped = flipScopeInExpression(marked, scope);
        // When all scopes cancel, flipScope returns a plain Symbol
        assert(logger, "flipScopeInExpression flip back to Symbol", flipped instanceof Symbol, true);
        assert(logger, "flipScopeInExpression returns interned symbol", flipped === intern('z'), true);

    } catch (e) {
        logger.fail(`flipScopeInExpression tests failed: ${e.message}`);
    }

    // unwrapSyntax tests
    try {

        // Test 1: Raw string returns as-is (not converted to Symbol)
        const rawString = "hello";
        const unwrappedString = unwrapSyntax(rawString);
        assert(logger, "unwrapSyntax raw string stays string", typeof unwrappedString, "string");
        assert(logger, "unwrapSyntax raw string value", unwrappedString, "hello");

        // Test 2: SyntaxObject with string name becomes Symbol
        const syntaxId = new SyntaxObject("foo", new Set(), null);
        const unwrappedId = unwrapSyntax(syntaxId);
        assert(logger, "unwrapSyntax SyntaxObject -> Symbol", unwrappedId instanceof Symbol, true);
        assert(logger, "unwrapSyntax SyntaxObject name", unwrappedId.name, "foo");

        // Test 3: Raw Symbol stays Symbol
        const rawSymbol = intern("bar");
        const unwrappedSymbol = unwrapSyntax(rawSymbol);
        assert(logger, "unwrapSyntax raw Symbol stays Symbol", unwrappedSymbol === rawSymbol, true);

        // Test 4: Number stays number
        const numResult = unwrapSyntax(42);
        assert(logger, "unwrapSyntax number stays number", numResult, 42);

        // Test 5: List of raw strings stays as list of strings
        const stringList = list("a", "b", "c");
        const unwrappedList = unwrapSyntax(stringList);
        assert(logger, "unwrapSyntax list car stays string", typeof unwrappedList.car, "string");
        assert(logger, "unwrapSyntax list car value", unwrappedList.car, "a");

        // Test 6: Vector of SyntaxObjects becomes vector of Symbols
        const syntaxVector = [
            new SyntaxObject("x", new Set(), null),
            new SyntaxObject("y", new Set(), null)
        ];
        const unwrappedVector = unwrapSyntax(syntaxVector);
        assert(logger, "unwrapSyntax vector[0] is Symbol", unwrappedVector[0] instanceof Symbol, true);
        assert(logger, "unwrapSyntax vector[0] name", unwrappedVector[0].name, "x");
        assert(logger, "unwrapSyntax vector[1] is Symbol", unwrappedVector[1] instanceof Symbol, true);

        // Test 7: Mixed vector - strings stay strings, SyntaxObjects become Symbols
        const mixedVector = ["hello", new SyntaxObject("world", new Set(), null)];
        const unwrappedMixed = unwrapSyntax(mixedVector);
        assert(logger, "unwrapSyntax mixed[0] stays string", typeof unwrappedMixed[0], "string");
        assert(logger, "unwrapSyntax mixed[1] is Symbol", unwrappedMixed[1] instanceof Symbol, true);

    } catch (e) {
        logger.fail(`unwrapSyntax tests failed: ${e.message}`);
    }
}
