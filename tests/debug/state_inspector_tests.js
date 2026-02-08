/**
 * @fileoverview Unit tests for StateInspector.
 *
 * Tests scope chain traversal and CDP value serialization.
 */

import { assert, createTestLogger } from '../harness/helpers.js';
import { Environment } from '../../src/core/interpreter/environment.js';
import { StateInspector } from '../../src/debug/state_inspector.js';
import { Cons, cons, list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import { Char } from '../../src/core/primitives/char_class.js';
import { Rational } from '../../src/core/primitives/rational.js';
import { Complex } from '../../src/core/primitives/complex.js';
import { SCHEME_CONTINUATION } from '../../src/core/interpreter/values.js';

/**
 * Runs all StateInspector tests.
 * @param {Object} logger - Test logger
 */
export async function runStateInspectorTests(logger) {
    logger.title('StateInspector - Scope Chain');

    // Test: getScopes returns local scope for simple env
    {
        const globalEnv = new Environment();
        globalEnv.define('x', 10);
        const localEnv = globalEnv.extend('y', 20);

        const inspector = new StateInspector();
        const scopes = inspector.getScopes(localEnv);

        assert(logger, 'getScopes returns array', Array.isArray(scopes), true);
        assert(logger, 'getScopes has at least 2 scopes', scopes.length >= 2, true);
        assert(logger, 'first scope is local', scopes[0].type, 'local');
    }

    // Test: getScopes returns closure chain for nested env
    {
        const globalEnv = new Environment();
        globalEnv.define('global', 1);
        const closure1 = globalEnv.extend('a', 2);
        const closure2 = closure1.extend('b', 3);
        const localEnv = closure2.extend('c', 4);

        const inspector = new StateInspector();
        const scopes = inspector.getScopes(localEnv);

        assert(logger, 'nested scopes count', scopes.length >= 4, true);
        assert(logger, 'first scope type', scopes[0].type, 'local');
        // Intermediate scopes should be 'closure' type
        assert(logger, 'second scope type', scopes[1].type, 'closure');
    }

    // Test: getScopes includes global scope
    {
        const globalEnv = new Environment();
        globalEnv.define('global-var', 100);
        const localEnv = globalEnv.extend('local-var', 200);

        const inspector = new StateInspector();
        const scopes = inspector.getScopes(localEnv);

        const lastScope = scopes[scopes.length - 1];
        assert(logger, 'last scope is global', lastScope.type, 'global');
    }

    // Test: getScopeProperties enumerates local bindings
    {
        const env = new Environment();
        env.define('x', 10);
        env.define('y', 'hello');
        env.define('z', true);

        const inspector = new StateInspector();
        const properties = inspector.getScopeProperties(env);

        assert(logger, 'properties is array', Array.isArray(properties), true);
        assert(logger, 'has 3 properties', properties.length, 3);

        const xProp = properties.find(p => p.name === 'x');
        assert(logger, 'x property exists', xProp !== undefined, true);
        assert(logger, 'x value is 10', xProp?.value?.value, 10);
    }

    logger.title('StateInspector - Value Serialization');

    // Test: Serialize number to CDP format
    {
        const inspector = new StateInspector();

        const intResult = inspector.serializeValue(42);
        assert(logger, 'integer type', intResult.type, 'number');
        assert(logger, 'integer value', intResult.value, 42);

        const floatResult = inspector.serializeValue(3.14);
        assert(logger, 'float type', floatResult.type, 'number');
        assert(logger, 'float value', floatResult.value, 3.14);

        // BigInt
        const bigintResult = inspector.serializeValue(BigInt(999999999999));
        assert(logger, 'bigint type', bigintResult.type, 'bigint');
    }

    // Test: Serialize string to CDP format
    {
        const inspector = new StateInspector();
        const result = inspector.serializeValue('hello world');
        assert(logger, 'string type', result.type, 'string');
        assert(logger, 'string value', result.value, 'hello world');
    }

    // Test: Serialize boolean to CDP format
    {
        const inspector = new StateInspector();

        const trueResult = inspector.serializeValue(true);
        assert(logger, 'true type', trueResult.type, 'boolean');
        assert(logger, 'true value', trueResult.value, true);

        const falseResult = inspector.serializeValue(false);
        assert(logger, 'false type', falseResult.type, 'boolean');
        assert(logger, 'false value', falseResult.value, false);
    }

    // Test: Serialize null/undefined to CDP format
    {
        const inspector = new StateInspector();

        const nullResult = inspector.serializeValue(null);
        assert(logger, 'null type', nullResult.type, 'object');
        assert(logger, 'null subtype', nullResult.subtype, 'null');

        const undefinedResult = inspector.serializeValue(undefined);
        assert(logger, 'undefined type', undefinedResult.type, 'undefined');
    }

    // Test: Serialize pair/list to CDP format
    {
        const inspector = new StateInspector();
        const pair = cons(1, 2);
        const result = inspector.serializeValue(pair);

        assert(logger, 'pair type', result.type, 'object');
        assert(logger, 'pair subtype', result.subtype, 'pair');
        assert(logger, 'pair has description', typeof result.description, 'string');
    }

    // Test: Serialize vector to CDP format
    {
        const inspector = new StateInspector();
        const vector = [1, 2, 3];
        const result = inspector.serializeValue(vector);

        assert(logger, 'vector type', result.type, 'object');
        assert(logger, 'vector subtype', result.subtype, 'vector');
        assert(logger, 'vector has description', result.description.includes('3'), true);
    }

    // Test: Serialize symbol to CDP format
    {
        const inspector = new StateInspector();
        const sym = intern('my-symbol');
        const result = inspector.serializeValue(sym);

        assert(logger, 'symbol type', result.type, 'symbol');
        assert(logger, 'symbol description', result.description, 'my-symbol');
    }

    // Test: Serialize character to CDP format
    {
        const inspector = new StateInspector();
        const char = new Char('A'.charCodeAt(0));
        const result = inspector.serializeValue(char);

        assert(logger, 'char type', result.type, 'object');
        assert(logger, 'char subtype', result.subtype, 'character');
        assert(logger, 'char description', result.description, '#\\A');
    }

    // Test: Serialize procedure (closure) to CDP format
    {
        const inspector = new StateInspector();
        // Create a mock closure-like function
        const closure = function () { };
        closure.__schemeClosure = true;
        closure.__params = ['x', 'y'];

        const result = inspector.serializeValue(closure);

        assert(logger, 'closure type', result.type, 'function');
        assert(logger, 'closure subtype', result.subtype, 'closure');
    }

    // Test: Serialize continuation to CDP format
    {
        const inspector = new StateInspector();
        // Create a mock continuation with proper marker
        const cont = function () { };
        cont[SCHEME_CONTINUATION] = true;

        const result = inspector.serializeValue(cont);

        assert(logger, 'continuation type', result.type, 'function');
        assert(logger, 'continuation subtype', result.subtype, 'continuation');
    }

    // Test: Serialize Rational to CDP format
    {
        const inspector = new StateInspector();
        const rat = new Rational(BigInt(3), BigInt(4));
        const result = inspector.serializeValue(rat);

        assert(logger, 'rational type', result.type, 'object');
        assert(logger, 'rational subtype', result.subtype, 'rational');
        assert(logger, 'rational description contains /', result.description.includes('/'), true);
    }

    // Test: Serialize Complex to CDP format
    {
        const inspector = new StateInspector();
        const complex = new Complex(3, 4);
        const result = inspector.serializeValue(complex);

        assert(logger, 'complex type', result.type, 'object');
        assert(logger, 'complex subtype', result.subtype, 'complex');
        assert(logger, 'complex description contains i', result.description.includes('i'), true);
    }

    // Test: Serialize JS object to CDP format
    {
        const inspector = new StateInspector();
        const obj = { foo: 1, bar: 'hello' };
        const result = inspector.serializeValue(obj);

        assert(logger, 'object type', result.type, 'object');
        assert(logger, 'object className', result.className, 'Object');
    }

    // Test: Serialize record to CDP format
    {
        const inspector = new StateInspector();
        // Create a mock record
        const record = { __schemeRecord: true, __rtd: { name: 'point' }, x: 10, y: 20 };
        const result = inspector.serializeValue(record);

        assert(logger, 'record type', result.type, 'object');
        assert(logger, 'record subtype', result.subtype, 'record');
        assert(logger, 'record className', result.className, 'point');
    }

    logger.title('StateInspector - Edge Cases');

    // Test: Serialize circular structure gracefully
    {
        const inspector = new StateInspector();
        const obj = { name: 'circular' };
        obj.self = obj;

        // Should not throw, should handle gracefully
        let result;
        try {
            result = inspector.serializeValue(obj);
            assert(logger, 'circular handled', true, true);
        } catch (e) {
            assert(logger, 'circular should not throw', false, true);
        }
    }

    // Test: Deep nested structure serialization
    {
        const inspector = new StateInspector();
        let nested = 'deepest';
        for (let i = 0; i < 10; i++) {
            nested = { level: i, inner: nested };
        }

        const result = inspector.serializeValue(nested);
        assert(logger, 'deep nested type', result.type, 'object');
    }

    // Test: Large vector serialization
    {
        const inspector = new StateInspector();
        const largeVector = new Array(1000).fill(0).map((_, i) => i);

        const result = inspector.serializeValue(largeVector);
        assert(logger, 'large vector type', result.type, 'object');
        assert(logger, 'large vector subtype', result.subtype, 'vector');
        assert(logger, 'large vector description mentions size', result.description.includes('1000'), true);
    }
}

export default runStateInspectorTests;
