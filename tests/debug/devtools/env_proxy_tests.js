/**
 * @fileoverview Unit tests for createEnvProxy and formatForDevTools.
 *
 * Tests the environment proxy that bridges Scheme environments
 * to Chrome DevTools' Scope pane via the `with()` pattern, and
 * the value formatting that converts Scheme values to DevTools-friendly
 * representations using Scheme print format.
 *
 * Phase 2: Basic proxy, scope chain, name mapping.
 * Phase 3: Value formatting, name filtering, custom formatters.
 */

import { assert } from '../../harness/helpers.js';
import {
  createEnvProxy,
  formatForDevTools,
  SchemeDisplayValue
} from '../../../src/debug/devtools/env_proxy.js';
import { Environment } from '../../../src/core/interpreter/environment.js';
import { Cons, cons, list } from '../../../src/core/interpreter/cons.js';
import { Symbol as SchemeSymbol, intern } from '../../../src/core/interpreter/symbol.js';
import { Char } from '../../../src/core/primitives/char_class.js';
import { Rational } from '../../../src/core/primitives/rational.js';
import { Complex } from '../../../src/core/primitives/complex.js';
import { createClosure, createContinuation, isSchemeClosure } from '../../../src/core/interpreter/values.js';
import { recordPrimitives } from '../../../src/core/primitives/record.js';

/**
 * Runs all environment proxy tests.
 * @param {Object} logger - Test logger
 */
export async function runEnvProxyTests(logger) {

  // =========================================================================
  // Basic Proxy Behavior
  // =========================================================================
  logger.title('EnvProxy - Basic Behavior');

  // Test: proxy exposes direct bindings via has()
  {
    const env = new Environment();
    env.define('x', 42);
    const proxy = createEnvProxy(env);

    assert(logger, 'has returns true for bound variable', 'x' in proxy, true);
    assert(logger, 'has returns false for unbound variable', 'y' in proxy, false);
  }

  // Test: proxy returns values via get()
  {
    const env = new Environment();
    env.define('x', 42);
    env.define('name', 'hello');
    const proxy = createEnvProxy(env);

    assert(logger, 'get returns number value', proxy.x, 42);
    assert(logger, 'get returns string value', proxy.name, 'hello');
  }

  // Test: proxy returns undefined for unbound variables
  {
    const env = new Environment();
    const proxy = createEnvProxy(env);

    assert(logger, 'get returns undefined for unbound', proxy.y, undefined);
  }

  // =========================================================================
  // Scope Chain
  // =========================================================================
  logger.title('EnvProxy - Scope Chain');

  // Test: proxy sees parent environment bindings
  {
    const parent = new Environment();
    parent.define('outer', 100);
    const child = parent.extend('inner', 200);
    const proxy = createEnvProxy(child);

    assert(logger, 'has finds inner binding', 'inner' in proxy, true);
    assert(logger, 'has finds outer binding', 'outer' in proxy, true);
    assert(logger, 'get returns inner value', proxy.inner, 200);
    assert(logger, 'get returns outer value', proxy.outer, 100);
  }

  // Test: inner binding shadows outer
  {
    const parent = new Environment();
    parent.define('x', 1);
    const child = parent.extend('x', 2);
    const proxy = createEnvProxy(child);

    assert(logger, 'inner shadows outer', proxy.x, 2);
  }

  // =========================================================================
  // Alpha-Renamed Variables (nameMap)
  // =========================================================================
  logger.title('EnvProxy - Name Mapping');

  // Test: proxy resolves original names via nameMap
  {
    const parent = new Environment();
    // Simulate alpha-renaming: user wrote "n", analyzer renamed to "n_42"
    const child = parent.extend('n_42', 5, 'n');
    const proxy = createEnvProxy(child);

    assert(logger, 'has finds original name', 'n' in proxy, true);
    assert(logger, 'get via original name returns value', proxy.n, 5);
  }

  // Test: extendMany with originalNames
  {
    const parent = new Environment();
    const child = parent.extendMany(
      ['a_1', 'b_2'],
      [10, 20],
      ['a', 'b']
    );
    const proxy = createEnvProxy(child);

    assert(logger, 'has finds original name a', 'a' in proxy, true);
    assert(logger, 'has finds original name b', 'b' in proxy, true);
    assert(logger, 'get via a returns value', proxy.a, 10);
    assert(logger, 'get via b returns value', proxy.b, 20);
  }

  // =========================================================================
  // ownKeys (for Scope pane enumeration)
  // =========================================================================
  logger.title('EnvProxy - ownKeys');

  // Test: ownKeys returns all visible bindings
  {
    const parent = new Environment();
    parent.define('global', 0);
    const child = parent.extend('local_42', 1, 'local');
    const proxy = createEnvProxy(child);

    const keys = Object.keys(proxy);
    assert(logger, 'ownKeys includes original name', keys.includes('local'), true);
    assert(logger, 'ownKeys includes direct binding', keys.includes('global'), true);
  }

  // =========================================================================
  // getOwnPropertyDescriptor
  // =========================================================================
  logger.title('EnvProxy - PropertyDescriptor');

  // Test: getOwnPropertyDescriptor for bound variable
  {
    const env = new Environment();
    env.define('x', 42);
    const proxy = createEnvProxy(env);

    const desc = Object.getOwnPropertyDescriptor(proxy, 'x');
    assert(logger, 'descriptor exists for bound var', desc !== undefined, true);
    assert(logger, 'descriptor is enumerable', desc.enumerable, true);
    assert(logger, 'descriptor is configurable', desc.configurable, true);
  }

  // Test: getOwnPropertyDescriptor for unbound variable
  {
    const env = new Environment();
    const proxy = createEnvProxy(env);

    const desc = Object.getOwnPropertyDescriptor(proxy, 'unknown');
    assert(logger, 'descriptor is undefined for unbound var', desc, undefined);
  }

  // =========================================================================
  // Symbol.unscopables
  // =========================================================================
  logger.title('EnvProxy - Symbol.unscopables');

  // Test: Symbol.unscopables is handled correctly
  {
    const env = new Environment();
    env.define('x', 42);
    const proxy = createEnvProxy(env);

    // has should return false for Symbol.unscopables
    assert(logger, 'has returns false for Symbol.unscopables',
      Reflect.has(proxy, Symbol.unscopables), false);

    // get should return undefined for Symbol.unscopables
    assert(logger, 'get returns undefined for Symbol.unscopables',
      proxy[Symbol.unscopables], undefined);
  }

  // =========================================================================
  // Probe function integration
  // =========================================================================
  logger.title('EnvProxy - Probe Integration');

  // Test: proxy works with `with()` statement (the actual use case)
  // Note: ES modules use strict mode where `with` is forbidden,
  // so we use indirect eval to run non-strict code, same as probe scripts.
  {
    const env = new Environment();
    env.define('x', 42);
    const proxy = createEnvProxy(env);

    // Simulate what a probe function does using indirect eval (non-strict)
    // This mirrors how probe scripts are evaluated in Node.js
    const testFn = (0, eval)('(function(envProxy) { with (envProxy) { return x; } })');
    const result = testFn(proxy);

    assert(logger, 'with() resolves bound variable', result, 42);
  }

  // =========================================================================
  // Phase 3: formatForDevTools — JS Primitive Pass-Through
  // =========================================================================
  logger.title('EnvProxy - formatForDevTools Primitive Pass-Through');

  // Test: JS numbers pass through unchanged
  {
    const result = formatForDevTools(42);
    assert(logger, 'JS number passes through', result, 42);
    assert(logger, 'JS number type is number', typeof result, 'number');
  }

  // Test: JS strings pass through unchanged
  {
    const result = formatForDevTools('hello');
    assert(logger, 'JS string passes through', result, 'hello');
    assert(logger, 'JS string type is string', typeof result, 'string');
  }

  // Test: JS booleans pass through unchanged
  {
    assert(logger, 'true passes through', formatForDevTools(true), true);
    assert(logger, 'false passes through', formatForDevTools(false), false);
  }

  // Test: BigInt converts to JS Number (for display without "n" suffix)
  {
    const result = formatForDevTools(42n);
    assert(logger, 'BigInt converts to Number', result, 42);
    assert(logger, 'BigInt result is number type', typeof result, 'number');
  }

  // Test: Large BigInt wraps in SchemeDisplayValue (can't safely convert)
  {
    const big = 9007199254740993n; // > Number.MAX_SAFE_INTEGER
    const result = formatForDevTools(big);
    assert(logger, 'large BigInt becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'large BigInt display string',
      result.toString(), '9007199254740993');
  }

  // =========================================================================
  // Phase 3: formatForDevTools — Scheme Types
  // =========================================================================
  logger.title('EnvProxy - formatForDevTools Scheme Types');

  // Test: null (empty list)
  {
    const result = formatForDevTools(null);
    assert(logger, 'null becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'null display is empty list',
      result.toString(), "'()");
  }

  // Test: Scheme Symbol
  {
    const sym = intern('foo');
    const result = formatForDevTools(sym);
    assert(logger, 'symbol becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'symbol display string', result.toString(), 'foo');
  }

  // Test: Scheme Char
  {
    const ch = new Char(97); // 'a'
    const result = formatForDevTools(ch);
    assert(logger, 'char becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'char display string', result.toString(), '#\\a');
  }

  // Test: Scheme Char — named character
  {
    const space = new Char(32);
    const result = formatForDevTools(space);
    assert(logger, 'space char display', result.toString(), '#\\space');
  }

  // Test: Proper list
  {
    const lst = list(1n, 2n, 3n);
    const result = formatForDevTools(lst);
    assert(logger, 'proper list becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'proper list display', result.toString(), '(1 2 3)');
  }

  // Test: Improper pair (dotted pair)
  {
    const pair = cons(1n, 2n);
    const result = formatForDevTools(pair);
    assert(logger, 'improper pair becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'dotted pair display', result.toString(), '(1 . 2)');
  }

  // Test: Vector (JS Array in Scheme)
  {
    const vec = [1n, 2n, 3n];
    const result = formatForDevTools(vec);
    assert(logger, 'vector becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'vector display', result.toString(), '#(1 2 3)');
  }

  // Test: Rational number
  {
    const rat = new Rational(1n, 3n);
    const result = formatForDevTools(rat);
    assert(logger, 'rational becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'rational display', result.toString(), '1/3');
  }

  // Test: Exact complex number (BigInt parts)
  {
    const cpx = new Complex(1n, 2n);
    const result = formatForDevTools(cpx);
    assert(logger, 'exact complex becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'exact complex display', result.toString(), '1+2i');
  }

  // Test: Inexact complex number (float parts)
  {
    const cpx = new Complex(1, 2);
    const result = formatForDevTools(cpx);
    assert(logger, 'inexact complex becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'inexact complex display', result.toString(), '1.0+2.0i');
  }

  // Test: Scheme closure
  {
    // Create a minimal closure — we don't need a real interpreter for display
    const closure = createClosure(
      ['x'], null, new Environment(), null, null, 'factorial'
    );
    const result = formatForDevTools(closure);
    assert(logger, 'closure becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'closure display includes name',
      result.toString().includes('factorial'), true);
    assert(logger, 'closure display has procedure tag',
      result.toString().includes('#<procedure'), true);
  }

  // Test: Scheme record
  {
    const makeRecordType = recordPrimitives['make-record-type'];
    const recordCtor = recordPrimitives['record-constructor'];

    const PointType = makeRecordType('point', list(intern('x'), intern('y')));
    const makePoint = recordCtor(PointType);
    const pt = makePoint(1n, 2n);

    const result = formatForDevTools(pt);
    assert(logger, 'record becomes SchemeDisplayValue',
      result instanceof SchemeDisplayValue, true);
    assert(logger, 'record display includes type name',
      result.toString().includes('point'), true);
    assert(logger, 'record display includes field values',
      result.toString().includes('1') && result.toString().includes('2'), true);
  }

  // =========================================================================
  // Phase 3: Name Filtering — Only Original Source Names
  // =========================================================================
  logger.title('EnvProxy - Name Filtering');

  // Test: ownKeys filters out internal alpha-renamed names
  {
    const parent = new Environment();
    parent.define('global-var', 99);
    // Simulate alpha-renaming: user wrote "x" and "y", analyzer renamed to "x_1" and "y_2"
    const child = parent.extendMany(
      ['x_1', 'y_2'],
      [10, 20],
      ['x', 'y']
    );
    const proxy = createEnvProxy(child);

    const keys = Object.keys(proxy);
    // Should have original names, not internal names
    assert(logger, 'ownKeys includes original name x', keys.includes('x'), true);
    assert(logger, 'ownKeys includes original name y', keys.includes('y'), true);
    // Internal renamed names should NOT appear
    assert(logger, 'ownKeys excludes internal name x_1', !keys.includes('x_1'), true);
    assert(logger, 'ownKeys excludes internal name y_2', !keys.includes('y_2'), true);
    // Non-renamed bindings should still appear
    assert(logger, 'ownKeys includes unrenamed global', keys.includes('global-var'), true);
  }

  // Test: multi-level scope chain filtering
  {
    const grandparent = new Environment();
    grandparent.define('top', 0);
    const parent = grandparent.extend('a_1', 10, 'a');
    const child = parent.extend('b_2', 20, 'b');
    const proxy = createEnvProxy(child);

    const keys = Object.keys(proxy);
    assert(logger, 'multi-level: includes original a', keys.includes('a'), true);
    assert(logger, 'multi-level: includes original b', keys.includes('b'), true);
    assert(logger, 'multi-level: includes unrenamed top', keys.includes('top'), true);
    assert(logger, 'multi-level: excludes a_1', !keys.includes('a_1'), true);
    assert(logger, 'multi-level: excludes b_2', !keys.includes('b_2'), true);
  }

  // =========================================================================
  // Phase 3: SchemeDisplayValue
  // =========================================================================
  logger.title('EnvProxy - SchemeDisplayValue');

  // Test: SchemeDisplayValue wraps display string
  {
    const val = new SchemeDisplayValue("'(1 2 3)", 'list');
    assert(logger, 'toString returns display string', val.toString(), "'(1 2 3)");
    assert(logger, 'schemeType is set', val.schemeType, 'list');
  }

  // Test: SchemeDisplayValue has marker for custom formatter detection
  {
    const val = new SchemeDisplayValue('#t', 'boolean');
    assert(logger, 'has scheme display marker',
      val.__schemeDisplayValue, true);
  }

  // =========================================================================
  // Phase 3: Custom Formatters
  // =========================================================================
  logger.title('EnvProxy - Custom Formatters');

  // Test: custom formatter header returns JsonML for SchemeDisplayValue
  {
    // Import will be available after implementation
    let installCustomFormatters;
    try {
      const mod = await import('../../../src/debug/devtools/custom_formatters.js');
      installCustomFormatters = mod.installCustomFormatters;
    } catch {
      // Module not yet implemented — skip
      logger.skip('custom_formatters.js not yet available');
      return; // This is the last test section
    }

    // Set up global devtoolsFormatters
    const savedFormatters = globalThis.devtoolsFormatters;
    globalThis.devtoolsFormatters = [];
    installCustomFormatters();

    const formatter = globalThis.devtoolsFormatters[0];
    assert(logger, 'formatter was installed', formatter !== undefined, true);

    // Test: header returns JsonML for SchemeDisplayValue
    const val = new SchemeDisplayValue("'(1 2 3)", 'list');
    const header = formatter.header(val);
    assert(logger, 'header returns non-null for SchemeDisplayValue',
      header !== null, true);
    assert(logger, 'header is an array (JsonML)', Array.isArray(header), true);

    // Test: header returns null for non-SchemeDisplayValue
    const plainHeader = formatter.header({ x: 1 });
    assert(logger, 'header returns null for plain object', plainHeader, null);

    // Test: hasBody returns false (no expandable body for now)
    assert(logger, 'hasBody returns false', formatter.hasBody(val), false);

    // Clean up
    globalThis.devtoolsFormatters = savedFormatters;
  }

  // =========================================================================
  // Phase 3: Proxy Integration with Formatted Values
  // =========================================================================
  logger.title('EnvProxy - Proxy With Formatted Values');

  // Test: proxy returns formatted values for Scheme types
  {
    const env = new Environment();
    env.define('n', 42n);
    env.define('lst', list(1n, 2n, 3n));
    env.define('sym', intern('hello'));
    const proxy = createEnvProxy(env);

    // BigInt should be converted to Number
    assert(logger, 'proxy formats BigInt as Number', proxy.n, 42);
    // List should be SchemeDisplayValue
    assert(logger, 'proxy formats list as SchemeDisplayValue',
      proxy.lst instanceof SchemeDisplayValue, true);
    // Symbol should be SchemeDisplayValue
    assert(logger, 'proxy formats symbol as SchemeDisplayValue',
      proxy.sym instanceof SchemeDisplayValue, true);
  }
}
