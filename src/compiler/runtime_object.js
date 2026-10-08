/**
 * @fileoverview The runtime as generated code is given it, its `R`: an
 * ordinary object holding each of src/compiler/runtime.js's exports.
 *
 * Generated code reads the runtime as properties of `R` -- `R.Cons`,
 * `R.callBinding` -- on its hottest paths. A module's namespace object, which
 * `import * as` gives, is read as fast as a property can be. But a bundler
 * that inlines the module -- rollup, which builds the page's bundle and a
 * program compiled ahead of time -- makes the namespace an object of its own,
 * `Object.freeze({__proto__: null, ...})`, and V8 keeps an object written
 * with a null prototype in dictionary mode, where every read is a hash
 * lookup: `earley`, compiled ahead of time and bundled, ran 16% slower for
 * it. A copy made by spreading is an ordinary object, kept fast, bundled or
 * not.
 */

import * as runtime from './runtime.js';

/**
 * The runtime, as generated code reads it.
 * @type {Object}
 */
export const RUNTIME = { ...runtime };
