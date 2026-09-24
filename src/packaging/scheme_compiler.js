/**
 * @fileoverview The compiler, as a bundle loads it on demand.
 *
 * `scheme_entry.js` reaches this module only through a dynamic `import()` in
 * `loadCompiler`, so a bundler splits it, and everything only it needs, into a
 * file of its own: the compiler's JavaScript, its Scheme sources, and its
 * prebuilt table, which together are most of what a bundle that can compile
 * weighs. A page that never compiles its own code never fetches it.
 *
 * A module of its own, rather than a dynamic import of `src/compiler/index.js`
 * itself, so that the split file has a name that says what it is and the
 * bundle's surface is only what a page needs.
 */

export {
  compileProgram,
  compileEnvironment,
  tryCompileDefinition,
  tryCompileClosure
} from '../compiler/index.js';
