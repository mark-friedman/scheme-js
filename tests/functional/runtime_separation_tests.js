/**
 * @fileoverview What compiled code needs as it runs, apart from the
 * interpreter and the library system.
 *
 * A program compiled ahead of time is to run with no interpreter, expander,
 * reader or library system: its code, the runtime its code calls
 * (`src/compiler/runtime.js`), the primitives it reaches and an environment
 * to find them in. Task 77's trial found what tied those to the rest -- the
 * number parser reached through the reader, whose module starts the library
 * system's seed and with it every library's table; `procedure?` and `apply`
 * registered where the interpreter's control forms are; the error-object
 * primitives beside the interpreter's raise; the console ports loading
 * `node:fs` with a top-level `await`. This bundles what compiled code needs,
 * as the build bundles a page's, and checks that none of the rest comes with
 * it. Only JavaScript can see a module graph, and only Node can bundle one.
 */

import path from 'path';
import { fileURLToPath } from 'url';
import { assert } from '../harness/helpers.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');

/**
 * What compiled code needs as it runs: the runtime, the values and the
 * environment it works on, and the primitive groups that stand on their own.
 * @type {string[]}
 */
const RUNTIME_MODULES = [
  'src/compiler/runtime.js',
  'src/core/interpreter/environment.js',
  'src/core/interpreter/primitive_bindings.js',
  'src/core/interpreter/values.js',
  'src/core/interpreter/unwind.js',
  'src/core/primitives/math.js',
  'src/core/primitives/list.js',
  'src/core/primitives/vector.js',
  'src/core/primitives/record.js',
  'src/core/primitives/string.js',
  'src/core/primitives/char.js',
  'src/core/primitives/eq.js',
  'src/core/primitives/bytevector.js',
  'src/core/primitives/apply.js',
  'src/core/primitives/error_object.js',
  'src/core/primitives/io/console_port.js',
  'src/core/primitives/io/stdout_port.js',
  'src/core/primitives/io/stdin_port.js',
  'src/extras/primitives/hash_table.js',
  'src/extras/primitives/bitwise.js'
];

/**
 * Modules none of which compiled code needs as it runs.
 * @type {string[]}
 */
const NOT_NEEDED = [
  'src/core/interpreter/interpreter.js',
  'src/core/interpreter/expand.js',
  'src/core/interpreter/assembler.js',
  'src/core/interpreter/reader/index.js',
  'src/core/interpreter/library_loader.js',
  'src/core/interpreter/library_registry.js',
  'src/core/interpreter/library_seed.js',
  'src/packaging/compiled_libraries.js',
  'src/core/primitives/control.js',
  'src/core/primitives/exception.js'
];

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runRuntimeSeparationTests(logger) {
  logger.title('What compiled code needs as it runs, apart from the interpreter');
  if (typeof process === 'undefined') {
    logger.skip('runtime separation (Node.js only)');
    return;
  }
  const { rollup } = await import('rollup');
  const entry = 'virtual:runtime-entry';
  const bundle = await rollup({
    input: entry,
    // A module it cannot find would be left out quietly, as if external.
    onwarn: (warning) => {
      if (warning.code === 'UNRESOLVED_IMPORT') throw new Error(warning.message);
    },
    plugins: [{
      name: 'runtime-entry',
      resolveId: (id) => (id === entry ? id : null),
      load: (id) => (id === entry
        ? RUNTIME_MODULES.map((m, i) => `export * as m${i} from ${JSON.stringify(path.join(ROOT, m))};`).join('\n')
        : null)
    }]
  });
  const { output } = await bundle.generate({ format: 'es' });
  await bundle.close();
  const chunk = output[0];
  const included = Object.entries(chunk.modules)
    .filter(([, info]) => info.renderedLength > 0)
    .map(([id]) => path.relative(ROOT, id));
  assert(logger, 'none of the interpreter, the expander, the reader or the library system comes with it',
    NOT_NEEDED.filter((m) => included.includes(m)), []);
  // Unminified and with its comments, about 430 KB; the library tables alone,
  // which one import of the reader once brought in, are 3.4 MB.
  assert(logger, 'and the bundle is under 1 MB, as it would not be with the library tables in it',
    chunk.code.length < 1024 * 1024, true);
  assert(logger, 'it waits on nothing as it loads: no module imports another with a top-level await',
    /await import\(/.test(chunk.code), false);
}
