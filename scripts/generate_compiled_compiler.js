/**
 * @fileoverview Compiles the compiler's own Scheme at build time.
 *
 * Writes `src/packaging/compiled_compiler.js`: one factory per procedure the
 * `(scheme-js compiler)` library defines -- `src/compiler/compiler.sld` and the
 * files it includes -- holding the JavaScript the compiler generates for it.
 * This is the step where the compiler compiles itself.
 *
 * ## Why it has to happen, and why at build time
 *
 * The interpreter can run the compiler's library from source, which is what
 * makes the bootstrap terminate without a compiler written in another
 * language. But interpreted it is about 300x the JavaScript it replaced, which
 * is too slow to compile anything with. Compiled, it is 16x, which is fast
 * enough not to notice.
 *
 * Doing it at build time rather than at startup buys the same two things it
 * buys the libraries: nothing calls `new Function`, so a page under a strict
 * Content-Security-Policy gets a compiled compiler; and compile *speed* stops
 * being a deployment concern and becomes a slower build, which is the trade
 * that makes writing the compiler in Scheme affordable at all.
 *
 * The table has the same shape as the libraries' -- the compiler is a library
 * -- but is a module of its own, so that a bundle can load it only when
 * something asks to compile.
 *
 * ## Order
 *
 * Run after `generate_compiled_libraries.js`, and the order is not cosmetic.
 * The lowering calls `memq` and `assq` on every scope lookup and every global
 * it records, and those are themselves Scheme. Compiling the compiler against
 * an interpreted standard library is worth 1.5x; against a compiled one, 20x.
 *
 *     node scripts/generate_compiled_compiler.js
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { createInterpreter } from '../src/core/interpreter/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import {
  setFileResolver, setLibraryLoadHook, loadLibrarySync, parseDefineLibrary
} from '../src/core/interpreter/library_loader.js';
import { libraryNameToKey, getLibraryEnv } from '../src/core/interpreter/library_registry.js';
import { generateEnvironment } from '../src/compiler/index.js';
import { installLibraryTable, fingerprintSources } from '../src/compiler/prebuilt.js';
import { COMPILER_LIBRARY, compilerStartFailure } from '../src/compiler/lowering.js';
import prebuiltLibraries from '../src/packaging/compiled_libraries.js';
import { renderLibraries, serializeConstants } from './lib/render_prebuilt.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const OUTPUT = path.join(ROOT, 'src/packaging/compiled_compiler.js');

/**
 * Where the compiler's files and the libraries it imports live.
 * @type {string[]}
 */
const SOURCE_DIRS = ['src/compiler', 'src/core/scheme', 'src/extras/scheme']
  .map((dir) => path.join(ROOT, dir));

/**
 * Reads a file of the compiler's library, or of a library it imports, by name.
 * @param {string} file - A file name, such as `ir.scm` or `1.sld`.
 * @returns {string|undefined} Its source, if there is such a file.
 */
function readSource(file) {
  for (const dir of SOURCE_DIRS) {
    const candidate = path.join(dir, file);
    if (fs.existsSync(candidate)) return fs.readFileSync(candidate, 'utf8');
  }
  return undefined;
}

/**
 * Finds a library's file, or a file it includes, by the last part of its name.
 * @param {string[]} name - A library name, or an include's path.
 * @returns {string} Its source.
 */
function resolve(name) {
  const last = name[name.length - 1];
  const source = readSource(`${last}.sld`) ?? readSource(last);
  if (source === undefined) throw new Error(`no file for ${name.join('/')}`);
  return source;
}

/**
 * Loads the compiler's library, with the libraries it imports installed from
 * their prebuilt tables.
 *
 * Installing those first is not only for speed: `generateEnvironment` reads
 * whichever bindings are still *interpreted* closures, and the compiler's own
 * procedures are the only ones it should find.
 *
 * @returns {{env: Object, exports: Map<string, *>, files: string[]}} The
 *   library's environment and exports, and the files the fingerprint covers.
 */
function bootstrap() {
  const { interpreter, env } = createInterpreter();
  setFileResolver(resolve);
  const stale = [];
  setLibraryLoadHook((name, libraryEnv) => {
    const outcome = installLibraryTable(prebuiltLibraries, name, libraryEnv, readSource);
    if (outcome !== null && outcome.stale) stale.push(libraryNameToKey(name));
  });
  const exports = loadLibrarySync(COMPILER_LIBRARY, analyze, interpreter, env);
  setLibraryLoadHook(null);
  if (stale.length > 0) {
    console.log(`  prebuilt tables are stale for ${stale.join(', ')}, so those stay interpreted;`);
    console.log('  run scripts/generate_compiled_libraries.js first for a much faster build');
  }

  const sld = `${COMPILER_LIBRARY[COMPILER_LIBRARY.length - 1]}.sld`;
  const libDef = parseDefineLibrary(parse(readSource(sld))[0]);
  const files = [sld, ...libDef.includes, ...libDef.includesCi, ...libDef.includeLibraryDeclarations];
  return { env: getLibraryEnv(COMPILER_LIBRARY), exports, files };
}

function main() {
  // The compiler declines everything when it cannot start, which here would
  // write empty tables and report success.
  const failure = compilerStartFailure();
  if (failure !== null) {
    console.error(`The compiler could not start, so nothing can be compiled: ${failure}`);
    process.exit(1);
  }
  const { env, exports, files } = bootstrap();
  const fingerprint = fingerprintSources(files.map(readSource));
  const { generated, declined } = generateEnvironment(env, { ownOnly: true });

  // Restricted to what the compiler can reach from its exports, which are the
  // entry points other code calls it through. A procedure nothing reaches is
  // left out rather than shipped; the compiler's Scheme tests, which call
  // internal procedures directly, run such a one interpreted.
  const byName = new Map(generated.map((entry) => [entry.name, entry]));
  const reachable = new Set();
  const pending = [...exports.keys()];
  while (pending.length > 0) {
    const name = pending.pop();
    if (reachable.has(name) || !byName.has(name)) continue;
    reachable.add(name);
    pending.push(...byName.get(name).globals);
  }
  const mine = generated.filter((entry) => reachable.has(entry.name));
  const usable = mine.filter((entry) => serializeConstants(entry.constants) !== null);
  const unserializable = mine.filter((entry) => serializeConstants(entry.constants) === null);

  fs.writeFileSync(OUTPUT, renderLibraries({
    generator: 'scripts/generate_compiled_compiler.js',
    title: 'The compiler\'s own library, compiled -- the step where it compiles itself.',
    libraries: [{ key: libraryNameToKey(COMPILER_LIBRARY), fingerprint, files, entries: usable }]
  }), 'utf8');

  const bytes = usable.reduce((total, entry) => total + entry.source.length, 0);
  console.log(`Compiled the compiler -> ${path.relative(ROOT, OUTPUT)}`);
  console.log(`  ${usable.length} procedures, ${(bytes / 1024).toFixed(0)} KB of generated code`);
  console.log(`  fingerprint ${fingerprint}`);
  const unreached = generated.length - mine.length;
  if (unreached > 0) console.log(`  ${unreached} not reachable from the exports, left out`);
  if (unserializable.length > 0) {
    console.log(`  ${unserializable.length} left out for a constant that cannot be written down: `
      + unserializable.map((e) => e.name).join(' '));
  }
  if (declined.length > 0) {
    console.log(`  ${declined.length} declined by the compiler:`);
    for (const d of declined) console.log(`    ${d.name}: ${d.reason}`);
  }
}

main();
