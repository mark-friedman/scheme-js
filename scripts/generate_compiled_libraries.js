/**
 * @fileoverview Compiles every library the bundle ships, at build time.
 *
 * Writes `src/packaging/compiled_libraries.js`: for each library in
 * `src/core/scheme/` and `src/extras/scheme/`, one factory per procedure the
 * library defines, holding the JavaScript the compiler would otherwise generate
 * when the library loads. The runtime installs a library's table into its
 * environment as it loads -- see `installLibraryTable` in
 * src/compiler/prebuilt.js.
 *
 * ## Why every library, and why at build time
 *
 * Compiling at run time needs the compiler, and the compiler is most of the
 * bundle's weight. A page that runs its program interpreted -- every page,
 * until user code is compiled -- still wants the libraries compiled: `map`,
 * `assoc` and SRFI 125's lookups are what compiled and interpreted code alike
 * spend their time in, and interpreted they cost their callers 10-17x. With
 * every shipped library prebuilt, such a page needs no compiler at all, so the
 * compiler can be loaded only by a page that asks for it.
 *
 * It also means nothing calls `new Function` at run time, so a page under a
 * strict Content-Security-Policy gets the compiled libraries rather than
 * interpreted ones.
 *
 * ## How
 *
 * Each library is loaded by name, through the ordinary loader, so it is
 * compiled in the environment it will have at run time. A load hook compiles
 * each one as it arrives and installs the result at once, so a library that
 * imports it sees compiled procedures here just as it will in the bundle.
 * Which procedures are compiled is decided by `generateEnvironment`, the same
 * function the runtime path uses, restricted to those the library itself
 * defines: a library's environment also holds everything it imported, and
 * those belong to the library that defined them.
 *
 * Run by `npm run prebuild`, after the source-text bundling and before the
 * compiler compiles itself.
 *
 *     node scripts/generate_compiled_libraries.js
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
import { libraryNameToKey } from '../src/core/interpreter/library_registry.js';
import { generateEnvironment } from '../src/compiler/index.js';
import { installPrebuilt, fingerprintSources } from '../src/compiler/prebuilt.js';
import { renderLibraries, serializeConstants } from './lib/render_prebuilt.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const OUTPUT = path.join(ROOT, 'src/packaging/compiled_libraries.js');

/**
 * Where the bundled libraries live, as `scripts/generate_bundled_libraries.js`
 * reads them.
 * @type {string[]}
 */
const LIBRARY_DIRS = ['src/core/scheme', 'src/extras/scheme'].map((dir) => path.join(ROOT, dir));

/**
 * Reads one of the bundled libraries' files by name.
 * @param {string} file - A file name, such as `list.scm` or `1.sld`.
 * @returns {string|undefined} Its source, if there is such a file.
 */
function readSource(file) {
  for (const dir of LIBRARY_DIRS) {
    const candidate = path.join(dir, file);
    if (fs.existsSync(candidate)) return fs.readFileSync(candidate, 'utf8');
  }
  return undefined;
}

/**
 * Finds a library's file, or a file it includes, by the last part of its name,
 * as the bundle's resolver does.
 * @param {string[]} name - A library name, or an include's path.
 * @returns {string} Its source.
 */
function resolve(name) {
  const last = name[name.length - 1];
  const source = readSource(`${last}.sld`) ?? readSource(last);
  if (source === undefined) throw new Error(`no bundled library file for ${name.join('/')}`);
  return source;
}

/**
 * The files a library is made of, in the order its fingerprint covers them:
 * its `.sld`, then what it includes.
 * @param {string[]} name - The library's name.
 * @returns {string[]} File names.
 */
function libraryFiles(name) {
  const sld = `${name[name.length - 1]}.sld`;
  const libDef = parseDefineLibrary(parse(readSource(sld))[0]);
  return [sld, ...libDef.includes, ...libDef.includesCi, ...libDef.includeLibraryDeclarations];
}

/**
 * Compiles the procedures a library defines, and installs them in its
 * environment.
 * @param {string[]} name - The library's name.
 * @param {Object} env - Its own environment, just loaded.
 * @returns {Object} Its table, and what was left out of it.
 */
function compileLibrary(name, env) {
  const files = libraryFiles(name);
  const fingerprint = fingerprintSources(files.map(readSource));
  const { generated, declined } = generateEnvironment(env, { ownOnly: true });

  // A constant pool has to be rebuilt in the generated module, and not every
  // value can be written down -- see `serializeConstants`. One that cannot be
  // is left out and reported, and the runtime leaves that procedure
  // interpreted.
  const entries = generated.filter((entry) => serializeConstants(entry.constants) !== null);
  const unserializable = generated.filter((entry) => serializeConstants(entry.constants) === null);

  const procedures = {};
  for (const entry of entries) {
    procedures[entry.name] = {
      params: entry.params,
      rest: entry.rest,
      constants: entry.constants,
      make: new Function('R', 'E', 'K', entry.source)
    };
  }
  installPrebuilt(env, { fingerprint, files, procedures }, fingerprint);

  return { key: libraryNameToKey(name), fingerprint, files, entries, declined, unserializable };
}

function main() {
  const { interpreter, env } = createInterpreter();
  setFileResolver(resolve);
  const libraries = [];
  setLibraryLoadHook((name, libraryEnv) => libraries.push(compileLibrary(name, libraryEnv)));

  // Loading a library loads what it imports first, so the hook sees every
  // library after the ones it depends on.
  const sldFiles = LIBRARY_DIRS.flatMap((dir) => fs.readdirSync(dir))
    .filter((file) => file.endsWith('.sld')).sort();
  for (const file of sldFiles) {
    const { name } = parseDefineLibrary(parse(readSource(file))[0]);
    loadLibrarySync(name, analyze, interpreter, env);
  }
  setLibraryLoadHook(null);

  // A library that defines no procedure of its own -- one that only
  // re-exports, or whose procedures are all primitives -- needs no table.
  const tables = libraries.filter((library) => library.entries.length > 0)
    .sort((a, b) => a.key.localeCompare(b.key));
  fs.writeFileSync(OUTPUT, renderLibraries({
    generator: 'scripts/generate_compiled_libraries.js',
    title: 'The libraries the bundle ships, compiled.',
    libraries: tables
  }), 'utf8');

  console.log(`Compiled libraries -> ${path.relative(ROOT, OUTPUT)}`);
  for (const library of tables) {
    const bytes = library.entries.reduce((total, entry) => total + entry.source.length, 0);
    console.log(`  ${library.key}: ${library.entries.length} procedures, `
      + `${(bytes / 1024).toFixed(0)} KB, fingerprint ${library.fingerprint}`);
    if (library.unserializable.length > 0) {
      console.log(`    ${library.unserializable.length} left out for a constant that cannot be `
        + `written down: ${library.unserializable.map((e) => e.name).join(' ')}`);
    }
    for (const d of library.declined) console.log(`    declined ${d.name}: ${d.reason}`);
  }
}

main();
