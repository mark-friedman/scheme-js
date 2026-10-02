/**
 * @fileoverview Finding the downloaded corpus's libraries, for the
 * measurements that load them.
 *
 * The corpus is other people's Scheme -- SRFI reference implementations and
 * Snow-Fort packages -- recorded in `benchmarks/corpus/manifest.json` and
 * downloaded by `benchmarks/corpus/fetch.js`. A library a program imports is
 * read from the corpus if the corpus has it, and from the bundled sources
 * otherwise, with the bundled one's prebuilt table installed as a browser page
 * installs it.
 */

import fs from 'fs';
import path from 'path';
import { parse } from '../../src/core/interpreter/reader.js';
import { installLibraryTable } from '../../src/compiler/prebuilt.js';
import prebuiltLibraries from '../../src/packaging/compiled_libraries.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { corpusSources, sourceDirectory } from '../corpus/fetch.js';

/**
 * A library name as written: `(srfi 146)`.
 * @param {Array} parts - The name's parts: symbols, numbers or strings.
 * @returns {string} The name.
 */
export function nameKey(parts) {
  return `(${parts.map((p) => p?.name ?? String(p)).join(' ')})`;
}

/**
 * A Scheme list's elements.
 * @param {Object} list - A list.
 * @returns {Array} Its elements.
 */
export function listParts(list) {
  const parts = [];
  for (let c = list; c !== null && c !== undefined; c = c.cdr) parts.push(c.car);
  return parts;
}

/**
 * The name a file's `define-library` declares, if it has one.
 * @param {string} file - The file.
 * @returns {string|null} The name, or null if the file is not a library.
 */
function libraryNameIn(file) {
  let forms;
  try {
    forms = parse(fs.readFileSync(file, 'utf8'));
  } catch (e) {
    return null;
  }
  const form = forms.find((f) => f?.car?.name === 'define-library');
  if (form === undefined) return null;
  return nameKey(listParts(form.cdr.car));
}

/**
 * Every file under a directory with one of some extensions, outside `.git`.
 * @param {string} dir - The directory.
 * @param {Array<string>} extensions - The extensions.
 * @returns {Array<string>} The files.
 */
function filesUnder(dir, extensions) {
  const out = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    if (entry.name === '.git') continue;
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) out.push(...filesUnder(full, extensions));
    else if (extensions.some((ext) => entry.name.endsWith(ext))) out.push(full);
  }
  return out;
}

/**
 * The corpus's libraries, by the name each declares; the programs whose
 * procedures are measured; and the test programs that run them.
 *
 * A Snow-Fort package's libraries are all measured except its tests. An SRFI
 * repository often holds several implementations, or libraries under more than
 * one name, so only the files its manifest entry lists are measured, and where
 * two files declare the same library the listed one is the one loaded. An
 * alias makes a library reachable under the name its importers use when it
 * declares another. A manifest entry's `tests` are programs, relative to its
 * directory, that run its library's own tests.
 *
 * @returns {{libraries: Map<string, Object>, programs: Array<Object>, tests: Array<Object>}}
 * @throws {Error} If a source is not downloaded.
 */
export function corpusIndex() {
  const libraries = new Map();
  const programs = [];
  const tests = [];
  for (const source of corpusSources()) {
    const dir = sourceDirectory(source);
    if (!fs.existsSync(dir)) {
      throw new Error(`${source.library} is not downloaded; run node benchmarks/corpus/fetch.js`);
    }
    const listed = new Set((source.measure ?? []).map((f) => path.join(dir, f)));
    const group = source.kind === 'git'
      ? `SRFI reference implementations${source.role === 'dependency' ? ' (dependencies)' : ''}`
      : `Snow-Fort packages${source.role === 'dependency' ? ' (dependencies)' : ''}`;
    for (const file of filesUnder(dir, ['.sld'])) {
      const name = libraryNameIn(file);
      if (name === null) continue;
      if (libraries.has(name) && !listed.has(file)) continue;
      const test = /(^|[-_/])tests?\.sld$/.test(file) && name !== source.library;
      const measured = source.kind === 'git' ? listed.has(file) : !test;
      libraries.set(name, { name, file, group, measured });
    }
    for (const [alias, file] of Object.entries(source.aliases ?? {})) {
      const full = path.join(dir, file);
      const declared = libraryNameIn(full);
      libraries.set(alias, { name: declared, file: full, group, measured: false });
      if (!listed.has(full) && source.role === 'dependency') {
        libraries.set(declared, { name: declared, file: full, group, measured: true });
      }
    }
    for (const file of listed) {
      if (!file.endsWith('.sld')) programs.push({ group, file, prefix: '' });
    }
    for (const file of source.tests ?? []) {
      tests.push({ name: source.name, library: source.library, file: path.join(dir, file) });
    }
  }
  return { libraries, programs, tests };
}

/**
 * A library resolver over the corpus and the bundled sources, and the load
 * hook that installs a bundled library's prebuilt table, for one set of
 * libraries loaded together: the resolver remembers which directories its
 * libraries came from, since the loader names a file a library includes by
 * the library's name prefix and the file's name, not by the library's
 * directory, and a package may keep `(chibi irregex)` in `irregex.sld` at its
 * root.
 * @param {{libraries: Map<string, Object>}} index - The corpus's libraries.
 * @returns {{resolve: function(Array): string, hook: function(Array, Object): void}}
 */
export function corpusResolver(index) {
  const loadedDirs = [];
  const resolve = (parts) => {
    const last = String(parts[parts.length - 1]?.name ?? parts[parts.length - 1]);
    const library = index.libraries.get(nameKey(parts));
    if (library !== undefined && !/\.[a-z]+$/.test(last)) {
      // The loader names the library's includes by the name it declares, which
      // an alias does not share.
      const declared = listParts(parse(library.name)[0]);
      loadedDirs.unshift({ prefix: nameKey(declared.slice(0, -1)), dir: path.dirname(library.file) });
      return fs.readFileSync(library.file, 'utf8');
    }
    if (/\.[a-z]+$/.test(last)) {
      const prefix = nameKey(parts.slice(0, -1));
      for (const { prefix: p, dir } of loadedDirs) {
        if (p !== prefix) continue;
        const file = path.join(dir, last);
        if (fs.existsSync(file)) return fs.readFileSync(file, 'utf8');
      }
    }
    const source = BUNDLED_SOURCES[`${last}.sld`] ?? BUNDLED_SOURCES[`${last}.scm`] ?? BUNDLED_SOURCES[last];
    if (source === undefined) throw new Error(`no library or file ${nameKey(parts)}`);
    return source;
  };
  const hook = (name, env) => {
    const last = String(name[name.length - 1]?.name ?? name[name.length - 1]);
    if (!index.libraries.has(nameKey(name)) && BUNDLED_SOURCES[`${last}.sld`] !== undefined && env) {
      installLibraryTable(prebuiltLibraries, name, env, (file) => BUNDLED_SOURCES[file]);
    }
  };
  return { resolve, hook };
}

/**
 * Whether a library ships with the bundle and the corpus does not replace it,
 * so that its procedures are compiled at build time and are not the tier's.
 * @param {{libraries: Map<string, Object>}} index - The corpus's libraries.
 * @param {Array} name - The library's name.
 * @returns {boolean}
 */
export function isBundled(index, name) {
  const last = String(name[name.length - 1]?.name ?? name[name.length - 1]);
  return !index.libraries.has(nameKey(name)) && BUNDLED_SOURCES[`${last}.sld`] !== undefined;
}
