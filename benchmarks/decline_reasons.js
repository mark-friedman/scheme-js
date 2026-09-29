/**
 * Why the compiler tier declines procedures in Scheme that is not a benchmark.
 *
 * The benchmark programs avoid `guard`, `raise`, `parameterize`,
 * `dynamic-wind` and the other control forms, and the compiler's own Scheme was
 * written to avoid them, so neither shows what the decline policy costs a real
 * program. It reports how many procedures would compile, and why each of the
 * rest would not, grouped by reason.
 *
 * By default it runs every Scheme file in the repository that is neither --
 * the tests' `.scm` files, the SRFI libraries, the compiler -- as the tier
 * would see it: the standard library compiled, each top-level definition
 * offered to the compiler, and the reachability rule applied over the file.
 * That turned out to measure little, because this repository's Scheme avoids
 * the control forms too.
 *
 * With `--corpus` it runs other people's Scheme instead: SRFI reference
 * implementations and Snow-Fort packages, recorded in
 * `benchmarks/corpus/manifest.json` and downloaded by
 * `benchmarks/corpus/fetch.js`. Each library is loaded, and its procedures are
 * put to the policy the build applies to a library (`generateEnvironment`,
 * procedures it defines itself only); a file that is a program rather than a
 * library is measured as the repository's files are. Two further tables say
 * which control form each control decline ends at, and how often the
 * libraries' source uses each form -- needed because `guard` expands into
 * `with-exception-handler`, and `parameterize` into a library procedure that
 * uses `dynamic-wind`, so the reasons alone cannot tell them apart.
 *
 * Usage: node benchmarks/decline_reasons.js [--corpus] [--files] [--reasons]
 *   --corpus   measure the downloaded corpus rather than the repository
 *   --files    also print the tally for each file or library
 *   --reasons  also print each declined procedure and its full reason
 * With DECLINE_STACKS=1 in the environment, a library that cannot be measured
 * is reported with the stack of its failure, for bringing a new one up.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { DefineNode, LambdaNode, BeginNode } from '../src/core/interpreter/ast_nodes.js';
import { setFileResolver, setLibraryLoadHook } from '../src/core/interpreter/library_loader.js';
import { getLibraryEnv } from '../src/core/interpreter/library_registry.js';
import { tryCompileDefinition, generateEnvironment } from '../src/compiler/index.js';
import { unsafeDefinitions } from '../src/compiler/index.js';
import { installLibraryTable } from '../src/compiler/prebuilt.js';
import prebuiltLibraries from '../src/packaging/compiled_libraries.js';
import { BUNDLED_SOURCES } from '../src/packaging/bundled_libraries.js';
import { interpretedLibrary, installStandardLibrary } from '../tests/harness/standard_library.js';
import { corpusSources, sourceDirectory } from './corpus/fetch.js';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const perFile = process.argv.includes('--files');
const showReasons = process.argv.includes('--reasons');
const corpusMode = process.argv.includes('--corpus');

// =============================================================================
// Finding libraries
// =============================================================================

/**
 * A library name as written: `(srfi 146)`.
 * @param {Array} parts - The name's parts, symbols or numbers.
 * @returns {string} The name.
 */
function nameKey(parts) {
  return `(${parts.map((p) => p?.name ?? String(p)).join(' ')})`;
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
  const parts = [];
  for (let c = form.cdr.car; c !== null && c !== undefined; c = c.cdr) parts.push(c.car);
  return nameKey(parts);
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
 * The corpus's libraries, by the name each declares, and its programs.
 *
 * A Snow-Fort package's libraries are all measured except its tests. An SRFI
 * repository often holds several implementations, or libraries under more than
 * one name, so only the files its manifest entry lists are measured, and where
 * two files declare the same library the listed one is the one loaded. An
 * alias makes a library reachable under the name its importers use when it
 * declares another.
 *
 * @returns {{libraries: Map<string, Object>, programs: Array<Object>}}
 */
function corpusIndex() {
  const libraries = new Map();
  const programs = [];
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
  }
  return { libraries, programs };
}

const index = corpusMode ? corpusIndex() : { libraries: new Map(), programs: [] };

/**
 * The directories an `include` might be relative to, most recently loaded
 * library first. The loader names an included file by the including library's
 * name prefix and the file's name, not by the library's directory, and a
 * package may keep `(chibi irregex)` in `irregex.sld` at its root.
 * @type {Array<{prefix: string, dir: string}>}
 */
const loadedDirs = [];

// A library is read from the corpus if it is there, and from the bundled
// sources otherwise, with its prebuilt table installed as a browser page does.
setFileResolver((parts) => {
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
});
setLibraryLoadHook((name, env) => {
  const last = name[name.length - 1];
  if (!index.libraries.has(nameKey(name)) && BUNDLED_SOURCES[`${last}.sld`] !== undefined && env) {
    installLibraryTable(prebuiltLibraries, name, env, (file) => BUNDLED_SOURCES[file]);
  }
});

/**
 * The files the repository's own measurement reads, by group.
 * @returns {Array<{group: string, file: string, prefix: string}>} Each file, and
 *   Scheme to run before it.
 */
function repositoryFiles() {
  const list = [];
  const walk = (dir) => {
    for (const entry of fs.readdirSync(path.join(root, dir), { withFileTypes: true })) {
      const rel = path.join(dir, entry.name);
      if (entry.isDirectory()) walk(rel);
      else if (entry.name.endsWith('.scm') && entry.name !== 'test.scm') list.push(rel);
    }
  };
  walk('tests');
  const harness = fs.readFileSync(path.join(root, 'tests/core/scheme/test.scm'), 'utf8');
  const out = list.filter((f) => !f.includes('/fuzz/')).map((file) => ({ group: 'tests', file, prefix: harness }));
  for (const f of ['list_lib.scm', 'hash_table.scm', 'comparator.scm', 'string_lib.scm']) {
    out.push({ group: 'SRFI libraries', file: `src/extras/scheme/${f}`, prefix: '(import (srfi 1))' });
  }
  for (const f of ['ir.scm', 'lift.scm', 'liveness.scm', 'inline.scm', 'emit.scm']) {
    out.push({ group: 'compiler', file: `src/compiler/${f}`, prefix: '(import (srfi 1) (srfi 152))' });
  }
  return out;
}

// =============================================================================
// Classifying reasons
// =============================================================================

/**
 * The reason a declined procedure is counted under: the control global at the
 * end of the path, and whether the path was direct.
 * @param {string} reason - The reason the tier gave.
 * @returns {string} The category.
 */
function category(reason) {
  const control = reason.match(/references(?: control global)? '([^']+)'/);
  if (control) return reason.includes(' -> ') || !reason.startsWith('references')
    ? `reaches '${control[1]}' through another procedure` : `references '${control[1]}'`;
  if (reason.includes('captures a continuation')) {
    return reason.startsWith('captures') ? 'captures a continuation' : 'reaches a capture through another procedure';
  }
  return reason.replace(/[`'"][^`'"]*[`'"]/g, '...').slice(0, 70);
}

/**
 * The control form a decline ends at, grouped by what it would need to
 * compile, or null if the decline is not for a control form.
 * @param {string} reason - The reason the tier gave.
 * @returns {string|null} The form.
 */
function controlForm(reason) {
  if (reason.includes('param-dynamic-bind')) return 'parameterize (through dynamic-wind)';
  const named = [...reason.matchAll(/'([^']+)'/g)].map((m) => m[1]);
  const last = named[named.length - 1];
  if (reason.includes('captures a continuation') && (last === undefined || !reason.endsWith(`'${last}'`))) {
    return 'call/cc';
  }
  if (last === undefined) return null;
  if (['call/cc', 'call-with-current-continuation', 'call-with-escape-continuation'].includes(last)) return 'call/cc';
  if (['with-exception-handler', 'guard', 'raise', 'raise-continuable'].includes(last)) {
    return last === 'raise' || last === 'raise-continuable' ? last : 'with-exception-handler, or guard';
  }
  if (['dynamic-wind', 'eval', 'call-with-values', 'exit', 'emergency-exit', 'make-parameter', 'parameterize'].includes(last)) {
    return last;
  }
  return null;
}

/**
 * The control forms a library's source uses, counted by name. The library's
 * `.sld` and the files it includes are read, comments dropped.
 * @param {string} file - The library's `.sld`, or a program.
 * @returns {Map<string, number>} Uses of each form.
 */
function controlUses(file) {
  const texts = [fs.readFileSync(file, 'utf8')];
  for (const m of texts[0].matchAll(/\(include(?:-ci)?((?:\s+"[^"]+")+)/g)) {
    for (const inc of m[1].matchAll(/"([^"]+)"/g)) {
      const full = path.join(path.dirname(file), inc[1]);
      if (fs.existsSync(full)) texts.push(fs.readFileSync(full, 'utf8'));
    }
  }
  const uses = new Map();
  const forms = /\((guard|with-exception-handler|raise|raise-continuable|error|parameterize|make-parameter|dynamic-wind|call\/cc|call-with-current-continuation|call-with-escape-continuation)[\s)]/g;
  for (const text of texts) {
    for (const m of text.replace(/;[^\n]*/g, '').matchAll(forms)) {
      const form = m[1] === 'call-with-current-continuation' ? 'call/cc' : m[1];
      uses.set(form, (uses.get(form) ?? 0) + 1);
    }
  }
  return uses;
}

// =============================================================================
// Measuring
// =============================================================================

/**
 * The top-level definitions in an analyzed form: the form itself, or those
 * in a `begin` it is, as `test-group` expands to.
 * @param {Object} ast - An analyzed top-level form.
 * @returns {Array<Object>} The definitions.
 */
function definitionsIn(ast) {
  if (ast instanceof DefineNode) return [ast];
  if (ast instanceof BeginNode) return ast.expressions.flatMap(definitionsIn);
  return [];
}

/**
 * Measures a program: each top-level definition offered to the compiler, and
 * the reachability rule applied over the file.
 * @param {string} file - The program.
 * @param {string} prefix - Scheme to run first.
 * @returns {{compiles: Set<string>, reasons: Map<string, string>}}
 */
function measureProgram(file, prefix) {
  const pair = interpretedLibrary();
  installStandardLibrary(pair.env);
  const reasons = new Map();
  const asts = [];
  const compiles = new Set();
  for (const form of parse(prefix)) pair.interpreter.run(analyze(form), pair.env, [], undefined, { jsAutoConvert: 'raw' });
  for (const form of parse(fs.readFileSync(file, 'utf8'))) {
    const ast = analyze(form);
    for (const def of definitionsIn(ast)) {
      asts.push(def);
      if (!(def.valueExpr instanceof LambdaNode)) {
        reasons.set(def.name, 'the definition is not a procedure');
      } else {
        const result = tryCompileDefinition(def, pair.env);
        if (result.compiled) compiles.add(def.name);
        else reasons.set(def.name, result.reason);
      }
    }
    try {
      pair.interpreter.run(ast, pair.env, [], undefined, { jsAutoConvert: 'raw' });
    } catch (e) {
      // A test file may raise on purpose; what matters here is its definitions.
    }
  }
  for (const [name, reason] of unsafeDefinitions(asts, pair.env)) {
    if (compiles.has(name)) { compiles.delete(name); reasons.set(name, reason); }
  }
  return { compiles, reasons };
}

/**
 * Measures a library: loads it, and puts the procedures it defines itself to
 * the policy the build applies to a library.
 * @param {Object} pair - The interpreter and environment to import it into.
 * @param {string} name - The library's name.
 * @returns {{compiles: Set<string>, reasons: Map<string, string>}}
 */
function measureLibrary(pair, name) {
  pair.interpreter.run(analyze(parse(`(import ${name})`)[0]), pair.env, [], undefined, { jsAutoConvert: 'raw' });
  const env = getLibraryEnv(listParts(parse(name)[0]));
  if (env === null) throw new Error('loaded, but not registered under its name');
  const { generated, declined } = generateEnvironment(env, { ownOnly: true });
  return {
    compiles: new Set(generated.map((g) => g.name)),
    reasons: new Map(declined.map((d) => [d.name, d.reason]))
  };
}

/**
 * A Scheme list's elements.
 * @param {Object} list - A list.
 * @returns {Array} Its elements.
 */
function listParts(list) {
  const parts = [];
  for (let c = list; c !== null && c !== undefined; c = c.cdr) parts.push(c.car);
  return parts;
}

// =============================================================================
// Reporting
// =============================================================================

const totals = new Map();
const byGroup = new Map();
const forms = new Map();
const uses = new Map();
const unmeasured = [];
const count = (map, key, n = 1) => map.set(key, (map.get(key) ?? 0) + n);
const realLog = console.log;

/**
 * Adds one file's or library's outcome to the tallies.
 * @param {string} group - Its group.
 * @param {string} label - Its name, for `--files`.
 * @param {{compiles: Set<string>, reasons: Map<string, string>}} result - Its outcome.
 * @param {string} file - Its source, whose control forms are counted.
 */
function record(group, label, { compiles, reasons }, file) {
  const tally = byGroup.get(group) ?? new Map();
  byGroup.set(group, tally);
  count(tally, 'compiles', compiles.size);
  count(totals, 'compiles', compiles.size);
  for (const reason of reasons.values()) {
    count(tally, category(reason));
    count(totals, category(reason));
    const form = controlForm(reason);
    if (form !== null) count(forms, form);
  }
  for (const [form, n] of controlUses(file)) count(uses, form, n);
  if (perFile) realLog(`${label}: ${compiles.size} compile, ${reasons.size} declined`);
  if (showReasons) {
    for (const [name, reason] of reasons) realLog(`  ${label} ${name}: ${reason}`);
  }
}

/**
 * Runs one measurement with the measured code's output suppressed, noting it
 * as unmeasured if it fails.
 * @param {string} label - What is being measured.
 * @param {Function} measure - Returns the outcome.
 * @returns {Object|null} The outcome, or null if it could not be measured.
 */
function quietly(label, measure) {
  console.log = () => {};
  try {
    return measure();
  } catch (e) {
    unmeasured.push(`${label}: ${String(e.message).split('\n')[0].slice(0, 100)}`);
    if (process.env.DECLINE_STACKS) unmeasured.push(e.stack);
    return null;
  } finally {
    console.log = realLog;
  }
}

if (corpusMode) {
  const pair = interpretedLibrary();
  installStandardLibrary(pair.env);
  const measured = [...index.libraries.entries()].filter(([key, lib]) => lib.measured && key === lib.name);
  for (const [name, lib] of measured) {
    const result = quietly(name, () => measureLibrary(pair, name));
    if (result !== null) record(lib.group, name, result, lib.file);
  }
  for (const { group, file, prefix } of index.programs) {
    const label = path.relative(root, file);
    const result = quietly(label, () => measureProgram(file, prefix));
    if (result !== null) record(group, label, result, file);
  }
} else {
  for (const { group, file, prefix } of repositoryFiles()) {
    const result = quietly(file, () => measureProgram(path.join(root, file), prefix));
    if (result !== null) record(group, file, result, path.join(root, file));
  }
}

const show = (title, map, unit = 'top-level definitions') => {
  const all = [...map.values()].reduce((a, b) => a + b, 0);
  console.log(`\n${title}: ${all} ${unit}`);
  for (const [k, v] of [...map].sort((a, b) => b[1] - a[1])) {
    console.log(`  ${String(v).padStart(5)}  ${(100 * v / all).toFixed(1).padStart(5)}%  ${k}`);
  }
};
for (const [group, tally] of byGroup) show(group, tally);
show('all', totals);
show('Declines for a control form, by the form each ends at', forms, 'declines');
show('Uses of each control form in the source', uses, 'uses');
if (unmeasured.length > 0) {
  console.log(`\nCould not be measured (${unmeasured.length}):`);
  for (const line of unmeasured) console.log(`  ${line}`);
}
