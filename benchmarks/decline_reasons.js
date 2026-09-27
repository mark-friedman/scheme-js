/**
 * Why the compiler tier declines procedures in Scheme that is not a benchmark.
 *
 * The benchmark programs avoid `guard`, `raise`, `parameterize`,
 * `dynamic-wind` and the other control forms, and the compiler's own Scheme was
 * written to avoid them, so neither shows what the decline policy costs a real
 * program. This runs every Scheme file in the repository that is neither --
 * the tests' `.scm` files, the SRFI libraries, the compiler -- as the tier
 * would see it: the standard library compiled, each top-level definition
 * offered to the compiler, and the reachability rule applied over the file.
 * It reports how many procedures would compile, and why each of the rest would
 * not, grouped by reason.
 *
 * Usage: node benchmarks/decline_reasons.js [--files]
 *   --files  also print the tally for each file
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { DefineNode, LambdaNode, BeginNode } from '../src/core/interpreter/ast_nodes.js';
import { setFileResolver, setLibraryLoadHook } from '../src/core/interpreter/library_loader.js';
import { tryCompileDefinition } from '../src/compiler/index.js';
import { unsafeDefinitions } from '../src/compiler/safety.js';
import { installLibraryTable } from '../src/compiler/prebuilt.js';
import prebuiltLibraries from '../src/packaging/compiled_libraries.js';
import { BUNDLED_SOURCES } from '../src/packaging/bundled_libraries.js';
import { interpretedLibrary, installStandardLibrary } from '../tests/harness/standard_library.js';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const perFile = process.argv.includes('--files');

// Libraries a file imports load from the bundled sources, with their prebuilt
// tables installed, as a browser page loads them.
setFileResolver((name) => {
  const file = name[name.length - 1];
  const source = BUNDLED_SOURCES[`${file}.sld`] ?? BUNDLED_SOURCES[`${file}.scm`] ?? BUNDLED_SOURCES[file];
  if (source === undefined) throw new Error(`no library ${name.join(' ')}`);
  return source;
});
setLibraryLoadHook((name, env) => {
  if (BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] !== undefined && env) {
    installLibraryTable(prebuiltLibraries, name, env, (file) => BUNDLED_SOURCES[file]);
  }
});

/**
 * The files measured, by group.
 * @returns {Array<{group: string, file: string, prefix: string}>} Each file, and
 *   Scheme to run before it.
 */
function files() {
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

const totals = new Map();
const byGroup = new Map();
let offered = 0;
const count = (map, key) => map.set(key, (map.get(key) ?? 0) + 1);
const realLog = console.log;

for (const { group, file, prefix } of files()) {
  const pair = interpretedLibrary();
  installStandardLibrary(pair.env);
  const reasons = new Map();
  const asts = [];
  const compiles = new Set();
  console.log = () => {};
  try {
    for (const form of parse(prefix)) pair.interpreter.run(analyze(form), pair.env, [], undefined, { jsAutoConvert: 'raw' });
    for (const form of parse(fs.readFileSync(path.join(root, file), 'utf8'))) {
      const ast = analyze(form);
      for (const def of definitionsIn(ast)) {
        asts.push(def);
        offered++;
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
  } catch (e) {
    console.log = realLog;
    console.log(`${file}: could not be measured: ${e.message.slice(0, 80)}`);
    continue;
  }
  console.log = realLog;
  const tally = byGroup.get(group) ?? new Map();
  byGroup.set(group, tally);
  for (const name of compiles) { count(tally, 'compiles'); count(totals, 'compiles'); }
  for (const reason of reasons.values()) { count(tally, category(reason)); count(totals, category(reason)); }
  if (perFile) console.log(`${file}: ${compiles.size} compile, ${reasons.size} declined`);
}

const show = (title, map) => {
  const all = [...map.values()].reduce((a, b) => a + b, 0);
  console.log(`\n${title}: ${all} top-level definitions`);
  for (const [k, v] of [...map].sort((a, b) => b[1] - a[1])) {
    console.log(`  ${String(v).padStart(5)}  ${(100 * v / all).toFixed(1).padStart(5)}%  ${k}`);
  }
};
for (const [group, tally] of byGroup) show(group, tally);
show('all', totals);
