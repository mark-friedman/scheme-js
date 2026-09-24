/**
 * @fileoverview Compiles the compiler's own Scheme at build time.
 *
 * Writes `src/packaging/compiled_compiler.js`: one factory per procedure in
 * `src/compiler/ir.scm`, holding the JavaScript the compiler generates for it.
 * This is the step where the compiler compiles itself.
 *
 * ## Why it has to happen, and why at build time
 *
 * `ir.scm` is the lowering pass, and the interpreter can run it from source --
 * which is what makes the bootstrap terminate without a compiler written in
 * another language. But interpreted it is about 300x the JavaScript it
 * replaced, which is too slow to compile anything with. Compiled, it is 16x,
 * which is fast enough not to notice.
 *
 * Doing it at build time rather than at startup buys the same two things it
 * buys the standard library: nothing calls `new Function`, so a page under a
 * strict Content-Security-Policy gets a compiled compiler; and compile *speed*
 * stops being a deployment concern and becomes a slower build, which is the
 * trade that makes writing the compiler in Scheme affordable at all.
 *
 * ## Order
 *
 * Run after `generate_compiled_stdlib.js`, and the order is not cosmetic. This
 * lowering calls `memq` and `assq` on every scope lookup and every global it
 * records, and those are themselves Scheme. Compiling `ir.scm` against an
 * interpreted standard library is worth 1.5x; against a compiled one, 20x.
 *
 *     node scripts/generate_compiled_compiler.js
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { createInterpreter } from '../src/core/interpreter/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { DefineNode } from '../src/core/interpreter/ast_nodes.js';
import { generateEnvironment } from '../src/compiler/index.js';
import { installPrebuilt, fingerprintSources } from '../src/compiler/prebuilt.js';
import { COMPILER_FILES } from '../src/compiler/lowering.js';
import prebuiltStdlib, { LIBRARY_FILES } from '../src/packaging/compiled_stdlib.js';
import { renderTable, serializeConstants } from './lib/render_prebuilt.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const OUTPUT = path.join(ROOT, 'src/packaging/compiled_compiler.js');

/**
 * The procedures other code calls into the compiler's Scheme through; see
 * `src/compiler/lowering.js`.
 * @type {string[]}
 */
const ENTRY_POINTS = ['lower-lambda', 'generate-unit', 'inline-expansion-names'];

/**
 * Where a compiler file lives. Most are the compiler's own; the first two are
 * the implementations of SRFI 1 and SRFI 152, which the compiler loads as it
 * would any Scheme it depends on, and which users reach as libraries.
 * @param {string} file - A name from `COMPILER_FILES`.
 * @returns {string} Its path.
 */
function compilerFilePath(file) {
  const own = path.join(ROOT, 'src/compiler', file);
  return fs.existsSync(own) ? own : path.join(ROOT, 'src/extras/scheme', file);
}

/**
 * Loads the standard library and then the compiler's Scheme into one
 * interpreter.
 *
 * The library is installed from its prebuilt table rather than recompiled. That
 * is not only for speed: `generateEnvironment` below reads whichever bindings
 * are still *interpreted* closures, so installing the library's compiled form
 * first is what leaves exactly the compiler's own procedures for it to find.
 *
 * @returns {{env: Object, sources: Array<string>, names: Set<string>}} The
 *   bootstrapped environment, the compiler sources the fingerprint covers, and
 *   the names those sources define.
 */
function bootstrap() {
  const { interpreter, env } = createInterpreter();

  /**
   * Evaluates Scheme source in the environment.
   * @param {string} source - Scheme source text.
   * @returns {void}
   */
  const run = (source) => {
    for (const form of parse(source)) {
      interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    }
  };

  const librarySources = LIBRARY_FILES.map(
    (file) => fs.readFileSync(path.join(ROOT, 'src/core/scheme', file), 'utf8'));
  for (const source of librarySources) run(source);

  const library = installPrebuilt(env, prebuiltStdlib, fingerprintSources(librarySources));
  if (library.stale) {
    console.log('  the prebuilt standard library is stale, so the library stays interpreted;');
    console.log('  run scripts/generate_compiled_stdlib.js first for a much faster build');
  }

  const sources = [];
  const names = new Set();
  for (const file of COMPILER_FILES) {
    const source = fs.readFileSync(compilerFilePath(file), 'utf8');
    sources.push(source);
    for (const form of parse(source)) {
      const ast = analyze(form);
      if (ast instanceof DefineNode) names.add(ast.name);
    }
    run(source);
  }

  return { env, sources, names };
}

function main() {
  const { env, sources, names } = bootstrap();
  const fingerprint = fingerprintSources(sources);
  const { generated, declined } = generateEnvironment(env);

  // Restricted to what the compiler's own sources defined. Anything else still
  // interpreted in this environment is a library procedure the prebuilt table
  // did not cover, and it belongs in that table rather than this one.
  // Restricted, too, to what the compiler can reach from its entry points.
  // It is written with SRFI 1 and SRFI 152 and uses a handful of each; the
  // rest are loaded, interpreted, and never called here, and compiling them
  // would put the whole of both libraries into every bundle that can compile.
  const byName = new Map(generated.map((entry) => [entry.name, entry]));
  const reachable = new Set();
  const pending = [...ENTRY_POINTS];
  while (pending.length > 0) {
    const name = pending.pop();
    if (reachable.has(name) || !byName.has(name)) continue;
    reachable.add(name);
    pending.push(...byName.get(name).globals);
  }
  const mine = generated.filter((entry) => names.has(entry.name) && reachable.has(entry.name));
  const usable = mine.filter((entry) => serializeConstants(entry.constants) !== null);
  const unserializable = mine.filter((entry) => serializeConstants(entry.constants) === null);

  fs.writeFileSync(OUTPUT, renderTable({
    generator: 'scripts/generate_compiled_compiler.js',
    title: 'The compiler\'s own Scheme, compiled -- the step where it compiles itself.',
    fingerprint,
    filesName: 'COMPILER_FILES',
    files: COMPILER_FILES,
    entries: usable
  }), 'utf8');

  const bytes = usable.reduce((total, entry) => total + entry.source.length, 0);
  console.log(`Compiled the compiler -> ${path.relative(ROOT, OUTPUT)}`);
  console.log(`  ${usable.length} procedures, ${(bytes / 1024).toFixed(0)} KB of generated code`);
  console.log(`  fingerprint ${fingerprint}`);
  if (unserializable.length > 0) {
    console.log(`  ${unserializable.length} left out for a constant that cannot be written down: `
      + unserializable.map((e) => e.name).join(' '));
  }
  const mineDeclined = declined.filter((d) => names.has(d.name));
  if (mineDeclined.length > 0) {
    console.log(`  ${mineDeclined.length} declined by the compiler:`);
    for (const d of mineDeclined) console.log(`    ${d.name}: ${d.reason}`);
  }
}

main();
