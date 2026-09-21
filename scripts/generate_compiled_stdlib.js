/**
 * @fileoverview Compiles the standard library at build time.
 *
 * Writes `src/packaging/compiled_stdlib.js`: one factory function per library
 * procedure, holding the JavaScript the compiler would otherwise generate when
 * the system starts. That saves about 20 ms of a 71 ms bootstrap, and it means
 * nothing calls `new Function` at run time -- so a page with a strict
 * Content-Security-Policy gets the compiled library instead of an interpreted
 * one.
 *
 * Which procedures are compiled is decided by `generateEnvironment`, the same
 * function the runtime path uses, so the bundle cannot contain code for a
 * different set of procedures than the runtime expects.
 *
 * Run by `npm run prebuild`, alongside the source-text bundling.
 *
 *     node scripts/generate_compiled_stdlib.js
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { createInterpreter } from '../src/core/interpreter/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { generateEnvironment } from '../src/compiler/index.js';
import { fingerprintSources } from '../src/compiler/prebuilt.js';
import { renderTable, serializeConstants } from './lib/render_prebuilt.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const OUTPUT = path.join(ROOT, 'src/packaging/compiled_stdlib.js');

/**
 * The library files, in load order.
 *
 * Order matters twice over: the later files depend on the earlier ones, and the
 * analyzer's renaming counter advances as it goes, so the names in the
 * generated code depend on this sequence. The fingerprint covers the sources in
 * exactly this order for that reason.
 *
 * @type {string[]}
 */
const LIBRARY_FILES = [
  'macros.scm',
  'equality.scm',
  'cxr.scm',
  'numbers.scm',
  'list.scm',
  'control.scm',
  'case_lambda.scm'
];

/**
 * Loads the library into a fresh interpreter, interpreted.
 * @returns {{interpreter: Object, env: Object, sources: Array<string>}} The
 *   bootstrapped pair and the sources that were loaded.
 */
function bootstrap() {
  const { interpreter, env } = createInterpreter();
  const sources = [];
  for (const file of LIBRARY_FILES) {
    const source = fs.readFileSync(path.join(ROOT, 'src/core/scheme', file), 'utf8');
    sources.push(source);
    for (const form of parse(source)) {
      interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    }
  }
  return { interpreter, env, sources };
}

function main() {
  const { env, sources } = bootstrap();
  const fingerprint = fingerprintSources(sources);
  const { generated, declined } = generateEnvironment(env);

  // A constant pool has to be rebuilt in the generated module, and not every
  // value can be written down -- see `serializeConstants`. One that cannot be
  // is left out and reported, and the runtime leaves that procedure
  // interpreted.
  const unserializable = generated.filter((entry) => serializeConstants(entry.constants) === null);
  const usable = generated.filter((entry) => serializeConstants(entry.constants) !== null);

  fs.writeFileSync(OUTPUT, renderTable({
    generator: 'scripts/generate_compiled_stdlib.js',
    title: 'The standard library, compiled.',
    fingerprint,
    filesName: 'LIBRARY_FILES',
    files: LIBRARY_FILES,
    entries: usable
  }), 'utf8');

  const bytes = usable.reduce((total, entry) => total + entry.source.length, 0);
  console.log(`Compiled standard library -> ${path.relative(ROOT, OUTPUT)}`);
  console.log(`  ${usable.length} procedures, ${(bytes / 1024).toFixed(0)} KB of generated code`);
  console.log(`  fingerprint ${fingerprint}`);
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
