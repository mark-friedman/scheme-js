/**
 * @fileoverview A program compiled ahead of time, made one file: its table,
 * the runtime it runs on and the primitives it carries, bundled as one ES
 * module that runs the program as it loads.
 *
 * The build, `(scheme-js ahead)` (scripts/lib/ahead.scm), writes the table, a
 * module of data and generated code that imports nothing; this bundles it
 * with `runMain` (src/compiler/ahead.js) and what that imports, with rollup,
 * as the system's own bundles are built (rollup.config.js). `node OUTPUT`
 * runs the result, and a page loads it with `<script type="module">`.
 *
 * JavaScript because bundling is rollup's, a JavaScript library, and what it
 * is given, modules on disk, is the host's. Used by the CLI's `--build` only.
 */

import fs from 'fs';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';

/**
 * The runtime of a program compiled ahead of time.
 * @type {string}
 */
const AHEAD = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', 'compiler', 'ahead.js');

/**
 * Writes a program's table and its runtime as one ES module.
 * @param {string} tableText - The table, as the build wrote it.
 * @param {string} outputFile - Where to write the module.
 * @returns {Promise<void>}
 */
export async function writeProgramBundle(tableText, outputFile) {
  const { rollup } = await import('rollup');
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-build-'));
  try {
    const table = path.join(dir, 'table.mjs');
    const entry = path.join(dir, 'entry.mjs');
    fs.writeFileSync(table, tableText);
    fs.writeFileSync(entry, `import program from ${JSON.stringify(table)};\n`
      + `import { runMain } from ${JSON.stringify(AHEAD)};\n`
      + 'runMain(program);\n');
    const bundle = await rollup({
      input: entry,
      // A module it cannot find would be left out quietly, as if external,
      // and the file would fail as it loads, wherever it is run.
      onwarn: (warning) => {
        if (warning.code === 'UNRESOLVED_IMPORT') throw new Error(warning.message);
      }
    });
    try {
      await bundle.write({ file: outputFile, format: 'es' });
    } finally {
      await bundle.close();
    }
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
