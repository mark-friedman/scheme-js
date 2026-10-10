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
 * The table comes with a source map placing its code in the Scheme it was
 * compiled from, the program's and the libraries' (`build-program-file` in
 * scripts/lib/ahead.scm), which the bundle's map is chained to, so that
 * DevTools shows the program's code at its Scheme. The system's sources --
 * its libraries' Scheme and the runtime's JavaScript -- are ignore-listed, so
 * that a step goes from the program's Scheme to its JavaScript and back
 * without stopping in them.
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
 * The system's sources, under the repository's `src/`: what a built program's
 * map ignore-lists.
 * @type {string}
 */
const SYSTEM = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..') + path.sep;

/**
 * Writes a program's table and its runtime as one ES module, with a source
 * map beside it if the table has one.
 * @param {string} tableText - The table, as the build wrote it.
 * @param {string|null} tableMap - The table's source map, or null.
 * @param {string} outputFile - Where to write the module.
 * @returns {Promise<void>}
 */
export async function writeProgramBundle(tableText, tableMap, outputFile) {
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
      // The table with its map, which its own `sourceMappingURL` comment
      // would name were the table read alone; a script's last one is its map.
      plugins: tableMap === null ? [] : [{
        name: 'table-map',
        load: (id) => (id === table
          ? { code: tableText.replace(/\n\/\/# sourceMappingURL=.*\n?$/, '\n'), map: tableMap }
          : null)
      }],
      // A module it cannot find would be left out quietly, as if external,
      // and the file would fail as it loads, wherever it is run.
      onwarn: (warning) => {
        if (warning.code === 'UNRESOLVED_IMPORT') throw new Error(warning.message);
      }
    });
    try {
      await bundle.write({
        file: outputFile,
        format: 'es',
        sourcemap: tableMap !== null,
        // The system's sources, and the module that starts the program,
        // made here, are none of the program's.
        sourcemapIgnoreList: (source, sourcemapPath) => {
          const file = path.resolve(path.dirname(sourcemapPath), source);
          return file.startsWith(SYSTEM) || file.startsWith(dir + path.sep);
        }
      });
    } finally {
      await bundle.close();
    }
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}
