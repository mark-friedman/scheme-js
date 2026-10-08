/**
 * Runs one canonical benchmark through a page's entry in its own process, the
 * tier attached as a page attaches it, and reports the time it measured
 * itself (`benchmarks/run_bundle.js`).
 *
 * The entry is the page's, `src/packaging/scheme_entry.js`: as the source
 * modules, or as rollup bundled it, `dist/scheme.js`. Both are run the same
 * way -- the compiler loaded, the program's own code compiled, the program
 * evaluated as a page runs a script that begins with its imports -- so that
 * what differs between them is the bundling alone.
 *
 * Usage (not intended to be run by hand):
 *   node benchmarks/lib/bundle_worker.js '<json request>'
 * where the request is `{entry, source}`: the entry's path, and the program.
 */

import { pathToFileURL } from 'url';
import { R7RS_DIR } from './r7rs_harness.js';

// Several programs open their data by a path relative to the suite's root,
// as r7rs_worker.js says.
process.chdir(R7RS_DIR);

const { entry, source } = JSON.parse(process.argv[2]);
const page = await import(pathToFileURL(entry).href);
await page.loadCompiler();
page.setUserCodeCompilation(true);

const lines = [];
const log = console.log;
console.log = (...args) => lines.push(args.join(' '));
let error = null;
try {
  page.schemeEval(source, { filename: 'benchmark.scm', program: true });
} catch (e) {
  error = String(e?.message ?? e).slice(0, 120);
}
console.log = log;
process.stdout.write(JSON.stringify({ output: lines.join('\n'), error }));
