/**
 * Runs the canonical R7RS suite through the page's bundle, beside the same
 * code as source modules, in Node and in headless Chrome.
 *
 * ## Why this exists
 *
 * Every figure for compiled code had been taken in Node from the source
 * modules, on the belief that a page runs the same generated code as fast. It
 * did not: rollup writes a module's namespace as an object V8 reads slowly,
 * and generated code read the runtime through it, so a page ran some programs
 * up to 1.9 times slower than Node did (R141). This measures what a page runs
 * as a page builds it, so that any other difference the bundle makes is seen.
 *
 * ## What is measured
 *
 * Each benchmark runs through the page's entry, `src/packaging/scheme_entry.js`
 * -- the compiler loaded, the program's own code compiled, the program
 * evaluated as a page runs a script that begins with its imports, as a
 * program (`assembleProgram` in lib/r7rs_harness.js) -- four ways: as the source modules and as
 * `dist/scheme.js`, each in Node (`lib/bundle_worker.js`, a process a run) and
 * in headless Chrome (a fresh browser context a run). The count is calibrated
 * once, on the source modules in Node, and the same count runs all four; each
 * figure is the benchmark's own time per iteration. A program that reads its
 * data from a file -- `dynamic`, `read1` -- has none in a browser.
 *
 * It loads the bundle as built, so `npm run build` comes first.
 *
 * Usage:
 *   node benchmarks/run_bundle.js [--profile default|full] [--target SECONDS]
 *                                 [--only name,name] [--no-browser]
 */

import { execFileSync, spawn } from 'child_process';
import path from 'path';
import { fileURLToPath } from 'url';

import { selectBenchmarks } from './r7rs/manifest.js';
import { assembleProgram, calibrate, parseCsvLine } from './lib/r7rs_harness.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const WORKER = path.join(ROOT, 'benchmarks', 'lib', 'bundle_worker.js');
const ENTRIES = { source: 'src/packaging/scheme_entry.js', bundle: 'dist/scheme.js' };

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};
const PROFILE = valueOf('--profile', 'default');
const TARGET = parseFloat(valueOf('--target', '1.0'));
const ONLY = valueOf('--only', null);
const BROWSER = !args.includes('--no-browser');
const BUDGET_MS = 300000;

/**
 * A benchmark's own seconds from what it wrote, or null with why.
 * @param {string} output - What it wrote.
 * @param {string|null} error - What it raised, if anything.
 * @returns {{seconds: (number|null), error: (string|null)}}
 */
function reading(output, error) {
  const parsed = parseCsvLine(output);
  if (parsed.seconds !== null) return { seconds: parsed.seconds, error: null };
  return { seconds: null, error: error ?? (parsed.incorrect ? 'incorrect' : 'no time') };
}

/**
 * Runs a benchmark through an entry in Node, in a process of its own.
 * @param {string} entry - `source` or `bundle`.
 * @param {string} source - The program.
 * @returns {{seconds: (number|null), error: (string|null)}}
 */
function inNode(entry, source) {
  try {
    const request = JSON.stringify({ entry: path.join(ROOT, ENTRIES[entry]), source });
    const result = JSON.parse(execFileSync(process.execPath, [WORKER, request],
      { cwd: ROOT, timeout: BUDGET_MS, maxBuffer: 1 << 26 }).toString());
    return reading(result.output, result.error);
  } catch (e) {
    return { seconds: null, error: String(e.message) };
  }
}

/**
 * The server a page imports the entries from: the repository's files, and a
 * blank page at `/bench.html`.
 */
const SERVER = `
const http = require('http'), fs = require('fs'), path = require('path');
const root = process.argv[1];
const types = { '.html': 'text/html', '.js': 'text/javascript' };
const server = http.createServer((request, response) => {
  const pathname = decodeURIComponent(new URL(request.url, 'http://x').pathname);
  if (pathname === '/bench.html') {
    response.writeHead(200, { 'Content-Type': 'text/html' });
    response.end('<!DOCTYPE html><html><head><meta charset="UTF-8"><title>bench</title></head><body></body></html>');
    return;
  }
  const file = path.join(root, pathname);
  if (!file.startsWith(root) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
    response.writeHead(404);
    response.end();
    return;
  }
  response.writeHead(200, { 'Content-Type': types[path.extname(file)] || 'application/octet-stream' });
  fs.createReadStream(file).pipe(response);
});
server.listen(0, '127.0.0.1', () => process.stdout.write(String(server.address().port) + '\\n'));
`;

/**
 * Serves the repository from a process of its own: this one runs Node's
 * measurements synchronously, and a server whose event loop that blocks
 * leaves a page's module fetches hanging, a run timing out with nothing
 * running.
 * @returns {Promise<{server: Object, port: number}>} The server's process,
 *   and its port.
 */
function serve() {
  const server = spawn(process.execPath, ['-e', SERVER, ROOT], { stdio: ['ignore', 'pipe', 'inherit'] });
  // However this process ends, the server ends with it.
  process.on('exit', () => server.kill());
  return new Promise((resolve) => {
    server.stdout.once('data', (data) => resolve({ server, port: Number(String(data).trim()) }));
  });
}

/**
 * Runs a benchmark through an entry in headless Chrome, in a fresh context.
 * @param {Object} browser - Puppeteer's browser.
 * @param {number} port - The server's port.
 * @param {string} entry - `source` or `bundle`.
 * @param {string} source - The program.
 * @returns {Promise<{seconds: (number|null), error: (string|null)}>}
 */
async function inChrome(browser, port, entry, source) {
  const context = await browser.createBrowserContext();
  try {
    const page = await context.newPage();
    await page.goto(`http://127.0.0.1:${port}/bench.html`);
    const result = await page.evaluate(async (url, program) => {
      const entryModule = await import(url);
      await entryModule.loadCompiler();
      entryModule.setUserCodeCompilation(true);
      const lines = [];
      const log = console.log;
      console.log = (...parts) => lines.push(parts.join(' '));
      let error = null;
      try {
        entryModule.schemeEval(program, { filename: 'benchmark.scm', program: true });
      } catch (e) {
        error = String(e?.message ?? e).slice(0, 120);
      }
      console.log = log;
      return { output: lines.join('\n'), error };
    }, `/${ENTRIES[entry]}`, source);
    return reading(result.output, result.error);
  } catch (e) {
    return { seconds: null, error: String(e.message) };
  } finally {
    await context.close();
  }
}

/**
 * A figure for the report: milliseconds per iteration, or a mark that there
 * is none, why written in full to standard error.
 * @param {{seconds: (number|null), error: (string|null)}} run - The run.
 * @param {number} count - Its iterations.
 * @returns {string}
 */
function figure(run, count) {
  if (run.seconds !== null) return `${(run.seconds / count * 1000).toFixed(3)}`.padStart(10);
  console.error(`  did not finish: ${run.error}`);
  return 'failed'.padStart(10);
}

const only = ONLY === null ? null : ONLY.split(',');
let browser = null;
let served = null;
if (BROWSER) {
  const { default: puppeteer } = await import('puppeteer');
  served = await serve();
  browser = await puppeteer.launch({ headless: true });
}
try {
  console.log('ms per iteration, the tier attached, the same count each way');
  console.log(`${'program'.padEnd(12)} ${'Node src'.padStart(10)} ${'bundle'.padStart(10)}`
    + (BROWSER ? ` ${'Chrome src'.padStart(10)} ${'bundle'.padStart(10)}` : '')
    + '   bundle/src in Node' + (BROWSER ? ', in Chrome' : ''));
  for (const bench of selectBenchmarks(PROFILE)) {
    if (only !== null && !only.includes(bench.name)) continue;
    const at = (count) => assembleProgram(bench.name, bench.params, count, 'scheme-js-4');
    const calibrated = calibrate((count) => inNode('source', at(count)), TARGET);
    if (calibrated.seconds === null) {
      console.log(`${bench.name.padEnd(12)} did not run: ${calibrated.result.error}`);
      continue;
    }
    const count = calibrated.count;
    const source = at(count);
    const runs = [calibrated.result, inNode('bundle', source)];
    if (BROWSER) {
      runs.push(await inChrome(browser, served.port, 'source', source));
      runs.push(await inChrome(browser, served.port, 'bundle', source));
    }
    const ratio = (a, b) => (a.seconds === null || b.seconds === null ? '   -' : (b.seconds / a.seconds).toFixed(2).padStart(4));
    console.log(`${bench.name.padEnd(12)} ${runs.map((run) => figure(run, count)).join(' ')}`
      + `   ${ratio(runs[0], runs[1])}` + (BROWSER ? `  ${ratio(runs[2], runs[3])}` : ''));
  }
} finally {
  if (browser !== null) await browser.close();
  if (served !== null) served.server.kill();
}
