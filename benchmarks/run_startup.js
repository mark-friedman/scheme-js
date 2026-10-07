/**
 * @fileoverview Start-up: how long before Scheme runs, in the CLI and on a
 * page.
 *
 * ## Why this exists
 *
 * Every other benchmark times code once the system has started, and every
 * change that ships more compiled code, or loads more of the system's own
 * Scheme at start, costs something none of them sees: the compiler's image
 * made the CLI 10-13 ms slower to start, the printer's Scheme 3 ms, each
 * measured once by hand. Start-up is what every CLI run and every page pays
 * first, and so an axis of its own.
 *
 * ## What is measured
 *
 * Each figure is the median of `--runs` starts, each in a fresh process or a
 * fresh browser context:
 *
 *  - Node itself, starting and exiting (`node -e ""`): the floor every CLI
 *    figure includes.
 *  - The CLI evaluating `1` (`node repl.js -e 1`), with the tier attached as
 *    it starts by default, and with `--no-compile`.
 *  - In a fresh Node process (`startup/probe.js`), phase by phase: importing
 *    the runtime's modules, which parses the prebuilt tables; making an
 *    interpreter, which starts the library system; importing `(scheme base)`
 *    and `(scheme write)`, restored from their tables as a page restores them;
 *    and starting the compiler.
 *  - A page (`startup/page.html`), in headless Chrome, from the start of its
 *    navigation: until its first `text/scheme` script has run, and until the
 *    compiler, which the bundle fetches once it has started, has arrived. It
 *    loads the bundle as built, so `npm run build` comes first.
 *
 * Usage:
 *   node benchmarks/run_startup.js [--runs N] [--no-browser]
 */

import { execFileSync } from 'child_process';
import fs from 'fs';
import http from 'http';
import path from 'path';
import { fileURLToPath } from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
const RUNS = Number(args.includes('--runs') ? args[args.indexOf('--runs') + 1] : 9);
const BROWSER = !args.includes('--no-browser');

/**
 * The median of some numbers.
 * @param {Array<number>} values - The numbers.
 * @returns {number}
 */
function median(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1 ? sorted[middle] : (sorted[middle - 1] + sorted[middle]) / 2;
}

/**
 * The milliseconds a command takes, from spawning it to its exit.
 * @param {Array<string>} command - Node's arguments.
 * @returns {number}
 */
function wallTime(command) {
  const start = performance.now();
  execFileSync(process.execPath, command, { cwd: ROOT, stdio: 'ignore' });
  return performance.now() - start;
}

/**
 * A line of the report: a label and a median, in milliseconds.
 * @param {string} label - What was measured.
 * @param {Array<number>} samples - The samples.
 */
function report(label, samples) {
  console.log(`  ${label.padEnd(52)} ${median(samples).toFixed(1).padStart(8)} ms`);
}

/**
 * Serves the repository's files, for the page to load the bundle from.
 * @returns {Promise<{server: http.Server, port: number}>}
 */
function serve() {
  const types = { '.html': 'text/html', '.js': 'text/javascript', '.scm': 'text/plain' };
  const server = http.createServer((request, response) => {
    const file = path.join(ROOT, decodeURIComponent(new URL(request.url, 'http://x').pathname));
    if (!file.startsWith(ROOT) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
      response.writeHead(404);
      response.end();
      return;
    }
    response.writeHead(200, { 'Content-Type': types[path.extname(file)] ?? 'application/octet-stream' });
    fs.createReadStream(file).pipe(response);
  });
  return new Promise((resolve) => server.listen(0, '127.0.0.1', () => resolve({ server, port: server.address().port })));
}

/**
 * Loads the page `RUNS` times, each in a fresh browser context, and reads when
 * its Scheme ran and its compiler arrived.
 * @returns {Promise<{ran: Array<number>, compiler: Array<number>}>}
 */
async function pageStarts() {
  const { default: puppeteer } = await import('puppeteer');
  const { server, port } = await serve();
  const browser = await puppeteer.launch({ headless: true });
  const ran = [];
  const compiler = [];
  try {
    for (let i = 0; i < RUNS; i++) {
      const context = await browser.createBrowserContext();
      const page = await context.newPage();
      await page.setCacheEnabled(false);
      await page.goto(`http://127.0.0.1:${port}/benchmarks/startup/page.html`);
      // Polled on a timer: a page in a context of its own is never painted,
      // so the default, polling on animation frames, would never ask.
      await page.waitForFunction(() => window.schemeRan !== undefined && window.compilerArrived !== undefined,
        { polling: 50, timeout: 60000 });
      const times = await page.evaluate(() => [window.schemeRan, window.compilerArrived]);
      ran.push(times[0]);
      compiler.push(times[1]);
      await context.close();
    }
  } finally {
    await browser.close();
    server.close();
  }
  return { ran, compiler };
}

const node = [];
const cli = [];
const cliInterpreted = [];
const phases = { modules: [], interpreter: [], libraries: [], compiler: [] };
for (let i = 0; i < RUNS; i++) {
  node.push(wallTime(['-e', '']));
  cli.push(wallTime(['repl.js', '-e', '1']));
  cliInterpreted.push(wallTime(['repl.js', '--no-compile', '-e', '1']));
  const probe = JSON.parse(execFileSync(process.execPath, ['benchmarks/startup/probe.js'], { cwd: ROOT, encoding: 'utf8' }));
  for (const phase of Object.keys(phases)) phases[phase].push(probe[phase]);
}

console.log(`Start-up: the median of ${RUNS} starts, each in a fresh process or browser context`);
console.log('\nThe CLI, from spawning to exit');
report('Node itself (node -e "")', node);
report('the CLI evaluating 1, the tier attached', cli);
report('the CLI evaluating 1, --no-compile', cliInterpreted);
console.log('\nA fresh process, phase by phase');
report('importing the runtime\'s modules', phases.modules);
report('making an interpreter: the library system starts', phases.interpreter);
report('importing (scheme base) and (scheme write), restored', phases.libraries);
report('starting the compiler', phases.compiler);
if (BROWSER) {
  const { ran, compiler } = await pageStarts();
  console.log('\nA page, in headless Chrome, from the start of its navigation');
  report('its first text/scheme script has run', ran);
  report('the compiler has arrived, and the tier attached', compiler);
}
