/**
 * @fileoverview A program compiled ahead of time, as one file, against the
 * same program run as today: its size, and how long it takes to start and
 * finish, under Node and on a page.
 *
 * ## What is compared
 *
 * Each program is built with `node repl.js --build PROGRAM -o OUTPUT`, one ES
 * module of the program, what it reaches of each library, the runtime and the
 * primitives, and run as a user runs it; and the same program is run as today:
 *
 *  - Size: the module, as written and gzipped, against what a page loads
 *    today before any of its Scheme runs, `dist/scheme.js` and
 *    `dist/scheme-html.js`, and the compiler it fetches after,
 *    `dist/scheme_compiler.js`. The module is not minified; neither is the
 *    bundle.
 *  - Under Node, from spawning to exit: `node OUTPUT` against
 *    `node repl.js PROGRAM`.
 *  - On a page, in headless Chrome, from the start of the navigation until
 *    the program has finished: a page importing the module, against a page
 *    loading the bundle and running the program as a `text/scheme` script.
 *
 * Each time is the median of `--runs` runs, each in a fresh process or a fresh
 * browser context. The programs are small, as a page's often are, and each
 * does one of the things a program does: prints, raises an error nobody
 * handles, makes records, uses a parameter, uses JavaScript, computes.
 *
 * Usage:
 *   node benchmarks/run_ahead_startup.js [--runs N] [--no-browser]
 */

import { execFileSync, spawnSync } from 'child_process';
import fs from 'fs';
import http from 'http';
import os from 'os';
import path from 'path';
import zlib from 'zlib';
import { fileURLToPath } from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
const RUNS = Number(args.includes('--runs') ? args[args.indexOf('--runs') + 1] : 9);
const BROWSER = !args.includes('--no-browser');

/**
 * The programs, by name.
 * @type {Object<string, string>}
 */
const PROGRAMS = {
  prints: `(import (scheme base) (scheme write))
(display "hello, world")
(newline)
(write (list 1 "two" #\\3 4.5 'five (vector 6 7)))
(newline)
`,
  raises: `(import (scheme base) (scheme write))
(display "before")
(newline)
(error "stopped:" 42)
`,
  records: `(import (scheme base) (scheme write))
(define-record-type point (make-point x y) point? (x point-x) (y point-y))
(define (distance2 p) (+ (* (point-x p) (point-x p)) (* (point-y p) (point-y p))))
(define points (map (lambda (i) (make-point i (- 10 i))) '(0 1 2 3 4 5)))
(write (map distance2 points))
(newline)
`,
  parameters: `(import (scheme base) (scheme write))
(define radix (make-parameter 10 (lambda (r) (if (memv r '(2 8 10 16)) r (error "bad radix" r)))))
(write (number->string 255 (radix)))
(radix 16)
(write (number->string 255 (radix)))
(newline)
`,
  interop: `(import (scheme base) (scheme write) (scheme-js interop))
(define obj (js-obj "name" "scheme" "size" 3))
(js-set! obj "size" 4)
(write (list (js-ref obj "name") (js-ref obj "size")
             (vector->list (js-invoke (js-eval "[1, 2, 3]") "map" (lambda (x . rest) (* x x))))))
(newline)
`,
  computes: `(import (scheme base) (scheme write))
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(write (fib 25))
(newline)
`
};

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
 * The milliseconds Node takes to run a command, from spawning it to its exit.
 * @param {Array<string>} command - Node's arguments.
 * @returns {number}
 */
function wallTime(command) {
  const start = performance.now();
  spawnSync(process.execPath, command, { cwd: ROOT, stdio: 'ignore' });
  return performance.now() - start;
}

/**
 * A file's size in KB, as written and gzipped.
 * @param {Array<string>} files - The files, taken together.
 * @returns {string}
 */
function size(files) {
  const bytes = Buffer.concat(files.map((file) => fs.readFileSync(file)));
  return `${(bytes.length / 1024).toFixed(0)} KB, ${(zlib.gzipSync(bytes).length / 1024).toFixed(0)} KB gzipped`;
}

/**
 * Serves the repository's `dist/` and a directory of programs and pages.
 * @param {string} dir - The programs' directory, served at the root.
 * @returns {Promise<{server: http.Server, port: number}>}
 */
function serve(dir) {
  const types = { '.html': 'text/html', '.js': 'text/javascript', '.mjs': 'text/javascript', '.scm': 'text/plain' };
  const server = http.createServer((request, response) => {
    const pathname = decodeURIComponent(new URL(request.url, 'http://x').pathname);
    const file = pathname.startsWith('/dist/') ? path.join(ROOT, pathname) : path.join(dir, pathname);
    if (!fs.existsSync(file) || !fs.statSync(file).isFile()) {
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
 * Writes the two pages of a program: one importing its module, one running
 * it with the bundle, each setting `window.done` when the program has
 * finished.
 * @param {string} dir - The programs' directory.
 * @param {string} name - The program's name.
 */
function writePages(dir, name) {
  fs.writeFileSync(path.join(dir, `${name}.ahead.html`), `<!DOCTYPE html>
<html><head><meta charset="UTF-8"><title>${name}, compiled ahead of time</title></head><body>
<script type="module">
  await import('./${name}.mjs');
  window.done = performance.now();
</script>
</body></html>
`);
  fs.writeFileSync(path.join(dir, `${name}.today.html`), `<!DOCTYPE html>
<html><head><meta charset="UTF-8"><title>${name}, as today</title></head><body>
<script type="module" src="/dist/scheme-html.js"></script>
<script type="text/scheme" src="./${name}.scm"></script>
<script type="text/scheme">
  (js-set! (js-eval "window") "done" (js-invoke (js-eval "performance") "now"))
</script>
</body></html>
`);
}

/**
 * Loads a page `RUNS` times, each in a fresh browser context, and reads when
 * its program finished.
 * @param {Object} browser - Puppeteer's browser.
 * @param {string} url - The page.
 * @returns {Promise<Array<number>>}
 */
async function pageTimes(browser, url) {
  const times = [];
  for (let i = 0; i < RUNS; i++) {
    const context = await browser.createBrowserContext();
    const page = await context.newPage();
    await page.setCacheEnabled(false);
    await page.goto(url);
    // Polled on a timer: a page in a context of its own is never painted.
    await page.waitForFunction(() => window.done !== undefined, { polling: 20, timeout: 60000 });
    times.push(await page.evaluate(() => window.done));
    await context.close();
  }
  return times;
}

const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-ahead-startup-'));
try {
  const today = ['dist/scheme.js', 'dist/scheme-html.js'].map((f) => path.join(ROOT, f));
  console.log(`A program compiled ahead of time as one file, against today: medians of ${RUNS}`);
  console.log(`\nToday a page loads ${size(today)} before its Scheme runs, `
    + `and fetches the compiler after, ${size([path.join(ROOT, 'dist/scheme_compiler.js')])}.`);
  const built = {};
  for (const [name, source] of Object.entries(PROGRAMS)) {
    const program = path.join(dir, `${name}.scm`);
    const output = path.join(dir, `${name}.mjs`);
    fs.writeFileSync(program, source);
    const start = performance.now();
    execFileSync(process.execPath, ['repl.js', '--build', program, '-o', output], { cwd: ROOT, stdio: 'pipe' });
    built[name] = { program, output, buildMs: performance.now() - start };
    writePages(dir, name);
  }
  console.log('\nprogram       one file                       build      node: one file  node repl.js');
  for (const [name, { program, output, buildMs }] of Object.entries(built)) {
    const ahead = [];
    const cli = [];
    for (let i = 0; i < RUNS; i++) {
      ahead.push(wallTime([output]));
      cli.push(wallTime(['repl.js', program]));
    }
    console.log(`${name.padEnd(12)}  ${size([output]).padEnd(30)} ${(buildMs / 1000).toFixed(1).padStart(4)} s`
      + `  ${median(ahead).toFixed(1).padStart(10)} ms  ${median(cli).toFixed(1).padStart(9)} ms`);
  }
  if (BROWSER) {
    const { default: puppeteer } = await import('puppeteer');
    const { server, port } = await serve(dir);
    const browser = await puppeteer.launch({ headless: true });
    try {
      console.log('\nOn a page, from the start of its navigation until the program has finished');
      console.log('program       one file      today');
      for (const name of Object.keys(built)) {
        const ahead = await pageTimes(browser, `http://127.0.0.1:${port}/${name}.ahead.html`);
        const now = await pageTimes(browser, `http://127.0.0.1:${port}/${name}.today.html`);
        console.log(`${name.padEnd(12)}  ${median(ahead).toFixed(1).padStart(8)} ms  ${median(now).toFixed(1).padStart(8)} ms`);
      }
    } finally {
      await browser.close();
      server.close();
    }
  }
} finally {
  fs.rmSync(dir, { recursive: true, force: true });
}
