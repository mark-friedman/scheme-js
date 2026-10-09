/**
 * @fileoverview Stepping between Scheme and JavaScript in DevTools.
 *
 * Drives DevTools' own front end, in the DevTools window of a headless Chrome
 * tab (devtools_driver.js), over a page whose Scheme and JavaScript call each
 * other (fixtures/boundary.*): breakpoints at Scheme lines and JavaScript
 * lines, steps into, over and out across the boundary both ways, and where
 * DevTools shows each pause. A step stops at the user's code on the other
 * side, never in what is between -- the conversions at the boundary, the
 * interpreter, the primitives, the libraries -- which DevTools skips as
 * ignore-listed. Twice: over the system's modules, which a user ignore-lists
 * by their URLs, and over a bundle built as `npm run build` builds one, whose
 * source map ignore-lists every source in it, with nothing set by hand.
 *
 * Node only: it launches Chrome, through Puppeteer, builds the bundle with
 * Rollup, and serves both, and the repository's files, to Chrome.
 */

import fs from 'fs';
import http from 'http';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';
import { assert, skip } from '../harness/helpers.js';
import { DevTools } from './devtools_driver.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');

/** What each kind of file served is. */
const TYPES = {
  '.html': 'text/html', '.js': 'text/javascript', '.mjs': 'text/javascript', '.json': 'application/json',
  '.scm': 'text/plain', '.sld': 'text/plain'
};

/**
 * Serves the repository's files, and a bundle's under `/bundle/`.
 * @param {string} bundle - The bundle's directory.
 * @returns {Promise<{server: http.Server, port: number}>}
 */
function serve(bundle) {
  const server = http.createServer((request, response) => {
    const pathname = decodeURIComponent(new URL(request.url, 'http://x').pathname);
    const [root, relative] = pathname.startsWith('/bundle/') ? [bundle, pathname.slice('/bundle'.length)] : [ROOT, pathname];
    const file = path.join(root, relative);
    if (!file.startsWith(root) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
      response.writeHead(404);
      response.end();
      return;
    }
    response.writeHead(200, { 'Content-Type': TYPES[path.extname(file)] ?? 'application/octet-stream' });
    fs.createReadStream(file).pipe(response);
  });
  return new Promise((resolve) => server.listen(0, '127.0.0.1', () => resolve({ server, port: server.address().port })));
}

/**
 * Builds the page's bundle as `npm run build` does (rollup.config.js), into a
 * directory of its own.
 * @returns {Promise<string>} The directory.
 */
async function buildBundle() {
  const { rollup } = await import('rollup');
  const { default: config } = await import('../../rollup.config.js');
  const page = config.find((options) => options.output.entryFileNames === 'scheme.js');
  const bundle = await rollup({
    input: path.join(ROOT, page.input), plugins: page.plugins, external: page.external,
    preserveEntrySignatures: page.preserveEntrySignatures, onwarn: () => {}
  });
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-devtools-'));
  await bundle.write({ ...page.output, dir });
  await bundle.close();
  return dir;
}

/**
 * A pause's place as a test compares it: the file's name and the line, and
 * whether DevTools ignore-lists it.
 * @param {{url: string, line: number, ignored: boolean}|null} place - Where.
 * @returns {string|null}
 */
function shown(place) {
  if (place === null) return null;
  const file = pathOf(place.url);
  return `${file.slice(file.lastIndexOf('/') + 1)}:${place.line}${place.ignored ? ' (ignored)' : ''}`;
}

/**
 * A URL's path, without its query: the page's own URL names its bundle in
 * one.
 * @param {string} url - The URL.
 * @returns {string}
 */
function pathOf(url) {
  return url.split(/[?#]/)[0];
}

/**
 * The line of a fixture that holds a text, one-based.
 * @param {string} file - The fixture's name.
 * @param {string} text - The text.
 * @returns {number}
 */
function lineOf(file, text) {
  return fs.readFileSync(path.join(ROOT, 'tests/devtools/fixtures', file), 'utf8')
    .split('\n').findIndex((line) => line.includes(text)) + 1;
}

/**
 * Sets a breakpoint, starts a call on the page, notes the locals DevTools
 * shows where it pauses, and takes steps from there, as a user of DevTools
 * would, then removes the breakpoint and lets the call finish.
 * @param {DevTools} devTools - The front end.
 * @param {Object} page - The tab.
 * @param {string} url - The breakpoint's source.
 * @param {number} line - Its line.
 * @param {string} expression - The call, as JavaScript.
 * @param {Array<string>} kinds - The steps.
 * @returns {Promise<{bound: number, at: (string|null), locals: (Array<string>|null),
 *   places: Array<string|null>}>} How many places the breakpoint was bound to,
 *   where the call paused, the names of the locals shown there, and where each
 *   step paused, while each did.
 */
async function session(devTools, page, url, line, expression, kinds) {
  const bound = await devTools.breakpoint(url, line);
  const seen = await devTools.pauses();
  const running = page.evaluate(expression).catch(() => null);
  const at = bound > 0 ? await devTools.pauseAfter(seen) : null;
  const locals = at === null ? null : await devTools.locals();
  const places = [];
  for (const kind of at === null ? [] : kinds) {
    const place = await devTools.step(kind);
    places.push(shown(place));
    if (place === null) break;
  }
  await devTools.resume();
  await running;
  return { bound, at: shown(at), locals, places };
}

/**
 * Opens a page whose Scheme and JavaScript call each other, and its DevTools.
 * @param {Object} browser - Puppeteer's browser, launched with `devtools`.
 * @param {string} url - The page's URL.
 * @returns {Promise<{page: Object, devTools: DevTools}>}
 */
async function open(browser, url) {
  const page = await browser.newPage();
  await page.goto(url);
  await ready(page);
  return { page, devTools: await DevTools.open(browser, page) };
}

/**
 * Waits until a page says it is ready. Asked rather than waited for:
 * Puppeteer's own wait did not see a tab whose DevTools window was in front.
 * @param {Object} page - The tab.
 * @returns {Promise<void>}
 */
async function ready(page) {
  for (let waited = 0; !(await page.evaluate(() => window.ready === true)); waited += 100) {
    if (waited > 60000) throw new Error(`${page.url()} did not become ready`);
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
}

/**
 * The tests of the switch that has every procedure compiled as it is
 * defined: a procedure is stepped into only compiled, and the tier compiles
 * one only on its second call, so while a page is debugged in DevTools every
 * procedure is compiled before its first. Turned on by the page's URL, or by
 * a call in the console, which the tab remembers.
 * @param {Object} logger - Test logger.
 * @param {string} over - What the system is loaded as, for the tests' names.
 * @param {Object} browser - Puppeteer's browser, launched with `devtools`.
 * @param {string} fixtures - The fixtures' URL.
 * @param {string} query - The page's query string for what the system is
 *   loaded as, ending in `&` if it is not empty.
 * @param {string|null} ignore - A pattern to ignore-list the system's code by,
 *   or null if its source map does.
 * @returns {Promise<void>}
 */
async function switchTests(logger, over, browser, fixtures, query, ignore) {
  const placing = `${fixtures}placing.scm`;
  const inside = lineOf('placing.scm', '(+ (* x 2)');
  const pausesInside = async (page, devTools) => {
    await devTools.breakpoint(placing, inside);
    const seen = await devTools.pauses();
    const running = page.evaluate("window.call('first-call', 1)").catch(() => null);
    const at = await devTools.pauseAfter(seen, 10000);
    await devTools.resume();
    return [shown(at), await running];
  };
  logger.title(`DevTools, over ${over} - every procedure compiled before its first call`);
  {
    const { page, devTools } = await open(browser, `${fixtures}boundary.html?${query}scheme-devtools`);
    if (ignore) await devTools.ignore(ignore);
    assert(logger, 'with scheme-devtools in its URL, a procedure the page never called is compiled',
      await page.evaluate("window.compiledNow('first-call')"), true);
    assert(logger, 'and a breakpoint in it pauses its first call', await pausesInside(page, devTools),
      [`placing.scm:${inside}`, 3]);
    await page.close();
  }
  {
    const { page, devTools } = await open(browser, `${fixtures}boundary.html?${query}tab=console`);
    if (ignore) await devTools.ignore(ignore);
    const before = await page.evaluate("window.compiledNow('first-call')");
    const said = await page.evaluate('schemeJS.devtools()');
    assert(logger, 'without it, a procedure never called is not compiled; turned on in the console, the page says so',
      [before, typeof said === 'string' && said.length > 0], [false, true]);
    assert(logger, "and compiles the page's procedures at once, so a breakpoint in one never called pauses its first call",
      [await page.evaluate("window.compiledNow('first-call')"), ...await pausesInside(page, devTools)],
      [true, `placing.scm:${inside}`, 3]);
    await page.reload();
    await ready(page);
    const remembered = await page.evaluate("window.compiledNow('first-call')");
    await page.evaluate('schemeJS.devtools(false)');
    await page.reload();
    await ready(page);
    assert(logger, 'the tab remembers it through a reload, until it is turned off',
      [remembered, await page.evaluate("window.compiledNow('first-call')")], [true, false]);
    await page.close();
  }
}

/**
 * Steps between the page's Scheme and JavaScript, and checks where DevTools
 * shows each pause.
 * @param {Object} logger - Test logger.
 * @param {string} over - What the system is loaded as, for the tests' names.
 * @param {DevTools} devTools - The front end.
 * @param {Object} page - The tab.
 * @param {string} fixtures - The fixtures' URL.
 * @returns {Promise<void>}
 */
async function steppingTests(logger, over, devTools, page, fixtures) {
  const scm = `${fixtures}boundary.scm`;
  const js = `${fixtures}boundary.js`;
  const placing = `${fixtures}placing.scm`;
  // Where the page's own JavaScript calls a Scheme procedure.
  const caller = `boundary.html:${lineOf('boundary.html', 'window.call =')}`;
  const at = (text) => `placing.scm:${lineOf('placing.scm', text)}`;

  logger.title(`DevTools, over ${over} - the page`);
  assert(logger, 'the procedures stepped into are compiled',
    await page.evaluate('JSON.stringify(window.compiled)'),
    JSON.stringify({
      'scheme-calls-js': true, 'scheme-called-from-js': true, 'scheme-round-trip': true,
      classify: true, assigning: true, counting: true, summing: true
    }));
  assert(logger, 'DevTools lists the Scheme file the compiled code is mapped to', await devTools.source(scm), true);

  logger.title(`DevTools, over ${over} - from Scheme into JavaScript, and back`);
  {
    const { bound, at, locals, places } = await session(devTools, page, scm, 8,
      "window.call('scheme-calls-js', 3)", ['stepInto', 'stepOut', 'stepOver']);
    assert(logger, 'a breakpoint at a Scheme call is bound', bound > 0, true);
    assert(logger, 'the program pauses at it', at, 'boundary.scm:8');
    // Beside them, the compiler's own: its temporaries and the like, all named
    // with a `$`, and JavaScript's `arguments`.
    assert(logger, "DevTools shows the procedure's locals by the names they were written with",
      locals?.filter((name) => !name.startsWith('$') && name !== 'arguments'), ['n', 'm', 'r']);
    // A step out comes back after the call: DevTools stepped into it skipping
    // what is mapped to the call's expression, and the engine skips the same
    // on the way out, as it does for any code a source map places.
    assert(logger, 'a step into goes to the JavaScript the Scheme calls; a step out comes back to the Scheme, '
      + 'after the call; a step over goes out of it, to the JavaScript that called it',
      places, ['boundary.js:13', 'boundary.scm:9', caller]);
  }

  logger.title(`DevTools, over ${over} - from JavaScript into Scheme, and back`);
  {
    const { bound, at, places } = await session(devTools, page, js, 24, "window.call('scheme-round-trip', 5)",
      ['stepInto', 'stepOut']);
    assert(logger, 'a breakpoint at a JavaScript call is bound', bound > 0, true);
    assert(logger, 'the program pauses at it', at, 'boundary.js:24');
    assert(logger, 'a step into goes to the Scheme the JavaScript calls, and a step out comes back to the JavaScript',
      places, ['boundary.scm:12', 'boundary.js:25']);
  }

  logger.title(`DevTools, over ${over} - within Scheme`);
  {
    const { bound, at, places } = await session(devTools, page, scm, 7, "window.call('scheme-calls-js', 3)",
      ['stepOver']);
    assert(logger, 'a breakpoint at a Scheme expression that calls no procedure is bound', bound > 0, true);
    assert(logger, 'the program pauses at it', at, 'boundary.scm:7');
    assert(logger, 'a step over goes to the next expression', places, ['boundary.scm:8']);
  }

  // A line of Scheme whose code calls nothing has code a breakpoint is bound
  // to, placed at its form, and a form's code is placed at its start once:
  // what follows the first line of it, an `if` after its test's call, is no
  // stop of its own, which would seem a step back.
  logger.title(`DevTools, over ${over} - where code that calls nothing is placed`);
  {
    const { bound, at: paused, places } = await session(devTools, page, placing, lineOf('placing.scm', '(cond'),
      "window.call('classify', 5)", ['stepOver', 'stepOver', 'stepOver']);
    assert(logger, 'a breakpoint at a cond is bound, and the program pauses at its first test',
      [bound > 0, paused], [true, at('(cond')]);
    assert(logger, 'a step over goes to each test in turn, then to the constant the cond gives, then out',
      places, [at('((= n 0)'), at("(else 'positive)"), caller]);
  }
  {
    const { bound, at: paused, places } = await session(devTools, page, placing, lineOf('placing.scm', '(let ((count 0))'),
      "window.call('assigning', true)", ['stepOver', 'stepOver', 'stepOver']);
    assert(logger, 'a breakpoint at a binding to a constant is bound, and the program pauses at it',
      [bound > 0, paused], [true, at('(let ((count 0))')]);
    assert(logger, "a step over goes to a system macro's use, when, then to the set! it was given, then out",
      places, [at('(when flag'), at('(set! count 1)'), caller]);
  }
  {
    const { bound, at: paused, places } = await session(devTools, page, placing, lineOf('placing.scm', '(do (('),
      "window.call('counting', 2)", Array(8).fill('stepOver'));
    assert(logger, 'a breakpoint at a do is bound, and the program pauses at it', [bound > 0, paused],
      [true, at('(do ((')]);
    assert(logger, "a step over goes to the do's test, then each step, and the test again, in turn, then out",
      places, [at('((= i n)'), at('(do (('), at("(acc '()"), at('((= i n)'), at('(do (('), at("(acc '()"),
        at('((= i n)'), caller]);
  }
  {
    const loop = lineOf('placing.scm', '(let loop');
    const { bound, at: paused, places } = await session(devTools, page, placing, loop,
      "window.call('summing', window.call('list', 1, 2))", Array(12).fill('stepOver'));
    assert(logger, 'a breakpoint at a named let is bound, and the program pauses at it', [bound > 0, paused],
      [true, at('(let loop')]);
    assert(logger, "its loop's iterations each go back to its test, not to its start, and then out",
      [places.filter((place) => place === at('(let loop')).length, places.filter((place) => place === at('(if (null?')).length,
        places.at(-1)],
      [0, 3, caller]);
  }

  logger.title(`DevTools, over ${over} - never in the system`);
  assert(logger, "no pause was in the system's code, nor in code DevTools could not place in a source",
    (await devTools.all()).filter((place) => place === null || place.ignored || place.url.startsWith('scheme:')
      || pathOf(place.url).includes('/src/') || pathOf(place.url).includes('/bundle/')).map(shown),
    []);
}

/**
 * Runs the stepping tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runDevToolsSteppingTests(logger) {
  let puppeteer;
  try {
    ({ default: puppeteer } = await import('puppeteer'));
  } catch (e) {
    skip(logger, 'DevTools - stepping between Scheme and JavaScript', 'needs Puppeteer, to drive Chrome');
    return;
  }
  const bundle = await buildBundle();
  const { server, port } = await serve(bundle);
  const browser = await puppeteer.launch({ headless: true, devtools: true });
  try {
    const fixtures = `http://127.0.0.1:${port}/tests/devtools/fixtures/`;
    {
      const { page, devTools } = await open(browser, `${fixtures}boundary.html`);
      await devTools.ignore(`^http://127\\.0\\.0\\.1:${port}/src/`);
      await steppingTests(logger, "the system's modules, ignore-listed by their URLs", devTools, page, fixtures);
    }
    {
      const { page, devTools } = await open(browser, `${fixtures}boundary.html?entry=/bundle/scheme.js`);
      await steppingTests(logger, 'the bundle, ignore-listed by its source map', devTools, page, fixtures);
    }
    await switchTests(logger, "the system's modules", browser, fixtures, '', `^http://127\\.0\\.0\\.1:${port}/src/`);
    await switchTests(logger, 'the bundle', browser, fixtures, 'entry=/bundle/scheme.js&', null);
    logger.title('DevTools - a page whose Scheme is in its own scripts');
    {
      // Whether the procedure the page's first script defines was compiled
      // as it was defined, the compiler already there.
      const at = async (query) => {
        const page = await browser.newPage();
        await page.goto(`${fixtures}scripts.html${query}`);
        await ready(page);
        const compiled = await page.evaluate(() => window.compiledAtOnce);
        await page.close();
        return compiled;
      };
      assert(logger, 'its first script runs before the compiler has arrived, and defines interpreted', await at(''), false);
      assert(logger, 'but with scheme-devtools in its URL, after, and every procedure is compiled as it is defined',
        await at('?scheme-devtools'), true);
    }
  } finally {
    await browser.close();
    server.close();
    fs.rmSync(bundle, { recursive: true, force: true });
  }
}
