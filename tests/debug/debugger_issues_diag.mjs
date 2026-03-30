/**
 * Diagnostic script: reproduces the 4 debugger issues and captures console output.
 * Run: node tests/debug/debugger_issues_diag.mjs
 */
import puppeteer from 'puppeteer';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';
import http from 'http';
import fs from 'fs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectRoot = resolve(__dirname, '../..');

const SOURCE_URL = 'scheme://scheme-sources/app_test.scm';
// Line 8 is the `let` inside setup-generate-button
const BP_LINE_LET = 8;
// Line 16 is `(console.log "App loaded" ...)`
const BP_LINE_LAST = 16;

function startServer(root, port) {
  return new Promise((resolve) => {
    const mimeTypes = {
      '.html': 'text/html', '.js': 'application/javascript',
      '.mjs': 'application/javascript', '.css': 'text/css',
      '.scm': 'text/plain', '.sld': 'text/plain', '.json': 'application/json',
    };
    const server = http.createServer((req, res) => {
      let filePath = root + decodeURIComponent(req.url.split('?')[0]);
      if (filePath.endsWith('/')) filePath += 'index.html';
      const ext = filePath.match(/\.[^.]+$/)?.[0] || '';
      try {
        const data = fs.readFileSync(filePath);
        res.writeHead(200, { 'Content-Type': mimeTypes[ext] || 'application/octet-stream' });
        res.end(data);
      } catch {
        res.writeHead(404);
        res.end('Not found: ' + filePath);
      }
    });
    server.listen(port, () => resolve(server));
  });
}

async function waitForPause(page, timeout = 10000) {
  await page.waitForFunction(() => window._pauseEvents?.length > 0, { timeout });
  return page.evaluate(() => window._pauseEvents[window._pauseEvents.length - 1]);
}

/**
 * Waits for a pause or resume event after a step command.
 * Note: events are cleared BEFORE calling step, then we wait for length > 0.
 * This avoids a race where the step's microtask fires before we read startCounts.
 */
async function waitForPauseOrResume(page, timeout = 5000) {
  await page.waitForFunction(
    () => window._pauseEvents.length > 0 || window._resumeEvents.length > 0,
    { timeout }
  );
  const pauses = await page.evaluate(() => window._pauseEvents.slice(0));
  const resumeCount = await page.evaluate(() => window._resumeEvents.length);
  return { pauses, resumeCount };
}

async function main() {
  const PORT = 9877;
  const server = await startServer(projectRoot, PORT);
  const browser = await puppeteer.launch({ headless: 'new' });

  // ================================================================
  // TEST 1: Breakpoint at line 8 (let inside function) — Issues 1, 2, 3
  // ================================================================
  console.log('=== TEST 1: Breakpoint at let (line 8) — Issues 1, 2, 3 ===\n');

  const page1 = await browser.newPage();
  const msgs1 = [];
  page1.on('console', msg => msgs1.push(msg.text()));
  page1.on('pageerror', err => console.log('[PAGE ERROR]', err.message));

  await page1.evaluateOnNewDocument((url, line) => {
    globalThis.__SCHEME_JS_DEBUG = true;
    globalThis.__SCHEME_JS_PANELCONNECTED = true;
    globalThis.__SCHEME_JS_BREAKPOINTS = [{ url, line }];
    window._pauseEvents = [];
    window._resumeEvents = [];
    window.addEventListener('scheme-debug-paused', e => window._pauseEvents.push(e.detail));
    window.addEventListener('scheme-debug-resumed', () => window._resumeEvents.push(Date.now()));
  }, SOURCE_URL, BP_LINE_LET);

  page1.goto(`http://localhost:${PORT}/tests/debug/debugger_issues_test.html`).catch(() => {});

  try {
    const pause1 = await waitForPause(page1);
    console.log(`Paused at: ${pause1.source?.filename}:${pause1.source?.line} reason=${pause1.reason}`);
  } catch {
    console.log('ERROR: No pause. Console output:');
    msgs1.forEach(m => console.log('  ', m));
    await page1.close();
    await browser.close();
    server.close();
    return;
  }

  // --- ISSUE 2: Call stack ---
  console.log('\n--- Issue 2: Call Stack ---');
  const stack = await page1.evaluate(() => __schemeDebug.getStack());
  console.log(`getStack() returned ${stack.length} frames:`);
  for (let i = 0; i < stack.length; i++) {
    const f = stack[i];
    const src = f.source ? `${f.source.filename}:${f.source.line}` : 'null';
    const cs = f.callSiteSource ? `${f.callSiteSource.filename}:${f.callSiteSource.line}` : 'null';
    console.log(`  [${i}] ${f.name} src=${src} callSite=${cs} ${f._synthetic ? '[SYNTHETIC]' : ''}`);
  }

  // --- ISSUE 1: Variables ---
  console.log('\n--- Issue 1: Variables ---');
  for (let i = 0; i < stack.length; i++) {
    const locals = await page1.evaluate((idx) => __schemeDebug.getLocals(idx), i);
    console.log(`getLocals(${i}) [${stack[i].name}]: ${locals.length} variables`);
    for (const v of locals) {
      console.log(`  [${v.scope}] ${v.name} = ${v.value} (${v.type})`);
    }
  }

  // --- ISSUE 3: Step-into then step-over ---
  console.log('\n--- Issue 3: Step-into from breakpoint, then step-over ---');

  // Step into
  console.log('stepInto()...');
  await page1.evaluate(() => { window._pauseEvents = []; window._resumeEvents = []; __schemeDebug.stepInto(); });
  try {
    const r = await waitForPauseOrResume(page1);
    if (r.pauses.length > 0) {
      const p = r.pauses[0];
      console.log(`  -> Paused at ${p.source?.filename}:${p.source?.line}:${p.source?.column} reason=${p.reason}`);
    } else {
      console.log(`  -> Resumed (no pause). Resume count: ${r.resumeCount}`);
    }
  } catch { console.log('  -> Timeout'); }

  // Step into again
  console.log('stepInto() again...');
  await page1.evaluate(() => { window._pauseEvents = []; window._resumeEvents = []; __schemeDebug.stepInto(); });
  try {
    const r = await waitForPauseOrResume(page1);
    if (r.pauses.length > 0) {
      const p = r.pauses[0];
      console.log(`  -> Paused at ${p.source?.filename}:${p.source?.line}:${p.source?.column} reason=${p.reason}`);
    } else {
      console.log(`  -> Resumed (no pause). Resume count: ${r.resumeCount}`);
    }
  } catch { console.log('  -> Timeout'); }

  // Step over
  console.log('stepOver()...');
  await page1.evaluate(() => { window._pauseEvents = []; window._resumeEvents = []; __schemeDebug.stepOver(); });
  try {
    const r = await waitForPauseOrResume(page1);
    if (r.pauses.length > 0) {
      const p = r.pauses[0];
      console.log(`  -> Paused at ${p.source?.filename}:${p.source?.line}:${p.source?.column} reason=${p.reason}`);
    } else {
      console.log(`  -> Resumed (no pause). Resume count: ${r.resumeCount}`);
    }
  } catch { console.log('  -> Timeout'); }

  // Print DBG messages
  console.log('\n[DBG] messages:');
  msgs1.filter(m => m.startsWith('[DBG')).forEach(m => console.log('  ' + m));

  await page1.close();

  // ================================================================
  // TEST 2: Breakpoint at line 17 (console.log) — Issue 4
  // ================================================================
  console.log('\n\n=== TEST 2: Breakpoint at console.log (line 17) — Issue 4 ===\n');

  const page2 = await browser.newPage();
  const msgs2 = [];
  page2.on('console', msg => msgs2.push(msg.text()));
  page2.on('pageerror', err => console.log('[PAGE ERROR]', err.message));

  await page2.evaluateOnNewDocument((url, line) => {
    globalThis.__SCHEME_JS_DEBUG = true;
    globalThis.__SCHEME_JS_PANELCONNECTED = true;
    globalThis.__SCHEME_JS_BREAKPOINTS = [{ url, line }];
    window._pauseEvents = [];
    window._resumeEvents = [];
    window.addEventListener('scheme-debug-paused', e => window._pauseEvents.push(e.detail));
    window.addEventListener('scheme-debug-resumed', () => window._resumeEvents.push(Date.now()));
  }, SOURCE_URL, BP_LINE_LAST);

  page2.goto(`http://localhost:${PORT}/tests/debug/debugger_issues_test.html`).catch(() => {});

  try {
    const pause2 = await waitForPause(page2);
    console.log(`Paused at: ${pause2.source?.filename}:${pause2.source?.line} reason=${pause2.reason}`);
  } catch {
    console.log('ERROR: No pause at line 17. Console:');
    msgs2.forEach(m => console.log('  ', m));
    await page2.close();
    await browser.close();
    server.close();
    return;
  }

  // Step into sub-expressions
  for (let step = 1; step <= 5; step++) {
    console.log(`stepInto() #${step}...`);
    await page2.evaluate(() => { window._pauseEvents = []; window._resumeEvents = []; __schemeDebug.stepInto(); });
    try {
      const r = await waitForPauseOrResume(page2);
      if (r.pauses.length > 0) {
        const p = r.pauses[0];
        console.log(`  -> Paused at ${p.source?.filename}:${p.source?.line}:${p.source?.column} reason=${p.reason} lastResult=${p.lastResult || ''}`);
      } else {
        console.log(`  -> Resumed (no pause). Resume count: ${r.resumeCount}`);
        break;
      }
    } catch {
      console.log('  -> Timeout');
      break;
    }
  }

  // Print DBG messages
  console.log('\n[DBG] messages:');
  msgs2.filter(m => m.startsWith('[DBG')).forEach(m => console.log('  ' + m));

  await page2.close();
  await browser.close();
  server.close();
  console.log('\nDone.');
}

main().catch(e => { console.error(e); process.exit(1); });
