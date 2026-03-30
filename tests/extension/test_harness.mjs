/**
 * @fileoverview Test harness for Puppeteer E2E extension tests.
 *
 * Provides: static file server, assertion helpers, constants,
 * browser launch with extension, and extensionId discovery.
 */

import puppeteer from 'puppeteer';
import { resolve, dirname, extname, join } from 'path';
import { fileURLToPath } from 'url';
import { createServer } from 'http';
import { readFile, stat } from 'fs/promises';
import { execSync } from 'child_process';

const __dirname = dirname(fileURLToPath(import.meta.url));

// =========================================================================
// macOS Chrome focus-stealing fix
// =========================================================================

/**
 * Patches Chrome for Testing's Info.plist to add LSBackgroundOnly=true.
 * This tells macOS to treat Chrome as a background-only app — no Dock icon,
 * no focus stealing, no activation when windows are created.
 *
 * Only needed on macOS. Modifies the CfT bundle in-place (safe since it's
 * only used for testing and managed by Puppeteer's cache).
 *
 * @param {string} chromePath - Path to the Chrome executable
 */
async function patchChromeInfoPlist(chromePath) {
  // Chrome path: .../Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing
  // Info.plist:  .../Google Chrome for Testing.app/Contents/Info.plist
  const contentsDir = resolve(dirname(chromePath), '..');
  const plistPath = join(contentsDir, 'Info.plist');

  try {
    // Check if LSBackgroundOnly is already set
    const result = execSync(
      `/usr/libexec/PlistBuddy -c "Print :LSBackgroundOnly" "${plistPath}" 2>&1`,
      { encoding: 'utf8', timeout: 5000 }
    ).trim();
    if (result === 'true') return; // Already patched
  } catch {
    // Key doesn't exist yet — add it
  }

  try {
    execSync(
      `/usr/libexec/PlistBuddy -c "Add :LSBackgroundOnly bool true" "${plistPath}" 2>/dev/null || ` +
      `/usr/libexec/PlistBuddy -c "Set :LSBackgroundOnly true" "${plistPath}"`,
      { timeout: 5000 }
    );
    console.log('[test harness] Patched Chrome Info.plist with LSBackgroundOnly=true');
  } catch (e) {
    console.warn('[test harness] Could not patch Info.plist:', e.message);
  }
}
export const projectRoot = resolve(__dirname, '../..');
export const extensionPath = resolve(projectRoot, 'extension');
export const port = parseInt(process.env.TEST_PORT || '8081', 10);

export const BASE_URL = `http://127.0.0.1:${port}`;
export const TEST_PAGE = `${BASE_URL}/tests/debug/puppeteer_test_page.html`;
export const JS_TEST_PAGE = `${BASE_URL}/tests/debug/puppeteer_js_test_page.html`;
export const INLINE_URL = 'scheme://inline-scripts/script-0.scm';
export const EXTERNAL_URL = 'scheme://scheme-sources/manual_script.scm';

// =========================================================================
// Simple static file server
// =========================================================================

const MIME_TYPES = {
  '.html': 'text/html',
  '.js': 'application/javascript',
  '.mjs': 'application/javascript',
  '.css': 'text/css',
  '.json': 'application/json',
  '.scm': 'text/plain',
  '.wasm': 'application/wasm',
};

/**
 * Starts a simple static file server rooted at projectRoot.
 * @param {number} serverPort
 * @returns {Promise<import('http').Server>}
 */
export function startServer(serverPort) {
  return new Promise((resolve, reject) => {
    const server = createServer(async (req, res) => {
      const urlPath = decodeURIComponent(req.url.split('?')[0]);
      const filePath = join(projectRoot, urlPath);
      try {
        if (!filePath.startsWith(projectRoot)) {
          res.writeHead(403);
          res.end('Forbidden');
          return;
        }
        const data = await readFile(filePath);
        const ext = extname(filePath);
        res.writeHead(200, { 'Content-Type': MIME_TYPES[ext] || 'application/octet-stream' });
        res.end(data);
      } catch {
        res.writeHead(404);
        res.end('Not Found');
      }
    });
    server.listen(serverPort, '127.0.0.1', () => resolve(server));
    server.on('error', reject);
  });
}

// =========================================================================
// Test infrastructure
// =========================================================================

let passed = 0;
let failed = 0;
const failures = [];

/**
 * Asserts a test condition.
 * @param {string} name - Test description
 * @param {boolean} condition - Pass/fail
 * @param {string} [detail] - Extra detail on failure
 */
export function assert(name, condition, detail) {
  if (condition) {
    passed++;
    console.log(`  \x1b[32mPASS\x1b[0m: ${name}`);
  } else {
    failed++;
    const msg = detail ? `${name} -- ${detail}` : name;
    failures.push(msg);
    console.log(`  \x1b[31mFAIL\x1b[0m: ${msg}`);
  }
}

/**
 * Polls a condition function until it returns true, or times out.
 * Use instead of fixed `setTimeout` waits in tests for more reliable,
 * faster test execution.
 *
 * @param {Function} conditionFn - Async function that returns truthy when condition is met
 * @param {number} [timeout=5000] - Maximum wait time in ms
 * @param {number} [interval=50] - Polling interval in ms
 * @returns {Promise<boolean>} True if condition was met, false if timed out
 */
export async function waitFor(conditionFn, timeout = 5000, interval = 50) {
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    try {
      if (await conditionFn()) return true;
    } catch { /* condition threw — keep polling */ }
    await new Promise(r => setTimeout(r, interval));
  }
  return false;
}

/**
 * Polls a Puppeteer page condition until it returns true, or times out.
 * The condition is evaluated in the page context.
 *
 * @param {import('puppeteer').Page} page - Puppeteer page
 * @param {string} conditionJs - JS expression that returns truthy when ready
 * @param {number} [timeout=5000] - Maximum wait time in ms
 * @returns {Promise<boolean>} True if condition was met
 */
export async function waitForPage(page, conditionJs, timeout = 5000) {
  return waitFor(
    () => page.evaluate(conditionJs).catch(() => false),
    timeout
  );
}

/** @returns {{ passed: number, failed: number, failures: string[] }} */
export function getResults() {
  return { passed, failed, failures };
}

/** Increments the failure count and records a crash message. */
export function recordCrash(name, message) {
  failed++;
  failures.push(`${name} CRASHED: ${message}`);
  console.log(`  \x1b[31mCRASH\x1b[0m: ${name} -- ${message}`);
}

// =========================================================================
// Browser launch
// =========================================================================

/**
 * Runs pre-flight checks and launches the browser with the extension.
 * @returns {Promise<{ server: import('http').Server, browser: import('puppeteer').Browser, extensionId: string|null }>}
 */
export async function launchTestBrowser() {
  // Pre-flight checks
  try {
    await stat(resolve(extensionPath, 'manifest.json'));
  } catch {
    console.error('ERROR: Extension manifest not found. Run `npm run build:panel` first.');
    process.exit(1);
  }
  try {
    await stat(resolve(projectRoot, 'dist/scheme.js'));
  } catch {
    console.error('ERROR: dist/scheme.js not found. Run `npm run build` first.');
    process.exit(1);
  }

  const server = await startServer(port);
  console.log(`Static server running on ${BASE_URL}`);

  // Use Puppeteer's Chrome for Testing (CfT) — system Chrome does not
  // reliably load unpacked extensions via --load-extension.
  //
  // On macOS, Chrome steals focus when launched headed. We patch the
  // Info.plist to add LSBackgroundOnly=true, which tells macOS to treat
  // Chrome as a background-only app (no Dock icon, no focus stealing).
  const chromePath = puppeteer.executablePath();
  if (process.platform === 'darwin') {
    await patchChromeInfoPlist(chromePath);
  }

  const chromeArgs = [
    `--disable-extensions-except=${extensionPath}`,
    `--load-extension=${extensionPath}`,
    '--no-first-run',
    '--no-default-browser-check',
    // Prevent background throttling — avoids disconnections when the user
    // activates their screensaver or switches to another app.
    '--disable-background-timer-throttling',
    '--disable-backgrounding-occluded-windows',
    '--disable-renderer-backgrounding',
  ];
  const browser = await puppeteer.launch({
    headless: false, // Extensions require headed mode
    args: chromeArgs,
  });

  // Wait for extension service worker to confirm extension loaded
  let extensionId = null;
  try {
    const swTarget = await browser.waitForTarget(
      t => t.type() === 'service_worker' && t.url().includes('background'),
      { timeout: 10000 }
    );
    const swUrl = swTarget.url();
    extensionId = swUrl.match(/chrome-extension:\/\/([^/]+)/)?.[1];
    console.log(`Extension loaded (ID: ${extensionId})`);
  } catch {
    console.warn('Warning: Extension service worker not found. Panel UI tests will be skipped.');
  }

  return { server, browser, extensionId };
}

/**
 * Launches a new browser with the extension loaded (no server creation).
 * Used for crash recovery — reuses the existing static file server.
 * @returns {Promise<{ browser: import('puppeteer').Browser, extensionId: string|null }>}
 */
export async function relaunchBrowser() {
  const chromePath = puppeteer.executablePath();
  if (process.platform === 'darwin') {
    await patchChromeInfoPlist(chromePath);
  }

  const chromeArgs = [
    `--disable-extensions-except=${extensionPath}`,
    `--load-extension=${extensionPath}`,
    '--no-first-run',
    '--no-default-browser-check',
    '--disable-background-timer-throttling',
    '--disable-backgrounding-occluded-windows',
    '--disable-renderer-backgrounding',
  ];
  const browser = await puppeteer.launch({
    headless: false,
    args: chromeArgs,
  });

  let extensionId = null;
  try {
    const swTarget = await browser.waitForTarget(
      t => t.type() === 'service_worker' && t.url().includes('background'),
      { timeout: 10000 }
    );
    const swUrl = swTarget.url();
    extensionId = swUrl.match(/chrome-extension:\/\/([^/]+)/)?.[1];
    console.log(`  [Browser relaunched, extension ID: ${extensionId}]`);
  } catch {
    console.warn('  [Warning: Extension service worker not found after relaunch]');
  }

  return { browser, extensionId };
}
