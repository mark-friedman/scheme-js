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

const __dirname = dirname(fileURLToPath(import.meta.url));
export const projectRoot = resolve(__dirname, '../..');
export const extensionPath = resolve(projectRoot, 'extension');
export const port = parseInt(process.env.TEST_PORT || '8081', 10);

export const BASE_URL = `http://127.0.0.1:${port}`;
export const TEST_PAGE = `${BASE_URL}/tests/debug/puppeteer_test_page.html`;
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
  const chromeArgs = [
    `--disable-extensions-except=${extensionPath}`,
    `--load-extension=${extensionPath}`,
    '--no-first-run',
    '--no-default-browser-check',
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
