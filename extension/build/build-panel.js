#!/usr/bin/env node
/**
 * @fileoverview esbuild script to bundle the Scheme-JS DevTools panel.
 *
 * Bundles extension/panel-src/main.js and its dependencies (including
 * CodeMirror 6) into extension/panel/panel.js. Run from the project root:
 *
 *   node extension/build/build-panel.js
 *   node extension/build/build-panel.js --watch
 */

import * as esbuild from 'esbuild';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const root = join(__dirname, '../..');
const watch = process.argv.includes('--watch');

const buildOptions = {
  entryPoints: [join(root, 'extension/panel-src/main.js')],
  bundle: true,
  outfile: join(root, 'extension/panel/panel.js'),
  format: 'esm',
  platform: 'browser',
  // Chrome extensions support modern JS, target a recent version
  target: ['chrome110'],
  sourcemap: false,
  minify: false,
  logLevel: 'info',
};

if (watch) {
  const ctx = await esbuild.context(buildOptions);
  await ctx.watch();
  console.log('[build-panel] Watching for changes...');
} else {
  await esbuild.build(buildOptions);
  console.log('[build-panel] Done.');
}
