#!/usr/bin/env node
/**
 * Package the Chrome DevTools extension for distribution.
 *
 * Copies all extension files into dist/extension/ and creates a
 * dist/scheme-stack-extension.zip archive ready for upload to the
 * Chrome Web Store or side-loading.
 *
 * Run: node scripts/package_extension.js
 */

import fs from 'fs';
import path from 'path';
import { execSync } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = path.join(__dirname, '..');

const extensionDir = path.join(projectRoot, 'extension');
const distDir = path.join(projectRoot, 'dist');
const distExtensionDir = path.join(distDir, 'extension');
const zipPath = path.join(distDir, 'scheme-stack-extension.zip');

/**
 * Recursively copies a directory, skipping .DS_Store files.
 * Creates destination directories as needed.
 * @param {string} src - Source directory path.
 * @param {string} dest - Destination directory path.
 * @returns {string[]} List of relative file paths that were copied.
 */
function copyDirRecursive(src, dest) {
  const copied = [];
  fs.mkdirSync(dest, { recursive: true });

  for (const entry of fs.readdirSync(src, { withFileTypes: true })) {
    // Skip .DS_Store and other dotfiles
    if (entry.name.startsWith('.')) continue;

    const srcPath = path.join(src, entry.name);
    const destPath = path.join(dest, entry.name);

    if (entry.isDirectory()) {
      copied.push(...copyDirRecursive(srcPath, destPath));
    } else {
      fs.copyFileSync(srcPath, destPath);
      copied.push(path.relative(distExtensionDir, destPath));
    }
  }

  return copied;
}

/**
 * Packages the Chrome DevTools extension into dist/extension/ and
 * creates a zip archive at dist/scheme-stack-extension.zip.
 */
function packageExtension() {
  // Verify the extension source directory exists
  if (!fs.existsSync(extensionDir)) {
    console.error(`Error: Extension directory not found at ${extensionDir}`);
    process.exit(1);
  }

  // Clean previous output
  if (fs.existsSync(distExtensionDir)) {
    fs.rmSync(distExtensionDir, { recursive: true });
  }
  if (fs.existsSync(zipPath)) {
    fs.unlinkSync(zipPath);
  }

  // Ensure dist/ exists
  fs.mkdirSync(distDir, { recursive: true });

  // Copy all extension files
  console.log('Copying extension files to dist/extension/ ...');
  const copiedFiles = copyDirRecursive(extensionDir, distExtensionDir);

  // Create zip archive
  console.log('Creating dist/scheme-stack-extension.zip ...');
  execSync(`zip -r "${zipPath}" .`, { cwd: distExtensionDir, stdio: 'pipe' });

  // Print summary
  console.log(`\nPackaged ${copiedFiles.length} files:`);
  for (const file of copiedFiles) {
    console.log(`  ${file}`);
  }

  const zipStats = fs.statSync(zipPath);
  const zipSizeKB = (zipStats.size / 1024).toFixed(1);
  console.log(`\nZip archive: ${zipPath} (${zipSizeKB} KB)`);
}

packageExtension();
