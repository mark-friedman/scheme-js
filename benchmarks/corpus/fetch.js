/**
 * Downloads the Scheme corpus `decline_reasons.js --corpus` measures.
 *
 * The corpus is other people's code, under their licenses, so it is not kept
 * in this repository; `manifest.json` records exactly what it is instead, so a
 * measurement can be repeated. Each SRFI reference implementation is a GitHub
 * repository pinned to a commit; each Snow-Fort package is an archive pinned to
 * its version and to the SHA-256 the Snow-Fort index gives for it, which the
 * download is checked against. A source already downloaded is left alone.
 *
 * Packages marked `dependency` are there because a measured one imports them
 * and this implementation does not provide them; they are measured too, and
 * reported apart.
 *
 * Usage: node benchmarks/corpus/fetch.js
 * Downloads into `benchmarks/corpus/downloads/`, which git ignores.
 */

import fs from 'fs';
import path from 'path';
import crypto from 'crypto';
import zlib from 'zlib';
import { execFileSync } from 'child_process';
import { fileURLToPath } from 'url';

const here = path.dirname(fileURLToPath(import.meta.url));

/**
 * Where the corpus is downloaded to.
 * @type {string}
 */
export const DOWNLOADS = path.join(here, 'downloads');

/**
 * The corpus's sources, as `manifest.json` records them.
 * @returns {Array<Object>} Each source.
 */
export function corpusSources() {
  return JSON.parse(fs.readFileSync(path.join(here, 'manifest.json'), 'utf8')).sources;
}

/**
 * The directory a source is downloaded to.
 * @param {Object} source - A manifest entry.
 * @returns {string} The directory.
 */
export function sourceDirectory(source) {
  return path.join(DOWNLOADS, source.kind, source.name);
}

/**
 * Checks out one repository at its recorded commit, fetching only that commit.
 * @param {Object} source - A `git` manifest entry.
 * @param {string} dir - Where to put it.
 */
function fetchGit(source, dir) {
  fs.mkdirSync(dir, { recursive: true });
  const git = (...args) => execFileSync('git', args, { cwd: dir, stdio: 'pipe' });
  git('init', '-q');
  git('fetch', '-q', '--depth', '1', source.url, source.commit);
  git('checkout', '-q', 'FETCH_HEAD');
}

/**
 * Downloads one Snow-Fort archive, checks it against its recorded SHA-256, and
 * unpacks it.
 *
 * The Snow-Fort index signs the tar, not the gzipped file served, so the
 * archive is decompressed before it is hashed.
 *
 * @param {Object} source - A `snow` manifest entry.
 * @param {string} dir - Where to unpack it.
 * @returns {Promise<void>}
 */
async function fetchSnow(source, dir) {
  const response = await fetch(source.url);
  if (!response.ok) throw new Error(`${source.url}: HTTP ${response.status}`);
  const tar = zlib.gunzipSync(Buffer.from(await response.arrayBuffer()));
  const digest = crypto.createHash('sha256').update(tar).digest('hex');
  if (digest !== source.sha256) {
    throw new Error(`${source.url}: SHA-256 ${digest}, but the manifest records ${source.sha256}`);
  }
  fs.mkdirSync(dir, { recursive: true });
  const file = path.join(dir, `${source.name}.tar`);
  fs.writeFileSync(file, tar);
  // Some archives name a file through `..` -- `(rapid mapping)` has
  // `rapid/mapping/../mapping.exports.scm` -- which tar refuses unless told
  // to allow it. Allowed only once every entry is known to stay inside the
  // package.
  const entries = execFileSync('tar', ['-tf', file], { encoding: 'utf8' }).split('\n').filter(Boolean);
  const escapes = entries.find((entry) => path.isAbsolute(entry) || path.normalize(entry).startsWith('..'));
  if (escapes !== undefined) throw new Error(`${source.url}: ${escapes} is outside the package`);
  execFileSync('tar', ['-xPf', file, '-C', dir]);
  fs.unlinkSync(file);
}

/**
 * Downloads every source not already downloaded.
 * @returns {Promise<void>}
 */
async function main() {
  for (const source of corpusSources()) {
    const dir = sourceDirectory(source);
    if (fs.existsSync(dir)) continue;
    process.stdout.write(`${source.library} ... `);
    try {
      if (source.kind === 'git') fetchGit(source, dir);
      else await fetchSnow(source, dir);
      console.log('ok');
    } catch (e) {
      fs.rmSync(dir, { recursive: true, force: true });
      console.log(`failed: ${e.message}`);
      process.exitCode = 1;
    }
  }
}

if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) await main();
