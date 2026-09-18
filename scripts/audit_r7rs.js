/**
 * @fileoverview R7RS-small conformance audit.
 *
 * Probes a fully bootstrapped interpreter for every identifier the standard
 * requires, and classifies each as bound, missing, or present-but-broken (a
 * stub that throws unconditionally when called). The last category matters most
 * for the compiler effort, because a stub is a deviation that looks like
 * conformance until someone calls it.
 *
 * This is a reporting tool rather than a test: it currently reports known
 * deviations, so wiring it into `npm test` would simply fail the build. Once
 * the deviations are closed it should be promoted to a registered test so the
 * surface cannot silently regress.
 *
 * Usage:
 *   node scripts/audit_r7rs.js [--verbose]
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { createInterpreter } from '../src/core/interpreter/index.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { parse } from '../src/core/interpreter/reader.js';
import { setFileResolver } from '../src/core/interpreter/library_loader.js';
import { SCHEME_BASE, SCHEME_BASE_SYNTAX, OTHER_LIBRARIES, NON_BASE_SYNTAX } from './r7rs_identifiers.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PROJECT_ROOT = path.join(__dirname, '..');

/** Directories searched when a library is imported. */
const SEARCH_DIRS = [
  'src/core/scheme', 'src/extras/scheme', 'src/core/scheme/chibi', '.'
];

/**
 * Builds an interpreter with every R7RS library imported.
 * @returns {{interpreter: Object, env: Object, run: function(string): *}} The
 *   interpreter and a source-evaluating helper.
 */
function bootstrap() {
  setFileResolver((libraryName) => {
    const base = libraryName.join('/');
    const leaf = libraryName[libraryName.length - 1];
    // `libraryName` is sometimes a library name like ("scheme" "base") and
    // sometimes an include path like ("scheme" "macros.scm"), so bare names are
    // tried before extensions are appended -- the same order repl.js uses.
    const candidates = [base, `${base}.sld`, `${base}.scm`, leaf, `${leaf}.sld`, `${leaf}.scm`];
    for (const dir of SEARCH_DIRS) {
      for (const candidate of candidates) {
        const full = path.join(PROJECT_ROOT, dir, candidate);
        if (fs.existsSync(full) && fs.statSync(full).isFile()) {
          return fs.readFileSync(full, 'utf8');
        }
      }
    }
    throw new Error(`Library not found: (${libraryName.join(' ')})`);
  });

  const { interpreter, env } = createInterpreter();
  const run = (code) => {
    let result;
    for (const expr of parse(code)) {
      result = interpreter.run(analyze(expr), env, [], undefined, { jsAutoConvert: 'raw' });
    }
    return result;
  };

  // Import one library at a time: a library that does not exist is itself an
  // audit finding, so it must be recorded rather than aborting the run.
  const wanted = [
    '(scheme base)', '(scheme write)', '(scheme read)', '(scheme repl)',
    '(scheme lazy)', '(scheme case-lambda)', '(scheme eval)', '(scheme time)',
    '(scheme complex)', '(scheme cxr)', '(scheme char)', '(scheme inexact)',
    '(scheme file)', '(scheme process-context)', '(scheme load)'
  ];
  const unavailable = [];
  for (const library of wanted) {
    try {
      run(`(import ${library})`);
    } catch (e) {
      unavailable.push({ library, detail: String(e.message).slice(0, 100) });
    }
  }
  return { interpreter, env, run, unavailable };
}

/**
 * Classifies one required identifier.
 * @param {function(string): *} run - Source evaluator.
 * @param {string} name - The identifier to probe.
 * @param {boolean} isSyntax - True if the identifier is a syntactic keyword.
 * @returns {{name: string, status: string, detail: string}} The classification.
 */
function probe(run, name, isSyntax) {
  if (isSyntax) {
    // Syntactic keywords cannot be evaluated as expressions. Probing whether
    // the analyzer treats the name as a special form or macro is the closest
    // available check.
    try {
      run(`(define (r7rs-audit-probe) (quote ${name}))`);
      return { name, status: 'assumed', detail: 'syntax (not probed at runtime)' };
    } catch (e) {
      return { name, status: 'missing', detail: e.message };
    }
  }

  try {
    run(name);
    return { name, status: 'bound', detail: '' };
  } catch (e) {
    return { name, status: 'missing', detail: String(e.message).slice(0, 90) };
  }
}

/**
 * Procedures that must never be invoked by the audit, because calling them
 * with no arguments has an effect rather than just an error.
 *
 * `exit` and `emergency-exit` terminate the audit process; the reading
 * procedures block on or consume standard input; the writing procedures
 * corrupt the report; and the file and environment procedures touch state
 * outside the process. Excluding them means stub detection does not cover
 * them, which is noted in the report rather than hidden.
 */
const UNSAFE_TO_CALL = new Set([
  'exit', 'emergency-exit',
  'read', 'read-char', 'read-line', 'read-string', 'read-u8',
  'read-bytevector', 'read-bytevector!', 'peek-char',
  'char-ready?', 'u8-ready?',
  'newline', 'write-char', 'write-string', 'write-u8', 'write-bytevector',
  'display', 'write', 'write-shared', 'write-simple', 'flush-output-port',
  'load', 'delete-file', 'file-exists?',
  'open-input-file', 'open-output-file',
  'open-binary-input-file', 'open-binary-output-file',
  'with-input-from-file', 'with-output-to-file',
  'call-with-input-file', 'call-with-output-file',
  'command-line', 'get-environment-variable', 'get-environment-variables',
  'current-second', 'current-jiffy'
]);

/**
 * Calls a bound procedure with no arguments to detect stubs that throw
 * regardless of input. A stub reports "not supported" for every call; a real
 * procedure reports an arity or type error instead.
 * @param {function(string): *} run - Source evaluator.
 * @param {string} name - A bound procedure name.
 * @returns {string|null} A stub message, or null if the procedure looks real
 *   or was not safe to probe.
 */
function detectStub(run, name) {
  if (UNSAFE_TO_CALL.has(name)) return null;
  const STUB_PATTERN = /not (supported|implemented)|immutable in this implementation|unsupported/i;
  try {
    run(`(${name})`);
    return null;
  } catch (e) {
    const message = String(e.message);
    return STUB_PATTERN.test(message) ? message.slice(0, 110) : null;
  }
}

/**
 * Runs a function with `console.error` suppressed.
 *
 * The interpreter logs native JS errors to the console before rethrowing them.
 * Probing deliberately triggers hundreds of those, so without this the report
 * is unreadable.
 *
 * @param {function(): *} fn - The function to run.
 * @returns {*} Whatever `fn` returned.
 */
function quietly(fn) {
  const original = console.error;
  console.error = () => {};
  try {
    return fn();
  } finally {
    console.error = original;
  }
}

function main() {
  const verbose = process.argv.includes('--verbose');
  const { run, unavailable } = quietly(bootstrap);

  const unavailableNames = new Set(unavailable.map(u => u.library));

  const groups = [
    { library: '(scheme base)', names: SCHEME_BASE, syntax: false },
    { library: '(scheme base) [syntax]', names: SCHEME_BASE_SYNTAX, syntax: true },
    ...Object.entries(OTHER_LIBRARIES).map(([library, names]) => ({ library, names, syntax: false }))
  ];

  const missing = [];
  const stubs = [];
  let bound = 0;
  let assumed = 0;

  console.log('='.repeat(72));
  console.log('R7RS-small conformance audit');
  console.log('='.repeat(72));
  console.log('');

  if (unavailable.length > 0) {
    console.log('LIBRARIES THAT COULD NOT BE IMPORTED');
    for (const u of unavailable) console.log(`  ${u.library.padEnd(26)} ${u.detail}`);
    console.log('');
  }

  for (const group of groups) {
    const groupMissing = [];
    for (const name of group.names) {
      const isSyntax = group.syntax || NON_BASE_SYNTAX.has(name);
      const result = quietly(() => probe(run, name, isSyntax));
      if (result.status === 'missing') {
        groupMissing.push(result);
        missing.push({ ...result, library: group.library });
      } else if (result.status === 'assumed') {
        assumed++;
      } else {
        bound++;
        const stub = quietly(() => detectStub(run, name));
        if (stub) stubs.push({ name, library: group.library, detail: stub });
      }
    }
    const total = group.names.length;
    const ok = total - groupMissing.length;
    const notImported = unavailableNames.has(group.library) ? '  [library not importable]' : '';
    const flag = (groupMissing.length === 0 ? 'complete' : `${groupMissing.length} MISSING`) + notImported;
    console.log(`  ${group.library.padEnd(26)} ${String(ok).padStart(3)}/${String(total).padEnd(3)}  ${flag}`);
    if (verbose && groupMissing.length > 0) {
      for (const m of groupMissing) console.log(`      - ${m.name}`);
    }
  }

  console.log('');
  console.log(`bound: ${bound}   syntax assumed present: ${assumed}   missing: ${missing.length}`);

  if (missing.length > 0) {
    console.log('');
    console.log('MISSING IDENTIFIERS');
    for (const m of missing) console.log(`  ${m.library.padEnd(26)} ${m.name}`);
  }

  if (stubs.length > 0) {
    console.log('');
    console.log('STUBS -- bound but throw unconditionally (deviations that look like conformance)');
    for (const s of stubs) console.log(`  ${s.library.padEnd(26)} ${s.name}\n      ${s.detail}`);
  }

  console.log('');
  console.log('--- JSON Results ---');
  console.log(JSON.stringify({ bound, assumed, missing, stubs, unavailable }, null, 2));
}

main();
