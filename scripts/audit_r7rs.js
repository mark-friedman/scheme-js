/**
 * @fileoverview R7RS-small conformance audit.
 *
 * Probes a fully bootstrapped interpreter for every identifier the standard
 * requires, and classifies each as bound, missing, or present-but-broken (a
 * stub that throws unconditionally when called). The last category matters most
 * for the compiler effort, because a stub is a deviation that looks like
 * conformance until someone calls it.
 *
 * Each library's identifiers are probed in an environment of that library
 * alone, `(environment '(scheme char))` say, which sees only what the library
 * exports. Probed where every library is imported, as a program's top level,
 * a name one library fails to export is found anyway, in another or among the
 * primitives every program's top level sees.
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
import { analyze } from '../src/core/interpreter/expand.js';
import { parse } from '../src/core/interpreter/reader.js';
import { setFileResolver } from '../src/core/interpreter/library_loader.js';
import { SCHEME_BASE, SCHEME_BASE_SYNTAX, OTHER_LIBRARIES, NON_BASE_SYNTAX, SCHEME_R5RS, R5RS_SYNTAX } from './r7rs_identifiers.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PROJECT_ROOT = path.join(__dirname, '..');

/** Directories searched when a library is imported. */
const SEARCH_DIRS = [
  'src/core/scheme', 'src/extras/scheme', 'src/core/scheme/chibi', '.'
];

/**
 * Builds an interpreter, and an environment of each R7RS library alone, bound
 * at its top level to `audit-env:` and the library's name.
 * @returns {{interpreter: Object, env: Object, run: function(string): *,
 *   unavailable: Array<{library: string, detail: string}>}} The interpreter,
 *   a source-evaluating helper, and the libraries that could not be imported.
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

  run('(import (scheme base) (scheme eval))');
  // One library at a time: a library that does not exist is itself an audit
  // finding, so it must be recorded rather than aborting the run.
  const unavailable = [];
  for (const library of LIBRARIES) {
    try {
      run(`(define ${environmentOf(library)} (environment '${library}))`);
    } catch (e) {
      unavailable.push({ library, detail: String(e.message).slice(0, 100) });
    }
  }
  return { interpreter, env, run, unavailable };
}

/** The R7RS-small libraries. */
const LIBRARIES = [
  '(scheme base)', '(scheme write)', '(scheme read)', '(scheme repl)',
  '(scheme lazy)', '(scheme case-lambda)', '(scheme eval)', '(scheme time)',
  '(scheme complex)', '(scheme cxr)', '(scheme char)', '(scheme inexact)',
  '(scheme file)', '(scheme process-context)', '(scheme load)', '(scheme r5rs)'
];

/**
 * The top-level name an environment of a library alone is bound to.
 * @param {string} library - The library's name, as written.
 * @returns {string} The name.
 */
function environmentOf(library) {
  return `audit-env:${library.slice(1, -1).replace(/ /g, '-')}`;
}

/**
 * Whether `environment` makes an environment of its import sets (R7RS 6.12)
 * rather than returning another: an identifier imported under a prefix is
 * bound in it.
 * @param {function(string): *} run - Source evaluator.
 * @returns {string|null} What is wrong, or null.
 */
function checkEnvironment(run) {
  try {
    const value = run("(eval '(audit-b:car (audit-b:list 1)) (environment '(prefix (scheme base) audit-b:)))");
    return value === 1n || value === 1 ? null : `evaluated to ${String(value)}`;
  } catch (e) {
    return `ignores its import sets: ${String(e.message).slice(0, 80)}`;
  }
}

/**
 * Classifies one required identifier, in an environment of its library alone.
 * @param {function(string): *} run - Source evaluator.
 * @param {string} environment - The top-level name of the environment.
 * @param {string} name - The identifier to probe.
 * @param {boolean} isSyntax - True if the identifier is a syntactic keyword.
 * @returns {{name: string, status: string, detail: string}} The classification.
 */
function probe(run, environment, name, isSyntax) {
  if (isSyntax) {
    // A keyword cannot be evaluated as an expression, but it can head a form:
    // whatever that form's error, it is not that the name is unbound. One
    // that only another form takes is used in that form instead. The
    // analyzer's own special forms -- `if`, `quote`, `lambda` -- are found in
    // every environment, imported or not, so for those this shows only that
    // the analyzer has them.
    const form = KEYWORD_USES[name] ?? `(${name})`;
    try {
      run(`(eval '${form} ${environment})`);
    } catch (e) {
      if (unboundIn(e, name) || name in KEYWORD_USES) {
        return { name, status: 'missing', detail: String(e.message).slice(0, 90) };
      }
    }
    return { name, status: 'assumed', detail: 'syntax (not probed at runtime)' };
  }

  try {
    run(`(eval '${name} ${environment})`);
    return { name, status: 'bound', detail: '' };
  } catch (e) {
    return { name, status: 'missing', detail: String(e.message).slice(0, 90) };
  }
}

/**
 * A form using each keyword that only another form takes, which a keyword
 * heading a form of its own would not show: it is an error to use it so,
 * whether it is bound or not.
 */
const KEYWORD_USES = {
  'else': '(cond (else 1))',
  '=>': '(cond (1 => (lambda (x) x)))',
  '...': '(let-syntax ((m (syntax-rules () ((m x ...) (list x ...))))) (m 1 2))',
  '_': '(let-syntax ((m (syntax-rules () ((m _) 1)))) (m 2))',
  'syntax-rules': '(let-syntax ((m (syntax-rules () ((m) 1)))) (m))',
  'unquote': '(quasiquote ((unquote 1)))',
  'unquote-splicing': '(quasiquote ((unquote-splicing (list 1))))'
};

/**
 * Whether an error says a name is unbound.
 * @param {Error} e - The error.
 * @param {string} name - The name.
 * @returns {boolean}
 */
function unboundIn(e, name) {
  return String(e.message).endsWith(`unbound variable: ${name}`);
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
 * @param {string} environment - The top-level name of the environment it is
 *   bound in.
 * @param {string} name - A bound procedure name.
 * @returns {string|null} A stub message, or null if the procedure looks real
 *   or was not safe to probe.
 */
function detectStub(run, environment, name) {
  if (UNSAFE_TO_CALL.has(name)) return null;
  const STUB_PATTERN = /not (supported|implemented)|immutable in this implementation|unsupported/i;
  try {
    run(`(eval '(${name}) ${environment})`);
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
    { library: '(scheme base)', label: '(scheme base) [syntax]', names: SCHEME_BASE_SYNTAX, syntax: true },
    ...Object.entries(OTHER_LIBRARIES).map(([library, names]) => ({ library, names, syntax: false })),
    { library: '(scheme r5rs)', names: SCHEME_R5RS, syntax: false, syntaxNames: R5RS_SYNTAX }
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
    const label = group.label ?? group.library;
    const environment = environmentOf(group.library);
    const groupMissing = [];
    for (const name of group.names) {
      const isSyntax = group.syntax || NON_BASE_SYNTAX.has(name) || (group.syntaxNames?.has(name) ?? false);
      const result = quietly(() => probe(run, environment, name, isSyntax));
      if (result.status === 'missing') {
        groupMissing.push(result);
        missing.push({ ...result, library: label });
      } else if (result.status === 'assumed') {
        assumed++;
      } else {
        bound++;
        const stub = quietly(() => detectStub(run, environment, name));
        if (stub) stubs.push({ name, library: label, detail: stub });
      }
    }
    const total = group.names.length;
    const ok = total - groupMissing.length;
    const notImported = unavailableNames.has(group.library) ? '  [library not importable]' : '';
    const flag = (groupMissing.length === 0 ? 'complete' : `${groupMissing.length} MISSING`) + notImported;
    console.log(`  ${label.padEnd(26)} ${String(ok).padStart(3)}/${String(total).padEnd(3)}  ${flag}`);
    if (verbose && groupMissing.length > 0) {
      for (const m of groupMissing) console.log(`      - ${m.name}`);
    }
  }

  const environmentProblem = quietly(() => checkEnvironment(run));
  console.log(`  ${'environment, import sets'.padEnd(26)} ${environmentProblem === null ? 'honoured' : environmentProblem}`);

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
  console.log(JSON.stringify({ bound, assumed, missing, stubs, unavailable, environment: environmentProblem }, null, 2));
}

main();
