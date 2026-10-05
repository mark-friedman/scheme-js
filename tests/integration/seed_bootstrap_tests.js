/**
 * @fileoverview How the library system's seed loads the libraries the reader
 * and the expander are made of, which are Scheme: with no text read and no
 * form expanded while their prebuilt tables are current, since each table has
 * its library's `define-library` form and its top-level forms as core forms,
 * as JSON;
 * and when one is not, reading and expanding its source with the pinned seed
 * (`library_seed.js`, `scripts/pin_seed.js`).
 *
 * JavaScript tests, since what is tested is the seed, which is JavaScript, and
 * the tables it reads.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { seedLibrarySystem, loadPinnedSeed, systemLibrary } from '../../src/core/interpreter/library_seed.js';
import { decodeDatum } from '../../src/compiler/prebuilt.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import { list } from '../../src/core/interpreter/cons.js';
import pinnedSeedImage from '../../src/packaging/pinned_seed.js';
import prebuiltLibraries from '../../src/packaging/compiled_libraries.js';

/**
 * A core form written out, with each name its expander made by renaming
 * numbered in the order the names first appear.
 * @param {*} core - The core form.
 * @returns {string}
 */
function normalized(core) {
  const renamed = new Map();
  return writeString(core).replace(/([^\s()]+?)_\$\d+/g, (name) => {
    if (!renamed.has(name)) renamed.set(name, renamed.size + 1);
    return `${name.replace(/_\$\d+$/, '')}_#${renamed.get(name)}`;
  });
}

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 */
export function runSeedBootstrapTests(logger) {
  logger.title("The seed loading the reader's and the expander's libraries");

  const core = prebuiltLibraries['scheme.core'];
  assert(logger, "a seed library's table has its define-library form",
    writeString(decodeDatum(core.declaration)).startsWith('(define-library (scheme core)'), true);
  assert(logger, "and so does one of nothing but re-exports, (scheme base)'s",
    writeString(decodeDatum(prebuiltLibraries['scheme.base']?.declaration ?? 'false')).startsWith('(define-library (scheme base)'), true);
  const seedKeys = ['scheme.core', 'scheme.control', 'scheme-js.reader', 'scheme-js.expander', 'scheme-js.library-system'];
  assert(logger, "and every form of the seed's libraries, a procedure or a core form, none to expand",
    seedKeys.filter((key) => prebuiltLibraries[key].restore.some((item) => item.form !== undefined)), []);
  assert(logger, "a macro's definition as one that binds it pending",
    core.restore.some((item) => item.core !== undefined && decodeDatum(item.core).car?.name === 'define-syntax'), true);

  const image = pinnedSeedImage();
  const pinned = loadPinnedSeed(image).loaded;
  const text = "(define (f x)\n  (list 'a \"b\" #\\c x.y #(1 2)))";
  const read = (reader) => callSchemeProcedure(reader.get('read-source'), [text, 'p.scm', false, false]);
  const pinnedData = read(pinned.get('scheme-js.reader'));
  assert(logger, 'the pinned reader reads as the seed\'s reader does',
    writeString(pinnedData), writeString(list(...parse(text, { filename: 'p.scm', dotAccess: false }))));
  assert(logger, 'spans and all', JSON.stringify(pinnedData.car.source),
    '{"filename":"p.scm","line":1,"column":1,"endLine":2,"endColumn":32}');

  const expand = (expander, form) => callSchemeProcedure(expander.get('expand'), [form]);
  const forms = parse("(define (g n) (let loop ((i 0) (acc '())) (if (< i n) (loop (+ i 1) (cons i acc)) acc)))"
    + " `(a ,b ,@c) (lambda args (apply + args))");
  assert(logger, "the pinned expander expands as the seed's expander does",
    forms.map((form) => normalized(expand(pinned.get('scheme-js.expander'), form))),
    forms.map((form) => normalized(expand(systemLibrary(['scheme-js', 'expander']), form))));

  // A seed with no table current -- here, none at all -- reads and expands its
  // libraries' sources, with the pinned seed until its own reader and
  // expander are loaded.
  const system = seedLibrarySystem({});
  assert(logger, 'a seed with no table current loads, reading and expanding its sources',
    String(callSchemeProcedure(system.get('library-key'), [list(intern('scheme'), intern('base'))])), 'scheme.base');
}
