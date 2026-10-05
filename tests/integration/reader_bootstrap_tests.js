/**
 * @fileoverview How the reader, which is Scheme, reads its own source: the
 * library system's seed reads no text while its libraries' prebuilt tables
 * are current, since each table has its library's `define-library` form; and
 * when one is not, it reads with the pinned reader, the reader's libraries'
 * sources as data, evaluated (`library_seed.js`, `scripts/pin_reader.js`).
 *
 * JavaScript tests, since what is tested is the seed, which is JavaScript, and
 * the tables it reads.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { seedLibrarySystem, pinnedReader } from '../../src/core/interpreter/library_seed.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import { list } from '../../src/core/interpreter/cons.js';
import pinnedReaderSources from '../../src/packaging/pinned_reader.js';
import prebuiltLibraries from '../../src/packaging/compiled_libraries.js';

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 */
export function runReaderBootstrapTests(logger) {
  logger.title('The reader reading its own source');

  const core = prebuiltLibraries['scheme.core'];
  assert(logger, "a seed library's table has its define-library form",
    writeString(core.declaration).startsWith('(define-library (scheme core)'), true);
  assert(logger, "and so does one of nothing but re-exports, (scheme base)'s",
    writeString(prebuiltLibraries['scheme.base']?.declaration ?? false).startsWith('(define-library (scheme base)'), true);

  const text = "(define (f x)\n  (list 'a \"b\" #\\c x.y #(1 2)))";
  const read = pinnedReader(pinnedReaderSources());
  const pinned = read(text, 'p.scm');
  assert(logger, 'the pinned reader reads as the seed\'s reader does',
    pinned.map((datum) => writeString(datum)), parse(text, { filename: 'p.scm', dotAccess: false }).map((datum) => writeString(datum)));
  assert(logger, 'spans and all', JSON.stringify(pinned[0].source),
    '{"filename":"p.scm","line":1,"column":1,"endLine":2,"endColumn":32}');

  // A seed with no table current -- here, none at all -- reads its libraries'
  // sources, with the pinned reader until its own is loaded.
  const system = seedLibrarySystem({});
  assert(logger, 'a seed with no table current loads, reading its sources',
    String(callSchemeProcedure(system.get('library-key'), [list(intern('scheme'), intern('base'))])), 'scheme.base');
}
