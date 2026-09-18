/**
 * Compiles a benchmark program under a chosen calling convention and returns a
 * runnable function.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { Frontend, assignmentConvert } from './frontend.js';
import * as RT from './runtime.js';
import * as STK from './stack_machine.js';
import { emitProgram as emitB } from './backend_b.js';
import { emitProgram as emitA } from './backend_a.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
export const PROGRAM_DIR = path.join(__dirname, '..', '..', 'benchmarks', 'programs');

/**
 * Compiles a benchmark to JavaScript source.
 * @param {string} file - Program filename within `benchmarks/programs`.
 * @param {number} size - Value to bind to `bench-size`.
 * @param {string} convention - 'A' or 'B'.
 * @returns {string} Generated JavaScript source.
 */
export function compileSource(file, size, convention) {
  const scheme = `(define bench-size ${size})\n` +
    fs.readFileSync(path.join(PROGRAM_DIR, file), 'utf8');
  const frontend = new Frontend();
  const nodes = assignmentConvert(frontend.compileProgram(scheme));
  const emit = convention === 'A' ? emitA : emitB;
  const body = emit(nodes);
  const runName = frontend.globals.lookup('bench-run');
  if (!runName) throw new Error(`${file}: no bench-run definition found`);

  const invoke = convention === 'A'
    ? `return STK.run(${runName}, []);`
    : `return RT.driveB(() => ${runName}());`;

  return `let RESULT;\n${body}\n\nreturn function () { ${invoke} };`;
}

/**
 * Compiles a benchmark and returns a callable.
 * @param {string} file - Program filename.
 * @param {number} size - Value to bind to `bench-size`.
 * @param {string} convention - 'A' or 'B'.
 * @returns {{run: function(): *, source: string}} The entry point and its source.
 */
export function compile(file, size, convention) {
  const source = compileSource(file, size, convention);
  // `new Function` rather than eval so the generated code gets its own scope
  // and V8 treats it as a normal script rather than as a debug evaluation.
  const factory = new Function('RT', 'STK', source);
  return { run: factory(RT, STK), source };
}
