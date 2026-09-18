/**
 * Measures what a JavaScript stack trace shows under each calling convention.
 *
 * This is the measurement that decides whether the Chrome DevTools extension
 * can be retired. A debugger's call-stack panel is rendered from the
 * JavaScript stack; a source map relabels those frames with the original
 * source's names and positions, but it cannot invent frames that are not
 * there. So the question "would DevTools show the Scheme call stack?" reduces
 * to "does the JavaScript stack contain one frame per live Scheme frame?",
 * which is directly observable.
 *
 * Usage: node experiments/stage2a/stack_shape.js [depth]
 */

import fs from 'fs';
import os from 'os';
import path from 'path';

import { compile, PROGRAM_DIR } from './compile.js';
import { Frontend, assignmentConvert } from './frontend.js';
import * as RT from './runtime.js';
import * as STK from './stack_machine.js';
import { emitProgram as emitA } from './backend_a.js';
import { emitProgram as emitB } from './backend_b.js';

const DEPTH = parseInt(process.argv[2] ?? '12', 10);

/**
 * A program that recurses to a known depth and then samples the stack.
 *
 * The recursive call is wrapped in a `cons` so it is genuinely *non-tail*.
 * A tail call is not a live frame and correctly does not appear on either
 * stack -- an earlier version of this probe recursed in tail position and
 * measured nothing at all.
 */
const PROGRAM = `
(define (deep n)
  (if (< n 1)
      (js-stack)
      (cons (deep (- n 1)) '())))

(define (bench-run) (deep ${DEPTH}))
`;

/**
 * Compiles the probe program under one convention and returns the stack text.
 * @param {string} convention - 'A' or 'B'.
 * @returns {string} The captured stack trace.
 */
function sample(convention) {
  const frontend = new Frontend();
  const nodes = assignmentConvert(frontend.compileProgram(PROGRAM));
  const emit = convention === 'A' ? emitA : emitB;
  const runName = frontend.globals.lookup('bench-run');
  const invoke = convention === 'A'
    ? `return STK.run(${runName}, []);`
    : `return RT.driveB(() => ${runName}());`;
  const source = `let RESULT;\n${emit(nodes)}\n\nreturn function () { ${invoke} };`;
  STK.reset();
  let value = new Function('RT', 'STK', source)(RT, STK)();
  while (value instanceof RT.Pair) value = value.car;
  return value;
}

/**
 * Counts how many stack frames belong to compiled Scheme procedures.
 * @param {string} stack - A stack trace.
 * @param {string} procedureName - The Scheme procedure to count.
 * @returns {{scheme: number, total: number, frames: Array<string>}} The counts.
 */
function analyse(stack, procedureName) {
  const lines = stack.split('\n').slice(1).map((l) => l.trim());
  const scheme = lines.filter((l) => l.includes(procedureName)).length;
  return { scheme, total: lines.length, frames: lines };
}

console.log('='.repeat(78));
console.log(`Stack shape under each calling convention (Scheme recursion depth ${DEPTH})`);
console.log('='.repeat(78));
console.log('');
console.log('The probe recurses non-tail to the given depth and samples the JavaScript');
console.log('stack. Every level is a live Scheme frame that a debugger should display.');
console.log('');

const results = {};
for (const convention of ['A', 'B']) {
  const stack = sample(convention);
  const analysis = analyse(stack, 'deep');
  results[convention] = analysis;

  console.log(`--- Convention ${convention} (${convention === 'A' ? 'explicit frame stack' : 'native JavaScript stack'}) ---`);
  console.log(`  JavaScript frames for the Scheme procedure 'deep': ${analysis.scheme}`);
  console.log(`  total JavaScript frames in the trace:              ${analysis.total}`);
  console.log('  innermost frames:');
  for (const frame of analysis.frames.slice(0, 6)) console.log(`    ${frame}`);
  console.log('');
}

console.log('='.repeat(78));
const a = results.A.scheme;
const b = results.B.scheme;
console.log(`Scheme frames visible to a debugger:  convention A = ${a}, convention B = ${b}`);
console.log(`Scheme frames actually live:          ${DEPTH}`);
console.log('');
if (b >= DEPTH && a < DEPTH) {
  console.log('Convention B puts the Scheme call stack on the JavaScript stack, so a source');
  console.log('map is enough for a debugger to show it. Convention A does not: its frames');
  console.log('are in an array the debugger cannot see, and a custom stack view has to');
  console.log('survive.');
}
console.log('');
console.log('--- JSON Results ---');
console.log(JSON.stringify({
  depth: DEPTH,
  A: { schemeFrames: results.A.scheme, totalFrames: results.A.total },
  B: { schemeFrames: results.B.scheme, totalFrames: results.B.total }
}, null, 2));
