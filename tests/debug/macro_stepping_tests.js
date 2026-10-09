/**
 * @fileoverview Where the REPL debugger stops in code a macro made.
 *
 * The interpreter pauses, while it is stepped, before each expression that has
 * a source span. A macro's template is made into fresh pairs, which have none,
 * so the use of a macro the system defines -- `when`, `cond`, `do` -- was no
 * stop of its own, and one whose expansion holds nothing the use gave it was
 * no stop at all. The expander gives an expansion its use's span, so a step
 * stops once at the use, then at what the use gave the macro, which keeps its
 * own: as DevTools does for the same code compiled
 * (tests/devtools/stepping_tests.js).
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';

/**
 * A procedure binding a constant, then using `when`, whose expansion is an
 * `if`, around an assignment.
 */
const PROGRAM = [
  '(define (st-assigning flag)',
  '  (let ((count 0))',
  '    (when flag',
  '      (set! count 1))',
  '    count))',
  ''
].join('\n');

/**
 * Runs a call with a breakpoint set, stepping over from where it pauses until
 * it has paused a number of times, and says where each pause was.
 * @param {string} call - The call, as Scheme.
 * @param {number} line - The breakpoint's line.
 * @param {number} pauses - How many pauses to take before letting it run on.
 * @returns {Promise<Array<string>>} Each pause, as line:column.
 */
async function stepsFrom(call, line, pauses) {
  const { interpreter, env } = createInterpreter();
  for (const form of parse(PROGRAM, { filename: 'steps.scm' })) {
    interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
  }
  const places = [];
  const runtime = new SchemeDebugRuntime({
    onPause: (info) => {
      places.push(`${info.source.line}:${info.source.column}`);
      if (places.length < pauses) setTimeout(() => runtime.stepOver(), 2);
      else {
        runtime.removeBreakpoint(id);
        setTimeout(() => runtime.resume(), 2);
      }
    }
  });
  interpreter.setDebugRuntime(runtime);
  runtime.enable();
  const id = runtime.setBreakpoint('steps.scm', line);
  try {
    await interpreter.runAsync(analyze(parse(call)[0]), env, { jsAutoConvert: 'raw' });
  } finally {
    interpreter.setDebugRuntime(null);
  }
  return places;
}

/**
 * Runs the tests of where the REPL debugger stops in code a macro made.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runMacroSteppingTests(logger) {
  logger.title('The REPL Debugger in Code a Macro Made');

  assert(logger, "a step over stops at the binding, at the system macro's use, when, then at the set! it was given",
    await stepsFrom('(st-assigning #t)', 2, 3), ['2:3', '3:5', '4:7']);
}

export default runMacroSteppingTests;
