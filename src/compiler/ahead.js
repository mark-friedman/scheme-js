/**
 * @fileoverview Running a program compiled ahead of time, with no
 * interpreter, expander, reader or library system.
 *
 * The build (`scripts/lib/ahead.scm`) compiles every top-level form of a
 * program and of each library it uses, and writes them as a table: each unit
 * -- a library, in the order the libraries load, then the program -- with
 * the bindings it imports, each from the unit that defined it, and its
 * items, each a top-level form: a procedure to bind, a value to define, or a
 * form to run for its effect, each as the code the compiler generated for
 * it. The table is data: it imports nothing, and is given the runtime here.
 *
 * Running it is building each unit's environment, inside one of the
 * primitives this runtime carries, binding its imports by value, as the
 * library system's `import-into!` binds them, and running its items in
 * order, each on the driver that needs no interpreter beneath it (`runAhead`
 * in src/core/interpreter/unwind.js). A raise nobody handles is thrown to
 * whoever ran the program, as one from an interpreted program is.
 *
 * JavaScript, as the runtime compiled code calls is (src/compiler/runtime.js):
 * what makes a compiled procedure, and what runs one with no interpreter,
 * are JavaScript's, and a loader written in Scheme would itself need this to
 * run.
 */

import * as R from './runtime.js';
import { RUNTIME } from './runtime_object.js';
import { Environment } from '../core/interpreter/environment.js';
import { registerPrimitive } from '../core/interpreter/primitive_bindings.js';
import { SCHEME_PRIMITIVE, SCHEME_RAW_CALL, registerGlobalEnvironment } from '../core/interpreter/values.js';
import { intern } from '../core/interpreter/symbol.js';
import { Cons } from '../core/interpreter/cons.js';
import { Char } from '../core/primitives/char_class.js';
import { Flonum } from '../core/interpreter/number_representation.js';
import { Rational } from '../core/primitives/rational.js';
import { Complex } from '../core/primitives/complex.js';
import { mathPrimitives } from '../core/primitives/math.js';
import { portPrimitives } from '../core/primitives/io/port_primitives.js';
import { printerPrimitives } from '../core/primitives/io/printer_primitives.js';
import { listPrimitives } from '../core/primitives/list.js';
import { vectorPrimitives } from '../core/primitives/vector.js';
import { recordPrimitives } from '../core/primitives/record.js';
import { stringPrimitives } from '../core/primitives/string.js';
import { charPrimitives } from '../core/primitives/char.js';
import { eqPrimitives } from '../core/primitives/eq.js';
import { procedurePrimitives } from '../core/primitives/apply.js';
import { raisePrimitives } from '../core/primitives/raise.js';
import { errorObjectPrimitives } from '../core/primitives/error_object.js';
import { timePrimitives } from '../core/primitives/time.js';
import { bytevectorPrimitives } from '../core/primitives/bytevector.js';
import { hashTablePrimitives } from '../extras/primitives/hash_table.js';
import { bitwisePrimitives } from '../extras/primitives/bitwise.js';
import { readerPrimitives } from '../core/primitives/reader_support.js';

/**
 * The primitives a program compiled ahead of time has: those that need
 * nothing of the interpreter or the library system, in the order the
 * interpreter's global environment binds them (`createGlobalEnvironment` in
 * src/core/primitives/index.js), so that a name two groups bind is bound as
 * it is there. A program that reaches any other is refused by the build.
 * @type {Object<string, Function>}
 */
export const AHEAD_PRIMITIVES = {
  ...mathPrimitives,
  ...portPrimitives,
  ...printerPrimitives,
  ...listPrimitives,
  ...vectorPrimitives,
  ...recordPrimitives,
  ...stringPrimitives,
  ...charPrimitives,
  ...eqPrimitives,
  ...procedurePrimitives,
  ...raisePrimitives,
  ...errorObjectPrimitives,
  ...timePrimitives,
  ...bytevectorPrimitives,
  ...hashTablePrimitives,
  ...bitwisePrimitives,
  // The reader's scans of a whole text, which the printer uses too.
  ...readerPrimitives
};

/**
 * What a table's constant pools are built with: the constructors of the
 * values a constant written down can be (`constant-expression` in
 * scripts/lib/table_writer.scm).
 */
const CONSTRUCTORS = { intern, Cons, Char, Flonum, Rational, Complex };

/**
 * The environment of the primitives, which every unit's is inside: each
 * marked as the interpreter marks its own, and its compiled procedures, which
 * have no interpreter to run on, run on the driver when JavaScript calls them
 * (`aheadRunner`).
 * @param {Object<string, Function>} primitives - The primitives, by name.
 * @returns {Environment}
 */
function primitiveEnvironment(primitives) {
  const env = new Environment(null);
  for (const [name, fn] of Object.entries(primitives)) {
    if (typeof fn === 'function') {
      fn[SCHEME_PRIMITIVE] = true;
      fn[SCHEME_RAW_CALL] = fn;
      if (!('schemeName' in fn)) fn.schemeName = name;
      registerPrimitive(name, fn);
    }
    env.define(name, fn);
  }
  registerGlobalEnvironment(env, R.aheadRunner);
  return env;
}

/**
 * Runs a program compiled ahead of time.
 * @param {{units: Array<Object>}} program - The table the build wrote: each
 *   unit `{library?: string[], imports: Array<[string, (string|null), string]>,
 *   items: Array<Object>}`, an import `[local, unit, name]` binding `local` to
 *   the value `name` has in the library whose key is `unit` now, or, where
 *   `unit` is null, to the primitive `name`; an item `{procedure: name}`,
 *   `{define: name}` or `{run: true}`, with its code's `make` and the
 *   function building its constant pool, `constants`.
 * @param {Object<string, Function>} [primitives] - The primitives it runs
 *   with.
 * @returns {*} The value of the program's last form that was run for its
 *   effect, or undefined.
 */
export function runProgram(program, primitives = AHEAD_PRIMITIVES) {
  const base = primitiveEnvironment(primitives);
  const libraries = new Map();
  let value;
  for (const unit of program.units) {
    const env = new Environment(base);
    if (unit.library !== undefined) libraries.set(unit.library.join('.'), env);
    for (const [local, from, name] of unit.imports) {
      env.define(local, (from === null ? base : libraries.get(from)).lookup(name));
    }
    for (const item of unit.items) {
      // A library's environment, which code referring to a library's own
      // binding reads it from, is written as the library's name.
      const pool = item.constants(CONSTRUCTORS).map((constant) => (constant !== null && typeof constant === 'object'
        && Array.isArray(constant.library) ? libraries.get(constant.library.join('.')) : constant));
      const made = item.make(RUNTIME, env, pool);
      if (item.procedure !== undefined) {
        env.define(item.procedure, made);
      } else if (item.define !== undefined) {
        env.define(item.define, R.runAhead(made, []));
      } else {
        value = R.runAhead(made, []);
      }
    }
  }
  return value;
}

/**
 * Runs a program compiled ahead of time as the whole of what a process or a
 * page does, as the CLI runs a program's file: what it wrote to the console
 * output port and had not ended with a newline written out as it ends, and a
 * raise nobody handled reported as the CLI reports one, on the console's
 * error stream, with the process's exit status 1 under Node. The module the
 * build writes calls this as it loads (src/packaging/ahead_bundle.js).
 * @param {{source: string, units: Array<Object>}} program - The table, and
 *   the program's file, which the report names.
 */
export function runMain(program) {
  const output = AHEAD_PRIMITIVES['%console-output-port']();
  try {
    runProgram(program);
    output.flush();
  } catch (e) {
    output.flush();
    console.error(`Error executing ${program.source}: ${e?.message ?? e}`);
    if (typeof process !== 'undefined') process.exitCode = 1;
  }
}
