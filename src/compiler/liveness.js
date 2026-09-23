/**
 * @fileoverview Which locals are live where a suspended frame will resume.
 *
 * ## What it is for
 *
 * When a continuation is captured beneath a compiled procedure, the procedure
 * saves its locals into a frame and its resumable form restores them later.
 * Saving every local at every suspension point is simple and was the original
 * rule, and it is quadratic: a procedure with *n* locals and *n* call sites
 * writes *n²* names. Measured, frame literals were 57% of all generated code in
 * the benchmark corpus, and 94% of the single largest procedure.
 *
 * A frame only needs the locals that can still be read after it resumes. This
 * works that out, per block of the resumable form, with ordinary backward
 * liveness.
 *
 * ## Why it runs over emitted statements rather than the IR
 *
 * Because what has to be saved includes values the IR has no name for. In
 * `(list (one) (capturer))` the result of `(one)` is sitting in a JavaScript
 * temporary while `(capturer)` runs, and it is live across that call. The
 * temporaries exist only in the generated code, so the generated code is what
 * is analysed.
 *
 * The emitted form is regular enough to make that safe. Every local is a name
 * the emitter declared, jumps are always `$pc = N; continue;`, and each
 * statement is its own string. The analysis never has to parse JavaScript; it
 * has to find identifiers, assignments at the start of a statement, and jumps.
 *
 * ## Which way it is allowed to be wrong
 *
 * Saving too few locals is a wrong answer on resume. Saving too many costs a
 * little size. So every approximation here leans one way: a name that *might*
 * be read is treated as read, an edge that *might* be taken is treated as
 * taken, and only a statement that unconditionally begins `name =` counts as a
 * definition. Where the text is ambiguous -- a local's name inside a string
 * literal, say -- the answer is a name saved unnecessarily, never one lost.
 */

/** An identifier, as generated code spells one. */
const IDENTIFIER = /[A-Za-z_$][\w$]*/g;

/**
 * A statement that assigns a whole variable.
 *
 * Anchored to the start, so only an unconditional assignment qualifies: an
 * assignment inside an `if` must not count as a definition, or a variable
 * written on one path would be judged dead on the other. `==`, `===` and `=>`
 * are excluded.
 */
const ASSIGNMENT = /^\s*([A-Za-z_$][\w$]*)\s*=(?![=>])/;

/** A jump in the resumable form. */
const JUMP = /\$pc = (\d+)/g;

/**
 * A statement that certainly ends its block by jumping.
 *
 * Only the exact shapes the resumable form emits: `$pc = N; continue;` on its
 * own, or after the closing brace of a conditional jump. Anything else is
 * treated as possibly falling through, which is the safe mistake.
 */
const ENDS_IN_JUMP = /(^|\})\s*\$pc = \d+; continue;$/;

/**
 * Summarises what one statement reads and writes.
 *
 * @param {string} statement - One emitted statement.
 * @param {Set<string>} locals - The names that can be saved in a frame.
 * @param {RegExp|null} spill - Matches a spill placeholder, capturing the block
 *   it resumes at.
 * @returns {{def: (string|null), uses: Set<string>, spillsTo: Array<number>}}
 *   The variable assigned, if the statement assigns one outright; the locals it
 *   reads; and the resume blocks of any spills it contains.
 */
function summarize(statement, locals, spill) {
  const spillsTo = [];
  let text = statement;
  if (spill !== null) {
    text = text.replace(spill, (_, block) => {
      spillsTo.push(Number(block));
      return ' ';
    });
  }

  let def = null;
  const assigned = ASSIGNMENT.exec(text);
  if (assigned !== null && locals.has(assigned[1])) {
    def = assigned[1];
    // What remains is the value being assigned, which is read before the
    // assignment happens -- so `x = g(x)` keeps `x` live above it.
    text = text.slice(assigned[0].length);
  }

  const uses = new Set();
  for (const token of text.match(IDENTIFIER) ?? []) {
    if (locals.has(token)) uses.add(token);
  }
  return { def, uses, spillsTo };
}

/**
 * The blocks control can pass to after a block.
 *
 * @param {Array<string>} statements - The block's statements.
 * @param {number} index - The block's own index.
 * @param {number} count - How many blocks there are.
 * @returns {Array<number>} Successor block indices.
 */
function successors(statements, index, count) {
  const next = new Set();
  for (const statement of statements) {
    for (const jump of statement.matchAll(JUMP)) next.add(Number(jump[1]));
    // `continue` without setting `$pc` would re-enter this same block. The
    // resumable form never does that, but the edge costs nothing to include:
    // a block's own live-in set cannot grow by flowing back into itself.
    if (statement.includes('continue')) next.add(index);
  }

  const last = statements.length > 0 ? statements[statements.length - 1].trim() : '';
  const ends = /^(return|throw)\b/.test(last) || ENDS_IN_JUMP.test(last);
  if (!ends && index + 1 < count) next.add(index + 1);

  return [...next].filter((block) => block >= 0 && block < count);
}

/**
 * Computes the live locals on entry to each block.
 *
 * @param {Array<Array<string>>} blocks - Statements per block, as the resumable
 *   form emits them. Block indices are the `$pc` values that select them.
 * @param {Set<string>} locals - The names a frame can hold. Anything else --
 *   globals, the runtime, `$r`, `$pc` -- is ignored.
 * @param {Object} [options] - Options.
 * @param {RegExp} [options.spill] - Matches a spill placeholder and captures
 *   the block it resumes at. A spill reads exactly what is live there, since
 *   the frame is the only way those values reach the code after a capture.
 *   Must have the global flag.
 * @returns {Array<Set<string>>} The live locals on entry to each block.
 */
export function liveIn(blocks, locals, options = {}) {
  const spill = options.spill ?? null;
  const summaries = blocks.map((statements) =>
    statements.map((statement) => summarize(statement, locals, spill)));
  const next = blocks.map((statements, i) => successors(statements, i, blocks.length));

  // Standard backward dataflow. Every set only ever grows, so comparing sizes
  // is enough to detect a fixed point. Blocks are visited last-first because
  // the resumable form mostly jumps forward, which makes one pass nearly
  // always sufficient.
  const live = blocks.map(() => new Set());
  let changed = true;
  while (changed) {
    changed = false;
    for (let i = blocks.length - 1; i >= 0; i--) {
      const current = new Set();
      for (const successor of next[i]) {
        for (const name of live[successor]) current.add(name);
      }
      for (let k = summaries[i].length - 1; k >= 0; k--) {
        const { def, uses, spillsTo } = summaries[i][k];
        if (def !== null) current.delete(def);
        for (const name of uses) current.add(name);
        for (const block of spillsTo) {
          for (const name of live[block]) current.add(name);
        }
      }
      if (current.size !== live[i].size) {
        live[i] = current;
        changed = true;
      }
    }
  }
  return live;
}
