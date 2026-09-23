/**
 * @fileoverview Unit tests for the liveness analysis behind frame spills.
 *
 * A suspended compiled frame saves only the variables that are live where it
 * will resume. Saving too few is a wrong answer on resume, so every case here
 * that could go wrong in the unsafe direction -- a variable judged dead that is
 * in fact read -- is tested explicitly. Saving too many only costs size, so the
 * analysis is allowed to over-approximate, and a few tests pin down exactly
 * where it does.
 *
 * Blocks are written as the resumable form emits them: one string per
 * statement, jumps as `$pc = N; continue;`.
 */

import { assert } from '../harness/helpers.js';
import { liveIn } from '../../src/compiler/liveness.js';

/**
 * Renders a live set for comparison.
 * @param {Set<string>} set - A live set.
 * @returns {string} Its members, sorted and comma-separated.
 */
function names(set) {
  return [...set].sort().join(',');
}

/**
 * Runs the liveness unit tests.
 * @param {Object} logger - Test logger.
 * @returns {void}
 */
export function runLivenessTests(logger) {
  logger.title('Liveness for frame spills');

  const locals = new Set(['a', 'b', 'c', 'x', 'y', 'z', '$t1', '$t12']);
  const at = (blocks, i = 0) => names(liveIn(blocks, locals)[i]);

  // --- Within a block --------------------------------------------------------

  assert(logger, 'a read makes a variable live',
    at([['return a;']]), 'a');
  assert(logger, 'an assignment ends liveness above it',
    at([['x = 1;', 'return x;']]), '');
  assert(logger, 'the value assigned is read before the assignment happens',
    at([['x = f(b);', 'return x;']]), 'b');
  assert(logger, 'reading and assigning in one statement keeps it live',
    at([['x = g(x);', 'return x;']]), 'x');

  // Assigning through a box reads the box. An assigned Scheme local is held in
  // a one-element array, and `x[0] = v` needs `x` -- so treating it as a
  // definition would drop the box from the frame.
  assert(logger, 'writing through a box is a read of the box',
    at([['x[0] = 5;', 'return 1;']]), 'x');

  // Only a whole statement that begins `name =` defines `name`. Anything
  // conditional must not, or a variable written on one path would be judged
  // dead on the other.
  assert(logger, 'an assignment inside a condition is not a definition',
    at([['if (c) { x = 1; }', 'return x;']]), 'c,x');

  assert(logger, 'names that are not locals are ignored',
    at([['return R.step(K[0], E);']]), '');
  assert(logger, 'a longer name is not mistaken for a shorter one',
    at([['return $t12;']]), '$t12');

  // --- Across blocks ---------------------------------------------------------

  // Liveness is the union over every path that can follow, not the path that
  // happened to run.
  assert(logger, 'both branches of a jump contribute',
    at([
      ['if (c) { $pc = 1; continue; } $pc = 2; continue;'],
      ['return a;'],
      ['return b;']
    ]), 'a,b,c');

  assert(logger, 'an unconditional return ends the block',
    at([['return a;'], ['return b;']]), 'a');

  // A block that does not end in a jump or a return runs on into the next one.
  // Missing that edge would be unsafe, so it is assumed wherever the ending is
  // not certain.
  assert(logger, 'a block without a jump falls through',
    at([['y = h();'], ['return y + z;']]), 'z');
  assert(logger, 'a conditional return does not end the block',
    at([['if (c) return a;'], ['return b;']]), 'a,b,c');

  // A variable defined on one branch and not the other is still live above the
  // branch, because the other path may reach the read.
  assert(logger, 'a definition on only one branch does not kill',
    at([
      ['if (c) { $pc = 1; continue; } $pc = 2; continue;'],
      ['x = 1;', '$pc = 3; continue;'],
      ['$pc = 3; continue;'],
      ['return x;']
    ]), 'c,x');

  // The resumable form's loops only ever arise from `for (;;) switch`, but the
  // analysis must reach a fixed point on a cycle all the same.
  assert(logger, 'a cycle reaches a fixed point',
    at([
      ['$pc = 1; continue;'],
      ['x = y;', 'if (c) { $pc = 0; continue; } $pc = 2; continue;'],
      ['return x;']
    ]), 'c,y');

  // --- The spill itself -------------------------------------------------------

  // The frame literal at a suspension point stands for exactly what is live at
  // the block it will resume at -- no more, which is the point, and no less,
  // which is the subtle part. A capture has no ordinary edge to its resume
  // block: it spills and returns, and the frame is the only way its variables
  // reach the code after it. So the spill *reads* that block's live set.
  //
  // Get this wrong and a variable read only after a capture is judged dead
  // before it. Resume at an earlier call site, run forward into the capture,
  // and the capture spills `undefined` in its place.
  {
    const spill = /\$FRAME(\d+)\$/g;
    const resumedAt1 = ['$t1 = $r;', 'return x;'];

    const assignedFirst = liveIn([
      ['x = 1;', 'y = 2;', 'R.reify(f$r, 1, $FRAME1$);', 'return R.UNWIND;'],
      resumedAt1
    ], locals, { spill });
    assert(logger, 'a spill needs what its resume block reads',
      names(assignedFirst[1]), 'x');
    assert(logger, 'a variable set before the spill is not live above it',
      names(assignedFirst[0]), '');

    const notAssigned = liveIn([
      ['y = 2;', 'R.reify(f$r, 1, $FRAME1$);', 'return R.UNWIND;'],
      resumedAt1
    ], locals, { spill });
    assert(logger, 'a variable read only after a capture is live before it',
      names(notAssigned[0]), 'x');

    // And only that: the placeholder does not stand for every name, which is
    // the conservative behaviour this analysis replaces.
    assert(logger, 'a variable no resume block reads is not kept for the spill',
      notAssigned[0].has('y'), false);
  }

  // A mention inside a string literal counts as a read. That is wrong, but it
  // is wrong in the safe direction -- one extra name saved -- and parsing
  // string literals to avoid it would be a way to be wrong in the unsafe one.
  assert(logger, 'a name inside a string is conservatively a read',
    at([['return "a";']]), 'a');
}
