/**
 * @fileoverview Unit tests for DebugLevel and DebugLevelStack.
 *
 * Tests for nested debug level creation, stack push/pop/popAll operations,
 * and depth tracking. Part of Phase 1: Debug Runtime Core.
 */

import { assert } from '../harness/helpers.js';
import { DebugLevel, DebugLevelStack } from '../../src/debug/debug_level.js';

/**
 * Runs all DebugLevel and DebugLevelStack tests.
 * @param {Object} logger - Test logger
 */
export function runDebugLevelTests(logger) {
  logger.title('DebugLevel - Creation');

  // Test: DebugLevel stores all constructor properties
  {
    const source = { filename: 'test.scm', line: 10, column: 1 };
    const stack = [{ name: 'foo', source }];
    const env = { bindings: {} };
    const parent = new DebugLevel(0, 'manual', null, [], {}, null);
    const level = new DebugLevel(1, 'breakpoint', source, stack, env, parent);

    assert(logger, 'level property', level.level, 1);
    assert(logger, 'reason property', level.reason, 'breakpoint');
    assert(logger, 'source property', level.source, source);
    assert(logger, 'stack property', level.stack, stack);
    assert(logger, 'env property', level.env, env);
    assert(logger, 'parentLevel property', level.parentLevel, parent);
  }

  // Test: DebugLevel parentLevel defaults to null
  {
    const level = new DebugLevel(0, 'manual', null, [], {});

    assert(logger, 'parentLevel defaults to null', level.parentLevel, null);
  }

  // Test: DebugLevel selectedFrameIndex defaults to 0
  {
    const level = new DebugLevel(0, 'step', null, [], {});

    assert(logger, 'selectedFrameIndex defaults to 0', level.selectedFrameIndex, 0);
  }

  // Test: All valid reason types
  {
    const reasons = ['breakpoint', 'step', 'exception', 'manual'];
    for (const reason of reasons) {
      const level = new DebugLevel(0, reason, null, [], {});
      assert(logger, `reason "${reason}" is stored`, level.reason, reason);
    }
  }

  logger.title('DebugLevelStack - Basic Operations');

  // Test: Empty stack has depth 0
  {
    const stack = new DebugLevelStack();

    assert(logger, 'empty stack depth is 0', stack.depth(), 0);
  }

  // Test: Current on empty stack returns null
  {
    const stack = new DebugLevelStack();

    assert(logger, 'current on empty stack is null', stack.current(), null);
  }

  // Test: Push increments depth
  {
    const stack = new DebugLevelStack();
    const level = new DebugLevel(0, 'manual', null, [], {});
    stack.push(level);

    assert(logger, 'depth after one push is 1', stack.depth(), 1);
  }

  // Test: Current returns topmost level
  {
    const stack = new DebugLevelStack();
    const level = new DebugLevel(0, 'breakpoint', null, [], {});
    stack.push(level);

    assert(logger, 'current returns pushed level', stack.current(), level);
  }

  // Test: Pop returns the popped level
  {
    const stack = new DebugLevelStack();
    const level = new DebugLevel(0, 'manual', null, [], {});
    stack.push(level);
    const popped = stack.pop();

    assert(logger, 'pop returns the level', popped, level);
    assert(logger, 'depth after pop is 0', stack.depth(), 0);
  }

  // Test: Pop on empty stack returns null
  {
    const stack = new DebugLevelStack();
    const popped = stack.pop();

    assert(logger, 'pop on empty returns null', popped, null);
  }

  logger.title('DebugLevelStack - PopAll');

  // Test: PopAll clears all levels and returns them
  {
    const stack = new DebugLevelStack();
    const l0 = new DebugLevel(0, 'manual', null, [], {});
    const l1 = new DebugLevel(1, 'breakpoint', null, [], {});
    stack.push(l0);
    stack.push(l1);

    const popped = stack.popAll();

    assert(logger, 'popAll returns array length 2', popped.length, 2);
    assert(logger, 'popAll first element', popped[0], l0);
    assert(logger, 'popAll second element', popped[1], l1);
    assert(logger, 'depth after popAll is 0', stack.depth(), 0);
    assert(logger, 'current after popAll is null', stack.current(), null);
  }

  // Test: PopAll on empty stack returns empty array
  {
    const stack = new DebugLevelStack();
    const popped = stack.popAll();

    assert(logger, 'popAll on empty returns empty array', popped.length, 0);
  }

  logger.title('DebugLevelStack - Nested Levels');

  // Test: Push 3 levels, verify depth, pop one, verify depth
  {
    const stack = new DebugLevelStack();
    const l0 = new DebugLevel(0, 'manual', null, [], {});
    const l1 = new DebugLevel(1, 'breakpoint', null, [], {});
    const l2 = new DebugLevel(2, 'exception', null, [], {});
    stack.push(l0);
    stack.push(l1);
    stack.push(l2);

    assert(logger, 'depth after 3 pushes is 3', stack.depth(), 3);
    assert(logger, 'current is last pushed', stack.current(), l2);

    const popped = stack.pop();

    assert(logger, 'pop returns last pushed', popped, l2);
    assert(logger, 'depth after pop is 2', stack.depth(), 2);
    assert(logger, 'current after pop is l1', stack.current(), l1);
  }

  // Test: Pop all remaining after partial pop
  {
    const stack = new DebugLevelStack();
    stack.push(new DebugLevel(0, 'manual', null, [], {}));
    stack.push(new DebugLevel(1, 'step', null, [], {}));
    stack.push(new DebugLevel(2, 'breakpoint', null, [], {}));
    stack.pop();

    assert(logger, 'depth is 2 after push 3 pop 1', stack.depth(), 2);

    stack.popAll();

    assert(logger, 'depth is 0 after popAll', stack.depth(), 0);
  }
}

export default runDebugLevelTests;
