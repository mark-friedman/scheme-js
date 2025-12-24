import { assert, createTestLogger } from '../../harness/helpers.js';
import { computeWindActions, findCommonAncestorIndex } from '../../../src/core/interpreter/winders.js';
import { WindFrame } from '../../../src/core/interpreter/stepables.js';

export function runWindersTests(interpreter, logger) {
    logger.title('Running Winders Logic Tests...');

    // 1. Test findCommonAncestorIndex
    // NOTE: findCommonAncestorIndex returns the LENGTH of common prefix,
    // not the index of the last common element. So if 2 elements match,
    // it returns 2 (meaning elements at indices 0 and 1 match).

    // Stacks are arrays of WindFrames.
    // We simulate them with simple objects for this low-level test if possible,
    // but winders.js uses === equality, so any objects work.

    const w1 = { id: 1 };
    const w2 = { id: 2 };
    const w3 = { id: 3 };
    const w4 = { id: 4 };

    // s1 = [w1, w2, w3], s2 = [w1, w2, w4]
    // Common: w1, w2 (indices 0, 1) -> length = 2
    const s1 = [w1, w2, w3];
    const s2 = [w1, w2, w4];
    assert(logger, "Common ancestor (2 elements)", findCommonAncestorIndex(s1, s2), 2);

    // s3 = [w1, w3], s4 = [w1, w4]
    // Common: w1 (index 0) -> length = 1
    const s3 = [w1, w3];
    const s4 = [w1, w4];
    assert(logger, "Common ancestor (1 element)", findCommonAncestorIndex(s3, s4), 1);

    // No common ancestor
    const s5 = [w3];
    const s6 = [w4];
    assert(logger, "No common ancestor", findCommonAncestorIndex(s5, s6), 0);

    // Empty stacks
    assert(logger, "Empty stacks", findCommonAncestorIndex([], []), 0);
    assert(logger, "One empty stack", findCommonAncestorIndex([w1], []), 0);

    // NOTE: computeWindActions requires real WindFrame instances with
    // before/after thunks. The function is tested indirectly via the
    // dynamic-wind functional tests (dynamic_wind_tests.scm).

}
