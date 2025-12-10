/**
 * Utility functions for dynamic-wind stack walking.
 * 
 * Extracted from AppFrame to support reuse by future features
 * like exception handlers and delimited continuations.
 */

import { getWindFrameClass } from './frame_registry.js';
import { TailApp, Literal, RestoreContinuation } from './stepables.js';

/**
 * Computes the sequence of wind actions needed when jumping
 * from one continuation stack to another.
 * 
 * This handles the dynamic-wind protocol:
 * 1. Run 'after' thunks for extents we're leaving (innermost first)
 * 2. Run 'before' thunks for extents we're entering (outermost first)
 * 3. Restore the target stack and value
 * 
 * @param {Array} currentStack - The current frame stack.
 * @param {Array} targetStack - The target continuation's frame stack.
 * @param {*} value - The value to pass to the continuation.
 * @returns {Array<Executable>} Sequence of actions to execute.
 */
export function computeWindActions(currentStack, targetStack, value) {
    const WindFrame = getWindFrameClass();

    // 1. Find common ancestor index
    let i = 0;
    while (i < currentStack.length && i < targetStack.length && currentStack[i] === targetStack[i]) {
        i++;
    }
    const ancestorIndex = i;

    // 2. Identify WindFrames to unwind (from top of current down to ancestor)
    const toUnwind = currentStack
        .slice(ancestorIndex)
        .reverse()
        .filter(f => f instanceof WindFrame);

    // 3. Identify WindFrames to rewind (from ancestor up to top of target)
    const toRewind = targetStack
        .slice(ancestorIndex)
        .filter(f => f instanceof WindFrame);

    // 4. Construct sequence of operations
    const actions = [];

    // Run 'after' thunks for extents we're leaving
    for (const frame of toUnwind) {
        actions.push(new TailApp(new Literal(frame.after), []));
    }

    // Run 'before' thunks for extents we're entering
    for (const frame of toRewind) {
        actions.push(new TailApp(new Literal(frame.before), []));
    }

    // Append the final restoration if there are actions
    if (actions.length > 0) {
        actions.push(new RestoreContinuation(targetStack, value));
    }

    return actions;
}

/**
 * Finds the index of the nearest common ancestor between two stacks.
 * Useful for debugging and visualization.
 * 
 * @param {Array} stack1 - First frame stack.
 * @param {Array} stack2 - Second frame stack.
 * @returns {number} The ancestor index.
 */
export function findCommonAncestorIndex(stack1, stack2) {
    let i = 0;
    while (i < stack1.length && i < stack2.length && stack1[i] === stack2[i]) {
        i++;
    }
    return i;
}
