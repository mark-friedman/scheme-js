/**
 * @fileoverview Unit tests for PauseController.
 * 
 * Tests for debugger pause state and stepping logic.
 * Part of Phase 1: Debug Runtime Core.
 */

import { assert } from '../../harness/helpers.js';
import { PauseController } from '../../../src/core/debug/pause_controller.js';

/**
 * Runs all PauseController tests.
 * @param {Object} logger - Test logger
 */
export function runPauseControllerTests(logger) {
    logger.title('PauseController - Initial State');

    // Test: Initial state is 'running'
    {
        const ctrl = new PauseController();
        assert(logger, 'initial state is running', ctrl.getState(), 'running');
    }

    // Test: stepInto sets state to 'stepping' with mode 'into'
    logger.title('PauseController - Step Commands');
    {
        const ctrl = new PauseController();
        ctrl.stepInto();

        assert(logger, 'stepInto sets state', ctrl.getState(), 'stepping');
        assert(logger, 'stepInto sets mode', ctrl.getStepMode(), 'into');
    }

    // Test: stepOver sets state with target depth
    {
        const ctrl = new PauseController();
        ctrl.stepOver(5); // Current depth is 5

        assert(logger, 'stepOver sets state', ctrl.getState(), 'stepping');
        assert(logger, 'stepOver sets mode', ctrl.getStepMode(), 'over');
        assert(logger, 'stepOver records depth', ctrl.getTargetDepth(), 5);
    }

    // Test: stepOut sets state with target depth
    {
        const ctrl = new PauseController();
        ctrl.stepOut(5); // Current depth is 5

        assert(logger, 'stepOut sets state', ctrl.getState(), 'stepping');
        assert(logger, 'stepOut sets mode', ctrl.getStepMode(), 'out');
        assert(logger, 'stepOut records depth', ctrl.getTargetDepth(), 5);
    }

    // Test: resume sets state to 'running'
    {
        const ctrl = new PauseController();
        ctrl.stepInto();
        ctrl.resume();

        assert(logger, 'resume sets state to running', ctrl.getState(), 'running');
    }

    // Test: shouldStepPause returns false when running
    logger.title('PauseController - Pause Decision');
    {
        const ctrl = new PauseController();

        assert(logger, 'shouldStepPause false when running', ctrl.shouldStepPause(5), false);
    }

    // Test: shouldStepPause returns true for stepInto always
    {
        const ctrl = new PauseController();
        ctrl.stepInto();

        assert(logger, 'shouldStepPause true for stepInto (depth 5)', ctrl.shouldStepPause(5), true);
        assert(logger, 'shouldStepPause true for stepInto (depth 10)', ctrl.shouldStepPause(10), true);
        assert(logger, 'shouldStepPause true for stepInto (depth 1)', ctrl.shouldStepPause(1), true);
    }

    // Test: shouldStepPause for stepOver at same depth -> true
    {
        const ctrl = new PauseController();
        ctrl.stepOver(5);

        assert(logger, 'stepOver pauses at same depth', ctrl.shouldStepPause(5), true);
    }

    // Test: shouldStepPause for stepOver at deeper depth -> false
    {
        const ctrl = new PauseController();
        ctrl.stepOver(5);

        assert(logger, 'stepOver does not pause at deeper depth', ctrl.shouldStepPause(6), false);
        assert(logger, 'stepOver does not pause at much deeper', ctrl.shouldStepPause(10), false);
    }

    // Test: shouldStepPause for stepOver at shallower depth -> true
    {
        const ctrl = new PauseController();
        ctrl.stepOver(5);

        assert(logger, 'stepOver pauses at shallower depth', ctrl.shouldStepPause(4), true);
        assert(logger, 'stepOver pauses at much shallower', ctrl.shouldStepPause(1), true);
    }

    // Test: shouldStepPause for stepOut at same depth -> false
    {
        const ctrl = new PauseController();
        ctrl.stepOut(5);

        assert(logger, 'stepOut does not pause at same depth', ctrl.shouldStepPause(5), false);
    }

    // Test: shouldStepPause for stepOut at shallower depth -> true
    {
        const ctrl = new PauseController();
        ctrl.stepOut(5);

        assert(logger, 'stepOut pauses at shallower depth', ctrl.shouldStepPause(4), true);
        assert(logger, 'stepOut pauses one level up', ctrl.shouldStepPause(1), true);
    }

    // Test: pause method sets paused state
    logger.title('PauseController - Pause State');
    {
        const ctrl = new PauseController();
        ctrl.pause();

        assert(logger, 'pause sets state to paused', ctrl.getState(), 'paused');
    }

    // Test: isPaused returns correct value
    {
        const ctrl = new PauseController();

        assert(logger, 'isPaused false when running', ctrl.isPaused(), false);

        ctrl.pause();
        assert(logger, 'isPaused true when paused', ctrl.isPaused(), true);

        ctrl.resume();
        assert(logger, 'isPaused false after resume', ctrl.isPaused(), false);
    }

    // Test: setBreakpointHitReason
    logger.title('PauseController - Pause Reasons');
    {
        const ctrl = new PauseController();
        ctrl.pause('breakpoint', 'bp-123');

        assert(logger, 'pause reason is breakpoint', ctrl.getPauseReason(), 'breakpoint');
        assert(logger, 'pause data is bp id', ctrl.getPauseData(), 'bp-123');
    }

    // Test: step pause clears pause reason
    {
        const ctrl = new PauseController();
        ctrl.pause('breakpoint', 'bp-123');
        ctrl.stepInto();

        assert(logger, 'stepping clears pause reason', ctrl.getPauseReason(), null);
    }

    // Test: reset clears all state
    logger.title('PauseController - Reset');
    {
        const ctrl = new PauseController();
        ctrl.stepOver(5);
        ctrl.reset();

        assert(logger, 'reset sets state to running', ctrl.getState(), 'running');
        assert(logger, 'reset clears step mode', ctrl.getStepMode(), null);
        assert(logger, 'reset clears target depth', ctrl.getTargetDepth(), null);
    }
}

export default runPauseControllerTests;
