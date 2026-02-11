/**
 * @fileoverview Unit tests for PauseController.
 *
 * Tests for debugger pause state and stepping logic.
 * Part of Phase 1: Debug Runtime Core.
 */

import { assert } from '../harness/helpers.js';
import { PauseController } from '../../src/debug/pause_controller.js';

/**
 * Runs all PauseController tests.
 * @param {Object} logger - Test logger
 */
export async function runPauseControllerTests(logger) {
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

    // ==============================
    // Cooperative Polling Tests
    // ==============================
    logger.title('PauseController - Cooperative Polling');

    // Test: shouldYield returns false before interval reached
    {
        const ctrl = new PauseController();
        assert(logger, 'shouldYield false before interval', ctrl.shouldYield(), false);
    }

    // Test: shouldYield returns true when interval reached
    {
        const ctrl = new PauseController();
        for (let i = 0; i < ctrl.baseYieldInterval - 1; i++) {
            ctrl.shouldYield();
        }
        assert(logger, 'shouldYield true at interval', ctrl.shouldYield(), true);
    }

    // Test: onYield resets op counter
    {
        const ctrl = new PauseController();
        for (let i = 0; i < ctrl.baseYieldInterval; i++) {
            ctrl.shouldYield();
        }
        ctrl.onYield();
        assert(logger, 'shouldYield false after onYield', ctrl.shouldYield(), false);
    }

    // Test: requestPause sets pauseRequested flag
    {
        const ctrl = new PauseController();
        assert(logger, 'pauseRequested initially false', ctrl.pauseRequested, false);
        ctrl.requestPause();
        assert(logger, 'requestPause sets pauseRequested', ctrl.pauseRequested, true);
    }

    // Test: requestPause lowers yield interval
    {
        const ctrl = new PauseController();
        ctrl.requestPause();
        // After requestPause, the yield interval should be emergency (100)
        // so shouldYield should return true after emergencyYieldInterval calls
        for (let i = 0; i < ctrl.emergencyYieldInterval - 1; i++) {
            ctrl.shouldYield();
        }
        assert(logger, 'shouldYield true at emergency interval', ctrl.shouldYield(), true);
    }

    // Test: resume clears pauseRequested
    {
        const ctrl = new PauseController();
        ctrl.requestPause();
        ctrl.pause();
        ctrl.resume();
        assert(logger, 'resume clears pauseRequested', ctrl.pauseRequested, false);
    }

    // Test: resume resets yield interval to base
    {
        const ctrl = new PauseController();
        ctrl.requestPause();
        ctrl.pause();
        ctrl.resume();
        assert(logger, 'resume resets currentYieldInterval', ctrl.currentYieldInterval, ctrl.baseYieldInterval);
    }

    // Test: abortAll flag
    {
        const ctrl = new PauseController();
        assert(logger, 'abortAll initially false', ctrl.abortAll, false);
        ctrl.abortAll = true;
        assert(logger, 'abortAll can be set', ctrl.abortAll, true);
    }

    // Test: reset clears abortAll
    {
        const ctrl = new PauseController();
        ctrl.abortAll = true;
        ctrl.requestPause();
        ctrl.opCount = 500;
        ctrl.reset();
        assert(logger, 'reset clears abortAll', ctrl.abortAll, false);
        assert(logger, 'reset clears pauseRequested', ctrl.pauseRequested, false);
        assert(logger, 'reset clears opCount', ctrl.opCount, 0);
        assert(logger, 'reset resets currentYieldInterval', ctrl.currentYieldInterval, ctrl.baseYieldInterval);
    }

    // ==============================
    // Wait/Resume Promise Tests
    // ==============================
    logger.title('PauseController - Wait/Resume Promise');

    // Test: waitForResume returns promise that resolves on resume
    {
        const ctrl = new PauseController();
        ctrl.pause();
        const p = ctrl.waitForResume();
        setTimeout(() => ctrl.resume(), 10);
        await p;
        assert(logger, 'waitForResume resolves after resume', ctrl.isPaused(), false);
    }
}

export default runPauseControllerTests;
