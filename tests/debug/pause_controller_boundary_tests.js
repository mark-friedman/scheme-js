import { assert } from '../harness/helpers.js';
import { PauseController } from '../../src/debug/pause_controller.js';

export function runPauseControllerBoundaryTests(logger) {
    logger.title('Running PauseController Boundary Tests...');

    const pc = new PauseController();
    assert(logger, "Initial state running", pc.getState(), 'running');
    
    // Simulate boundary step
    pc.stepInto();
    assert(logger, "State stepping", pc.getState(), 'stepping');
    assert(logger, "Step mode into", pc.getStepMode(), 'into');

    // Trigger boundary pause
    const func = function() {};
    pc.pause('boundary', { funcName: 'test', func });
    assert(logger, "State paused on boundary", pc.getState(), 'paused');
    assert(logger, "Pause reason boundary", pc.getPauseReason(), 'boundary');
    assert(logger, "Pause data funcName", pc.getPauseData().funcName, 'test');
    assert(logger, "Pause data func", pc.getPauseData().func, func);

    // Resume
    pc.resume();
    assert(logger, "Resumed running", pc.getState(), 'running');
}
