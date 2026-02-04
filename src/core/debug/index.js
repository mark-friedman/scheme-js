/**
 * @fileoverview Debug module barrel export.
 * Provides the main debugging components for scheme-js.
 */

export { BreakpointManager } from './breakpoint_manager.js';
export { StackTracer } from './stack_tracer.js';
export { PauseController } from './pause_controller.js';
export { SchemeDebugRuntime } from './scheme_debug_runtime.js';
export { DebugBackend, NoOpDebugBackend, TestDebugBackend } from './debug_backend.js';
