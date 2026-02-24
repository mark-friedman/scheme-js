/**
 * @fileoverview Debug module barrel export.
 * Provides the main debugging components for scheme-js.
 */

export { DebugLevel, DebugLevelStack } from './debug_level.js';
export { BreakpointManager } from './breakpoint_manager.js';
export { StackTracer } from './stack_tracer.js';
export { PauseController } from './pause_controller.js';
export { DebugExceptionHandler } from './exception_handler.js';
export { SchemeDebugRuntime } from './scheme_debug_runtime.js';
export { DebugBackend, NoOpDebugBackend, TestDebugBackend } from './debug_backend.js';
export { StateInspector } from './state_inspector.js';
export { ReplDebugBackend } from './repl_debug_backend.js';
export { ReplDebugCommands } from './repl_debug_commands.js';

// Chrome DevTools integration (Phase 1 + Phase 2)
export { SchemeSourceRegistry } from './devtools/source_registry.js';
export { generateProbeScript, schemeUrlToProbeUrl } from './devtools/probe_generator.js';
export { encodeVLQ, generateProbeSourceMap } from './devtools/sourcemap_generator.js';
export { DevToolsDebugIntegration } from './devtools/devtools_debug.js';
export { createEnvProxy } from './devtools/env_proxy.js';
