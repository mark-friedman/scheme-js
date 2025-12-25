/**
 * AST Module - Barrel File
 * 
 * This file re-exports all stepable classes for backwards compatibility.
 * The actual implementations are in:
 * - stepables.js: All AST nodes and continuation frames
 * - frame_registry.js: Factory functions for frame creation
 * - winders.js: Dynamic-wind utilities
 */

// Re-export everything from stepables.js
export {
    // Register constants
    ANS,
    CTL,
    ENV,
    FSTACK,

    // Base class
    Executable,

    // AST Nodes
    LiteralNode,
    VariableNode,
    ScopedVariable,
    LambdaNode,
    LetNode,
    LetRecNode,
    IfNode,
    SetNode,
    DefineNode,
    TailAppNode,
    CallCCNode,
    BeginNode,
    ImportNode,
    DynamicWindInit,
    RestoreContinuation,
    CallWithValuesNode,
    WithExceptionHandlerInit,
    RaiseNode,
    InvokeExceptionHandler,

    // Frames
    LetFrame,
    LetRecFrame,
    IfFrame,
    SetFrame,
    DefineFrame,
    AppFrame,
    BeginFrame,
    DynamicWindSetupFrame,
    WindFrame,
    RestoreValueFrame,
    CallWithValuesFrame,
    ExceptionHandlerFrame,
    RaiseContinuableResumeFrame
} from './stepables.js';

// Re-export from winders.js
export { computeWindActions, findCommonAncestorIndex } from './winders.js';

// Re-export from frame_registry.js (for advanced use cases)
export {
    createLetFrame,
    createLetRecFrame,
    createIfFrame,
    createSetFrame,
    createDefineFrame,
    createAppFrame,
    createBeginFrame,
    createDynamicWindSetupFrame,
    createWindFrame,
    createRestoreValueFrame,
    createCallWithValuesFrame,
    getWindFrameClass
} from './frame_registry.js';

