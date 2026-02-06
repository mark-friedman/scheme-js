/**
 * Stepables Barrel Module
 * 
 * This is the main entry point for stepable classes. It re-exports everything
 * from the three focused modules:
 * - stepables_base.js: Base class and register constants
 * - ast_nodes.js: AST node classes
 * - frames.js: Frame classes
 * 
 * Import from this file for backwards compatibility.
 */

// Re-export base module (constants and Executable)
export { ANS, CTL, ENV, FSTACK, Executable } from './stepables_base.js';

// Re-export all AST nodes
export {
    ensureExecutable,
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
    DefineLibraryNode,
    DynamicWindInit,
    RestoreContinuation,
    CallWithValuesNode,
    WithExceptionHandlerInit,
    RaiseNode,
    InvokeExceptionHandler
} from './ast_nodes.js';

// Re-export all frames
export {
    LetFrame,
    LetRecFrame,
    IfFrame,
    SetFrame,
    DefineFrame,
    BeginFrame,
    AppFrame,
    DebugExitFrame,
    DynamicWindSetupFrame,
    WindFrame,
    RestoreValueFrame,
    CallWithValuesFrame,
    ExceptionHandlerFrame,
    RaiseContinuableResumeFrame
} from './frames.js';
