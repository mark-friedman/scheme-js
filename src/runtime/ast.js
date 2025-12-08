/**
 * AST Module - Barrel File
 * 
 * This file re-exports all AST nodes and frames for backwards compatibility.
 * The actual implementations are split across:
 * - nodes.js: AST node classes (Literal, Variable, Lambda, etc.)
 * - frames.js: Continuation frame classes (AppFrame, LetFrame, etc.)
 * - frame_registry.js: Factory functions for frame creation
 * - winders.js: Dynamic-wind utilities
 */

// Re-export from nodes.js
export {
    Executable,
    Literal,
    Variable,
    Lambda,
    Let,
    LetRec,
    If,
    Set,
    Define,
    TailApp,
    CallCC,
    Begin,
    DynamicWindInit,
    RestoreContinuation,
    CallWithValuesNode
} from './nodes.js';


// Re-export from frames.js
export {
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
    CallWithValuesFrame
} from './frames.js';

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
