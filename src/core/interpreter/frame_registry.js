/**
 * Frame Registry - Factory functions to break circular dependencies.
 * 
 * This module provides factory functions for creating frame instances.
 * It acts as an indirection layer to avoid circular imports within
 * stepables.js during module initialization.
 */

// Frame classes are registered here by stepables.js
let LetFrame, LetRecFrame, IfFrame, SetFrame, DefineFrame;
let AppFrame, BeginFrame, DynamicWindSetupFrame, WindFrame, RestoreValueFrame;
let CallWithValuesFrame, ExceptionHandlerFrame, RaiseContinuableResumeFrame;
let RaiseNonContinuableResumeFrame;

/**
 * Continues an application: evaluates any remaining operands, then applies.
 * See `continueApplication` in `frames.js`.
 *
 * Registered by `frames.js` alongside the frame classes, for the same reason:
 * `ast_nodes.js` cannot import `frames.js` directly without a cycle. This is an
 * `export let` rather than a forwarding function so that callers reach the real
 * implementation through the module's live binding, with no wrapper call on a
 * path taken by every single procedure application.
 *
 * @type {function(Array, number, Array, Object, Array, Object): boolean}
 */
export let continueApplication;

/**
 * Registers frame classes. Called by stepables.js during module initialization.
 * @param {Object} frames - Object containing all frame classes.
 */
export function registerFrames(frames) {
    LetFrame = frames.LetFrame;
    LetRecFrame = frames.LetRecFrame;
    IfFrame = frames.IfFrame;
    SetFrame = frames.SetFrame;
    DefineFrame = frames.DefineFrame;
    AppFrame = frames.AppFrame;
    BeginFrame = frames.BeginFrame;
    DynamicWindSetupFrame = frames.DynamicWindSetupFrame;
    WindFrame = frames.WindFrame;
    RestoreValueFrame = frames.RestoreValueFrame;
    CallWithValuesFrame = frames.CallWithValuesFrame;
    ExceptionHandlerFrame = frames.ExceptionHandlerFrame;
    RaiseContinuableResumeFrame = frames.RaiseContinuableResumeFrame;
    RaiseNonContinuableResumeFrame = frames.RaiseNonContinuableResumeFrame;
    continueApplication = frames.continueApplication;
}

// --- Factory Functions ---

/**
 * Creates a LetFrame instance.
 */
export function createLetFrame(varName, body, env) {
    return new LetFrame(varName, body, env);
}

/**
 * Creates a LetRecFrame instance.
 */
export function createLetRecFrame(varName, body, env) {
    return new LetRecFrame(varName, body, env);
}

/**
 * Creates an IfFrame instance.
 */
export function createIfFrame(consequent, alternative, env) {
    return new IfFrame(consequent, alternative, env);
}

/**
 * Creates a SetFrame instance.
 */
export function createSetFrame(name, env) {
    return new SetFrame(name, env);
}

/**
 * Creates a DefineFrame instance.
 */
export function createDefineFrame(name, env) {
    return new DefineFrame(name, env);
}

/**
 * Creates an AppFrame instance.
 * @param {Array<Executable>} exprs - Operator followed by operand expressions.
 * @param {number} index - Index within `exprs` awaiting a value.
 * @param {Array<*>} values - Values evaluated so far.
 * @param {Environment} env - The captured environment.
 * @returns {AppFrame} The new frame.
 */
export function createAppFrame(exprs, index, values, env) {
    return new AppFrame(exprs, index, values, env);
}

/**
 * Creates a BeginFrame instance.
 */
export function createBeginFrame(remainingExprs, env) {
    return new BeginFrame(remainingExprs, env);
}

/**
 * Creates a DynamicWindSetupFrame instance.
 */
export function createDynamicWindSetupFrame(before, thunk, after, env) {
    return new DynamicWindSetupFrame(before, thunk, after, env);
}

/**
 * Creates a WindFrame instance.
 */
export function createWindFrame(before, after, env) {
    return new WindFrame(before, after, env);
}

/**
 * Creates a RestoreValueFrame instance.
 */
export function createRestoreValueFrame(savedValue) {
    return new RestoreValueFrame(savedValue);
}

/**
 * Gets the WindFrame class (for instanceof checks).
 */
export function getWindFrameClass() {
    return WindFrame;
}

/**
 * Creates a CallWithValuesFrame instance.
 */
export function createCallWithValuesFrame(consumer, env) {
    return new CallWithValuesFrame(consumer, env);
}

/**
 * Creates an ExceptionHandlerFrame instance.
 */
export function createExceptionHandlerFrame(handler, env) {
    return new ExceptionHandlerFrame(handler, env);
}

/**
 * Creates a RaiseContinuableResumeFrame instance.
 */
export function createRaiseContinuableResumeFrame(savedFrames, env) {
    return new RaiseContinuableResumeFrame(savedFrames, env);
}

/**
 * Creates a RaiseNonContinuableResumeFrame instance.
 */
export function createRaiseNonContinuableResumeFrame(exception, env) {
    return new RaiseNonContinuableResumeFrame(exception, env);
}

/**
 * Gets the ExceptionHandlerFrame class (for instanceof checks).
 */
export function getExceptionHandlerFrameClass() {
    return ExceptionHandlerFrame;
}
