/**
 * Control Form Handlers
 * 
 * Handlers for control flow special forms:
 * call/cc, call-with-values, dynamic-wind, with-exception-handler, raise
 */

import {
    CallCCNode,
    CallWithValuesNode,
    DynamicWindInit,
    WithExceptionHandlerInit,
    RaiseNode
} from '../ast.js';
import { cadr, caddr, cadddr } from '../cons.js';
import { registerHandler } from './registry.js';

// Set by analyzer initialization
let analyze;

/**
 * Initializes the control forms with dependencies from the main analyzer.
 * @param {Object} deps - Dependencies
 */
export function initControlForms(deps) {
    analyze = deps.analyze;
}

// =============================================================================
// Handler Functions
// =============================================================================

function analyzeCallCC(exp, syntacticEnv, ctx) {
    const proc = analyze(cadr(exp), syntacticEnv, ctx);
    return new CallCCNode(proc);
}

function analyzeCallWithValues(exp, syntacticEnv, ctx) {
    const producer = analyze(cadr(exp), syntacticEnv, ctx);
    const consumer = analyze(caddr(exp), syntacticEnv, ctx);
    return new CallWithValuesNode(producer, consumer);
}

function analyzeDynamicWind(exp, syntacticEnv, ctx) {
    const before = analyze(cadr(exp), syntacticEnv, ctx);
    const thunk = analyze(caddr(exp), syntacticEnv, ctx);
    const after = analyze(cadddr(exp), syntacticEnv, ctx);
    return new DynamicWindInit(before, thunk, after);
}

function analyzeWithExceptionHandler(exp, syntacticEnv, ctx) {
    const handler = analyze(cadr(exp), syntacticEnv, ctx);
    const thunk = analyze(caddr(exp), syntacticEnv, ctx);
    return new WithExceptionHandlerInit(handler, thunk);
}

function analyzeRaise(exp, syntacticEnv, ctx, continuable = false) {
    const obj = analyze(cadr(exp), syntacticEnv, ctx);
    return new RaiseNode(obj, continuable);
}

function analyzeRaiseContinuable(exp, syntacticEnv, ctx) {
    return analyzeRaise(exp, syntacticEnv, ctx, true);
}

// =============================================================================
// Registration
// =============================================================================

export function registerControlForms() {
    // These are all handled as primitives in control.js or exception.js
    // and should not be macros in this implementation.
    // registerHandler('call/cc', analyzeCallCC);
    // registerHandler('call-with-current-continuation', analyzeCallCC);
    // registerHandler('call-with-values', analyzeCallWithValues);
    // registerHandler('dynamic-wind', analyzeDynamicWind);
    // registerHandler('with-exception-handler', analyzeWithExceptionHandler);
    // registerHandler('raise', analyzeRaise);
    // registerHandler('raise-continuable', analyzeRaiseContinuable);
}

export {
    analyzeCallCC,
    analyzeCallWithValues,
    analyzeDynamicWind,
    analyzeWithExceptionHandler,
    analyzeRaise
};
