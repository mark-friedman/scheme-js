/**
 * Analyzer Handler Registry Index
 * 
 * Initializes and registers all special form handlers.
 * Provides the main entry point for handler initialization.
 */

import { registerHandler, getHandler, hasHandler, getRegisteredForms } from './registry.js';
import { registerCoreForms, initCoreForms, analyzeBody, analyzeScopedBody } from './core_forms.js';
import { registerControlForms, initControlForms } from './control_forms.js';
import { registerModuleForms, initModuleForms } from './module_forms.js';

/**
 * Initializes all handler modules with dependencies from the main analyzer.
 * Must be called before any analysis occurs.
 * 
 * @param {Object} deps - Dependencies from the main analyzer
 * @param {Function} deps.analyze - The main analyze function
 * @param {Function} deps.generateUniqueName - Unique name generator
 */
export function initHandlers(deps) {
    initCoreForms(deps);
    initControlForms(deps);
    initModuleForms(deps);
}

/**
 * Registers all built-in special form handlers.
 * Should be called once during module initialization.
 */
export function registerAllHandlers() {
    registerCoreForms();
    registerControlForms();
    registerModuleForms();
}

// Re-export registry functions for convenience
export {
    registerHandler,
    getHandler,
    hasHandler,
    getRegisteredForms,
    analyzeBody,
    analyzeScopedBody
};
