/**
 * Special Forms Handler Registry
 * 
 * Encapsulates the registry of special form handlers with a clean API.
 * Handlers are registered during module initialization and looked up
 * during analysis.
 */

// Private Map - not directly exported
const SPECIAL_FORMS = new Map();

/**
 * Registers a handler for a special form.
 * @param {string} name - The special form name (e.g., 'if', 'lambda')
 * @param {Function} handler - Handler function: (exp, syntacticEnv, ctx) => Executable
 */
export function registerHandler(name, handler) {
    if (SPECIAL_FORMS.has(name)) {
        console.warn(`Overwriting handler for special form: ${name}`);
    }
    SPECIAL_FORMS.set(name, handler);
}

/**
 * Gets the handler for a special form.
 * @param {string} name - The special form name
 * @returns {Function|undefined} The handler function, or undefined if not found
 */
export function getHandler(name) {
    return SPECIAL_FORMS.get(name);
}

/**
 * Checks if a handler exists for a special form.
 * @param {string} name - The special form name
 * @returns {boolean} True if a handler is registered
 */
export function hasHandler(name) {
    return SPECIAL_FORMS.has(name);
}

/**
 * Gets all registered special form names.
 * Useful for debugging and introspection.
 * @returns {string[]} Array of registered form names
 */
export function getRegisteredForms() {
    return [...SPECIAL_FORMS.keys()];
}
