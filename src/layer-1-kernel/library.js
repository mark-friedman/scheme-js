/**
 * A minimal library registry for Layer 1.
 * Supports loading native JS modules.
 */
export class LibraryRegistry {
    constructor() {
        this.libraries = new Map();
    }

    /**
     * Defines a library (placeholder for future layers).
     * @param {string} name 
     * @param {object} library 
     */
    define(name, library) {
        this.libraries.set(name, library);
    }

    /**
     * Imports a native JS module and extends the environment.
     * @param {string} path - Path to the JS module.
     * @param {Environment} env - The environment to extend.
     */
    async importNative(path, env) {
        // In a real implementation, this would use dynamic import().
        // However, dynamic import paths must be relative to the current file
        // or absolute.
        // For now, we'll assume the path is resolvable or we might need a loader.

        try {
            const module = await import(path);
            if (module.install) {
                module.install(env);
            } else if (module.default) {
                // If default export is a function, call it?
                // Or if it's an object of primitives?
                // Let's assume it exports a 'install' function or object of primitives.
                // If it's just primitives, we can add them.
                // For now, let's just log.
                console.log(`Imported native module: ${path}`);
            }
        } catch (e) {
            console.error(`Failed to import native module: ${path}`, e);
            throw e;
        }
    }
}
