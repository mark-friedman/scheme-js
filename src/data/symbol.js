/**
 * Represents a Scheme symbol.
 * Symbols are unique objects identified by their name.
 */
export class Symbol {
    constructor(name) {
        this.name = name;
    }

    toString() {
        return this.name;
    }
}

class SymbolRegistry {
    constructor() {
        this.symbols = new Map();
    }

    intern(name) {
        if (!this.symbols.has(name)) {
            this.symbols.set(name, new Symbol(name));
        }
        return this.symbols.get(name);
    }
}

export const globalSymbolRegistry = new SymbolRegistry();

/**
 * Helper to get an interned symbol.
 */
export function intern(name) {
    return globalSymbolRegistry.intern(name);
}
