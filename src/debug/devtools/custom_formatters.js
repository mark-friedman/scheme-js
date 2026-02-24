/**
 * @fileoverview Chrome DevTools custom formatters for Scheme values.
 *
 * Registers a `window.devtoolsFormatters` entry that renders
 * SchemeDisplayValue objects as unquoted styled text in the Scope pane,
 * Console, and variable hover tooltips.
 *
 * Users must enable "Custom formatters" in DevTools Settings > Console
 * for this to take effect. Without it, values fall back to the hybrid
 * display (JS primitives natively, Scheme types as quoted strings).
 *
 * @see https://docs.google.com/document/d/1FTascZXT9cxfetuPRT2eXPQKXui4nWFivUnS_335T3U
 */

// =============================================================================
// Style Constants
// =============================================================================

/**
 * Styling for Scheme values in the custom formatter.
 * Uses a monospaced font with Scheme-appropriate coloring.
 * @type {Object<string, string>}
 */
const STYLES = {
    // General Scheme value — dark mode friendly blue-ish
    default: 'color: #9cdcfe; font-family: monospace; font-style: italic;',
    // Number types — orange/amber
    number: 'color: #b5cea8; font-family: monospace;',
    // String type - uses default DevTools string color
    string: 'color: #ce9178; font-family: monospace;',
    // Symbol type — teal
    symbol: 'color: #4ec9b0; font-family: monospace;',
    // Boolean — blue
    boolean: 'color: #569cd6; font-family: monospace;',
    // Procedure/closure — purple
    closure: 'color: #c586c0; font-family: monospace; font-style: italic;',
    continuation: 'color: #c586c0; font-family: monospace; font-style: italic;',
    procedure: 'color: #c586c0; font-family: monospace; font-style: italic;',
    // List/pair — default
    pair: 'color: #9cdcfe; font-family: monospace;',
    list: 'color: #9cdcfe; font-family: monospace;',
    // Char — green
    char: 'color: #6a9955; font-family: monospace;',
    // Null — gray
    null: 'color: #808080; font-family: monospace;',
    // Vector — default
    vector: 'color: #9cdcfe; font-family: monospace;',
    // Rational/Complex — number color
    rational: 'color: #b5cea8; font-family: monospace;',
    complex: 'color: #b5cea8; font-family: monospace;',
    // Integer (large BigInt) — number color
    integer: 'color: #b5cea8; font-family: monospace;',
    // Record — teal
    record: 'color: #4ec9b0; font-family: monospace; font-style: italic;',
    // Object fallback
    object: 'color: #9cdcfe; font-family: monospace;',
    // Values
    values: 'color: #9cdcfe; font-family: monospace;',
};

// =============================================================================
// Custom Formatter
// =============================================================================

/**
 * The custom formatter object for Chrome DevTools.
 * Detects SchemeDisplayValue objects and renders them as styled text.
 */
const schemeFormatter = {
    /**
     * Returns a JsonML header for SchemeDisplayValue objects.
     * Returns null for non-Scheme objects (allowing other formatters to handle them).
     *
     * @param {*} obj - The object to format
     * @returns {Array|null} JsonML array or null
     */
    header(obj) {
        // Only handle SchemeDisplayValue objects
        if (!obj || !obj.__schemeDisplayValue) {
            return null;
        }

        const style = STYLES[obj.schemeType] || STYLES.default;
        return ['span', { style }, obj.displayString];
    },

    /**
     * Whether the formatted object has an expandable body.
     * Currently returns false — expandable tree views for nested
     * structures are planned for Phase 8.
     *
     * @param {*} obj - The object to check
     * @returns {boolean}
     */
    hasBody(obj) {
        return false;
    },

    /**
     * Returns the expandable body content.
     * Not used currently (hasBody returns false).
     *
     * @param {*} obj - The object to expand
     * @returns {Array|null} JsonML array or null
     */
    body(obj) {
        return null;
    }
};

// =============================================================================
// Installation
// =============================================================================

/**
 * Installs the Scheme custom formatter into `globalThis.devtoolsFormatters`.
 *
 * Safe to call multiple times — checks for duplicate installation.
 * Safe in non-browser environments (Node.js) — no-ops gracefully.
 */
export function installCustomFormatters() {
    // Initialize the array if it doesn't exist
    if (!globalThis.devtoolsFormatters) {
        globalThis.devtoolsFormatters = [];
    }

    // Avoid duplicate installation
    if (globalThis.devtoolsFormatters.includes(schemeFormatter)) {
        return;
    }

    globalThis.devtoolsFormatters.push(schemeFormatter);
}

/**
 * Removes the Scheme custom formatter from `globalThis.devtoolsFormatters`.
 * Useful for cleanup in tests.
 */
export function removeCustomFormatters() {
    if (!globalThis.devtoolsFormatters) return;
    const idx = globalThis.devtoolsFormatters.indexOf(schemeFormatter);
    if (idx !== -1) {
        globalThis.devtoolsFormatters.splice(idx, 1);
    }
}
