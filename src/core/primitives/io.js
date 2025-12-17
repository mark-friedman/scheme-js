/**
 * I/O Primitives for Scheme.
 * 
 * Provides basic input/output operations.
 */

/**
 * I/O primitives exported to Scheme.
 */
export const ioPrimitives = {
    /**
     * Displays a value.
     * @param {*} val - Value to display.
     * @returns {*} The value displayed.
     */
    'display': (val) => {
        console.log("DISPLAY:", val);
        return val;
    },

    /**
     * Outputs a newline.
     * @returns {boolean} Always returns true.
     */
    'newline': () => {
        console.log("");
        return true;
    }
};
