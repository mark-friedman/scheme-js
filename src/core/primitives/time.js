/**
 * Time Primitives
 * 
 * R7RS (scheme time) procedures.
 */

import { assertArity } from '../interpreter/type_check.js';

export const timePrimitives = {
    /**
     * current-second: Returns the current time in seconds since epoch.
     * R7RS requires no arguments.
     * @returns {number} Seconds since 1970-01-01 00:00:00 UTC.
     */
    'current-second': (...args) => {
        assertArity('current-second', args, 0, 0);
        return Date.now() / 1000;
    },

    /**
     * current-jiffy: Returns the current jiffy count.
     * A jiffy is the smallest measurable time unit - we use milliseconds.
     * R7RS requires no arguments.
     * @returns {number} Current time in milliseconds.
     */
    'current-jiffy': (...args) => {
        assertArity('current-jiffy', args, 0, 0);
        // Use performance.now() if available for higher precision
        if (typeof performance !== 'undefined' && performance.now) {
            return Math.floor(performance.now());
        }
        return Date.now();
    },

    /**
     * jiffies-per-second: Returns the number of jiffies per second.
     * R7RS requires no arguments.
     * @returns {number} 1000 (milliseconds per second).
     */
    'jiffies-per-second': (...args) => {
        assertArity('jiffies-per-second', args, 0, 0);
        return 1000;
    }
};
