/**
 * Error Object Primitives for Scheme.
 *
 * What an error object is: error-object?, error-object-message,
 * error-object-irritants, file-error? and read-error?. Apart from raising,
 * which the interpreter performs (exception.js), so that compiled code, which
 * reads error objects as it runs, needs nothing of the interpreter to.
 */

import { SchemeError } from '../interpreter/errors.js';
import { list } from '../interpreter/cons.js';

/**
 * The error-object primitives.
 * @type {Object<string, Function>}
 */
export const errorObjectPrimitives = {
    /**
     * error-object?: Check if value is a SchemeError.
     */
    'error-object?': (obj) => obj instanceof SchemeError,

    /**
     * error-object-message: Get the error message.
     */
    'error-object-message': (obj) => {
        if (!(obj instanceof SchemeError)) {
            throw new SchemeError('error-object-message: expected error object', [obj]);
        }
        return obj.message;
    },

    /**
     * error-object-irritants: Get the error irritants as a list.
     */
    'error-object-irritants': (obj) => {
        if (!(obj instanceof SchemeError)) {
            throw new SchemeError('error-object-irritants: expected error object', [obj]);
        }
        return list(...obj.irritants);
    },

    /**
     * file-error?: Check if exception is a file-related error.
     * Returns #t for I/O errors like ENOENT, EACCES, etc.
     */
    'file-error?': (obj) => {
        if (obj instanceof SchemeError || obj instanceof Error) {
            const msg = obj.message || '';
            // Check for common Node.js file error codes
            return /ENOENT|EACCES|EEXIST|EISDIR|ENOTDIR|EMFILE|ENFILE|EBADF|EROFS|ENOSPC/.test(msg) ||
                /no such file|permission denied|file exists|is a directory|not a directory/.test(msg.toLowerCase()) ||
                /open|read|write|delete|rename|file/i.test(msg);
        }
        return false;
    },

    /**
     * read-error?: Check if exception is a read/parse error.
     * Returns #t for syntax errors, parse errors, etc.
     */
    'read-error?': (obj) => {
        if (obj instanceof SchemeError || obj instanceof Error) {
            const msg = obj.message || '';
            return /parse|syntax|unexpected|read|token|end of input/i.test(msg);
        }
        return false;
    }
};
