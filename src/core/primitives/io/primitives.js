/**
 * I/O primitives exported to Scheme: the port primitives (port_primitives.js),
 * and the two that ask the system -- `features`, the library system's, and
 * `read`, the reader's.
 */

import { portPrimitives } from './port_primitives.js';
import { systemLibrary } from '../../interpreter/library_seed.js';
import { callSchemeProcedure } from '../../interpreter/values.js';
import { callLibrarySystem, currentLibraryRegistry } from '../../interpreter/library_registry.js';
import { assertArity } from '../../interpreter/type_check.js';

/**
 * I/O primitives exported to Scheme.
 */
export const ioPrimitives = {
    ...portPrimitives,

    // The features are the library system's, in the current registry: what
    // `cond-expand` finds, `node` or `browser` and any a host added included.
    'features': (...args) => {
        assertArity('features', args, 0, 0);
        return callLibrarySystem('registry-feature-list', currentLibraryRegistry());
    },

    // The reader's door for `read`: a datum from a port, read by
    // `(scheme-js reader)` (`read-from-port` in reader.scm), which checks the
    // port too.
    '%read': (port) => callSchemeProcedure(systemLibrary(['scheme-js', 'reader']).get('read-from-port'), [port])
};
