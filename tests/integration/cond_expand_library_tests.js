import { assert, run, loadSpecialForms } from '../harness/helpers.js';
import { getLibraryExports, clearLibraryRegistry, withPrivateLibraries } from '../../src/core/interpreter/library_registry.js';
import { list, cons } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';

export async function runLibraryLoaderTests(interpreter, logger) {
    logger.title('cond-expand Library Tests');
    loadSpecialForms(interpreter, interpreter.globalEnv);

    // Test 1: Simple cond-expand in library
    logger.log('Testing simple cond-expand in library...');

    try {
        await run(interpreter, `
          (define-library (test simple)
            (import (scheme-js special-forms))
            (cond-expand
              (scheme-js
                (export x)
                (begin (define x 10)))
              (else
                (export y)
                (begin (define y 20)))))
        `);

        const exports = getLibraryExports(['test', 'simple']);
        assert(logger, 'simple cond-expand exports x', exports.has('x'), true);
        assert(logger, 'simple cond-expand does not export y', exports.has('y'), false);

    } catch (e) {
        logger.fail(`Simple cond-expand failed: ${e.message}`);
    }

    // Test 2: Nested cond-expand in library
    logger.log('Testing nested cond-expand in library...');
    try {
        await run(interpreter, `
          (define-library (test nested)
            (import (scheme-js special-forms))
            (cond-expand
              (scheme-js
                (cond-expand
                  (r7rs
                    (export a)
                    (begin (define a 100)))
                  (else
                    (export b)
                    (begin (define b 200)))))
              (else
                (export c)
                (begin (define c 300)))))
        `);

        const exports = getLibraryExports(['test', 'nested']);
        assert(logger, 'nested cond-expand exports a', exports.has('a'), true);
        assert(logger, 'nested cond-expand does not export b', exports.has('b'), false);
        assert(logger, 'nested cond-expand does not export c', exports.has('c'), false);

    } catch (e) {
        logger.fail(`Nested cond-expand failed: ${e.message}`);
    }

    // Test 3: (library <name>) holds for a library available for import, as
    // R7RS 4.2.1 says, not only for one already loaded. Whether one is
    // available is the file resolver's to say, so these resolve libraries
    // from a table of sources, synchronously.
    logger.log('Testing (library <name>) for libraries not yet loaded...');
    const sources = {
        'ce-probe available': '(define-library (ce-probe available) (export v) (import (scheme base)) (begin (define v 1)))',
        // What a resolver finding libraries by their last name part returns
        // for (ce-probe other): another library's source.
        'ce-probe other': '(define-library (ce-probe something-else) (export w) (begin (define w 2)))'
    };
    const resolver = (parts) => {
        const key = parts.map((p) => p?.name ?? String(p)).join(' ');
        if (sources[key] === undefined) throw new Error(`no library (${key})`);
        return sources[key];
    };
    const probe = (name) => run(interpreter, `(cond-expand ((library ${name}) 'available) (else 'unavailable))`).name;
    try {
        withPrivateLibraries({ resolver }, () => {
            assert(logger, '(library) holds for a library the resolver finds',
                probe('(ce-probe available)'), 'available');
            assert(logger, '(library) does not load the library it asks about',
                getLibraryExports(['ce-probe', 'available']), null);
            assert(logger, '(library) fails for a library the resolver does not find',
                probe('(ce-probe missing)'), 'unavailable');
            assert(logger, '(library) fails for a file declaring another library',
                probe('(ce-probe other)'), 'unavailable');
        });
        withPrivateLibraries({ resolver: async () => sources['ce-probe available'] }, () => {
            assert(logger, '(library) fails for what an asynchronous resolver would have to fetch',
                probe('(ce-probe available)'), 'unavailable');
        });
    } catch (e) {
        logger.fail(`(library <name>) availability failed: ${e.message}`);
    }
}
