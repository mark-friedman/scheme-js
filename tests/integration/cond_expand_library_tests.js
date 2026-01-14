import { assert, run } from '../harness/helpers.js';
import { evaluateLibraryDefinitionSync, evaluateLibraryDefinition } from '../../src/core/interpreter/library_loader.js';
import { getLibraryExports, clearLibraryRegistry } from '../../src/core/interpreter/library_registry.js';
import { parseDefineLibrary } from '../../src/core/interpreter/library_parser.js';
import { list, cons } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';

export async function runLibraryLoaderTests(interpreter, logger) {
    logger.title('cond-expand Library Tests');

    // Test 1: Simple cond-expand in library
    logger.log('Testing simple cond-expand in library...');

    try {
        await run(interpreter, `
          (define-library (test simple)
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
}
