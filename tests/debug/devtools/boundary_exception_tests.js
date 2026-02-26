/**
 * @fileoverview Boundary & Exception Tests for Chrome DevTools Debugging.
 *
 * Tests the JS ↔ Scheme boundary crossing and exception behavior,
 * ensuring DevTools breaks correctly and aborts stepping on non-local
 * jumps (e.g., call/cc). Part of Phase 6.
 */

import { assert } from '../../harness/helpers.js';
import { SchemeSourceRegistry } from '../../../src/debug/devtools/source_registry.js';
import { DevToolsDebugIntegration } from '../../../src/debug/devtools/devtools_debug.js';
import { SchemeDebugRuntime } from '../../../src/debug/scheme_debug_runtime.js';
import { createInterpreter } from '../../../src/core/interpreter/index.js';
// Side-effect import: installs globalThis.__schemeProbeRuntime
import '../../../src/debug/devtools/probe_runtime.js';

function createTestSetup() {
    const { interpreter, env } = createInterpreter();
    const registry = new SchemeSourceRegistry();
    const devtools = new DevToolsDebugIntegration(registry);
    devtools.enable();

    const debugRuntime = new SchemeDebugRuntime();
    debugRuntime.enable();

    // Enable break on caught and uncaught for tests
    debugRuntime.exceptionHandler.breakOnCaughtException = true;
    debugRuntime.exceptionHandler.breakOnUncaughtException = true;

    interpreter.setDebugRuntime(debugRuntime);
    debugRuntime.setDevToolsIntegration(devtools);
    interpreter.devtoolsDebug = devtools;

    return { interpreter, env, devtools, registry, debugRuntime };
}

export async function runBoundaryTests(logger) {
    logger.title('DevTools - Boundary Tests (JS ↔ Scheme)');

    // Test JS calling Scheme (JS -> Scheme)
    {
        const { interpreter, devtools } = createTestSetup();

        // Define a scheme procedure
        await interpreter.evaluateStringDebug(`
            (define (my-func x)
              (+ x 1))
        `, { sourceUrl: 'scheme://test/js-to-scheme.scm' });

        const myFunc = interpreter.globalEnv.lookup('my-func');

        devtools.enableTracking();
        // Call it from JS
        const result = myFunc(42);

        assert(logger, 'JS -> Scheme call returns correct result', result, 43);

        const history = devtools.disableTracking();
        // It should hit line 2 (the (+ x 1) expression)
        assert(logger, 'JS -> Scheme call fires probe', history.includes('scheme://test/js-to-scheme.scm:3:15'), true);
    }

    // Test Scheme calling JS (Scheme -> JS)
    {
        const { interpreter, devtools, env } = createTestSetup();

        // Inject a JS function
        let jsCalled = false;
        env.define('js-callback', (val) => {
            jsCalled = true;
            return val * 2;
        });

        devtools.enableTracking();

        const code = `
            (define x 10)
            (js-callback x)
        `;
        const result = await interpreter.evaluateStringDebug(code, { sourceUrl: 'scheme://test/scheme-to-js.scm' });

        assert(logger, 'Scheme -> JS returns correct result', result, 20);
        assert(logger, 'JS callback was executed', jsCalled, true);

        const history = devtools.disableTracking();
        // It should hit the define and the js-callback invocation
        assert(logger, 'Scheme -> JS evaluates before call', history.length > 0, true);
    }
}

export async function runExceptionTests(logger) {
    logger.title('DevTools - Exception Handling Tests');

    // Test exception handling with DevTools enabled — onException fires probe
    // without throwing, so the original SchemeError propagates normally.
    {
        const { interpreter, devtools } = createTestSetup();

        const code = `
            (define (fail)
              (error "Boom"))
            (fail)
        `;

        let caughtSchemeError = false;
        try {
            await interpreter.evaluateStringDebug(code, { sourceUrl: 'scheme://test/error.scm' });
        } catch (e) {
            // The original SchemeError should propagate (not SchemeRuntimeException)
            if (e.message && e.message.includes('Boom')) {
                caughtSchemeError = true;
            }
        }

        assert(logger, 'onException lets original SchemeError propagate', caughtSchemeError, true);
    }

    // Test abortStepping on ContinuationUnwind
    {
        const { interpreter, devtools } = createTestSetup();

        let abortCalled = false;
        const originalAbort = globalThis.__schemeProbeRuntime.abortStepping;
        globalThis.__schemeProbeRuntime.abortStepping = () => {
            abortCalled = true;
        };

        try {
            const code = `
                (define k-save #f)
                (+ 1 (call/cc (lambda (k) (set! k-save k) 2)))
                (if k-save
                    (let ((k k-save))
                      (set! k-save #f)
                      (k 5)))
            `;
            await interpreter.evaluateStringDebug(code, { sourceUrl: 'scheme://test/callcc.scm' });

            assert(logger, 'abortStepping was called on ContinuationUnwind via call/cc', abortCalled, true);
        } finally {
            globalThis.__schemeProbeRuntime.abortStepping = originalAbort;
        }
    }
}
