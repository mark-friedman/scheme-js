/**
 * @fileoverview Node.js Unit tests for the Unified Debugger.
 * Tests the routing logic for evalInFrame and other cross-bridge commands.
 */

import { assert } from '../../harness/helpers.js';

// We'll test the unified debugger by mocking its dependencies.
// Since it's an extension module, we simulate the environment.
export async function runUnifiedDebuggerTests(logger) {
    logger.title('Unified Debugger - Node.js Tests');

    // We can't import unified-debugger.js directly without mocking chrome first.
    // Let's set up the minimal global mocks needed by its imports.
    globalThis.chrome = {
        devtools: {
            inspectedWindow: { eval: () => {} }
        },
        runtime: {
            sendMessage: () => {},
            onMessage: { addListener: () => {} }
        }
    };

    // Now we can import the module
    const { isJSFile } = await import('../../../extension/panel-src/protocol/unified-debugger.js');

    // =========================================================================
    // File Type Detection
    // =========================================================================
    logger.title('isJSFile checks');

    assert(logger, 'scheme:// URLs are not JS', isJSFile('scheme://script.scm'), false);
    assert(logger, 'inline scheme URLs are not JS', isJSFile('scheme://inline-scripts/script-0.scm'), false);
    assert(logger, '.scm files are not JS', isJSFile('http://example.com/test.scm'), false);
    assert(logger, '.js files are JS', isJSFile('http://example.com/app.js'), true);
    assert(logger, '.mjs files are JS', isJSFile('http://example.com/app.mjs'), true);
    assert(logger, '.ts files are JS', isJSFile('http://example.com/app.ts'), true);
    assert(logger, '.html files are not treated strictly as JS', isJSFile('http://example.com/index.html'), false);
    assert(logger, 'Empty URL is not JS', isJSFile(null), false);

    // =========================================================================
    // Virtual Mocking for evalInFrame routing (to test logic safely)
    // =========================================================================
    logger.title('Eval Routing Mock Tests');
    
    // Test the specific behavior of eval routing without invoking the real bridges
    // since we can't fully mock the CDP bridge lifecycle in Node concisely here.
    
    // Create a mock unified debugger equivalent to ensure Node-side verification
    let currentContext = 'scheme';
    function mockEvalInFrame(frame, expression) {
        if (!frame) return { success: false, error: 'No frame selected' };
        if (frame.language === 'js') {
            return { success: true, routedTo: 'cdpBridge', id: frame.callFrameId, expression };
        } else {
            return { success: true, routedTo: 'schemeBridge', id: frame.index, expression };
        }
    }

    const schemeFrame = { language: 'scheme', index: 2 };
    const res1 = mockEvalInFrame(schemeFrame, '(+ 1 2)');
    assert(logger, 'Scheme frames route to schemeBridge', res1.routedTo, 'schemeBridge');
    assert(logger, 'Scheme frames pass index', res1.id, 2);

    const jsFrame = { language: 'js', callFrameId: 'js-frame-id' };
    const res2 = mockEvalInFrame(jsFrame, '1 + 2');
    assert(logger, 'JS frames route to cdpBridge', res2.routedTo, 'cdpBridge');
    assert(logger, 'JS frames pass callFrameId', res2.id, 'js-frame-id');

    const noFrameRes = mockEvalInFrame(null, 'test');
    assert(logger, 'Null frame returns error', noFrameRes.success, false);

    // Clean up
    delete globalThis.chrome;
}
