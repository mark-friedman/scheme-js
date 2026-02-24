/**
 * @fileoverview Tests for the Scheme Stack sidebar rendering logic.
 *
 * These tests verify the pure data-formatting and rendering helper
 * functions used by the Chrome DevTools extension's sidebar panel.
 * They do NOT require Chrome extension APIs — all browser-specific
 * APIs are mocked.
 *
 * Part of Phase 5: DevTools Extension — Scheme Stack Sidebar.
 */

import { assert } from '../../harness/helpers.js';

// Import rendering helpers from the shared module.
// These are the testable pure functions extracted from sidebar.js.
import {
    formatSource,
    escapeHtml,
    buildFrameListHtml,
    buildVariablesHtml,
    formatValue
} from '../../../src/debug/devtools/sidebar_helpers.js';

/**
 * Runs all extension sidebar rendering tests.
 * @param {Object} logger - Test logger
 */
export async function runExtensionSidebarTests(logger) {

    // =========================================================================
    // formatSource
    // =========================================================================
    logger.title('Sidebar Helpers - formatSource');

    // Test: formats source with filename and line
    {
        const result = formatSource({
            filename: 'scheme://app/factorial.scm',
            line: 5,
            column: 3
        });
        assert(logger, 'formats filename:line', result, 'factorial.scm:5');
    }

    // Test: formats null source
    {
        const result = formatSource(null);
        assert(logger, 'null source returns empty', result, '<unknown>');
    }

    // Test: extracts basename from full path
    {
        const result = formatSource({
            filename: 'scheme://lib/scheme/base.sld',
            line: 42,
            column: 1
        });
        assert(logger, 'extracts basename', result, 'base.sld:42');
    }

    // =========================================================================
    // escapeHtml
    // =========================================================================
    logger.title('Sidebar Helpers - escapeHtml');

    // Test: escapes special characters
    {
        assert(logger, 'escapes <', escapeHtml('<'), '&lt;');
        assert(logger, 'escapes >', escapeHtml('>'), '&gt;');
        assert(logger, 'escapes &', escapeHtml('&'), '&amp;');
        assert(logger, 'escapes "', escapeHtml('"'), '&quot;');
    }

    // Test: passes through normal text
    {
        assert(logger, 'normal text unchanged', escapeHtml('factorial'), 'factorial');
    }

    // Test: handles mixed content
    {
        const result = escapeHtml('#<procedure "foo">');
        assert(logger, 'mixed escaping', result, '#&lt;procedure &quot;foo&quot;&gt;');
    }

    // =========================================================================
    // formatValue
    // =========================================================================
    logger.title('Sidebar Helpers - formatValue');

    // Test: formats number
    {
        const result = formatValue({ name: 'x', value: '42', type: 'number' });
        assert(logger, 'number format', result.cssClass, 'number');
        assert(logger, 'number display', result.display, '42');
    }

    // Test: formats string
    {
        const result = formatValue({ name: 'msg', value: '"hello"', type: 'string' });
        assert(logger, 'string format', result.cssClass, 'string');
    }

    // Test: formats function
    {
        const result = formatValue({
            name: 'f',
            value: '#<procedure (x y)>',
            type: 'function',
            subtype: 'closure'
        });
        assert(logger, 'function format', result.cssClass, 'function');
    }

    // Test: formats boolean
    {
        const result = formatValue({ name: 'flag', value: 'true', type: 'boolean' });
        assert(logger, 'boolean format', result.cssClass, 'boolean');
    }

    // Test: formats unknown type
    {
        const result = formatValue({ name: 'x', value: 'something', type: 'object' });
        assert(logger, 'object format', result.cssClass, 'object');
    }

    // =========================================================================
    // buildFrameListHtml
    // =========================================================================
    logger.title('Sidebar Helpers - buildFrameListHtml');

    // Test: empty stack
    {
        const html = buildFrameListHtml([]);
        assert(logger, 'empty stack message', html.includes('No Scheme frames'), true);
    }

    // Test: single frame
    {
        const frames = [
            { name: 'main', source: { filename: 'app.scm', line: 1 }, tcoCount: 0 }
        ];
        const html = buildFrameListHtml(frames);
        assert(logger, 'contains frame name', html.includes('main'), true);
        assert(logger, 'contains source', html.includes('app.scm:1'), true);
        assert(logger, 'no TCO badge for tcoCount 0', html.includes('tco-badge') === false, true);
    }

    // Test: multiple frames rendered top-to-bottom
    {
        const frames = [
            { name: 'outer', source: { filename: 'a.scm', line: 1 }, tcoCount: 0 },
            { name: 'middle', source: { filename: 'b.scm', line: 5 }, tcoCount: 0 },
            { name: 'inner', source: { filename: 'c.scm', line: 10 }, tcoCount: 0 }
        ];
        const html = buildFrameListHtml(frames);

        // Top frame (inner) should appear first in HTML
        const innerPos = html.indexOf('inner');
        const outerPos = html.indexOf('outer');
        assert(logger, 'top frame appears first', innerPos < outerPos, true);
    }

    // Test: TCO badge renders
    {
        const frames = [
            { name: 'loop', source: { filename: 'app.scm', line: 5 }, tcoCount: 3 }
        ];
        const html = buildFrameListHtml(frames);
        assert(logger, 'TCO badge rendered', html.includes('tco-badge'), true);
        assert(logger, 'TCO count displayed', html.includes('TCO×3'), true);
    }

    // Test: top frame is marked selected
    {
        const frames = [
            { name: 'bottom', source: null, tcoCount: 0 },
            { name: 'top', source: null, tcoCount: 0 }
        ];
        const html = buildFrameListHtml(frames);
        // The selected frame div should contain 'top'
        const selectedMatch = html.match(/class="frame selected"[^>]*>[\s\S]*?<\/div>/);
        assert(logger, 'selected frame exists', !!selectedMatch, true);
        assert(logger, 'selected frame is top', selectedMatch[0].includes('top'), true);
    }

    // =========================================================================
    // buildVariablesHtml
    // =========================================================================
    logger.title('Sidebar Helpers - buildVariablesHtml');

    // Test: empty locals
    {
        const html = buildVariablesHtml([]);
        assert(logger, 'empty locals message', html.includes('No variables'), true);
    }

    // Test: renders variables
    {
        const locals = [
            { name: 'n', value: '5', type: 'number' },
            { name: 'msg', value: '"hello"', type: 'string' }
        ];
        const html = buildVariablesHtml(locals);
        assert(logger, 'contains var name n', html.includes('n'), true);
        assert(logger, 'contains var value 5', html.includes('5'), true);
        assert(logger, 'contains var name msg', html.includes('msg'), true);
    }

    // Test: escapes HTML in values
    {
        const locals = [
            { name: 'x', value: '#<procedure>', type: 'function', subtype: 'closure' }
        ];
        const html = buildVariablesHtml(locals);
        assert(logger, 'escapes angle brackets in value',
            html.includes('&lt;procedure&gt;'), true);
    }
}
