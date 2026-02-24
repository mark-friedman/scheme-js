/**
 * @fileoverview Pure rendering helper functions for the Scheme Stack sidebar.
 *
 * These functions are extracted from the sidebar panel logic so they can
 * be unit-tested without Chrome extension APIs. They handle formatting
 * of stack frames, variables, and source locations for display.
 *
 * Part of Phase 5: DevTools Extension — Scheme Stack Sidebar.
 */

// =========================================================================
// Text Formatting
// =========================================================================

/**
 * Formats a source location for display.
 * Extracts the basename from the filename and appends the line number.
 *
 * @param {Object|null} source - Source location { filename, line, column }
 * @returns {string} Formatted source like "factorial.scm:5" or "<unknown>"
 */
export function formatSource(source) {
    if (!source || !source.filename) return '<unknown>';
    const basename = source.filename.split('/').pop();
    return `${basename}:${source.line}`;
}

/**
 * Escapes HTML special characters to prevent XSS.
 *
 * @param {string} text - Raw text to escape
 * @returns {string} HTML-safe text
 */
export function escapeHtml(text) {
    return text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

// =========================================================================
// Value Formatting
// =========================================================================

/**
 * Formats a variable value for display with CSS type classification.
 *
 * @param {Object} variable - Variable descriptor { name, value, type, subtype }
 * @returns {{ display: string, cssClass: string }}
 */
export function formatValue(variable) {
    const { value, type, subtype } = variable;
    let cssClass;
    switch (type) {
        case 'number':
        case 'bigint':
            cssClass = 'number';
            break;
        case 'string':
            cssClass = 'string';
            break;
        case 'boolean':
            cssClass = 'boolean';
            break;
        case 'function':
            cssClass = 'function';
            break;
        case 'symbol':
            cssClass = 'symbol';
            break;
        case 'undefined':
            cssClass = 'undefined';
            break;
        default:
            cssClass = 'object';
            break;
    }
    return { display: value, cssClass };
}

// =========================================================================
// HTML Builders
// =========================================================================

/**
 * Builds the HTML for the frame list.
 * Frames are rendered top-to-bottom (most recent call first).
 * The top frame is marked as selected.
 *
 * @param {Array<{name: string, source: Object|null, tcoCount: number}>} frames
 *   Stack frames from __schemeDebug.getStack() (bottom-to-top order).
 * @returns {string} HTML string
 */
export function buildFrameListHtml(frames) {
    if (frames.length === 0) {
        return '<div class="empty-message">No Scheme frames</div>';
    }

    // Reverse: display top frame (most recent) first
    const reversed = [...frames].reverse();
    const parts = [];

    for (let i = 0; i < reversed.length; i++) {
        const frame = reversed[i];
        const isTop = i === 0;
        const originalIndex = frames.length - 1 - i;
        const selectedClass = isTop ? ' selected' : '';

        let html = `<div class="frame${selectedClass}" data-index="${originalIndex}">`;
        html += `<span class="frame-name">${escapeHtml(frame.name)}</span>`;

        // TCO badge
        if (frame.tcoCount > 0) {
            html += ` <span class="tco-badge">TCO×${frame.tcoCount}</span>`;
        }

        // Source location
        const sourceText = formatSource(frame.source);
        html += ` <span class="frame-source">${escapeHtml(sourceText)}</span>`;

        html += '</div>';
        parts.push(html);
    }

    return parts.join('\n');
}

/**
 * Builds the HTML for the variables section.
 *
 * @param {Array<{name: string, value: string, type: string, subtype: string|null}>} locals
 *   Variable list from __schemeDebug.getLocals().
 * @returns {string} HTML string
 */
export function buildVariablesHtml(locals) {
    if (locals.length === 0) {
        return '<div class="empty-message">No variables</div>';
    }

    const parts = [];
    for (const local of locals) {
        const { display, cssClass } = formatValue(local);
        parts.push(
            `<div class="variable">` +
            `<span class="var-name">${escapeHtml(local.name)}</span>` +
            `<span class="var-separator">: </span>` +
            `<span class="var-value ${cssClass}">${escapeHtml(display)}</span>` +
            `</div>`
        );
    }

    return parts.join('\n');
}
