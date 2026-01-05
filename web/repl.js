import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { prettyPrint } from '../src/core/interpreter/printer.js';
import { isCompleteExpression, findMatchingDelimiter } from '../src/core/interpreter/expression_utils.js';

/**
 * Shell-style REPL UI with:
 * - Multiline expressions stay in input until complete
 * - Rainbow parentheses (6 colors by depth)
 * - Paren match highlighting
 * - Auto-indentation on Enter
 * 
 * Architecture:
 * - Single contenteditable for input (no innerHTML manipulation)
 * - Separate highlight layer behind input (CSS Grid stacking)
 * - Prompt column on left shows > and ... prompts
 * - All text manipulation via textContent only
 */
export function setupRepl(interpreter, globalEnv) {
    // DOM elements
    const shell = document.getElementById('repl-shell');
    const history = document.getElementById('repl-history');
    const inputArea = document.getElementById('repl-input-area');
    const highlightLayer = document.getElementById('repl-highlight-layer');
    const promptColumn = document.getElementById('repl-prompt-column');
    const replInput = document.getElementById('repl-input');
    const replRunBtn = document.getElementById('repl-run-btn');

    const PAREN_COLORS = 6;

    // ==================== History Functions ====================

    /**
     * Add an entry to the history area
     */
    function addToHistory(content, type) {
        const div = document.createElement('div');

        if (type === 'input') {
            div.className = 'repl-history-multiline';
            const lines = content.split('\n');
            let depth = 0;

            lines.forEach((line, i) => {
                const lineDiv = document.createElement('div');
                lineDiv.className = 'repl-history-line';

                const promptSpan = document.createElement('span');
                promptSpan.className = i === 0 ? 'repl-prompt' : 'repl-prompt continuation';
                promptSpan.textContent = i === 0 ? '>' : '...';

                const contentSpan = document.createElement('span');
                contentSpan.className = 'repl-history-input';
                contentSpan.innerHTML = renderRainbowParens(line, -1, depth);

                // Update depth for next line
                depth = calculateDepthAfterLine(line, depth);

                lineDiv.appendChild(promptSpan);
                lineDiv.appendChild(contentSpan);
                div.appendChild(lineDiv);
            });
        } else if (type === 'result') {
            div.className = 'repl-result';
            div.textContent = content;
        } else if (type === 'error') {
            div.className = 'repl-error';
            div.textContent = content;
        }

        history.appendChild(div);
        shell.scrollTop = shell.scrollHeight;
    }

    // ==================== Depth Calculation ====================

    /**
     * Calculate paren depth after processing a line, starting from initialDepth
     */
    function calculateDepthAfterLine(line, initialDepth) {
        let depth = initialDepth;
        let inString = false;
        let inComment = false;

        for (let i = 0; i < line.length; i++) {
            const char = line[i];

            if (inComment) continue;

            if (inString) {
                if (char === '\\' && i + 1 < line.length) { i++; continue; }
                if (char === '"') inString = false;
                continue;
            }

            if (char === ';') { inComment = true; continue; }
            if (char === '"') { inString = true; continue; }
            if (char === '(') depth++;
            if (char === ')') depth = Math.max(0, depth - 1);
        }

        return depth;
    }

    /**
     * Calculate paren depth at end of text
     */
    function calculateDepth(text) {
        return calculateDepthAfterLine(text.replace(/\n/g, ''), 0);
    }

    // ==================== Prompt Management ====================

    /**
     * Update prompt column to match number of lines in input
     */
    function updatePrompts() {
        const text = inputArea.textContent || '';
        const lineCount = (text.match(/\n/g) || []).length + 1;
        const prompts = promptColumn.getElementsByClassName('repl-prompt');
        const currentCount = prompts.length;

        // Add prompts if needed
        while (prompts.length < lineCount) {
            const p = document.createElement('span');
            p.className = 'repl-prompt continuation';
            p.textContent = '...';
            promptColumn.appendChild(p);
        }

        // Remove prompts if needed
        while (prompts.length > lineCount) {
            promptColumn.removeChild(promptColumn.lastChild);
        }
    }

    /**
     * Reset prompts to initial state (single >)
     */
    function resetPrompts() {
        while (promptColumn.children.length > 1) {
            promptColumn.removeChild(promptColumn.lastChild);
        }
    }

    // ==================== Cursor Utilities ====================

    /**
     * Get cursor position as character offset in input text
     */
    function getCursorPos() {
        const sel = window.getSelection();
        if (!sel.rangeCount) return 0;

        const range = sel.getRangeAt(0);
        if (!inputArea.contains(range.startContainer)) return -1;

        // Walk text nodes to find position
        let pos = 0;
        const walker = document.createTreeWalker(inputArea, NodeFilter.SHOW_TEXT);
        let node;

        while ((node = walker.nextNode())) {
            if (node === range.startContainer) {
                return pos + range.startOffset;
            }
            pos += node.textContent.length;
        }

        return pos;
    }

    /**
     * Set cursor position by character offset
     */
    function setCursorPos(targetPos) {
        const text = inputArea.textContent || '';
        targetPos = Math.max(0, Math.min(targetPos, text.length));

        const sel = window.getSelection();
        const range = document.createRange();

        // Walk text nodes to find position
        const walker = document.createTreeWalker(inputArea, NodeFilter.SHOW_TEXT);
        let node;
        let pos = 0;

        while ((node = walker.nextNode())) {
            const nodeLen = node.textContent.length;
            if (pos + nodeLen >= targetPos) {
                range.setStart(node, targetPos - pos);
                range.collapse(true);
                sel.removeAllRanges();
                sel.addRange(range);
                return;
            }
            pos += nodeLen;
        }

        // Fallback: end of content
        if (inputArea.lastChild) {
            range.selectNodeContents(inputArea);
            range.collapse(false);
            sel.removeAllRanges();
            sel.addRange(range);
        }
    }

    // ==================== Rainbow Parens ====================

    function escapeHtml(s) {
        return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    }

    /**
     * Render text with rainbow-colored parens
     */
    function renderRainbowParens(text, cursorPos, initialDepth) {
        let html = '';
        let depth = initialDepth;
        let inString = false;
        let inComment = false;

        // Find matching paren if cursor is on one
        let matchPos = -1;
        if (cursorPos >= 0 && cursorPos <= text.length) {
            // Check char before cursor
            if (cursorPos > 0 && '()'.includes(text[cursorPos - 1])) {
                matchPos = findMatchingDelimiter(text, cursorPos - 1);
            } else if (cursorPos < text.length && '()'.includes(text[cursorPos])) {
                matchPos = findMatchingDelimiter(text, cursorPos);
            }
        }

        const matchSet = new Set();
        if (matchPos !== null && matchPos >= 0) {
            matchSet.add(matchPos);
            if (cursorPos > 0 && '()'.includes(text[cursorPos - 1])) {
                matchSet.add(cursorPos - 1);
            } else if (cursorPos >= 0 && cursorPos < text.length && '()'.includes(text[cursorPos])) {
                matchSet.add(cursorPos);
            }
        }

        for (let i = 0; i < text.length; i++) {
            const c = text[i];

            if (c === '\n') {
                inComment = false;
                html += '\n';
                continue;
            }

            if (inComment) {
                html += escapeHtml(c);
                continue;
            }

            if (inString) {
                if (c === '\\' && i + 1 < text.length) {
                    html += escapeHtml(c + text[++i]);
                    continue;
                }
                if (c === '"') inString = false;
                html += escapeHtml(c);
                continue;
            }

            if (c === ';') { inComment = true; html += escapeHtml(c); continue; }
            if (c === '"') { inString = true; html += escapeHtml(c); continue; }

            if (c === '(') {
                const cls = `paren-${depth % PAREN_COLORS}${matchSet.has(i) ? ' paren-match' : ''}`;
                html += `<span class="${cls}">(</span>`;
                depth++;
                continue;
            }

            if (c === ')') {
                depth--;
                if (depth < 0) {
                    html += `<span class="paren-mismatch">)</span>`;
                    depth = 0;
                } else {
                    const cls = `paren-${depth % PAREN_COLORS}${matchSet.has(i) ? ' paren-match' : ''}`;
                    html += `<span class="${cls}">)</span>`;
                }
                continue;
            }

            html += escapeHtml(c);
        }

        return html;
    }

    // ==================== Main Update Function ====================

    /**
     * Update highlight overlay to match input content
     */
    function updateOverlay() {
        const text = inputArea.textContent || '';
        const cursor = getCursorPos();
        highlightLayer.innerHTML = renderRainbowParens(text, cursor, 0);
        updatePrompts();
    }

    // ==================== Evaluation ====================

    /**
     * Evaluate current input
     */
    function evaluate() {
        const code = (inputArea.textContent || '').trim();
        if (!code) return;

        addToHistory(code, 'input');

        try {
            const sexps = parse(code);
            let result;
            for (const sexp of sexps) {
                result = interpreter.run(analyze(sexp), globalEnv);
            }
            addToHistory(prettyPrint(result), 'result');
        } catch (e) {
            console.error('REPL Error:', e);
            addToHistory(`Error: ${e.message}`, 'error');
        }

        // Clear
        inputArea.textContent = '';
        highlightLayer.innerHTML = '';
        resetPrompts();
    }

    // ==================== Event Handlers ====================

    inputArea.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') {
            const text = inputArea.textContent || '';

            if (e.ctrlKey || e.metaKey) {
                // Ctrl/Cmd+Enter: always evaluate
                e.preventDefault();
                evaluate();
            } else if (e.shiftKey) {
                // Shift+Enter: let browser insert newline
            } else {
                // Plain Enter
                if (isCompleteExpression(text) && text.trim()) {
                    e.preventDefault();
                    evaluate();
                } else {
                    // Insert newline with auto-indent
                    e.preventDefault();
                    const cursor = getCursorPos();
                    const before = text.slice(0, cursor);
                    const after = text.slice(cursor);
                    const depth = calculateDepth(before);
                    const indent = '  '.repeat(depth);

                    inputArea.textContent = before + '\n' + indent + after;
                    setCursorPos(before.length + 1 + indent.length);
                    updateOverlay();
                }
            }
        }
    });

    // Update on any input change
    inputArea.addEventListener('input', updateOverlay);
    inputArea.addEventListener('keyup', updateOverlay);
    inputArea.addEventListener('click', updateOverlay);
    inputArea.addEventListener('focus', updateOverlay);

    // Focus input when clicking shell area
    shell.addEventListener('click', (e) => {
        if (e.target === shell || e.target === history || e.target.closest('#repl-current-line')) {
            inputArea.focus();
        }
    });

    // Textarea paste area handler
    replRunBtn.addEventListener('click', () => {
        const code = replInput.value.trim();
        if (!code) return;

        addToHistory(code, 'input');

        try {
            const sexps = parse(code);
            let result;
            for (const sexp of sexps) {
                result = interpreter.run(analyze(sexp), globalEnv);
            }
            addToHistory(prettyPrint(result), 'result');
        } catch (e) {
            console.error('REPL Error:', e);
            addToHistory(`Error: ${e.message}`, 'error');
        }

        replInput.value = '';
    });

    // Welcome
    addToHistory('Welcome to Scheme.js! Type expressions and press Enter.', 'result');
    addToHistory('For incomplete expressions, Enter adds a newline with auto-indent.', 'result');

    inputArea.focus();
}
