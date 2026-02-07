export const replStyles = `
    .repl-shell {
      background-color: #1e1e1e;
      color: #d4d4d4;
      font-family: 'Fira Code', 'Courier New', monospace;
      font-size: 14px;
      line-height: 1.5em;
      padding: 16px;
      border-radius: 8px;
      min-height: 300px;
      max-height: 400px;
      overflow-y: auto;
      display: flex;
      flex-direction: column;
    }

    .repl-line {
      display: flex;
      align-items: flex-start;
      margin-bottom: 4px;
    }

    /* Multiline history entries - stacks lines vertically */
    .repl-history-multiline {
      margin-bottom: 4px;
    }

    .repl-history-line {
      display: flex;
      align-items: flex-start;
      white-space: pre-wrap;
      word-break: break-all;
    }

    /* History prompts: fixed width to match input area */
    .repl-history-line .repl-prompt {
      width: 3ch;
      text-align: right;
      margin-right: 8px;
      flex-shrink: 0;
      user-select: none;
    }

    /* Prompt column: fixed width to prevent shift when adding ... prompts */
    .repl-prompt-column {
      display: flex;
      flex-direction: column;
      flex-shrink: 0;
      width: 3ch;
      /* Width of "..." - prevents width change */
      margin-right: 8px;
      user-select: none;
      pointer-events: none;
    }

    .repl-prompt {
      color: #6a9955;
      font-weight: bold;
      height: 1.5em;
      line-height: 1.5em;
      text-align: right;
      /* Right-align so > and ... align at right edge */
    }

    .repl-prompt.continuation {
      color: #808080;
    }

    /* Grid wrapper for overlay stacking */
    .repl-input-wrapper {
      display: grid;
      flex: 1;
      min-width: 0;
    }

    /* Both layers occupy the same grid cell with identical styling */
    .repl-highlight-layer,
    .repl-input {
      grid-area: 1 / 1 / 2 / 2;
      font-family: inherit;
      font-size: inherit;
      line-height: 1.5em;
      white-space: pre-wrap;
      word-break: break-all;
      margin: 0;
      padding: 0;
      border: 0;
      text-align: left;
    }

    .repl-highlight-layer {
      pointer-events: none;
      user-select: none;
    }

    .repl-input {
      background: transparent;
      color: transparent;
      caret-color: #fff;
      z-index: 1;
      outline: none;
    }

    .repl-input:focus {
      outline: none;
    }

    /* Make selected text visible for copying */
    .repl-input::selection {
      background: #264f78;
      color: #fff;
    }

    /* Rainbow parentheses - 6 levels cycling */
    .paren-0 { color: #ffd700; } /* Gold */
    .paren-1 { color: #da70d6; } /* Orchid */
    .paren-2 { color: #87ceeb; } /* Sky blue */
    .paren-3 { color: #98fb98; } /* Pale green */
    .paren-4 { color: #ffa07a; } /* Light salmon */
    .paren-5 { color: #dda0dd; } /* Plum */

    /* Matching paren highlight */
    .paren-match {
      background-color: rgba(255, 255, 0, 0.4);
      border-radius: 2px;
    }

    /* Mismatched paren */
    .paren-mismatch {
      background-color: rgba(255, 0, 0, 0.4);
      color: #ff6b6b;
      border-radius: 2px;
    }

    .repl-result {
      color: #569cd6;
      margin-bottom: 4px;
      padding-left: 24px;
      white-space: pre-wrap;
    }

    .repl-error {
      color: #f14c4c;
      margin-bottom: 4px;
      padding-left: 24px;
      white-space: pre-wrap;
    }

    .repl-history-input {
      color: #d4d4d4;
    }

    .repl-history-continuation {
      color: #808080;
    }
    
    /* Paste Area styles */
    .repl-paste-area {
        margin-top: 1rem;
        padding-top: 1rem;
        border-top: 1px solid #e5e7eb;
    }
    .repl-paste-label {
        display: block;
        font-size: 0.875rem;
        font-weight: 500;
        color: #4b5563;
        margin-bottom: 0.5rem;
    }
    .repl-textarea {
        width: 100%;
        padding: 0.75rem;
        border: 1px solid #d1d5db;
        border-radius: 0.5rem;
        font-family: monospace;
        font-size: 0.875rem;
    }
    .repl-button {
      margin-top: 0.5rem;
      padding: 0.5rem 1.5rem;
      background-color: #2563eb;
      color: white;
      font-weight: 600;
      border-radius: 0.5rem;
      border: none;
      cursor: pointer;
      transition: background-color 0.15s;
    }
    .repl-button:hover {
      background-color: #1d4ed8;
    }
`;

export const replTemplate = `
      <div id="repl-shell" class="repl-shell">
        <div id="repl-history"></div>
        <div id="repl-current-line" class="repl-line">
          <div id="repl-prompt-column" class="repl-prompt-column">
            <span class="repl-prompt">&gt;</span>
          </div>
          <div class="repl-input-wrapper">
            <div id="repl-highlight-layer" class="repl-highlight-layer" aria-hidden="true"></div>
            <span id="repl-input-area" class="repl-input" contenteditable="true" spellcheck="false"></span>
          </div>
        </div>
      </div>
      
      <!-- Paste Area -->
      <div class="repl-paste-area">
        <label class="repl-paste-label">Paste larger expressions:</label>
        <textarea id="repl-input" class="repl-textarea" rows="4" placeholder="(define (factorial n) ...)"></textarea>
        <button id="repl-run-btn" class="repl-button">Run</button>
      </div>
`;

/**
 * setupRepl
 * @param {Object} interpreter - The Scheme interpreter instance
 * @param {Object} globalEnv - The global environment
 * @param {HTMLElement} [rootElement=document] - Root element to search for REPL nodes (Document or ShadowRoot)
 * @param {Object} [deps] - Dependencies (parse, analyze, prettyPrint, expressionUtils)
 */
export function setupRepl(interpreter, globalEnv, rootElement = document, deps = {}) {
    // Destructure dependencies or throw error if missing
    // We allow defaults for backward compatibility if modules were globally loaded,
    // but better to enforce explicit passing to avoid bundling.
    const {
        parse,
        analyze,
        prettyPrint,
        isCompleteExpression,
        findMatchingDelimiter,
        ReplDebugBackend,
        ReplDebugCommands,
        SchemeDebugRuntime
    } = deps;

    if (!parse || !analyze || !prettyPrint || !isCompleteExpression || !findMatchingDelimiter) {
        console.error("Missing dependencies for REPL setup. Please pass { parse, analyze, prettyPrint, isCompleteExpression, findMatchingDelimiter }.");
    }

    // Debugger state
    let debugBackend = null;
    let debugCommands = null;
    let isEvaluating = false;

    // Initialize debugger if not already present on interpreter
    if (!interpreter.debugRuntime) {
        interpreter.debugRuntime = new SchemeDebugRuntime();
    }

    // Setup backend to output to this REPL
    debugBackend = new ReplDebugBackend((msg) => addToHistory(msg, 'result'));
    interpreter.debugRuntime.setBackend(debugBackend);
    debugCommands = new ReplDebugCommands(interpreter, interpreter.debugRuntime, debugBackend);

    // Watch for pause to change UI state
    debugBackend.setOnPause(() => {
        promptColumn.querySelector('.repl-prompt').textContent = 'debug>';
        shell.classList.add('paused');
        // Scroll to bottom to show pause message
        shell.scrollTop = shell.scrollHeight;
    });

    // Reset on resume
    const originalOnResume = debugBackend.onResume.bind(debugBackend);
    debugBackend.onResume = () => {
        originalOnResume();
        promptColumn.querySelector('.repl-prompt').textContent = '>';
        shell.classList.remove('paused');
    };


    // DOM elements
    // ... rest of function

    const shell = rootElement.querySelector('#repl-shell');
    const history = rootElement.querySelector('#repl-history');
    const inputArea = rootElement.querySelector('#repl-input-area');
    const highlightLayer = rootElement.querySelector('#repl-highlight-layer');
    const promptColumn = rootElement.querySelector('#repl-prompt-column');

    // Optional elements
    const replInput = rootElement.querySelector('#repl-input');
    const replRunBtn = rootElement.querySelector('#repl-run-btn');

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
        const prompts = promptColumn.querySelectorAll('.repl-prompt');
        const currentCount = prompts.length;

        // Add prompts if needed
        while (promptColumn.getElementsByClassName('repl-prompt').length < lineCount) {
            const p = document.createElement('span');
            p.className = 'repl-prompt continuation';
            p.textContent = '...';
            promptColumn.appendChild(p);
        }

        // Remove prompts if needed
        while (promptColumn.getElementsByClassName('repl-prompt').length > lineCount) {
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
        const sel = (rootElement.getSelection) ? rootElement.getSelection() : window.getSelection();
        if (!sel.rangeCount) return 0;
        const range = sel.getRangeAt(0);

        // Check if selection is within (or is) inputArea
        if (!inputArea.contains(range.startContainer) && range.startContainer !== inputArea) return -1;

        // Create a range from the start of the editable area to the caret
        const preCaretRange = range.cloneRange();
        preCaretRange.selectNodeContents(inputArea);
        preCaretRange.setEnd(range.startContainer, range.startOffset);

        return preCaretRange.toString().length;
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
    async function evaluate() {
        if (isEvaluating) return;

        const code = (inputArea.textContent || '').trim();
        if (!code) return;

        // Check for debug commands
        if (code.startsWith(':')) {
            addToHistory(code, 'input');
            const result = debugCommands.execute(code);
            if (result) {
                addToHistory(result, 'result');
            }
            inputArea.textContent = '';
            highlightLayer.innerHTML = '';
            resetPrompts();
            return;
        }

        // If paused, any input is treated as a debug eval or command
        if (debugBackend.isPaused()) {
            addToHistory(code, 'input');
            const result = debugCommands.handleEval(code);
            addToHistory(result, 'result');

            inputArea.textContent = '';
            highlightLayer.innerHTML = '';
            // Don't reset prompts yet if still paused
            updatePrompts();
            return;
        }

        addToHistory(code, 'input');

        // Clear input immediately and hide prompt
        inputArea.textContent = '';
        highlightLayer.innerHTML = '';
        const currentLine = rootElement.querySelector('#repl-current-line');
        if (currentLine) currentLine.style.display = 'none';
        isEvaluating = true;

        try {
            const sexps = parse(code);
            let result;
            for (const sexp of sexps) {
                // Check debug state
                if (debugBackend && interpreter.debugRuntime && !interpreter.debugRuntime.enabled) {
                    // FAST MODE (Sync)
                    // Note: This WILL freeze the browser for long computations
                    result = interpreter.run(analyze(sexp), globalEnv, [], undefined, { jsAutoConvert: 'raw' });
                } else {
                    // DEBUG MODE (Async)
                    result = await interpreter.runAsync(analyze(sexp), globalEnv, { jsAutoConvert: 'raw' });
                }
            }
            // If we hit a breakpoint/pause, the result will be what runAsync returned (or it might still be running)
            if (!debugBackend.isPaused()) {
                addToHistory(prettyPrint(result), 'result');
            }

        } catch (e) {
            console.error('REPL Error:', e);
            addToHistory(`Error: ${e.message}`, 'error');
        } finally {
            isEvaluating = false;
            if (currentLine) currentLine.style.display = '';

            // Post-eval cleanup
            if (!debugBackend.isPaused()) {
                inputArea.textContent = '';
                highlightLayer.innerHTML = '';
                resetPrompts();
            } else {
                inputArea.textContent = '';
                highlightLayer.innerHTML = '';
                updatePrompts();
            }
            inputArea.focus();
        }
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
                if ((text.trim().startsWith(':') || isCompleteExpression(text)) && text.trim()) {
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

    // Bind buttons if they exist
    if (replRunBtn && replInput) {
        replRunBtn.addEventListener('click', async () => {
            if (isEvaluating) return;

            const code = replInput.value.trim();
            if (!code) return;

            addToHistory(code, 'input');

            // UI feedback
            replRunBtn.disabled = true;
            const currentLine = rootElement.querySelector('#repl-current-line');
            if (currentLine) currentLine.style.display = 'none';
            isEvaluating = true;

            try {
                const sexps = parse(code);
                let result;
                for (const sexp of sexps) {
                    result = await interpreter.runAsync(analyze(sexp), globalEnv, { jsAutoConvert: 'raw' });
                }
                if (!debugBackend.isPaused()) {
                    addToHistory(prettyPrint(result), 'result');
                }
            } catch (e) {
                console.error('REPL Error:', e);
                addToHistory(`Error: ${e.message}`, 'error');
            } finally {
                isEvaluating = false;
                replRunBtn.disabled = false;
                if (currentLine) currentLine.style.display = '';
                replInput.value = '';
                inputArea.focus();
            }
        });
    }

    // Welcome
    addToHistory('Welcome to Scheme-JS! Type expressions and press Enter.', 'result');
    addToHistory('For incomplete expressions, Enter adds a newline with auto-indent.', 'result');

    inputArea.focus();
}
