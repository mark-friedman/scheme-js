/**
 * @fileoverview Eval Console component for the Scheme-JS DevTools panel.
 *
 * Provides a REPL-like interface for evaluating expressions within the
 * current execution context (Scheme or JS frame).
 * Supports history navigation (Up/Down arrows) and renders results
 * including basic syntax highlighting for different value types.
 */

/**
 * Creates and mounts the eval console panel.
 *
 * @param {HTMLElement} container - The element to render into
 * @param {Object} options
 * @param {function(string): Promise<{success: boolean, result: any, error: string|null}>} options.onEvaluate - Called to evaluate input
 * @returns {{ clear: function(): void, focus: function(): void }}
 */
export function createConsole(container, { onEvaluate }) {
  let history = [];
  let historyIndex = -1;
  let currentInput = '';

  container.className = 'eval-console';

  // Create layout
  const outputContainer = document.createElement('div');
  outputContainer.id = 'console-output';
  outputContainer.className = 'console-output';
  container.appendChild(outputContainer);

  const inputContainer = document.createElement('div');
  inputContainer.className = 'console-input-container';

  const prompt = document.createElement('span');
  prompt.className = 'console-prompt';
  prompt.textContent = '>';
  inputContainer.appendChild(prompt);

  const inputField = document.createElement('input');
  inputField.id = 'console-input';
  inputField.className = 'console-input';
  inputField.type = 'text';
  inputField.placeholder = 'Evaluate expression...';
  inputContainer.appendChild(inputField);

  const clearBtn = document.createElement('button');
  clearBtn.id = 'console-clear';
  clearBtn.className = 'console-clear-btn';
  clearBtn.textContent = '\u2298';
  clearBtn.title = 'Clear console';
  clearBtn.addEventListener('click', () => {
    outputContainer.innerHTML = '';
  });
  inputContainer.appendChild(clearBtn);

  container.appendChild(inputContainer);

  // Helper to append a line to the output
  function appendOutput(content, isError = false, isCommand = false) {
    const entry = document.createElement('div');
    entry.className = 'console-message';
    entry.dataset.testid = 'console-message';
    entry.dataset.messageType = isCommand ? 'command' : (isError ? 'error' : 'result');
    if (isError) entry.classList.add('error');
    if (isCommand) entry.classList.add('console-command');

    const icon = document.createElement('span');
    icon.className = 'console-icon';
    if (isCommand) {
      icon.textContent = '>';
    } else if (isError) {
      icon.textContent = '✖';
    } else {
      icon.textContent = '←';
    }
    entry.appendChild(icon);

    const text = document.createElement('span');
    text.className = 'console-text';
    
    // Simple formatting for results
    if (isCommand || isError) {
      text.textContent = content;
    } else {
      formatResult(content, text);
    }
    entry.appendChild(text);

    outputContainer.appendChild(entry);
    outputContainer.scrollTop = outputContainer.scrollHeight;
  }

  // Simple formatter for REPL results
  function formatResult(value, container) {
    if (value === null) {
      container.textContent = 'null';
      container.classList.add('var-type-null');
    } else if (typeof value === 'undefined') {
      container.textContent = 'undefined';
      container.classList.add('var-type-null');
    } else if (typeof value === 'boolean') {
      container.textContent = value.toString();
      container.classList.add('var-type-boolean');
    } else if (typeof value === 'number') {
      container.textContent = value.toString();
      container.classList.add('var-type-number');
    } else if (typeof value === 'string') {
      container.textContent = `"${value}"`;
      container.classList.add('var-type-string');
    } else if (typeof value === 'object') {
      // Basic object rendering
      if (Array.isArray(value)) {
        container.textContent = `Array(${value.length}) [${value.map(v => typeof v === 'string' ? `"${v}"` : v).join(', ')}]`;
      } else {
        try {
          container.textContent = JSON.stringify(value);
        } catch {
          container.textContent = Object.prototype.toString.call(value);
        }
      }
      container.classList.add('var-type-other');
    } else {
      container.textContent = String(value);
      container.classList.add('var-type-other');
    }
  }

  // Handle Input
  inputField.addEventListener('keydown', async (e) => {
    if (e.key === 'Enter') {
      const code = inputField.value.trim();
      if (!code) return;

      appendOutput(code, false, true);
      
      // Save history (cap at 100 entries to prevent unbounded growth)
      history.push(code);
      if (history.length > 100) {
        history.splice(0, history.length - 100);
      }
      historyIndex = history.length;
      inputField.value = '';
      currentInput = '';

      inputField.disabled = true;

      try {
        const result = await onEvaluate(code);
        if (result.success) {
          appendOutput(result.result);
        } else {
          appendOutput(result.error || 'Evaluation failed', true);
        }
      } catch (err) {
        appendOutput(err.message, true);
      } finally {
        inputField.disabled = false;
        inputField.focus();
      }
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (historyIndex > 0) {
        if (historyIndex === history.length) {
          currentInput = inputField.value;
        }
        historyIndex--;
        inputField.value = history[historyIndex];
      }
    } else if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (historyIndex < history.length - 1) {
        historyIndex++;
        inputField.value = history[historyIndex];
      } else if (historyIndex === history.length - 1) {
        historyIndex++;
        inputField.value = currentInput;
      }
    }
  });

  return {
    clear: () => {
      outputContainer.innerHTML = '';
      history = [];
      historyIndex = -1;
      currentInput = '';
    },
    focus: () => {
      inputField.focus();
    }
  };
}
