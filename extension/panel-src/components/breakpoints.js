/**
 * @fileoverview Breakpoint List component for the Scheme-JS DevTools panel.
 *
 * Renders a list of active breakpoints across all files.
 * Clicking a breakpoint navigates the editor to its source location.
 * Each breakpoint also features a remove button to delete it.
 */

/**
 * Creates and mounts the breakpoint list panel.
 *
 * @param {HTMLElement} container - The element to render into
 * @param {Object} callbacks
 * @param {function(string, number, number|null): void} callbacks.onClickBreakpoint - Called when a breakpoint is clicked
 * @param {function(string, string, number, number|null): void} callbacks.onRemoveBreakpoint - Called when removal is requested
 * @returns {{ setBreakpoints: function(Array<{id: string, filename: string, line: number, column: number|null}>): void, clear: function(): void }}
 */
export function createBreakpointsList(container, { onClickBreakpoint, onRemoveBreakpoint }) {
  let breakpoints = [];

  container.classList.add('breakpoints-list');

  /**
   * Renders the current breakpoints array into the container.
   * @private
   */
  function render() {
    container.innerHTML = '';

    if (breakpoints.length === 0) {
      const empty = document.createElement('div');
      empty.className = 'breakpoints-empty';
      empty.textContent = 'No breakpoints';
      container.appendChild(empty);
      return;
    }

    // Sort breakpoints: filename -> line -> col
    const sorted = [...breakpoints].sort((a, b) => {
      if (a.filename !== b.filename) return a.filename.localeCompare(b.filename);
      if (a.line !== b.line) return a.line - b.line;
      return (a.column || 0) - (b.column || 0);
    });

    for (const bp of sorted) {
      const item = document.createElement('div');
      item.className = 'breakpoint-item';
      item.dataset.testid = 'breakpoint-item';
      item.dataset.bpId = bp.id;

      // Checkbox / Icon to show it's a breakpoint
      const icon = document.createElement('span');
      icon.className = 'breakpoint-icon';
      icon.textContent = '⚫'; // basic dot icon
      item.appendChild(icon);
      
      const fileLabel = document.createElement('span');
      fileLabel.className = 'breakpoint-file';
      const shortName = bp.filename.split('/').pop();
      fileLabel.textContent = shortName;
      item.appendChild(fileLabel);

      const locLabel = document.createElement('span');
      locLabel.className = 'breakpoint-location';
      if (bp.column) {
         locLabel.textContent = `:${bp.line}:${bp.column}`;
      } else {
         locLabel.textContent = `:${bp.line}`;
      }
      item.appendChild(locLabel);

      const removeBtn = document.createElement('span');
      removeBtn.className = 'breakpoint-remove';
      removeBtn.textContent = '❌';
      removeBtn.title = 'Remove breakpoint';
      
      // Stop event propagation so clicking remove doesn't trigger navigation
      removeBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        onRemoveBreakpoint(bp.id, bp.filename, bp.line, bp.column);
      });
      item.appendChild(removeBtn);

      item.addEventListener('click', () => {
        onClickBreakpoint(bp.filename, bp.line, bp.column);
      });

      container.appendChild(item);
    }
  }

  /**
   * Renders a new list of breakpoints.
   *
   * @param {Array<{id: string, filename: string, line: number, column: number|null}>} newBreakpoints
   */
  function setBreakpoints(newBreakpoints) {
    breakpoints = newBreakpoints || [];
    render();
  }

  /**
   * Clears all breakpoints from the display.
   */
  function clear() {
    breakpoints = [];
    render();
  }

  render();

  return { setBreakpoints, clear };
}
