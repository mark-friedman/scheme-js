/**
 * @fileoverview Call Stack component for the Scheme-JS DevTools panel.
 *
 * Renders a list of call stack frames when execution is paused,
 * supporting both Scheme and JavaScript frames with distinct badges.
 * Clicking a frame selects it, triggering a callback so the editor can
 * navigate to the frame's source location and the variables panel can
 * display that frame's locals.
 *
 * Each frame shows:
 *   - Language badge ([SCM] green or [JS] blue)
 *   - Function name (or <anonymous>)
 *   - Source filename:line (if available)
 *   - TCO count badge if > 0 (Scheme frames only)
 */

/**
 * @typedef {Object} StackFrame
 * @property {string} name - Function name
 * @property {{filename: string, line: number, column: number}|null} source
 * @property {number} tcoCount - Number of tail calls collapsed
 * @property {'scheme'|'js'} [language='scheme'] - Frame language type
 */

/**
 * @typedef {Object} CallStackAPI
 * @property {function(StackFrame[]): void} setFrames - Renders a new frame list
 * @property {function(): void} clear - Clears the frame list
 * @property {function(): number} getSelectedIndex - Returns the currently selected index
 */

/**
 * Creates and mounts the call stack panel.
 *
 * @param {HTMLElement} container - The element to render into
 * @param {function(number, StackFrame): void} onSelectFrame - Called when a frame is clicked
 * @returns {CallStackAPI}
 */
export function createCallStack(container, onSelectFrame) {
  let frames = [];
  let selectedIndex = -1;

  container.classList.add('call-stack');

  /**
   * Renders the current frames array into the container.
   * @private
   */
  function render() {
    container.innerHTML = '';

    if (frames.length === 0) {
      const empty = document.createElement('div');
      empty.className = 'call-stack-empty';
      empty.textContent = 'No stack frames';
      container.appendChild(empty);
      return;
    }

    // Render in reverse (top of stack first = most recent call at top)
    for (let i = frames.length - 1; i >= 0; i--) {
      const frame = frames[i];
      const item = document.createElement('div');
      item.className = 'call-stack-frame';
      item.dataset.testid = 'call-stack-frame';
      item.dataset.frameIndex = String(i);
      item.dataset.frameName = frame.name || '<anonymous>';
      if (i === selectedIndex) {
        item.classList.add('selected');
      }

      // Language badge — [SCM] (green) or [JS] (blue)
      const language = frame.language || 'scheme';
      const badge = document.createElement('span');
      if (language === 'js') {
        badge.className = 'frame-badge frame-badge-js';
        badge.textContent = 'JS';
      } else {
        badge.className = 'frame-badge frame-badge-scheme';
        badge.textContent = 'SCM';
      }
      item.appendChild(badge);

      // Function name
      const nameEl = document.createElement('span');
      nameEl.className = 'frame-name';
      nameEl.textContent = frame.name || '<anonymous>';
      item.appendChild(nameEl);

      // Source location
      if (frame.source) {
        const srcEl = document.createElement('span');
        srcEl.className = 'frame-source';
        const filename = frame.source.filename.split('/').pop();
        srcEl.textContent = ` ${filename}:${frame.source.line}`;
        item.appendChild(srcEl);
      }

      // TCO badge (Scheme frames only)
      if (frame.tcoCount > 0) {
        const tcoEl = document.createElement('span');
        tcoEl.className = 'frame-tco';
        tcoEl.textContent = `×${frame.tcoCount + 1}`;
        tcoEl.title = `${frame.tcoCount + 1} tail calls collapsed`;
        item.appendChild(tcoEl);
      }

      // Click handler — use closure over loop variable i
      const frameIndex = i;
      item.addEventListener('click', () => {
        selectedIndex = frameIndex;
        render();
        onSelectFrame(frameIndex, frame);
      });

      container.appendChild(item);
    }
  }

  /**
   * Renders a new list of stack frames.
   * Selects the top frame (most recent) by default.
   *
   * @param {StackFrame[]} newFrames
   * @param {boolean} [suppressAutoSelect=false] - If true, renders but does not
   *   auto-select the top frame or fire the onSelectFrame callback. Use this when
   *   the caller will handle highlighting separately (e.g., using pause source info).
   */
  function setFrames(newFrames, suppressAutoSelect = false) {
    frames = newFrames || [];
    // Default to top frame (last in array = most recent)
    selectedIndex = frames.length > 0 ? frames.length - 1 : -1;
    render();
    // Trigger selection callback for the default frame
    if (!suppressAutoSelect && selectedIndex >= 0 && onSelectFrame) {
      onSelectFrame(selectedIndex, frames[selectedIndex]);
    }
  }

  /**
   * Clears all frames from the display.
   */
  function clear() {
    frames = [];
    selectedIndex = -1;
    render();
  }

  /**
   * Returns the currently selected frame index.
   * @returns {number}
   */
  function getSelectedIndex() {
    return selectedIndex;
  }

  render();

  return { setFrames, clear, getSelectedIndex };
}
