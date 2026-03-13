/**
 * @fileoverview Generic splitter drag-to-resize behavior.
 *
 * Provides a reusable helper for creating draggable splitter handles
 * that resize adjacent panels. Supports both horizontal (col-resize)
 * and vertical (row-resize) orientations.
 */

/**
 * Initializes a drag-to-resize splitter.
 *
 * @param {HTMLElement} splitterEl - The splitter handle element
 * @param {Object} options
 * @param {'horizontal'|'vertical'} options.direction - Resize direction
 * @param {HTMLElement} options.target - The element to resize
 * @param {number} [options.min=50] - Minimum size in pixels
 * @param {number} [options.max=600] - Maximum size in pixels
 * @param {boolean} [options.invert=false] - If true, dragging up/left increases size
 */
export function initSplitter(splitterEl, { direction, target, min = 50, max = 600, invert = false }) {
  let dragging = false;
  let dragStart = 0;
  let dragStartSize = 0;

  const isHorizontal = direction === 'horizontal';
  const cursor = isHorizontal ? 'col-resize' : 'row-resize';

  splitterEl.addEventListener('mousedown', (e) => {
    dragging = true;
    dragStart = isHorizontal ? e.clientX : e.clientY;
    dragStartSize = isHorizontal ? target.offsetWidth : target.offsetHeight;
    splitterEl.classList.add('dragging');
    document.body.style.cursor = cursor;
    document.body.style.userSelect = 'none';
  });

  document.addEventListener('mousemove', (e) => {
    if (!dragging) return;
    const current = isHorizontal ? e.clientX : e.clientY;
    const rawDelta = current - dragStart;
    const delta = invert ? -rawDelta : rawDelta;
    const newSize = Math.max(min, Math.min(max, dragStartSize + delta));
    if (isHorizontal) {
      target.style.width = `${newSize}px`;
    } else {
      target.style.height = `${newSize}px`;
    }
  });

  document.addEventListener('mouseup', () => {
    if (!dragging) return;
    dragging = false;
    splitterEl.classList.remove('dragging');
    document.body.style.cursor = '';
    document.body.style.userSelect = '';
  });
}
