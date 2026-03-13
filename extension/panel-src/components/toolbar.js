/**
 * @fileoverview Toolbar component for the Scheme-JS DevTools panel.
 *
 * Renders the debug control buttons (Resume, Step Into, Step Over, Step Out)
 * and a status label. Buttons are disabled when execution is running and
 * enabled when paused.
 */

import { evalInPage } from '../protocol/scheme-bridge.js';

/**
 * @typedef {Object} ToolbarAPI
 * @property {function(string): void} setStatus - Updates the status text
 * @property {function(): void} setPaused - Enables step/resume buttons
 * @property {function(): void} setRunning - Disables step/resume buttons
 */

/**
 * Creates and mounts the debug toolbar into the given container.
 *
 * @param {HTMLElement} container - The element to mount the toolbar into
 * @param {Object} callbacks - Action callbacks
 * @param {function(): void} callbacks.onResume
 * @param {function(): void} callbacks.onStepInto
 * @param {function(): void} callbacks.onStepOver
 * @param {function(): void} callbacks.onStepOut
 * @returns {ToolbarAPI}
 */
export function createToolbar(container, callbacks) {
  // ---- Build DOM ----

  const toolbar = document.createElement('div');
  toolbar.className = 'toolbar-controls';

  /**
   * Creates a toolbar button with a unicode icon and tooltip.
   *
   * @param {string} icon - Unicode icon character
   * @param {string} title - Tooltip text
   * @param {function(): void} onClick
   * @returns {HTMLButtonElement}
   */
  function makeButton(icon, title, testId, onClick) {
    const btn = document.createElement('button');
    btn.className = 'toolbar-btn';
    btn.title = title;
    btn.dataset.testid = testId;
    btn.textContent = icon;
    btn.disabled = true; // Disabled until paused
    btn.addEventListener('click', onClick);
    return btn;
  }

  const resumeBtn   = makeButton('▶', 'Resume (F8)',           'btn-resume',    () => callbacks.onResume());
  const stepIntoBtn = makeButton('⬇', 'Step Into (F11)',       'btn-step-into', () => callbacks.onStepInto());
  const stepOverBtn = makeButton('↷', 'Step Over (F10)',       'btn-step-over', () => callbacks.onStepOver());
  const stepOutBtn  = makeButton('⬆', 'Step Out (Shift+F11)', 'btn-step-out',  () => callbacks.onStepOut());

  const statusEl = document.createElement('span');
  statusEl.className = 'toolbar-status';
  statusEl.textContent = 'Running';

  toolbar.appendChild(resumeBtn);
  toolbar.appendChild(stepIntoBtn);
  toolbar.appendChild(stepOverBtn);
  toolbar.appendChild(stepOutBtn);
  toolbar.appendChild(statusEl);

  container.appendChild(toolbar);

  // ---- Keyboard shortcuts ----

  document.addEventListener('keydown', (e) => {
    if (e.key === 'F8') { e.preventDefault(); callbacks.onResume(); }
    if (e.key === 'F11' && !e.shiftKey) { e.preventDefault(); callbacks.onStepInto(); }
    if (e.key === 'F10') { e.preventDefault(); callbacks.onStepOver(); }
    if (e.key === 'F11' && e.shiftKey) { e.preventDefault(); callbacks.onStepOut(); }
  });

  // ---- API ----

  /**
   * Updates the status text in the toolbar.
   * @param {string} text
   */
  function setStatus(text) {
    statusEl.textContent = text;
  }

  /**
   * Enables the step/resume buttons (call when execution is paused).
   */
  function setPaused() {
    resumeBtn.disabled   = false;
    stepIntoBtn.disabled = false;
    stepOverBtn.disabled = false;
    stepOutBtn.disabled  = false;
  }

  /**
   * Disables the step/resume buttons (call when execution is running).
   */
  function setRunning() {
    resumeBtn.disabled   = true;
    stepIntoBtn.disabled = true;
    stepOverBtn.disabled = true;
    stepOutBtn.disabled  = true;
  }

  return { setStatus, setPaused, setRunning };
}
