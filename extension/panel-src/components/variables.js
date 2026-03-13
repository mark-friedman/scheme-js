/**
 * @fileoverview Variables panel component for the Scheme-JS DevTools panel.
 *
 * Renders the local bindings for the selected call stack frame.
 * Each variable shows its name, value string, and type.
 */

/**
 * @typedef {Object} LocalVar
 * @property {string} name - Variable name
 * @property {string} value - String representation of the value
 * @property {string} type - Type name (number, boolean, string, pair, etc.)
 * @property {string|null} subtype
 */

/**
 * @typedef {Object} VariablesAPI
 * @property {function(LocalVar[]): void} setLocals - Renders a list of local variables
 * @property {function(): void} clear - Clears the variable list
 */

/**
 * Creates and mounts the variables panel.
 *
 * @param {HTMLElement} container - The element to render into
 * @returns {VariablesAPI}
 */
export function createVariables(container) {
  container.className = 'variables-panel';

  /**
   * Maps a type name to a CSS class for coloring.
   *
   * @param {string} type
   * @returns {string}
   */
  function typeClass(type) {
    switch (type) {
      case 'number':  return 'var-type-number';
      case 'boolean': return 'var-type-boolean';
      case 'string':  return 'var-type-string';
      case 'null':    return 'var-type-null';
      default:        return 'var-type-other';
    }
  }

  /**
   * Renders a list of local variable bindings.
   *
   * @param {LocalVar[]} locals
   */
  function setLocals(locals) {
    container.innerHTML = '';

    if (!locals || locals.length === 0) {
      const empty = document.createElement('div');
      empty.className = 'variables-empty';
      empty.textContent = 'No local bindings';
      container.appendChild(empty);
      return;
    }

    for (const { name, value, type } of locals) {
      const row = document.createElement('div');
      row.className = 'variable-row';
      row.dataset.testid = 'variable-row';
      row.dataset.varName = name;

      const nameEl = document.createElement('span');
      nameEl.className = 'var-name';
      nameEl.textContent = name;
      row.appendChild(nameEl);

      const sep = document.createElement('span');
      sep.className = 'var-sep';
      sep.textContent = ': ';
      row.appendChild(sep);

      const valueEl = document.createElement('span');
      valueEl.className = `var-value ${typeClass(type)}`;
      valueEl.textContent = value;
      valueEl.title = type;
      row.appendChild(valueEl);

      container.appendChild(row);
    }
  }

  /**
   * Clears all variable rows.
   */
  function clear() {
    container.innerHTML = '';
  }

  return { setLocals, clear };
}
