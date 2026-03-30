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
 * @property {string} [scope] - Scope level: 'local', 'closure', or 'global'
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
  container.classList.add('variables-panel');

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
   * Renders a single variable row.
   *
   * @param {LocalVar} variable
   * @returns {HTMLElement}
   */
  function createVarRow({ name, value, type }) {
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

    return row;
  }

  /**
   * Renders a list of local variable bindings, grouped by scope.
   * Variables with a `scope` field are grouped under section headers.
   * Variables without a `scope` field are treated as 'local'.
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

    // Check if any variable has scope information
    const hasScopes = locals.some(v => v.scope);

    if (!hasScopes) {
      // No scope info — flat list (backwards compat)
      for (const v of locals) {
        container.appendChild(createVarRow(v));
      }
      return;
    }

    // Group by scope, preserving order within each group
    const groups = { local: [], closure: [], global: [] };
    for (const v of locals) {
      const scope = v.scope || 'local';
      if (groups[scope]) {
        groups[scope].push(v);
      } else {
        groups.local.push(v);
      }
    }

    const scopeLabels = { local: 'Local', closure: 'Closure', global: 'Global' };

    for (const [scope, vars] of Object.entries(groups)) {
      if (vars.length === 0) continue;

      const header = document.createElement('div');
      header.className = 'variables-scope-header';
      header.dataset.testid = 'scope-header';
      header.textContent = scopeLabels[scope] || scope;
      container.appendChild(header);

      for (const v of vars) {
        container.appendChild(createVarRow(v));
      }
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
