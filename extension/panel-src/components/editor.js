/**
 * @fileoverview CodeMirror 6 editor wrapper for the Scheme-JS panel.
 *
 * Provides a read-only source viewer with Scheme syntax highlighting,
 * line numbers, a breakpoint gutter (click to toggle), and a current-line
 * highlight for showing where execution is paused.
 *
 * Theme tracks the Chrome DevTools theme (dark or light) via the
 * `prefers-color-scheme` media query. A `Compartment` holds the active
 * theme + highlight style for hot-swapping.
 *
 * Syntax highlighting uses `HighlightStyle.define()` with lezer `tags` —
 * the correct CodeMirror 6 API (not `.tok-*` CSS overrides).
 *
 * Dark theme:  Dracula-inspired — vibrant, high-contrast on dark backgrounds.
 * Light theme: GitHub-light-inspired — semantic colors on white.
 */

import {
  EditorView, lineNumbers, highlightActiveLine, gutter, GutterMarker,
  Decoration, WidgetType,
} from '@codemirror/view';
import {
  EditorState, Compartment, StateField, StateEffect, RangeSetBuilder,
} from '@codemirror/state';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { search, searchKeymap } from '@codemirror/search';
import { keymap } from '@codemirror/view';
import { tags } from '@lezer/highlight';
import { schemeLanguage } from '../language/scheme-mode.js';
import { javascript } from '@codemirror/lang-javascript';
import { html } from '@codemirror/lang-html';

// =========================================================================
// Shared structural styles (layout only — no colors)
// =========================================================================

/** Layout styles applied regardless of theme. */
const layoutTheme = EditorView.theme({
  '&': {
    height: '100%',
    fontSize: '12px',
    fontFamily: "'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', monospace",
  },
  '.cm-scroller': {
    overflow: 'auto',
    height: '100%',
  },
  '.cm-content': {
    padding: '4px 0',
  },
  '.cm-line': {
    padding: '0 8px 0 4px',
  },
  '.cm-lineNumbers .cm-gutterElement': {
    padding: '0 8px 0 4px',
    minWidth: '36px',
    textAlign: 'right',
  },
  // Breakpoint gutter: narrow column on the left
  '.cm-breakpoint-gutter': {
    width: '16px',
    cursor: 'pointer',
    padding: '0',
  },
  '.cm-breakpoint-gutter .cm-gutterElement': {
    width: '16px',
    padding: '0',
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
  },
  // Current-line highlight for the debug arrow
  '.cm-debug-current-line': {
    backgroundColor: 'rgba(255, 193, 7, 0.15) !important',
    borderLeft: '3px solid #ffc107',
  },
  // Current expression highlight (exact sub-expression being executed)
  '.cm-debug-current-expr': {
    backgroundColor: 'rgba(255, 193, 7, 0.35) !important',
    borderBottom: '2px solid #ffc107',
    borderRadius: '2px',
  },
  // Inline expression diamond markers (Chrome DevTools-style).
  // Structural styles only — colors are set per-theme (dark/light).
  '.cm-expr-diamond': {
    cursor: 'pointer',
    fontSize: '8px',
    lineHeight: '1',
    verticalAlign: 'middle',
    padding: '0 1px',
    userSelect: 'none',
    transition: 'opacity 0.15s, color 0.15s',
  },
  // Breakpoint dot in the gutter
  '.cm-breakpoint-dot': {
    width: '8px',
    height: '8px',
    borderRadius: '50%',
    backgroundColor: '#ff5555',
    display: 'inline-block',
  },
});

// =========================================================================
// Dark theme — Dracula palette
// =========================================================================

/** CodeMirror `EditorView` theme for dark mode (colours + gutter). */
const darkEditorTheme = EditorView.theme(
  {
    '&': { backgroundColor: '#282a36', color: '#f8f8f2' },
    '.cm-gutters': {
      backgroundColor: '#21222c',
      borderRight: '1px solid #44475a',
      color: '#6272a4',
      minWidth: '40px',
    },
    '.cm-activeLineGutter': { backgroundColor: '#2d2f3f' },
    '.cm-activeLine':       { backgroundColor: '#2d2f3f' },
    '.cm-selectionBackground': { backgroundColor: '#44475a !important' },
    '&.cm-focused .cm-selectionBackground': { backgroundColor: '#44475a !important' },
    '.cm-cursor': { borderLeftColor: '#f8f8f2' },
    // Diamond colors for dark mode — use a clearly visible color at decent opacity
    '.cm-expr-diamond':        { color: '#8be9fd', opacity: '0.75' },
    '.cm-expr-diamond:hover':  { color: '#8be9fd', opacity: '1' },
    '.cm-expr-diamond-active': { color: '#ff79c6', opacity: '1' },
  },
  { dark: true }
);

/** `HighlightStyle` for dark mode token colours. */
const darkHighlight = HighlightStyle.define([
  { tag: tags.comment,                          color: '#8d93ac', fontStyle: 'italic' },
  { tag: tags.string,                           color: '#f1fa8c' },
  { tag: tags.number,                           color: '#bd93f9' },
  { tag: tags.keyword,                          color: '#ff79c6', fontWeight: 'bold' },
  { tag: [tags.atom, tags.bool],                color: '#8be9fd' },
  { tag: tags.meta,                             color: '#ff79c6' },
  { tag: tags.definition(tags.variableName),    color: '#50fa7b' },
  { tag: tags.variableName,                     color: '#f8f8f2' },
  { tag: tags.bracket,                          color: '#f8f8f2' },
  { tag: tags.invalid,                          color: '#ff5555', textDecoration: 'underline' },
]);

// =========================================================================
// Light theme — GitHub-light palette
// =========================================================================

/** CodeMirror `EditorView` theme for light mode (colours + gutter). */
const lightEditorTheme = EditorView.theme(
  {
    '&': { backgroundColor: '#ffffff', color: '#24292f' },
    '.cm-gutters': {
      backgroundColor: '#f6f8fa',
      borderRight: '1px solid #d0d7de',
      color: '#6e7781',
      minWidth: '40px',
    },
    '.cm-activeLineGutter': { backgroundColor: '#eaf5fb' },
    '.cm-activeLine':       { backgroundColor: '#f0f7fb' },
    '.cm-selectionBackground': { backgroundColor: '#b3d7ff !important' },
    '&.cm-focused .cm-selectionBackground': { backgroundColor: '#b3d7ff !important' },
    '.cm-cursor': { borderLeftColor: '#24292f' },
    // Diamond colors for light mode — subdued but visible on white
    '.cm-expr-diamond':        { color: '#6272a4', opacity: '0.65' },
    '.cm-expr-diamond:hover':  { color: '#6272a4', opacity: '1' },
    '.cm-expr-diamond-active': { color: '#dc3545', opacity: '1' },
  },
  { dark: false }
);

/** `HighlightStyle` for light mode token colours. */
const lightHighlight = HighlightStyle.define([
  { tag: tags.comment,                          color: '#6e7781', fontStyle: 'italic' },
  { tag: tags.string,                           color: '#0a3069' },
  { tag: tags.number,                           color: '#0550ae' },
  { tag: tags.keyword,                          color: '#cf222e', fontWeight: 'bold' },
  { tag: [tags.atom, tags.bool],                color: '#0550ae' },
  { tag: tags.meta,                             color: '#8250df' },
  { tag: tags.definition(tags.variableName),    color: '#116329' },
  { tag: tags.variableName,                     color: '#24292f' },
  { tag: tags.bracket,                          color: '#24292f' },
  { tag: tags.invalid,                          color: '#cf222e', textDecoration: 'underline' },
]);

// =========================================================================
// Theme compartment — hot-swap without rebuilding editor state
// =========================================================================

const themeCompartment = new Compartment();

/**
 * Compartment for hot-swapping entre Scheme and JavaScript language modes.
 * @type {import('@codemirror/state').Compartment}
 */
const languageCompartment = new Compartment();

/**
 * Bundles an editor theme + highlight style into a single extension value.
 *
 * @param {boolean} dark
 * @returns {import('@codemirror/state').Extension}
 */
function makeThemeExtension(dark) {
  return dark
    ? [darkEditorTheme,  syntaxHighlighting(darkHighlight)]
    : [lightEditorTheme, syntaxHighlighting(lightHighlight)];
}

// =========================================================================
// Breakpoint state management
// =========================================================================

/**
 * StateEffect for toggling a breakpoint at a given 1-indexed line.
 * @type {import('@codemirror/state').StateEffectType<number>}
 */
const toggleBreakpointEffect = StateEffect.define();

/**
 * StateEffect for bulk-setting the full breakpoint set.
 * Accepts a Set<number> of 1-indexed line numbers.
 * @type {import('@codemirror/state').StateEffectType<Set<number>>}
 */
const setBreakpointsEffect = StateEffect.define();

/**
 * StateField that stores the set of 1-indexed line numbers with breakpoints.
 */
const breakpointField = StateField.define({
  create: () => new Set(),
  update(breakpoints, tr) {
    let updated = breakpoints;
    for (const effect of tr.effects) {
      if (effect.is(toggleBreakpointEffect)) {
        const line = effect.value;
        // Clone set so CodeMirror detects the change
        updated = new Set(updated);
        if (updated.has(line)) {
          updated.delete(line);
        } else {
          updated.add(line);
        }
      } else if (effect.is(setBreakpointsEffect)) {
        updated = effect.value;
      }
    }
    return updated;
  },
});

/** GutterMarker that renders a red dot for a line with a breakpoint. */
class BreakpointMarker extends GutterMarker {
  /** @returns {Node} */
  toDOM() {
    const dot = document.createElement('div');
    dot.className = 'cm-breakpoint-dot';
    return dot;
  }
}

const breakpointMarker = new BreakpointMarker();

// =========================================================================
// Current-line decoration (shows execution position when paused)
// =========================================================================

/**
 * StateEffect for setting the current execution line (1-indexed) or null.
 * @type {import('@codemirror/state').StateEffectType<number|null>}
 */
const setCurrentLineEffect = StateEffect.define();

/**
 * StateField that stores the current 1-indexed execution line, or null.
 */
const currentLineField = StateField.define({
  create: () => null,
  update(line, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setCurrentLineEffect)) {
        return effect.value;
      }
    }
    return line;
  },
  provide: (field) => EditorView.decorations.from(field, (currentLine) => {
    if (currentLine === null) return Decoration.none;
    const builder = new RangeSetBuilder();
    // decorations.from is called with the state — build decoration lazily
    // (will be recalculated by CodeMirror each time the field changes)
    return builder.finish(); // Populated in a separate ViewPlugin
  }),
});

/**
 * ViewPlugin that applies a `.cm-debug-current-line` line decoration
 * when `currentLineField` is non-null.
 *
 * Using a ViewPlugin here (rather than pure StateField.provide) is simpler
 * because we need to look up the actual document position from the line number.
 */
const currentLinePlugin = EditorView.decorations.compute(
  [currentLineField],
  (state) => {
    const lineNo = state.field(currentLineField);
    if (lineNo === null) return Decoration.none;

    try {
      const lineInfo = state.doc.line(lineNo);
      return Decoration.set([
        Decoration.line({ class: 'cm-debug-current-line' }).range(lineInfo.from),
      ]);
    } catch {
      return Decoration.none;
    }
  }
);

// =========================================================================
// Current expression decoration (exact sub-expression when paused)
// =========================================================================

/**
 * StateEffect for setting the current expression range or null.
 * Accepts { from: number, to: number } (doc offsets) or null to clear.
 * @type {import('@codemirror/state').StateEffectType<{from: number, to: number}|null>}
 */
const setCurrentExpressionEffect = StateEffect.define();

/**
 * StateField that stores the current expression range (doc offsets) or null.
 */
const currentExpressionField = StateField.define({
  create: () => null,
  update(range, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setCurrentExpressionEffect)) {
        return effect.value;
      }
    }
    return range;
  },
});

/**
 * Decoration that applies `.cm-debug-current-expr` to the current expression range.
 */
const currentExpressionPlugin = EditorView.decorations.compute(
  [currentExpressionField],
  (state) => {
    const range = state.field(currentExpressionField);
    if (!range) return Decoration.none;

    try {
      return Decoration.set([
        Decoration.mark({ class: 'cm-debug-current-expr' }).range(range.from, range.to),
      ]);
    } catch {
      return Decoration.none;
    }
  }
);

// =========================================================================
// Inline expression diamond markers (Chrome DevTools-style)
// =========================================================================

/**
 * StateEffect for setting inline expression diamond markers.
 * Each marker represents a breakable sub-expression on a line.
 * Accepts an array of { pos: number, active: boolean, line: number, column: number }.
 * - pos: document offset where the diamond appears
 * - active: true if an expression breakpoint is set here (filled diamond)
 * - line/column: 1-indexed source location (passed back on click)
 * @type {import('@codemirror/state').StateEffectType<Array>}
 */
const setDiamondMarkersEffect = StateEffect.define();

/**
 * StateField that stores the current diamond marker descriptors.
 */
const diamondMarkersField = StateField.define({
  create: () => [],
  update(markers, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setDiamondMarkersEffect)) {
        return effect.value;
      }
    }
    return markers;
  },
});

/**
 * Callback holder for diamond click events.
 * Set by createEditor; called with (line, column) when a diamond is clicked.
 * @type {{fn: Function|null}}
 */
const diamondClickCallback = { fn: null };

/**
 * Widget that renders a small diamond (◆ or ◇) at an expression boundary.
 * Filled = active breakpoint, hollow = available but not set.
 */
class DiamondWidget extends WidgetType {
  /**
   * @param {boolean} active - Whether this expression has an active breakpoint
   * @param {number} line - 1-indexed line number
   * @param {number} column - 1-indexed column number
   */
  constructor(active, line, column) {
    super();
    this.active = active;
    this.line = line;
    this.column = column;
  }

  /** @returns {HTMLElement} */
  toDOM() {
    const span = document.createElement('span');
    span.className = this.active ? 'cm-expr-diamond cm-expr-diamond-active' : 'cm-expr-diamond';
    span.textContent = this.active ? '◆' : '◇';
    span.title = this.active
      ? `Expression breakpoint (line ${this.line}, col ${this.column}) — click to remove`
      : `Click to set expression breakpoint (line ${this.line}, col ${this.column})`;
    span.dataset.line = String(this.line);
    span.dataset.column = String(this.column);
    span.addEventListener('mousedown', (e) => {
      e.preventDefault();
      e.stopPropagation();
      if (diamondClickCallback.fn) {
        diamondClickCallback.fn(this.line, this.column);
      }
    });
    return span;
  }

  /** @param {DiamondWidget} other */
  eq(other) {
    return this.active === other.active && this.line === other.line && this.column === other.column;
  }

  get estimatedHeight() { return -1; }
  ignoreEvent() { return false; }
}

/**
 * Decoration computed from diamondMarkersField — renders diamond widgets
 * at each marker position.
 */
const diamondMarkersPlugin = EditorView.decorations.compute(
  [diamondMarkersField],
  (state) => {
    const markers = state.field(diamondMarkersField);
    if (markers.length === 0) return Decoration.none;

    try {
      const sorted = [...markers].sort((a, b) => a.pos - b.pos);
      const decos = [];
      for (const m of sorted) {
        decos.push(
          Decoration.widget({
            widget: new DiamondWidget(m.active, m.line, m.column),
            side: -1, // Before the expression text
          }).range(m.pos)
        );
      }
      return Decoration.set(decos);
    } catch {
      return Decoration.none;
    }
  }
);

// =========================================================================
// Helper: convert 1-indexed line/column to doc offsets
// =========================================================================

/**
 * Converts 1-indexed line/column span to 0-based document offsets.
 * Columns are 1-indexed in the reader, CodeMirror uses 0-indexed from line start.
 *
 * @param {import('@codemirror/state').Text} doc - CodeMirror document
 * @param {number} line - 1-indexed start line
 * @param {number} column - 1-indexed start column
 * @param {number} endLine - 1-indexed end line
 * @param {number} endColumn - 1-indexed end column
 * @returns {{from: number, to: number}|null} Doc offsets, or null if out of range
 */
function spanToOffsets(doc, line, column, endLine, endColumn) {
  try {
    const startLine = doc.line(line);
    const from = startLine.from + (column - 1);
    const eLine = doc.line(endLine);
    const to = eLine.from + (endColumn - 1);
    if (from < 0 || to > doc.length || from > to) return null;
    return { from, to };
  } catch {
    return null;
  }
}

// =========================================================================
// Editor component
// =========================================================================

/**
 * Creates a CodeMirror 6 source editor for the Scheme-JS panel.
 *
 * The editor is read-only (except for breakpoint toggling via gutter clicks).
 * The theme matches the current DevTools theme and hot-swaps on change.
 *
 * @param {HTMLElement} container - DOM element to mount the editor into
 * @param {function(number, boolean): void} [onBreakpointToggle]
 *   Called when a breakpoint is toggled. Arguments: (1-indexed line, isNowSet).
 * @param {function(number, number): void} [onDiamondClick]
 *   Called when a diamond marker is clicked. Arguments: (1-indexed line, 1-indexed column).
 * @returns {{
 *   view: EditorView,
 *   setContent: function(string): void,
 *   scrollToLine: function(number): void,
 *   highlightLine: function(number|null): void,
 *   setBreakpoints: function(Set<number>): void,
 *   highlightExpression: function(number|null, number|null, number|null, number|null): void,
 *   setDiamondMarkers: function(Array<{line: number, column: number, active: boolean}>): void,
 * }}
 */
export function createEditor(container, onBreakpointToggle, onDiamondClick) {
  const prefersDark = window.matchMedia('(prefers-color-scheme: dark)');

  const state = EditorState.create({
    doc: '',
    extensions: [
      // Breakpoint gutter (left of line numbers)
      breakpointField,
      gutter({
        class: 'cm-breakpoint-gutter',
        markers: (view) => {
          const bps = view.state.field(breakpointField);
          const builder = new RangeSetBuilder();
          for (let i = 1; i <= view.state.doc.lines; i++) {
            if (bps.has(i)) {
              const line = view.state.doc.line(i);
              builder.add(line.from, line.from, breakpointMarker);
            }
          }
          return builder.finish();
        },
        domEventHandlers: {
          mousedown(view, line) {
            // Convert character position to 1-indexed line number
            const lineNo = view.state.doc.lineAt(line.from).number;
            view.dispatch({ effects: toggleBreakpointEffect.of(lineNo) });
            const isNowSet = view.state.field(breakpointField).has(lineNo);
            if (onBreakpointToggle) onBreakpointToggle(lineNo, isNowSet);
            return true;
          },
        },
      }),

      // Current-line highlight
      currentLineField,
      currentLinePlugin,

      // Current expression highlight
      currentExpressionField,
      currentExpressionPlugin,

      // Inline diamond markers at expression boundaries
      diamondMarkersField,
      diamondMarkersPlugin,

      lineNumbers(),
      highlightActiveLine(),
      languageCompartment.of(schemeLanguage),
      search({ top: true }),
      keymap.of(searchKeymap),
      layoutTheme,
      themeCompartment.of(makeThemeExtension(prefersDark.matches)),
      EditorView.editable.of(false),
      EditorView.lineWrapping,
    ],
  });

  const view = new EditorView({ state, parent: container });

  // Wire up diamond click callback
  diamondClickCallback.fn = onDiamondClick || null;

  // Re-theme when DevTools switches between dark and light mode.
  prefersDark.addEventListener('change', (e) => {
    view.dispatch({
      effects: themeCompartment.reconfigure(makeThemeExtension(e.matches)),
    });
  });

  /**
   * Replaces the editor content with new source text.
   * Clears current-line highlight when content changes.
   * Optionally switches the language mode (Scheme or JavaScript).
   *
   * @param {string} content - The source code to display
   * @param {'scheme'|'javascript'|'html'} [language='scheme'] - Syntax highlighting mode
   */
  function setContent(content, language = 'scheme') {
    let langExtension;
    if (language === 'javascript') {
      langExtension = javascript();
    } else if (language === 'html') {
      langExtension = html({
        nestedLanguages: [
          {
            tag: "script",
            attrs: attrs => attrs.type === "text/scheme" || attrs.type === "text/x-scheme",
            parser: schemeLanguage.parser
          }
        ]
      });
    } else {
      langExtension = schemeLanguage;
    }
    
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: content },
      effects: [
        setCurrentLineEffect.of(null),
        setBreakpointsEffect.of(new Set()),
        setCurrentExpressionEffect.of(null),
        setDiamondMarkersEffect.of([]),
        languageCompartment.reconfigure(langExtension),
      ],
    });
  }

  /**
   * Scrolls the editor to bring a 1-indexed line into view.
   *
   * @param {number} line - 1-indexed line number
   */
  function scrollToLine(line) {
    try {
      const lineInfo = view.state.doc.line(Math.max(1, line));
      view.dispatch({
        effects: EditorView.scrollIntoView(lineInfo.from, { y: 'center' }),
      });
    } catch {
      // Line out of range — ignore
    }
  }

  /**
   * Highlights the current execution line (shown as a yellow bar).
   * Pass null to clear the highlight.
   *
   * @param {number|null} line - 1-indexed line number, or null to clear
   */
  function highlightLine(line) {
    view.dispatch({ effects: setCurrentLineEffect.of(line ?? null) });
    if (line !== null) {
      scrollToLine(line);
    }
  }

  /**
   * Bulk-sets the breakpoints displayed in the gutter.
   * Replaces any existing breakpoints.
   *
   * @param {Set<number>} lines - Set of 1-indexed line numbers
   */
  function setBreakpoints(lines) {
    view.dispatch({ effects: setBreakpointsEffect.of(new Set(lines)) });
  }

  /**
   * Highlights the exact expression range in the editor (e.g., when paused).
   * Pass all nulls to clear the highlight.
   *
   * @param {number|null} line - 1-indexed start line, or null to clear
   * @param {number|null} col - 1-indexed start column, or null to clear
   * @param {number|null} endLine - 1-indexed end line, or null to clear
   * @param {number|null} endCol - 1-indexed end column, or null to clear
   */
  function highlightExpression(line, col, endLine, endCol) {
    if (line == null || col == null || endLine == null || endCol == null) {
      view.dispatch({ effects: setCurrentExpressionEffect.of(null) });
      return;
    }
    const offsets = spanToOffsets(view.state.doc, line, col, endLine, endCol);
    if (offsets) {
      view.dispatch({ effects: setCurrentExpressionEffect.of(offsets) });
    } else {
      view.dispatch({ effects: setCurrentExpressionEffect.of(null) });
    }
  }

  /**
   * Sets inline diamond markers at expression boundaries.
   * Diamonds appear at the start of each breakable sub-expression on lines
   * that have a line breakpoint or are the current paused line.
   *
   * @param {Array<{line: number, column: number, active: boolean}>} markers
   *   Array of 1-indexed line/column positions with active state.
   *   `active` = true renders a filled diamond (◆), false renders hollow (◇).
   */
  function setDiamondMarkers(markers) {
    const resolved = [];
    for (const m of markers) {
      try {
        const lineInfo = view.state.doc.line(m.line);
        const pos = lineInfo.from + (m.column - 1);
        if (pos >= 0 && pos <= view.state.doc.length) {
          resolved.push({ pos, active: m.active, line: m.line, column: m.column });
        }
      } catch {
        // Line out of range — skip
      }
    }
    view.dispatch({ effects: setDiamondMarkersEffect.of(resolved) });
  }

  return {
    view, setContent, scrollToLine, highlightLine, setBreakpoints,
    highlightExpression, setDiamondMarkers,
  };
}
