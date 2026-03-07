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
  Decoration,
} from '@codemirror/view';
import {
  EditorState, Compartment, StateField, StateEffect, RangeSetBuilder,
} from '@codemirror/state';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { schemeLanguage } from '../language/scheme-mode.js';

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
 * @returns {{
 *   view: EditorView,
 *   setContent: function(string): void,
 *   scrollToLine: function(number): void,
 *   highlightLine: function(number|null): void,
 *   setBreakpoints: function(Set<number>): void,
 * }}
 */
export function createEditor(container, onBreakpointToggle) {
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

      lineNumbers(),
      highlightActiveLine(),
      schemeLanguage,
      layoutTheme,
      themeCompartment.of(makeThemeExtension(prefersDark.matches)),
      EditorView.editable.of(false),
      EditorView.lineWrapping,
    ],
  });

  const view = new EditorView({ state, parent: container });

  // Re-theme when DevTools switches between dark and light mode.
  prefersDark.addEventListener('change', (e) => {
    view.dispatch({
      effects: themeCompartment.reconfigure(makeThemeExtension(e.matches)),
    });
  });

  /**
   * Replaces the editor content with new Scheme source text.
   * Clears current-line highlight when content changes.
   *
   * @param {string} content - The source code to display
   */
  function setContent(content) {
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: content },
      effects: [
        setCurrentLineEffect.of(null),
        setBreakpointsEffect.of(new Set()),
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

  return { view, setContent, scrollToLine, highlightLine, setBreakpoints };
}
