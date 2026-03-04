/**
 * @fileoverview CodeMirror 6 editor wrapper for the Scheme-JS panel.
 *
 * Provides a read-only source viewer with Scheme syntax highlighting,
 * line numbers, and a theme that tracks the Chrome DevTools theme
 * (dark or light) via the `prefers-color-scheme` media query.
 *
 * Chrome DevTools extension panels respond to `prefers-color-scheme`
 * based on the DevTools theme setting, not the OS setting. A `Compartment`
 * holds the active theme + highlight style so they can be hot-swapped
 * without rebuilding the editor when DevTools switches themes.
 *
 * Syntax highlighting uses `HighlightStyle.define()` with lezer `tags`
 * rather than the `.tok-*` CSS-override approach, which has specificity
 * conflicts with `defaultHighlightStyle`'s auto-generated class names.
 *
 * Dark theme:  Dracula-inspired — vibrant, high-contrast on dark backgrounds.
 * Light theme: GitHub-light-inspired — semantic colors on white.
 */

import { EditorView, lineNumbers, highlightActiveLine } from '@codemirror/view';
import { EditorState, Compartment } from '@codemirror/state';
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
});

// =========================================================================
// Dark theme — Dracula palette
//
// Every token colour is chosen to pop clearly on the dark background:
//   pink  (#ff79c6) for keywords — unmistakable, high contrast
//   yellow (#f1fa8c) for strings — warm, distinct from code
//   purple (#bd93f9) for numbers — cool contrast against pink
//   cyan  (#8be9fd) for atoms (#t, #f, chars) — distinct from both
//   green (#50fa7b) for def-predicates (car?, set!) — vivid accent
//   muted blue-gray (#8d93ac) for comments — recedes without disappearing
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
  { tag: tags.string,                           color: '#f1fa8c' },   // yellow
  { tag: tags.number,                           color: '#bd93f9' },   // purple
  { tag: tags.keyword,                          color: '#ff79c6', fontWeight: 'bold' }, // pink
  { tag: [tags.atom, tags.bool],                color: '#8be9fd' },   // cyan
  { tag: tags.meta,                             color: '#ff79c6' },   // pink (quote, `)
  { tag: tags.definition(tags.variableName),    color: '#50fa7b' },   // green (predicates)
  { tag: tags.variableName,                     color: '#f8f8f2' },   // foreground
  { tag: tags.bracket,                          color: '#f8f8f2' },   // foreground
  { tag: tags.invalid,                          color: '#ff5555', textDecoration: 'underline' },
]);

// =========================================================================
// Light theme — GitHub-light palette
//
//   red   (#cf222e) keywords — conventional "reserved word" colour
//   navy  (#0a3069) strings — calm, readable on white
//   blue  (#0550ae) numbers — same family as strings, distinct weight
//   green (#116329) def-predicates — distinct from keywords
//   purple (#8250df) meta/quote — traditional Lisp highlight colour
//   gray  (#6e7781) comments — recedes without disappearing
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
  { tag: tags.string,                           color: '#0a3069' },   // navy
  { tag: tags.number,                           color: '#0550ae' },   // blue
  { tag: tags.keyword,                          color: '#cf222e', fontWeight: 'bold' }, // red
  { tag: [tags.atom, tags.bool],                color: '#0550ae' },   // blue
  { tag: tags.meta,                             color: '#8250df' },   // purple (quote, `)
  { tag: tags.definition(tags.variableName),    color: '#116329' },   // dark green
  { tag: tags.variableName,                     color: '#24292f' },   // foreground
  { tag: tags.bracket,                          color: '#24292f' },   // foreground
  { tag: tags.invalid,                          color: '#cf222e', textDecoration: 'underline' },
]);

// =========================================================================
// Compartment — hot-swap theme without rebuilding editor state
// =========================================================================

/**
 * A `Compartment` that holds both the editor visual theme and the syntax
 * highlight style together, so they switch atomically.
 */
const themeCompartment = new Compartment();

/**
 * Bundles an editor theme + highlight style into a single extension value
 * suitable for the `themeCompartment`.
 *
 * @param {boolean} dark - Whether to use the dark or light theme
 * @returns {import('@codemirror/state').Extension}
 */
function makeThemeExtension(dark) {
  return dark
    ? [darkEditorTheme,  syntaxHighlighting(darkHighlight)]
    : [lightEditorTheme, syntaxHighlighting(lightHighlight)];
}

// =========================================================================
// Editor component
// =========================================================================

/**
 * Creates a CodeMirror 6 read-only source editor for Scheme code.
 *
 * The editor starts with the theme matching the current DevTools theme
 * (`prefers-color-scheme`) and re-themes automatically if the user
 * switches between dark and light DevTools while the panel is open.
 *
 * @param {HTMLElement} container - DOM element to mount the editor into
 * @returns {{view: EditorView, setContent: Function, scrollToLine: Function}}
 */
export function createEditor(container) {
  const prefersDark = window.matchMedia('(prefers-color-scheme: dark)');

  const state = EditorState.create({
    doc: '',
    extensions: [
      lineNumbers(),
      highlightActiveLine(),
      schemeLanguage,
      layoutTheme,
      themeCompartment.of(makeThemeExtension(prefersDark.matches)),
      EditorView.editable.of(false),
      EditorView.lineWrapping,
    ],
  });

  const view = new EditorView({
    state,
    parent: container,
  });

  // Re-theme when DevTools switches between dark and light mode.
  // Chrome DevTools extension pages fire this event when the DevTools
  // theme setting changes (independent of the OS theme).
  prefersDark.addEventListener('change', (e) => {
    view.dispatch({
      effects: themeCompartment.reconfigure(makeThemeExtension(e.matches)),
    });
  });

  /**
   * Replaces the editor content with new Scheme source text.
   * @param {string} content - The source code to display
   */
  function setContent(content) {
    view.dispatch({
      changes: {
        from: 0,
        to: view.state.doc.length,
        insert: content,
      },
    });
  }

  /**
   * Scrolls the editor to bring a 1-indexed line into view.
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

  return { view, setContent, scrollToLine };
}
