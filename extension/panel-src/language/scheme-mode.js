/**
 * @fileoverview CodeMirror 6 Scheme language mode.
 *
 * A StreamParser-based language mode for S-expression syntax.
 * Handles: parentheses, strings, characters, numbers, comments,
 * keywords/special forms, booleans, and symbols.
 */

import { StreamLanguage } from '@codemirror/language';

// Special forms and built-in syntax keywords
const KEYWORDS = new Set([
  'define', 'lambda', 'if', 'cond', 'case', 'and', 'or', 'when', 'unless',
  'let', 'let*', 'letrec', 'letrec*', 'begin', 'do', 'delay', 'force',
  'quasiquote', 'unquote', 'unquote-splicing', 'quote', 'set!', 'define-syntax',
  'let-syntax', 'letrec-syntax', 'syntax-rules', 'define-record-type',
  'define-values', 'define-library', 'import', 'export', 'include',
  'parameterize', 'guard', 'with-exception-handler', 'raise', 'raise-continuable',
  'dynamic-wind', 'call-with-current-continuation', 'call/cc',
  'values', 'call-with-values', 'apply', 'map', 'for-each',
  'let-values', 'let*-values', 'receive',
]);

/**
 * Checks if a character can be part of a symbol name.
 * Scheme symbols can contain most non-whitespace, non-delimiter chars.
 * @param {string} ch
 * @returns {boolean}
 */
function isSymbolChar(ch) {
  return !/[\s\(\)\[\]{}"`;,]/.test(ch);
}

/**
 * Checks if a string looks like a Scheme number.
 * Handles: integers, floats, rationals (a/b), #b/#o/#d/#x prefixes.
 * @param {string} token
 * @returns {boolean}
 */
function isNumber(token) {
  // Exact/inexact prefix
  const stripped = token.replace(/^#[eEiI]/, '');

  // Radix prefixed numbers: #b, #o, #d, #x
  if (/^#[bBoOdDxX]/.test(stripped)) return true;

  // Standard number patterns: integer, float, rational, signed
  return /^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/.test(stripped)
    || /^[+-]?\d+\/\d+$/.test(stripped)
    || /^[+-]inf\.0$/.test(stripped)
    || /^[+-]nan\.0$/.test(stripped);
}

/**
 * The Scheme StreamParser for CodeMirror 6.
 */
const schemeParser = {
  /**
   * @param {import('@codemirror/language').StringStream} stream
   * @param {Object} state
   * @returns {string|null} CSS token class
   */
  token(stream, state) {
    // Skip whitespace
    if (stream.eatSpace()) return null;

    const ch = stream.next();

    // Line comment
    if (ch === ';') {
      stream.skipToEnd();
      return 'comment';
    }

    // Block comment #| ... |#
    if (ch === '#' && stream.peek() === '|') {
      stream.next(); // consume |
      state.blockComment = true;
      // Fall through to handle in next token if still open
    }
    if (state.blockComment) {
      while (!stream.eol()) {
        if (stream.match('|#')) {
          state.blockComment = false;
          break;
        }
        stream.next();
      }
      return 'comment';
    }

    // Datum comment #; (skip next datum)
    if (ch === '#' && stream.peek() === ';') {
      stream.next();
      return 'comment';
    }

    // String literal
    if (ch === '"') {
      while (!stream.eol()) {
        const c = stream.next();
        if (c === '\\') {
          stream.next(); // skip escaped char
        } else if (c === '"') {
          break;
        }
      }
      return 'string';
    }

    // Character literal #\...
    if (ch === '#' && stream.peek() === '\\') {
      stream.next(); // consume \
      stream.match(/\S+/); // consume the character name/value
      return 'atom'; // treat chars like atoms/literals
    }

    // Boolean literals #t #f #true #false
    if (ch === '#') {
      if (stream.match('true') || stream.match('false')) {
        return 'atom';
      }
      if (stream.peek() === 't' || stream.peek() === 'f'
          || stream.peek() === 'T' || stream.peek() === 'F') {
        stream.next();
        return 'atom';
      }
      // Number prefix: #b #o #d #x #e #i
      if (stream.match(/[boOdDxXeEiI][^\s\(\)\[\]{}"`;,]*/)) {
        return 'number';
      }
      // Vector/bytevector prefix #( #u8(
      return 'bracket';
    }

    // Parentheses and brackets
    if (ch === '(' || ch === ')' || ch === '[' || ch === ']') {
      return 'bracket';
    }

    // Quote shorthands
    if (ch === "'" || ch === '`') return 'meta';
    if (ch === ',') {
      stream.eat('@');
      return 'meta';
    }

    // Symbol or number: read the rest of the token
    let token = ch;
    while (stream.peek() && isSymbolChar(stream.peek())) {
      token += stream.next();
    }

    // Number check
    if (isNumber(token)) return 'number';

    // Keyword check (special forms)
    if (KEYWORDS.has(token)) return 'keyword';

    // Predicates and mutators by convention
    if (token.endsWith('?')) return 'def';
    if (token.endsWith('!')) return 'def';

    // Default: symbol
    return 'variable';
  },

  startState() {
    return { blockComment: false };
  },

  copyState(state) {
    return { blockComment: state.blockComment };
  },
};

/**
 * A CodeMirror 6 LanguageSupport extension for Scheme.
 */
export const schemeLanguage = StreamLanguage.define(schemeParser);
