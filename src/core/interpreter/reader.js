import { Cons, cons, list } from './cons.js';
import { Symbol, intern } from './symbol.js';


/**
 * Parses a string of Scheme code into a list of S-expressions.
 * @param {string} input - Source code to parse
 * @param {Object} [options] - Parsing options
 * @param {boolean} [options.caseFold=false] - If true, fold symbol names to lowercase (for include-ci)
 * @returns {Array} Array of S-expressions (Cons, Symbol, number, etc.)
 */
export function parse(input, options = {}) {
  const caseFold = options.caseFold || false;
  const tokens = tokenize(input);
  const expressions = [];
  while (tokens.length > 0) {
    expressions.push(readFromTokens(tokens, caseFold));
  }
  return expressions;
}

function tokenize(input) {
  // Regex based tokenizer
  // Matches:
  // 1. #\ followed by character name or single char (character literals)
  // 2. #( - Vector start
  // 3. ( or ) - List delimiters
  // 4. ' ` ,@ , - Quote/Quasiquote
  // 5. Strings
  // 6. Comments (;...)
  // 7. Special numbers (+nan.0, etc.)
  // 8. Atoms (anything else, stops at whitespace, parens, or semicolon)
  // Note: #\\ must come before #( to avoid partial matches
  const regex = /\s*(#\\(?:x[0-9a-fA-F]+|[a-zA-Z]+|.)|#\(|[()]|'|`|,@|,|"(?:\\.|[^"])*"|;[^\n]*|[+-]?(?:nan|inf)\.0|[^\s();]+)(.*)/s;


  const tokens = [];
  let current = input;

  while (current.length > 0) {
    const match = current.match(regex);
    if (!match) break;

    const token = match[1];
    current = match[2]; // Rest of string

    if (token.startsWith(';')) continue; // Skip comments
    if (token.trim() === '') continue;   // Skip whitespace

    tokens.push(token);
  }
  return tokens;
}

function readFromTokens(tokens, caseFold = false) {
  if (tokens.length === 0) {
    throw new Error("Unexpected EOF");
  }

  const token = tokens.shift();

  if (token === '(') {
    return readList(tokens, caseFold);
  }
  if (token === '#(') {
    return readVector(tokens, caseFold);
  }
  if (token === ')') {
    throw new Error("Unexpected ')'");
  }

  // Quotes
  if (token === "'") {
    return list(intern('quote'), readFromTokens(tokens, caseFold));
  }
  if (token === '`') {
    return list(intern('quasiquote'), readFromTokens(tokens, caseFold));
  }
  if (token === ',') {
    return list(intern('unquote'), readFromTokens(tokens, caseFold));
  }
  if (token === ',@') {
    return list(intern('unquote-splicing'), readFromTokens(tokens, caseFold));
  }

  return readAtom(token, caseFold);
}

function readList(tokens, caseFold = false) {
  const listItems = [];
  while (tokens[0] !== ')') {
    if (tokens.length === 0) {
      throw new Error("Missing ')'");
    }
    if (tokens[0] === '.') {
      tokens.shift(); // consume '.'
      const tail = readFromTokens(tokens, caseFold);
      if (tokens.shift() !== ')') {
        throw new Error("Expected ')' after improper list tail");
      }
      // Build improper list
      let result = tail;
      for (let i = listItems.length - 1; i >= 0; i--) {
        result = cons(listItems[i], result);
      }
      return result;
    }
    listItems.push(readFromTokens(tokens, caseFold));
  }
  tokens.shift(); // consume ')'

  // Build proper list
  return list(...listItems);
}

function readVector(tokens, caseFold = false) {
  const elements = [];
  while (tokens[0] !== ')') {
    if (tokens.length === 0) {
      throw new Error("Missing ')' for vector");
    }
    elements.push(readFromTokens(tokens, caseFold));
  }
  tokens.shift(); // consume ')'
  return elements; // Return raw JS array
}

function readAtom(token, caseFold = false) {
  // Numbers
  const num = Number(token);
  if (!isNaN(num)) {
    return num;
  }

  // Special numbers
  if (token === '+nan.0' || token === '-nan.0') return NaN;
  if (token === '+inf.0') return Infinity;
  if (token === '-inf.0') return -Infinity;

  // Booleans
  if (token === '#t') return true;
  if (token === '#f') return false;

  // Character literals (#\a, #\newline, #\x41, etc.)
  if (token.startsWith('#\\')) {
    return readCharacter(token.slice(2));
  }

  // Strings (not case-folded)
  if (token.startsWith('"')) {
    // Remove quotes and handle escapes
    return token.slice(1, -1)
      .replace(/\\n/g, '\n')
      .replace(/\\"/g, '"')
      .replace(/\\\\/g, '\\');
  }

  // Symbols - apply case folding if enabled
  const symbolName = caseFold ? token.toLowerCase() : token;
  return intern(symbolName);
}

/**
 * Named character constants per R7RS §6.6.
 */
const NAMED_CHARACTERS = {
  'alarm': '\x07',
  'backspace': '\x08',
  'delete': '\x7F',
  'escape': '\x1B',
  'newline': '\n',
  'null': '\x00',
  'return': '\r',
  'space': ' ',
  'tab': '\t'
};

/**
 * Parses a character literal (after the #\ prefix).
 * @param {string} name - The character name or literal
 * @returns {string} Single-character string (Scheme character)
 */
function readCharacter(name) {
  // Hex escape: #\x41 -> 'A'
  if (name.startsWith('x') && name.length > 1) {
    const codePoint = parseInt(name.slice(1), 16);
    if (isNaN(codePoint)) {
      throw new Error(`Invalid character hex escape: #\\${name}`);
    }
    return String.fromCodePoint(codePoint);
  }

  // Named character: #\newline -> '\n'
  const lower = name.toLowerCase();
  if (NAMED_CHARACTERS.hasOwnProperty(lower)) {
    return NAMED_CHARACTERS[lower];
  }

  // Single character: #\a -> 'a'
  if (name.length === 1) {
    return name;
  }

  throw new Error(`Unknown character name: #\\${name}`);
}
