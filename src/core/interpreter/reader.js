import { Cons, cons, list } from './cons.js';
import { Symbol, intern } from './symbol.js';
import { Rational } from '../primitives/rational.js';
import { Complex } from '../primitives/complex.js';


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
  // Try to parse as a number (including rationals and complex)
  const numResult = parseNumber(token);
  if (numResult !== null) {
    return numResult;
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
 * Parses a numeric literal (integers, rationals, complex, with optional prefixes)
 * R7RS supports prefixes: #b (binary), #o (octal), #d (decimal), #x (hex), #e (exact), #i (inexact)
 * @param {string} token 
 * @returns {number|Rational|Complex|null}
 */
function parseNumber(token) {
  // Handle prefixed numbers (#x, #o, #b, #d, #e, #i)
  if (token.startsWith('#')) {
    return parsePrefixedNumber(token);
  }

  // Complex number with real and imaginary parts: 3+4i, 3-4i, 1+i, 1-i
  const complexMatch = token.match(/^([+-]?\d+\.?\d*|\d*\.?\d+)([+-])(\d+\.?\d*)?i$/);
  if (complexMatch) {
    const real = parseFloat(complexMatch[1]);
    const sign = complexMatch[2] === '-' ? -1 : 1;
    const imag = complexMatch[3] ? sign * parseFloat(complexMatch[3]) : sign;
    return new Complex(real, imag);
  }

  // Pure imaginary with explicit sign: +i, -i, +3i, -3i 
  // Or with coefficient: 3i, 3.5i (but NOT bare 'i')
  const pureImagMatch = token.match(/^([+-]?\d+\.?\d*|\d*\.\d+|[+-])i$/);
  if (pureImagMatch) {
    const prefix = pureImagMatch[1];
    let imag;
    if (prefix === '+') {
      imag = 1;
    } else if (prefix === '-') {
      imag = -1;
    } else {
      imag = parseFloat(prefix);
    }
    return new Complex(0, imag);
  }

  // Check for rational: 1/2, -3/4, etc.
  const rationalMatch = token.match(/^([+-]?\d+)\/(\d+)$/);
  if (rationalMatch) {
    const num = parseInt(rationalMatch[1], 10);
    const den = parseInt(rationalMatch[2], 10);
    if (den === 0) {
      throw new Error('Division by zero in rational literal');
    }
    return new Rational(num, den);
  }

  // Regular number (integer or decimal)
  const num = Number(token);
  if (!isNaN(num)) {
    return num;
  }

  return null; // Not a number
}

/**
 * Parses a number with R7RS prefix notation.
 * Handles #x (hex), #o (octal), #b (binary), #d (decimal), #e (exact), #i (inexact)
 * and combinations like #e#x10 or #x#e10
 * @param {string} token - Token starting with #
 * @returns {number|Rational|null}
 */
function parsePrefixedNumber(token) {
  let exactness = null; // 'exact', 'inexact', or null
  let radix = 10;
  let rest = token;

  // Parse up to 2 prefixes (one exactness, one radix)
  for (let i = 0; i < 2 && rest.startsWith('#'); i++) {
    const prefix = rest.substring(0, 2).toLowerCase();
    switch (prefix) {
      case '#e':
        exactness = 'exact';
        rest = rest.substring(2);
        break;
      case '#i':
        exactness = 'inexact';
        rest = rest.substring(2);
        break;
      case '#b':
        radix = 2;
        rest = rest.substring(2);
        break;
      case '#o':
        radix = 8;
        rest = rest.substring(2);
        break;
      case '#d':
        radix = 10;
        rest = rest.substring(2);
        break;
      case '#x':
        radix = 16;
        rest = rest.substring(2);
        break;
      default:
        return null; // Not a numeric prefix
    }
  }

  // If still starts with #, it's not a valid number
  if (rest.startsWith('#')) {
    return null;
  }

  // Handle rational with radix: #x10/2 means 16/2 = 8
  const rationalMatch = rest.match(/^([+-]?[0-9a-fA-F]+)\/([0-9a-fA-F]+)$/);
  if (rationalMatch) {
    const num = parseInt(rationalMatch[1], radix);
    const den = parseInt(rationalMatch[2], radix);
    if (isNaN(num) || isNaN(den)) return null;
    if (den === 0) throw new Error('Division by zero in rational literal');

    if (exactness === 'inexact') {
      return num / den;
    }
    return new Rational(num, den);
  }

  // Handle complex with radix: #d10+11i
  const complexMatch = rest.match(/^([+-]?[0-9a-fA-F.]+)([+-])([0-9a-fA-F.]+)?i$/);
  if (complexMatch) {
    const real = radix === 10 ? parseFloat(complexMatch[1]) : parseInt(complexMatch[1], radix);
    const sign = complexMatch[2] === '-' ? -1 : 1;
    const imagPart = complexMatch[3] || '1';
    const imag = sign * (radix === 10 ? parseFloat(imagPart) : parseInt(imagPart, radix));
    return new Complex(real, imag);
  }

  // Parse as integer in the given radix
  let result;
  if (radix === 10 && (rest.includes('.') || rest.toLowerCase().includes('e'))) {
    // Decimal with fractional part or exponent
    result = parseFloat(rest);
  } else {
    result = parseInt(rest, radix);
  }

  if (isNaN(result)) {
    return null;
  }

  // Apply exactness
  if (exactness === 'inexact' && Number.isInteger(result)) {
    result = result + 0.0; // Ensure it's a float (though in JS all numbers are floats)
  } else if (exactness === 'exact' && Number.isInteger(result)) {
    // Already exact, return as-is or as Rational
    return result;
  }

  return result;
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
