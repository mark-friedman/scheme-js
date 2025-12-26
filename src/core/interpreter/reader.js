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
  // Context for datum labels: map of id -> Placeholder
  const context = { labels: new Map() };

  while (tokens.length > 0) {
    const expr = readFromTokens(tokens, caseFold, context);
    // If the expression is a Placeholder, it means top-level #n# (unlikely but possible)
    // or #n=... which returns the value. 
    // We need to run fixup on the result to resolve internal cycles.
    expressions.push(fixup(expr));
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
  const regex = /\s*(#\\(?:x[0-9a-fA-F]+|[a-zA-Z]+|.)|#u8\(|#\(|[()]|'|`|,@|,|"(?:\\.|[^"])*"|\|(?:[^|\\]|\\.)*\||;[^\n]*|[+-]?(?:nan|inf)\.0|[^\s();]+)(.*)/s;


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

function readFromTokens(tokens, caseFold = false, context = { labels: new Map() }) {
  if (tokens.length === 0) {
    throw new Error("Unexpected EOF");
  }
  // console.log("readFromTokens:", tokens[0], tokens.length);

  const token = tokens.shift();

  // Handle fused tokens like #1=100
  if (token.includes('=') && /^#\d+=/.test(token)) {
    const match = token.match(/^(#\d+=)(.*)/);
    if (match) {
      const labelToken = match[1];
      let remainder = match[2];

      const id = parseInt(labelToken.slice(1, -1), 10);

      // Create placeholder and register it
      const placeholder = new Placeholder(id);
      context.labels.set(id, placeholder);

      let val;
      if (remainder) {
        // Recurse on the remainder as a single token (or unshift if it's complex?)
        // If remainder is "100", readAtom("100") works.
        // If remainder is "(...", it's a list start.
        // Unshifting is safer generally, but readAtom takes a single token.
        // Tokenizer splits at delimiters, so remainder is likely an atom or empty if separated by space.

        // If remainder is empty (split by space), we read NEXT token.
        // But here we have remainder.
        // Check if remainder is start of list? 
        // Tokenizer splits before '(', so remainder won't be '('.
        // Unless it's something like #1=#u8(...

        // Let's assume remainder is a valid single token if present.
        // But wait, if input is "#1=(a)", tokenizer gives "#1=", "(" ... because ( is delimiter.
        // Remainder might be start of a list/vector (e.g. #0=(a...))
        // Unshift it back to tokens and recurse to handle it properly
        // Check if remainder was split from a delimiter (e.g. #0=#( -> remainder=#, next=( )
        if (remainder === '#' && tokens.length > 0 && tokens[0] === '(') {
          remainder = '#(';
          tokens.shift();
        } else if (remainder === '#u8' && tokens.length > 0 && tokens[0] === '(') {
          remainder = '#u8(';
          tokens.shift();
        }

        tokens.unshift(remainder);
        val = readFromTokens(tokens, caseFold, context);
      } else {
        // No remainder, read next token
        val = readFromTokens(tokens, caseFold, context);
      }

      placeholder.value = val;
      placeholder.resolved = true;
      return val;
    }
  }

  // Handle #n= standalone token
  if (/^#\d+=$/.test(token)) {
    const id = parseInt(token.slice(1, -1), 10);
    const placeholder = new Placeholder(id);
    context.labels.set(id, placeholder);

    const val = readFromTokens(tokens, caseFold, context);
    placeholder.value = val;
    placeholder.resolved = true;
    return val;
  }

  // Handle #n# reference
  if (/^#\d+#$/.test(token)) {
    const id = parseInt(token.slice(1, -1), 10);
    const placeholder = context.labels.get(id);
    if (!placeholder) {
      throw new Error(`Reference to undefined label #${id}#`);
    }
    return placeholder;
  }

  if (token === '(') {
    return readList(tokens, caseFold, context);
  }
  if (token === '#(') {
    return readVector(tokens, caseFold, context);
  }
  if (token === '#u8(') {
    return readBytevector(tokens);
  }
  if (token === ')') {
    throw new Error("Unexpected ')'");
  }

  // Quotes
  if (token === "'") {
    return list(intern('quote'), readFromTokens(tokens, caseFold, context));
  }
  if (token === '`') {
    return list(intern('quasiquote'), readFromTokens(tokens, caseFold, context));
  }
  if (token === ',') {
    return list(intern('unquote'), readFromTokens(tokens, caseFold, context));
  }
  if (token === ',@') {
    return list(intern('unquote-splicing'), readFromTokens(tokens, caseFold, context));
  }

  // Vertical bar delimited symbol |...|
  if (token.startsWith('|') && token.endsWith('|')) {
    // Extract the name between the bars and process escape sequences
    const inner = token.slice(1, -1);
    // Handle escape sequences: \| -> |, \\ -> \, \x...; -> char
    const name = inner.replace(/\\(.)/g, (match, char) => {
      if (char === '|') return '|';
      if (char === '\\') return '\\';
      if (char === 'x') {
        // Hex escape should be handled separately with the semicolon
        return match; // Keep as-is for now
      }
      return char;
    });
    return intern(name);
  }

  return readAtom(token, caseFold);
}

function readList(tokens, caseFold = false, context) {
  const listItems = [];
  while (tokens[0] !== ')') {
    if (tokens.length === 0) {
      throw new Error("Missing ')'");
    }
    if (tokens[0] === '.') {
      tokens.shift(); // consume '.'
      const tail = readFromTokens(tokens, caseFold, context);
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
    listItems.push(readFromTokens(tokens, caseFold, context));
  }
  tokens.shift(); // consume ')'

  // Build proper list
  return list(...listItems);
}

function readVector(tokens, caseFold = false, context) {
  const elements = [];
  while (tokens[0] !== ')') {
    if (tokens.length === 0) {
      throw new Error("Missing ')' for vector");
    }
    elements.push(readFromTokens(tokens, caseFold, context));
  }
  tokens.shift(); // consume ')'
  return elements; // Return raw JS array
}

/**
 * Reads a bytevector literal #u8(...)
 * @param {string[]} tokens - Token array
 * @returns {Uint8Array}
 */
function readBytevector(tokens) {
  const bytes = [];
  while (tokens[0] !== ')') {
    if (tokens.length === 0) {
      throw new Error("Missing ')' for bytevector");
    }
    const token = tokens.shift();
    const num = parseInt(token, 10);
    if (isNaN(num) || num < 0 || num > 255) {
      throw new Error(`Invalid byte value in bytevector: ${token}`);
    }
    bytes.push(num);
  }
  tokens.shift(); // consume ')'
  return new Uint8Array(bytes);
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
  // Normalize R7RS exponent markers (s, f, d, l) to 'e' globally before parsing
  // This handles 1s2 -> 1e2, 1s2+3d4i -> 1e2+3e4i, etc.
  if (/[sSfFdDlL]/.test(token) && !token.startsWith('#')) {
    token = token.replace(/[sSfFdDlL]/g, 'e');
  }

  // Handle prefixed numbers (#x, #o, #b, #d, #e, #i)
  if (token.startsWith('#')) {
    return parsePrefixedNumber(token);
  }

  // Helper to parse a real component string into a number
  const parseRealStr = (str) => {
    if (!str) return 0;
    const lower = str.toLowerCase();
    if (lower.endsWith('inf.0')) {
      return lower.startsWith('-') ? -Infinity : Infinity;
    }
    if (lower.endsWith('nan.0')) {
      return NaN;
    }
    return parseFloat(str);
  };

  // Handle immediate special values
  if (/^[+-]?inf\.0$/i.test(token)) return parseRealStr(token);
  if (/^[+-]?nan\.0$/i.test(token)) return NaN;

  // Pattern for real numbers: integers, decimals, scientific notation, inf.0, nan.0
  const REAL_PATTERN = '([+-]?(?:(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?|inf\\.0|nan\\.0))';

  // Complex: real+imag (e.g., 1+2i, 1e2+3e4i, 1+inf.0i)
  const complexRegex = new RegExp(`^${REAL_PATTERN}([+-])${REAL_PATTERN}?i$`, 'i');
  const complexMatch = token.match(complexRegex);

  if (complexMatch) {
    const real = parseRealStr(complexMatch[1]);
    const sign = complexMatch[2] === '-' ? -1 : 1;
    const imagStr = complexMatch[3];
    const imag = imagStr ? sign * parseRealStr(imagStr) : sign;
    return new Complex(real, imag);
  }

  // Pure imaginary: +i, -i, 3i, +inf.0i
  const pureImagRegex = new RegExp(`^(${REAL_PATTERN}|[+-])i$`, 'i');
  const pureImagMatch = token.match(pureImagRegex);

  if (pureImagMatch) {
    const part = pureImagMatch[1];
    if (part === '+' || part === '') return new Complex(0, 1);
    if (part === '-') return new Complex(0, -1);
    return new Complex(0, parseRealStr(part));
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
  let num = Number(token);
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

  // Normalize alternative exponent markers for decimal numbers
  if (radix === 10 && /^[+-]?(\d+\.?\d*|\.\d+)[sSfFdDlL][+-]?\d+$/.test(rest)) {
    rest = rest.replace(/[sSfFdDlL]/, 'e');
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

// Helper for circular structure resolution
class Placeholder {
  constructor(id) {
    this.id = id;
    this.value = null; // Will be set when the labelled datum is fully read
    this.resolved = false;
  }
}

/**
 * Traverses the object graph replacing Placeholders with their resolved values.
 * Handles recursion and cycle detection using a visited Set.
 * @param {*} obj
 * @param {Set} visited
 * @returns {*} The fixed-up object
 */
function fixup(obj, visited = new Set()) {
  if (obj === null || typeof obj !== 'object') {
    return obj;
  }

  if (visited.has(obj)) {
    return obj;
  }
  visited.add(obj);

  // Handle Cons pairs
  if (obj instanceof Cons) {
    if (obj.car instanceof Placeholder) {
      if (!obj.car.resolved) throw new Error(`Reference to undefined label #${obj.car.id}#`);
      obj.car = obj.car.value;
    } else {
      fixup(obj.car, visited);
    }

    if (obj.cdr instanceof Placeholder) {
      if (!obj.cdr.resolved) throw new Error(`Reference to undefined label #${obj.cdr.id}#`);
      obj.cdr = obj.cdr.value;
    } else {
      fixup(obj.cdr, visited);
    }
    return obj;
  }

  // Handle Vectors (Arrays)
  if (Array.isArray(obj)) {
    for (let i = 0; i < obj.length; i++) {
      if (obj[i] instanceof Placeholder) {
        if (!obj[i].resolved) throw new Error(`Reference to undefined label #${obj[i].id}#`);
        obj[i] = obj[i].value;
      } else {
        fixup(obj[i], visited);
      }
    }
    return obj;
  }

  return obj;
}
