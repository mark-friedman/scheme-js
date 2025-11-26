import { Cons, cons, list } from '../data/cons.js';
import { Symbol, intern } from '../data/symbol.js';


/**
 * Parses a string of Scheme code into a list of S-expressions.
 * @param {string} input
 * @returns {Array} Array of S-expressions (Cons, Symbol, number, etc.)
 */
export function parse(input) {
  const tokens = tokenize(input);
  const expressions = [];
  while (tokens.length > 0) {
    expressions.push(readFromTokens(tokens));
  }
  return expressions;
}

function tokenize(input) {
  // Regex based tokenizer
  // Matches:
  // 1. #( - Vector start
  // 2. ( or ) - List delimiters
  // 3. ' ` ,@ , - Quote/Quasiquote
  // 4. Strings
  // 5. Comments
  // 6. Atoms (anything else)
  const regex = /\s*(#\(|[()]|'|`|,@|,|"(?:\\.|[^"])*"|;.*|[^\s()]+)(.*)/s;

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

function readFromTokens(tokens) {
  if (tokens.length === 0) {
    throw new Error("Unexpected EOF");
  }

  const token = tokens.shift();

  if (token === '(') {
    return readList(tokens);
  }
  if (token === '#(') {
    return readVector(tokens);
  }
  if (token === ')') {
    throw new Error("Unexpected ')'");
  }

  // Quotes
  if (token === "'") {
    return list(intern('quote'), readFromTokens(tokens));
  }
  if (token === '`') {
    return list(intern('quasiquote'), readFromTokens(tokens));
  }
  if (token === ',') {
    return list(intern('unquote'), readFromTokens(tokens));
  }
  if (token === ',@') {
    return list(intern('unquote-splicing'), readFromTokens(tokens));
  }

  return readAtom(token);
}

function readList(tokens) {
  const listItems = [];
  while (tokens[0] !== ')') {
    if (tokens.length === 0) {
      throw new Error("Missing ')'");
    }
    if (tokens[0] === '.') {
      tokens.shift(); // consume '.'
      const tail = readFromTokens(tokens);
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
    listItems.push(readFromTokens(tokens));
  }
  tokens.shift(); // consume ')'

  // Build proper list
  return list(...listItems);
}

function readVector(tokens) {
  const elements = [];
  while (tokens[0] !== ')') {
    if (tokens.length === 0) {
      throw new Error("Missing ')' for vector");
    }
    elements.push(readFromTokens(tokens));
  }
  tokens.shift(); // consume ')'
  return elements; // Return raw JS array
}

function readAtom(token) {
  // Numbers
  const num = Number(token);
  if (!isNaN(num)) {
    return num;
  }

  // Booleans
  if (token === '#t') return true;
  if (token === '#f') return false;

  // Strings
  if (token.startsWith('"')) {
    // Remove quotes and handle escapes
    return token.slice(1, -1)
      .replace(/\\n/g, '\n')
      .replace(/\\"/g, '"')
      .replace(/\\\\/g, '\\');
  }

  // Symbols
  return intern(token);
}
