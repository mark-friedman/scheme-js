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
  // State object for parsing context
  const state = {
    caseFold: options.caseFold || false,
    labels: new Map()
  };

  // Strip block comments before tokenizing
  const preprocessed = stripBlockComments(input);
  const tokens = tokenize(preprocessed);
  const expressions = [];

  try {
    while (tokens.length > 0) {
      const expr = readFromTokens(tokens, state);
      // If the expression is a Placeholder, it means top-level #n# (unlikely but possible)
      // or #n=... which returns the value. 
      // We need to run fixup on the result to resolve internal cycles.
      expressions.push(fixup(expr));
    }
  } catch (e) {
    console.error(`Parse error in input: "${input.substring(0, 100)}${input.length > 100 ? '...' : ''}"`);
    throw e;
  }
  return expressions;
}

/**
 * Strips block comments #|...|# from input, including nested ones.
 * @param {string} input - Source code
 * @returns {string} Input with block comments removed
 */
function stripBlockComments(input) {
  let result = '';
  let i = 0;

  while (i < input.length) {
    if (i + 1 < input.length && input[i] === '#' && input[i + 1] === '|') {
      // Start of block comment - find matching end
      let depth = 1;
      i += 2;
      while (i < input.length && depth > 0) {
        if (i + 1 < input.length && input[i] === '#' && input[i + 1] === '|') {
          depth++;
          i += 2;
        } else if (i + 1 < input.length && input[i] === '|' && input[i + 1] === '#') {
          depth--;
          i += 2;
        } else {
          i++;
        }
      }
      // Replace with space to preserve token boundaries
      result += ' ';
    } else {
      result += input[i];
      i++;
    }
  }
  return result;
}



function tokenize(input) {
  // Regex based tokenizer
  // Matches (order matters - longer/more specific matches first):
  // 1. #\ followed by character name or single char (character literals)
  // 2. #u8( - Bytevector start
  // 3. #{ - JS object literal start
  // 4. #( - Vector start
  // 5. #; - Datum comment
  // 6. #!fold-case, #!no-fold-case - Case folding directives
  // 7. ( or ) or } - List/object delimiters
  // 8. ' ` ,@ , - Quote/Quasiquote
  // 9. Strings
  // 10. |...| - Vertical bar delimited symbols
  // 11. Line comments (;...)
  // 12. Complex numbers with inf/nan (e.g., +inf.0+inf.0i, -nan.0+inf.0i)
  // 13. Single special numbers (+nan.0, +inf.0, etc.)
  // 14. Atoms (anything else, stops at whitespace, parens, or semicolon)
  // Note: Updated to capture leading whitespace in group 1
  const regex = /^(\s*)(#\\(?:x[0-9a-fA-F]+|[a-zA-Z]+|.)|#u8\(|#\{|#\(|#;|#!fold-case|#!no-fold-case|'|`|,@|,|[(){}]|"(?:\\.|[^"])*"|\|(?:[^|\\]|\\.)*\||;[^\n]*|[+-]?(?:nan|inf)\.0[+-](?:nan|inf)\.0i|[+-]?(?:nan|inf)\.0|[^\s(){};]+)(.*)/si;

  const tokens = [];
  let current = input;

  // Track if we are at the very beginning of the input
  let isStart = true;

  while (current.length > 0) {
    const match = current.match(regex);
    if (!match) {
      // If we can't match a token but have content, it might be just whitespace at EOF or invalid input
      // Skip one char and try again (fallback, though regex should cover whitespace)
      if (current.trim() === '') break;
      current = current.slice(1);
      continue;
    }

    const whitespace = match[1];
    const tokenStr = match[2];
    current = match[3]; // Rest of string

    if (tokenStr.startsWith(';')) {
      // Comments act as whitespace separator effectively, but we don't count them as "space" for adjacency.
      // Actually, standard behavior: `(a) ;comment\n.prop` -> separate line, effectively separate.
      // We will count comments as breaking adjacency.
      isStart = false;
      continue;
    }

    // hasPrecedingSpace is true if there was whitespace OR we are at start of input
    const hasSpace = (whitespace.length > 0) || isStart;

    tokens.push({ value: tokenStr, hasPrecedingSpace: hasSpace });
    isStart = false; // After first token, not start anymore
  }
  return tokens;
}

function handleDotAccess(expr, tokens) {
  let currentExpr = expr;

  while (tokens.length > 0) {
    const nextToken = tokens[0];

    // Check if next token is a dot property access:
    // 1. Starts with dot
    // 2. Not just "." (improper list delimiter)
    // 3. NO preceding space (adjacent)
    if (nextToken.value.startsWith('.') &&
      nextToken.value !== '.' &&
      !nextToken.hasPrecedingSpace) {

      // Consume token
      tokens.shift();

      // Split property chain (e.g. .a.b -> ["", "a", "b"])
      const parts = nextToken.value.split('.');

      for (let i = 1; i < parts.length; i++) {
        const prop = parts[i];
        if (prop.length === 0) {
          // Skip empty parts (consecutive dots or trailing dot)
          continue;
        }
        currentExpr = list(intern('js-ref'), currentExpr, prop);
      }
    } else {
      break;
    }
  }
  return currentExpr;
}

function readFromTokens(tokens, state) {
  if (tokens.length === 0) {
    throw new Error(`Unexpected EOF while reading ${state.current || 'unknown'}`);
  }

  const tokenObj = tokens.shift();
  const token = tokenObj.value;

  // Handle fold-case directives
  if (token === '#!fold-case') {
    state.caseFold = true;
    if (tokens.length === 0) return undefined;
    return readFromTokens(tokens, state); // Continue to next datum
  }
  if (token === '#!no-fold-case') {
    state.caseFold = false;
    if (tokens.length === 0) return undefined;
    return readFromTokens(tokens, state); // Continue to next datum
  }

  // Handle datum comments: #; skips the next datum
  if (token === '#;') {
    if (tokens.length === 0) {
      throw new Error("read: Unexpected EOF after datum comment marker #;");
    }
    readFromTokens(tokens, state); // Read and discard next datum
    if (tokens.length === 0) return undefined;
    return readFromTokens(tokens, state); // Return the datum after that
  }

  // Handle fused tokens like #1=100
  if (token.includes('=') && /^#\d+=/.test(token)) {
    const match = token.match(/^(#\d+=)(.*)/);
    if (match) {
      const labelToken = match[1];
      let remainder = match[2];

      const id = parseInt(labelToken.slice(1, -1), 10);

      // Create placeholder and register it
      const placeholder = new Placeholder(id);
      state.labels.set(id, placeholder);

      let val;
      if (remainder) {
        // Recurse on the remainder as a single token (or unshift if it's complex?)

        // Check if remainder was split from a delimiter (e.g. #0=#( -> remainder=#, next=( )
        if (remainder === '#' && tokens.length > 0 && tokens[0].value === '(') {
          remainder = '#(';
          tokens.shift();
        } else if (remainder === '#u8' && tokens.length > 0 && tokens[0].value === '(') {
          remainder = '#u8(';
          tokens.shift();
        }

        tokens.unshift({ value: remainder, hasPrecedingSpace: false });
        val = readFromTokens(tokens, state);
      } else {
        // No remainder, read next token
        val = readFromTokens(tokens, state);
      }

      placeholder.value = val;
      placeholder.resolved = true;
      return handleDotAccess(val, tokens);
    }
  }

  // Handle #n= standalone token
  if (/^#\d+=$/.test(token)) {
    const id = parseInt(token.slice(1, -1), 10);
    const placeholder = new Placeholder(id);
    state.labels.set(id, placeholder);

    const val = readFromTokens(tokens, state);
    placeholder.value = val;
    placeholder.resolved = true;
    return handleDotAccess(val, tokens);
  }

  // Handle #n# reference
  if (/^#\d+#$/.test(token)) {
    const id = parseInt(token.slice(1, -1), 10);
    const placeholder = state.labels.get(id);
    if (!placeholder) {
      throw new Error(`Reference to undefined label #${id}#`);
    }
    return handleDotAccess(placeholder, tokens);
  }

  let result;
  if (token === '(') {
    result = readList(tokens, state);
  } else if (token === '#(') {
    result = readVector(tokens, state);
  } else if (token === '#u8(') {
    result = readBytevector(tokens);
  } else if (token === '#{') {
    result = readJSObjectLiteral(tokens, state);
  } else if (token === ')') {
    throw new Error("Unexpected ')' - unbalanced parentheses");
  } else if (token === '}') {
    throw new Error("Unexpected '}' - unbalanced braces");
  } else if (token === "'") {
    // Quotes
    result = list(intern('quote'), readFromTokens(tokens, state));
  } else if (token === '`') {
    result = list(intern('quasiquote'), readFromTokens(tokens, state));
  } else if (token === ',') {
    result = list(intern('unquote'), readFromTokens(tokens, state));
  } else if (token === ',@') {
    result = list(intern('unquote-splicing'), readFromTokens(tokens, state));
  } else if (token.startsWith('|') && token.endsWith('|')) {
    // Vertical bar delimited symbol |...|
    const inner = token.slice(1, -1);
    const name = processSymbolEscapes(inner);
    result = intern(name);
  } else {
    result = readAtom(token, state.caseFold);
  }

  return handleDotAccess(result, tokens);
}

/**
 * Process escape sequences in vertical bar delimited symbols.
 * @param {string} str - Inner content of |...|
 * @returns {string} Processed string
 */
function processSymbolEscapes(str) {
  let result = '';
  let i = 0;
  while (i < str.length) {
    if (str[i] === '\\' && i + 1 < str.length) {
      const next = str[i + 1];
      if (next === '|') {
        result += '|';
        i += 2;
      } else if (next === '\\') {
        result += '\\';
        i += 2;
      } else if (next === 'x') {
        // Hex escape: \xNN; or \xNNNN;
        const semicolonIdx = str.indexOf(';', i + 2);
        if (semicolonIdx !== -1) {
          const hexStr = str.slice(i + 2, semicolonIdx);
          const codePoint = parseInt(hexStr, 16);
          if (!isNaN(codePoint)) {
            result += String.fromCodePoint(codePoint);
            i = semicolonIdx + 1;
            continue;
          }
        }
        // Invalid hex escape - keep as-is
        result += str[i];
        i++;
      } else {
        // Other escapes - keep the character after backslash
        result += next;
        i += 2;
      }
    } else {
      result += str[i];
      i++;
    }
  }
  return result;
}


function readList(tokens, state) {
  const listItems = [];
  while (true) {
    if (tokens.length === 0) {
      throw new Error("Missing ')'");
    }
    if (tokens[0].value === ')') break;

    // Handle datum comment inside list
    if (tokens[0].value === '#;') {
      tokens.shift(); // consume #;
      if (tokens.length === 0) throw new Error("Invalid datum comment: unexpected EOF");

      // Can't use datum comment on syntactic markers
      if (tokens[0].value === '.') {
        throw new Error("Invalid datum comment: cannot comment out '.' in dotted notation");
      }
      if (tokens[0].value === ')') {
        throw new Error("Invalid datum comment: no datum following #;");
      }
      readFromTokens(tokens, state); // discard next datum
      continue;
    }
    if (tokens[0].value === '.') {
      // R7RS: dotted list must have at least one element before the dot
      if (listItems.length === 0) {
        throw new Error("Illegal use of '.' - no elements before dot in dotted list");
      }
      tokens.shift(); // consume '.'
      // Handle datum comment after dot
      while (tokens.length > 0 && tokens[0].value === '#;') {
        tokens.shift();
        readFromTokens(tokens, state);
      }
      // Ensure there's actually a datum after the dot
      if (tokens.length === 0 || tokens[0].value === ')') {
        throw new Error("Illegal use of '.' - no datum after dot in dotted list");
      }
      const tail = readFromTokens(tokens, state);
      // Skip any datum comments before closing paren
      while (tokens.length > 0 && tokens[0].value === '#;') {
        tokens.shift();
        readFromTokens(tokens, state);
      }
      if (tokens.length === 0 || tokens.shift().value !== ')') {
        throw new Error("Expected ')' after improper list tail");
      }
      // Build improper list
      let result = tail;
      for (let i = listItems.length - 1; i >= 0; i--) {
        result = cons(listItems[i], result);
      }
      return result;
    }
    listItems.push(readFromTokens(tokens, state));
  }
  tokens.shift(); // consume ')'

  // Build proper list
  return list(...listItems);
}

function readVector(tokens, state) {
  const elements = [];
  while (true) {
    if (tokens.length === 0) {
      throw new Error("Missing ')' for vector");
    }
    if (tokens[0].value === ')') break;
    elements.push(readFromTokens(tokens, state));
  }
  tokens.shift(); // consume ')'
  return elements; // Return raw JS array
}

/**
 * Reads a JS object literal #{(key val) ...}
 * Each entry is a 2-element list where:
 * - First element: key (symbol -> quoted, string -> verbatim, expr -> evaluated)
 * - Second element: value (evaluated normally)
 * 
 * Special: (... obj) spreads obj's properties into the result
 * 
 * @param {Object[]} tokens - Token array
 * @param {Object} state - Reader state
 * @returns {Cons} S-expression representing the js-obj call
 */
function readJSObjectLiteral(tokens, state) {
  const entries = [];

  while (tokens.length > 0 && tokens[0].value !== '}') {
    // Each entry must be a list (key val) or (... obj)
    if (tokens[0].value !== '(') {
      throw new Error(`Expected '(' for property entry in #{...}, got '${tokens[0].value}'`);
    }

    // Read the entry as a list
    tokens.shift(); // consume '('
    const entryItems = [];
    while (tokens.length > 0 && tokens[0].value !== ')') {
      entryItems.push(readFromTokens(tokens, state));
    }
    if (tokens.length === 0) {
      throw new Error("Missing ')' in #{...} property entry");
    }
    tokens.shift(); // consume ')'

    // Check for spread syntax: (... obj)
    if (entryItems.length >= 1 &&
      entryItems[0] instanceof Symbol &&
      entryItems[0].name === '...') {
      if (entryItems.length !== 2) {
        throw new Error("Spread syntax (... obj) requires exactly one object");
      }
      // Mark as spread entry
      entries.push({ spread: true, value: entryItems[1] });
    } else if (entryItems.length === 2) {
      // Normal (key val) entry
      entries.push({ spread: false, key: entryItems[0], value: entryItems[1] });
    } else {
      throw new Error(`Property entry must be (key value) or (... obj), got ${entryItems.length} elements`);
    }
  }

  if (tokens.length === 0) {
    throw new Error("Missing '}' for #{...}");
  }
  tokens.shift(); // consume '}'

  // Build the (js-obj ...) or (js-obj-merge ...) expression
  // If there are spreads, we need to use js-obj-merge
  const hasSpread = entries.some(e => e.spread);

  if (hasSpread) {
    // Use special merge form: (js-obj-merge (spread obj1) (pairs k1 v1 k2 v2) (spread obj2) ...)
    const parts = [];
    let currentPairs = [];

    for (const entry of entries) {
      if (entry.spread) {
        // Flush any pending pairs
        if (currentPairs.length > 0) {
          parts.push(list(intern('js-obj'), ...currentPairs));
          currentPairs = [];
        }
        // Add spread object directly
        parts.push(entry.value);
      } else {
        // Accumulate key-value pair
        // Quote the key if it's a symbol
        const key = (entry.key instanceof Symbol)
          ? list(intern('quote'), entry.key)
          : entry.key;
        currentPairs.push(key, entry.value);
      }
    }

    // Flush remaining pairs
    if (currentPairs.length > 0) {
      parts.push(list(intern('js-obj'), ...currentPairs));
    }

    // Return (js-obj-merge part1 part2 ...)
    return list(intern('js-obj-merge'), ...parts);
  } else {
    // Simple case: (js-obj k1 v1 k2 v2 ...)
    const args = [];
    for (const entry of entries) {
      // Quote the key if it's a symbol
      const key = (entry.key instanceof Symbol)
        ? list(intern('quote'), entry.key)
        : entry.key;
      args.push(key, entry.value);
    }
    return list(intern('js-obj'), ...args);
  }
}


/**
 * Reads a bytevector literal #u8(...)
 * @param {Object[]} tokens - Token array
 * @returns {Uint8Array}
 */
function readBytevector(tokens) {
  const bytes = [];
  while (true) {
    if (tokens.length === 0) {
      throw new Error("Missing ')' for bytevector");
    }
    if (tokens[0].value === ')') break;

    const tokenObj = tokens.shift();
    const num = parseInt(tokenObj.value, 10);
    if (isNaN(num) || num < 0 || num > 255) {
      throw new Error(`Invalid byte value in bytevector: ${tokenObj.value}`);
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

  // Special numbers (case-insensitive)
  const lowerToken = token.toLowerCase();
  if (lowerToken === '+nan.0' || lowerToken === '-nan.0') return NaN;
  if (lowerToken === '+inf.0') return Infinity;
  if (lowerToken === '-inf.0') return -Infinity;

  // Booleans (R7RS: #t, #f, #true, #false)
  if (token === '#t' || token === '#true') return true;
  if (token === '#f' || token === '#false') return false;

  // Character literals (#\a, #\newline, #\x41, etc.)
  if (token.startsWith('#\\')) {
    return readCharacter(token.slice(2));
  }

  // Strings (not case-folded) - handle R7RS escape sequences
  if (token.startsWith('"')) {
    if (token.length < 2 || !token.endsWith('"')) {
      throw new Error("Unterminated string");
    }
    return processStringEscapes(token.slice(1, -1));
  }

  // Symbols - apply case folding if enabled
  const symbolName = caseFold ? token.toLowerCase() : token;

  // R7RS: The identifier consisting of a single dot is used only in pairs and is not an identifier.
  // readList handles '.' as a delimiter/improper list marker. 
  // If we reach here, '.' appeared where a datum was expected.
  if (symbolName === '.') {
    throw new Error("Unexpected '.'");
  }

  // JS Property Access: obj.prop1.prop2 -> (js-ref (js-ref obj "prop1") "prop2")
  // Only transform if there's at least one dot with non-empty parts on both sides
  if (symbolName.includes('.') && !symbolName.startsWith('.') && !symbolName.endsWith('.')) {
    const parts = symbolName.split('.');
    // Ensure all parts are non-empty (no consecutive dots)
    if (parts.every(part => part.length > 0)) {
      return buildPropertyAccessForm(parts);
    }
  }

  return intern(symbolName);
}

/**
 * Builds a nested property access form from a chain like ["obj", "prop1", "prop2"].
 * Returns: (js-ref (js-ref obj "prop1") "prop2")
 * @param {string[]} parts - Array of property path segments
 * @returns {Cons|Symbol} The nested js-ref form
 */
function buildPropertyAccessForm(parts) {
  // Start with the base object (first part as a symbol)
  let result = intern(parts[0]);

  // Chain property accesses for remaining parts
  for (let i = 1; i < parts.length; i++) {
    result = list(intern('js-ref'), result, parts[i]);
  }

  return result;
}

/**
 * Process R7RS string escape sequences.
 * \a - alarm (bell)
 * \b - backspace
 * \t - tab
 * \n - newline
 * \r - return
 * \" - double quote
 * \\ - backslash
 * \| - vertical bar
 * \xN...N; - hex escape
 * \<newline><intraline-whitespace> - line continuation
 * @param {string} str - String content without quotes
 * @returns {string} Processed string
 */
function processStringEscapes(str) {
  let result = '';
  let i = 0;
  while (i < str.length) {
    if (str[i] === '\\' && i + 1 < str.length) {
      const next = str[i + 1];
      switch (next) {
        case 'a':
          result += '\x07'; // alarm (bell)
          i += 2;
          break;
        case 'b':
          result += '\x08'; // backspace
          i += 2;
          break;
        case 't':
          result += '\t';
          i += 2;
          break;
        case 'n':
          result += '\n';
          i += 2;
          break;
        case 'r':
          result += '\r';
          i += 2;
          break;
        case '"':
          result += '"';
          i += 2;
          break;
        case '\\':
          result += '\\';
          i += 2;
          break;
        case '|':
          result += '|';
          i += 2;
          break;
        case 'x':
          // Hex escape: \xN...N;
          const semicolonIdx = str.indexOf(';', i + 2);
          if (semicolonIdx !== -1) {
            const hexStr = str.slice(i + 2, semicolonIdx);
            const codePoint = parseInt(hexStr, 16);
            if (!isNaN(codePoint)) {
              result += String.fromCodePoint(codePoint);
              i = semicolonIdx + 1;
              break;
            }
          }
          // Invalid hex escape - keep as-is
          result += str[i];
          i++;
          break;
        case '\n':
        case '\r':
        case ' ':
        case '\t':
          // Line continuation: skip backslash, skip whitespace including newline
          i += 1; // skip backslash
          // Skip leading whitespace before newline
          while (i < str.length && (str[i] === ' ' || str[i] === '\t')) {
            i++;
          }
          // Skip newline (could be \n, \r, or \r\n)
          if (i < str.length && str[i] === '\r') {
            i++;
          }
          if (i < str.length && str[i] === '\n') {
            i++;
          }
          // Skip trailing whitespace after newline
          while (i < str.length && (str[i] === ' ' || str[i] === '\t')) {
            i++;
          }
          break;
        default:
          // Unknown escape - keep the character
          result += next;
          i += 2;
      }
    } else {
      result += str[i];
      i++;
    }
  }
  return result;
}


/**
 * Parses a numeric literal (integers, rationals, complex, with optional prefixes)
 * R7RS supports prefixes: #b (binary), #o (octal), #d (decimal), #x (hex), #e (exact), #i (inexact)
 * @param {string} token 
 * @returns {number|Rational|Complex|null}
 */
export function parseNumber(token, exactness) {
  // Normalize R7RS exponent markers (s, f, d, l) to 'e' globally before parsing
  // This handles 1s2 -> 1e2, 1s2+3d4i -> 1e2+3e4i, etc.
  // Use lookahead to ensure we only replace exponent markers followed by a sign or digit,
  // preventing "inf.0" from becoming "ine.0"
  if (/[sSfFdDlL]/.test(token) && !token.startsWith('#')) {
    token = token.replace(/[sSfFdDlL](?=[+-]?\d)/g, 'e');
  }

  // Handle prefixed numbers (#x, #o, #b, #d, #e, #i)
  if (token.startsWith('#')) {
    return parsePrefixedNumber(token);
  }

  // Helper to force number (float) from potential Rational for Complex arithmetic
  // (We don't support Exact Complex yet)


  // Helper to parse a real component string into a number or Rational
  const parseRealStr = (str) => {
    if (!str) return 0;
    const lower = str.toLowerCase();
    if (lower.endsWith('inf.0')) {
      const val = lower.startsWith('-') ? -Infinity : Infinity;
      return val;
    }
    if (lower.endsWith('nan.0')) {
      return NaN;
    }
    // Handle rational components (e.g. 1/2) - return Rational if exact components
    if (str.includes('/')) {
      const parts = str.split('/');
      // If either part is decimal/scientific, it's inexact float division
      if (parts[0].includes('.') || parts[0].toLowerCase().includes('e') ||
        parts[1].includes('.') || parts[1].toLowerCase().includes('e')) {
        return parseFloat(parts[0]) / parseFloat(parts[1]);
      }
      return new Rational(parseInt(parts[0], 10), parseInt(parts[1], 10));
    }
    return parseFloat(str);
  };

  // Handle immediate special values
  if (/^[+-]?inf\.0$/i.test(token)) return parseRealStr(token);
  if (/^[+-]?nan\.0$/i.test(token)) return NaN;

  // Pattern for unsigned real numbers: rationals, integers, decimals, scientific notation, inf.0, nan.0
  const UNSIGNED_REAL = '(?:\\d+/\\d+|(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?|inf\\.0|nan\\.0)';

  // Pattern for signed real numbers (capturing group 1)
  const REAL_PATTERN = `([+-]?${UNSIGNED_REAL})`;

  // Complex: real+imag (e.g., 1+2i, 1e2+3e4i, +inf.0+inf.0i)
  // The regex captures: 1:real, 2:sign, 3:imag(unsigned)
  const complexRegex = new RegExp(`^${REAL_PATTERN}([+-])(${UNSIGNED_REAL})?i$`, 'i');
  // console.log('Checking token:', token);
  // console.log('Complex regex:', complexRegex);
  const complexMatch = token.match(complexRegex);
  // console.log('Match result:', complexMatch);

  if (complexMatch) {
    const real = parseRealStr(complexMatch[1]);
    const sign = complexMatch[2] === '-' ? -1 : 1;
    const imagStr = complexMatch[3];

    let imagVal = imagStr ? parseRealStr(imagStr) : 1;
    // Apply sign
    if (sign === -1) {
      if (imagVal instanceof Rational) imagVal = imagVal.negate();
      else imagVal = -imagVal;
    }

    return new Complex(real, imagVal);
  }

  // Pure imaginary: +i, -i, 3i, +inf.0i
  // Captures: 1:real(signed) or sign
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
    const rat = new Rational(num, den);
    // console.log(`DEBUG: parsePrefixedNumber ${token} -> Rational(${rat.numerator}, ${rat.denominator})`);
    return rat;
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

  // Handle special values: +inf.0, -inf.0, +nan.0, -nan.0 (case-insensitive)
  const lowerRest = rest.toLowerCase();
  if (/^[+-]?inf\.0$/.test(lowerRest)) {
    return lowerRest.startsWith('-') ? -Infinity : Infinity;
  }
  if (/^[+-]?nan\.0$/.test(lowerRest)) {
    return NaN;
  }

  if (radix === 10 && (rest.includes('.') || rest.toLowerCase().includes('e') || rest.toLowerCase().includes('s') || rest.toLowerCase().includes('f') || rest.toLowerCase().includes('d') || rest.toLowerCase().includes('l'))) {
    // Decimal with fractional part or exponent
    // Validate strict format: optional sign, digits, optional dot, optional digits, optional exponent
    if (!/^[+-]?(\d+(\.\d*)?|\.\d+)([eEsSfFdDlL][+-]?\d+)?$/.test(rest)) {
      return null;
    }
    // Normalize exponent before parsing (already done in parseNumber but rest might be fresh substring)
    // Actually parseNumber does global replace on TOKEN. 
    // parsePrefixedNumber receives TOKEN. 
    // rest is substring. The replace happened on token.
    // So rest should have 'e'.
    // But safety:
    const normalized = rest.replace(/[sSfFdDlL](?=[+-]?\d)/g, 'e');
    result = parseFloat(normalized);
  } else {
    // Integer in given radix
    // Validate chars strictly
    const validChars = '0123456789abcdefghijklmnopqrstuvwxyz'.slice(0, radix);
    const checkRest = rest.replace(/^[+-]/, '').toLowerCase();
    for (const char of checkRest) {
      if (!validChars.includes(char)) return null;
    }
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
