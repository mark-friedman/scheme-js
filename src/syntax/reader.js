import { Cons, cons, list } from '../data/cons.js';
import { intern } from '../data/symbol.js';

/**
 * S-expression parser.
 * Reads text and produces Scheme data structures:
 * - Lists -> Cons chains (or null)
 * - Symbols -> Symbol objects
 * - Numbers -> JS numbers
 * - Strings -> JS strings
 * - Booleans -> JS booleans
 */
class Reader {
  /**
   * Tokenizes the input string.
   * @param {string} code
   * @returns {Array<string>} List of tokens.
   */
  tokenize(code) {
    // Added '.' to the regex to ensure it's captured if it's a standalone token
    // Actually, [^()\s]+ captures '.' correctly.
    const regex = /\s*([()]|"(?:[\\].|[^"\\])*"|,@|,|`|'|[^()\s]+)\s*/g;
    const tokens = [];
    let match;
    while ((match = regex.exec(code)) !== null) {
      tokens.push(match[1]);
    }
    return tokens;
  }

  /**
   * Reads an atom (number, bool, string, symbol) from a token.
   * @param {string} token
   * @returns {*}
   */
  readAtom(token) {
    if (token.startsWith('"')) {
      // It's a string
      return token.substring(1, token.length - 1)
        .replace(/\\"/g, '"')
        .replace(/\\n/g, '\n')
        .replace(/\\t/g, '\t')
        .replace(/\\\\/g, '\\');
    }
    // Try to parse as a number
    const num = parseFloat(token);
    if (!isNaN(num) && num.toString() === token) {
      return num;
    }
    // Check for booleans
    if (token === '#t') { return true; }
    if (token === '#f') { return false; }

    // Note: 'null' is just a symbol in Scheme, evaluating to ().
    // We treat it as a symbol here.

    // Otherwise, it's a symbol
    return intern(token);
  }

  /**
   * Recursively reads S-expressions from a list of tokens.
   * @param {Array<string>} tokens - Mutable list of tokens.
   * @returns {*} A single S-expression.
   */
  readFromTokens(tokens) {
    if (tokens.length === 0) {
      throw new SyntaxError("Unexpected EOF");
    }
    const token = tokens.shift();

    if (token === '(') {
      return this.readList(tokens);
    } else if (token === ')') {
      throw new SyntaxError("Unexpected ')'");
    } else if (token === '`') {
      return list(intern('quasiquote'), this.readFromTokens(tokens));
    } else if (token === ',') {
      return list(intern('unquote'), this.readFromTokens(tokens));
    } else if (token === ',@') {
      return list(intern('unquote-splicing'), this.readFromTokens(tokens));
    } else if (token === "'") {
      return list(intern('quote'), this.readFromTokens(tokens));
    } else {
      return this.readAtom(token);
    }
  }

  /**
   * Reads a list (proper or improper) from tokens.
   * Assumes the opening '(' has already been consumed.
   * @param {Array<string>} tokens
   * @returns {*} Cons chain or null.
   */
  readList(tokens) {
    if (tokens.length === 0) {
      throw new SyntaxError("Missing ')'");
    }

    if (tokens[0] === ')') {
      tokens.shift(); // Consume ')'
      return null; // Empty list
    }

    // Read first element
    const first = this.readFromTokens(tokens);

    // Check for dot notation: (a . b)
    if (tokens[0] === '.') {
      tokens.shift(); // Consume '.'
      const second = this.readFromTokens(tokens);
      if (tokens[0] !== ')') {
        throw new SyntaxError("Expected ')' after dot notation");
      }
      tokens.shift(); // Consume ')'
      return cons(first, second);
    }

    // Recursive read for the rest of the list
    const rest = this.readList(tokens);
    return cons(first, rest);
  }

  /**
   * Parses a string of Scheme code into a list of S-expressions.
   * @param {string} code
   * @returns {Array<*>} A list of S-expressions.
   */
  parse(code) {
    const tokens = this.tokenize(code);
    const asts = [];
    while (tokens.length > 0) {
      asts.push(this.readFromTokens(tokens));
    }
    return asts;
  }
}

// Export a singleton instance
const reader = new Reader();
export const parse = (code) => reader.parse(code);
