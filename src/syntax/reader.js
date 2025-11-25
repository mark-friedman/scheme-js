import { Literal, Variable } from './ast.js';

/**
 * Simple S-expression parser.
 * This version's `readAtom` is "smarter" and returns
 * AST nodes (Literal, Variable) directly.
 */
class Reader {
  /**
   * Tokenizes the input string.
   * @param {string} code
   * @returns {Array<string>} List of tokens.
   */
  tokenize(code) {
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
   * @returns {Literal | Variable}
   */
  readAtom(token) {
    if (token.startsWith('"')) {
      // It's a string
      const strVal = token.substring(1, token.length - 1)
        .replace(/\\"/g, '"')
        .replace(/\\n/g, '\n')
        .replace(/\\t/g, '\t')
        .replace(/\\\\/g, '\\');
      return new Literal(strVal);
    }
    // Try to parse as a number
    const num = parseFloat(token);
    if (!isNaN(num) && num.toString() === token) {
      return new Literal(num);
    }
    // Check for booleans or null
    if (token === '#t') { return new Literal(true); }
    if (token === '#f') { return new Literal(false); }
    if (token === 'null') { return new Literal(null); }

    // Otherwise, it's a symbol (Variable)
    return new Variable(token);
  }

  /**
   * Recursively reads S-expressions from a list of tokens.
   * @param {Array<string>} tokens - Mutable list of tokens.
   * @returns {*} A single S-expression (Array or Executable).
   */
  readFromTokens(tokens) {
    if (tokens.length === 0) {
      throw new SyntaxError("Unexpected EOF");
    }
    const token = tokens.shift();

    if (token === '(') {
      const list = [];
      while (tokens[0] !== ')') {
        if (tokens.length === 0) {
          throw new SyntaxError("Missing ')'");
        }
        list.push(this.readFromTokens(tokens));
      }
      tokens.shift(); // Pop the ')'
      return list;
    } else if (token === ')') {
      throw new SyntaxError("Unexpected ')'");
    } else if (token === '`') {
      return [new Variable('quasiquote'), this.readFromTokens(tokens)];
    } else if (token === ',') {
      return [new Variable('unquote'), this.readFromTokens(tokens)];
    } else if (token === ',@') {
      return [new Variable('unquote-splicing'), this.readFromTokens(tokens)];
    } else if (token === "'") {
      return [new Variable('quote'), this.readFromTokens(tokens)];
    } else {
      return this.readAtom(token);
    }
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
