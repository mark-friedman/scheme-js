/**
 * @fileoverview Reader module - S-expression parser.
 * 
 * This file is a backward-compatible re-export from the modular reader/ directory.
 * For new code, prefer importing directly from './reader/index.js'.
 */

// Re-export everything from the modular reader
export {
  parse,
  tokenize,
  stripBlockComments,
  readFromTokens,
  readList,
  readVector,
  readAtom,
  readBytevector,
  readJSObjectLiteral,
  parseNumber,
  parsePrefixedNumber,
  handleDotAccess,
  buildPropertyAccessForm,
  processStringEscapes,
  processSymbolEscapes,
  readCharacter,
  NAMED_CHARACTERS,
  Placeholder,
  fixup
} from './reader/index.js';
