/**
 * @fileoverview Environment proxy for Chrome DevTools Scope pane integration.
 *
 * Creates a Proxy that exposes Scheme environment bindings as JS properties.
 * Used inside `with()` blocks in probe functions so that DevTools' Scope pane
 * can display Scheme variables when paused at a breakpoint.
 *
 * Phase 2: Core proxy structure, scope chain, name mapping.
 * Phase 3: Value formatting (Scheme print format), name filtering,
 *          SchemeDisplayValue wrapper for custom formatters.
 */

import { Environment } from '../../core/interpreter/environment.js';
import { Cons } from '../../core/interpreter/cons.js';
import { Symbol as SchemeSymbol } from '../../core/interpreter/symbol.js';
import { Char } from '../../core/primitives/char_class.js';
import { Rational } from '../../core/primitives/rational.js';
import { Complex } from '../../core/primitives/complex.js';
import { isSchemeClosure, isSchemeContinuation, Values } from '../../core/interpreter/values.js';
import { prettyPrint } from '../../core/interpreter/printer.js';

// =============================================================================
// SchemeDisplayValue — Wrapper for Custom Formatters
// =============================================================================

/**
 * Wraps a Scheme value's display string for use in Chrome DevTools.
 *
 * When custom formatters are enabled in DevTools, the registered formatter
 * detects this wrapper and renders the display string as unquoted styled text.
 * Without custom formatters, DevTools shows the toString() result (in quotes
 * for non-primitive types in the Scope pane).
 *
 * @example
 *   const val = new SchemeDisplayValue("'(1 2 3)", 'list');
 *   val.toString(); // => "'(1 2 3)"
 */
export class SchemeDisplayValue {
  /**
   * @param {string} displayString - The Scheme print representation
   * @param {string} schemeType - The Scheme type name (for formatter styling)
   */
  constructor(displayString, schemeType) {
    /** @type {string} */
    this.displayString = displayString;
    /** @type {string} */
    this.schemeType = schemeType;
    /** @type {boolean} Marker for custom formatter detection */
    this.__schemeDisplayValue = true;
  }

  /**
   * Returns the Scheme print representation.
   * @returns {string}
   */
  toString() {
    return this.displayString;
  }

  /**
   * Allows coercion to string in template literals and concatenation.
   * @param {string} hint
   * @returns {string|number}
   */
  [globalThis.Symbol.toPrimitive](hint) {
    if (hint === 'number') return NaN;
    return this.displayString;
  }
}

// =============================================================================
// Named Character Map (for Char formatting)
// =============================================================================

/** @type {Map<number, string>} Maps code points to named character representations */
const NAMED_CHARS = new Map([
  [0, 'null'],
  [7, 'alarm'],
  [8, 'backspace'],
  [9, 'tab'],
  [10, 'newline'],
  [13, 'return'],
  [27, 'escape'],
  [32, 'space'],
  [127, 'delete'],
]);

// =============================================================================
// Value Formatting
// =============================================================================

/**
 * Formats a Scheme character for DevTools display using Scheme notation.
 *
 * @param {Char} ch - The Scheme character
 * @returns {string} Scheme char literal like `#\a` or `#\space`
 */
function formatChar(ch) {
  const named = NAMED_CHARS.get(ch.codePoint);
  if (named) return `#\\${named}`;
  return `#\\${String.fromCodePoint(ch.codePoint)}`;
}

/**
 * Formats a Scheme record for DevTools display, including type name and fields.
 *
 * @param {Object} record - A Scheme record instance
 * @returns {string} Display like `#<point x: 1 y: 2>`
 */
function formatRecord(record) {
  const typeName = record.constructor?.schemeName || record.constructor?.name || 'record';
  // Get own properties (the record fields)
  const fields = Object.getOwnPropertyNames(record);
  if (fields.length === 0) {
    return `#<${typeName}>`;
  }
  const fieldParts = fields.map(f => `${f}: ${prettyPrint(record[f])}`);
  return `#<${typeName} ${fieldParts.join(' ')}>`;
}

/**
 * Formats a Scheme value for display in Chrome DevTools.
 *
 * Uses a hybrid approach:
 * - JS-native types (number, string, boolean) pass through directly so
 *   DevTools displays them without quotes.
 * - Scheme-specific types (lists, symbols, chars, closures, etc.) are
 *   wrapped in SchemeDisplayValue with their Scheme print representation.
 *
 * When custom formatters are enabled, SchemeDisplayValue objects render
 * as unquoted styled text. Without custom formatters, they show as
 * quoted strings in the Scope pane.
 *
 * @param {*} value - The Scheme value to format
 * @returns {*} A JS value or SchemeDisplayValue for DevTools display
 */
export function formatForDevTools(value) {
  // --- JS primitive pass-through ---

  // JS numbers pass through directly (displayed without quotes)
  if (typeof value === 'number') {
    return value;
  }

  // JS strings pass through directly (displayed as quoted strings)
  if (typeof value === 'string') {
    return value;
  }

  // JS booleans pass through directly
  if (typeof value === 'boolean') {
    return value;
  }

  // BigInt: convert to JS Number if safe, otherwise wrap
  if (typeof value === 'bigint') {
    if (value >= Number.MIN_SAFE_INTEGER && value <= Number.MAX_SAFE_INTEGER) {
      return Number(value);
    }
    return new SchemeDisplayValue(value.toString(), 'integer');
  }

  // undefined passes through
  if (value === undefined) {
    return undefined;
  }

  // --- Scheme-specific types → SchemeDisplayValue ---

  // null = empty list '()
  if (value === null) {
    return new SchemeDisplayValue("'()", 'null');
  }

  // Scheme Symbol
  if (value instanceof SchemeSymbol) {
    return new SchemeDisplayValue(value.name, 'symbol');
  }

  // Scheme Char
  if (value instanceof Char) {
    return new SchemeDisplayValue(formatChar(value), 'char');
  }

  // Rational number
  if (value instanceof Rational) {
    return new SchemeDisplayValue(value.toString(), 'rational');
  }

  // Complex number
  if (value instanceof Complex) {
    return new SchemeDisplayValue(value.toString(), 'complex');
  }

  // Scheme pair/list (Cons)
  if (value instanceof Cons) {
    return new SchemeDisplayValue(prettyPrint(value), 'pair');
  }

  // Scheme closure
  if (typeof value === 'function' && isSchemeClosure(value)) {
    const name = value.__name || value.name || 'anonymous';
    return new SchemeDisplayValue(`#<procedure ${name}>`, 'closure');
  }

  // Scheme continuation
  if (typeof value === 'function' && isSchemeContinuation(value)) {
    return new SchemeDisplayValue('#<continuation>', 'continuation');
  }

  // Regular JS function (primitive)
  if (typeof value === 'function') {
    return new SchemeDisplayValue('#<procedure>', 'procedure');
  }

  // Scheme vector (JS Array)
  if (Array.isArray(value)) {
    return new SchemeDisplayValue(prettyPrint(value), 'vector');
  }

  // Multiple values
  if (value instanceof Values) {
    return new SchemeDisplayValue(prettyPrint(value.first()), 'values');
  }

  // Scheme record (instance of a dynamically-created record type class)
  if (value && typeof value === 'object' && value.constructor?.schemeName) {
    return new SchemeDisplayValue(formatRecord(value), 'record');
  }

  // Fallback: use prettyPrint for anything else
  return new SchemeDisplayValue(prettyPrint(value), 'object');
}

// =============================================================================
// Reverse Name Map
// =============================================================================

/**
 * Builds a reverse name map from the environment's nameMap chain.
 * Maps original (user-facing) names → internal (alpha-renamed) names.
 *
 * The environment's nameMap stores originalName → renamedName mappings.
 * We walk up the chain and collect them all, with inner scopes taking
 * precedence over outer scopes.
 *
 * @param {Environment} env - The Scheme environment
 * @returns {Map<string, string>} Map from original names to internal names
 */
function buildReverseNameMap(env) {
  const map = new Map();
  let current = env;
  while (current) {
    if (current.nameMap) {
      for (const [original, renamed] of current.nameMap) {
        // Inner scopes take precedence — don't overwrite
        if (!map.has(original)) {
          map.set(original, renamed);
        }
      }
    }
    current = current.parent;
  }
  return map;
}

/**
 * Builds a set of all internal (alpha-renamed) names that have
 * corresponding original names. These should be hidden from the
 * Scope pane since the original names are shown instead.
 *
 * @param {Map<string, string>} reverseNameMap - Original → internal name map
 * @returns {Set<string>} Set of internal names to exclude
 */
function buildInternalNameSet(reverseNameMap) {
  return new Set(reverseNameMap.values());
}

/**
 * Collects all visible binding names from the environment chain,
 * using original names where available (via nameMap) and filtering
 * out internal alpha-renamed names.
 *
 * @param {Environment} env - The Scheme environment
 * @param {Map<string, string>} reverseNameMap - Original→internal name map
 * @returns {string[]} Array of visible binding names
 */
function collectVisibleBindingNames(env, reverseNameMap) {
  const seen = new Set();
  const names = [];
  const internalNames = buildInternalNameSet(reverseNameMap);

  // Add original names from the reverse map
  for (const original of reverseNameMap.keys()) {
    if (!seen.has(original)) {
      seen.add(original);
      names.push(original);
    }
  }

  // Walk the environment chain for direct bindings,
  // skipping internal alpha-renamed names
  let current = env;
  while (current) {
    for (const name of current.bindings.keys()) {
      if (!seen.has(name) && !internalNames.has(name)) {
        seen.add(name);
        names.push(name);
      }
    }
    current = current.parent;
  }

  return names;
}

/**
 * Looks up the internal (alpha-renamed) name for an original name.
 * Walks up the environment chain checking each nameMap.
 *
 * @param {Environment} env - The Scheme environment
 * @param {string} originalName - The original (user-facing) name
 * @returns {string|null} The internal name, or null if not found
 */
function getInternalName(env, originalName) {
  let current = env;
  while (current) {
    if (current.nameMap && current.nameMap.has(originalName)) {
      return current.nameMap.get(originalName);
    }
    current = current.parent;
  }
  return null;
}

// =============================================================================
// Environment Proxy
// =============================================================================

/**
 * Creates a Proxy that exposes Scheme environment bindings as JS properties.
 * Used inside `with()` blocks in probe functions for DevTools Scope inspection.
 *
 * When DevTools pauses inside a probe function like:
 *   function __scheme_E1(envProxy) { with (envProxy) { __schemeProbeRuntime.hit(1); } }
 *
 * The `with` statement makes the proxy's properties visible in DevTools'
 * Scope pane as "With Block" scope entries.
 *
 * Values are formatted using `formatForDevTools()`:
 * - JS primitives (numbers, strings, booleans) pass through directly.
 * - Scheme-specific types are wrapped in SchemeDisplayValue with their
 *   Scheme print representation.
 *
 * @param {Environment} env - The Scheme environment to expose
 * @returns {Proxy} A proxy suitable for `with(proxy) { ... }`
 */
export function createEnvProxy(env) {
  const reverseNameMap = buildReverseNameMap(env);

  return new Proxy(Object.create(null), {
    has(target, prop) {
      if (prop === Symbol.unscopables) return false;
      if (typeof prop !== 'string') return false;
      // Check if this binding exists via original name or direct name
      return reverseNameMap.has(prop) || env.findEnv(prop) !== null;
    },

    get(target, prop) {
      if (prop === Symbol.unscopables) return undefined;
      if (typeof prop !== 'string') return undefined;

      // Try original name first (reverse lookup from nameMap)
      const internalName = getInternalName(env, prop);
      if (internalName) {
        try {
          return formatForDevTools(env.lookup(internalName));
        } catch {
          return undefined;
        }
      }

      // Try direct lookup
      try {
        return formatForDevTools(env.lookup(prop));
      } catch {
        return undefined;
      }
    },

    ownKeys(target) {
      return collectVisibleBindingNames(env, reverseNameMap);
    },

    getOwnPropertyDescriptor(target, prop) {
      if (typeof prop !== 'string') return undefined;
      if (this.has(target, prop)) {
        return { enumerable: true, configurable: true, writable: true };
      }
      return undefined;
    }
  });
}
