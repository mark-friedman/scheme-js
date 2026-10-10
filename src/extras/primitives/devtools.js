/**
 * @fileoverview The JavaScript of (scheme-js devtools): what DevTools' custom
 * formatters need of the host, the rest being Scheme (devtools.scm).
 *
 * DevTools draws an object with the formatters a page lists in
 * `globalThis.devtoolsFormatters`, once its user turns custom formatters on:
 * each is asked for a header, whether there is a body, and the body, as
 * JsonML. These primitives register one whose answers are Scheme's, read
 * where the program is paused -- DevTools calls a formatter on the paused
 * thread, so the frame beneath it is the paused one -- and turn the markup the
 * Scheme answers with into JsonML; and tell the Scheme what only the host
 * can: whether a function is a Scheme procedure, and a record's type and
 * fields.
 */

import { SCHEME_CLOSURE, SCHEME_PRIMITIVE, callSchemeProcedure } from '../../core/interpreter/values.js';
import { Cons, list, toArray } from '../../core/interpreter/cons.js';
import { intern } from '../../core/interpreter/symbol.js';
import { SchemeString, stringValue } from '../../core/primitives/string_class.js';
import { RECORD_FIELDS } from '../../core/primitives/record.js';

/**
 * The URL of the script of the frame beneath the function that took a stack
 * trace: the second frame of it. Empty if there is none, as when DevTools
 * draws a value with nothing paused.
 * @param {string} stack - An `Error`'s stack.
 * @returns {string}
 */
function frameBeneath(stack) {
  const line = (stack ?? '').split('\n')[2] ?? '';
  const place = /\((.*):\d+:\d+\)$/.exec(line) ?? /at (.*):\d+:\d+$/.exec(line);
  return place === null ? '' : place[1];
}

/**
 * JsonML of the markup devtools.scm answers with: a string; a list whose head
 * is a tag -- `span`, `div`, `ol`, `li` -- whose second element is a style or
 * #f, and the rest its children; or `(object value javascript?)`, a value for
 * DevTools to draw itself, as JavaScript draws it if `javascript?`.
 * @param {*} markup - The markup.
 * @returns {*} JsonML.
 */
function jsonml(markup) {
  if (typeof markup === 'string' || markup instanceof SchemeString) return stringValue(markup);
  const items = toArray(markup);
  const tag = items[0].name;
  if (tag === 'object') {
    return ['object', items[2] === true ? { object: items[1], config: { javascript: true } } : { object: items[1] }];
  }
  return [tag, items[1] === false ? {} : { style: stringValue(items[1]) }, ...items.slice(2).map(jsonml)];
}

/**
 * The formatter this module registers, so that a second registration
 * replaces it rather than adding another.
 * @type {symbol}
 */
const OURS = Symbol('scheme-js devtools formatter');

/**
 * Primitives for (scheme-js devtools).
 */
export const devtoolsPrimitives = {
  /**
   * Registers a custom formatter for DevTools whose answers are Scheme
   * procedures': each takes the value and the URL of the script the program
   * is paused in, or "", and the header and body answer markup or #f. A value
   * DevTools is asked to draw as JavaScript, from a body's row, is left to it.
   * A formatter that threw would stop DevTools drawing anything, so a failure
   * leaves the value to DevTools too.
   * @param {Function} header - The header's procedure.
   * @param {Function} hasBody - Whether there is a body.
   * @param {Function} body - The body's procedure.
   * @returns {boolean} True.
   */
  '%install-devtools-formatter!': (header, hasBody, body) => {
    const ask = (procedure, object, config, stack) => {
      if (config?.javascript === true) return false;
      try {
        return callSchemeProcedure(procedure, [object, frameBeneath(stack)]);
      } catch (e) {
        return false;
      }
    };
    const formatter = {
      [OURS]: true,
      header(object, config) {
        const markup = ask(header, object, config, new Error().stack);
        return markup === false ? null : jsonml(markup);
      },
      hasBody(object, config) {
        return ask(hasBody, object, config, new Error().stack) === true;
      },
      body(object, config) {
        const markup = ask(body, object, config, new Error().stack);
        return markup === false ? null : jsonml(markup);
      }
    };
    const formatters = (globalThis.devtoolsFormatters ?? []).filter((f) => f?.[OURS] !== true);
    formatters.push(formatter);
    globalThis.devtoolsFormatters = formatters;
    return true;
  },

  /**
   * Whether a value is a Scheme procedure: a closure, a compiled procedure,
   * or a primitive -- not a function of JavaScript's own.
   * @param {*} value - The value.
   * @returns {boolean}
   */
  '%scheme-procedure?': (value) => typeof value === 'function'
    && (value[SCHEME_CLOSURE] !== undefined || value.$compiled !== undefined || value[SCHEME_PRIMITIVE] === true),

  /**
   * A record's type's name and its fields, `(name (field . value) ...)`, or
   * #f if the value is no record.
   * @param {*} value - The value.
   * @returns {Cons|boolean}
   */
  '%record-description': (value) => {
    const type = value !== null && typeof value === 'object' ? value.constructor : undefined;
    const fields = type?.[RECORD_FIELDS];
    if (fields === undefined) return false;
    return new Cons(type.schemeName, list(...fields.map((field) => new Cons(intern(field), value[field]))));
  }
};
