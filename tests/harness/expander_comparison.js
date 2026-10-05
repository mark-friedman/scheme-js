/**
 * @fileoverview The two expanders compared: the JavaScript analyzer
 * (src/core/interpreter/analyzer.js) and the Scheme expander,
 * `(scheme-js expander)`, on every form a run analyzes at a top level.
 *
 * Installed in place of the expander in use (`useTopLevelAnalyzer` in
 * expand.js), it expands each form with the Scheme expander and then analyzes
 * it with the analyzer, compares the two -- the Scheme expander's core form
 * assembled into nodes, both written out with the names they made and the
 * scopes they marked numbered in the order they appear -- and returns the
 * analyzer's, so that the run goes as it would have. Each `syntax-rules` macro
 * a top-level form defines, both define, the analyzer last; the one left is
 * replaced by one that calls both transformers on every use, from either
 * expander, and compares what they make of it. A `define-macro`'s procedure,
 * which may do anything, is not called twice.
 *
 * JavaScript, since what it compares is the JavaScript analyzer with the
 * Scheme that replaces it.
 */

import { Cons, list } from '../../src/core/interpreter/cons.js';
import { Symbol, intern } from '../../src/core/interpreter/symbol.js';
import { SyntaxObject } from '../../src/core/interpreter/syntax_object.js';
import { globalContext } from '../../src/core/interpreter/context.js';
import { Environment } from '../../src/core/interpreter/environment.js';
import { analyzeInJavaScript } from '../../src/core/interpreter/analyzer.js';
import { expander, useTopLevelAnalyzer } from '../../src/core/interpreter/expand.js';
import { assemble } from '../../src/core/interpreter/assembler.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import {
  LiteralNode, VariableNode, ScopedVariable, LibraryVariableNode, LibrarySetNode, LambdaNode,
  LetRecNode, IfNode, SetNode, DefineNode, TailAppNode, BeginNode, ImportNode, DefineLibraryNode
} from '../../src/core/interpreter/ast_nodes.js';
import { Executable } from '../../src/core/interpreter/stepables_base.js';

/**
 * A node as a core form, as `(scheme-js expander)` writes them, so that the
 * two expanders' nodes can be written out alike.
 * @param {Executable} node - The node.
 * @returns {*}
 */
function coreOf(node) {
  const names = (strings) => list(...strings.map((s) => intern(s)));
  const optional = (s) => (s === null || s === undefined ? false : intern(s));
  let form;
  if (node instanceof LiteralNode) form = list(intern('lit'), node.value);
  else if (node instanceof VariableNode) form = list(intern('var'), intern(node.name));
  else if (node instanceof LibraryVariableNode) form = list(intern('library-var'), intern(node.name), node.env);
  else if (node instanceof ScopedVariable) form = list(intern('scoped-var'), intern(node.name), list(...node.scopes));
  else if (node instanceof IfNode) {
    form = list(intern('if'), coreOf(node.test), coreOf(node.consequent), coreOf(node.alternative));
  } else if (node instanceof BeginNode) form = list(intern('seq'), list(...node.expressions.map(coreOf)));
  else if (node instanceof LambdaNode) {
    form = list(intern('lambda'), names(node.params), optional(node.restParam), node.name, coreOf(node.body),
      names(node.originalParams), optional(node.originalRestParam));
  } else if (node instanceof LetRecNode) {
    form = list(intern('letrec'), names(node.names), list(...node.lambdaExprs.map(coreOf)), coreOf(node.body),
      names(node.originalNames));
  } else if (node instanceof SetNode) form = list(intern('set'), intern(node.name), coreOf(node.valueExpr));
  else if (node instanceof LibrarySetNode) {
    form = list(intern('library-set'), intern(node.name), node.env, coreOf(node.valueExpr));
  } else if (node instanceof DefineNode) form = list(intern('define'), intern(node.name), coreOf(node.valueExpr));
  else if (node instanceof TailAppNode) form = list(intern('app'), coreOf(node.funcExpr), list(...node.argExprs.map(coreOf)));
  else if (node instanceof ImportNode) form = list(intern('import'), list(...node.importSpecs));
  else if (node instanceof DefineLibraryNode) form = list(intern('define-library'), node.form);
  else form = list(intern('node'), node);
  if (node.source) form.source = node.source;
  return form;
}

/**
 * Writes a datum out with what two expansions of one form may differ in and
 * still mean the same numbered in the order it appears: the names locals are
 * renamed to, `x_$12`, and scopes.
 * @param {*} datum - The datum.
 * @returns {string}
 */
function written(datum) {
  const renamed = new Map();
  const scopes = new Map();
  const seen = new Set();
  // A local renamed is renamed again where a macro's template binds a name
  // it took from a local where the macro was defined: `x_$12_$40`.
  const nameOf = (name) => {
    const written = name.replace(/(_\$\d+)+$/, '');
    if (written === name) return name;
    if (!renamed.has(name)) renamed.set(name, renamed.size + 1);
    return `${written}_#${renamed.get(name)}`;
  };
  const scopeOf = (scope) => {
    if (!scopes.has(scope)) scopes.set(scope, scopes.size + 1);
    return scopes.get(scope);
  };
  const span = (x) => (x.source ? `@${x.source.filename ?? ''}:${x.source.line}:${x.source.column}` : '');
  const write = (x) => {
    if (x === null) return '()';
    if (x === undefined) return '#<undefined>';
    if (x instanceof Cons || Array.isArray(x)) {
      if (seen.has(x)) return '#<shared>';
      seen.add(x);
      if (Array.isArray(x)) return `#(${x.map(write).join(' ')})`;
      const parts = [write(x.car)];
      let rest = x.cdr;
      for (; rest instanceof Cons && !seen.has(rest); rest = rest.cdr) {
        seen.add(rest);
        parts.push(write(rest.car));
      }
      if (rest !== null) parts.push('.', write(rest));
      return `(${parts.join(' ')})${span(x)}`;
    }
    if (x instanceof Symbol) return nameOf(x.name);
    if (x instanceof SyntaxObject) {
      return `#<syntax ${nameOf(x.name)}{${[...x.scopes].map(scopeOf).sort((a, b) => a - b).join(',')}}>`;
    }
    if (x instanceof Environment) return `#<environment ${x.libraryName?.join(' ') ?? ''}>`;
    if (x instanceof Executable) return `#<node ${x.constructor.name}>`;
    if (typeof x === 'function') return '#<procedure>';
    try {
      return writeString(x);
    } catch (e) {
      return `#<${typeof x}>`;
    }
  };
  return write(datum);
}

/**
 * What an expansion came to: its nodes written out, or its error's message.
 * @param {function(): Executable} expansion - Makes the nodes.
 * @returns {{text: string, node: (Executable|undefined), error: (*|undefined)}}
 */
function outcome(expansion) {
  try {
    const node = expansion();
    return { text: written(coreOf(node)), node };
  } catch (error) {
    return { text: `error: ${error?.message ?? String(error)}`, error };
  }
}

/**
 * The comparison, installed: every form analyzed at a top level from now on is
 * compared, and what does not agree is recorded.
 * @param {{limit: (number|undefined)}} [options] - How many disagreements to
 *   keep in full; every one is counted.
 * @returns {{forms: number, macroUses: number, disagreements: Array<Object>, count: number,
 *   uninstall: function(): void}} The record, kept up to date.
 */
export function installExpanderComparison(options = {}) {
  const limit = options.limit ?? 200;
  const record = { forms: 0, macroUses: 0, disagreements: [], count: 0, uninstall: null };
  const disagree = (kind, form, scheme, javascript) => {
    record.count++;
    if (record.disagreements.length < limit) {
      record.disagreements.push({ kind, form: abbreviated(form), scheme, javascript });
    }
  };

  const compareTransformers = (name, schemeMade, javascriptMade) => {
    const comparing = (form, useEnv) => {
      record.macroUses++;
      const javascript = outcomeOfData(() => javascriptMade(form, useEnv));
      const scheme = outcomeOfData(() => schemeMade(form, useEnv));
      if (scheme.text !== javascript.text) disagree(`macro ${name}`, form, scheme.text, javascript.text);
      if (javascript.error !== undefined) throw javascript.error;
      return javascript.value;
    };
    return comparing;
  };

  const compare = (form, context) => {
    record.forms++;
    const macros = globalContext.macroRegistry.macros;
    const before = new Map(macros);
    const scheme = outcome(() => assemble(callSchemeProcedure(expander('expand'), [form]), analyze));
    const afterScheme = new Map(macros);
    const javascript = outcome(() => analyzeInJavaScript(form, null, context));
    if (scheme.text !== javascript.text) disagree('form', form, scheme.text, javascript.text);
    for (const [name, javascriptMade] of macros) {
      const schemeMade = afterScheme.get(name);
      if (schemeMade === before.get(name) || javascriptMade === schemeMade) continue;
      if (schemeMade?.scheme === undefined || javascriptMade.transformerProcedure !== undefined) continue;
      const comparing = compareTransformers(name, schemeMade, javascriptMade);
      macros.set(name, comparing);
      const defining = globalContext.definingScopes;
      if (defining.length > 0) {
        globalContext.defineKeyword(defining[defining.length - 1], name, name, comparing);
      }
    }
    if (javascript.error !== undefined) throw javascript.error;
    return javascript.node;
  };

  const previous = useTopLevelAnalyzer(compare);
  record.uninstall = () => useTopLevelAnalyzer(previous);
  return record;
}

/**
 * What a transformer made of a use: the datum written out, or its error.
 * @param {function(): *} expansion - Calls the transformer.
 * @returns {{text: string, value: *, error: *}}
 */
function outcomeOfData(expansion) {
  try {
    const value = expansion();
    return { text: written(value), value };
  } catch (error) {
    return { text: `error: ${error?.message ?? String(error)}`, error };
  }
}

/**
 * A form written out, cut short if it is long.
 * @param {*} form - The form.
 * @returns {string}
 */
function abbreviated(form) {
  let text;
  try {
    text = writeString(form);
  } catch (e) {
    text = String(form);
  }
  return text.length > 300 ? `${text.slice(0, 300)}...` : text;
}

/**
 * The disagreements recorded, written for a person to read.
 * @param {Object} record - What `installExpanderComparison` returned.
 * @returns {string}
 */
export function comparisonReport(record) {
  const lines = [`Expanders compared on ${record.forms} forms and ${record.macroUses} macro uses: `
    + `${record.count} disagreement${record.count === 1 ? '' : 's'}.`];
  for (const { kind, form, scheme, javascript } of record.disagreements) {
    lines.push('', `-- ${kind}: ${form}`, `   scheme:     ${scheme.slice(0, 2000)}`,
      `   javascript: ${javascript.slice(0, 2000)}`);
  }
  return lines.join('\n');
}
