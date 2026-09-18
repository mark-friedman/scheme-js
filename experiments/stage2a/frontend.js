/**
 * Shared front end for the Stage 2a prototypes.
 *
 * Reads Scheme with the project's own reader, then lowers a fixed subset to a
 * small normalized tree that both backends consume. Sharing the front end is
 * what makes the bake-off meaningful: any difference in the resulting numbers
 * is a difference between the two calling conventions, not between two
 * independently written compilers.
 *
 * The subset is whatever the eight benchmark programs use, and no more.
 */

import { parse } from '../../src/core/interpreter/reader.js';
import { Cons, toArray } from '../../src/core/interpreter/cons.js';
import { Symbol as SchemeSymbol } from '../../src/core/interpreter/symbol.js';

/**
 * Primitives that transfer control and therefore cannot be compiled as direct
 * JavaScript calls. Every other primitive can be, because it neither captures a
 * continuation nor tail-calls.
 */
export const CONTROL_PRIMITIVES = new Set(['call/cc', 'call-with-current-continuation']);

/** Special forms handled directly by the front end. */
const SPECIAL = new Set([
  'define', 'lambda', 'if', 'let', 'let*', 'set!', 'begin', 'quote',
  'and', 'or', 'when', 'unless'
]);

/**
 * @typedef {Object} Node
 * @property {string} t - Node kind.
 */

/**
 * Compile-time scope: maps Scheme names to generated JavaScript identifiers.
 */
class Scope {
  /**
   * @param {Scope|null} parent - Enclosing scope.
   */
  constructor(parent = null) {
    this.parent = parent;
    this.names = new Map();
  }

  /**
   * Binds a Scheme name to a fresh JavaScript identifier.
   * @param {string} name - The Scheme name.
   * @param {function(string): string} mangle - Identifier generator.
   * @returns {string} The generated identifier.
   */
  bind(name, mangle) {
    const js = mangle(name);
    this.names.set(name, js);
    return js;
  }

  /**
   * Resolves a Scheme name to a JavaScript identifier, or null if free.
   * @param {string} name - The Scheme name.
   * @returns {string|null} The identifier, or null if not lexically bound.
   */
  lookup(name) {
    let scope = this;
    while (scope) {
      const found = scope.names.get(name);
      if (found !== undefined) return found;
      scope = scope.parent;
    }
    return null;
  }
}

/** Characters that are legal in Scheme names but not in JavaScript ones. */
function mangleName(name, counter) {
  const safe = name.replace(/[^A-Za-z0-9_]/g, (ch) => '_' + ch.charCodeAt(0).toString(16));
  return `${safe}$${counter}`;
}

/**
 * Lowers a parsed program to the normalized tree.
 */
export class Frontend {
  constructor() {
    this.counter = 0;
    this.callId = 0;
    this.globals = new Scope(null);
  }

  /** @returns {string} A fresh identifier suffix. */
  fresh(name) {
    return mangleName(name, this.counter++);
  }

  /**
   * Lowers Scheme source text to a list of top-level nodes.
   * @param {string} source - Scheme source.
   * @returns {Array<Node>} Top-level nodes.
   */
  compileProgram(source) {
    const forms = parse(source);
    // Top-level definitions are visible to each other regardless of order, so
    // their names are bound before any body is lowered.
    for (const form of forms) {
      if (form instanceof Cons && nameOf(form.car) === 'define') {
        const target = form.cdr.car;
        const defName = target instanceof Cons ? nameOf(target.car) : nameOf(target);
        if (defName && !this.globals.names.has(defName)) {
          this.globals.bind(defName, (n) => this.fresh(n));
        }
      }
    }
    return forms.map((form) => this.lower(form, this.globals));
  }

  /**
   * Lowers one expression.
   * @param {*} exp - A datum from the reader.
   * @param {Scope} scope - The enclosing scope.
   * @returns {Node} The normalized node.
   */
  lower(exp, scope) {
    if (exp instanceof SchemeSymbol) {
      const js = scope.lookup(exp.name);
      if (js) return { t: 'ref', js, name: exp.name };
      return { t: 'prim', name: exp.name, control: CONTROL_PRIMITIVES.has(exp.name) };
    }
    if (typeof exp === 'bigint' || typeof exp === 'number' ||
        typeof exp === 'boolean' || typeof exp === 'string' || exp === null) {
      return { t: 'const', value: exp };
    }
    if (!(exp instanceof Cons)) {
      throw new Error(`stage2a front end: unsupported datum ${String(exp)}`);
    }

    const head = nameOf(exp.car);
    if (head && SPECIAL.has(head) && scope.lookup(head) === null) {
      return this[`lower_${head.replace(/[!*]/g, (c) => c === '!' ? '_bang' : '_star')}`](exp, scope);
    }

    const parts = toArray(exp);
    // Every call site gets a stable identifier in the front end so that
    // convention B's straight-line code and its resumable twin agree on where
    // to resume without having to coordinate their emitters.
    // `tmp` names the variable holding this call's result. Assigning it here,
    // rather than in each backend, is what lets convention B's straight-line
    // code and its resumable twin build and consume the same frame layout.
    const callId = this.callId++;
    return {
      t: 'call',
      callId,
      tmp: `r${callId}`,
      fn: this.lower(parts[0], scope),
      args: parts.slice(1).map((a) => this.lower(a, scope))
    };
  }

  lower_define(exp, scope) {
    const parts = toArray(exp);
    const target = parts[1];
    if (target instanceof Cons) {
      const name = nameOf(target.car);
      const js = scope.lookup(name) ?? scope.bind(name, (n) => this.fresh(n));
      const lambda = this.makeLambda(target.cdr, parts.slice(2), scope, name);
      return { t: 'define', js, name, value: lambda };
    }
    const name = nameOf(target);
    const js = scope.lookup(name) ?? scope.bind(name, (n) => this.fresh(n));
    return { t: 'define', js, name, value: this.lower(parts[2], scope) };
  }

  lower_lambda(exp, scope) {
    const parts = toArray(exp);
    return this.makeLambda(parts[1], parts.slice(2), scope, 'lambda');
  }

  /**
   * Builds a lambda node, handling both fixed and dotted parameter lists.
   * @param {*} params - The parameter list datum.
   * @param {Array<*>} bodyForms - Body expressions.
   * @param {Scope} scope - Enclosing scope.
   * @param {string} name - Name for diagnostics and generated identifiers.
   * @returns {Node} A lambda node.
   */
  makeLambda(params, bodyForms, scope, name) {
    const inner = new Scope(scope);
    const fixed = [];
    let rest = null;
    let cursor = params;
    while (cursor instanceof Cons) {
      fixed.push(inner.bind(nameOf(cursor.car), (n) => this.fresh(n)));
      cursor = cursor.cdr;
    }
    if (cursor !== null) {
      rest = inner.bind(nameOf(cursor), (n) => this.fresh(n));
    }
    // Internal definitions are bound before the body is lowered so that
    // internal procedures can be mutually recursive, matching R7RS.
    for (const form of bodyForms) {
      if (form instanceof Cons && nameOf(form.car) === 'define') {
        const target = form.cdr.car;
        const defName = target instanceof Cons ? nameOf(target.car) : nameOf(target);
        if (defName) inner.bind(defName, (n) => this.fresh(n));
      }
    }
    return {
      t: 'lambda',
      name,
      // A name for the variable this lambda is stored in when it appears in
      // value position, shared by both backends for the same reason `tmp` is.
      tmp: `fn${this.callId++}`,
      params: fixed,
      rest,
      body: bodyForms.map((f) => this.lower(f, inner))
    };
  }

  lower_if(exp, scope) {
    const parts = toArray(exp);
    return {
      t: 'if',
      tmp: `c${this.callId++}`,
      test: this.lower(parts[1], scope),
      then: this.lower(parts[2], scope),
      else: parts.length > 3 ? this.lower(parts[3], scope) : { t: 'const', value: undefined }
    };
  }

  lower_let(exp, scope) {
    const parts = toArray(exp);
    // Named let: (let loop ((v init) ...) body ...)
    if (parts[1] instanceof SchemeSymbol) {
      const loopName = nameOf(parts[1]);
      const bindings = toArray(parts[2]).map((b) => toArray(b));
      const inits = bindings.map((b) => this.lower(b[1], scope));
      const loopScope = new Scope(scope);
      const js = loopScope.bind(loopName, (n) => this.fresh(n));
      const lambdaParams = bindings.map((b) => b[0]);
      const lambda = this.makeLambda(
        listFrom(lambdaParams), parts.slice(3), loopScope, loopName);
      return {
        t: 'labels', js, name: loopName, lambda,
        call: (() => { const id = this.callId++; return { t: 'call', callId: id, tmp: `r${id}`, fn: { t: 'ref', js, name: loopName }, args: inits }; })()
      };
    }
    const bindings = toArray(parts[1]).map((b) => toArray(b));
    const inits = bindings.map((b) => this.lower(b[1], scope));
    const inner = new Scope(scope);
    const names = bindings.map((b) => inner.bind(nameOf(b[0]), (n) => this.fresh(n)));
    return { t: 'let', names, inits, body: parts.slice(2).map((f) => this.lower(f, inner)) };
  }

  lower_let_star(exp, scope) {
    const parts = toArray(exp);
    const bindings = toArray(parts[1]).map((b) => toArray(b));
    // let* is nested lets; lowering it that way keeps one `let` node kind.
    let scopeCursor = scope;
    const names = [];
    const inits = [];
    for (const b of bindings) {
      inits.push(this.lower(b[1], scopeCursor));
      scopeCursor = new Scope(scopeCursor);
      names.push(scopeCursor.bind(nameOf(b[0]), (n) => this.fresh(n)));
    }
    return {
      t: 'letSeq', names, inits,
      body: parts.slice(2).map((f) => this.lower(f, scopeCursor))
    };
  }

  lower_set_bang(exp, scope) {
    const parts = toArray(exp);
    const name = nameOf(parts[1]);
    const js = scope.lookup(name);
    if (!js) throw new Error(`stage2a front end: set! on unbound ${name}`);
    return { t: 'set', js, name, value: this.lower(parts[2], scope) };
  }

  lower_begin(exp, scope) {
    return { t: 'begin', body: toArray(exp).slice(1).map((f) => this.lower(f, scope)) };
  }

  lower_quote(exp, scope) {
    return { t: 'quote', value: toArray(exp)[1] };
  }

  /**
   * Lowers `and` to nested conditionals.
   *
   * Desugaring here rather than in each backend keeps the backends smaller and
   * guarantees both get identical control flow for short-circuit operators,
   * which is the point of sharing a front end.
   */
  lower_and(exp, scope) {
    const parts = toArray(exp).slice(1).map((f) => this.lower(f, scope));
    const build = (i) => (i >= parts.length
      ? { t: 'const', value: true }
      : (i === parts.length - 1
        ? parts[i]
        : { t: 'if', tmp: `c${this.callId++}`, test: parts[i], then: build(i + 1), else: { t: 'const', value: false } }));
    return build(0);
  }

  /**
   * Lowers `or` to nested conditionals, binding each test to a temporary so it
   * is evaluated once and its value can be returned.
   */
  lower_or(exp, scope) {
    const parts = toArray(exp).slice(1).map((f) => this.lower(f, scope));
    const build = (i) => {
      if (i >= parts.length) return { t: 'const', value: false };
      if (i === parts.length - 1) return parts[i];
      const tmp = this.fresh('or');
      return {
        t: 'let',
        names: [tmp],
        inits: [parts[i]],
        body: [{ t: 'if', tmp: `c${this.callId++}`, test: { t: 'ref', js: tmp, name: 'or' }, then: { t: 'ref', js: tmp, name: 'or' }, else: build(i + 1) }]
      };
    };
    return build(0);
  }

  lower_when(exp, scope) {
    const parts = toArray(exp);
    return {
      t: 'if',
      tmp: `c${this.callId++}`,
      test: this.lower(parts[1], scope),
      then: { t: 'begin', body: parts.slice(2).map((f) => this.lower(f, scope)) },
      else: { t: 'const', value: undefined }
    };
  }

  lower_unless(exp, scope) {
    const parts = toArray(exp);
    return {
      t: 'if',
      tmp: `c${this.callId++}`,
      test: (() => { const id = this.callId++; return { t: 'call', callId: id, tmp: `r${id}`, fn: { t: 'prim', name: 'not', control: false }, args: [this.lower(parts[1], scope)] }; })(),
      then: { t: 'begin', body: parts.slice(2).map((f) => this.lower(f, scope)) },
      else: { t: 'const', value: undefined }
    };
  }
}

/**
 * Extracts a symbol's name, or null if the datum is not a symbol.
 * @param {*} exp - A datum.
 * @returns {string|null} The name.
 */
function nameOf(exp) {
  return exp instanceof SchemeSymbol ? exp.name : null;
}

/**
 * Builds a Scheme list from an array of data.
 * @param {Array<*>} items - The elements.
 * @returns {*} A proper list.
 */
function listFrom(items) {
  let list = null;
  for (let i = items.length - 1; i >= 0; i--) list = new Cons(items[i], list);
  return list;
}

/**
 * Classifies whether an expression can capture a continuation.
 *
 * This is the static form of the observation that drove Stage 1: a constant or
 * a variable reference has no suspension point inside it, so it needs no
 * continuation frame and, under convention B, no unwind check after it. Both
 * backends use this, so neither is credited for it over the other.
 *
 * Anything containing a call is treated as capturing. A real compiler would
 * refine this with an effect analysis -- a call to a known non-capturing
 * primitive does not need a frame either -- but over-approximating here is
 * conservative and keeps the prototypes comparable.
 *
 * @param {Node} node - A normalized node.
 * @returns {boolean} True if evaluating it might capture a continuation.
 */
export function canCapture(node) {
  switch (node.t) {
    case 'const': case 'ref': case 'prim': case 'quote': case 'lambda':
    case 'boxref':
      return false;
    case 'if':
      return canCapture(node.test) || canCapture(node.then) || canCapture(node.else);
    case 'set': case 'boxset':
      return canCapture(node.value);
    case 'begin':
      return node.body.some(canCapture);
    default:
      return true;
  }
}

/**
 * Assignment conversion: boxes local variables that are assigned to.
 *
 * Both conventions need this, for the same underlying reason. A procedure that
 * can be re-entered -- because control left it through the trampoline under
 * convention A, or because a continuation capture unwound it under convention B
 * -- restores its locals from a frame, which creates a *fresh* JavaScript
 * binding. A closure created before that point still refers to the old binding,
 * so an assignment through one is invisible to the other. Boxing the variable
 * gives both a single shared cell.
 *
 * Pettyjohn et al. list this as the first step of their transformation, and
 * Marshall's variant keeps it. It is done here in the shared front end rather
 * than in a backend, because it is a requirement of supporting first-class
 * continuations at all and not a property of either convention.
 *
 * Only *local* variables need it: top-level bindings compile to module-level
 * JavaScript variables, which every closure already shares.
 *
 * @param {Array<Node>} nodes - Top-level nodes.
 * @returns {Array<Node>} The same tree with boxed variables rewritten.
 */
export function assignmentConvert(nodes) {
  const assigned = new Set();
  const topLevel = new Set();

  for (const node of nodes) {
    if (node.t === 'define') topLevel.add(node.js);
  }
  collectAssigned(nodes, assigned);

  const boxed = new Set([...assigned].filter((name) => !topLevel.has(name)));
  if (boxed.size === 0) return nodes;
  return nodes.map((n) => rewrite(n, boxed));
}

/**
 * Collects every name that appears as the target of a `set!`.
 * @param {*} node - A node or array of nodes.
 * @param {Set<string>} out - Accumulator.
 * @returns {void}
 */
function collectAssigned(node, out) {
  if (Array.isArray(node)) { for (const n of node) collectAssigned(n, out); return; }
  if (!node || typeof node !== 'object' || !node.t) return;
  if (node.t === 'set') out.add(node.js);
  for (const key of Object.keys(node)) {
    if (key === 't' || key === 'js' || key === 'name') continue;
    collectAssigned(node[key], out);
  }
}

/**
 * Rewrites references, assignments and binding sites for boxed variables.
 * @param {*} node - A node or array of nodes.
 * @param {Set<string>} boxed - Names to box.
 * @returns {*} The rewritten node.
 */
function rewrite(node, boxed) {
  if (Array.isArray(node)) return node.map((n) => rewrite(n, boxed));
  if (!node || typeof node !== 'object' || !node.t) return node;

  switch (node.t) {
    case 'ref':
      return boxed.has(node.js) ? { t: 'boxref', js: node.js, name: node.name } : node;
    case 'set':
      return boxed.has(node.js)
        ? { t: 'boxset', js: node.js, name: node.name, value: rewrite(node.value, boxed) }
        : { ...node, value: rewrite(node.value, boxed) };
    case 'lambda': {
      const boxedParams = [...node.params, node.rest].filter((p) => p && boxed.has(p));
      return { ...node, boxedParams, body: rewrite(node.body, boxed) };
    }
    case 'let': case 'letSeq':
      return {
        ...node,
        boxedNames: node.names.filter((n) => boxed.has(n)),
        inits: rewrite(node.inits, boxed),
        body: rewrite(node.body, boxed)
      };
    default: {
      const out = { ...node };
      for (const key of Object.keys(node)) {
        if (key === 't' || key === 'js' || key === 'name') continue;
        out[key] = rewrite(node[key], boxed);
      }
      return out;
    }
  }
}
