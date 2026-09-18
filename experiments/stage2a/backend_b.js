/**
 * Convention B — native JavaScript stack.
 *
 * Non-tail calls are ordinary JavaScript calls, so one live Scheme frame is one
 * live JavaScript frame. Tail calls return through a per-call-site trampoline,
 * which keeps tail recursion in constant space while still letting genuine
 * recursion show up as genuine JavaScript recursion.
 *
 * `call/cc` is implemented by the cooperative unwind of Pettyjohn et al., with
 * Marshall's modification: control is transferred by a distinguished return
 * value rather than by throwing, because on a high-level VM a throw costs
 * thousands of times more than a compare-and-branch.
 *
 * The cost this convention imposes on ordinary code is one identity comparison
 * after every call that might capture, plus the fragmentation needed to make a
 * procedure re-enterable at each such call site. That cost is what the bake-off
 * is measuring, so it is emitted faithfully rather than optimized away.
 */

import { canCapture } from './frontend.js';
import { emitResumable } from './backend_b_resume.js';

/**
 * Emits JavaScript for a whole program under convention B.
 * @param {Array<Object>} nodes - Top-level nodes from the front end.
 * @returns {string} JavaScript source.
 */
export function emitProgram(nodes) {
  // `frameVars` is filled in by the resumable twins and read by the fast path,
  // so the two agree on what a reified frame contains.
  const ctx = { temps: 0, lines: [], frameVars: new Map() };
  // Resumable twins need to build closures with the ordinary calling
  // convention; this is how the shared block emitter reaches back for it.
  ctx.emitNestedPair = (name, lambda, c) =>
    `${emitNestedPair(`${name}Fn`, lambda, c)}\n${name} = ${name}Fn;`;
  const out = [];
  for (const node of nodes) {
    out.push(emitTopLevel(node, ctx));
  }
  return out.join('\n\n');
}

/**
 * Emits one top-level form.
 * @param {Object} node - A node.
 * @param {Object} ctx - Emitter state.
 * @returns {string} JavaScript source.
 */
function emitTopLevel(node, ctx) {
  if (node.t === 'define' && node.value.t === 'lambda') {
    return emitPair(node.js, node.value, ctx);
  }
  if (node.t === 'define') {
    const body = new Emitter(ctx, node.js, new Set());
    const value = body.expr(node.value);
    return `${body.flush()}\nlet ${node.js} = ${value};`;
  }
  const body = new Emitter(ctx, 'TOPLEVEL', new Set());
  const value = body.expr(node);
  return `${body.flush()}\nRESULT = ${value};`;
}

/**
 * Emits a named function for a lambda node.
 *
 * The generated function carries the Scheme procedure's name so that a
 * JavaScript stack trace -- which is what a debugger's call-stack panel is
 * built from -- reads in Scheme terms.
 *
 * @param {string} name - Generated identifier for the procedure.
 * @param {Object} lambda - The lambda node.
 * @param {Object} ctx - Emitter state.
 * @returns {string} JavaScript source.
 */
/**
 * Emits a procedure together with its resumable twin.
 *
 * The twin is emitted first because it is what decides each call site's frame
 * layout, which the fast path then has to construct.
 *
 * @param {string} name - Identifier for the procedure.
 * @param {Object} lambda - The lambda node.
 * @param {Object} ctx - Emitter state.
 * @returns {string} JavaScript source declaring both.
 */
function emitPair(name, lambda, ctx) {
  const twin = emitResumable(`${name}$k`, lambda, ctx);
  const fast = emitFunction(name, lambda, ctx);
  return `${twin}\n${fast}\n${name}.$k = ${name}$k;`;
}

/**
 * Emits a procedure and its twin as statements, for nested lambdas.
 *
 * Nested procedures cannot be hoisted: both halves must close over the same
 * enclosing variables, so they are declared side by side in place.
 *
 * @param {string} name - Identifier to assign to.
 * @param {Object} lambda - The lambda node.
 * @param {Object} ctx - Emitter state.
 * @returns {string} JavaScript source.
 */
function emitNestedPair(name, lambda, ctx) {
  const twin = emitResumable(`${name}$k`, lambda, ctx);
  const fast = emitFunction(name, lambda, ctx);
  return `${twin}\n${fast}\n${name}.$k = ${name}$k;`;
}

function emitFunction(name, lambda, ctx) {
  const params = lambda.params.join(', ');
  const rest = lambda.rest ? `${lambda.params.length ? ', ' : ''}...${lambda.rest}Raw` : '';
  const declared = new Set();
  const emitter = new Emitter(ctx, name, declared);
  emitter.body(lambda.body, true);
  let restSetup = lambda.rest
    ? `  let ${lambda.rest} = RT.primitives['list'](...${lambda.rest}Raw);\n`
    : '';
  // Boxed parameters are re-bound to cells on entry, so closures created in
  // this activation and any later re-entry share one location.
  for (const p of (lambda.boxedParams || [])) restSetup += `  ${p} = { v: ${p} };\n`;
  const hoisted = declared.size > 0 ? `  let ${[...declared].join(', ')};\n` : '';
  return `function ${name}(${params}${rest}) {\n${hoisted}${restSetup}${emitter.flush('  ')}\n}`;
}

/**
 * Statement-oriented expression emitter.
 *
 * Expressions compile to a list of statements plus a JavaScript expression for
 * the value, which keeps generated code free of immediately-invoked functions.
 * An IIFE per `let` would have distorted the measurement badly, since both
 * conventions would then be paying for the emitter's convenience.
 */
class Emitter {
  /**
   * @param {Object} ctx - Shared emitter state (temporary counter, frame layouts).
   * @param {string} selfName - Enclosing procedure's identifier, so a call site
   *   can name its own resumable twin when it reifies a frame.
   * @param {Set<string>} declared - Temporaries to hoist to the top of the
   *   procedure. They are hoisted rather than declared inline because a reified
   *   frame has to name them, and a `let` inside a branch would not be in scope.
   */
  constructor(ctx, selfName, declared) {
    this.ctx = ctx;
    this.selfName = selfName;
    this.declared = declared;
    this.out = [];
  }

  /**
   * Creates a sub-emitter sharing this one's procedure context.
   * @returns {Emitter} The sub-emitter.
   */
  sub() {
    return new Emitter(this.ctx, this.selfName, this.declared);
  }

  /** @returns {string} A fresh temporary name. */
  temp() {
    const name = `t${this.ctx.temps++}`;
    this.declared.add(name);
    return name;
  }

  /**
   * Emits a sequence of body expressions.
   * @param {Array<Object>} body - Body expressions.
   * @param {boolean} tail - Whether the sequence is in tail position.
   * @returns {void}
   */
  body(body, tail) {
    for (let i = 0; i < body.length; i++) {
      const last = i === body.length - 1;
      if (last && tail) {
        this.tailReturn(body[i]);
      } else if (last) {
        this.out.push(`return ${this.expr(body[i])};`);
      } else {
        const value = this.expr(body[i]);
        this.out.push(`${value};`);
      }
    }
    if (body.length === 0) this.out.push('return undefined;');
  }

  /**
   * Emits an expression in tail position.
   * @param {Object} node - The expression.
   * @returns {void}
   */
  tailReturn(node) {
    if (node.t === 'if') {
      const test = this.expr(node.test);
      const then = this.sub();
      then.tailReturn(node.then);
      const other = this.sub();
      other.tailReturn(node.else);
      this.out.push(`if (RT.truthy(${test})) {\n${then.flush('  ')}\n} else {\n${other.flush('  ')}\n}`);
      return;
    }
    if (node.t === 'begin') {
      this.body(node.body, true);
      return;
    }
    if (node.t === 'call') {
      const fn = this.expr(node.fn);
      const args = node.args.map((a) => this.expr(a));
      if (node.fn.t === 'prim' && !node.fn.control) {
        this.out.push(`return ${fn}(${args.join(', ')});`);
        return;
      }
      // A tail call sets the shared registers and returns the sentinel. The
      // caller's trampoline picks it up, so the JavaScript stack does not grow.
      this.out.push(`RT.tail.fn = ${fn}; RT.tail.args = [${args.join(', ')}]; return RT.TAIL_CALL;`);
      return;
    }
    if (node.t === 'let' || node.t === 'letSeq' || node.t === 'labels') {
      this.bindingForm(node, true);
      return;
    }
    this.out.push(`return ${this.expr(node)};`);
  }

  /**
   * Emits a binding form.
   * @param {Object} node - A let, let* or named-let node.
   * @param {boolean} tail - Whether the form is in tail position.
   * @returns {string|undefined} Value expression when not in tail position.
   */
  bindingForm(node, tail) {
    if (node.t === 'labels') {
      this.out.push(emitNestedPair(node.js, node.lambda, this.ctx));
      if (tail) { this.tailReturn(node.call); return undefined; }
      return this.expr(node.call);
    }
    const boxedHere = new Set(node.boxedNames || []);
    // Names are declared before their initializers are emitted, because an
    // initializer may contain a call whose reified frame mentions them.
    node.names.forEach((n) => this.declared.add(n));
    node.names.forEach((n, i) => {
      const init = this.expr(node.inits[i]);
      this.out.push(`${n} = ${boxedHere.has(n) ? `{ v: ${init} }` : init};`);
    });
    if (tail) { this.body(node.body, true); return undefined; }
    const values = node.body.map((b) => this.expr(b));
    return values[values.length - 1];
  }

  /**
   * Emits an expression in value position.
   * @param {Object} node - The expression.
   * @returns {string} A JavaScript expression for its value.
   */
  expr(node) {
    switch (node.t) {
      case 'const':
        return literal(node.value);
      case 'quote':
        return quoted(node.value);
      case 'ref':
        return node.js;
      case 'boxref':
        return `${node.js}.v`;
      case 'boxset':
        this.out.push(`${node.js}.v = ${this.expr(node.value)};`);
        return 'undefined';
      case 'prim':
        return node.control ? 'RT.callcc' : `RT.primitives[${JSON.stringify(node.name)}]`;
      case 'lambda': {
        // Shares the front end's name with the twin, so a reified frame that
        // mentions this closure resolves in both halves.
        const name = node.tmp ?? `anon${this.ctx.temps++}`;
        this.declared.add(name);
        this.out.push(emitNestedPair(`${name}Fn`, node, this.ctx));
        this.out.push(`${name} = ${name}Fn;`);
        return name;
      }
      case 'set':
        this.out.push(`${node.js} = ${this.expr(node.value)};`);
        return 'undefined';
      case 'begin': {
        const values = node.body.map((b) => this.expr(b));
        return values[values.length - 1] ?? 'undefined';
      }
      case 'if': {
        const test = this.expr(node.test);
        const result = node.tmp ?? this.temp();
        this.declared.add(result);
        const then = this.sub();
        const thenValue = then.expr(node.then);
        const other = this.sub();
        const elseValue = other.expr(node.else);
        this.out.push(
          `if (RT.truthy(${test})) {\n${then.flush('  ')}\n  ${result} = ${thenValue};\n` +
          `} else {\n${other.flush('  ')}\n  ${result} = ${elseValue};\n}`);
        return result;
      }
      case 'let': case 'letSeq': case 'labels':
        return this.bindingForm(node, false);
      case 'call': {
        const fn = this.expr(node.fn);
        const args = node.args.map((a) => this.expr(a));
        const result = node.tmp ?? this.temp();
        this.declared.add(result);
        // A call to a known primitive is a plain JavaScript call: a primitive
        // never tail-calls and never captures, so it needs neither the
        // trampoline check nor a continuation frame. Both backends do this, so
        // neither is credited for it over the other.
        if (node.fn.t === 'prim' && !node.fn.control) {
          this.out.push(`${result} = ${fn}(${args.join(', ')});`);
          return result;
        }
        // An ordinary JavaScript call: this is the whole point of convention B.
        this.out.push(`${result} = ${fn}(${args.join(', ')});`);
        // Tail calls made by the callee surface here as the sentinel, so every
        // non-tail call site carries a small trampoline.
        this.out.push(`while (${result} === RT.TAIL_CALL) { const f = RT.tail.fn, a = RT.tail.args; ${result} = f(...a); }`);
        // And the unwind check. On the normal path this is one identity
        // comparison; the frame is built only when a capture is actually in
        // progress, which is the whole argument for this convention.
        const layout = this.ctx.frameVars.get(node.callId);
        if (layout) {
          const frame = `{ ${layout.vars.join(', ')}, $r: undefined }`;
          this.out.push(
            `if (${result} === RT.UNWIND) { RT.unwinding.frames.push(` +
            `{ proc: ${this.selfName}.$k, pc: ${layout.pc}, frame: ${frame} }); return RT.UNWIND; }`);
        }
        return result;
      }
      default:
        throw new Error(`stage2a backend B: cannot emit ${node.t}`);
    }
  }

  /**
   * Renders accumulated statements.
   * @param {string} [indent] - Indentation prefix.
   * @returns {string} JavaScript source.
   */
  flush(indent = '') {
    return this.out.map((line) => line.split('\n').map((l) => indent + l).join('\n')).join('\n');
  }
}

/**
 * Renders a literal datum as a JavaScript expression.
 * @param {*} value - The datum.
 * @returns {string} JavaScript source.
 */
export function literal(value) {
  if (value === null) return 'null';
  if (value === undefined) return 'undefined';
  if (typeof value === 'bigint') return `${value}n`;
  if (typeof value === 'boolean') return String(value);
  if (typeof value === 'string') return JSON.stringify(value);
  if (typeof value === 'number') return String(value);
  throw new Error(`stage2a: cannot emit literal ${String(value)}`);
}

/**
 * Renders a quoted datum, which for these benchmarks is only ever the empty list.
 * @param {*} value - The datum.
 * @returns {string} JavaScript source.
 */
export function quoted(value) {
  if (value === null) return 'null';
  if (value && typeof value === 'object' && typeof value.name === 'string' && !('car' in value)) {
    // A symbol from the reader. Interning it keeps `eq?` an identity test.
    return `RT.sym(${JSON.stringify(value.name)})`;
  }
  return literal(value);
}
