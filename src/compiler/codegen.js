/**
 * @fileoverview JavaScript code generation for the Scheme compiler.
 *
 * Emits convention B, chosen in Stage 2a: non-tail calls are ordinary
 * JavaScript calls, so one live Scheme frame is one live JavaScript frame and a
 * debugger can see the Scheme call stack; tail calls return a `TailCall` through
 * a per-call-site trampoline, so tail recursion runs in constant space.
 *
 * Reusing the interpreter's own `TailCall` as the tail-call signal, rather than
 * a private sentinel, is what makes interoperation free in both directions: the
 * interpreter already knows how to continue a returned `TailCall`, and compiled
 * trampolines already know how to continue one returned by an interpreted
 * procedure.
 *
 * Code is emitted as statements with explicit temporaries rather than as nested
 * expressions, because an immediately-invoked function per `let` would cost on
 * every evaluation what it saves once at compile time.
 */

/**
 * Renders a Scheme value that survived to compile time as a JavaScript
 * expression, or reports that it has to be passed in as a constant instead.
 *
 * Only immediates are inlined. Pairs, symbols, vectors and the like are
 * interned into the constant pool: they have identity that `eq?` can observe,
 * so re-creating them per evaluation would be wrong as well as slow.
 *
 * @param {*} value - The value.
 * @param {Object} ctx - Emitter context holding the constant pool.
 * @returns {string} A JavaScript expression.
 */
function constant(value, ctx) {
  if (value === null) return 'null';
  if (value === undefined) return 'undefined';
  if (value === true) return 'true';
  if (value === false) return 'false';
  if (typeof value === 'bigint') return `${value}n`;
  if (typeof value === 'number') return Number.isFinite(value) ? String(value) : `Number("${value}")`;
  if (typeof value === 'string') return JSON.stringify(value);
  const index = ctx.constants.length;
  ctx.constants.push(value);
  return `K[${index}]`;
}

import { inlinableFor, INLINABLE } from './inline.js';
import { currentBinding } from './runtime.js';

/**
 * Emits JavaScript for one procedure.
 */
class ProcedureEmitter {
  /**
   * @param {string} name - Generated identifier for the procedure.
   * @param {Object} ir - A `lambda` IR node.
   * @param {Object} ctx - Shared emitter context.
   */
  constructor(name, ir, ctx) {
    this.name = name;
    this.ir = ir;
    this.ctx = ctx;
    this.declared = new Set();
    this.out = [];
  }

  /** @returns {string} A fresh temporary name. */
  /**
   * Emits a `letrec` group's bindings.
   *
   * All names are declared before any initializer is emitted, so a reference
   * from one lambda to another resolves regardless of the order they appear
   * in -- which is what mutual recursion needs. Every initializer is a lambda,
   * so evaluating them cannot have a side effect and the order is unobservable;
   * R7RS's rule that all initializers run before any assignment is satisfied
   * whichever way they are emitted.
   *
   * @param {Object} node - A `letrec` IR node.
   * @returns {void}
   */
  emitLetRecBindings(node) {
    for (const name of node.names) this.declared.add(jsName(name));
    for (let i = 0; i < node.names.length; i++) {
      const init = this.value(node.inits[i]);
      this.out.push(`${jsName(node.names[i])} = ${init};`);
    }
  }

  temp() {
    const name = `$t${this.ctx.temps++}`;
    this.declared.add(name);
    return name;
  }

  /**
   * Emits the procedure.
   * @returns {string} JavaScript source for a function expression.
   */
  emit() {
    const params = this.ir.params.map(jsName);
    const rest = this.ir.rest ? `...${jsName(this.ir.rest)}$raw` : null;
    const signature = [...params, rest].filter(Boolean).join(', ');

    this.statement(this.ir.body);

    const prologue = [];
    if (this.ir.rest) {
      // A rest parameter is a Scheme list, not a JavaScript array.
      prologue.push(`let ${jsName(this.ir.rest)} = R.listFrom(${jsName(this.ir.rest)}$raw);`);
    }
    const declarations = [...this.declared];
    if (declarations.length > 0) prologue.unshift(`let ${declarations.join(', ')};`);

    const body = [...prologue, ...this.out].map((l) => '  ' + l).join('\n');
    return `function ${this.name}(${signature}) {\n${body}\n}`;
  }

  /**
   * Emits an IR node as a statement. A node in tail position ends the
   * procedure; anything else contributes statements and a value.
   * @param {Object} node - An IR node.
   * @returns {void}
   */
  statement(node) {
    if (!node.tail) {
      const value = this.value(node);
      if (value !== null) this.out.push(`${value};`);
      return;
    }

    switch (node.k) {
      case 'if': {
        const test = this.value(node.test);
        const then = this.branch(node.then);
        const other = this.branch(node.else);
        this.out.push(`if (${test} !== false) {\n${then}\n} else {\n${other}\n}`);
        return;
      }
      case 'seq': {
        for (let i = 0; i < node.exprs.length; i++) this.statement(node.exprs[i]);
        if (node.exprs.length === 0) this.out.push('return undefined;');
        return;
      }
      case 'let': {
        const init = this.value(node.init);
        this.declared.add(jsName(node.name));
        this.out.push(`${jsName(node.name)} = ${init};`);
        this.statement(node.body);
        return;
      }
      case 'letrec': {
        this.emitLetRecBindings(node);
        this.statement(node.body);
        return;
      }
      case 'call': {
        // A primitive in tail position needs no trampoline: it cannot itself
        // tail-call, so its value is this procedure's value. Returning it
        // directly avoids allocating a `TailCall` for what is one of the most
        // common shapes in Scheme -- `(+ ...)` closing out a procedure body.
        const inlined = this.tryInline(node);
        if (inlined !== null) {
          this.out.push(`return ${inlined};`);
          return;
        }
        const fn = this.value(node.fn);
        const args = node.args.map((a) => this.value(a));
        // Otherwise the tail call returns rather than calls, so the caller's
        // trampoline -- or the interpreter's, if that is who invoked us --
        // continues it. This is what keeps tail recursion in constant space.
        this.out.push(`return new R.TailCall(${fn}, [${args.join(', ')}]);`);
        return;
      }
      default: {
        const value = this.value(node);
        this.out.push(`return ${value};`);
      }
    }
  }

  /**
   * Emits a branch of a tail-position conditional as an indented block.
   * @param {Object} node - An IR node in tail position.
   * @returns {string} Indented JavaScript source.
   */
  branch(node) {
    const nested = new ProcedureEmitter(this.name, this.ir, this.ctx);
    nested.declared = this.declared;
    nested.statement(node);
    return nested.out.map((l) => '  ' + l).join('\n');
  }

  /**
   * Attempts to expand a call to a known primitive inline.
   *
   * Returns null when the call is not eligible, in which case the caller emits
   * an ordinary call. Eligibility needs three things: the operator must be a
   * global, that global must currently be bound to a primitive with an inline
   * expansion, and the argument count must match the expansion's arity.
   *
   * The emitted code is guarded on the binding, because Scheme allows the
   * primitive to be redefined after this code was compiled. Arguments are
   * forced into temporaries first, since an expansion mentions each operand
   * more than once and re-evaluating one would be wrong.
   *
   * @param {Object} node - A `call` IR node.
   * @returns {string|null} A JavaScript expression, or null if not inlined.
   */
  tryInline(node) {
    if (node.fn.k !== 'global') return null;
    const entry = inlinableFor(node.fn.name, node.args.length);
    if (entry === null) return null;

    const guard = this.ctx.primitiveGuard(node.fn.name);
    if (guard === null) return null;

    const operands = node.args.map((arg) => {
      const value = this.value(arg);
      // A literal or a variable can be mentioned twice safely; anything else
      // has to be evaluated once into a temporary.
      if (/^(-?\d+n?|null|true|false|s_[A-Za-z0-9_$]*|\$t\d+|K\[\d+\])$/.test(value)) {
        return value;
      }
      const temp = this.temp();
      this.out.push(`${temp} = ${value};`);
      return temp;
    });

    const accessor = this.ctx.globalRef(node.fn.name);
    const slow = `${accessor}()(${operands.join(', ')})`;
    const shapeTest = entry.test(operands);
    const fast = entry.value(operands);
    const result = this.temp();

    // The binding guard always applies; a shape test is added when the fast
    // path only covers some operands.
    const condition = shapeTest === null
      ? `${accessor}() === ${guard}`
      : `${accessor}() === ${guard} && (${shapeTest})`;

    this.out.push(`${result} = ${condition} ? (${fast}) : ${slow};`);
    return result;
  }

  /**
   * Emits an IR node in value position.
   * @param {Object} node - An IR node.
   * @returns {string} A JavaScript expression for its value.
   */
  value(node) {
    switch (node.k) {
      case 'const':
        return constant(node.value, this.ctx);

      case 'local':
        return jsName(node.name);

      case 'global':
        // Resolved through the environment on every reference, because Scheme
        // allows a top-level binding to be redefined after this code was
        // compiled -- including by the REPL that is running it.
        return `${this.ctx.globalRef(node.name)}()`;

      case 'lambda': {
        const name = `$fn${this.ctx.temps++}`;
        const emitter = new ProcedureEmitter(name, node, this.ctx);
        this.out.push(`${emitter.emit()}`);
        this.out.push(`R.markProcedure(${name}, ${JSON.stringify(node.name ?? 'anonymous')});`);
        return name;
      }

      case 'set': {
        const value = this.value(node.value);
        if (node.local) {
          this.out.push(`${jsName(node.name)} = ${value};`);
        } else {
          this.out.push(`E.set(${JSON.stringify(node.name)}, ${value});`);
        }
        return 'undefined';
      }

      case 'define': {
        const value = this.value(node.value);
        this.declared.add(jsName(node.name));
        this.out.push(`${jsName(node.name)} = ${value};`);
        return 'undefined';
      }

      case 'seq': {
        let last = 'undefined';
        for (const expr of node.exprs) last = this.value(expr);
        return last;
      }

      case 'if': {
        const test = this.value(node.test);
        const result = this.temp();
        const then = new ProcedureEmitter(this.name, this.ir, this.ctx);
        then.declared = this.declared;
        const thenValue = then.value(node.then);
        const other = new ProcedureEmitter(this.name, this.ir, this.ctx);
        other.declared = this.declared;
        const elseValue = other.value(node.else);
        const block = (emitter, value) =>
          [...emitter.out, `${result} = ${value};`].map((l) => '  ' + l).join('\n');
        this.out.push(
          `if (${test} !== false) {\n${block(then, thenValue)}\n} else {\n${block(other, elseValue)}\n}`);
        return result;
      }

      case 'let': {
        this.declared.add(jsName(node.name));
        const init = this.value(node.init);
        this.out.push(`${jsName(node.name)} = ${init};`);
        return this.value(node.body);
      }

      case 'letrec': {
        this.emitLetRecBindings(node);
        return this.value(node.body);
      }

      case 'call': {
        const inlined = this.tryInline(node);
        if (inlined !== null) return inlined;

        const args = node.args.map((a) => this.value(a));
        const callee = this.temp();
        const raw = this.temp();
        const result = this.temp();
        // The callee is bound to a temporary first because it is examined twice
        // and must not be evaluated twice.
        this.out.push(`${callee} = ${this.value(node.fn)};`);
        // A compiled procedure or a primitive takes Scheme values directly. An
        // *interpreted* closure is a callable function too, but calling it that
        // way enters Scheme from JavaScript and its wrapper converts -- exact
        // integers become doubles, bignums beyond 2^53 throw. Which tier the
        // callee belongs to is a property of the value, not of the name, so the
        // choice cannot be made at compile time. See R26.
        this.out.push(`${raw} = ${callee}[R.SCHEME_RAW_CALL];`);
        this.out.push(
          `${result} = ${raw} === undefined ? ${callee}(${args.join(', ')})` +
          ` : ${raw}(${args.join(', ')});`);
        // The callee may have made a tail call, whether it was compiled or
        // interpreted. One identity-free instance check per call site settles it.
        this.out.push(
          `while (${result} instanceof R.TailCall) ` +
          `{ ${result} = R.step(${result}); }`);
        return result;
      }

      default:
        throw new Error(`codegen: cannot emit IR node '${node.k}'`);
    }
  }
}

/**
 * Mangles a renamed Scheme identifier into a legal JavaScript one.
 *
 * The analyzer's names look like `x_$147`, which is already close, but Scheme
 * permits characters JavaScript does not.
 *
 * @param {string} name - A renamed Scheme identifier.
 * @returns {string} A legal JavaScript identifier.
 */
export function jsName(name) {
  return 's_' + name.replace(/[^A-Za-z0-9_$]/g, (ch) => '_' + ch.charCodeAt(0).toString(16));
}

/**
 * Generates a JavaScript function expression for a lowered procedure.
 *
 * @param {Object} ir - A `lambda` IR node.
 * @param {Set<string>} globals - Globals the procedure references.
 * @param {string} name - A display name for the generated function.
 * @returns {{source: string, constants: Array<*>, globals: Array<string>}} The
 *   generated source, its constant pool, and the globals to bind.
 */
export function generate(ir, globals, name, env) {
  const globalNames = [...globals];
  const slots = new Map();
  globalNames.forEach((g, i) => slots.set(g, `G${i}`));

  // Globals with an inline expansion also get a constant naming the primitive
  // they denote right now, so generated code can guard on the binding not
  // having been replaced since.
  const guards = new Map();
  if (env) {
    globalNames.forEach((g, i) => {
      if (INLINABLE[g] === undefined) return;
      const current = currentBinding(env, g);
      if (typeof current === 'function') guards.set(g, `P${i}`);
    });
  }

  const ctx = {
    temps: 0,
    constants: [],
    globalRef: (g) => slots.get(g),
    primitiveGuard: (g) => guards.get(g) ?? null
  };

  // A direct, binding-guarded self-recursive call was tried here and removed:
  // it measured 7% *slower* than going through the global accessor (fib(30)
  // 82.7 ms against 76.7 ms). The accessor is already a single hash lookup that
  // V8 inlines, and the guard's conditional callee appears to cost more than it
  // saves. The profile that suggested the optimization was taken at a 9 ms wall
  // time, where the sampling profiler's own overhead was 66% and inflated the
  // accessor's apparent share.

  const emitter = new ProcedureEmitter('$proc', ir, ctx);
  const procedure = emitter.emit();

  // Each global gets a memoizing accessor rather than a direct value, so that a
  // forward reference compiles and a later redefinition is still observed.
  const accessors = globalNames
    .map((g, i) => {
      const lines = [`const G${i} = R.globalAccessor(E, ${JSON.stringify(g)});`];
      if (guards.has(g)) lines.push(`const P${i} = R.currentBinding(E, ${JSON.stringify(g)});`);
      return lines.join('\n');
    })
    .join('\n');

  const source = [
    accessors,
    procedure,
    `R.markProcedure($proc, ${JSON.stringify(name)});`,
    'return $proc;'
  ].filter(Boolean).join('\n');

  return { source, constants: ctx.constants, globals: globalNames };
}
