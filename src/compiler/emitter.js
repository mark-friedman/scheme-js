/**
 * @fileoverview The fast form of a compiled procedure.
 *
 * Emits straight-line JavaScript: a non-tail call is an ordinary JavaScript
 * call, so a debugger sees one JavaScript frame per live Scheme frame, and a
 * tail call returns a `TailCall` through the trampoline so that it does not
 * grow the stack.
 *
 * Reusing the interpreter's own `TailCall` as the tail-call signal, rather than
 * inventing one, is what makes the two tiers interoperate for free: the
 * interpreter already knows how to continue a returned `TailCall`, and compiled
 * trampolines continue interpreted ones.
 *
 * The resumable form of the same procedure is emitted by `resume.js`, which
 * subclasses the emitter here and overrides only control flow.
 */

import { inlinableFor, INLINABLE } from './inline.js';
import { currentBinding } from './runtime.js';

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

/**
 * A fresh naming scope for the procedure at `path`.
 *
 * Each *emission* counts its own temporaries from zero, including the two
 * emissions of the same procedure. That is the point: the fast form and the
 * resumable form have to arrive at the same name for the same value, and they
 * only do if neither inherits a count from the other.
 *
 * @param {string} path - Where the procedure sits in the tree of procedures.
 * @returns {{counter: {n: number}, path: string}} A naming scope.
 */
export function procedureScope(path) {
  return { counter: { n: 0 }, path };
}

/**
 * Emits the fast form of one procedure.
 *
 * Walks the IR once, appending statements to `out` and returning a JavaScript
 * expression for anything in value position. Control flow is emitted as
 * ordinary JavaScript `if`s and calls, which is what makes this form fast and
 * also what makes it impossible to re-enter part-way through -- `TwinEmitter`
 * in `resume.js` subclasses this and overrides only control flow to produce a
 * form that can be.
 */
export class ProcedureEmitter {
  /**
   * @param {string} name - Generated identifier for the procedure.
   * @param {Object} ir - A `lambda` IR node.
   * @param {Object} ctx - Shared emitter context.
   * @param {{counter: {n: number}, path: string}} [scope] - Where this
   *   procedure's generated names come from. A sub-emitter for a branch shares
   *   its parent's, so names stay unique within the procedure; a nested
   *   procedure gets a fresh one, so its names start again from zero and do not
   *   depend on what encloses it.
   */
  constructor(name, ir, ctx, scope) {
    this.name = name;
    this.ir = ir;
    this.ctx = ctx;
    this.scope = scope ?? procedureScope('');
    this.declared = new Set();
    this.out = [];
  }

  /**
   * Names a procedure nested directly inside this one.
   *
   * The name spells out where the procedure sits in the tree of procedures --
   * `$fn2_0` is the first one inside the third one inside the outermost -- which
   * makes it unique across the whole unit without a shared counter to draw
   * from. Uniqueness is not cosmetic here: a procedure refers to its own
   * resumable form by name when it suspends, and a nested procedure whose name
   * collided with its parent's would shadow exactly that reference.
   *
   * @returns {{name: string, path: string}} The identifier for the nested
   *   procedure, and its position in the tree.
   */
  nested() {
    const index = this.scope.counter.n++;
    const path = this.scope.path === ''
      ? String(index) : `${this.scope.path}_${index}`;
    return { name: `$fn${path}`, path };
  }

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
    // Boxes are created before any initializer runs. A name one of its siblings
    // refers to is boxed for exactly this reason: the sibling is built first and
    // has to be handed something that will hold the procedure later.
    for (const name of node.names) {
      if (this.isBoxed(name)) this.out.push(`${jsName(name)} = [undefined];`);
    }
    for (let i = 0; i < node.names.length; i++) {
      const init = this.value(node.inits[i]);
      const name = node.names[i];
      this.out.push(this.isBoxed(name)
        ? `${jsName(name)}[0] = ${init};`
        : `${jsName(name)} = ${init};`);
    }
  }

  /**
   * Emits a nested procedure and its resumable form.
   *
   * A named `let`'s loop and every anonymous procedure become one of these, so
   * many call sites in a program are inside one rather than in the top-level
   * procedure. Each therefore needs its own resumable form, or a continuation
   * captured in one of those places could not be resumed.
   *
   * A plain `let` body used to become one too, and no longer does: lowering
   * reduces an immediately-applied lambda to bindings, which is what keeps this
   * from nesting once per binding in a `let*` chain.
   *
   * @param {string} name - Identifier for the nested procedure.
   * @param {Object} node - A `lambda` IR node.
   * @returns {void}
   */
  emitLambdaPair(name, node, path) {
    // Generated through a factory so that `resume.js` can import this module
    // without this module importing it back.
    const twin = this.ctx.twinFor;
    // Generated *before* the fast form, even though it is emitted after it:
    // generating it is what decides where each of this procedure's call sites
    // resumes and which locals a suspended frame carries, and the fast form
    // needs both of those to be able to suspend itself.
    const twinSource = twin ? twin(`${name}$r`, node, this.ctx, path) : null;

    this.out.push(
      new ProcedureEmitter(name, node, this.ctx, procedureScope(path)).emit());
    this.out.push(`R.markProcedure(${name}, ${JSON.stringify(node.name ?? 'anonymous')});`);
    if (twinSource !== null) {
      this.out.push(twinSource);
      this.out.push(`${name}.$resume = ${name}$r;`);
    }
  }

  /**
   * Whether a local is held in a box rather than a plain variable.
   * @param {string} name - A renamed Scheme identifier.
   * @returns {boolean} True if it is boxed.
   */
  isBoxed(name) {
    return this.ctx.boxed !== undefined && this.ctx.boxed.has(name);
  }

  /**
   * An expression reading a local.
   * @param {string} name - A renamed Scheme identifier.
   * @returns {string} A JavaScript expression.
   */
  readLocal(name) {
    return this.isBoxed(name) ? `${jsName(name)}[0]` : jsName(name);
  }

  /**
   * A statement binding a local to an initial value.
   *
   * A boxed local's box is created here, so that every reader of the name --
   * including a closure and a resumed frame -- reaches the same one.
   *
   * @param {string} name - A renamed Scheme identifier.
   * @param {string} value - A JavaScript expression.
   * @returns {string} A JavaScript statement.
   */
  bindLocal(name, value) {
    this.declared.add(jsName(name));
    return this.isBoxed(name)
      ? `${jsName(name)} = [${value}];` : `${jsName(name)} = ${value};`;
  }

  /**
   * A statement assigning to an already-bound local.
   * @param {string} name - A renamed Scheme identifier.
   * @param {string} value - A JavaScript expression.
   * @returns {string} A JavaScript statement.
   */
  assignLocal(name, value) {
    return `${this.readLocal(name)} = ${value};`;
  }

  /**
   * Allocates a temporary, numbered within this procedure rather than across
   * the whole compilation unit.
   *
   * That is what lets a procedure's two forms -- the fast one here and the
   * resumable one in `resume.js` -- agree on what to call each value. They
   * traverse the same IR in the same order, so counting separately from zero
   * makes them reach the same name at the same point. The agreement is
   * load-bearing: one form spills its locals into a frame that the other
   * restores by name.
   *
   * @returns {string} A fresh temporary name.
   */
  temp() {
    const name = `$t${this.scope.counter.n++}`;
    this.declared.add(name);
    return name;
  }

  /**
   * Creates the boxes for internal definitions, before any of the body runs.
   *
   * Mutually recursive internal definitions are the reason. A procedure defined
   * earlier in a body can refer to one defined later, so it has to be handed
   * something that will hold that procedure once the later definition executes.
   * The box has to exist before either definition is evaluated, which is why it
   * cannot be made at the definition itself.
   *
   * Only definitions whose names are boxed need it, and a name is boxed only if
   * some nested procedure actually refers to it, so an ordinary internal helper
   * stays a plain variable.
   *
   * @returns {void}
   */
  emitDefineBoxes() {
    // Found anywhere in this procedure's own body, stopping only at a nested
    // procedure, which makes its own. Written as a general walk rather than a
    // list of node kinds because enumerating them missed `if` -- and a
    // definition inside a conditional branch is ordinary Scheme.
    //
    // Creating the box at entry rather than where the definition appears is
    // safe, and necessary: something defined earlier in the body may already
    // hold it. A procedure body runs once per call, and tail calls go through
    // the trampoline rather than looping here, so there is exactly one box per
    // invocation either way.
    const names = [];
    const find = (node) => {
      if (node === null || typeof node !== 'object') return;
      if (Array.isArray(node)) { for (const item of node) find(item); return; }
      if (node.k === 'lambda') return;
      if (node.k === 'define') names.push(node.name);
      for (const key of Object.keys(node)) {
        if (key === 'k' || key === 'name' || key === 'names' || key === 'params') continue;
        find(node[key]);
      }
    };
    find(this.ir.body);

    for (const name of names) {
      if (!this.isBoxed(name)) continue;
      this.declared.add(jsName(name));
      this.out.push(`${jsName(name)} = [undefined];`);
    }
  }

  /**
   * Emits the procedure.
   * @returns {string} JavaScript source for a function expression.
   */
  emit() {
    const params = this.ir.params.map(jsName);
    const rest = this.ir.rest ? `...${jsName(this.ir.rest)}$raw` : null;
    const signature = [...params, rest].filter(Boolean).join(', ');

    this.emitDefineBoxes();
    this.statement(this.ir.body);

    const prologue = [];
    if (this.ir.rest) {
      // A rest parameter is a Scheme list, not a JavaScript array.
      const list = `R.listFrom(${jsName(this.ir.rest)}$raw)`;
      prologue.push(`let ${jsName(this.ir.rest)} = `
        + `${this.isBoxed(this.ir.rest) ? `[${list}]` : list};`);
    }
    // A boxed parameter arrives as a plain value and is boxed on entry, so that
    // the body, any closure it makes, and any frame spilled from it all reach
    // one binding. The resumable form does not do this: its parameters come out
    // of a frame already boxed.
    for (const param of this.ir.params) {
      if (this.isBoxed(param)) prologue.push(`${jsName(param)} = [${jsName(param)}];`);
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
        this.out.push(this.bindLocal(node.name, init));
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
    const nested = new ProcedureEmitter(this.name, this.ir, this.ctx, this.scope);
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
    // The fallback goes through `invoke` and `settle` rather than calling the
    // binding directly. It is reached only once the guard has failed, so the
    // binding is no longer the primitive that was inlined and may be anything
    // at all -- including an interpreted closure, whose plain call signature
    // converts Scheme values as if they were leaving Scheme, and which may
    // return a pending tail call.
    const slow = `R.settle(R.invoke(${accessor}(), [${operands.join(', ')}]))`;
    const shapeTest = entry.test(operands);
    const fast = entry.value(operands);
    const result = this.temp();

    // The binding guard always applies; a shape test is added when the fast
    // path only covers some operands.
    const condition = shapeTest === null
      ? `${accessor}() === ${guard}`
      : `${accessor}() === ${guard} && (${shapeTest})`;

    this.out.push(`${result} = ${condition} ? (${fast}) : ${slow};`);
    // A redefined primitive that captures a continuation. There is no resume
    // point here -- an inline expansion is not a call site the resumable form
    // splits at -- so this frame cannot suspend into the continuation, and
    // saying so is better than continuing with the sentinel as a value.
    // The message lives in the runtime rather than here: there are hundreds of
    // these sites and inlining the text was a fifth of the generated library.
    this.out.push(`if (${result} === R.UNWIND) R.captureUnderPrimitive();`);
    return result;
  }

  /**
   * Emits what happens when a callee reports that a continuation is being
   * captured beneath this frame.
   *
   * This frame has to become part of that continuation, and it cannot do so as
   * the JavaScript frame it is -- there is no way to read one back. So it hands
   * over the two things its resumable form needs in order to carry on from
   * here: the block to re-enter at, and the values of its locals. Then it
   * reports the same thing to its own caller, so that every frame between the
   * capture and the interpreter records itself on the way out.
   *
   * @param {Object} node - The `call` IR node just emitted.
   * @param {string} result - The temporary holding the call's result.
   * @returns {string} A JavaScript statement.
   */
  suspension(node, result) {
    const pc = this.ctx.resumePoints.get(node);
    const slots = this.ctx.frameSlots.get(this.ir);
    // `R.UNWIND` as the tested expression means the caller knows the suspension
    // is unconditional, so the check collapses to the spill and the return.
    const unconditional = result === 'R.UNWIND';
    if (pc === undefined || slots === undefined) {
      // No resumable form was generated for this procedure, so there is nothing
      // to suspend into. Refusing is the point: the alternative is to carry on
      // with the sentinel as though it were the call's value, and return an
      // answer assembled from a continuation with a hole in it.
      return unconditional
        ? 'R.captureWithoutResume();'
        : `if (${result} === R.UNWIND) R.captureWithoutResume();`;
    }
    const spill = `R.reify(${this.name}$r, ${pc}, { ${slots.join(', ')} }); return R.UNWIND;`;
    return unconditional
      ? spill : `if (${result} === R.UNWIND) { ${spill} }`;
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
        return this.readLocal(node.name);

      case 'global':
        // Resolved through the environment on every reference, because Scheme
        // allows a top-level binding to be redefined after this code was
        // compiled -- including by the REPL that is running it.
        return `${this.ctx.globalRef(node.name)}()`;

      case 'lambda': {
        const { name, path } = this.nested();

        // Emitted once, at the top level, as a factory taking its free
        // variables -- so that every form of every parent shares one emission
        // rather than each carrying its own copy. Without this a lambda at
        // nesting depth d appears about 4^d times.
        if (this.ctx.lift !== undefined && this.ctx.lift.has(node)) {
          const factory = this.ctx.factoryFor(name, path, node);
          const args = (this.ctx.free.get(node) ?? []).map((free) => jsName(free));
          const result = this.temp();
          this.out.push(`${result} = ${factory}(${args.join(', ')});`);
          return result;
        }

        this.emitLambdaPair(name, node, path);
        return name;
      }

      case 'set': {
        const value = this.value(node.value);
        if (node.local) {
          this.out.push(this.assignLocal(node.name, value));
        } else {
          this.out.push(`E.set(${JSON.stringify(node.name)}, ${value});`);
        }
        return 'undefined';
      }

      case 'define': {
        // A boxed name's box already exists -- `emitDefineBoxes` created it
        // before any of the body ran, because a procedure defined earlier may
        // already be holding it in order to refer to this definition. So this
        // fills the box rather than making one.
        if (this.isBoxed(node.name)) {
          this.declared.add(jsName(node.name));
          const value = this.value(node.value);
          this.out.push(`${jsName(node.name)}[0] = ${value};`);
          return 'undefined';
        }
        const value = this.value(node.value);
        this.out.push(this.bindLocal(node.name, value));
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
        const then = new ProcedureEmitter(this.name, this.ir, this.ctx, this.scope);
        then.declared = this.declared;
        const thenValue = then.value(node.then);
        const other = new ProcedureEmitter(this.name, this.ir, this.ctx, this.scope);
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
        this.out.push(this.bindLocal(node.name, init));
        return this.value(node.body);
      }

      case 'letrec': {
        this.emitLetRecBindings(node);
        return this.value(node.body);
      }

      case 'capture': {
        // Always suspends, so there is no branch here: the sentinel is what
        // `R.capture` returns, unconditionally, and the value the continuation
        // is eventually invoked with arrives at the resume point instead.
        const receiver = this.value(node.receiver);
        const result = this.temp();
        this.out.push(`R.capture(${receiver});`);
        this.out.push(this.suspension(node, 'R.UNWIND'));
        return result;
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
        // choice cannot be made at compile time.
        this.out.push(`${raw} = ${callee}[R.SCHEME_RAW_CALL];`);
        this.out.push(
          `${result} = ${raw} === undefined ? ${callee}(${args.join(', ')})` +
          ` : ${raw}(${args.join(', ')});`);
        // The callee may have made a tail call, whether it was compiled or
        // interpreted. One identity-free instance check per call site settles it.
        this.out.push(
          `while (${result} instanceof R.TailCall) ` +
          `{ ${result} = R.step(${result}); }`);
        this.out.push(this.suspension(node, result));
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
